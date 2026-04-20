-- |
-- Module      : Streamly.Coreutils.RealPath
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Resolve a path to its canonical form: make it absolute, normalize
-- @.@ and @..@ segments, and follow every symbolic link along the way.
-- Corresponds to the shell @realpath@ command.
--
-- Call 'realPath' with @id@ for the default behavior, or compose
-- modifiers with @(.)@ to customize:
--
-- * 'requireExistence' - control which path components must exist on
--   disk: 'AllParents' (default, GNU @-E@), 'EntirePath' (GNU @-e@),
--   or 'DontRequire' (GNU @-m@).
-- * 'resolveSymlinks' - control when (and whether) symbolic links
--   are expanded: 'TargetParents' (default, GNU @-P@), 'OriginalParents'
--   (GNU @-L@), or 'DontResolve' (GNU @-s@).
-- * 'relativeTo' - produce a path relative to a given base directory
--   (GNU @realpath --relative-to=DIR@).
--
-- Each modifier's Haddock describes the default that applies in its
-- absence.
--
-- == Caveats
--
-- * On Windows, @subst@ drives are resolved through to their
--   underlying path.
-- * On POSIX, two paths referring to the same object are not
--   guaranteed to canonicalize identically (bind mounts,
--   case-insensitive filesystems, etc.).
-- * 'relativeTo' falls back to returning the canonicalized absolute
--   path unchanged when no common prefix exists with the base
--   (e.g. different drives on Windows).
-- * 'resolveSymlinks' 'DontResolve' combined with 'requireExistence'
--   'DontRequire' is the only configuration that performs no
--   filesystem access on the path components (only
--   'System.Directory.getCurrentDirectory' when the path is
--   relative). All other configurations involve some filesystem IO.
-- * Because 'DontResolve' is lexical, it can give a different result
--   than the default mode when the path traverses through a symlink
--   via @..@: @\/link\/..@ lexically resolves to @\/@, but physically
--   resolves to the parent of the symlink's target.

module Streamly.Coreutils.RealPath
    ( RealPathOptions
    , ExistenceCheck (..)
    , SymlinkResolution (..)
    , requireExistence
    , resolveSymlinks
    , relativeTo
    , realPath
    )
where

import Control.Monad (when)
import System.Directory
    (canonicalizePath, doesDirectoryExist, doesPathExist, makeAbsolute)
import System.FilePath
    (makeRelative, splitDirectories, joinPath, isAbsolute, takeDirectory)
-- import System.IO.Error (ioError, userError)

-- $setup
-- >>> import Control.Exception (try, SomeException)
-- >>> import System.Directory (canonicalizePath, getCurrentDirectory, getTemporaryDirectory)
-- >>> import System.FilePath ((</>), isAbsolute)

-- = Design notes
--
-- * Thin wrapper over 'System.Directory.canonicalizePath' for the
--   default 'TargetParents' (physical) mode; 'OriginalParents' and
--   'DontResolve' use 'makeAbsolute' plus a custom @..@-collapsing
--   walker ('lexicalCollapse').
--
-- * Why a single 'SymlinkResolution' enum instead of two flags.
--   Symlink expansion is a three-way choice, not two orthogonal
--   booleans. An earlier iteration exposed 'logical' and 'noSymlinks'
--   as separate modifiers, which required a precedence rule for
--   @logical . noSymlinks@ ('noSymlinks' won). Collapsing to one
--   enum makes the choice exclusive by construction - no precedence
--   rule needed, no way to express contradictory combinations. The
--   same reasoning applies to 'ExistenceCheck': three mutually
--   exclusive modes are one enum, not two flags.
--
-- * Why a custom walker for the lexical modes. We initially tried
--   'System.FilePath.normalise', but its documentation is explicit:
--   "Does not remove \"..\", because of symlinks" - e.g. Posix:
--   @normalise "/a/../c" == "/a/../c"@. That is the correct default
--   for a symlink-aware normalizer but is the wrong semantics for
--   @realpath -s@ and @-L@, which want symlink-oblivious lexical
--   resolution of @..@. So we collapse @..@ ourselves in
--   'lexicalCollapse'.
--
-- * 'canonicalizePath' diverges from GNU @realpath@ on nonexistent
--   paths: it canonicalizes as much as it can rather than failing.
--   The 'ExistenceCheck' pre-check restores GNU-compatible behavior
--   by rejecting missing paths before we call 'canonicalizePath'.
--
-- * Default 'ExistenceCheck' is 'AllParents', matching GNU @-E@.
--   This is a genuine behavior change from a pre-release iteration
--   that defaulted to \"accept anything\" - we chose GNU
--   compatibility as the cost of being slightly less permissive by
--   default.
--
-- * 'AllParents' is implemented via @doesDirectoryExist@ on
--   'takeDirectory' of the path. If the immediate parent directory
--   exists then every intermediate ancestor must too (by
--   transitivity of directory existence), so a single check covers
--   the GNU @-E@ requirement. Edge cases handled by 'takeDirectory':
--   bare filenames give @"."@ (always exists), @\/@ gives @\/@
--   (always exists), so these pass without special-casing.
--
-- * 'EntirePath' uses 'doesPathExist' rather than
--   'doesDirectoryExist' so that files (not just directories) at the
--   leaf are accepted.
--
-- * 'OriginalParents' is implemented as
--   @canonicalizePath . lexicalCollapse . makeAbsolute@: collapse
--   @..@ as text first, then let 'canonicalizePath' expand whatever
--   symlinks remain in the surviving components. This matches GNU
--   @-L@'s spec of "resolve @..@ before symlinks". Note that
--   symlinks /inside/ a symlink's target are still resolved
--   physically - @-L@ only governs @..@ in the input path.
--
-- * 'ExistenceCheck' is checked on the path as given, before any
--   symlink resolution or @..@ collapsing. This matches GNU
--   @realpath -e -s@ and @realpath -e -L@.
--
-- * 'relativeTo' always canonicalizes the base physically (following
--   symlinks) regardless of the 'SymlinkResolution' mode. Otherwise
--   @relativeTo "foo/../bar"@ with a lexical base would give
--   surprising results. If a future use case needs a lexical base,
--   add a separate modifier rather than overloading this one.
--
-- * Throws 'IOError' rather than returning 'Maybe'. A canonicalization
--   failure is an exceptional condition, not a lookup miss - matches
--   the error-handling guidance in the package design notes.

-- | Which components of a path must exist on disk for 'realPath' to
-- succeed.
--
-- * 'EntirePath': every component - including the leaf - must exist.
--   Matches GNU @realpath -e@ / @--canonicalize-existing@.
-- * 'AllParents': every ancestor directory must exist, but the leaf
--   component may be missing. Matches GNU @realpath -E@ /
--   @--canonicalize@, the default. This is useful for paths that
--   name something you're about to create, like the destination of
--   a copy.
-- * 'DontRequire': no component needs to exist. The result is
--   canonicalized as far as the existing prefix allows and the rest
--   is appended as-is. Matches GNU @realpath -m@ /
--   @--canonicalize-missing@.
data ExistenceCheck
    = EntirePath
    | AllParents
    | DontRequire

-- | How @..@ and symbolic links interact when resolving a path.
-- The three modes differ on where a @..@ segment points when it
-- follows a symlink, and on whether symlinks are expanded at all.
--
-- * 'TargetParents': @..@ means the parent of the symlink's
--   /target/. Symlinks are expanded first, so @..@ ascends from the
--   resolved location. Matches GNU @realpath@'s default physical
--   mode (@-P@).
-- * 'OriginalParents': @..@ means the parent in the /original/ path
--   you supplied - @..@ textually cancels the preceding segment,
--   regardless of whether that segment was a symlink. Remaining
--   symlinks in the surviving path are still expanded. Matches GNU
--   @realpath -L@ / @--logical@.
-- * 'DontResolve': no symlinks are expanded anywhere in the path.
--   @..@ is lexical (same as 'OriginalParents'), and symlinks in
--   other components are preserved as-is. Matches GNU @realpath -s@
--   / @--no-symlinks@.
--
-- The three modes produce the same result on paths that contain no
-- symlinks. 'TargetParents' and 'OriginalParents' diverge when a
-- symlink is followed by @..@; 'DontResolve' diverges from both
-- whenever the path contains any symlink.
data SymlinkResolution
    = TargetParents
    | OriginalParents
    | DontResolve

-- | Options for 'realPath'. Users don't construct 'RealPathOptions'
-- directly - instead, pass @id@ for the default behavior, or a
-- modifier (or composition of modifiers with @(.)@) to 'realPath'.
data RealPathOptions = RealPathOptions
    { _existenceCheck    :: ExistenceCheck
    , _symlinkResolution :: SymlinkResolution
    , _relativeBase      :: Maybe FilePath
    }

-- Default configuration: the seed value that modifiers are composed
-- onto. Users supply @id@ (or a modifier chain) at the call site
-- rather than referring to this directly.
defaultConfig :: RealPathOptions
defaultConfig = RealPathOptions
    { _existenceCheck    = AllParents
    , _symlinkResolution = TargetParents
    , _relativeBase      = Nothing
    }

-- | Set which components of a path must exist. See 'ExistenceCheck'
-- for the three modes and a full explanation.
--
-- Default (without this modifier): 'AllParents' - every ancestor
-- directory must exist, but the leaf may be missing (GNU
-- @realpath -E@).
--
-- 'EntirePath' rejects a path whose leaf does not exist:
--
-- >>> cwd <- getCurrentDirectory
-- >>> r1 <- realPath (requireExistence EntirePath) cwd
-- >>> r2 <- realPath (requireExistence EntirePath) r1
-- >>> r1 == r2
-- True
--
-- >>> result <- try (realPath (requireExistence EntirePath) "/definitely/does/not/exist/xyzzy") :: IO (Either SomeException FilePath)
-- >>> either (const True) (const False) result
-- True
--
-- 'AllParents' (the default) accepts a missing leaf as long as the
-- parent directory exists. Comparing against 'canonicalizePath' of
-- the same input (which has the same symlink-expansion behavior on
-- the existing prefix):
--
-- >>> tmp <- getTemporaryDirectory
-- >>> r1 <- realPath id (tmp </> "missing-leaf")
-- >>> r2 <- canonicalizePath (tmp </> "missing-leaf")
-- >>> r1 == r2
-- True
--
-- 'AllParents' rejects a path whose parent does not exist:
--
-- >>> result <- try (realPath id "/definitely/does/not/exist/child") :: IO (Either SomeException FilePath)
-- >>> either (const True) (const False) result
-- True
--
-- 'DontRequire' accepts any path, existent or not:
--
-- >>> r <- realPath (requireExistence DontRequire) "/definitely/does/not/exist/child"
-- >>> null r
-- False
requireExistence :: ExistenceCheck -> RealPathOptions -> RealPathOptions
requireExistence check opts = opts { _existenceCheck = check }

-- | Choose how @..@ and symbolic links interact. See
-- 'SymlinkResolution' for the three modes and a full explanation.
--
-- Default (without this modifier): 'TargetParents' - @..@ ascends
-- from the symlink's target (GNU @realpath@'s physical mode, @-P@).
--
-- The examples below compose with @'requireExistence' 'DontRequire'@
-- so that the @..@ component in the test path doesn't trigger a
-- parent-existence failure. On a path that contains no symlinks, all
-- three modes produce the same result (both examples below go
-- through 'canonicalizePath', which expands any symlinks in the
-- base):
--
-- >>> tmp <- getTemporaryDirectory
-- >>> let opts m = resolveSymlinks m . requireExistence DontRequire
-- >>> r1 <- realPath (opts OriginalParents) (tmp </> "a" </> ".." </> "b")
-- >>> r2 <- realPath (requireExistence DontRequire) (tmp </> "b")
-- >>> r1 == r2
-- True
--
-- 'DontResolve' collapses @..@ and @.@ textually and performs no
-- symlink resolution (so the base is not canonicalized - the result
-- may differ from 'TargetParents' when the base contains symlinks):
--
-- >>> r <- realPath (opts DontResolve) (tmp </> "a" </> ".." </> "b")
-- >>> r == tmp </> "b"
-- True
-- >>> r <- realPath (opts DontResolve) (tmp </> "." </> "x")
-- >>> r == tmp </> "x"
-- True
resolveSymlinks :: SymlinkResolution -> RealPathOptions -> RealPathOptions
resolveSymlinks mode opts = opts { _symlinkResolution = mode }

-- | Return the canonical path relative to the given base directory.
-- Corresponds to GNU @realpath --relative-to=DIR@.
--
-- Default (without this modifier): an absolute path is returned.
--
-- The base is canonicalized (physically, following symlinks) before
-- the relative path is computed, so @..@ segments and symlinks in the
-- base are handled correctly.
--
-- If the canonical path and base share no common prefix (e.g. they
-- live on different Windows drives), the canonical absolute path is
-- returned unchanged.
--
-- A path relative to itself is @\".\"@:
--
-- >>> cwd <- getCurrentDirectory
-- >>> realPath (relativeTo cwd) cwd
-- "."
relativeTo :: FilePath -> RealPathOptions -> RealPathOptions
relativeTo base opts = opts { _relativeBase = Just base }

-- Collapse @.@ and @..@ segments lexically. On absolute paths, @..@
-- at the root is dropped (you can't ascend above @\/@). On relative
-- paths, leading @..@ segments are preserved.
--
-- Uses 'splitDirectories' / 'joinPath' from @filepath@ to stay
-- platform-correct on separator handling.
lexicalCollapse :: FilePath -> FilePath
lexicalCollapse p =
    let parts = splitDirectories p
        absolute = isAbsolute p
        (root, rest) =
            if absolute
            then case parts of
                (r:xs) -> (Just r, xs)
                [] -> (Nothing, [])
            else (Nothing, parts)
        step acc "." = acc
        step acc ".." = case acc of
            -- Relative path: preserve leading ..
            [] -> if absolute then [] else [".."]
            (x:xs)
                | x == ".." -> "..":x:xs
                | otherwise -> xs
        step acc x = x : acc
        collapsed = reverse (foldl step [] rest)
    in case root of
        Just r -> joinPath (r : collapsed)
        Nothing -> if null collapsed then "." else joinPath collapsed

-- Perform the pre-resolution existence check demanded by the given
-- 'ExistenceCheck'. Throws 'IOError' on violation.
checkExistence :: ExistenceCheck -> FilePath -> IO ()
checkExistence check path = case check of
    DontRequire -> return ()
    EntirePath -> do
        exists <- doesPathExist path
        when (not exists) $
            ioError
                (userError ("realPath: path does not exist: " ++ path))
    AllParents -> do
        let parent = takeDirectory path
        parentExists <- doesDirectoryExist parent
        when (not parentExists) $
            ioError
                (userError
                    ("realPath: parent directory does not exist: "
                        ++ parent))

-- | Resolve a path to its canonical form.
-- Corresponds to the shell @realpath@ command.
--
-- Pass @id@ for default behavior, or a modifier (or modifier chain
-- composed with @(.)@) to customize. Each modifier's Haddock
-- documents the default that applies in its absence.
--
-- Throws 'IOError' if the path cannot be canonicalized, or - when
-- 'requireExistence' demands - if required components do not exist.
--
-- The default-mode result on an existing directory is absolute:
--
-- >>> cwd <- getCurrentDirectory
-- >>> r <- realPath id cwd
-- >>> isAbsolute r
-- True
realPath
    :: (RealPathOptions -> RealPathOptions)
    -> FilePath
    -> IO FilePath
realPath modifier path = do
    let opts = modifier defaultConfig
    checkExistence (_existenceCheck opts) path
    resolved <- case _symlinkResolution opts of
        TargetParents -> canonicalizePath path
        OriginalParents ->
            fmap lexicalCollapse (makeAbsolute path) >>= canonicalizePath
        DontResolve -> fmap lexicalCollapse (makeAbsolute path)
    case _relativeBase opts of
        Nothing -> return resolved
        Just base -> do
            canonicalBase <- canonicalizePath base
            return (makeRelative canonicalBase resolved)
