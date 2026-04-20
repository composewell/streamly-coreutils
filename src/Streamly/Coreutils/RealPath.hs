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
-- * 'pathMustExist' - require the path to exist (GNU @realpath -e@).
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
-- * 'resolveSymlinks' 'DontResolve' is purely lexical - no filesystem
--   access is made on the path itself (only
--   'System.Directory.getCurrentDirectory' when the path is relative).
--   It does not check whether the path exists unless combined with
--   'pathMustExist'. Because it's lexical, it can give a different
--   result than the default mode when the path traverses through a
--   symlink via @..@: @\/link\/..@ lexically resolves to @\/@, but
--   physically resolves to the parent of the symlink's target.

module Streamly.Coreutils.RealPath
    ( RealPathOptions
    , SymlinkResolution (..)
    , pathMustExist
    , resolveSymlinks
    , relativeTo
    , realPath
    )
where

import Control.Monad (when)
import System.Directory
    (canonicalizePath, doesPathExist, makeAbsolute)
import System.FilePath
    (makeRelative, splitDirectories, joinPath, isAbsolute)
-- import System.IO.Error (ioError, userError)

-- $setup
-- >>> import Control.Exception (try, SomeException)
-- >>> import System.Directory (getCurrentDirectory, getTemporaryDirectory)
-- >>> import System.FilePath ((</>), isAbsolute)

-- = Design notes
--
-- * Thin wrapper over 'System.Directory.canonicalizePath' for the
--   default 'TargetParents' (physical) mode; 'OriginalParents' and 'DontResolve'
--   use 'makeAbsolute' plus a custom @..@-collapsing walker
--   ('lexicalCollapse').
--
-- * Why a single 'SymlinkResolution' enum instead of two flags.
--   Symlink expansion is a three-way choice, not two orthogonal
--   booleans. An earlier iteration exposed 'logical' and 'noSymlinks'
--   as separate modifiers, which required a precedence rule for
--   @logical . noSymlinks@ ('noSymlinks' won). Collapsing to one
--   enum makes the choice exclusive by construction - no precedence
--   rule needed, no way to express contradictory combinations.
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
--   'pathMustExist' restores the GNU default via a pre-check.
--
-- * 'OriginalParents' is implemented as
--   @canonicalizePath . lexicalCollapse . makeAbsolute@: collapse
--   @..@ as text first, then let 'canonicalizePath' expand whatever
--   symlinks remain in the surviving components. This matches GNU
--   @-L@'s spec of "resolve @..@ before symlinks". Note that
--   symlinks /inside/ a symlink's target are still resolved
--   physically - @-L@ only governs @..@ in the input path.
--
-- * 'pathMustExist' composes with any 'SymlinkResolution': existence
--   is checked on the path as given, not on the expanded form,
--   matching GNU @realpath -e -s@ and @realpath -e -L@.
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
    { _requireExistence  :: Bool
    , _symlinkResolution :: SymlinkResolution
    , _relativeBase      :: Maybe FilePath
    }

-- Default configuration: the seed value that modifiers are composed
-- onto. Users supply @id@ (or a modifier chain) at the call site
-- rather than referring to this directly.
defaultConfig :: RealPathOptions
defaultConfig = RealPathOptions
    { _requireExistence  = False
    , _symlinkResolution = TargetParents
    , _relativeBase      = Nothing
    }

-- | Require that the path exists. Corresponds to GNU @realpath -e@.
-- Throws 'IOError' if the path does not exist.
--
-- Default (without this modifier): the path does not need to exist;
-- nonexistent trailing components are preserved in the result.
--
-- Succeeds on an existing path (result is canonicalized and is
-- idempotent under another 'realPath'):
--
-- >>> cwd <- getCurrentDirectory
-- >>> r1 <- realPath pathMustExist cwd
-- >>> r2 <- realPath pathMustExist r1
-- >>> r1 == r2
-- True
--
-- Throws on a nonexistent path:
--
-- >>> result <- try (realPath pathMustExist "/definitely/does/not/exist/xyzzy") :: IO (Either SomeException FilePath)
-- >>> either (const True) (const False) result
-- True
pathMustExist :: RealPathOptions -> RealPathOptions
pathMustExist opts = opts { _requireExistence = True }

-- | Choose how @..@ and symbolic links interact. See
-- 'SymlinkResolution' for the three modes and a full explanation.
--
-- Default (without this modifier): 'TargetParents' - @..@ ascends
-- from the symlink's target (GNU @realpath@'s physical mode, @-P@).
--
-- 'DontResolve' does not check whether the path exists unless
-- combined with 'pathMustExist'.
--
-- On a path that contains no symlinks, all three modes produce the
-- same result (both examples below go through 'canonicalizePath',
-- which expands any symlinks in the base):
--
-- >>> tmp <- getTemporaryDirectory
-- >>> r1 <- realPath (resolveSymlinks OriginalParents) (tmp </> "a" </> ".." </> "b")
-- >>> r2 <- realPath id (tmp </> "b")
-- >>> r1 == r2
-- True
--
-- 'DontResolve' collapses @..@ and @.@ textually and performs no
-- filesystem resolution (so the base is not canonicalized - the
-- result may differ from 'TargetParents' when the base contains
-- symlinks):
--
-- >>> r <- realPath (resolveSymlinks DontResolve) (tmp </> "a" </> ".." </> "b")
-- >>> r == tmp </> "b"
-- True
-- >>> r <- realPath (resolveSymlinks DontResolve) (tmp </> "." </> "x")
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

-- | Resolve a path to its canonical form.
-- Corresponds to the shell @realpath@ command.
--
-- Pass @id@ for default behavior, or a modifier (or modifier chain
-- composed with @(.)@) to customize. Each modifier's Haddock
-- documents the default that applies in its absence.
--
-- Throws 'IOError' if the path cannot be canonicalized, or - when
-- 'pathMustExist' is set - if the path does not exist.
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
    when (_requireExistence opts) $ do
        exists <- doesPathExist path
        when (not exists) $
            ioError (userError ("realPath: path does not exist: " ++ path))
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
