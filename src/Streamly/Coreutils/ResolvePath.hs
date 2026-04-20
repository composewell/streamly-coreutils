-- |
-- Module      : Streamly.Coreutils.ResolvePath
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
-- Call 'resolvePath' with @id@ for the default behavior, or compose
-- modifiers with @(.)@ to customize:
--
-- * 'requireExistence' - control which path components must exist on
--   disk: 'AllParents' (default, GNU @-E@), 'EntirePath' (GNU @-e@),
--   or 'DontRequire' (GNU @-m@).
-- * 'resolutionMode' - control when (and whether) symbolic links
--   are expanded: 'UseTargetParents' (default, GNU @-P@), 'UseOriginalParents'
--   (GNU @-L@), or 'DontResolveSymlinks' (GNU @-s@).
-- * 'relativeTo' - produce a path relative to a given base directory
--   (GNU @realpath --relative-to=DIR@).
-- * 'relativeIfWithin' - produce a relative path only if it's under
--   a given directory, otherwise absolute (GNU
--   @realpath --relative-base=DIR@).
--
-- Each modifier's Haddock describes the default that applies in its
-- absence.
--
-- == GNU @realpath@ equivalences
--
-- Each binding below corresponds to a common GNU @realpath@ flag
-- combination.
--
-- Default (GNU @-E -P@, no relative output):
--
-- >>> _ = resolvePath id                                   -- realpath
-- >>> _ = resolvePath (requireExistence EntirePath)        -- realpath -e
-- >>> _ = resolvePath (requireExistence AllParents)        -- realpath -E
-- >>> _ = resolvePath (requireExistence DontRequire)       -- realpath -m
-- >>> _ = resolvePath (resolutionMode UseTargetParents)    -- realpath -P
-- >>> _ = resolvePath (resolutionMode UseOriginalParents)  -- realpath -L
-- >>> _ = resolvePath (resolutionMode DontResolveSymlinks) -- realpath -s
-- >>> _ = resolvePath (relativeTo "/usr/bin")              -- realpath --relative-to=/usr/bin
-- >>> _ = resolvePath (relativeIfWithin "/usr")            -- realpath --relative-base=/usr
--
-- Composed modifiers:
--
-- >>> -- realpath --relative-to=/usr/bin --relative-base=/usr
-- >>> _ = resolvePath (relativeTo "/usr/bin" . relativeIfWithin "/usr")
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
-- * 'resolutionMode' 'DontResolveSymlinks' combined with 'requireExistence'
--   'DontRequire' is the only configuration that performs no
--   filesystem access on the path components (only
--   'System.Directory.getCurrentDirectory' when the path is
--   relative). All other configurations involve some filesystem IO.
-- * Because 'DontResolveSymlinks' is lexical, it can give a different result
--   than the default mode when the path traverses through a symlink
--   via @..@: @\/link\/..@ lexically resolves to @\/@, but physically
--   resolves to the parent of the symlink's target.

module Streamly.Coreutils.ResolvePath
    ( ResolvePathOptions
    , ExistenceCheck (..)
    , ResolutionMode (..)
    , requireExistence
    , resolutionMode
    , relativeTo
    , relativeIfWithin
    , resolvePath
    )
where

import Control.Monad (when)
import Data.List (isPrefixOf)
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
--   default 'UseTargetParents' (physical) mode; 'UseOriginalParents' and
--   'DontResolveSymlinks' use 'makeAbsolute' plus a custom @..@-collapsing
--   walker ('lexicalCollapse').
--
-- * Why a single 'ResolutionMode' enum instead of two flags.
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
-- * 'UseOriginalParents' is implemented as
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
--   symlinks) regardless of the 'ResolutionMode' mode. Otherwise
--   @relativeTo "foo/../bar"@ with a lexical base would give
--   surprising results. If a future use case needs a lexical base,
--   add a separate modifier rather than overloading this one.
--
-- * 'relativeIfWithin' composes with 'relativeTo' as two independent
--   concerns: 'relativeTo' chooses the target, 'relativeIfWithin'
--   gates whether relativization fires. When 'relativeIfWithin' is
--   set alone, its directory serves both roles (matching GNU
--   @--relative-base=DIR@ without @--relative-to@). Containment is
--   tested component-wise via 'splitDirectories' so that partial
--   name prefixes (@\/foo@ vs @\/foobar@) don't register as matches.
--
-- * Throws 'IOError' rather than returning 'Maybe'. A canonicalization
--   failure is an exceptional condition, not a lookup miss - matches
--   the error-handling guidance in the package design notes.

-- | Which components of a path must exist on disk for 'resolvePath' to
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
-- * 'UseTargetParents': @..@ means the parent of the symlink's
--   /target/. Symlinks are expanded first, so @..@ ascends from the
--   resolved location. Matches GNU @realpath@'s default physical
--   mode (@-P@).
-- * 'UseOriginalParents': @..@ means the parent in the /original/ path
--   you supplied - @..@ textually cancels the preceding segment,
--   regardless of whether that segment was a symlink. Remaining
--   symlinks in the surviving path are still expanded. Matches GNU
--   @realpath -L@ / @--logical@.
-- * 'DontResolveSymlinks': no symlinks are expanded anywhere in the path.
--   @..@ is lexical (same as 'UseOriginalParents'), and symlinks in
--   other components are preserved as-is. Matches GNU @realpath -s@
--   / @--no-symlinks@.
--
-- The three modes produce the same result on paths that contain no
-- symlinks. 'UseTargetParents' and 'UseOriginalParents' diverge when a
-- symlink is followed by @..@; 'DontResolveSymlinks' diverges from both
-- whenever the path contains any symlink.
data ResolutionMode
    = UseTargetParents
    | UseOriginalParents
    | DontResolveSymlinks

-- | Options for 'resolvePath'. Users don't construct 'ResolvePathOptions'
-- directly - instead, pass @id@ for the default behavior, or a
-- modifier (or composition of modifiers with @(.)@) to 'resolvePath'.
data ResolvePathOptions = ResolvePathOptions
    { _existenceCheck    :: ExistenceCheck
    , _resolutionMode :: ResolutionMode
    , _relativeTo        :: Maybe FilePath
    , _relativeIfWithin  :: Maybe FilePath
    }

-- Default configuration: the seed value that modifiers are composed
-- onto. Users supply @id@ (or a modifier chain) at the call site
-- rather than referring to this directly.
defaultConfig :: ResolvePathOptions
defaultConfig = ResolvePathOptions
    { _existenceCheck    = AllParents
    , _resolutionMode = UseTargetParents
    , _relativeTo        = Nothing
    , _relativeIfWithin  = Nothing
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
-- >>> r1 <- resolvePath (requireExistence EntirePath) cwd
-- >>> r2 <- resolvePath (requireExistence EntirePath) r1
-- >>> r1 == r2
-- True
--
-- >>> result <- try (resolvePath (requireExistence EntirePath) "/definitely/does/not/exist/xyzzy") :: IO (Either SomeException FilePath)
-- >>> either (const True) (const False) result
-- True
--
-- 'AllParents' (the default) accepts a missing leaf as long as the
-- parent directory exists. Comparing against 'canonicalizePath' of
-- the same input (which has the same symlink-expansion behavior on
-- the existing prefix):
--
-- >>> tmp <- getTemporaryDirectory
-- >>> r1 <- resolvePath id (tmp </> "missing-leaf")
-- >>> r2 <- canonicalizePath (tmp </> "missing-leaf")
-- >>> r1 == r2
-- True
--
-- 'AllParents' rejects a path whose parent does not exist:
--
-- >>> result <- try (resolvePath id "/definitely/does/not/exist/child") :: IO (Either SomeException FilePath)
-- >>> either (const True) (const False) result
-- True
--
-- 'DontRequire' accepts any path, existent or not:
--
-- >>> r <- resolvePath (requireExistence DontRequire) "/definitely/does/not/exist/child"
-- >>> null r
-- False
requireExistence :: ExistenceCheck -> ResolvePathOptions -> ResolvePathOptions
requireExistence check opts = opts { _existenceCheck = check }

-- | Set the resolution mode - how @..@ segments and symbolic links
-- are handled. See 'ResolutionMode' for the three modes and a full
-- explanation.
--
-- Default (without this modifier): 'UseTargetParents' - @..@ ascends
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
-- >>> let opts m = resolutionMode m . requireExistence DontRequire
-- >>> r1 <- resolvePath (opts UseOriginalParents) (tmp </> "a" </> ".." </> "b")
-- >>> r2 <- resolvePath (requireExistence DontRequire) (tmp </> "b")
-- >>> r1 == r2
-- True
--
-- 'DontResolveSymlinks' collapses @..@ and @.@ textually and performs no
-- symlink resolution (so the base is not canonicalized - the result
-- may differ from 'UseTargetParents' when the base contains symlinks):
--
-- >>> r <- resolvePath (opts DontResolveSymlinks) (tmp </> "a" </> ".." </> "b")
-- >>> r == tmp </> "b"
-- True
-- >>> r <- resolvePath (opts DontResolveSymlinks) (tmp </> "." </> "x")
-- >>> r == tmp </> "x"
-- True
resolutionMode :: ResolutionMode -> ResolvePathOptions -> ResolvePathOptions
resolutionMode mode opts = opts { _resolutionMode = mode }

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
-- >>> resolvePath (relativeTo cwd) cwd
-- "."
relativeTo :: FilePath -> ResolvePathOptions -> ResolvePathOptions
relativeTo base opts = opts { _relativeTo = Just base }

-- | Return a relative path only when the resolved path lies within
-- the given directory; otherwise return an absolute path.
-- Corresponds to GNU @realpath --relative-base=DIR@.
--
-- Default (without this modifier): no containment check is applied -
-- if 'relativeTo' is set, the result is always relative (possibly
-- with @..@ segments); if not, the result is absolute.
--
-- When composed with 'relativeTo', the 'relativeTo' directory is
-- used as the relativization target and this modifier's directory
-- is used as the containment boundary. When 'relativeTo' is not set,
-- this modifier's directory serves both roles.
--
-- Inside the boundary, the path is relativized:
--
-- >>> tmp <- getTemporaryDirectory
-- >>> let child = tmp </> "missing-leaf"
-- >>> resolvePath (relativeIfWithin tmp) child
-- "missing-leaf"
--
-- Outside the boundary, the absolute path is returned unchanged:
--
-- >>> r1 <- resolvePath (relativeIfWithin tmp) "/"
-- >>> r2 <- canonicalizePath "/"
-- >>> r1 == r2
-- True
relativeIfWithin :: FilePath -> ResolvePathOptions -> ResolvePathOptions
relativeIfWithin dir opts = opts { _relativeIfWithin = Just dir }

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
                (userError ("resolvePath: path does not exist: " ++ path))
    AllParents -> do
        let parent = takeDirectory path
        parentExists <- doesDirectoryExist parent
        when (not parentExists) $
            ioError
                (userError
                    ("resolvePath: parent directory does not exist: "
                        ++ parent))

-- Is the second path a descendant of (or equal to) the first?
-- Both arguments should already be canonicalized and absolute.
-- Uses component-wise comparison via 'splitDirectories' so that
-- partial-name matches (e.g. @\/foo@ vs @\/foobar@) don't register
-- as containment.
isPathUnder :: FilePath -> FilePath -> Bool
isPathUnder dir p = splitDirectories dir `isPrefixOf` splitDirectories p

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
-- >>> r <- resolvePath id cwd
-- >>> isAbsolute r
-- True
resolvePath
    :: (ResolvePathOptions -> ResolvePathOptions)
    -> FilePath
    -> IO FilePath
resolvePath modifier path = do
    let opts = modifier defaultConfig
    checkExistence (_existenceCheck opts) path
    resolved <- case _resolutionMode opts of
        UseTargetParents -> canonicalizePath path
        UseOriginalParents ->
            fmap lexicalCollapse (makeAbsolute path) >>= canonicalizePath
        DontResolveSymlinks -> fmap lexicalCollapse (makeAbsolute path)
    -- Relativization and containment logic:
    --   * _relativeTo chooses the target to relativize against.
    --   * _relativeIfWithin gates whether relativization fires: if
    --     the resolved path is not under the boundary, we return the
    --     absolute result instead.
    --   * When _relativeIfWithin is set alone (no _relativeTo), its
    --     directory serves both roles.
    let target = case _relativeTo opts of
            Just t -> Just t
            Nothing -> _relativeIfWithin opts
    case target of
        Nothing -> return resolved
        Just t -> do
            canonicalTarget <- canonicalizePath t
            case _relativeIfWithin opts of
                Nothing -> return (makeRelative canonicalTarget resolved)
                Just boundary -> do
                    canonicalBoundary <- canonicalizePath boundary
                    if isPathUnder canonicalBoundary resolved
                    then return (makeRelative canonicalTarget resolved)
                    else return resolved
