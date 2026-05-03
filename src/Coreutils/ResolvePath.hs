-- |
-- Module      : Coreutils.ResolvePath
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Resolve a filesystem path, similar to the GNU @realpath@ utility.
--
-- This function performs the following general functions:
--
--   1. Prefixes current directory to a relative path to convert it to an
--      absolute path.
--   2. Normalizes @..@ (parent directory) components by removing the preceding
--      path segment per occurrence.
--   3. Resolves symbolic links by replacing them with their target paths.
--
-- Use 'resolvePath' with @id@ as option modifier for default options. Custom
-- behavior can be achieved by composing option modifiers using @(.)@.
--
-- * Resolution order:
-- The order in which @..@ components and symbolic links are resolved can
-- affect the final result. This is controlled by the 'resolutionMode'
-- modifier and is the most important aspect to understand when using
-- 'resolvePath'.
-- * Existence requirements:
-- By default, all the parents in the input path must exist. Use the
-- 'existenceCheck' modifier to enforce whether the entire path or none of it
-- must exist on disk.
-- * Output form:
-- The resulting path is absolute by default. It can be made relative using
-- the 'relativeTo' or 'relativeWithin' modifiers.
-- * Defaults:
-- Each modifier documents the default behavior that applies when it is not
-- explicitly provided.
--
-- == GNU @realpath@ equivalences
--
-- Each binding below corresponds to a common GNU @realpath@ flag
-- combination.
--
-- Default (GNU @-E -P@, no relative output):
--
-- >>> _ = resolvePath id                                   -- realpath
-- >>> _ = resolvePath (existenceCheck RequirePath)         -- realpath -e
-- >>> _ = resolvePath (existenceCheck RequireParents)      -- realpath -E
-- >>> _ = resolvePath (existenceCheck RequireNone)         -- realpath -m
-- >>> _ = resolvePath (resolutionMode ResolveThenParent)   -- realpath -P
-- >>> _ = resolvePath (resolutionMode ParentThenResolve)   -- realpath -L
-- >>> _ = resolvePath (resolutionMode NoSymlinkResolution) -- realpath -s
-- >>> _ = resolvePath (relativeTo [path|/usr/bin|])              -- realpath --relative-to=/usr/bin
-- >>> _ = resolvePath (relativeWithin [path|/usr|])              -- realpath --relative-base=/usr
--
-- Composed modifiers:
--
-- >>> -- realpath --relative-to=/usr/bin --relative-base=/usr
-- >>> _ = resolvePath (relativeTo [path|/usr/bin|] . relativeWithin [path|/usr|])
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
-- * 'resolutionMode' 'NoSymlinkResolution' combined with 'existenceCheck'
--   'RequireNone' is the only configuration that performs no
--   filesystem access on the path components (only
--   'System.Directory.getCurrentDirectory' when the path is
--   relative). All other configurations involve some filesystem IO.
-- * Because 'NoSymlinkResolution' is lexical, it can give a different result
--   than the default mode when the path traverses through a symlink
--   via @..@: @\/link\/..@ lexically resolves to @\/@, but physically
--   resolves to the parent of the symlink's target.

module Coreutils.ResolvePath
    ( ResolvePathOptions
    , ExistenceCheck (..)
    , ResolutionMode (..)
    , existenceCheck
    , resolutionMode
    , relativeTo
    , relativeWithin
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

import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Control.Exception (try, SomeException)
-- >>> import System.Directory (canonicalizePath, getCurrentDirectory, getTemporaryDirectory)
-- >>> import System.FilePath ((</>), isAbsolute)
-- >>> import Streamly.FileSystem.Path (path)
-- >>> import qualified Streamly.FileSystem.Path as Path

-- = Design notes
--
-- * Thin wrapper over 'System.Directory.canonicalizePath' for the
--   default 'ResolveThenParent' (physical) mode; 'ParentThenResolve' and
--   'NoSymlinkResolution' use 'makeAbsolute' plus a custom @..@-collapsing
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
-- * Default 'ExistenceCheck' is 'RequireParents', matching GNU @-E@.
--   This is a genuine behavior change from a pre-release iteration
--   that defaulted to \"accept anything\" - we chose GNU
--   compatibility as the cost of being slightly less permissive by
--   default.
--
-- * 'RequireParents' is implemented via @doesDirectoryExist@ on
--   'takeDirectory' of the path. If the immediate parent directory
--   exists then every intermediate ancestor must too (by
--   transitivity of directory existence), so a single check covers
--   the GNU @-E@ requirement. Edge cases handled by 'takeDirectory':
--   bare filenames give @"."@ (always exists), @\/@ gives @\/@
--   (always exists), so these pass without special-casing.
--
-- * 'RequirePath' uses 'doesPathExist' rather than
--   'doesDirectoryExist' so that files (not just directories) at the
--   leaf are accepted.
--
-- * 'ParentThenResolve' is implemented as
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
-- * 'relativeWithin' composes with 'relativeTo' as two independent
--   concerns: 'relativeTo' chooses the target, 'relativeWithin'
--   gates whether relativization fires. When 'relativeWithin' is
--   set alone, its directory serves both roles (matching GNU
--   @--relative-base=DIR@ without @--relative-to@). Containment is
--   tested component-wise via 'splitDirectories' so that partial
--   name prefixes (@\/foo@ vs @\/foobar@) don't register as matches.
--
-- * Throws 'IOError' rather than returning 'Maybe'. A canonicalization
--   failure is an exceptional condition, not a lookup miss - matches
--   the error-handling guidance in the package design notes.

-- | Specifies which parts of the input path are required to exist on disk
-- for 'resolvePath' to succeed.
--
-- * 'RequirePath':
--     Every component of the path, including the final (leaf) component,
--     must exist.
--
-- * 'RequireParents':
--     All ancestor directories must exist, but the final component may be
--     missing. This is useful for paths that refer to something that does
--     not yet exist (e.g. a destination file).
--
-- * 'RequireNone':
--     No part of the path is required to exist. The path is resolved as far
--     as possible using the longest existing prefix; any remaining
--     components are appended unchanged.
--
-- All three modes behave identically for paths that fully exist. They
-- differ only when some components are missing.
data ExistenceCheck
    = RequirePath
    | RequireParents
    | RequireNone

-- | Specifies how symbolic links and @..@ (parent directory) components are
-- handled during path resolution.
--
-- The key distinction is whether symbolic links are resolved at all, and if
-- they are, whether @..@ is interpreted before or after symlink expansion.
--
-- * 'NoSymlinkResolution':
--     Symbolic links are not resolved. Each @..@ removes the preceding path
--     segment from the original, unresolved path.
--
-- * 'ResolveThenParent':
--     Symbolic links are resolved as they are encountered. A subsequent @..@
--     refers to the parent of the /resolved target/, i.e. symlinks are expanded
--     before processing @..@.
--
-- * 'ParentThenResolve':
--     Each @..@ removes the preceding segment from the original input path,
--     regardless of whether that segment is a symlink. Any remaining symlinks
--     in the resulting path are then resolved.
--
-- These modes behave identically for paths that contain no symbolic links.
-- The difference between 'ResolveThenParent' and 'ParentThenResolve' appears
-- when a symlink is followed by @..@. 'NoSymlinkResolution' differs whenever
-- the path contains any symlink.
data ResolutionMode
    = NoSymlinkResolution
    | ResolveThenParent
    | ParentThenResolve

-- | Options for 'resolvePath'. Users don't construct 'ResolvePathOptions'
-- directly - instead, pass @id@ for the default behavior, or a
-- modifier (or composition of modifiers with @(.)@) to 'resolvePath'.
data ResolvePathOptions = ResolvePathOptions
    { _existenceCheck :: ExistenceCheck
    , _resolutionMode :: ResolutionMode
    , _relativeTo :: Maybe Path
    , _relativeWithin :: Maybe Path
    }

-- Default configuration: the seed value that modifiers are composed
-- onto. Users supply @id@ (or a modifier chain) at the call site
-- rather than referring to this directly.
defaultConfig :: ResolvePathOptions
defaultConfig = ResolvePathOptions
    { _existenceCheck = RequireParents
    , _resolutionMode = ResolveThenParent
    , _relativeTo = Nothing
    , _relativeWithin = Nothing
    }

-- | Set the path existence requirement for 'resolvePath'.
-- See 'ExistenceCheck' for a detailed description of each mode.
--
-- Default (when not specified): 'RequireParents' - all ancestor
-- directories must exist, but the final component may be missing.
--
-- === Examples
--
-- 'RequirePath' rejects a path whose final component does not exist:
--
-- >>> cwd <- getCurrentDirectory >>= Path.fromString
-- >>> r1 <- resolvePath (existenceCheck RequirePath) cwd
-- >>> r2 <- resolvePath (existenceCheck RequirePath) r1
-- >>> Path.toString r1 == Path.toString r2
-- True
--
-- >>> result <- try (resolvePath (existenceCheck RequirePath) [path|/definitely/does/not/exist/xyzzy|]) :: IO (Either SomeException Path.Path)
-- >>> either (const True) (const False) result
-- True
--
-- 'RequireParents' (the default) allows a missing final component as long
-- as its parent directory exists.
--
-- >>> tmp <- getTemporaryDirectory
-- >>> r1 <- resolvePath id =<< Path.fromString (tmp </> "missing-leaf")
-- >>> r2 <- canonicalizePath (tmp </> "missing-leaf") >>= Path.fromString
-- >>> Path.toString r1 == Path.toString r2
-- True
--
-- 'RequireParents' rejects a path whose parent directory does not exist:
--
-- >>> result <- try (resolvePath id [path|/definitely/does/not/exist/child|]) :: IO (Either SomeException Path.Path)
-- >>> either (const True) (const False) result
-- True
--
-- 'RequireNone' accepts any path, whether it exists or not:
--
-- >>> r <- resolvePath (existenceCheck RequireNone) [path|/definitely/does/not/exist/child|]
-- >>> null (Path.toString r)
-- False
existenceCheck :: ExistenceCheck -> ResolvePathOptions -> ResolvePathOptions
existenceCheck check opts = opts { _existenceCheck = check }

-- | Set the resolution mode - how @..@ segments and symbolic links
-- are handled. See 'ResolutionMode' for the three modes and a full
-- explanation.
--
-- Default (without this modifier): 'ResolveThenParent' - @..@ ascends
-- from the symlink's target (GNU @realpath@'s physical mode, @-P@).
--
-- The examples below compose with @'existenceCheck' 'RequireNone'@
-- so that the @..@ component in the test path doesn't trigger a
-- parent-existence failure. On a path that contains no symlinks, all
-- three modes produce the same result (both examples below go
-- through 'canonicalizePath', which expands any symlinks in the
-- base):
--
-- >>> tmp <- getTemporaryDirectory
-- >>> let opts m = resolutionMode m . existenceCheck RequireNone
-- >>> r1 <- resolvePath (opts ParentThenResolve) =<< Path.fromString (tmp </> "a" </> ".." </> "b")
-- >>> r2 <- resolvePath (existenceCheck RequireNone) =<< Path.fromString (tmp </> "b")
-- >>> Path.toString r1 == Path.toString r2
-- True
--
-- 'NoSymlinkResolution' collapses @..@ and @.@ textually and performs no
-- symlink resolution (so the base is not canonicalized - the result
-- may differ from 'ResolveThenParent' when the base contains symlinks):
--
-- >>> r <- resolvePath (opts NoSymlinkResolution) =<< Path.fromString (tmp </> "a" </> ".." </> "b")
-- >>> Path.toString r == tmp </> "b"
-- True
-- >>> r <- resolvePath (opts NoSymlinkResolution) =<< Path.fromString (tmp </> "." </> "x")
-- >>> Path.toString r == tmp </> "x"
-- True
resolutionMode :: ResolutionMode -> ResolvePathOptions -> ResolvePathOptions
resolutionMode mode opts = opts { _resolutionMode = mode }

-- | Return the canonical path relative to the given base directory.
--
-- Default (without this modifier): an absolute path is returned.
--
-- The base is canonicalized (physically, following symlinks) before
-- the relative path is computed, so that @..@ segments and symlinks in the
-- base are handled correctly.
--
-- If the canonical path and base do not share the same root (for
-- example, on different Windows drives), the absolute path is
-- returned unchanged.
--
-- A path relative to itself is @\".\"@:
--
-- >>> Path.toString <$> resolvePath (relativeTo [path|/|]) [path|/|]
-- "."
relativeTo :: Path -> ResolvePathOptions -> ResolvePathOptions
relativeTo base opts = opts { _relativeTo = Just base }

-- XXX realpath performs existence check for --relative-to path as well:
-- $ realpath --relative-base /usr /usr/bin --relative-to /usr/bin/x/y
-- realpath: /usr/bin/x/y: No such file or directory
--
-- Add these tests:
-- $ realpath --relative-base /usr /usr/bin/tr --relative-to /usr/bin/ls
-- ../tr
-- $ realpath --relative-base /usr /usr/bin --relative-to /etc
-- /usr/bin

-- | Relativize the resolved path only when both the result and the
-- relativization base lie within the given directory.
--
-- This modifier introduces a /containment boundary/. Relativization is
-- performed only if both:
--
--   * the resolved path, and
--   * the base used for relativization
--
-- are within the specified directory. Otherwise, the absolute path is
-- returned unchanged.
--
-- When 'relativeTo' is not specified, the given directory serves as
-- both the relativization base and the containment boundary.
-- When 'relativeTo' is specified, that determines the base, while this
-- modifier only controls the boundary.
--
-- Without this modifier, if 'relativeTo' is set, the result is made
-- relative whenever possible (even if it requires @..@ segments that
-- escape the base directory).
--
-- === Examples
--
-- Inside the boundary, the path is relativized:
--
-- >>> Path.toString <$> resolvePath (relativeWithin [path|/|]) [path|/missing-leaf|]
-- "missing-leaf"
--
-- Outside the boundary, the absolute path is returned unchanged:
--
-- >>> tmp <- getTemporaryDirectory >>= Path.fromString
-- >>> Path.toString <$> resolvePath (relativeWithin tmp) [path|/missing-leaf|]
-- "/missing-leaf"
relativeWithin :: Path -> ResolvePathOptions -> ResolvePathOptions
relativeWithin dir opts = opts { _relativeWithin = Just dir }

-- Collapse @.@ and @..@ segments lexically. On absolute paths, @..@
-- at the root is dropped (you can't ascend above @\/@). On relative
-- paths, leading @..@ segments are preserved.
--
-- Uses 'splitDirectories' / 'joinPath' from @filepath@ to stay
-- platform-correct on separator handling.
lexicalCollapse :: String -> String
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
checkExistence :: ExistenceCheck -> String -> IO ()
checkExistence check path = case check of
    RequireNone -> return ()
    RequirePath -> do
        exists <- doesPathExist path
        when (not exists) $
            ioError
                (userError ("resolvePath: path does not exist: " ++ path))
    RequireParents -> do
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
isPathUnder :: String -> String -> Bool
isPathUnder dir p = splitDirectories dir `isPrefixOf` splitDirectories p

-- | Resolve a filesystem path to its canonical form, similar to the
-- shell @realpath@ command.
--
-- By default, pass @id@ to use standard behavior. Custom behavior can be
-- enabled by supplying a modifier, or by composing multiple modifiers
-- with @(.)@. Each modifier documents the default that applies when it
-- is not explicitly provided.
--
-- Throws 'IOError' if the path cannot be resolved, or when
-- 'existenceCheck' is enabled and required components do not exist.
--
-- In the default mode, the result for an existing directory is always
-- an absolute path:
--
-- >>> r <- resolvePath id [path|.|]
-- >>> isAbsolute (Path.toString r)
-- True
resolvePath
    :: (ResolvePathOptions -> ResolvePathOptions)
    -> Path
    -> IO Path
resolvePath modifier path = do
    let opts = modifier defaultConfig
        pathStr = Path.toString path
    checkExistence (_existenceCheck opts) pathStr
    resolved <- case _resolutionMode opts of
        ResolveThenParent -> canonicalizePath pathStr
        ParentThenResolve ->
            fmap lexicalCollapse (makeAbsolute pathStr) >>= canonicalizePath
        NoSymlinkResolution -> fmap lexicalCollapse (makeAbsolute pathStr)
    -- Relativization and containment logic:
    --   * _relativeTo chooses the target to relativize against.
    --   * _relativeWithin gates whether relativization fires: if
    --     the resolved path is not under the boundary, we return the
    --     absolute result instead.
    --   * When _relativeWithin is set alone (no _relativeTo), its
    --     directory serves both roles.
    let target = case _relativeTo opts of
            Just t -> Just t
            Nothing -> _relativeWithin opts
    case target of
        Nothing -> Path.fromString resolved
        Just t -> do
            canonicalTarget <- canonicalizePath (Path.toString t)
            case _relativeWithin opts of
                Nothing -> Path.fromString (makeRelative canonicalTarget resolved)
                Just boundary -> do
                    canonicalBoundary <- canonicalizePath (Path.toString boundary)
                    if isPathUnder canonicalBoundary resolved
                    then Path.fromString (makeRelative canonicalTarget resolved)
                    else Path.fromString resolved
