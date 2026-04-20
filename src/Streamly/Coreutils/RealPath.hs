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
-- * 'noSymlinks' - don't expand symbolic links; normalize @.@ and
--   @..@ lexically only (GNU @realpath -s@ / @--no-symlinks@).
-- * 'logical' - resolve @..@ components lexically before expanding
--   symlinks (GNU @realpath -L@ / @--logical@).
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
-- * 'noSymlinks' is purely lexical - no filesystem access is made on
--   the path itself (only 'System.Directory.getCurrentDirectory' when
--   the path is relative). It does not check whether the path exists
--   unless combined with 'pathMustExist'. Because it's lexical, it
--   can give a different result than the default mode when the path
--   traverses through a symlink via @..@: @\/link\/..@ lexically
--   resolves to @\/@, but physically resolves to the parent of the
--   symlink's target.

module Streamly.Coreutils.RealPath
    ( RealPathOptions
    , pathMustExist
    , noSymlinks
    , logical
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
--   default physical mode; 'noSymlinks' uses 'makeAbsolute' plus a
--   custom @..@-collapsing walker.
--
-- * Why a custom walker for 'noSymlinks'. We initially tried
--   'System.FilePath.normalise', but its documentation is explicit:
--   "Does not remove \"..\", because of symlinks" - e.g. Posix:
--   @normalise "/a/../c" == "/a/../c"@. That is the correct default
--   for a symlink-aware normalizer but is the wrong semantics for
--   @realpath -s@, which explicitly asks for symlink-oblivious
--   lexical resolution. So we collapse @..@ ourselves with
--   'lexicalCollapse' below.
--
-- * 'canonicalizePath' diverges from GNU @realpath@ on nonexistent
--   paths: it canonicalizes as much as it can rather than failing.
--   'pathMustExist' restores the GNU default via a pre-check.
--
-- * 'logical' is implemented as @canonicalizePath . lexicalCollapse@:
--   collapse @..@ segments as text first, then let 'canonicalizePath'
--   expand whatever symlinks remain in the surviving components. This
--   matches GNU @-L@'s spec of "resolve @..@ before symlinks". Note
--   that symlinks /inside/ a symlink's target are still resolved
--   physically - @-L@ only governs @..@ in the input path. An earlier
--   version of this module deferred implementing @-L@ out of caution
--   about a custom per-component walker; once 'lexicalCollapse' was
--   written and tested for 'noSymlinks', 'logical' reduced to the
--   two-step composition above.
--
-- * When 'logical' and 'noSymlinks' are both set, 'noSymlinks' wins:
--   no symlink expansion happens in either phase. 'logical' in that
--   combination is redundant (its lexical step is also what
--   'noSymlinks' does).
--
-- * 'pathMustExist' composes with 'noSymlinks': existence is checked
--   on the path as given, not the expanded form, matching GNU
--   @realpath -e -s@.
--
-- * 'relativeTo' always canonicalizes the base physically (following
--   symlinks) regardless of 'noSymlinks'. Otherwise
--   @relativeTo "foo/../bar"@ with a lexical base would give
--   surprising results. If a future use case needs a lexical base,
--   add a separate modifier rather than overloading this one.
--
-- * Throws 'IOError' rather than returning 'Maybe'. A canonicalization
--   failure is an exceptional condition, not a lookup miss - matches
--   the error-handling guidance in the package design notes.

-- | Options for 'realPath'. Users don't construct 'RealPathOptions'
-- directly - instead, pass @id@ for the default behavior, or a
-- modifier (or composition of modifiers with @(.)@) to 'realPath'.
data RealPathOptions = RealPathOptions
    { _requireExistence :: Bool
    , _expandSymlinks   :: Bool
    , _logicalDots      :: Bool
    , _relativeBase     :: Maybe FilePath
    }

-- Default configuration: the seed value that modifiers are composed
-- onto. Users supply @id@ (or a modifier chain) at the call site
-- rather than referring to this directly.
defaultConfig :: RealPathOptions
defaultConfig = RealPathOptions
    { _requireExistence = False
    , _expandSymlinks   = True
    , _logicalDots      = False
    , _relativeBase     = Nothing
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

-- | Don't expand symbolic links. The path is made absolute and
-- @.@\/@..@ segments are normalized lexically, but symlinks are left
-- in place. Corresponds to GNU @realpath -s@ / @--no-symlinks@.
--
-- Default (without this modifier): symbolic links are fully expanded
-- (GNU @realpath@'s physical mode, @-P@).
--
-- This is a purely lexical operation on the path string - no
-- filesystem access is made on the path components (only
-- 'System.Directory.getCurrentDirectory' when the input is relative).
--
-- Collapses @..@ and @.@ textually:
--
-- >>> tmp <- getTemporaryDirectory
-- >>> r <- realPath noSymlinks (tmp </> "a" </> ".." </> "b")
-- >>> r == tmp </> "b"
-- True
--
-- Handles @.@ segments:
--
-- >>> r <- realPath noSymlinks (tmp </> "." </> "x")
-- >>> r == tmp </> "x"
-- True
noSymlinks :: RealPathOptions -> RealPathOptions
noSymlinks opts = opts { _expandSymlinks = False }

-- | Resolve @..@ components lexically before expanding symbolic
-- links. Corresponds to GNU @realpath -L@ / @--logical@.
--
-- Default (without this modifier): @..@ is resolved physically, i.e.
-- symlinks in the path are expanded first and @..@ then applies to
-- the resolved location. With this modifier, @..@ is applied as text
-- first (so @\/link\/..@ becomes @\/@), and any symlinks remaining in
-- the surviving components are expanded afterwards.
--
-- The two modes give the same result on paths that don't mix @..@
-- with symlinks. They diverge on a path like @\/link\/..@ where
-- @\/link@ is a symlink: physical resolution follows the symlink and
-- then ascends from its target; logical resolution cancels the
-- @link@ and @..@ textually, giving @\/@.
--
-- If combined with 'noSymlinks', 'noSymlinks' wins - no symlinks are
-- expanded in either phase.
--
-- On a path without symlinks, logical mode is equivalent to default
-- mode:
--
-- >>> tmp <- getTemporaryDirectory
-- >>> r <- realPath logical (tmp </> "a" </> ".." </> "b")
-- >>> r == tmp </> "b"
-- True
logical :: RealPathOptions -> RealPathOptions
logical opts = opts { _logicalDots = True }

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
                     []     -> (Nothing, [])
            else (Nothing, parts)
        step acc "."  = acc
        step acc ".." = case acc of
                            -- Relative path: preserve leading ..
                            []                   -> if absolute then [] else [".."]
                            (x:xs) | x == ".."   -> "..":x:xs
                                   | otherwise   -> xs
        step acc x    = x : acc
        collapsed = reverse (foldl step [] rest)
    in case root of
           Just r  -> joinPath (r : collapsed)
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
    -- Three modes, in precedence order:
    --   1. noSymlinks: purely lexical (wins over logical if both set).
    --   2. logical: lexically collapse .., then expand symlinks.
    --   3. physical (default): canonicalizePath does everything.
    resolved <-
        if not (_expandSymlinks opts)
        then fmap lexicalCollapse (makeAbsolute path)
        else if _logicalDots opts
             then fmap lexicalCollapse (makeAbsolute path) >>= canonicalizePath
             else canonicalizePath path
    case _relativeBase opts of
        Nothing   -> return resolved
        Just base -> do
            canonicalBase <- canonicalizePath base
            return (makeRelative canonicalBase resolved)
