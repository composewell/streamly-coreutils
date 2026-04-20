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
-- By default the path does not need to exist — nonexistent trailing
-- components are preserved in the result. Use 'pathMustExist' to
-- require existence (matching GNU @realpath@'s default). Use
-- 'relativeTo' to produce a path relative to a given base directory
-- (corresponds to @realpath --relative-to@).
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

module Streamly.Coreutils.RealPath
    ( RealPathOptions
    , defaultConfig
    , pathMustExist
    , relativeTo
    , realPath
    )
where

import Control.Monad (when)
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath (makeRelative)
-- import System.IO.Error (ioError, userError)

-- = Design notes
--
-- * Thin wrapper over 'System.Directory.canonicalizePath' from the
--   @directory@ package. Per the package design notes, we don't
--   reimplement what @directory@ already does well.
--
-- * 'canonicalizePath' diverges from GNU @realpath@ on nonexistent
--   paths: it canonicalizes as much as it can rather than failing.
--   'pathMustExist' restores the GNU default via a pre-check.
--
-- * 'relativeTo' canonicalizes the base directory before diffing,
--   otherwise a base containing @..@ or symlinks would yield a
--   misleading relative path.
--
-- * Options-driven API following the package convention. Leaves room
--   for further GNU flags (@-s@ no-symlinks, @-q@ quiet) without
--   breaking the signature.
--
-- * Throws 'IOError' rather than returning 'Maybe'. A canonicalization
--   failure is an exceptional condition, not a lookup miss — matches
--   the error-handling guidance in the package design notes.

-- | Options for 'realPath'. Construct via 'defaultConfig' and compose
-- modifiers with @(.)@.
data RealPathOptions = RealPathOptions
    { _requireExistence :: Bool
    , _relativeBase     :: Maybe FilePath
    }

-- | Default configuration: does not require the path to exist and
-- returns an absolute path (no relative-to base).
defaultConfig :: RealPathOptions
defaultConfig = RealPathOptions
    { _requireExistence = False
    , _relativeBase     = Nothing
    }

-- | Require that the path exists. Corresponds to GNU @realpath -e@.
-- Throws 'IOError' if the path does not exist.
pathMustExist :: RealPathOptions -> RealPathOptions
pathMustExist opts = opts { _requireExistence = True }

-- | Return the canonical path relative to the given base directory.
-- Corresponds to GNU @realpath --relative-to=DIR@.
--
-- The base is canonicalized before the relative path is computed, so
-- @..@ segments and symlinks in the base are handled correctly.
--
-- If the canonical path and base share no common prefix (e.g. they
-- live on different Windows drives), the canonical absolute path is
-- returned unchanged.
relativeTo :: FilePath -> RealPathOptions -> RealPathOptions
relativeTo base opts = opts { _relativeBase = Just base }

-- | Resolve a path to its canonical form: make it absolute, normalize
-- @.@ and @..@, and follow all symbolic links.
-- Corresponds to the shell @realpath@ command.
--
-- Throws 'IOError' if the path cannot be canonicalized, or — when
-- 'pathMustExist' is set — if the path does not exist.
--
-- > realPath id                              "./foo/../bar"
-- > realPath pathMustExist                   "/etc/hostname"
-- > realPath (relativeTo "/home/alice")      "/home/alice/docs/file"
-- > realPath (pathMustExist . relativeTo "/") "/etc/hostname"
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
    canonical <- canonicalizePath path
    case _relativeBase opts of
        Nothing   -> return canonical
        Just base -> do
            canonicalBase <- canonicalizePath base
            return (makeRelative canonicalBase canonical)
