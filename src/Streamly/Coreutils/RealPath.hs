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
-- components are preserved in the result. Use 'existenceCheck' to
-- require existence (matching GNU @realpath@'s default).
--
-- == Caveats
--
-- * On Windows, @subst@ drives are resolved through to their
--   underlying path.
-- * On POSIX, two paths referring to the same object are not
--   guaranteed to canonicalize identically (bind mounts,
--   case-insensitive filesystems, etc.).

module Streamly.Coreutils.RealPath
    ( RealPathOptions
    , defaultConfig
    , existenceCheck
    , realPath
    )
where

import Control.Monad (when)
import System.Directory (canonicalizePath, doesPathExist)
-- import System.IO.Error (ioError, userError)

-- = Design notes
--
-- * Thin wrapper over 'System.Directory.canonicalizePath' from the
--   @directory@ package. Per the package design notes, we don't
--   reimplement what @directory@ already does well.
--
-- * 'canonicalizePath' diverges from GNU @realpath@ on nonexistent
--   paths: it canonicalizes as much as it can rather than failing.
--   'existenceCheck' restores the GNU default via a pre-check.
--
-- * Options-driven API following the package convention even though
--   only one flag is currently honored. Leaves room for
--   @--relative-to@, @-s@ (no-symlinks), etc. without breaking the
--   signature.
--
-- * Throws 'IOError' rather than returning 'Maybe'. A canonicalization
--   failure is an exceptional condition, not a lookup miss — matches
--   the error-handling guidance in the package design notes.

-- | Options for 'realPath'. Construct via 'defaultConfig' and compose
-- modifiers with @(.)@.
newtype RealPathOptions = RealPathOptions
    { _requireExistence :: Bool
    }

-- | Default configuration: does not require the path to exist.
defaultConfig :: RealPathOptions
defaultConfig = RealPathOptions { _requireExistence = False }

-- | Require that the path exists. Corresponds to GNU @realpath -e@.
-- Throws 'IOError' if the path does not exist.
existenceCheck :: RealPathOptions -> RealPathOptions
existenceCheck opts = opts { _requireExistence = True }

-- | Resolve a path to its canonical absolute form: make it absolute,
-- normalize @.@ and @..@, and follow all symbolic links.
-- Corresponds to the shell @realpath@ command.
--
-- Throws 'IOError' if the path cannot be canonicalized, or — when
-- 'existenceCheck' is set — if the path does not exist.
--
-- > realPath id              "./foo/../bar"
-- > realPath existenceCheck  "/etc/hostname"
realPath :: (RealPathOptions -> RealPathOptions) -> FilePath -> IO FilePath
realPath modifier path = do
    let opts = modifier defaultConfig
    when (_requireExistence opts) $ do
        exists <- doesPathExist path
        when (not exists) $
            ioError (userError ("realPath: path does not exist: " ++ path))
    canonicalizePath path
