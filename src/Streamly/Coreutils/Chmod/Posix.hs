{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Streamly.Coreutils.Chmod.Posix
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- change file mode bits.

-- TODO: change this module to Chmod.Posix and later create a portable module.
--
-- Design notes:
--
-- On Posix systems:
--
-- Roles: User (Owner), group (only one), others
-- Permissions: rwxX(ugo), s(go), t(o)
--
-- 1. write: create or delete a file in a directory. Modify contents of a file.
-- 2. write: modify metadata of a directory or file.
-- 3. execute: to list a directory's contents
--
-- On Windows:
--
-- Could not find any good docs by microsoft on a google search.
-- Managing permissions: https://learn.microsoft.com/en-us/previous-versions/windows/it-pro/windows-server-2008-R2-and-2008/cc770962(v=ws.11)
-- https://learn.microsoft.com/en-us/windows/security/identity-protection/access-control/access-control
--
-- Roles: User (Owner), group (many)
-- Permissions: read, read+execute, modify (metadata, create/delete files in
-- dirs), write (write to a file), list dir, full control
-- Inheritance: permissions can be inherited from parent directories
-- Advanced Permissions: ...
--
-- 1. write: create or delete a file in a directory. Modify contents of a file.
-- 2. modify: modify metadata of a directory or file.
-- 3. list dir: to list a directory's contents
--
-- Common abstraction for windows/posix:
--
-- Roles: User/Owner
-- Permissions:
--
-- 1. write on Posix: write+modify on windows
-- 2. execute on dir: "list dir" on windows
--
-- Other's default permissions are controlled by umask on Posix. When setting
-- permissions we can ensure that other's permissions are less restrictive than
-- the owner? But we cannot do the same on windows.

module Streamly.Coreutils.Chmod.Posix
    (
    -- * Roles
      Role (..)

    -- * Permissions
    , Permissions
    , setReadable
    , setWritable
    , setExecutable
    , reset

    -- * Chmod
    , chmodWith
    , chmod
    )
where

import Data.Bits ((.|.), Bits ((.&.), complement))
import Streamly.Coreutils.StringQ
import qualified System.Posix as Posix
import GHC.IO.Unsafe (unsafePerformIO)

modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
modifyBit False b m = m .&. complement b
modifyBit True  b m = m .|. b

chmodWith :: Role -> Permissions -> FilePath -> IO ()
chmodWith utype (Permissions r w e) path = do
    case utype of
        Owner -> setOwnerPermissions
        Group -> setGroupPermissions
        Others -> setOthersPermissions

    where

    setOwnerPermissions = do
        stat <- Posix.getFileStatus path
        Posix.setFileMode
            path
            ( modifyBit e Posix.ownerExecuteMode
            . modifyBit w Posix.ownerWriteMode
            . modifyBit r Posix.ownerReadMode
            . Posix.fileMode $ stat
            )

    setGroupPermissions = do
        stat <- Posix.getFileStatus path
        Posix.setFileMode
            path
            ( modifyBit e Posix.groupExecuteMode
            . modifyBit w Posix.groupWriteMode
            . modifyBit r Posix.groupReadMode
            . Posix.fileMode $ stat
            )

    setOthersPermissions = do
        stat <- Posix.getFileStatus path
        Posix.setFileMode
            path
            ( modifyBit e Posix.otherExecuteMode
            . modifyBit w Posix.otherWriteMode
            . modifyBit r Posix.otherReadMode
            . Posix.fileMode $ stat
            )


setMode :: Role -> Permissions -> Posix.FileMode -> Posix.FileMode
setMode utype (Permissions r w e) mode =
    case utype of
        Owner -> setOwnerPermissions
        Group -> setGroupPermissions
        Others -> setOthersPermissions

        where

        setOwnerPermissions =
            modifyBit e Posix.ownerExecuteMode
                $ modifyBit w Posix.ownerWriteMode
                $ modifyBit r Posix.ownerReadMode mode

        setGroupPermissions =
            modifyBit e Posix.groupExecuteMode
                $ modifyBit w Posix.groupWriteMode
                $ modifyBit r Posix.groupReadMode mode

        setOthersPermissions =
            modifyBit e Posix.otherExecuteMode
                $ modifyBit w Posix.otherWriteMode
                $ modifyBit r Posix.otherReadMode mode

-- | Change the file permission modes for specified roles using the specified
-- permission modifier functions.
--
-- You can use the @mode@ quasiquoter to build the mode conveniently, for
-- example:
--
-- >> chmod [mode|a=rwx|] "a.txt"
--
chmod :: [(Role, Permissions -> Permissions)] -> FilePath -> IO ()
-- To implement this, get the file mode. Transform the FileMode using the roles
-- and permissions, and then use a single setFileMode call to set the mode in
-- the end.
chmod perms path = do
    stat <- Posix.getFileStatus path
    let fm = foldl tr (Posix.fileMode stat) perms
    Posix.setFileMode path fm
    return ()

    where

    tr mode (role, f) = unsafePerformIO $ do
        stat <- Posix.getFileStatus path
        let perm = case role of
                    Owner ->  uPerm stat
                    Group ->  gPerm stat
                    Others -> oPerm stat
            fperm = f perm
        return $ setMode role fperm mode

    -- current permissions
    uPerm stat =
        Permissions
            (Posix.fileMode stat .&. Posix.ownerReadMode
                == Posix.ownerReadMode)
            (Posix.fileMode stat .&. Posix.ownerWriteMode
                == Posix.ownerWriteMode)
            (Posix.fileMode stat .&. Posix.ownerExecuteMode
                == Posix.ownerExecuteMode)

    gPerm stat =
        Permissions
            (Posix.fileMode stat .&. Posix.groupReadMode
                == Posix.groupReadMode)
            (Posix.fileMode stat .&. Posix.groupWriteMode
                == Posix.groupWriteMode)
            (Posix.fileMode stat .&. Posix.groupExecuteMode
                == Posix.groupExecuteMode)

    oPerm stat =
        Permissions
            (Posix.fileMode stat .&. Posix.otherReadMode
                == Posix.otherReadMode)
            (Posix.fileMode stat .&. Posix.otherWriteMode
                == Posix.otherWriteMode)
            (Posix.fileMode stat .&. Posix.otherExecuteMode
                == Posix.otherExecuteMode)
