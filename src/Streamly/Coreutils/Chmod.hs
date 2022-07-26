{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Streamly.Coreutils.Chmod
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- change file mode bits.

module Streamly.Coreutils.Chmod
    ( chmod
    , perm
    )
where

import Data.Bits ((.|.), Bits ((.&.), complement))
import Streamly.Coreutils.StringQ
import qualified System.Posix as Posix

modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
modifyBit False b m = m .&. complement b
modifyBit True  b m = m .|. b

chmodWith :: UserType -> Permissions -> FilePath -> IO ()
chmodWith utype (Permissions r w e) path = do
    case utype of
        Owner -> setOwnerPermissions
        Group -> setGroupPermissions
        Others -> setOthersPermissions
        All -> setAllPermissions

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

        setAllPermissions = do
            setOwnerPermissions
            setGroupPermissions
            setOthersPermissions

-- | Supports only override permissions bits
-- >> chmod [perm|a=rwx|] "a.txt"
--
chmod :: UserTypePerm -> FilePath -> IO ()
chmod pat = chmodWith (utype pat) (permssions pat)
