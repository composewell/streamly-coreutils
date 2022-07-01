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
    (  chmod
    )
where

import Data.Bits ((.|.), Bits ((.&.), complement))
import Data.Default.Class (Default(..))

import qualified System.Posix as Posix

data UserType = Owner | Group | Others deriving (Eq, Ord, Read, Show)

data Permissions = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  } deriving (Eq, Ord, Read, Show)

instance Default Permissions where
    def = Permissions
        { readable = False
        , writable = False
        , executable = False
        }

modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
modifyBit False b m = m .&. complement b
modifyBit True  b m = m .|. b

chmod :: UserType -> Permissions -> FilePath -> IO ()
chmod utype (Permissions r w e) path = do
    case utype of
        Owner -> do
            stat <- Posix.getFileStatus path
            Posix.setFileMode
                path
                ( modifyBit e Posix.ownerExecuteMode
                . modifyBit w Posix.ownerWriteMode
                . modifyBit r Posix.ownerReadMode
                . Posix.fileMode $ stat
                )
        Group ->  do
            stat <- Posix.getFileStatus path
            Posix.setFileMode
                path
                ( modifyBit e Posix.groupExecuteMode
                . modifyBit w Posix.groupWriteMode
                . modifyBit r Posix.groupReadMode
                . Posix.fileMode $ stat
                )
        Others -> do
            stat <- Posix.getFileStatus path
            Posix.setFileMode
                path
                ( modifyBit e Posix.otherExecuteMode
                . modifyBit w Posix.otherWriteMode
                . modifyBit r Posix.otherReadMode
                . Posix.fileMode $ stat
                )
