module Streamly.Coreutils.Types (
      defaultCpOptions
    , CpOptions (..)
    , defaultUniqOptions
    , UniqOptions (..)
    , defaultCatOptions
    , CatOptions (..)
    , defaultEchoOptions
    , EchoOptions (..)
    , someFileToFP
    , someDirToFP
    , module Path
    , module Path.Posix
   )
where

import Path
import Path.Posix
      (Path
      , File
      , Dir
      , Abs
      , Rel
      , parseAbsFile
      , parseRelFile
      , parseSomeFile
      , fromRelFile
      , fromAbsFile
      , fromRelFile
      , parseAbsDir
      , (</>)
      , parseRelDir
      , fromRelDir
      , fromAbsDir)

-------------------------------------------------------------------------------
-- Record for options used with cp
-------------------------------------------------------------------------------


data CpOptions = CpOptions {
                  cpVerbose :: Bool
                }


defaultCpOptions :: CpOptions
defaultCpOptions = CpOptions True        -- set to False later


-------------------------------------------------------------------------------
-- Record for options used with uniq
-------------------------------------------------------------------------------


data UniqOptions = UniqOptions {
                     uniqVerbose :: Bool
                   , count :: Bool
--                     repeated :: Bool,      -- display only duplicate lines once for each group
--                     duplicate :: Bool,     -- print all duplicate lines
--                     skipFields :: Int,
--                     ignoreCase :: Bool,
--                     unique :: Bool,        -- print only unique lines
--                     zeroTerminated :: Bool,
--                     checkChar :: Int,
                  }

defaultUniqOptions :: UniqOptions
defaultUniqOptions = UniqOptions True True      -- add other options later


-------------------------------------------------------------------------------
-- Record for options used with cat
-------------------------------------------------------------------------------

data CatOptions = CatOptions {
                     showAll :: Bool
                   , numberNonEmptyLines :: Bool
                   , showEnds :: Bool
                   , numberAllLines :: Bool
                   , suppressRepeatedEmpty :: Bool
                  }

defaultCatOptions :: CatOptions
defaultCatOptions = CatOptions True True True True True


-------------------------------------------------------------------------------
-- Record for options used with cat
-------------------------------------------------------------------------------

data EchoOptions = EchoOptions {
                       trailingLine :: Bool
                     , interpretBackSlash :: Bool
                   }

defaultEchoOptions :: EchoOptions
defaultEchoOptions = EchoOptions True False

-------------------------------------------------------------------------------
-- helper functions for converting from SomeBase t to FilePath
-------------------------------------------------------------------------------


someFileToFP :: SomeBase File -> FilePath
someFileToFP some =
                  case some of
                     Abs x -> fromAbsFile x
                     Rel x -> fromRelFile x


someDirToFP :: SomeBase Dir -> FilePath
someDirToFP some =
                   case some of
                     Abs x -> fromAbsDir x
                     Rel x -> fromRelDir x

