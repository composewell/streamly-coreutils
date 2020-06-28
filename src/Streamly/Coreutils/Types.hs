module Streamly.Coreutils.Types (
      defaultOptsDict
    , OptsDict (..)
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


data OptsDict = OptsDict {
                  verbose :: Bool
                }


defaultOptsDict :: OptsDict
defaultOptsDict = OptsDict True         -- set to False later

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

