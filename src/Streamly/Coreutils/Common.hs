module Streamly.Coreutils.Common (
      someFileToFP
    , someDirToFP
    , module Path
    , module Path.Posix
   )
where

import Path
import Path.Posix
      ( Path
      , File
      , Dir
      , Abs
      , Rel
      , SomeBase (..)
      , parseAbsFile
      , parseRelFile
      , parseSomeFile
      , fromRelFile
      , fromAbsFile
      , fromRelDir
      , fromAbsDir
      , parseAbsDir
      , parseRelDir)


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
