module Streamly.Coreutils.Common (
      someFileToFP
    , someDirToFP
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

-- |
-- Converts a type @SomeBase File@ to the type @FilePath@
--
-- @since 0.1.0.0
someFileToFP :: SomeBase File -> FilePath
someFileToFP some =
    case some of
        Abs x -> fromAbsFile x
        Rel x -> fromRelFile x


-- |
-- Converts a type @SomeBase Dir@ to the type @FilePath@
--
-- @since 0.1.0.0
someDirToFP :: SomeBase Dir -> FilePath
someDirToFP some =
    case some of
        Abs x -> fromAbsDir x
        Rel x -> fromRelDir x
