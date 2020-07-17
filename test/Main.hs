module Main(main) where

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.FileSystem.File as File

import Streamly.Coreutils.Uniq

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch)
import System.Directory (getHomeDirectory)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

home :: IO FilePath
home = getHomeDirectory

opt :: UniqOptions
opt = defaultUniqOptions { skipFields = 1
                         , skipChar = 1
                         , ignoreCase = True
                         }

charStrm
    :: (IsStream t, Monad m, MonadCatch m, MonadIO m)
    => FilePath -> t m Char
charStrm = Un.decodeLatin1 . File.toBytes

splitOnNewLine
    :: (IsStream t, Monad m, MonadIO m)
    => t m Char -> t m String
splitOnNewLine = S.splitOnSuffix (== '\n') FL.toList


main :: IO ()
main = do
    homeFP <- home
    let srcFP = homeFP ++ "/test-uniq.txt"
    let comp = compareUsingOptions opt
    S.drain $ S.mapM print
        $ getRepetition comp
        $ splitOnNewLine
        $ charStrm srcFP
    S.drain $ S.mapM print
        $ uniq opt
        $ splitOnNewLine
        $ charStrm srcFP
    S.drain $ S.mapM print
        $ uniqResultToString
        $ uniq opt
        $ splitOnNewLine
        $ charStrm srcFP
