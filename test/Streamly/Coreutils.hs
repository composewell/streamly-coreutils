module Main(main) where

import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unicode.Stream as Un
import qualified Streamly.Internal.FileSystem.File as File

import Streamly.Coreutils.Uniq
import System.Directory (getHomeDirectory)


opt :: UniqOptions
opt = defaultUniqOptions {skipFields = 1, skipChar = 1, duplicate = False, unique = False, repeated = True}

home :: IO FilePath
home = getHomeDirectory

main :: IO ()
main = do
         homeFP <- home
         let srcFP = homeFP ++ "/test-uniq.txt"
         S.drain $ S.mapM putChar (ignCase True $ Un.decodeLatin1 $ File.toBytes srcFP)
         S.drain $ S.mapM putChar (ignCase False $ Un.decodeLatin1 $ File.toBytes srcFP)
         S.drain $ S.mapM putStrLn (splitOnNewLine $ ignCase False $ Un.decodeLatin1 $ File.toBytes srcFP)
         S.drain $ S.mapM print (uniqCount opt $ Un.decodeLatin1 $ File.toBytes srcFP)
         S.drain $ S.mapM print (getRepetition opt $ splitOnNewLine $ ignCase True $ Un.decodeLatin1 $ File.toBytes srcFP)
         S.drain $ S.mapM print (filterStream opt $ getRepetition opt $ splitOnNewLine $ ignCase True $ Un.decodeLatin1 $ File.toBytes srcFP)
         S.drain $ S.mapM putStrLn (merge opt $ filterStream opt $ getRepetition opt $ splitOnNewLine $ ignCase True $ Un.decodeLatin1 $ File.toBytes srcFP)
