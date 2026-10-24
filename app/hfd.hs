module Main (main) where

import Data.Maybe (fromMaybe)
import System.IO (stdout)

import Options.Applicative
    ( Parser
    , ParserInfo
    , briefDesc
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , optional
    , option
    , progDesc
    , strArgument
    , (<**>)
    )
import qualified Options.Applicative as OA
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.Find
    ( FindOptions
    , Traversal(..)
    , batched
    , findByteChunked
    , followSymlinks
    , maxDepth
    , maxResults
    , minDepth
    , ordered
    , parallel
    , reverseSiblings
    , traversal
    )

data Config = Config
    { cfgTraversal :: Traversal
    , cfgBatched :: Bool
    , cfgReverseSiblings :: Bool
    , cfgParallel :: Bool
    , cfgOrdered :: Bool
    , cfgFollowSymlinks :: Bool
    , cfgMinDepth :: Maybe Int
    , cfgMaxDepth :: Maybe Int
    , cfgMaxResults :: Maybe Int
    , cfgRoot :: FilePath
    }

parseNonNegativeInt :: String -> Either String Int
parseNonNegativeInt str =
    case reads str of
        [(n, "")]
            | n >= 0 -> Right n
            | otherwise -> Left "N must not be negative"
        _ -> Left "N must be an integer"

traversalParser :: Parser Traversal
traversalParser =
    OA.flag DepthFirst BreadthFirst
        (long "bfs" <> help "Breadth-first traversal (default depth-first)")

countOption :: String -> String -> Parser (Maybe Int)
countOption name desc =
    optional
        (option (OA.eitherReader parseNonNegativeInt)
            (long name <> metavar "N" <> help desc))

configParser :: Parser Config
configParser =
    Config
        <$> traversalParser
        <*> OA.switch
                (long "batched"
                    <> help "Read a directory to the end before any other")
        <*> OA.switch
                (long "reverse-siblings"
                    <> help "With --batched, read subdirectories last first")
        <*> OA.switch
                (long "parallel" <> help "Read directories concurrently")
        <*> OA.switch
                (long "ordered"
                    <> help "With --parallel, keep the serial order")
        <*> OA.switch
                (OA.short 'L'
                    <> long "follow"
                    <> help "Descend into symlinked directories")
        <*> countOption "min-depth" "Do not list entries above depth N"
        <*> countOption "max-depth" "Do not descend below depth N"
        <*> countOption "max-results" "Stop after emitting N results"
        <*> (fromMaybe "." <$> optional
                (strArgument
                    (metavar "PATH" <> help "Root path to search")))

parserInfo :: ParserInfo Config
parserInfo =
    info
        (configParser <**> helper)
        (fullDesc
            <> briefDesc
            <> progDesc "A basic fd-like driver for Streamly.Coreutils.Find."
            <> header "hfd")

findOptions :: Config -> FindOptions -> FindOptions
findOptions cfg =
      maybe id maxResults (cfgMaxResults cfg)
    . maybe id maxDepth (cfgMaxDepth cfg)
    . maybe id minDepth (cfgMinDepth cfg)
    . followSymlinks (cfgFollowSymlinks cfg)
    . ordered (cfgOrdered cfg)
    . parallel (cfgParallel cfg)
    . reverseSiblings (cfgReverseSiblings cfg)
    . batched (cfgBatched cfg)
    . traversal (cfgTraversal cfg)

main :: IO ()
main = do
    cfg <- execParser parserInfo
    path <- Path.fromString (cfgRoot cfg)
    Stream.fold (Handle.writeChunks stdout)
        $ findByteChunked (findOptions cfg) path
