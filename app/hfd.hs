module Main (main) where

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
    , findByteChunked
    , maxResults
    , parallelInterleaved
    , parallelOrdered
    , parallelUnordered
    , serialAppend
    , serialBfs
    , serialBfsRev
    , serialDfs
    , serialInterleaved
    )

data Config = Config
    { cfgTraversal :: FindOptions -> FindOptions
    , cfgRoot :: FilePath
    , cfgMaxResults :: Maybe Int
    }

data Traversal
    = TraversalDfs
    | TraversalBfs
    | TraversalBfsRev
    | TraversalAppend
    | TraversalInterleaved
    | TraversalParallel
    | TraversalParallelInterleaved
    | TraversalParallelOrdered

toTraversalConfig :: Traversal -> FindOptions -> FindOptions
toTraversalConfig traversal =
    case traversal of
        TraversalDfs -> serialDfs
        TraversalBfs -> serialBfs
        TraversalBfsRev -> serialBfsRev
        TraversalAppend -> serialAppend
        TraversalInterleaved -> serialInterleaved
        TraversalParallel -> parallelUnordered
        TraversalParallelInterleaved -> parallelInterleaved
        TraversalParallelOrdered -> parallelOrdered

mkConfig :: Traversal -> Maybe Int -> Maybe FilePath -> Config
mkConfig traversal mMaxResults mPath =
    Config
        { cfgTraversal = toTraversalConfig traversal
        , cfgRoot = maybe "." id mPath
        , cfgMaxResults = mMaxResults
        }

traversalParser :: Parser Traversal
traversalParser =
    OA.flag' TraversalBfs
        (long "bfs" <> help "Breadth-first traversal")
    OA.<|> OA.flag' TraversalBfsRev
        (long "bfs-rev" <> help "Reverse breadth-first traversal")
    OA.<|> OA.flag' TraversalAppend
        (long "append" <> help "Serial append traversal")
    OA.<|> OA.flag' TraversalInterleaved
        (long "interleaved" <> help "Serial interleaved traversal")
    OA.<|> OA.flag' TraversalParallel
        (long "parallel" <> help "Parallel unordered traversal")
    OA.<|> OA.flag' TraversalParallelInterleaved
        (long "parallel-interleaved" <> help "Parallel interleaved traversal")
    OA.<|> OA.flag' TraversalParallelOrdered
        (long "parallel-ordered" <> help "Parallel ordered traversal")
    OA.<|> OA.flag' TraversalDfs
        (long "dfs" <> help "Depth-first traversal")
    OA.<|> OA.pure TraversalDfs

configParser :: Parser Config
configParser =
    mkConfig
        <$> traversalParser
        <*> optional
                (option (OA.eitherReader parsePositiveInt)
                    (long "max-results"
                        <> metavar "N"
                        <> help "Stop after emitting N results"))
        <*> optional
                (strArgument
                    (metavar "PATH" <> help "Root path to search"))

parserInfo :: ParserInfo Config
parserInfo =
    info
        (configParser <**> helper)
        (fullDesc
            <> briefDesc
            <> progDesc "A basic fd-like driver for Streamly.Coreutils.Find."
            <> header "hfd")

main :: IO ()
main = do
    cfg <- execParser parserInfo
    path <- Path.fromString (cfgRoot cfg)
    let applyConfig opts =
            maybe id maxResults (cfgMaxResults cfg) $
                cfgTraversal cfg opts
    Stream.fold (Handle.writeChunks stdout)
        $ findByteChunked applyConfig path

parsePositiveInt :: String -> Either String Int
parsePositiveInt str =
    case reads str of
        [(n, "")]
            | n > 0 -> Right n
            | otherwise -> Left "N must be positive"
        _ -> Left "N must be an integer"
