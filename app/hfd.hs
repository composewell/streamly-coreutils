{-# LANGUAGE CPP #-}

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
    , progDesc
    , strArgument
    , (<**>)
    )
import qualified Options.Applicative as OA
import qualified Streamly.Data.Stream.Prelude as Stream
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Stream as Stream (unfoldEachEndBy)
import qualified Streamly.Unicode.Stream as Unicode
#endif
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.FileSystem.Path as Path

import Streamly.Coreutils.Find
    ( FindOptions
    , parallelInterleaved
    , parallelOrdered
    , parallelUnordered
    , serialAppend
    , serialBfs
    , serialBfsRev
    , serialDfs
    , serialInterleaved
    )
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Streamly.Coreutils.Find (findByteChunked)
#else
import Streamly.Coreutils.Find (findChunked)
#endif

data Config = Config
    { cfgTraversal :: FindOptions -> FindOptions
    , cfgRoot :: FilePath
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

mkConfig :: Traversal -> Maybe FilePath -> Config
mkConfig traversal mPath =
    Config
        { cfgTraversal = toTraversalConfig traversal
        , cfgRoot = maybe "." id mPath
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

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#endif

main :: IO ()
main = do
    cfg <- execParser parserInfo
    path <- Path.fromString (cfgRoot cfg)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    Stream.fold (Handle.writeChunks stdout)
        $ findByteChunked (cfgTraversal cfg) path
#else
    Stream.fold (Handle.writeWith 32000 stdout)
        $ Unicode.encodeUtf8
        $ Unicode.decodeUtf16le
        $ Stream.unfoldEachEndBy 10 Array.reader
        $ fmap Path.toArray
        $ Stream.unfoldEach Unfold.fromList
        $ findChunked (cfgTraversal cfg) path
#endif
