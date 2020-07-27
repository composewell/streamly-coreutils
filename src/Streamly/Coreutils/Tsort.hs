module Streamly.Coreutils.Tsort
    (
    ,
    ,
    ,
    ,
    ,
    ,
    ,
    ) where

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Data.Char (isSpace, toLower)

import Streamly

vertices
    :: (IsStream t, Monad m, Eq a)
    => t m (a, a)
    -- ^ Edges
    -> t m (Int, a)
    -- ^ Map each vertex to a unique integer
vertices strm =
    S.indexed $
    S.uniq $
    S.concatMap (\(x, y) -> S.fromList [x, y]) strm


buildAdjList
    :: (IsStream t, Monad m)
    => t m (a, a)
    -- ^ stream of edges
    -> t m (Int, a)
    -- ^ Map from @a@ to @Int@
    -> t m (t m Int)
    -- ^ initial adj list
    -> t m (t m Int)
    -- ^ adj list now


dfs
    :: (IsStream t, Monad m)
    => Int
    -- ^ root node to start dfs
    -> t m (t m Int)
    -- ^ adj list
    -> t m Bool
    -- ^ visited
    -> t m Int
    -- ^ parent
    -> t m Int
    -- ^ the stack
    -> m ()
dfs root adj vis par stck = do
    strm <- S.(!!) adj root
    case strm of
        Just nbd -> S.filterM isVisited nbd
        Nothing -> return ()

    where

    isVisited
        :: (IsStream t, Monad m)
        -> t m Bool
        -> Int
        -> m Bool
    isVisited vis n = do
        ele <- S.(!!) vis n
        case ele of
            Just v -> return v
            Nothing -> return True
