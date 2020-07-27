module Streamly.Coreutils.Tsort
    ( vertices
    --, dfs
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
    S.indexed
    $ S.uniq
    $ S.concatMap (\(x, y) -> S.fromList [x, y]) strm


--buildAdjList
--    :: (IsStream t, Monad m)
--    => t m (a, a)
--    -- ^ stream of edges
--    -> t m (Int, a)
--    -- ^ Map from @a@ to @Int@
--    -> t m (t m Int)
--    -- ^ initial adj list
--    -> t m (t m Int)
--    -- ^ adj list now


dfs
    :: (IsStream t, Monad m)
    => Int
    -- ^ root node to start dfs
    -> SerialT m (SerialT m Int)
    -- ^ adj list
    -> SerialT m Bool
    -- ^ visited
    -> t m Int
    -- ^ parent
    -> t m Int
    -- ^ the stack
    -> m (SerialT m Bool, t m Int, t m Int)
    -- ^ (parent, stack)
dfs root adj vis par stck = do
    strm <- (S.!!) adj root
    case strm of
        Just nbd -> do
            maybeTuple <- S.last
               $ S.scanlM'
               (\(visi, parent, stack) v -> dfs v adj
                  (markVisited visi v) (setParent parent v root) (S.cons v stck))
               (vis, par, stck)
               $ S.filterM (unVisited vis) nbd
            case maybeTuple of
                   Just ans -> return ans
        Nothing -> return (vis, par, stck)

    where

    unVisited
        :: Monad m
        => SerialT m Bool
        -> Int
        -> m Bool
    unVisited vis n = do
        ele <- (S.!!) vis n
        case ele of
            Just v -> return $ not v
            Nothing -> return False

    markVisited
       :: (IsStream t, Monad m)
       => t m Bool
       -> Int
       -> t m Bool
    markVisited strm vtx = do
        S.map (\(_, v) -> v)
        $ S.map (\(i, v) -> if i == vtx then (i, not v) else (i, v))
        $ S.indexed strm

    setParent
        :: (IsStream t, Monad m)
        => t m Int
        -> Int
        -> Int
        -> t m Int
    setParent strm u par = do
        S.map (\(_, v) -> v)
        $ S.map (\(i, v) -> if i == u then (i, par) else (i, v))
        $ S.indexed strm
