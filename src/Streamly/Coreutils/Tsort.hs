module Streamly.Coreutils.Tsort
    ( vertices
    , buildAdjList
    , dfs
    ) where

import qualified Streamly.Prelude as S

import Streamly
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.Stream.StreamK (adapt)


vertices
    :: (IsStream t, Eq a)
    => t IO (a, a)
    -- ^ Edges
    -> t IO (Int, a)
    -- ^ Map each vertex to a unique integer
vertices strm =
    S.indexed
    $ S.uniq
    $ S.concatMap (\(x, y) -> S.fromList [x, y]) strm


buildAdjList
    :: (IsStream t, Eq a)
    => t IO (a, a)
    -- ^ stream of edges
    -> t IO (Int, a)
    -- ^ map from @a@ to @Int@
    -> t IO (t IO Int)
    -- ^ initial adj list
    -> t IO (t IO Int)
    -- ^ adj list now
buildAdjList edges vtx adj = do
    let maybeStrm =
           ( unsafePerformIO
           $ S.last
           $ adapt
           $ S.scanl' (insertPair vtx) adj edges
           )
    case maybeStrm of
        Just strm -> strm
        Nothing -> S.nil

    where

    insertPair
        :: (IsStream t, Eq a)
        => t IO (Int, a)
        -> t IO (t IO Int)
        -> (a, a)
        -> t IO (t IO Int)
    insertPair vtxMap adjl (xa, xb) = do
        let indexA = unsafePerformIO $ getInt vtxMap xa
        let indexB = unsafePerformIO $ getInt vtxMap xb
        let maybeStrm = unsafePerformIO $ (S.!!) (adapt adjl) indexA
        case maybeStrm of
            Just strm -> modifyNeighbours indexA (S.cons indexB strm) adjl
            Nothing -> modifyNeighbours indexA (S.yield indexB) adjl



    getInt
        :: (IsStream t, Eq a)
        => t IO (Int, a)
        -> a
        -> IO Int
    getInt vtxMap ele = do
        maybeIndex <- S.findIndex (\(_, v) -> v == ele) $ adapt vtxMap
        case maybeIndex of
            Just idx -> return idx
            Nothing -> return (-1) -- won't ever equal any other index in a stream

    modifyNeighbours
       :: IsStream t
       => Int
       -> t IO Int
       -> t IO (t IO Int)
       -> t IO (t IO Int)
    modifyNeighbours idx newNbd adjStrm =
        S.map (\(_, v) -> v)
        $ S.map (\(i, v) -> do
            if i == idx
            then (i, newNbd)
            else (i, v))
        $ S.indexed adjStrm


-- | dfs
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
    -> m (t m Bool, t m Int, t m Int)
    -- ^ (visited, parent, stack)
dfs root adj vis par stck = do
    strm <- (S.!!) (adapt adj) root
    case strm of
        Just nbd -> do
            maybeTuple <- S.last
               $ adapt
               $ S.scanlM'
               (\(visi, parent, stack) v ->
                   dfs v adj (markVisited visi v)
                   (setParent parent v root) (S.cons v stack))
               (vis, par, stck)
               $ S.filterM (unVisited vis) nbd
            case maybeTuple of
                   Just ans -> return ans
                   _ -> return (S.nil, S.nil, S.nil)
        Nothing -> return (vis, par, stck)

    where

    unVisited
        :: (IsStream t, Monad m)
        => t m Bool
        -> Int
        -> m Bool
    unVisited visStream n = do
        ele <- (S.!!) (adapt visStream) n
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
    setParent strm u parent = do
        S.map (\(_, v) -> v)
        $ S.map (\(i, v) -> if i == u then (i, parent) else (i, v))
        $ S.indexed strm
