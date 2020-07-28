module Streamly.Coreutils.Tsort
    ( vertices
    , buildAdjList
    , dfs
    ) where

import qualified Streamly.Prelude as S
--import qualified Streamly.Internal.Data.Fold as FL

--import Data.Char (isSpace, toLower)
import System.IO.Unsafe (unsafePerformIO)

import Streamly

vertices
    :: Eq a
    => SerialT IO (a, a)
    -- ^ Edges
    -> SerialT IO (Int, a)
    -- ^ Map each vertex to a unique integer
vertices strm =
    S.indexed
    $ S.uniq
    $ S.concatMap (\(x, y) -> S.fromList [x, y]) strm


buildAdjList
    :: Eq a
    => SerialT IO (a, a)
    -- ^ stream of edges
    -> SerialT IO (Int, a)
    -- ^ Map from @a@ to @Int@
    -> SerialT IO (SerialT IO Int)
    -- ^ initial adj list
    -> SerialT IO (SerialT IO Int)
    -- ^ adj list now
buildAdjList edges vtx adj = do
    let maybeStrm = unsafePerformIO $ S.last $ S.scanl' (insertPair vtx) adj edges
    case maybeStrm of
         Just strm -> strm
         Nothing -> S.nil

    where

    insertPair
        :: Eq a
        => SerialT IO (Int, a)
        -> SerialT IO (SerialT IO Int)
        -> (a, a)
        -> SerialT IO (SerialT IO Int)
    insertPair vtxMap adjl (xa, xb) = do
        let indexA = unsafePerformIO $ getInt vtxMap xa
        let indexB = unsafePerformIO $ getInt vtxMap xb
        let maybeStrm = unsafePerformIO $ (S.!!) adjl indexA
        case maybeStrm of
            Just strm -> modifyNeighbours indexA (S.cons indexB $ strm) adjl
            Nothing -> modifyNeighbours indexA (S.yield indexB) adjl


    getInt
        :: Eq a
        => SerialT IO (Int, a)
        -> a
        -> IO Int
    getInt vtxMap ele = do
        maybeIndex <- S.findIndex (\(_, v) -> v == ele) vtxMap
        case maybeIndex of
            Just idx -> return idx
            Nothing -> return (-1) -- won't ever equal any other index in a stream

    modifyNeighbours
       :: Int
       -> SerialT IO Int
       -> SerialT IO (SerialT IO Int)
       -> SerialT IO (SerialT IO Int)
    modifyNeighbours idx newNbd adjStrm =
        S.map (\(_, v) -> v)
        $ S.map (\(i, v) -> do
                   if i == idx
                   then (i, newNbd)
                   else (i, v))
        $ S.indexed adjStrm


-- | dfs
dfs
    :: IsStream t
    => Int
    -- ^ root node to start dfs
    -> SerialT IO (SerialT IO Int)
    -- ^ adj list
    -> SerialT IO Bool
    -- ^ visited
    -> t IO Int
    -- ^ parent
    -> t IO Int
    -- ^ the stack
    -> IO (SerialT IO Bool, t IO Int, t IO Int)
    -- ^ (visited, parent, stack)
dfs root adj vis par stck = do
    strm <- (S.!!) adj root
    case strm of
        Just nbd -> do
            maybeTuple <- S.last
               $ S.scanlM'
               (\(visi, parent, stack) v -> dfs v adj
                  (markVisited visi v) (setParent parent v root) (S.cons v stack))
               (vis, par, stck)
               $ S.filterM (unVisited vis) nbd
            case maybeTuple of
                   Just ans -> return ans
                   _ -> return (S.nil, S.nil, S.nil)
        Nothing -> return (vis, par, stck)

    where

    unVisited
        :: Monad m
        => SerialT m Bool
        -> Int
        -> m Bool
    unVisited visStream n = do
        ele <- (S.!!) visStream n
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
