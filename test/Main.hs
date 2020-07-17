module Main(main) where

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Streamly.Coreutils.Uniq

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

opt :: UniqOptions
opt = defaultUniqOptions { skipFields = 1
                         , skipChar = 1
                         }

splitOnNewLine
    :: (IsStream t, Monad m, MonadIO m)
    => t m Char -> t m String
splitOnNewLine = S.splitOnSuffix (== '\n') FL.toList


gen :: (IsStream t, Monad m) => Char -> Int -> t m Char
gen c n = S.unfoldr step (0, 1)
    where
        step (i, flg) | i == n = Just ('\n', (0, flg))
        step (0, 0) = Just ('x', (1, 1))
        step (0, 1) = Just ('y', (1, 0))
        step (1, flg) = Just (' ', (2, flg))
        step (i, flg) = Just (c, (i + 1, flg))


main :: IO ()
main = do
    let comp = compareUsingOptions opt
    S.drain $ S.mapM print
        $ splitOnNewLine
        $ S.take 100
        $ gen 'a' 6
    S.drain $ S.mapM print
        $ getRepetition comp
        $ splitOnNewLine
        $ S.take 100
        $ gen 'a' 6
    S.drain $ S.mapM print
        $ uniq opt
        $ splitOnNewLine
        $ S.take 100
        $ gen 'a' 6
    S.drain $ S.mapM print
        $ uniqResultToString
        $ uniq opt
        $ splitOnNewLine
        $ S.take 100
        $ gen 'a' 6
