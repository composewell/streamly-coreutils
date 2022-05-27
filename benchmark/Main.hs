module Main where

import Control.Monad.IO.Class (MonadIO)

import Gauge
import Streamly.Prelude (IsStream, SerialT)
import System.Random
import Streamly.Coreutils.Uniq

import qualified Streamly.Prelude as S

-- repeat (repeat str 1 n times then repeat str2 n times) l times
{-# INLINE alternateStrings #-}
alternateStrings ::
       (IsStream t, MonadIO m) => Int -> Int -> String -> String -> t m String
alternateStrings l n str1 str2 = S.unfoldr step (l, Left (n - 1))
  where
    step (0, _) = Nothing
    step (k, Left 0) = Just (str1, (k, Right (n - 1)))
    step (k, Left i) = Just (str1, (k, Left (i - 1)))
    step (k, Right 0) = Just (str2, (k - 1, Left (n - 1)))
    step (k, Right i) = Just (str2, (k, Right (i - 1)))

{-# INLINE benchUniq #-}
benchUniq ::
       Int
    -> String
    -> (SerialT IO a -> SerialT IO b)
    -> (Int -> SerialT IO a)
    -> Benchmark
benchUniq i name transform src =
    bench name $ nfIO $ randomRIO (i, i) >>= S.drain . transform . src

-- XXX Should be improved a lot
main :: IO ()
main = do
    str1 <- take 100 . randoms <$> newStdGen
    str2 <- take 100 . randoms <$> newStdGen
    let opt = defaultUniqOptions {skipFields = 12, skipChar = 10}
        comp = compareUsingOptions opt
        repeated i = S.replicate i str1 -- i = 100
        alternateX5 i = alternateStrings i 5 str1 str2 -- i = 10
        alternateX1 i = alternateStrings i 1 str1 str2 -- i = 50
    defaultMain
        [ bgroup "uniq"
            [ bgroup "repeated"
                [ benchUniq 100 "getRepetition" (getRepetition comp) repeated
                , benchUniq 100 "uniq" (uniq opt) repeated
                , benchUniq 100 "uniqResultToString" (uniqResultToString . uniq opt) repeated
                ]
            , bgroup "alternateX5"
                [ benchUniq 100 "getRepetition" (getRepetition comp) alternateX5
                , benchUniq 100 "uniq" (uniq opt) alternateX5
                , benchUniq 100 "uniqResultToString" (uniqResultToString . uniq opt) alternateX5
                ]
            , bgroup "alternateX1"
                [ benchUniq 100 "getRepetition" (getRepetition comp) alternateX1
                , benchUniq 100 "uniq" (uniq opt) alternateX1
                , benchUniq 100 "uniqResultToString" (uniqResultToString . uniq opt) alternateX1
                ]
            ]
        ]
