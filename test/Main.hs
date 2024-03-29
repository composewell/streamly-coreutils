module Main
    (main)
where

import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as FL

import Streamly.Coreutils.Uniq

import Control.Monad.IO.Class (MonadIO)
import Streamly.Data.Stream (Stream)

opt :: UniqOptions
opt = defaultUniqOptions {skipFields = 1, skipChar = 1}

splitOnNewLine :: MonadIO m => Stream m Char -> Stream m String
splitOnNewLine = S.foldMany (FL.takeEndBy (== '\n') FL.toList)

gen :: Monad m => Char -> Int -> Stream m Char
gen c n = S.unfoldr step (0, True)
  where
    step (i, flg)
        | i == n = Just ('\n', (0, flg))
    step (0, False) = Just ('x', (1, True))
    step (0, True) = Just ('y', (1, False))
    step (1, flg) = Just (' ', (2, flg))
    step (i, flg) = Just (c, (i + 1, flg))

-- TODO: rm tests. Compare rm with GNU rm for the following cases:
--
-- * rm
-- * rm (force Force)
-- * rm (force Nuke)
--
-- * rm (recursive On)
-- * rm (force Force . recursive On)
-- * rm (force Nuke . recursive On)
--
-- * File/dir with
--      * rwx
--      * rw-
--      * r-x
--      * r--
--      * ---
--  * Dir cases
--      * Empty dir
--      * Dir with file (different permission modes on dir and file)
--      * Dir with subdir (different permission modes on dir and subdir)
--  * File parent dirs not having permissions
--  * File owned by someone else

main :: IO ()
main = do
    let comp = compareUsingOptions opt
    S.fold FL.drain $ S.mapM print $ splitOnNewLine $ S.take 100 $ gen 'a' 6
    S.fold FL.drain $
        S.mapM print $
        getRepetition comp $ splitOnNewLine $ S.take 100 $ gen 'a' 6
    S.fold FL.drain $ S.mapM print $ uniq opt $ splitOnNewLine $ S.take 100 $ gen 'a' 6
    S.fold FL.drain $
        S.mapM print $
        uniqResultToString $ uniq opt $ splitOnNewLine $ S.take 100 $ gen 'a' 6
