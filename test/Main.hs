{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main
    (main)
where

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Streamly.Coreutils.Common (Switch(..))
import Streamly.Coreutils.Uniq
import Streamly.Coreutils.Rm
import Streamly.Coreutils.Chmod
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO)
import Streamly.Prelude (IsStream)
import Common
import System.Exit (exitFailure)

opt :: UniqOptions
opt = defaultUniqOptions {skipFields = 1, skipChar = 1}

splitOnNewLine :: (IsStream t, MonadIO m) => t m Char -> t m String
splitOnNewLine = S.splitOnSuffix (== '\n') FL.toList

gen :: (IsStream t, Monad m) => Char -> Int -> t m Char
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

rmDir :: FilePath
rmDir = "rmDir"

processResult :: Either SomeException s -> IO String
processResult res = return $
    case res of
        Left _ -> "Failed"
        Right _ -> "Passed"

testRmDefault :: IO String
testRmDefault =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "file.txt"
            path = dir </> file
        createFileWithParent file dir
        try (rm id path) >>= processResult

testRmDefaultFail :: IO String
testRmDefaultFail =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRO.txt"
            path = dir </> file
        createFileWithParent file dir
        chmod [perm|u=r|] path
        try (rm id path) >>= processResult

testRmNonExist :: IO String
testRmNonExist =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            fileNE = "fileNE.txt"
            pathNE = dir </> fileNE
        try (rm id pathNE) >>= processResult

-- make path read-only
-- chmod [perm|u=r|] "path"

testRmROFile :: IO String
testRmROFile =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRO.txt"
            path = dir </> file
        createFileWithParent file dir
        chmod [perm|u=r|] path
        try (rm id path) >>= processResult

testRmForceFile :: IO String
testRmForceFile =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRO.txt"
            path = dir </> file
        createFileWithParent file dir
        chmod [perm|u=r|] path
        try (rm (force Force) path) >>= processResult

testRmForceFailRO :: IO String
testRmForceFailRO =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRW.txt"
            path = dir </> file
        createFileWithParent file dir
        chmod [perm|u=r|] dir
        try (rm (force Force) path) >>= processResult

testRmForceFailNP :: IO String
testRmForceFailNP =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRW.txt"
            path = dir </> file
        createFileWithParent file dir
        chmod [perm|a=|] dir
        try (rm (force Force) path) >>= processResult

testRmNuke :: IO String
testRmNuke =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRW.txt"
        createFileWithParent file dir
        chmod [perm|u=r|] dir
        try (rm (force Nuke . recursive On) dir) >>= processResult

testRmNukeNoPerm :: IO String
testRmNukeNoPerm =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRW.txt"
        createFileWithParent file dir
        chmod [perm|a=|] dir
        try (rm (force Nuke . recursive On) dir) >>= processResult

testRmNukeRecOff :: IO String
testRmNukeRecOff =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir"
            file = "fileRW.txt"
        createFileWithParent file dir
        chmod [perm|a=|] dir
        try (rm (force Nuke . recursive Off) dir) >>= processResult

testRmRecursive ::(Rm -> Rm) -> IO String
testRmRecursive f =
    withSystemTempDirectory rmDir $ \fp -> do
        let dir = fp </> "testDir" </> "testDir"
            file = "fileRW.txt"
        createFileWithParent file dir
        try (rm f dir) >>= processResult

testRmRecursiveOn :: IO String
testRmRecursiveOn = testRmRecursive (recursive On)

testRmRecursiveOff :: IO String
testRmRecursiveOff = testRmRecursive (recursive Off)

describe :: String -> String -> IO String -> IO ()
describe tc expec m = do
    res <- m
    if res == expec
    then print (tc ++ "->" ++ "PASS")
    else print (tc ++ "->" ++ "FAILED") >> exitFailure

testRm :: IO ()
testRm = do
    describe "default" "Passed" testRmDefault
    describe "defaultFail" "Failed" testRmDefaultFail
    describe "nonExistant" "Failed" testRmNonExist
    describe "readOnly" "Failed" testRmROFile
    describe "forcePass" "Passed" testRmForceFile
    describe "forceFail ReadOnly" "Failed" testRmForceFailRO
    describe "forceFail None Permission" "Failed" testRmForceFailNP
    describe "recursiveOn" "Passed" testRmRecursiveOn
    describe "recursiveOff" "Failed" testRmRecursiveOff
    describe "nuke" "Passed" testRmNuke
    describe "nuke None Permission" "Passed" testRmNukeNoPerm
    describe "nuke Recursive Off" "Failed" testRmNukeRecOff

main :: IO ()
main = do
    testRm
    let comp = compareUsingOptions opt
    S.drain $ S.mapM print $ splitOnNewLine $ S.take 100 $ gen 'a' 6
    S.drain $
        S.mapM print $
        getRepetition comp $ splitOnNewLine $ S.take 100 $ gen 'a' 6
    S.drain $ S.mapM print $ uniq opt $ splitOnNewLine $ S.take 100 $ gen 'a' 6
    S.drain $
        S.mapM print $
        uniqResultToString $ uniq opt $ splitOnNewLine $ S.take 100 $ gen 'a' 6
