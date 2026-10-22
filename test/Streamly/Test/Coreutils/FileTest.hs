{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.FileTest
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.FileTest'. Behavior mirrors GNU @test@ unless
-- noted otherwise in the module documentation.

module Streamly.Test.Coreutils.FileTest (main) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (void)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import System.Directory
    ( createDirectory
    , createDirectoryLink
    , createFileLink
    , getTemporaryDirectory
    , removePathForcibly
    , setAccessTime
    , setModificationTime
    )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.PosixCompat.Files (createLink)
#if !defined(CABAL_OS_WINDOWS)
import System.Posix.Files (createNamedPipe, setFileMode)
import System.Posix.Types (FileMode)
import System.Posix.User (getEffectiveUserID)
#else
import System.Directory (getPermissions, setOwnerWritable, setPermissions)
#endif
import Test.Hspec

import Prelude hiding (and, or)
import Streamly.Coreutils.FileTest hiding (test, testl)
import qualified Streamly.Coreutils.FileTest as FileTest
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

test :: FilePath -> FileTest -> IO Bool
test path p = Path.fromString path >>= \x -> FileTest.test x p

testl :: FilePath -> FileTest -> IO Bool
testl path p = Path.fromString path >>= \x -> FileTest.testl x p

-- | A predicate on another path, for the predicates that take one.
withPath :: (Path.Path -> FileTest) -> FilePath -> FileTest
withPath f path = f (Path.fromString_ path)

-- | Run a test inside a fresh temporary directory, cleaning up afterwards.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpRoot <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpRoot "filetest-test-")
        removePathForcibly'
        action

    where

    -- Best-effort cleanup; we do not want cleanup failures to mask test
    -- failures.
    removePathForcibly' p =
        void (try (removePathForcibly p) :: IO (Either SomeException ()))

shouldFail :: IO Bool -> IO ()
shouldFail action = do
    r <- try action :: IO (Either SomeException Bool)
    case r of
        Left _ -> return ()
        Right _ ->
            expectationFailure "Expected an exception but none was thrown"

-- | Create the fixtures used by most tests in directory @d@:
--
-- * @file@: regular file, 5 bytes
-- * @empty@: regular file, 0 bytes
-- * @dir@: directory
-- * @fileLink -> file@, @dirLink -> dir@, @dangling -> missing@
--
-- @missing@ does not exist.
withFixtures :: (FilePath -> IO ()) -> IO ()
withFixtures action = withTempDir $ \d -> do
    writeFile (d </> "file") "hello"
    writeFile (d </> "empty") ""
    createDirectory (d </> "dir")
    createFileLink "file" (d </> "fileLink")
    createDirectoryLink "dir" (d </> "dirLink")
    createFileLink "missing" (d </> "dangling")
    action d

-- | Check the result of a predicate on each of the named fixtures.
shouldGive :: (FilePath -> FileTest -> IO Bool)
    -> FilePath -> FileTest -> [(FilePath, Bool)] -> IO ()
shouldGive run d p expected =
    mapM (\(name, _) -> (,) name <$> run (d </> name) p) expected
        `shouldReturn` expected

-- | A time in the past, 2001-09-09.
oldPosixTime :: POSIXTime
oldPosixTime = 1000000000

oldTime :: UTCTime
oldTime = posixSecondsToUTCTime oldPosixTime

-- | Set the access and modification times of a file to 'oldTime'.
setOld :: FilePath -> IO ()
setOld path = do
    setAccessTime path oldTime
    setModificationTime path oldTime

#if !defined(CABAL_OS_WINDOWS)
-- | A path under a regular file. A stat of this path fails with ENOTDIR.
notDirPath :: FilePath -> FilePath
notDirPath d = d </> "file" </> "x"

-- | Raises an exception if evaluated.
raises :: FilePath -> FileTest
raises d = withPath largerThanFile (d </> "missing")

isRoot :: IO Bool
isRoot = (== 0) <$> getEffectiveUserID

-- | Skip the test when run as root; root bypasses permission checks.
needsNonRoot :: IO () -> IO ()
needsNonRoot action = do
    root <- isRoot
    if root
    then pendingWith "Root bypasses permission checks"
    else action

-- | Run an action with the mode of a path set, then restore mode 700 so that
-- the path can be removed.
withMode :: FilePath -> FileMode -> IO () -> IO ()
withMode path mode action =
    bracket
        (setFileMode path mode)
        (\_ -> setFileMode path 0o700)
        (const action)
#endif

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

existenceTests :: Spec
existenceTests = do
    describe "doesItExist" $ do
        it "test follows symlinks" $ withFixtures $ \d ->
            shouldGive test d doesItExist
                [ ("file", True), ("dir", True), ("fileLink", True)
                , ("dirLink", True), ("dangling", False), ("missing", False)
                ]

        it "not_ doesItExist is True for a missing path, like test ! -e"
            $ withFixtures $ \d -> do
            shouldGive test d (not_ doesItExist)
                [ ("file", False), ("dangling", True), ("missing", True) ]
            shouldGive testl d (not_ doesItExist)
                [ ("file", False), ("dangling", False), ("missing", True) ]

        it "testl tests the symlink itself" $ withFixtures $ \d ->
            shouldGive testl d doesItExist
                [ ("file", True), ("dir", True), ("fileLink", True)
                , ("dirLink", True), ("dangling", True), ("missing", False)
                ]

    describe "doesItExist `and_` isDir" $ do
        it "test is True for a directory or a symlink to one"
            $ withFixtures $ \d ->
            shouldGive test d (doesItExist `and_` isDir)
                [ ("file", False), ("dir", True), ("fileLink", False)
                , ("dirLink", True), ("dangling", False), ("missing", False)
                ]

        it "testl is True only for a directory" $ withFixtures $ \d ->
            shouldGive testl d (doesItExist `and_` isDir)
                [ ("file", False), ("dir", True), ("fileLink", False)
                , ("dirLink", False), ("dangling", False), ("missing", False)
                ]

    describe "doesItExist `and_` isFile" $
        it "test is True for a file or a symlink to one" $ withFixtures $ \d ->
            shouldGive test d (doesItExist `and_` isFile)
                [ ("file", True), ("empty", True), ("dir", False)
                , ("fileLink", True), ("dirLink", False)
                , ("dangling", False), ("missing", False)
                ]

-- | The predicates that check access for the current process.
accessPredicates :: [FileTest]
accessPredicates = [isReadable, isWritable, isExecutable]

missingPathTests :: Spec
missingPathTests = do
    describe "a predicate on a missing path" $ do
        it "is False, like GNU test" $ withFixtures $ \d -> do
            let missing = d </> "missing"
            test missing isDir `shouldReturn` False
            test missing isFile `shouldReturn` False
            test missing (size (> 0)) `shouldReturn` False
            testl missing isDir `shouldReturn` False
            test missing isSymLink `shouldReturn` False
            testl missing isSymLink `shouldReturn` False
            test (d </> "dangling") isFile `shouldReturn` False
            mapM (test missing) accessPredicates
                `shouldReturn` [False, False, False]
            mapM (testl missing) accessPredicates
                `shouldReturn` [False, False, False]
            mapM (test (d </> "dangling")) accessPredicates
                `shouldReturn` [False, False, False]
            test missing isOwnedByCurrentUser `shouldReturn` False
            test missing (withPath sameFileAs (d </> "file"))
                `shouldReturn` False

        it "access predicates are False for a dangling symlink under testl"
            $ withFixtures $ \d ->
                mapM (testl (d </> "dangling")) accessPredicates
                    `shouldReturn` [False, False, False]

        it "is True under not_, like test ! -d" $ withFixtures $ \d -> do
            test (d </> "missing") (not_ isDir) `shouldReturn` True
            testl (d </> "missing") (not_ isDir) `shouldReturn` True

        it "gives the same result in either order in and_"
            $ withFixtures $ \d -> do
            test (d </> "missing") (isDir `and_` doesItExist)
                `shouldReturn` False
            test (d </> "missing") (doesItExist `and_` isDir)
                `shouldReturn` False

    describe "comparison with a missing file" $ do
        it "raises for size and time comparisons" $ withFixtures $ \d -> do
            shouldFail $ test (d </> "file") (withPath largerThanFile
                (d </> "missing"))
            shouldFail $ test (d </> "file") (withPath newerThanFile
                (d </> "missing"))

#if !defined(CABAL_OS_WINDOWS)
    describe "a path under a regular file" $ do
        it "does not exist, like test -e file/x" $ withFixtures $ \d -> do
            test (notDirPath d) doesItExist `shouldReturn` False
            testl (notDirPath d) doesItExist `shouldReturn` False
            test (notDirPath d) (not_ doesItExist) `shouldReturn` True
            test (notDirPath d) (doesItExist `and_` isDir)
                `shouldReturn` False

        it "is False for other predicates" $ withFixtures $ \d -> do
            test (notDirPath d) isDir `shouldReturn` False
            testl (notDirPath d) isDir `shouldReturn` False

        it "is not the same file as an existing file" $ withFixtures $ \d ->
            test (d </> "file") (withPath sameFileAs (notDirPath d))
                `shouldReturn` False

    describe "a symlink loop" $
        it "test raises" $ withFixtures $ \d -> do
            createFileLink "loop" (d </> "loop")
            shouldFail $ test (d </> "loop") doesItExist

    describe "a parent directory without search permission" $
        it "raises" $ needsNonRoot $ withFixtures $ \d ->
            withMode (d </> "dir") 0o000 $ do
                shouldFail $ test (d </> "dir" </> "x") doesItExist
                shouldFail $ test (d </> "dir" </> "x") isFile
#endif

fileTypeTests :: Spec
fileTypeTests = do
    describe "isDir" $ do
        it "test follows symlinks" $ withFixtures $ \d ->
            shouldGive test d isDir
                [ ("file", False), ("dir", True)
                , ("fileLink", False), ("dirLink", True)
                ]

        it "testl does not follow symlinks" $ withFixtures $ \d ->
            shouldGive testl d isDir
                [ ("dir", True), ("dirLink", False) ]

    describe "isFile" $ do
        it "test follows symlinks" $ withFixtures $ \d ->
            shouldGive test d isFile
                [ ("file", True), ("empty", True), ("dir", False)
                , ("fileLink", True), ("dirLink", False)
                ]

        it "testl does not follow symlinks" $ withFixtures $ \d ->
            shouldGive testl d isFile
                [ ("file", True), ("fileLink", False) ]

    describe "isSymLink" $ do
        it "test tests the path itself" $ withFixtures $ \d ->
            shouldGive test d isSymLink
                [ ("file", False), ("dir", False), ("fileLink", True)
                , ("dirLink", True), ("dangling", True)
                ]

        it "testl tests the path itself" $ withFixtures $ \d ->
            shouldGive testl d isSymLink
                [ ("file", False), ("dir", False), ("fileLink", True)
                , ("dirLink", True), ("dangling", True)
                ]

        it "test isSymLink `and_` isDir is True for a symlink to a directory"
            $ withFixtures $ \d ->
            shouldGive test d (isSymLink `and_` isDir)
                [ ("dir", False), ("fileLink", False), ("dirLink", True) ]

        it "test isSymLink `and_` isFile is True for a symlink to a file"
            $ withFixtures $ \d ->
            shouldGive test d (isSymLink `and_` isFile)
                [ ("file", False), ("fileLink", True), ("dirLink", False) ]

    describe "special files" $ do
        it "are False for regular files and directories"
            $ withFixtures $ \d -> do
            let special = or [isCharDevice, isBlockDevice, isPipe, isSocket]
            shouldGive test d special
                [ ("file", False), ("dir", False), ("fileLink", False) ]

#if !defined(CABAL_OS_WINDOWS)
        it "isPipe is True for a named pipe" $ withTempDir $ \d -> do
            createNamedPipe (d </> "fifo") 0o600
            test (d </> "fifo") isPipe `shouldReturn` True
            test (d </> "fifo") isFile `shouldReturn` False

        it "isCharDevice is True for /dev/null" $ do
            test "/dev/null" isCharDevice `shouldReturn` True
            test "/dev/null" isBlockDevice `shouldReturn` False
#endif

-- and / or here are the FileTest combinators, not the Prelude functions.
{- HLINT ignore "Use &&" -}
{- HLINT ignore "Use ||" -}
combinatorTests :: Spec
combinatorTests = do
    describe "true and false" $
        it "ignore the path" $ withFixtures $ \d -> do
            test (d </> "file") true `shouldReturn` True
            test (d </> "file") false `shouldReturn` False

    describe "not_" $
        it "negates a predicate" $ withFixtures $ \d ->
            shouldGive test d (not_ isDir)
                [ ("file", True), ("dir", False) ]

    describe "and_ and or_" $ do
        it "and_ is True only if both are True" $ withFixtures $ \d -> do
            test (d </> "file") (true `and_` true) `shouldReturn` True
            test (d </> "file") (true `and_` false) `shouldReturn` False
            test (d </> "file") (false `and_` true) `shouldReturn` False
            test (d </> "file") (isFile `and_` size (== 5)) `shouldReturn` True
            test (d </> "file") (isFile `and_` size (== 4)) `shouldReturn` False

        it "or_ is True if either is True" $ withFixtures $ \d -> do
            test (d </> "file") (false `or_` true) `shouldReturn` True
            test (d </> "file") (true `or_` false) `shouldReturn` True
            test (d </> "file") (false `or_` false) `shouldReturn` False
            test (d </> "file") (isDir `or_` isFile) `shouldReturn` True

        it "not_ doesItExist `or_` isDir is True for missing or a directory"
            $ withFixtures $ \d ->
            shouldGive test d (not_ doesItExist `or_` isDir)
                [ ("file", False), ("dir", True), ("missing", True) ]

    describe "and and or" $ do
        it "and of an empty list is True" $ withFixtures $ \d ->
            test (d </> "file") (and []) `shouldReturn` True

        it "or of an empty list is False" $ withFixtures $ \d ->
            test (d </> "file") (or []) `shouldReturn` False

        it "and is True only if all are True" $ withFixtures $ \d -> do
            test (d </> "file") (and [isFile, size (== 5), true])
                `shouldReturn` True
            test (d </> "file") (and [isFile, isDir]) `shouldReturn` False

        it "or is True if any is True" $ withFixtures $ \d -> do
            test (d </> "file") (or [isDir, isSymLink, isFile])
                `shouldReturn` True
            test (d </> "file") (or [isDir, isSymLink]) `shouldReturn` False

#if !defined(CABAL_OS_WINDOWS)
    describe "evaluation order" $ do
        it "the raising predicate raises when evaluated"
            $ withFixtures $ \d ->
            shouldFail $ test (d </> "file") (raises d)

        it "and_ does not evaluate the second predicate after False"
            $ withFixtures $ \d -> do
            test (d </> "file") (false `and_` raises d) `shouldReturn` False
            shouldFail $ test (d </> "file") (raises d `and_` false)

        it "or_ does not evaluate the second predicate after True"
            $ withFixtures $ \d -> do
            test (d </> "file") (true `or_` raises d) `shouldReturn` True
            shouldFail $ test (d </> "file") (raises d `or_` true)

        it "and stops at the first False" $ withFixtures $ \d ->
            test (d </> "file") (and [true, false, raises d])
                `shouldReturn` False

        it "or stops at the first True" $ withFixtures $ \d ->
            test (d </> "file") (or [false, true, raises d])
                `shouldReturn` True
#endif

sizeTests :: Spec
sizeTests = do
    describe "size" $ do
        it "tests the size in bytes" $ withFixtures $ \d -> do
            shouldGive test d (size (== 5)) [ ("file", True), ("empty", False) ]
            shouldGive test d (size (== 0)) [ ("file", False), ("empty", True) ]

        it "test uses the size of the symlink target" $ withFixtures $ \d ->
            test (d </> "fileLink") (size (== 5)) `shouldReturn` True

#if !defined(CABAL_OS_WINDOWS)
        it "testl uses the size of the symlink, the length of its target"
            $ withFixtures $ \d ->
            testl (d </> "fileLink") (size (== 4)) `shouldReturn` True
#endif

    describe "size comparison with a file" $ do
        it "sizeComparedTo applies the relation to both sizes"
            $ withFixtures $ \d -> do
            test (d </> "file")
                (withPath (`sizeComparedTo` (>)) (d </> "empty"))
                `shouldReturn` True
            test (d </> "file")
                (withPath (`sizeComparedTo` (<)) (d </> "empty"))
                `shouldReturn` False

        it "largerThanFile, smallerThanFile, sameSizeAs"
            $ withFixtures $ \d -> do
            let file = d </> "file"
                empty = d </> "empty"
            test file (withPath largerThanFile empty) `shouldReturn` True
            test empty (withPath largerThanFile file) `shouldReturn` False
            test empty (withPath smallerThanFile file) `shouldReturn` True
            test file (withPath smallerThanFile empty) `shouldReturn` False
            test file (withPath sameSizeAs file) `shouldReturn` True
            test file (withPath sameSizeAs empty) `shouldReturn` False

        it "dereferences a symlink given as the other file"
            $ withFixtures $ \d ->
            test (d </> "file") (withPath sameSizeAs (d </> "fileLink"))
                `shouldReturn` True

timeTests :: Spec
timeTests = do
    describe "time units" $
        it "convert to seconds" $ do
            seconds 1 `shouldBe` 1
            minutes 1 `shouldBe` 60
            hours 1 `shouldBe` 3600
            days 1 `shouldBe` 86400
            (days 1 + hours 1 + minutes 1 + seconds 1) `shouldBe` 90061

    describe "timestamps" $ do
        it "modifyTime and accessTime test the timestamps"
            $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "file") (modifyTime (== oldPosixTime))
                `shouldReturn` True
            test (d </> "file") (accessTime (== oldPosixTime))
                `shouldReturn` True
            test (d </> "empty") (modifyTime (> oldPosixTime))
                `shouldReturn` True

        it "metadataChangeTime is the time of the last change"
            $ withFixtures $ \d -> do
            setOld (d </> "file")
            -- Setting the times changes the metadata.
            test (d </> "file") (metadataChangeTime (> oldPosixTime))
                `shouldReturn` True

        it "test uses the times of the symlink target" $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "fileLink") (modifyTime (== oldPosixTime))
                `shouldReturn` True

    describe "ages" $ do
        it "modifyAge and modifiedWithin test the modification age"
            $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "file") (modifyAge (> days 365)) `shouldReturn` True
            test (d </> "file") (modifiedWithin (minutes 10))
                `shouldReturn` False
            test (d </> "empty") (modifiedWithin (minutes 10))
                `shouldReturn` True
            test (d </> "empty") (not_ (modifiedWithin (minutes 10)))
                `shouldReturn` False

        it "accessAge tests the access age" $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "file") (accessAge (> days 365)) `shouldReturn` True

        it "metadataAge tests the metadata change age" $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "file") (metadataAge (< minutes 10))
                `shouldReturn` True

    describe "time comparison with a file" $ do
        it "olderThanFile and newerThanFile compare modification times"
            $ withFixtures $ \d -> do
            setOld (d </> "file")
            let file = d </> "file"
                empty = d </> "empty"
            test file (withPath olderThanFile empty) `shouldReturn` True
            test empty (withPath olderThanFile file) `shouldReturn` False
            test empty (withPath newerThanFile file) `shouldReturn` True
            test file (withPath newerThanFile empty) `shouldReturn` False
            test file (withPath olderThanFile file) `shouldReturn` False

        it "modifyTimeComparedTo applies the relation to both times"
            $ withFixtures $ \d ->
            test (d </> "file")
                (withPath (`modifyTimeComparedTo` (==)) (d </> "file"))
                `shouldReturn` True

        it "accessTimeComparedTo applies the relation to both access times"
            $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "file")
                (withPath (`accessTimeComparedTo` (<)) (d </> "empty"))
                `shouldReturn` True

        it "dereferences a symlink given as the other file"
            $ withFixtures $ \d -> do
            setOld (d </> "file")
            test (d </> "empty") (withPath newerThanFile (d </> "fileLink"))
                `shouldReturn` True

sameFileTests :: Spec
sameFileTests =
    describe "sameFileAs" $ do
        it "is True for the same path and a hard link" $ withFixtures $ \d -> do
            createLink (d </> "file") (d </> "hardLink")
            test (d </> "file") (withPath sameFileAs (d </> "file"))
                `shouldReturn` True
            test (d </> "file") (withPath sameFileAs (d </> "hardLink"))
                `shouldReturn` True

        it "is False for a different file" $ withFixtures $ \d ->
            test (d </> "file") (withPath sameFileAs (d </> "empty"))
                `shouldReturn` False

        it "dereferences symlinks" $ withFixtures $ \d -> do
            test (d </> "file") (withPath sameFileAs (d </> "fileLink"))
                `shouldReturn` True
            test (d </> "fileLink") (withPath sameFileAs (d </> "file"))
                `shouldReturn` True

        it "testl tests the symlink itself, not its target"
            $ withFixtures $ \d ->
            testl (d </> "fileLink") (withPath sameFileAs (d </> "file"))
                `shouldReturn` False

        it "is False if the other file is missing, like GNU test -ef"
            $ withFixtures $ \d ->
            test (d </> "file") (withPath sameFileAs (d </> "missing"))
                `shouldReturn` False

permissionTests :: Spec
permissionTests = do
    -- Not tested: isOwnedByCurrentUser on a symlink checks the owner of the
    -- target under test and of the symlink itself under testl. The fixtures
    -- are all created by the current user, so a symlink and its target have
    -- the same owner and the two cases give the same result. Testing this
    -- needs a symlink or target owned by another user.
    describe "ownership" $
        it "isOwnedByCurrentUser is True for a file we created"
            $ withFixtures $ \d ->
            test (d </> "file") isOwnedByCurrentUser `shouldReturn` True

#if !defined(CABAL_OS_WINDOWS)
    describe "mode bits" $ do
        it "test each permission bit" $ withFixtures $ \d ->
            withMode (d </> "file") 0o754 $
            mapM (test (d </> "file"))
                [ hasOwnerRead, hasOwnerWrite, hasOwnerExec
                , hasGroupRead, hasGroupWrite, hasGroupExec
                , hasOtherRead, hasOtherWrite, hasOtherExec
                ]
                `shouldReturn`
                [ True, True, True
                , True, False, True
                , True, False, False
                ]

        it "are all False for mode 000" $ withFixtures $ \d ->
            withMode (d </> "file") 0o000 $
            test (d </> "file")
                (or [ hasOwnerRead, hasOwnerWrite, hasOwnerExec
                    , hasGroupRead, hasGroupWrite, hasGroupExec
                    , hasOtherRead, hasOtherWrite, hasOtherExec
                    ])
                `shouldReturn` False

        it "test the special bits" $ withFixtures $ \d -> do
            let specials = [hasSetUid, hasSetGid, hasStickyBit]
            mapM (test (d </> "file")) specials
                `shouldReturn` [False, False, False]
            withMode (d </> "file") 0o4700 $
                mapM (test (d </> "file")) specials
                    `shouldReturn` [True, False, False]
            withMode (d </> "dir") 0o1700 $
                mapM (test (d </> "dir")) specials
                    `shouldReturn` [False, False, True]

    describe "access by mode" $ do
        it "uses the owner bits for a file we own" $ withFixtures $ \d -> do
            withMode (d </> "file") 0o400 $
                mapM (test (d </> "file"))
                    [isReadableByMode, isWritableByMode, isExecutableByMode]
                    `shouldReturn` [True, False, False]
            withMode (d </> "file") 0o300 $
                mapM (test (d </> "file"))
                    [isReadableByMode, isWritableByMode, isExecutableByMode]
                    `shouldReturn` [False, True, True]

        it "ignores the group and other bits for a file we own"
            $ withFixtures $ \d ->
            withMode (d </> "file") 0o077 $
                mapM (test (d </> "file"))
                    [isReadableByMode, isWritableByMode, isExecutableByMode]
                    `shouldReturn` [False, False, False]

    describe "real access" $ do
        it "is True when the owner has all permissions" $ withFixtures $ \d ->
            withMode (d </> "file") 0o700 $
                mapM (test (d </> "file"))
                    [isReadable, isWritable, isExecutable]
                    `shouldReturn` [True, True, True]

        it "is False without permissions" $ needsNonRoot $ withFixtures $ \d ->
            withMode (d </> "file") 0o000 $
                mapM (test (d </> "file"))
                    [isReadable, isWritable, isExecutable]
                    `shouldReturn` [False, False, False]

        it "follows the owner bits" $ needsNonRoot $ withFixtures $ \d ->
            withMode (d </> "file") 0o400 $
                mapM (test (d </> "file"))
                    [isReadable, isWritable, isExecutable]
                    `shouldReturn` [True, False, False]
#else
    describe "read-only attribute" $
        it "hasOwnerWrite and isWritableByMode are False for a read-only file"
            $ withFixtures $ \d -> do
            let file = d </> "file"
            test file (hasOwnerWrite `and_` isWritableByMode)
                `shouldReturn` True
            getPermissions file >>= setPermissions file . setOwnerWritable False
            test file (hasOwnerWrite `or_` isWritableByMode)
                `shouldReturn` False
            getPermissions file >>= setPermissions file . setOwnerWritable True
#endif

main :: IO ()
main = hspec $ do
    existenceTests
    missingPathTests
    fileTypeTests
    combinatorTests
    sizeTests
    timeTests
    sameFileTests
    permissionTests
