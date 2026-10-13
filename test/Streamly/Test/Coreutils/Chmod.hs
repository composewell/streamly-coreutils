-- |
--
-- Module      : Streamly.Test.Coreutils.Chmod
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Streamly.Coreutils.Chmod'. The expected modes are those given by
-- GNU @chmod@ for the command shown in each test name.
--
-- The sticky bit is only set on directories: macOS and FreeBSD do not allow
-- a normal user to set it on a regular file.

module Streamly.Test.Coreutils.Chmod (main) where

import Control.Exception (bracket, try, SomeException)
import Control.Monad (forM_, void, when)
import Data.Bits ((.&.))
import Numeric (showOct)
import System.Directory
    ( createDirectory
    , createFileLink
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.PosixCompat.Files (fileMode, getFileStatus, setFileMode)
import System.PosixCompat.Types (FileMode)
import Test.Hspec

import Streamly.Coreutils.Chmod hiding (chmod)
import qualified Streamly.Coreutils.Chmod as Chmod
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

chmod :: (ChmodOptions -> ChmodOptions) -> (Mode -> Mode) -> FilePath -> IO ()
chmod f m path = Path.fromString path >>= Chmod.chmod f m

-- | Run a test inside a fresh temporary directory, cleaning up afterwards.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpRoot <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpRoot "chmod-test-")
        removePathForcibly'
        action
  where
    -- Best-effort cleanup; we do not want cleanup failures to mask test
    -- failures.
    removePathForcibly' p =
        void (try (removePathForcibly p) :: IO (Either SomeException ()))

shouldFail :: IO () -> IO ()
shouldFail action = do
    r <- try action :: IO (Either SomeException ())
    case r of
        Left _ -> return ()
        Right _ ->
            expectationFailure "Expected an exception but none was thrown"

-- | Permission and special bits of a path, in octal.
modeOf :: FilePath -> IO String
modeOf path = do
    m <- fileMode <$> getFileStatus path
    return $ showOct (m .&. 0o7777) ""

data Kind = File | Dir

-- | Set the mode of a path. Marks the test pending if the system does not let
-- us set that mode, e.g. the set-group-ID bit of a file whose group we are
-- not a member of. The first argument names the mode in the message.
setModeOrPending :: String -> FileMode -> FilePath -> IO Bool
setModeOrPending name m path = do
    setFileMode path m
    got <- modeOf path
    if got == showOct m ""
    then return True
    else do
        pendingWith $ "cannot set " ++ name ++ " mode " ++ showOct m ""
        return False

-- | Create a file or directory with the given mode. Marks the test pending if
-- the system does not let us set that mode.
createWithMode :: Kind -> FileMode -> FilePath -> IO Bool
createWithMode kind m path = do
    case kind of
        File -> writeFile path ""
        Dir -> createDirectory path
    setModeOrPending "initial" m path

-- | Create a file or directory with the initial mode, run chmod on it and
-- check the resulting mode.
--
-- The system may silently drop a bit that chmod sets, e.g. macOS drops the
-- set-group-ID bit when we are not a member of the group of the path, so
-- the test is marked pending if the expected mode cannot be set either.
check :: Kind -> FileMode -> (Mode -> Mode) -> FileMode -> Expectation
check kind initial mode expected = withTempDir $ \d -> do
    let path = d </> "target"
    okInitial <- createWithMode kind initial path
    okExpected <- setModeOrPending "expected" expected path
    when (okInitial && okExpected) $ do
        setFileMode path initial
        chmod id mode path
        modeOf path `shouldReturn` showOct expected ""

-- | A test case: shell command, kind, initial mode, mode, result.
type Case = (String, Kind, FileMode, Mode -> Mode, FileMode)

checkCases :: [Case] -> Spec
checkCases cases =
    forM_ cases $ \(name, kind, initial, mode, expected) ->
        it (name ++ " on " ++ kindName kind ++ " " ++ showOct initial ""
                ++ " gives " ++ showOct expected "") $
            check kind initial mode expected

    where

    kindName File = "file"
    kindName Dir = "dir"

-------------------------------------------------------------------------------
-- Cases
-------------------------------------------------------------------------------

-- | Add and remove each permission of each class. Sticky is checked on a
-- directory only.
addRemoveCases :: [Case]
addRemoveCases =
    concatMap rwxCases
        [ ("u", addOwner, removeOwner, 0o100)
        , ("g", addGroup, removeGroup, 0o010)
        , ("o", addOther, removeOther, 0o001)
        , ("a", addAll, removeAll, 0o111)
        ]
    ++
    [ ("chmod u+s", File, 0, addOwner [SetId], 0o4000)
    , ("chmod u-s", File, 0o6777, removeOwner [SetId], 0o2777)
    , ("chmod g+s", File, 0, addGroup [SetId], 0o2000)
    , ("chmod g-s", File, 0o6777, removeGroup [SetId], 0o4777)
    , ("chmod a+s", Dir, 0o755, addAll [SetId], 0o6755)
    , ("chmod a-s", Dir, 0o6755, removeAll [SetId], 0o755)
    , ("chmod o+t", Dir, 0o755, addOther [Sticky], 0o1755)
    , ("chmod o-t", Dir, 0o1755, removeOther [Sticky], 0o755)
    , ("chmod a+t", Dir, 0o755, addAll [Sticky], 0o1755)
    , ("chmod a-t", Dir, 0o1755, removeAll [Sticky], 0o755)
    -- "s" is ignored for other and "t" for owner and group, as in GNU.
    , ("chmod o+s", Dir, 0o755, addOther [SetId], 0o755)
    , ("chmod o-s", Dir, 0o6777, removeOther [SetId], 0o6777)
    , ("chmod u+t", Dir, 0o755, addOwner [Sticky], 0o755)
    , ("chmod g+t", Dir, 0o755, addGroup [Sticky], 0o755)
    , ("chmod u-t", Dir, 0o1777, removeOwner [Sticky], 0o1777)
    , ("chmod u+", File, 0o640, addOwner [], 0o640)
    , ("chmod u-", File, 0o640, removeOwner [], 0o640)
    , ("chmod u+rw", File, 0o200, addOwner [Read, Write], 0o600)
    , ("chmod u+r", File, 0o0, addOwner [Read, Read], 0o400)
    ]

    where

    -- "unit" is the execute bit of the class; read is 4 times and write 2
    -- times that.
    rwxCases (who, add, remove, unit) =
        concat
            [ [ ( "chmod " ++ who ++ "+" ++ letter
                , File, 0, add [perm], factor * unit)
              , ( "chmod " ++ who ++ "-" ++ letter
                , File, 0o6777, remove [perm], 0o6777 - factor * unit)
              ]
            | (letter, perm, factor) <-
                [("r", Read, 4), ("w", Write, 2), ("x", Exec, 1)]
            ]

-- | Numeric modes: set all three classes.
numericCases :: [Case]
numericCases =
    [ ("chmod 600", File, 0o755, rw_, 0o600)
    , ("chmod 600", File, 0o6755, rw_, 0o600)
    , ("chmod 0", File, 0o6777, setAll [], 0)
    , ("chmod 444", File, 0o644, setAll [Read], 0o444)
    , ("chmod 4755", File, 0o644
      , setOwner [Read, Write, Exec, SetId] . setGroup [Read, Exec]
            . setOther [Read, Exec]
      , 0o4755)
    -- GNU keeps the set-ID bits of a directory unless they are named.
    , ("chmod 600", Dir, 0o6755, rw_, 0o6600)
    , ("chmod 0", Dir, 0o7777, setAll [], 0o6000)
    , ("chmod 755", Dir, 0o7777
      , setOwner [Read, Write, Exec] . setGroup [Read, Exec]
            . setOther [Read, Exec]
      , 0o6755)
    , ("chmod 00700", Dir, 0o6755
      , setOwner [Read, Write, Exec] . setGroup [] . setOther []
            . removeAll [SetId]
      , 0o700)
    , ("chmod 02700", Dir, 0o4755
      , setOwner [Read, Write, Exec] . setGroup [SetId] . setOther []
            . removeOwner [SetId]
      , 0o2700)
    , ("chmod 1777", Dir, 0o755
      , setOwner [Read, Write, Exec] . setGroup [Read, Write, Exec]
            . setOther [Read, Write, Exec, Sticky]
      , 0o1777)
    ]

    where

    rw_ = setOwner [Read, Write] . setGroup [] . setOther []

-- | Assigning classes, like a symbolic GNU mode with "=".
assignCases :: [Case]
assignCases =
    [ ("chmod u=rw", File, 0o755, setOwner [Read, Write], 0o655)
    , ("chmod u=rw", File, 0o6755, setOwner [Read, Write], 0o2655)
    , ("chmod u=rw", Dir, 0o6755, setOwner [Read, Write], 0o6655)
    , ("chmod u=", File, 0o6777, setOwner [], 0o2077)
    , ("chmod u=", Dir, 0o7777, setOwner [], 0o7077)
    , ("chmod u=rwx", File, 0, setOwner [Exec, Write, Read], 0o700)
    , ("chmod u=rwxs", Dir, 0o7777, setOwner [Read, Write, Exec, SetId]
      , 0o7777)
    , ("chmod u=rs", File, 0o755, setOwner [Read, SetId], 0o4455)
    , ("chmod u=r", File, 0, setOwner [Read, Read], 0o400)
    , ("chmod g=r", File, 0o6777, setGroup [Read], 0o4747)
    , ("chmod g=r", Dir, 0o7777, setGroup [Read], 0o7747)
    , ("chmod g=wx", File, 0, setGroup [Write, Exec], 0o030)
    , ("chmod o=r", File, 0o6777, setOther [Read], 0o6774)
    -- GNU "o=" clears the sticky bit of a directory too.
    , ("chmod o=r", Dir, 0o7777, setOther [Read], 0o6774)
    , ("chmod o=", Dir, 0o7777, setOther [], 0o6770)
    , ("chmod o=rwxt", Dir, 0o7777, setOther [Read, Write, Exec, Sticky]
      , 0o7777)
    , ("chmod a=r", File, 0o6777, setAll [Read], 0o444)
    , ("chmod a=r", Dir, 0o7777, setAll [Read], 0o6444)
    , ("chmod a=rwx", Dir, 0o7777, setAll [Read, Write, Exec], 0o6777)
    , ("chmod a=", File, 0o6777, setAll [], 0)
    , ("chmod a=", Dir, 0o7777, setAll [], 0o6000)
    , ("chmod u=rw,go=", File, 0o6777
      , setOwner [Read, Write] . setGroup [] . setOther [], 0o600)
    , ("chmod u=rw,go=", Dir, 0o7777
      , setOwner [Read, Write] . setGroup [] . setOther [], 0o6600)
    , ("chmod u=rw,g=rx", File, 0o6777
      , setGroup [Read, Exec] . setOwner [Read, Write], 0o657)
    , ("chmod ug=rw", Dir, 0o7777
      , setOwner [Read, Write] . setGroup [Read, Write], 0o7667)
    ]

-- | Mixed and ordered modifiers.
symbolicCases :: [Case]
symbolicCases =
    [ ("chmod u+x,g-w", File, 0o664, addOwner [Exec] . removeGroup [Write]
      , 0o744)
    , ("chmod u-s", Dir, 0o6777, removeOwner [SetId], 0o2777)
    , ("chmod g-s", Dir, 0o6777, removeGroup [SetId], 0o4777)
    , ("chmod u+s", Dir, 0o755, addOwner [SetId], 0o4755)
    -- Modifiers apply right to left: the leftmost one wins.
    , ("chmod u=r,u+s", File, 0o755, addOwner [SetId] . setOwner [Read]
      , 0o4455)
    , ("chmod u+s,u=r", File, 0o755, setOwner [Read] . addOwner [SetId]
      , 0o455)
    , ("chmod a=,u+r", File, 0o777, addOwner [Read] . setAll [], 0o400)
    , ("chmod go-w,o+r", File, 0o622
      , addOther [Read] . removeGroup [Write] . removeOther [Write], 0o604)
    , ("chmod (no change)", File, 0o6751, id, 0o6751)
    , ("chmod (no change)", Dir, 0o7751, id, 0o7751)
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "add and remove" $ checkCases addRemoveCases
    describe "numeric modes" $ checkCases numericCases
    describe "assign a class" $ checkCases assignCases
    describe "symbolic modes" $ checkCases symbolicCases

    describe "modeFrom" $ do
        it "chmod --reference=REF copies the mode of a file" $
            withTempDir $ \d -> do
                ok1 <- createWithMode File 0o640 (d </> "ref")
                ok2 <- createWithMode File 0o6777 (d </> "a")
                when (ok1 && ok2) $ do
                    ref <- Path.fromString (d </> "ref")
                    chmod (modeFrom ref) id (d </> "a")
                    modeOf (d </> "a") `shouldReturn` "640"
                    modeOf (d </> "ref") `shouldReturn` "640"

        it "chmod --reference=REF clears the set-ID bits of a directory" $
            withTempDir $ \d -> do
                ok1 <- createWithMode File 0o644 (d </> "ref")
                ok2 <- createWithMode Dir 0o7777 (d </> "a")
                when (ok1 && ok2) $ do
                    ref <- Path.fromString (d </> "ref")
                    chmod (modeFrom ref) id (d </> "a")
                    modeOf (d </> "a") `shouldReturn` "644"

        it "applies the modifiers over the reference mode" $
            withTempDir $ \d -> do
                ok1 <- createWithMode File 0o640 (d </> "ref")
                ok2 <- createWithMode File 0o777 (d </> "a")
                when (ok1 && ok2) $ do
                    ref <- Path.fromString (d </> "ref")
                    chmod (modeFrom ref) (addGroup [Write]) (d </> "a")
                    modeOf (d </> "a") `shouldReturn` "660"

        it "fails if the reference does not exist" $ withTempDir $ \d -> do
            writeFile (d </> "a") ""
            ref <- Path.fromString (d </> "missing")
            shouldFail $ chmod (modeFrom ref) id (d </> "a")

    describe "errors and symlinks" $ do
        it "fails on a missing file" $ withTempDir $ \d ->
            shouldFail $ chmod id (setOwner [Read]) (d </> "missing")

        it "follows a symlink and changes the target" $ withTempDir $ \d -> do
            ok <- createWithMode File 0o600 (d </> "target")
            when ok $ do
                createFileLink "target" (d </> "link")
                chmod id (setOther [Read]) (d </> "link")
                modeOf (d </> "target") `shouldReturn` "604"

        it "fails on a dangling symlink" $ withTempDir $ \d -> do
            createFileLink "missing" (d </> "link")
            shouldFail $ chmod id (setOwner [Read]) (d </> "link")
