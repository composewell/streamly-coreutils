{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Rm
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for 'Coreutils.Rm'. Behavior mirrors GNU @rm@ unless noted
-- otherwise in the module documentation.
--
-- = Test Structure
--
-- Each test group is named after the rm invocation being tested.  Within each
-- group, cases are organised by the shape of the filesystem target:
--
--   * Plain file with various permission modes
--   * Empty directory
--   * Directory containing a file
--   * Directory containing a subdirectory
--   * Symbolic links
--   * Parent directory permission restrictions
--   * Files owned by another user
--
-- = Notes on Permission Tests
--
-- Permission tests that require a different owning user are marked
-- @needsRoot@ and skipped unless the test suite is run as root.  Tests that
-- merely restrict parent-directory permissions are safe to run as a normal
-- user.
--
-- All temporary files and directories are created under @/tmp@ and cleaned up
-- after each test, even on failure, using 'bracket'.

module Streamly.Test.Coreutils.Rm (main) where

import Control.Exception (bracket, try, evaluate, SomeException)
import Control.Monad (void, when)
import System.Directory
    ( createDirectory
    , createDirectoryLink
    , createFileLink
    , doesPathExist
    , getTemporaryDirectory
    )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.PosixCompat.Files
    ( setFileMode
    , ownerReadMode
    , ownerWriteMode
    , ownerExecuteMode
    , nullFileMode
    , unionFileModes
    )
-- TODO
#if 0
#if !defined(CABAL_OS_WINDOWS)
import System.Posix.User (getRealUserID)
#endif
#endif
import System.Posix.Types (FileMode)
import Test.Hspec

import Coreutils.Rm hiding (rm)
import qualified Coreutils.Rm as Rm
import qualified Streamly.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

rm :: (RmOptions -> RmOptions) -> FilePath -> IO ()
rm f path = do
    pathP <- Path.fromString path
    Rm.rm f pathP

-- | Run a test inside a fresh temporary directory, cleaning up afterwards.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpRoot <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpRoot "rm-test-")
        removePathForcibly'
        action
  where
    -- Best-effort cleanup; we do not want cleanup failures to mask test
    -- failures.
    removePathForcibly' p =
        void (try (rm (withForce FullForce . recursive True) p)
            :: IO (Either SomeException ()))

-- | Create a regular file with the given octal permission mode.
createFileWithMode :: FilePath -> FileMode -> IO ()
createFileWithMode path mode = do
    writeFile path ""
    setFileMode path mode

-- | Create a directory with the given octal permission mode.
createDirWithMode :: FilePath -> FileMode -> IO ()
createDirWithMode path mode = do
    createDirectory path
    setFileMode path mode

-- | Assert that a path no longer exists (file, dir, or link).
shouldNotExist :: FilePath -> IO ()
shouldNotExist path = doesPathExist path `shouldReturn` False

-- | Assert that a path still exists.
shouldStillExist :: FilePath -> IO ()
shouldStillExist path = doesPathExist path `shouldReturn` True

-- | Assert that an IO action throws any exception.
shouldThrow' :: IO () -> IO ()
shouldThrow' action = do
    result <- try (evaluate =<< action) :: IO (Either SomeException ())
    case result of
        Left _  -> return ()
        Right _ -> expectationFailure "Expected an exception but none was thrown"

-- TODO
#if 0
#if !defined(CABAL_OS_WINDOWS)
-- | Check whether we are running as root.
isRoot :: IO Bool
isRoot = (== 0) <$> getRealUserID

-- | Skip a test when not running as root.
needsRoot :: IO () -> IO ()
needsRoot action = do
    root <- isRoot
    if root
    then action
    else pendingWith "Requires root privileges"
#endif
#endif

-- Convenience mode constants
modeRWX, modeRW_, modeR_X, modeR__, mode___ :: FileMode
modeRWX = ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode
modeRW_ = ownerReadMode `unionFileModes` ownerWriteMode
modeR_X = ownerReadMode `unionFileModes` ownerExecuteMode
modeR__ = ownerReadMode
mode___ = nullFileMode

-- | On Unix, assert that an action fails due to directory permission
-- restrictions. On Windows, directory permission bits are not enforced;
-- the action is run but its result is ignored.
parentDirPermCheck :: IO () -> IO ()
parentDirPermCheck =
#if defined(CABAL_OS_WINDOWS)
    -- On Windows, we do not check parent dir permission as they are controlled
    -- by ACLs, not permission modes.
    id
#else
    shouldThrow'
#endif

-------------------------------------------------------------------------------
-- Test groups
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do

    -- -------------------------------------------------------------------------
    describe "rm (no options)" $ do

        -- Files
        describe "file" $ do

            it "rwx: removes file" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeRWX
                rm id f
                shouldNotExist f

            it "rw-: removes file" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeRW_
                rm id f
                shouldNotExist f

            it "r-x: errors (not writable by us)" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeR_X
                shouldThrow' $ rm id f

            it "r--: errors (not writable by us)" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeR__
                shouldThrow' $ rm id f

            it "---: errors (not writable by us)" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f mode___
                shouldThrow' $ rm id f

            it "missing: errors" $ withTempDir $ \d ->
                shouldThrow' $ rm id (d </> "missing")

        -- Symlinks
        describe "symlink to file" $ do

            it "removes the link, not the target" $ withTempDir $ \d -> do
                let target = d </> "target"
                    link   = d </> "link"
                createFileWithMode target modeRWX
                createFileLink target link
                rm id link
                shouldNotExist link
                shouldStillExist target

            it "removes broken symlink" $ withTempDir $ \d -> do
                let link = d </> "link"
                createFileLink (d </> "nonexistent") link
                rm id link
                shouldNotExist link

        describe "symlink to dir" $ do

            it "removes the link, not the target directory" $ withTempDir $ \d -> do
                let target = d </> "target"
                    link   = d </> "link"
                createDirectory target
                createDirectoryLink target link
                rm id link
                shouldNotExist link
                shouldStillExist target

        -- Directories
        describe "empty dir" $ do

            it "errors (is a directory)" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirectory sub
                shouldThrow' $ rm id sub

        describe "dir with file" $ do

            it "errors (is a directory)" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirectory sub
                writeFile (sub </> "file") ""
                shouldThrow' $ rm id sub

        -- Parent directory restrictions
        describe "parent dir permissions" $ do

            it "parent r-x: errors (cannot unlink without write on parent)" $
                withTempDir $ \d -> do
                    let sub  = d </> "sub"
                        f    = sub </> "file"
                    createDirectory sub
                    createFileWithMode f modeRWX
                    setFileMode sub modeR_X
                    parentDirPermCheck $ rm id f
                    -- restore so cleanup succeeds
                    setFileMode sub modeRWX

            it "parent ---: errors (cannot traverse or unlink)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                        f   = sub </> "file"
                    createDirectory sub
                    createFileWithMode f modeRWX
                    setFileMode sub mode___
                    parentDirPermCheck $ rm id f
                    setFileMode sub modeRWX

-- TODO
#if 0
#if !defined(CABAL_OS_WINDOWS)
        -- Ownership
        describe "file owned by another user" $ do

            it "rwx: removes if parent is writable (unlink depends on parent)" $
                needsRoot $ withTempDir $ \d -> do
                    -- As root we can create such files; the key point is
                    -- that unlink success depends on the parent, not the
                    -- file owner.
                    let f = d </> "file"
                    createFileWithMode f modeRWX
                    -- chown to nobody (uid 65534) would go here in a full
                    -- integration suite requiring user switching.
                    pendingWith "Requires user-switching support"
#endif
#endif

    -- -------------------------------------------------------------------------
    describe "rm (withForce Force)" $ do

        describe "file" $ do

            it "rwx: removes file" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeRWX
                rm (withForce Force) f
                shouldNotExist f

            it "rw-: removes file" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeRW_
                rm (withForce Force) f
                shouldNotExist f

            it "r-x: errors (Force skips pre-check but file still not writable)" $
                withTempDir $ \d -> do
                    let f = d </> "file"
                    createFileWithMode f modeR_X
                    rm (withForce Force) f
                    shouldNotExist f

            it "r--: removes (unlink depends on parent, not file mode)" $
                withTempDir $ \d -> do
                    let f = d </> "file"
                    createFileWithMode f modeR__
                    rm (withForce Force) f
                    shouldNotExist f

            it "---: removes (unlink depends on parent, not file mode)" $
                withTempDir $ \d -> do
                    let f = d </> "file"
                    createFileWithMode f mode___
                    rm (withForce Force) f
                    shouldNotExist f

            it "missing: silent no-op" $ withTempDir $ \d ->
                -- BUG: testl throws on a missing path instead of returning
                -- False, so rm errors rather than silently ignoring it.
                -- This should succeed per the Force semantics in the docs.
                rm (withForce Force) (d </> "missing")

        describe "empty dir" $ do

            it "errors (is a directory, not recursive)" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirectory sub
                shouldThrow' $ rm (withForce Force) sub

        it "parent dir r-x: errors (cannot unlink)" $
            withTempDir $ \d -> do
                let sub = d </> "sub"
                    f   = sub </> "file"
                createDirectory sub
                createFileWithMode f modeRWX
                setFileMode sub modeR_X
                parentDirPermCheck $ rm (withForce Force) f
                setFileMode sub modeRWX

    -- -------------------------------------------------------------------------
    describe "rm (withForce FullForce)" $ do

        describe "file" $ do

            it "rwx: removes file" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeRWX
                rm (withForce FullForce) f
                shouldNotExist f

            it "---: removes file (FullForce overrides)" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f mode___
                rm (withForce FullForce) f
                shouldNotExist f

            it "missing: silent no-op" $ withTempDir $ \d ->
                rm (withForce FullForce) (d </> "missing")

        describe "empty dir" $ do

            it "errors (not recursive even under FullForce)" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirectory sub
                shouldThrow' $ rm (withForce FullForce) sub

    -- -------------------------------------------------------------------------
    describe "rm (recursive True)" $ do

        describe "file" $ do

            it "rwx: removes file" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeRWX
                rm (recursive True) f
                shouldNotExist f

            it "r--: errors (not writable by us, NoForce)" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeR__
                shouldThrow' $ rm (recursive True) f

            it "missing: errors" $ withTempDir $ \d ->
                shouldThrow' $ rm (recursive True) (d </> "missing")

        describe "empty dir" $ do

            it "rwx: removes dir" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirWithMode sub modeRWX
                rm (recursive True) sub
                shouldNotExist sub

            it "r--: errors (write-protected dir)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                    createDirWithMode sub modeR__
                    shouldThrow' $ rm (recursive True) sub

            it "---: errors" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirWithMode sub mode___
                shouldThrow' $ rm (recursive True) sub

        describe "dir with file" $ do

            it "dir rwx, file rwx: removes all" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                    f   = sub </> "file"
                createDirWithMode sub modeRWX
                createFileWithMode f modeRWX
                rm (recursive True) sub
                shouldNotExist sub

            it "dir rwx, file r--: errors (file not writable by us)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                        f   = sub </> "file"
                    createDirWithMode sub modeRWX
                    createFileWithMode f modeR__
                    shouldThrow' $ rm (recursive True) sub

            it "dir rw-, file rwx: errors (cannot traverse dir, no execute bit)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                        f   = sub </> "file"
                    createDirWithMode sub modeRWX
                    createFileWithMode f modeRWX
                    setFileMode sub modeRW_
                    parentDirPermCheck $ rm (recursive True) sub
                    -- Restore permissions for cleanup; path may already be
                    -- gone on platforms that do not enforce execute bits
                    -- (e.g. Windows).
                    exists <- doesPathExist sub
                    when exists $ setFileMode sub modeRWX

            it "dir r-x, file rwx: errors (cannot rmdir, no write on dir)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                        f   = sub </> "file"
                    createDirWithMode sub modeRWX
                    createFileWithMode f modeRWX
                    setFileMode sub modeR_X
                    shouldThrow' $ rm (recursive True) sub
                    setFileMode sub modeRWX -- restore for cleanup

        describe "dir with subdir" $ do

            it "both rwx: removes entire tree" $ withTempDir $ \d -> do
                let sub    = d </> "sub"
                    subsub = sub </> "subsub"
                createDirWithMode sub modeRWX
                createDirWithMode subsub modeRWX
                rm (recursive True) sub
                shouldNotExist sub

            it "outer rwx, inner r--: errors (inner dir write-protected)" $
                withTempDir $ \d -> do
                    let sub    = d </> "sub"
                        subsub = sub </> "subsub"
                    createDirWithMode sub modeRWX
                    createDirWithMode subsub modeR__
                    shouldThrow' $ rm (recursive True) sub

            it "outer r-x, inner rwx: errors (cannot rmdir outer, no write)" $
                withTempDir $ \d -> do
                    let sub    = d </> "sub"
                        subsub = sub </> "subsub"
                    createDirWithMode sub modeRWX
                    createDirWithMode subsub modeRWX
                    setFileMode sub modeR_X -- restrict after subsub is created
                    shouldThrow' $ rm (recursive True) sub
                    setFileMode sub modeRWX -- restore for cleanup

        describe "symlink to dir" $ do

            it "removes the link only, not the target tree" $ withTempDir $ \d -> do
                let target = d </> "target"
                    link   = d </> "link"
                    f      = target </> "file"
                createDirectory target
                writeFile f ""
                createDirectoryLink target link
                rm (recursive True) link
                shouldNotExist link
                shouldStillExist target
                shouldStillExist f

    -- -------------------------------------------------------------------------
    describe "rm (withForce Force . recursive True)" $ do

        describe "file" $ do

            it "missing: silent no-op" $ withTempDir $ \d ->
                -- BUG: same as rm (withForce Force) missing case above.
                rm (withForce Force . recursive True) (d </> "missing")

            it "r--: removes (Force skips write pre-check)" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f modeR__
                rm (withForce Force . recursive True) f
                shouldNotExist f

        describe "empty dir rwx: removes" $ do
            it "" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirWithMode sub modeRWX
                rm (withForce Force . recursive True) sub
                shouldNotExist sub

        describe "dir with file" $ do

            it "dir rwx, file r--: removes all (Force skips write pre-check)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                        f   = sub </> "file"
                    createDirWithMode sub modeRWX
                    createFileWithMode f modeR__
                    rm (withForce Force . recursive True) sub
                    shouldNotExist sub

            it "dir r-x, file rwx: errors (cannot unlink, no write on dir)" $
                withTempDir $ \d -> do
                    let sub = d </> "sub"
                        f   = sub </> "file"
                    createDirWithMode sub modeRWX
                    createFileWithMode f modeRWX
                    setFileMode sub modeR_X -- restrict after file is created
                    parentDirPermCheck $ rm (withForce Force . recursive True) sub
                    -- Restore permissions for cleanup; path may already be
                    -- gone on platforms that do not enforce execute bits
                    -- (e.g. Windows).
                    exists <- doesPathExist sub
                    when exists $ setFileMode sub modeRWX

        describe "dir with subdir" $ do

            it "outer rwx, inner r--: removes all" $ withTempDir $ \d -> do
                let sub    = d </> "sub"
                    subsub = sub </> "subsub"
                createDirWithMode sub modeRWX
                createDirWithMode subsub modeR__
                rm (withForce Force . recursive True) sub
                shouldNotExist sub

    -- -------------------------------------------------------------------------
    describe "rm (withForce FullForce . recursive True)" $ do

        describe "file" $ do

            it "---: removes" $ withTempDir $ \d -> do
                let f = d </> "file"
                createFileWithMode f mode___
                rm (withForce FullForce . recursive True) f
                shouldNotExist f

            it "missing: silent no-op" $ withTempDir $ \d ->
                rm (withForce FullForce . recursive True) (d </> "missing")

        describe "empty dir" $ do

            it "---: removes (FullForce chmods)" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                createDirWithMode sub mode___
                rm (withForce FullForce . recursive True) sub
                shouldNotExist sub

        describe "dir with file" $ do

            it "dir ---, file ---: removes all" $ withTempDir $ \d -> do
                let sub = d </> "sub"
                    f   = sub </> "file"
                -- Temporarily open dir enough to create the file, then lock down.
                createDirWithMode sub modeRWX
                createFileWithMode f mode___
                setFileMode sub mode___
                rm (withForce FullForce . recursive True) sub
                shouldNotExist sub

        describe "dir with subdir" $ do

            it "outer ---, inner ---: removes entire tree" $ withTempDir $ \d -> do
                let sub    = d </> "sub"
                    subsub = sub </> "subsub"
                createDirWithMode sub modeRWX
                createDirWithMode subsub mode___
                setFileMode sub mode___
                rm (withForce FullForce . recursive True) sub
                shouldNotExist sub

        describe "symlink to dir" $ do

            it "removes the link only, not the target tree" $ withTempDir $ \d -> do
                let target = d </> "target"
                    link   = d </> "link"
                createDirectory target
                createDirectoryLink target link
                rm (withForce FullForce . recursive True) link
                shouldNotExist link
                shouldStillExist target
