{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
--
-- Module      : Streamly.Test.Coreutils.Id
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC (POSIX only)
--
-- Tests for 'Streamly.Coreutils.Id'. The results for the current process are
-- compared with the output of the @id@ and @logname@ commands.

module Streamly.Test.Coreutils.Id (main) where

import Control.Exception (SomeException, try)
#if defined(CABAL_OS_DARWIN)
import Control.Monad (unless)
import Data.List (nub)
#else
import Data.List (nub, sort)
#endif
import Test.Hspec

import Streamly.Coreutils.Id

import qualified Streamly.Internal.System.Process as Process

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Output of a command, without the trailing newline.
command :: FilePath -> [String] -> IO String
command cmd args =
    reverse . dropWhile (== '\n') . reverse <$> Process.toString cmd args

idCommand :: [String] -> IO String
idCommand = command "id"

-- | A uid that no user has.
missingUid :: Int
missingUid = 1999999999

-- | Compare the groups of the process with the groups that @id@ prints,
-- ignoring order and duplicates. GNU @id@ prints the groups of the process.
-- macOS @id@ prints the groups of the user from the directory service, which
-- can include groups that the process does not have; on macOS, check only
-- that the process groups are among them, and report the extra groups as
-- pending.
shouldHaveGroups :: (Ord a, Show a) => [a] -> [a] -> Expectation
#if defined(CABAL_OS_DARWIN)
shouldHaveGroups actual expected = do
    filter (`notElem` expected) actual `shouldBe` []
    let extra = filter (`notElem` actual) expected
    unless (null extra) $
        pendingWith
            ("id lists groups the process does not have: " ++ show extra)
#else
shouldHaveGroups actual expected =
    sort (nub actual) `shouldBe` sort (nub expected)
#endif

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "idNum" $ do
        it "id is the effective uid (id -u)" $ do
            expected <- idCommand ["-u"]
            show <$> idNum id `shouldReturn` expected

        it "idReal is the real uid (id -ru)" $ do
            expected <- idCommand ["-ru"]
            show <$> idNum idReal `shouldReturn` expected

        it "idGroup is the effective gid (id -g)" $ do
            expected <- idCommand ["-g"]
            show <$> idNum idGroup `shouldReturn` expected

        it "idReal . idGroup is the real gid (id -rg)" $ do
            expected <- idCommand ["-rg"]
            show <$> idNum (idReal . idGroup) `shouldReturn` expected

        it "idEffective overrides idReal" $ do
            expected <- idNum id
            idNum (idEffective . idReal) `shouldReturn` expected

        it "idUser overrides idGroup" $ do
            expected <- idNum id
            idNum (idUser . idGroup) `shouldReturn` expected

    describe "idName" $ do
        it "id is the effective user name (id -un)" $ do
            expected <- idCommand ["-un"]
            idName id `shouldReturn` Just expected

        it "idGroup is the effective group name (id -gn)" $ do
            expected <- idCommand ["-gn"]
            idName idGroup `shouldReturn` Just expected

        it "idReal is the real user name (id -unr)" $ do
            expected <- idCommand ["-unr"]
            idName idReal `shouldReturn` Just expected

    describe "groupIds" $ do
        it "has the same groups as id -G" $ do
            expected <- map read . words <$> idCommand ["-G"]
            gs <- groupIds
            gs `shouldHaveGroups` (expected :: [Int])

        it "starts with the effective gid" $ do
            egid <- idNum idGroup
            take 1 <$> groupIds `shouldReturn` [egid]

        it "has no duplicates" $ do
            gs <- groupIds
            nub gs `shouldBe` gs

    describe "groupNames" $
        it "has the same groups as id -Gn" $ do
            expected <- words <$> idCommand ["-Gn"]
            ns <- groupNames
            ns `shouldHaveGroups` expected

    describe "loginName" $
        it "is the output of logname, or fails when logname fails" $ do
            expected <- try (command "logname" [])
            actual <- try loginName
            case (expected, actual) of
                (Right e, Right a) -> a `shouldBe` e
                (Left (_ :: SomeException), Left (_ :: SomeException)) ->
                    return ()
                (Right e, Left err) ->
                    expectationFailure
                        ("logname gave " ++ show e ++ ", loginName failed: "
                            ++ show err)
                (Left err, Right a) ->
                    expectationFailure
                        ("logname failed: " ++ show err
                            ++ ", loginName gave " ++ show a)

    describe "lookups by id" $ do
        it "userNameFromId 0 is root" $
            userNameFromId 0 `shouldReturn` Just "root"

        it "userNameFromId of the current uid is id -un" $ do
            uid <- idNum id
            expected <- idCommand ["-un"]
            userNameFromId uid `shouldReturn` Just expected

        it "userNameFromId of a missing uid is Nothing" $
            userNameFromId missingUid `shouldReturn` Nothing

        it "groupNameFromId of the current gid is id -gn" $ do
            gid <- idNum idGroup
            expected <- idCommand ["-gn"]
            groupNameFromId gid `shouldReturn` Just expected

        it "groupNameFromId of a missing gid is Nothing" $
            groupNameFromId missingUid `shouldReturn` Nothing

        it "primaryGroupIdFromUserId of the current uid is id -g <user>" $ do
            uid <- idNum id
            user <- idCommand ["-un"]
            expected <- idCommand ["-g", user]
            fmap show <$> primaryGroupIdFromUserId uid
                `shouldReturn` Just expected

        it "primaryGroupNameFromUserId of the current uid is id -gn <user>"
            $ do
                uid <- idNum id
                user <- idCommand ["-un"]
                expected <- idCommand ["-gn", user]
                primaryGroupNameFromUserId uid `shouldReturn` Just expected

        it "groupIdsFromUserId starts with the primary gid" $ do
            uid <- idNum id
            Just primary <- primaryGroupIdFromUserId uid
            fmap (take 1) <$> groupIdsFromUserId uid
                `shouldReturn` Just [primary]

        it "groupNamesFromUserId starts with the primary group name" $ do
            uid <- idNum id
            Just primary <- primaryGroupNameFromUserId uid
            fmap (take 1) <$> groupNamesFromUserId uid
                `shouldReturn` Just [primary]

        it "lookups of a missing uid are Nothing" $ do
            primaryGroupIdFromUserId missingUid `shouldReturn` Nothing
            primaryGroupNameFromUserId missingUid `shouldReturn` Nothing
            groupIdsFromUserId missingUid `shouldReturn` Nothing
            groupNamesFromUserId missingUid `shouldReturn` Nothing
