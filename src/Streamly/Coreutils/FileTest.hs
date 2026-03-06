-- |
-- Module      : Streamly.Coreutils.FileTest
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A composable predicate DSL for testing file properties, inspired by the
-- GNU @test@ utility. This module is portable across Linux, macOS, and
-- Windows platforms.
--
-- Predicates can be combined using boolean operators. Multiple composed
-- predicates are evaluated using a single file status query, minimizing
-- system calls and providing better performance than performing each test
-- independently.
--
-- === GNU @test@ Utility Mapping
--
-- This module provides Haskell equivalents for the file-related functionality
-- of the GNU coreutils @test@ utility and the standard POSIX shell
-- style tests such as:
--
-- > [ -d path ]
-- > [ -r path ]
--
-- It offers greater composability and improved performance by allowing
-- multiple predicates to share a single file status query.
--
-- String comparison tests provided by GNU @test@ are intentionally omitted,
-- as they can be expressed directly using standard Haskell operators.
--
-- The mapping below makes it straightforward to translate shell scripts
-- using @test@ or @[ ... ]@ file predicates directly into Haskell code.
--
-- The following table shows the correspondence between common GNU @test@
-- file predicates and the predicates provided by this module.
--
-- > test -b file        -> isBlockDevice
-- > test -c file        -> isCharDevice
-- > test -d file        -> isDir
-- > test -e file        -> doesExist
-- > test -f file        -> isFile
-- > test -g file        -> hasSetGid
-- > test -G file        -> isOwnedByCurrentGroup
-- > test -h file        -> isSymLink
-- > test -k file        -> hasStickyBit
-- > test -L file        -> isSymLink
-- > test -N file        -> modifiedSinceLastAccess
-- > test -O file        -> isOwnedByCurrentUser
-- > test -p file        -> isPipe
-- > test -r file        -> isReadable
-- > test -s file        -> size (> 0)
-- > test -S file        -> isSocket
-- > test -t fd          -> isTerminalFd (not implemented)
-- > test -u file        -> hasSetUid
-- > test -w file        -> isWritable
-- > test -x file        -> isExecutable
--
-- > test file1 -nt file2  -> newerThanFile file2
-- > test file1 -ot file2  -> olderThanFile file2
-- > test file1 -ef file2  -> isHardLinkOf file (not implemented)
--
-- Example:
--
-- > test path doesExist
-- > test path isReadable
-- > test path (size (> 4096))
-- > test path (modifyTimeComparedTo (>) "reference.txt")

module Streamly.Coreutils.FileTest
    (
    -- * File Test Predicate Type
      FileTest

    -- * Running Predicates
    , test
    , testl

    -- * Boolean Predicate Combinators
    , not_
    , and_
    , or_

    -- * Folding Predicates
    , true
    , false
    , and
    , or

    -- * Predicates

    -- ** General
    -- , predicate -- exposes FileStatus
    , doesExist -- XXX doesItExist or doesPathExist

    -- ** File Type
    , isDir
    , isFile
    , isSymLink
    , isCharDevice
    , isBlockDevice
    , isPipe
    , isSocket

    -- ** File Mode
    -- | We can define convenience operations by combining multiple elementary
    -- checks, for example:
    --
    -- @
    -- hasOwnerRWX = and [hasOwnerRead, hasOwnerWrite, hasOwnerExec]
    -- @
    --
    -- === Portability Notes
    --
    -- On POSIX systems, this checks the standard Unix permission bits.
    --
    -- On Windows, only one or two predicates make sense:
    --
    -- * 'hasOwnerWrite' - returns false if the file is marked read only via attributes.
    -- * 'hasOwnerExec' - returns true based on the file extension: @.bat@, @.cmd@,
    -- @.com@, @.exe@.
    -- * 'hasOwnerRead' - always returns true.
    -- * Group, and Other predicates are same as owner predicates.

    , hasOwnerRead
    , hasOwnerWrite
    , hasOwnerExec

    , hasGroupRead
    , hasGroupWrite
    , hasGroupExec

    , hasOtherRead
    , hasOtherWrite
    , hasOtherExec

    , hasSetUid
    , hasSetGid
    , hasStickyBit

    -- ** File Access (Current User)

    -- XXX currently not working fully well, hasPermissions need to be fixed
    -- for checking acess via all groups.
    --
    -- These have limited use on Windows as windows uses mostly ACLs, only
    -- read only bit is used in modes.

    -- *** Mode based access
    -- | These APIs perform only the file permission mode checks, actual
    -- readability, writability or executability may depend many other factors
    -- like filesystem mount permissions, access control lists (ACLs) etc. For
    -- deeper checks see: 'isReadable', 'isWritable', 'isExecutable'.
    --
    , isReadableByMode
    , isWritableByMode
    , isExecutableByMode

    -- *** Real Access
    -- | These tests determine whether the file is actually accessible at this
    -- time including file permission mode, ACLs, mount permissions.
    , isReadable
    , isWritable
    , isExecutable

    {-
    -- *** Lock based
    -- | These do not make much sense on posix as posix does not use mandatory
    -- locks.
    , isReadableNow
    , isWritableNow
    , isExecutableNow
    -}

    -- ** File Ownership (Current User)
    {-
    , isOwnedByUserId
    , isOwnedByGroupId
    , isOwnedByUserName
    , isOwnedByGroupName
    -}
    , isOwnedByCurrentUser
    , isOwnedByCurrentGroup

    -- ** File size
    -- XXX Need convenient size units and conversions (e.g. kB 1, kiB 1, mB 2)
    , size
    , sizeComparedTo
    , largerThanFile
    , smallerThanFile
    , sameSizeAs

    -- ** File times
    -- | 'NominalDiffTime' is time duration specified in seconds possibly
    -- fractional. It has a Num instance so you can specify literals and cast
    -- common types as follows:
    --
    -- >>> 0.5 :: NominalDiffTime
    -- >>> fromIntegral :: Int -> NominalDiffTime
    -- >>> realToFrac :: Double -> NominalDiffTime
    -- >>> fromInteger :: Integer -> NominalDiffTime
    --
    -- Unit helpers are convenient to specify time durations:
    --
    -- >>> modifiedWithin (days 1 + hours 5 + minutes 10 + seconds 20)

    -- *** Time units
    , seconds
    , minutes
    , hours
    , days

    -- *** File age
    , modifyAge
    , modifiedWithin
    -- , modifiedOlderThan -- (not_ modifiedWithin) is better
    , accessAge
    , metadataAge

    -- *** File timestamp
    , modifyTime
    , accessTime
    , metadataChangeTime

    -- *** Compare timestamps with file
    , modifyTimeComparedTo
    , olderThanFile
    , newerThanFile
    , accessTimeComparedTo

    -- * Deprecated
    , isExisting
    )
where

import System.Posix.Types (FileMode)

import qualified System.PosixCompat.Files as Files

#if !defined(CABAL_OS_WINDOWS)
import qualified Streamly.Coreutils.FileTest.Posix as FileTest
#else
import qualified Streamly.Coreutils.FileTest.Windows as FileTest
import System.Win32.Types
#endif

import Streamly.Coreutils.FileTest.Common
import Prelude hiding (and, or)

-------------------------------------------------------------------------------
-- User and group ownerships
-------------------------------------------------------------------------------

_isOwnedByUserId :: FileTest.Uid -> FileTest
_isOwnedByUserId = FileTest.isOwnedByUserId

_isOwnedByGroupId :: FileTest.Gid -> FileTest
_isOwnedByGroupId = FileTest.isOwnedByGroupId

-- | Unimplemented
_isOwnedByUserName :: String -> FileTest
_isOwnedByUserName = undefined

-- | Unimplemented
_isOwnedByGroupName :: String -> FileTest
_isOwnedByGroupName = undefined

-- | True if the file owner matches the effective user id of the current
-- process.
--
-- On Windows, effective user id means effective SID.
--
-- Like coreutil @test -O file@
isOwnedByCurrentUser :: FileTest
isOwnedByCurrentUser = FileTest.isOwnedByCurrentUser

-- Unix files have a GID and group permission bits. A process has an effective
-- GID (egid) and a list of supplementary groups stored in its credentials.
-- These supplementary groups are typically initialized at login from the
-- user's group memberships and inherited by child processes; they can be
-- changed via setgroups(2), setgid(2), newgrp, or in user namespaces.
--
-- The egid and supplementary groups are used for:
--
-- * Permission checks: if a file's GID matches the egid or any supplementary
--   group, the group permission bits apply. Certain IPC and kernel security
--   checks based on group ownership.
-- * Default group ownership of newly created files (unless overridden by
--   a setgid bit on a directory).
-- * Execution of setgid binaries, which set the process's egid to the
--   file's group. For directories, setgid changes group inheritance semantics.
--
-- Windows files have an associated "group" SID, and process tokens contain
-- a primary group plus a list of group SIDs.
--
--  * file's gSID is not used in access checks; only ACLS determines access.
--  * process token’s primary gSID is used as gSID for new files.
--  * there is no setgid concept or equivalent.
--
-- The group SID in Windows exists mainly for POSIX/NFS interoperability, where
-- a file must have a GID for Unix permission semantics.

-- XXX On Windows we can match against the primary group SID of the process
-- token, though it won't mean much in terms of actual implications. But it can
-- still be used for Posix based semantics. But since there are no group based
-- permission bits even returning False is effectively equivalent.

-- | True if file exists and its group matches the effective
-- group id of the current process.
--
-- Like coreutil @test -G file@.
--
-- On Windows effective group id means the primary group SID.
--
isOwnedByCurrentGroup :: FileTest
isOwnedByCurrentGroup = FileTest.isOwnedByCurrentGroup

-------------------------------------------------------------------------------
-- Mode based access
-------------------------------------------------------------------------------

hasPermissions :: (FileMode, FileMode, FileMode) -> FileTest
hasPermissions (user, group, other) = withStateM $ \fp st -> do
    isOwner <- testGeneral fp st isOwnedByCurrentUser
    let checkMode = testGeneral fp st . hasMode
    if isOwner
    then checkMode user
#if !defined(CABAL_OS_WINDOWS)
    else do
        -- XXX need to check access via other group memberships as well
        isGroup <- testGeneral fp st isOwnedByCurrentGroup
        if isGroup
        then checkMode group
        else checkMode other
#else
    else return False
#endif

-- | True if the file mode bits allow the file to be read by the current
-- effective user id.
--
-- On Windows this is always true.
--
-- This does not check the ACLs and other conditions that can make the file
-- unreadable, see 'isReadable' for that.
--
isReadableByMode :: FileTest
isReadableByMode =
    hasPermissions
        (
          Files.ownerReadMode
        , Files.groupReadMode
        , Files.otherReadMode
        )

-- | True if the file mode bits make it writable for the current user.
--
-- On Windows this returns false if the read only flag is set on the file.
--
-- This does not check the ACLs, see 'isWritable' for that.
--
isWritableByMode :: FileTest
isWritableByMode =
    hasPermissions
        (
          Files.ownerWriteMode
        , Files.groupWriteMode
        , Files.otherWriteMode
        )

-- | True if the file mode bits make it executable for the current user.
--
-- On Windows this returns true if it is a directory or if it is a file with an
-- executable extension @.bat@, @.cmd@, @.com@, or @.exe@.
--
-- This does not check the ACLs, see isExecutable for that.
--
isExecutableByMode :: FileTest
isExecutableByMode =
    hasPermissions
        (
          Files.ownerExecuteMode
        , Files.groupExecuteMode
        , Files.otherExecuteMode
        )

-------------------------------------------------------------------------------
-- General access, excluding locks
-------------------------------------------------------------------------------

-- | True if the file is readable by the current process.
--
-- This is a dynamic check and determines the readability of the file at this
-- moment based on the permission checks applied by the kernel (e.g. dynamic
-- group membership based permissions, effective user id, acls).
--
-- Does not consider advisory or mandatory locks.
--
-- Like coreutil @test -r file@
--
isReadable :: FileTest
isReadable = FileTest.isReadable

-- XXX What does "isWritable" mean on windows? Windows has separate write and
-- modify permissions. We can use two separate functions, isWritableData (or
-- just isWritable), isWritableMeta. On unix both will have the same underlying
-- permission.

-- | True if the file is writable by the current process.
--
-- This is a dynamic check and determines the writability of the file at this
-- moment based on the permission checks applied by the kernel (e.g. mount
-- options, dynamic group membership based permissions, effective user id,
-- acls).
--
-- Does not consider advisory or mandatory locks.
--
-- Like coreutil @test -w file@
--
isWritable :: FileTest
isWritable = FileTest.isWritable

-- NOTE: On POSIX: You do NOT need directory read (r) permission to access a
-- known file. You DO need directory execute (x) permission. On Windows, the
-- (r) equivalent is "List Folder" and (x) equivalent is "Traverse Folder". By
-- default, almost all users have: SeChangeNotifyPrivilege ("Bypass traverse
-- checking"), it is a fast path to grant the access compared to giving
-- traverse folder access to everyone and checking it on each directory in the
-- path.

-- | True if the file is executable for the current user.
--
-- Like coreutil @test -x file@ .
--
isExecutable :: FileTest
isExecutable = FileTest.isExecutable
