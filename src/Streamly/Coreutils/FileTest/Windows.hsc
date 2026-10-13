{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Coreutils.FileTest.Windows
    ( sameFileAs
    , isTerminalFd
    {-
    , Uid
    , Gid
    , isOwnedByUserId
    , isOwnedByGroupId
    -}
    , isOwnedByCurrentUser
    -- , isOwnedByCurrentGroup

    , isReadableNow
    , isWritableNow
    , isExecutableNow

    , isReadable
    , isWritable
    , isExecutable

    , isDirSymLink
    ) where

import Control.Exception
    ( AsyncException
    , IOException
    , SomeException
    , bracket
    , catch
    , fromException
    , throwIO
    )

import Foreign.C.Types (CInt(..))
import System.PosixCompat.Files (FileStatus, isSymbolicLink, ownerWriteMode)
import System.Posix.Types (Fd(..))
import System.Win32.Console (getConsoleMode)
import System.Win32.File
    ( BY_HANDLE_FILE_INFORMATION(..)
    , bhfiFileIndex
    , bhfiVolumeSerialNumber
    , closeHandle
    , createFile
    , fILE_ADD_FILE
    , fILE_EXECUTE
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_READ_DATA
    , fILE_SHARE_DELETE
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , fILE_TYPE_CHAR
    , fILE_WRITE_DATA
    , gENERIC_ALL
    , gENERIC_EXECUTE
    , gENERIC_READ
    , gENERIC_WRITE
    , getFileAttributes
    , getFileInformationByHandle
    , getFileType
    , oPEN_EXISTING
    , rEAD_CONTROL
    )
import System.Win32.Security
    ( PSID
    , dACL_SECURITY_INFORMATION
    , oWNER_SECURITY_INFORMATION
    )
import System.Win32.Types
    ( BOOL
    , DWORD
    , HANDLE
    , LPDWORD
    , failIfFalse_
    , iNVALID_HANDLE_VALUE
    , withFilePath
    )

import qualified System.Win32.Types as Win32

import Foreign
import Streamly.FileSystem.Path (Path)
import qualified Streamly.FileSystem.Path as Path
import Streamly.Coreutils.FileTest.Common

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{-
-- | Wraps a Windows SID pointer representing a user identity.
newtype Uid = Uid PSID
-- | Wraps a Windows SID pointer representing a group identity.
newtype Gid = Gid PSID

isOwnedByUserId :: Uid -> FileTest
isOwnedByUserId (Uid uid) = withPathM $ \fp -> undefined

isOwnedByGroupId :: Gid -> FileTest
isOwnedByGroupId (Gid gid) = withPathM $ \fp -> undefined
-}

-------------------------------------------------------------------------------
-- Raw FFI declarations
--
-- These are all bindings not exposed by the Win32 package.
-------------------------------------------------------------------------------

##include "windows_cconv.h"

foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentProcess"
    c_GetCurrentProcess :: IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h OpenProcessToken"
    c_OpenProcessToken
        :: HANDLE   -- ProcessHandle
        -> DWORD    -- DesiredAccess
        -> Ptr HANDLE
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetTokenInformation"
    c_GetTokenInformation
        :: HANDLE   -- TokenHandle
        -> DWORD    -- TokenInformationClass
        -> Ptr ()   -- TokenInformation
        -> DWORD    -- TokenInformationLength
        -> LPDWORD  -- ReturnLength
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h DuplicateToken"
    c_DuplicateToken
        :: HANDLE     -- ExistingTokenHandle
        -> DWORD      -- ImpersonationLevel (SECURITY_IMPERSONATION_LEVEL)
        -> Ptr HANDLE -- DuplicateTokenHandle
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h EqualSid"
    c_EqualSid :: PSID -> PSID -> IO BOOL

-- In aclapi.h, linked via -ladvapi32 (same as other security calls)
foreign import WINDOWS_CCONV unsafe "aclapi.h GetNamedSecurityInfoW"
    c_GetNamedSecurityInfo
        :: Win32.LPWSTR -- pObjectName
        -> DWORD        -- ObjectType (SE_OBJECT_TYPE)
        -> DWORD        -- SecurityInfo
        -> Ptr PSID     -- ppsidOwner  (out, optional)
        -> Ptr PSID     -- ppsidGroup  (out, optional)
        -> Ptr (Ptr ()) -- ppDacl      (out, optional)
        -> Ptr (Ptr ()) -- ppSacl      (out, optional)
        -> Ptr (Ptr ()) -- ppSecurityDescriptor (out)
        -> IO DWORD     -- ERROR_SUCCESS (0) on success

-- Like GetNamedSecurityInfoW but takes a handle, in aclapi.h
foreign import WINDOWS_CCONV unsafe "aclapi.h GetSecurityInfo"
    c_GetSecurityInfo
        :: HANDLE       -- handle
        -> DWORD        -- ObjectType (SE_OBJECT_TYPE)
        -> DWORD        -- SecurityInfo
        -> Ptr PSID     -- ppsidOwner  (out, optional)
        -> Ptr PSID     -- ppsidGroup  (out, optional)
        -> Ptr (Ptr ()) -- ppDacl      (out, optional)
        -> Ptr (Ptr ()) -- ppSacl      (out, optional)
        -> Ptr (Ptr ()) -- ppSecurityDescriptor (out)
        -> IO DWORD     -- ERROR_SUCCESS (0) on success

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    c_LocalFree :: Ptr () -> IO (Ptr ())

foreign import WINDOWS_CCONV unsafe "windows.h AccessCheck"
    c_AccessCheck
        :: Ptr ()              -- pSecurityDescriptor
        -> HANDLE              -- ClientToken (must be impersonation token)
        -> DWORD               -- DesiredAccess
        -> Ptr GENERIC_MAPPING
        -> Ptr ()              -- PrivilegeSet (out)
        -> LPDWORD             -- PrivilegeSetLength (in/out)
        -> LPDWORD             -- GrantedAccess (out)
        -> Ptr BOOL            -- AccessStatus (out)
        -> IO BOOL

-------------------------------------------------------------------------------
-- Constants not exported by Win32
-------------------------------------------------------------------------------

-- TOKEN_QUERY (0x0008) is not re-exported, define locally
tOKEN_QUERY :: DWORD
tOKEN_QUERY = 0x0008

-- TokenUser = 1 in the TOKEN_INFORMATION_CLASS enum
tOKEN_USER :: DWORD
tOKEN_USER = 1

-- SecurityImpersonation = 2 in SECURITY_IMPERSONATION_LEVEL
sECURITY_IMPERSONATION :: DWORD
sECURITY_IMPERSONATION = 2

-- SE_FILE_OBJECT = 1 in SE_OBJECT_TYPE enum
sE_FILE_OBJECT :: DWORD
sE_FILE_OBJECT = 1

-- FILE_GENERIC_* are composites not exported by Win32; values from WinNT.h
fILE_GENERIC_READ :: DWORD
fILE_GENERIC_READ = 0x00120089

fILE_GENERIC_WRITE :: DWORD
fILE_GENERIC_WRITE = 0x00120116

fILE_GENERIC_EXECUTE :: DWORD
fILE_GENERIC_EXECUTE = 0x001200A0

-- Not exported by Win32; value from WinBase.h
fILE_FLAG_OPEN_REPARSE_POINT :: DWORD
fILE_FLAG_OPEN_REPARSE_POINT = 0x00200000

-------------------------------------------------------------------------------
-- GENERIC_MAPPING storable struct
-------------------------------------------------------------------------------

-- GENERIC_MAPPING { GenericRead, GenericWrite, GenericExecute, GenericAll }
data GENERIC_MAPPING = GENERIC_MAPPING DWORD DWORD DWORD DWORD

instance Storable GENERIC_MAPPING where
    sizeOf    _ = 4 * sizeOf (undefined :: DWORD)
    alignment _ = alignment  (undefined :: DWORD)
    peek p = GENERIC_MAPPING
        <$> peekByteOff p 0
        <*> peekByteOff p 4
        <*> peekByteOff p 8
        <*> peekByteOff p 12
    poke p (GENERIC_MAPPING r w e a) = do
        pokeByteOff p 0  r
        pokeByteOff p 4  w
        pokeByteOff p 8  e
        pokeByteOff p 12 a

-------------------------------------------------------------------------------
-- Same file
-------------------------------------------------------------------------------

-- | Open a file handle suitable for metadata queries, with the given desired
-- access. If the second argument is 'False' and the path is a symlink, open
-- the symlink itself instead of its target, like @lstat@.
-- Uses fILE_FLAG_BACKUP_SEMANTICS so that directories can be opened too.
withFileHandle :: DWORD -> Bool -> Path -> (HANDLE -> IO a) -> IO a
withFileHandle access followLinks path action =
    bracket
        (createFile (Path.toString path) access
            (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
            Nothing
            oPEN_EXISTING
            flags
            Nothing)
        closeHandle
        action
  where
    flags
        | followLinks = fILE_FLAG_BACKUP_SEMANTICS
        | otherwise =
            fILE_FLAG_BACKUP_SEMANTICS .|. fILE_FLAG_OPEN_REPARSE_POINT

-- | Return a (volumeSerialNumber, fileIndex) pair that uniquely identifies
-- a file on the local system, following the same-inode logic as POSIX.
fileId :: Bool -> Path -> IO (DWORD, Word64)
fileId followLinks path =
    -- 0 desired access: only metadata is needed, so this succeeds even when
    -- data-read access is restricted.
    withFileHandle 0 followLinks path $ \h -> do
        info <- getFileInformationByHandle h
        let vol = bhfiVolumeSerialNumber info
            idx = bhfiFileIndex info
        pure (vol, idx)

-- A missing path1 or path2 gives False, as in the POSIX implementation. The
-- status of path1 is fetched first, so that the result for a missing path1
-- and the exceptions for other errors are the same as for the other
-- predicates. Under 'testl' path1 is not dereferenced, like the lstat used by
-- the POSIX implementation; path2 is always dereferenced.

-- | True if both paths refer to the same underlying file or directory,
-- equivalent to POSIX inode comparison. Returns 'False' if either path does
-- not exist.
sameFileAs :: Path -> FileTest
sameFileAs path2 =
    withFollowStateM $ \followLinks path1 _ -> do
        id1 <- fileId followLinks path1
        catchNonExistent $ do
            id2 <- fileId True path2
            pure (id1 == id2)

-------------------------------------------------------------------------------
-- Terminal
-------------------------------------------------------------------------------

-- | Convert a CRT file descriptor to a Windows HANDLE.
-- Returns -1 (cast to HANDLE) on error, per CRT documentation.
foreign import ccall unsafe "_get_osfhandle"
    c_get_osfhandle :: CInt -> IO HANDLE

-- | Returns True only if GetConsoleMode succeeds on the handle,
-- which is the Windows-canonical way to test for a console.
isConsoleHandle :: HANDLE -> IO Bool
isConsoleHandle h =
    (getConsoleMode h >> pure True)
    `catch` \(_ :: IOError) -> pure False

-- | True if the fd is connected to a console (terminal).
-- Mirrors the behaviour of POSIX @test -t@:
--   1. fd must be a valid Windows HANDLE
--   2. The handle's file type must be FILE_TYPE_CHAR
--   3. GetConsoleMode must succeed — this distinguishes real console
--      handles from other character devices (serial ports, NUL, etc.)
isTerminalFd :: Fd -> FileTest
isTerminalFd (Fd fd) =
    withPathM $ \_ -> do
        h <- c_get_osfhandle fd
        if h == iNVALID_HANDLE_VALUE
        then pure False
        else do
            t <- getFileType h
            if t /= fILE_TYPE_CHAR
            then pure False
            else isConsoleHandle h

-------------------------------------------------------------------------------
-- withFileOwnerSID
-------------------------------------------------------------------------------

-- GetFileSecurityW always reads the symlink itself, not its target. To get
-- the owner of the target under 'test', like stat on POSIX, we read the
-- owner from a handle, opened with or without FILE_FLAG_OPEN_REPARSE_POINT.

-- | Run an action on the owner SID of a file. If the first argument is
-- 'False' and the path is a symlink, use the owner of the symlink itself.
withFileOwnerSID :: Bool -> Path -> (PSID -> IO a) -> IO a
withFileOwnerSID followLinks path action =
    withFileHandle rEAD_CONTROL followLinks path $ \h ->
    alloca $ \ppSid ->
    alloca $ \ppSd -> do
        Win32.failUnlessSuccess "GetSecurityInfo" $
            c_GetSecurityInfo
                h sE_FILE_OBJECT oWNER_SECURITY_INFORMATION
                ppSid nullPtr nullPtr nullPtr ppSd
        pSd <- peek ppSd
        -- The owner SID points into the security descriptor allocated by
        -- GetSecurityInfo; free the descriptor with LocalFree.
        bracket (return pSd) (\p -> c_LocalFree p >> return ()) $ \_ ->
            peek ppSid >>= action

-------------------------------------------------------------------------------
-- withEffectiveUserSID
-------------------------------------------------------------------------------

withEffectiveUserSID :: (PSID -> IO a) -> IO a
withEffectiveUserSID action = do
    proc <- c_GetCurrentProcess
    alloca $ \pToken -> do
        failIfFalse_ "OpenProcessToken" $
            c_OpenProcessToken proc tOKEN_QUERY pToken
        token <- peek pToken
        bracket (return token) (\h -> c_CloseHandle h >> return ()) $ \h ->
            alloca $ \pRetLen -> do
                -- First call is expected to fail; returns the required size.
                _ <- c_GetTokenInformation h tOKEN_USER nullPtr 0 pRetLen
                retLen <- peek pRetLen
                allocaBytes (fromIntegral retLen) $ \buf -> do
                    failIfFalse_ "GetTokenInformation" $
                        c_GetTokenInformation
                            h tOKEN_USER buf retLen pRetLen
                    -- TOKEN_USER layout:
                    -- { SID_AND_ATTRIBUTES { PSID Sid; DWORD Attributes } }
                    -- First field is the PSID pointer.
                    sid <- peek (castPtr buf :: Ptr PSID)
                    action sid

-------------------------------------------------------------------------------
-- Ownership
-------------------------------------------------------------------------------

isPathOwnedByCurrentUser :: Bool -> Path -> IO Bool
isPathOwnedByCurrentUser followLinks path =
    withFileOwnerSID followLinks path $ \fileSid ->
        withEffectiveUserSID $ \userSid ->
            c_EqualSid fileSid userSid

isOwnedByCurrentUser :: FileTest
isOwnedByCurrentUser =
    withFollowStateM $ \followLinks path _ ->
        isPathOwnedByCurrentUser followLinks path

{-
withFilePrimaryGroupSID = undefined
withEffectiveGroupSID = undefined

isPathOwnedByCurrentGroup :: Path -> IO Bool
isPathOwnedByCurrentGroup path =
    withFilePrimaryGroupSID path $ \fileSid ->
        withEffectiveGroupSID $ \userSid ->
            c_EqualSid fileSid userSid

isOwnedByCurrentGroup :: FileTest
isOwnedByCurrentGroup = withPathM isPathOwnedByCurrentGroup
-}

-------------------------------------------------------------------------------
-- File Access including share locks
-------------------------------------------------------------------------------

-- May return false if file is open and not readable because it is locked
-- using a share mode that denies read. This is something additional on
-- Windows; POSIX does not have a feature where read access is denied due
-- to a file being open. We implement this additional behavior to align
-- with the goal that a readability check tells us whether we can read
-- the file at this moment.

-- | True if the file is readable by the current process.
--
-- Returns false if the file is locked and not shared for reading.
--
isPathReadableNow :: Path -> IO Bool
isPathReadableNow path =
    (do h <- createFile
                 (Path.toString path)
                 fILE_READ_DATA shareMode Nothing oPEN_EXISTING
                 flags Nothing
        closeHandle h
        return True
    ) `catch` \(_ :: IOException) -> return False
  where
    shareMode = fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE
    -- Required to open directories
    flags     = fILE_FLAG_BACKUP_SEMANTICS

isReadableNow :: FileTest
isReadableNow = withExistingPathM isPathReadableNow

-- | True if the file is writable by the current process.
--
-- Returns false if the file is locked and not shared for writing.
--
isFileWritableNow :: Path -> FileStatus -> IO Bool
isFileWritableNow path st = do
    isDirectory <- testWithStatus path st isDir
    -- Under unix-compat on Windows, ownerWriteMode corresponds to the
    -- FILE_ATTRIBUTE_READONLY flag being unset.
    writable <- testWithStatus path st (hasMode ownerWriteMode)
    -- The READONLY attribute on directories does not prevent creating
    -- files inside the directory.
    if not writable && not isDirectory
    then return False
    else do
        let desiredAccess
                | isDirectory = fILE_ADD_FILE
                | otherwise   = fILE_WRITE_DATA
            shareMode =
                    fILE_SHARE_READ
                .|. fILE_SHARE_WRITE
                .|. fILE_SHARE_DELETE
            flags
                | isDirectory = fILE_FLAG_BACKUP_SEMANTICS
                | otherwise   = 0
        bracket
            (createFile
                (Path.toString path)
                desiredAccess shareMode Nothing oPEN_EXISTING
                flags Nothing)
            closeHandle
            (\_ -> return True)
        `catch` (\(_ :: IOException) -> return False)

isWritableNow :: FileTest
isWritableNow = withStateM isFileWritableNow

-- | Returns true if file is executable.
-- Returns false if the file is locked and not shared for execution.
isPathExecutableNow :: Path -> IO Bool
isPathExecutableNow path =
    (do h <- createFile
                 (Path.toString path)
                 fILE_EXECUTE shareMode Nothing oPEN_EXISTING
                 flags Nothing
        closeHandle h
        return True
    ) `catch` \(_ :: IOException) -> return False
  where
    shareMode = fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE
    flags     = fILE_FLAG_BACKUP_SEMANTICS

isExecutableNow :: FileTest
isExecutableNow = withExistingPathM isPathExecutableNow

-------------------------------------------------------------------------------
-- ACL-based checks (POSIX access() equivalent)
-------------------------------------------------------------------------------

-- | Open an impersonation token for the current process.
-- AccessCheck requires an impersonation token, not a primary token.
openCurrentProcessImpersonationToken :: IO HANDLE
openCurrentProcessImpersonationToken = do
    proc <- c_GetCurrentProcess
    alloca $ \pToken -> do
        failIfFalse_ "OpenProcessToken" $
            c_OpenProcessToken proc tOKEN_QUERY pToken
        primaryToken <- peek pToken
        bracket
            (return primaryToken)
            (\h -> c_CloseHandle h >> return ())
            $ \pt -> alloca $ \pImpToken -> do
                failIfFalse_ "DuplicateToken" $
                    c_DuplicateToken pt sECURITY_IMPERSONATION pImpToken
                peek pImpToken

-- | Checks the file's DACL against the current process token for the
-- given access mask. Implements the Windows equivalent of POSIX access().
pathAccess :: Path -> DWORD -> IO Bool
pathAccess path mask =
    bracket
        openCurrentProcessImpersonationToken
        (\h -> c_CloseHandle h >> return ())
        $ \token ->
            withFilePath (Path.toString path) $ \pPath ->
            alloca $ \ppSd -> do
                ret <- c_GetNamedSecurityInfo
                           pPath
                           sE_FILE_OBJECT
                           dACL_SECURITY_INFORMATION
                           nullPtr nullPtr nullPtr nullPtr
                           ppSd
                -- ret is ERROR_SUCCESS (0) on success
                if ret /= 0
                then return False
                else do
                    pSd <- peek ppSd
                    -- pSd allocated by GetNamedSecurityInfo; free with
                    -- LocalFree.
                    bracket (return pSd)
                            (\p -> c_LocalFree p >> return ()) $ \sd ->
                        with (GENERIC_MAPPING
                                gENERIC_READ
                                gENERIC_WRITE
                                gENERIC_EXECUTE
                                gENERIC_ALL)
                            $ \pMapping ->
                        -- PrivilegeSet: DWORD size + space for at least one
                        -- LUID_AND_ATTRIBUTES; 64 bytes is more than enough.
                        allocaBytes 64 $ \pPrivSet ->
                        alloca $ \pPrivSetLen ->
                        alloca $ \pGrantedAccess ->
                        alloca $ \pAccessStatus -> do
                            poke pPrivSetLen 64
                            ok <- c_AccessCheck
                                      sd token mask pMapping
                                      pPrivSet pPrivSetLen
                                      pGrantedAccess pAccessStatus
                            -- ok == 0 means AccessCheck itself failed
                            if ok
                            then peek pAccessStatus
                            else return False
    `catch` handler
  where
    handler :: SomeException -> IO Bool
    handler e
        | Just (_ :: AsyncException) <- fromException e = throwIO e
        | otherwise = return False

-- | Windows equivalent of POSIX: access(path, R_OK). Matches @test -r@.
pathIsReadable :: Path -> IO Bool
pathIsReadable path = pathAccess path fILE_GENERIC_READ

-- | Windows equivalent of POSIX: access(path, W_OK). Matches @test -w@.
pathIsWritable :: Path -> IO Bool
pathIsWritable path = pathAccess path fILE_GENERIC_WRITE

-- | Windows equivalent of POSIX: access(path, X_OK). Matches @test -x@.
pathIsExecutable :: Path -> IO Bool
pathIsExecutable path = pathAccess path fILE_GENERIC_EXECUTE

isReadable :: FileTest
isReadable = withExistingPathM pathIsReadable

isWritable :: FileTest
isWritable = withExistingPathM pathIsWritable

isExecutable :: FileTest
isExecutable = withExistingPathM pathIsExecutable

fILE_ATTRIBUTE_DIRECTORY    :: DWORD
fILE_ATTRIBUTE_DIRECTORY    = 0x10

-- | True iff the path is a reparse point (symlink or junction) that the OS
-- also marks as a directory object.
isPathDirSymLink :: Path -> FileStatus -> IO Bool
isPathDirSymLink path st =
    -- XXX We should cache the raw attributes for multiple checks Currently we
    -- cache only the FileStatus from unix-compat which does not have full info
    -- for windows and we need to make additional calls.
    if isSymbolicLink st
    then do
        attrs <- getFileAttributes (Path.toString path)
        return $ attrs .&. fILE_ATTRIBUTE_DIRECTORY /= 0
    else
        return False

-- | True if path is a symbolic link or a junction which the directory
-- attribute set. This is meaningful only when 'testl' is used, in case of
-- 'test' it always returns false.
isDirSymLink :: FileTest
isDirSymLink = withStateM isPathDirSymLink
