{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Coreutils.Filetest.Windows
    ( Uid
    , Gid
    {-
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
    ) where

import Control.Exception
    ( AsyncException
    , SomeException
    , bracket
    , catch
    , fromException
    , throwIO
    )
import Data.Bits ((.|.))
import Foreign.Ptr (nullPtr)
import System.IO.Error (IOException)

import System.PosixCompat.Files (FileStatus)
import System.Posix.Types (ownerWriteMode)

import System.Win32.Security
    ( PSID
    , SID
    , dACL_SECURITY_INFORMATION
    , oWNER_SECURITY_INFORMATION
    )
import System.Win32.File
    ( closeHandle
    , createFile
    , fILE_ADD_FILE
    , fILE_EXECUTE
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_READ_DATA
    , fILE_SHARE_DELETE
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , fILE_WRITE_DATA
    , gENERIC_ALL
    , gENERIC_EXECUTE
    , gENERIC_READ
    , gENERIC_WRITE
    , oPEN_EXISTING
    )
import System.Win32.Types
    ( BOOL
    , DWORD
    , HANDLE
    , LPDWORD
    , failIfFalse_
    , withFilePath
    )

import qualified System.Win32.Types as Win32

import Foreign
import Streamly.Coreutils.FileTest.Common

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Wraps a Windows SID pointer representing a user identity.
newtype Uid = Uid PSID
-- | Wraps a Windows SID pointer representing a group identity.
newtype Gid = Gid PSID

{-
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

-- GetFileSecurityW is used directly so we can work with the raw
-- PSECURITY_DESCRIPTOR (Ptr ()) rather than the opaque SecurityDescriptor
-- newtype returned by getFileSecurity.
foreign import WINDOWS_CCONV unsafe "windows.h GetFileSecurityW"
    c_GetFileSecurity
        :: Win32.LPCWSTR -- lpFileName
        -> DWORD         -- RequestedInformation (SECURITY_INFORMATION)
        -> Ptr ()        -- pSecurityDescriptor
        -> DWORD         -- nLength
        -> LPDWORD       -- lpnLengthNeeded
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorOwner"
    c_GetSecurityDescriptorOwner
        :: Ptr ()   -- pSecurityDescriptor
        -> Ptr PSID -- pOwner
        -> Ptr BOOL -- lpbOwnerDefaulted
        -> IO BOOL

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
-- withFileOwnerSID
-------------------------------------------------------------------------------

-- We bind c_GetFileSecurity directly (as Ptr ()) rather than using
-- getFileSecurity, whose SecurityDescriptor newtype is opaque and
-- incompatible with c_GetSecurityDescriptorOwner.
withFileOwnerSID :: FilePath -> (PSID -> IO a) -> IO a
withFileOwnerSID path action =
    withFilePath path $ \pPath ->
    alloca $ \pLenNeeded -> do
        -- First call: get required buffer size
        _ <- c_GetFileSecurity
                 pPath oWNER_SECURITY_INFORMATION nullPtr 0 pLenNeeded
        needed <- peek pLenNeeded
        allocaBytes (fromIntegral needed) $ \pSd -> do
            failIfFalse_ "GetFileSecurityW" $
                c_GetFileSecurity
                    pPath oWNER_SECURITY_INFORMATION pSd needed pLenNeeded
            alloca $ \ppSid ->
                alloca $ \pDefaulted -> do
                    failIfFalse_ "GetSecurityDescriptorOwner" $
                        c_GetSecurityDescriptorOwner pSd ppSid pDefaulted
                    sid <- peek ppSid
                    action sid

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

isPathOwnedByCurrentUser :: FilePath -> IO Bool
isPathOwnedByCurrentUser path =
    withFileOwnerSID path $ \fileSid ->
        withEffectiveUserSID $ \userSid ->
            (/= 0) <$> c_EqualSid fileSid userSid

isOwnedByCurrentUser :: FileTest
isOwnedByCurrentUser = withPathM isPathOwnedByCurrentUser

{-
withFilePrimaryGroupSID = undefined
withEffectiveGroupSID = undefined

isPathOwnedByCurrentGroup :: FilePath -> IO Bool
isPathOwnedByCurrentGroup path =
    withFilePrimaryGroupSID path $ \fileSid ->
        withEffectiveGroupSID $ \userSid ->
            (/= 0) <$> c_EqualSid fileSid userSid

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
isPathReadableNow :: FilePath -> IO Bool
isPathReadableNow path =
    (do h <- createFile
                 path fILE_READ_DATA shareMode Nothing oPEN_EXISTING
                 flags Nothing
        closeHandle h
        return True
    ) `catch` \(_ :: IOException) -> return False
  where
    shareMode = fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE
    -- Required to open directories
    flags     = fILE_FLAG_BACKUP_SEMANTICS

isReadableNow :: FileTest
isReadableNow = withPathM isPathReadableNow

-- | True if the file is writable by the current process.
--
-- Returns false if the file is locked and not shared for writing.
--
isFileWritableNow :: FilePath -> FileStatus -> IO Bool
isFileWritableNow path st = do
    isDirectory <- testGeneral path st isDir
    -- Under unix-compat on Windows, ownerWriteMode corresponds to the
    -- FILE_ATTRIBUTE_READONLY flag being unset.
    writable <- testGeneral path st (hasMode ownerWriteMode)
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
                path desiredAccess shareMode Nothing oPEN_EXISTING
                flags Nothing)
            closeHandle
            (\_ -> return True)
        `catch` (\(_ :: IOException) -> return False)

isWritableNow :: FileTest
isWritableNow = withStateM isFileWritableNow

-- | Returns true if file is executable.
-- Returns false if the file is locked and not shared for execution.
isPathExecutableNow :: FilePath -> IO Bool
isPathExecutableNow path =
    (do h <- createFile
                 path fILE_EXECUTE shareMode Nothing oPEN_EXISTING
                 flags Nothing
        closeHandle h
        return True
    ) `catch` \(_ :: IOException) -> return False
  where
    shareMode = fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE
    flags     = fILE_FLAG_BACKUP_SEMANTICS

isExecutableNow :: FileTest
isExecutableNow = withPathM isPathExecutableNow

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
pathAccess :: FilePath -> DWORD -> IO Bool
pathAccess path mask =
    bracket
        openCurrentProcessImpersonationToken
        (\h -> c_CloseHandle h >> return ())
        $ \token ->
            withFilePath path $ \pPath ->
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
                            if ok == 0
                            then return False
                            else peek pAccessStatus
    `catch` handler
  where
    handler :: SomeException -> IO Bool
    handler e
        | Just (_ :: AsyncException) <- fromException e = throwIO e
        | otherwise = return False

-- | Windows equivalent of POSIX: access(path, R_OK). Matches @test -r@.
pathIsReadable :: FilePath -> IO Bool
pathIsReadable path = pathAccess path fILE_GENERIC_READ

-- | Windows equivalent of POSIX: access(path, W_OK). Matches @test -w@.
pathIsWritable :: FilePath -> IO Bool
pathIsWritable path = pathAccess path fILE_GENERIC_WRITE

-- | Windows equivalent of POSIX: access(path, X_OK). Matches @test -x@.
pathIsExecutable :: FilePath -> IO Bool
pathIsExecutable path = pathAccess path fILE_GENERIC_EXECUTE

isReadable :: FileTest
isReadable = withPathM pathIsReadable

isWritable :: FileTest
isWritable = withPathM pathIsWritable

isExecutable :: FileTest
isExecutable = withPathM pathIsExecutable
