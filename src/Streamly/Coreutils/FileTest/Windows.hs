{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Coreutils.Filetest.Windows
    ( Uid
    , Gid
    , isOwnedByUserId
    , isOwnedByGroupId
    , isOwnedByCurrentUser
    , isOwnedByCurrentGroup

    , isReadableNow
    , isWritableNow
    , isExecutableNow
    ) where

import Control.Exception (bracket)
import System.Win32.Security (oWNER_SECURITY_INFORMATION)
import System.Win32.Types (failIfFalse_, HANDLE, DWORD, BOOL, PSID)

import qualified System.Win32.Security as Win32

import Foreign
import Streamly.Coreutils.FileTest.Common

newtype Uid = Uid SID
newtype Gid = Gid SID

isOwnedByUserId :: Uid -> FileTest
isOwnedByUserId (Uid uid) = withPathM $ \fp -> undefined

isOwnedByGroupId :: Gid -> FileTest
isOwnedByGroupId (Gid gid) = withPathM $ \fp -> undefined

--------------------------------------------------------------------------------
-- withFileOwnerSID
--------------------------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorOwner"
    c_GetSecurityDescriptorOwner
        :: Ptr Win32.SECURITY_DESCRIPTOR
        -> Ptr PSID
        -> Ptr BOOL
        -> IO BOOL

withFileOwnerSID :: FilePath -> (PSID -> IO a) -> IO a
withFileOwnerSID path action = do
    sd <- Win32.getFileSecurity path oWNER_SECURITY_INFORMATION

    alloca $ \ppSid ->
        alloca $ \pDefaulted -> do
            failIfFalse_ "GetSecurityDescriptorOwner" $
                c_GetSecurityDescriptorOwner sd ppSid pDefaulted

            sid <- peek ppSid
            action sid

--------------------------------------------------------------------------------
-- withEffectiveUserSID
--------------------------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentProcess"
    c_GetCurrentProcess :: IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h OpenProcessToken"
    c_OpenProcessToken
        :: HANDLE
        -> DWORD
        -> Ptr HANDLE
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetTokenInformation"
    c_GetTokenInformation
        :: HANDLE
        -> DWORD
        -> Ptr ()
        -> DWORD
        -> Ptr DWORD
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

tOKEN_QUERY :: DWORD
tOKEN_QUERY = 0x0008

tOKEN_USER :: DWORD
tOKEN_USER = 1

withEffectiveUserSID :: (PSID -> IO a) -> IO a
withEffectiveUserSID action = do
    proc <- c_GetCurrentProcess

    alloca $ \pToken -> do
        failIfFalse_ "OpenProcessToken" $
            c_OpenProcessToken proc tOKEN_QUERY pToken

        token <- peek pToken

        bracket (pure token) c_CloseHandle $ \h ->
            alloca $ \pRetLen -> do
                _ <- c_GetTokenInformation h tOKEN_USER nullPtr 0 pRetLen
                retLen <- peek pRetLen

                allocaBytes (fromIntegral retLen) $ \buf -> do
                    failIfFalse_ "GetTokenInformation" $
                        c_GetTokenInformation h tOKEN_USER buf retLen pRetLen

                    sid <- peek (castPtr buf :: Ptr PSID)
                    action sid

--------------------------------------------------------------------------------
-- Compare
--------------------------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h EqualSid"
    c_EqualSid :: PSID -> PSID -> IO BOOL

isPathOwnedByCurrentUser :: FilePath -> IO Bool
isPathOwnedByCurrentUser path =
    withFileOwnerSID path $ \fileSid ->
        withEffectiveUserSID $ \userSid ->
            (/= 0) <$> c_EqualSid fileSid userSid

isOwnedByCurrentUser :: FileTest
isOwnedByCurrentUser = withPathM isPathOwnedByCurrentUser

withFilePrimaryGroupSID = undefined
withEffectiveGroupSID = undefined

isPathOwnedByCurrentGroup :: FilePath -> IO Bool
isPathOwnedByCurrentGroup path =
    withFilePrimaryGroupSID path $ \fileSid ->
        withEffectiveGroupSID $ \userSid ->
            (/= 0) <$> c_EqualSid fileSid userSid

isOwnedByCurrentGroup :: FileTest
isOwnedByCurrentGroup = withPathM isPathOwnedByCurrentGroup

--------------------------------------------------------------------------------
-- File Access including locks
--------------------------------------------------------------------------------

-- May return false if file is open and not readable because it is locked
-- using a share mode that denies read. This is something that is additional on
-- windows, Posix does not have a feature where read access is denied due to
-- opening a file, therefore, there is nothing like being Posix compliant here,
-- we decide to implement the additional behavior in this way. This aligns with
-- the goal that readability check tells us whether we can read the file at
-- this moment.

-- | True if the file is readable by the current process.
--
-- Returns false if the file locked and not shared for reading.
--
isPathReadableNow :: FilePath -> IO Bool
isPathReadableNow path =
    (do
        let desiredAccess = Win32.fILE_READ_DATA

            -- Be as non-intrusive as possible.
            shareMode =
                    Win32.fILE_SHARE_READ
                .|. Win32.fILE_SHARE_WRITE
                .|. Win32.fILE_SHARE_DELETE

            -- Required to open directories.
            flags = Win32.fILE_FLAG_BACKUP_SEMANTICS

        h <- Win32.createFile
                path
                desiredAccess
                shareMode
                Nothing
                Win32.oPEN_EXISTING
                flags
                Nothing

        Win32.closeHandle h
        return True
    )
    `catch` \(_ :: IOException) -> return False

isReadableNow :: FileTest
isReadableNow = withPathM isPathReadableNow

-- | True if the file is writable by the current process.
--
-- Returns false if the file locked and not shared for writing.
--
isFileWritableNow :: FilePath -> FileStatus -> IO Bool
isFileWritableNow path st = do
    isDirectory <- apply st isDir
    -- On Windows, hasMode ownerWriteMode = READONLY attribute not set.
    writable <- apply st (hasMode ownerWriteMode)
    -- The READONLY attribute on directories does not prevent creating files
    -- inside the directory
    if not writable && not isDirectory
    then return False
    else do
        let desiredAccess
                | isDirectory = Win32.fILE_ADD_FILE
                | otherwise   = Win32.fILE_WRITE_DATA

            -- Be as non-intrusive as possible.
            shareMode =
                    Win32.fILE_SHARE_READ
                .|. Win32.fILE_SHARE_WRITE
                .|. Win32.fILE_SHARE_DELETE

            flags =
                if isDirectory
                then Win32.fILE_FLAG_BACKUP_SEMANTICS
                else 0

        -- Attempt to open handle with write access
        (Win32.createFile
            path
            desiredAccess
            shareMode
            Nothing
            Win32.oPEN_EXISTING
            flags
            Nothing
         >>= Win32.closeHandle >> return True)
        `catch` (\(_ :: IOException) -> return False)

isWritableNow :: FileTest
isWritableNow = withStateM isFileWritableNow

-- | Returns true if file is executable. Returns false if the file is locked
-- and not shared for execution.
isPathExecutableNow :: FilePath -> IO Bool
isPathExecutableNow path =
    (do
        let desiredAccess = Win32.fILE_EXECUTE

            -- Be as non-intrusive as possible.
            shareMode =
                    Win32.fILE_SHARE_READ
                .|. Win32.fILE_SHARE_WRITE
                .|. Win32.fILE_SHARE_DELETE

            -- Required to open directories.
            flags = Win32.fILE_FLAG_BACKUP_SEMANTICS

        h <- Win32.createFile
                path
                desiredAccess
                shareMode
                Nothing
                Win32.oPEN_EXISTING
                flags
                Nothing

        Win32.closeHandle h
        return True
    )
    `catch` \(_ :: IOException) -> return False

isExecutableNow :: FileTest
isExecutableNow = withPathM isPathExecutableNow
