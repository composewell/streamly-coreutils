/*
 * Rename that fails instead of replacing an existing destination.
 */

#if defined(_WIN32)

#include <windows.h>

/*
 * Returns 0 on success, otherwise the GetLastError() code, read here so that
 * nothing can overwrite it before the caller sees it. ERROR_FILE_EXISTS or
 * ERROR_ALREADY_EXISTS mean the destination exists.
 */
DWORD streamly_coreutils_rename_noreplace_w(LPCWSTR src, LPCWSTR dst)
{
    /* Without MOVEFILE_REPLACE_EXISTING, MoveFileExW does not replace. */
    if (MoveFileExW(src, dst, 0)) {
        return 0;
    }
    return GetLastError();
}

#else

/*
 * Returns 0 on success, -1 with errno set on failure. EEXIST means the
 * destination exists; ENOSYS means the platform has no such call.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#if defined(__linux__)
#include <sys/syscall.h>
#ifndef RENAME_NOREPLACE
#define RENAME_NOREPLACE (1 << 0)
#endif
#endif

int streamly_coreutils_rename_noreplace(const char *src, const char *dst)
{
#if defined(__linux__) && defined(SYS_renameat2)
    /* syscall() instead of renameat2(), which needs glibc >= 2.28. */
    return syscall(SYS_renameat2, AT_FDCWD, src, AT_FDCWD, dst,
                   RENAME_NOREPLACE);
#elif defined(__APPLE__) && defined(RENAME_EXCL)
    return renamex_np(src, dst, RENAME_EXCL);
#else
    (void)src;
    (void)dst;
    errno = ENOSYS;
    return -1;
#endif
}

#endif
