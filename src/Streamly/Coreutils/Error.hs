-- |
-- Module      : Streamly.Coreutils.Error
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Construct the 'IOError' exceptions raised by the commands in this package.

module Streamly.Coreutils.Error
    ( errnoError
    , commandError
    , throwFirstError
    , relocateErrors
    )
where

import Foreign.C.Error (Errno, errnoToIOError)
import GHC.IO.Exception (IOErrorType, IOException(..))
import System.IO.Error (ioeGetLocation, ioeSetLocation)
import Streamly.Data.Stream (Stream)
import Streamly.FileSystem.Path (Path)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Path as Path

-- = Design notes
--
-- See design/error-handling.md for the full rules and the rejected
-- alternatives.
--
-- * Every failure of a command raises an 'IOError', so that one handler
--   catches all of them and the predicates in "System.IO.Error", e.g.
--   'System.IO.Error.isDoesNotExistError', apply. 'error' is used only for
--   bugs. 'IOError's raised by dependencies are not wrapped: they already
--   carry the errno and the file name.
--
-- * The location is the command name, e.g. @"rm"@, and the file name is the
--   path the command failed on. The library does not format GNU style
--   messages; a command line front end can build them from these fields.
--
-- * A condition that GNU reports with an errno is raised with that errno,
--   so that the error type, 'ioe_errno' and the description are those of the
--   system error. Other conditions use a specific 'IOErrorType', never
--   'GHC.IO.Exception.UserError': 'System.IO.Error.isUserError' is 'True' for
--   every 'userError' and every 'fail' in 'IO', there is no way to distinguish
--   it other than the message text.
--
-- * A command taking several items tries all of them, like GNU, then
--   raises the error of the first failing item. An aggregate exception type
--   was rejected: a handler for 'IOError' would no longer catch every
--   failure. Returning the list of errors was rejected: a caller can drop it
--   silently.

-- | @errnoError cmd errno path@ is the error that a system call failing with
-- @errno@ on @path@ raises, with @cmd@ as the location.
errnoError :: String -> Errno -> Path -> IOError
errnoError cmd errno path =
    errnoToIOError cmd errno Nothing (Just (Path.toString path))

-- | @commandError errType cmd description path@ is an error without an
-- errno, for a condition that GNU reports without one, e.g. a source and a
-- destination that are the same file.
commandError :: IOErrorType -> String -> String -> Maybe Path -> IOError
commandError errType cmd description path =
    IOError
        { ioe_handle = Nothing
        , ioe_type = errType
        , ioe_location = cmd
        , ioe_description = description
        , ioe_errno = Nothing
        , ioe_filename = fmap Path.toString path
        }

-- | Raise the first of the errors, with the number of the others added to
-- its description. Do nothing for an empty list.
throwFirstError :: [IOError] -> IO ()
throwFirstError errs =
    case errs of
        [] -> return ()
        [e] -> ioError e
        e : rest ->
            ioError e {ioe_description = ioe_description e ++ more rest}

    where

    more rest =
        let n = length rest
         in " (and " ++ show n ++ " more "
                ++ (if n == 1 then "failure" else "failures") ++ ")"

-- | @relocateErrors from to stream@ changes the location of the 'IOError's
-- raised by @stream@ from @from@ to @to@, e.g. from @"find"@ to @"rm"@ for a
-- command built on another command. Errors with any other location
-- propagate unchanged.
relocateErrors :: String -> String -> Stream IO a -> Stream IO a
relocateErrors from to = Stream.handle (ioError . relocate)

    where

    relocate :: IOError -> IOError
    relocate e
        | ioeGetLocation e == from = ioeSetLocation e to
        | otherwise = e
