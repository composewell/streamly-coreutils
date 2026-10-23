# Error handling

The rules for how commands in streamly-coreutils report failures, and the
reasons for them. The helpers that build the errors are in
`Streamly.Coreutils.Error`.

## Rules

1. Every failure of a command raises an `IOError`. No command raises
   `ErrorCall` (`error`), a `UserError` (`userError`) or an exception type
   defined in this package.
2. A failure that GNU reports with an errno is raised with that errno.
3. A failure that GNU reports without an errno is raised with a specific
   `IOErrorType` from `GHC.IO.Exception`, e.g. `InvalidArgument` or
   `UnsupportedOperation`, never `UserError`.
4. An `IOError` raised by a dependency propagates unchanged. An exception
   of any other type from a dependency is caught and converted to
   `IOError`.
5. `error` is used only for bugs: code that no input can reach, or a
   broken internal invariant.
6. A documented miss is a value, not an exception, e.g. `False` from a
   `FileTest` predicate for a missing path. Only the specific errors named
   by the rule are caught; every other error is raised.
7. A command taking several items tries every item, like GNU, then raises
   the error of the first failing item.
8. An unimplemented feature is not exported.

## Why one exception type

One handler for `IOError` catches every failure of every command,
including failures raised by the dependencies. With a mix of exception
types, a handler for `IOError` misses some failures, and a handler for
`SomeException` also catches async exceptions.

`IOError` is what `base`, `directory` and `unix` raise, so the predicates
in `System.IO.Error` (`isDoesNotExistError`, `isAlreadyExistsError`,
`isPermissionError`) and the fields `ioe_errno` and `ioe_filename` work
without anything from this package. A `UserError` has no errno, and
`isUserError` is `True` for every `userError` and every `fail` in `IO`,
so the only way to tell one `userError` failure from another is to parse
its message.

## Fields

* Location: the command name, e.g. `"rm"`.
* File name: the path the command failed on.
* Description: `strerror` of the errno, or the GNU text where it says more
  than `strerror`, e.g. "not replacing" with `EEXIST`.

The library does not format GNU messages such as
`rm: cannot remove 'x': Is a directory`; a command line front end can
build them from these fields.

An error from a dependency keeps the dependency's function name as the
location. Re-wrapping every call to change the location costs code and
adds nothing that the errno and the file name do not already give.

## Choosing the errno or the type

Use the errno that GNU prints for the condition, or that the system call
would fail with. When one check covers two GNU conditions, e.g. "missing"
and "not a directory", tell them apart on the failure path and raise
`ENOENT` or `ENOTDIR` accordingly.

Use `NoSuchThing` (`ENOENT`) only for a path that does not exist. Callers
read `isDoesNotExistError` as "the path does not exist", so for example a
write through a dangling symlink is `InvalidArgument`, although the
target of the symlink is missing.

## Several items

GNU commands such as `mv a b c dir` process every item, report each
failure and exit with status 1. The library equivalent tries every item,
then raises the `IOError` of the first failing item, unchanged except for
the number of other failures added to its description. Arguments that
would make every item fail, such as a target that is not a directory,
are checked before any item is processed.

The single-item function is the primary API. A caller that needs every
failure maps `try` over it.

Rejected:

* An aggregate exception type holding all the errors: a handler for
  `IOError` would no longer catch every failure.
* Returning the list of errors: a caller can discard it, and a script
  continues after a failure.
* Joining all the messages into one `userError`: the type, the errno and
  the path of every failure are lost.

## Catching

Catch the narrowest exception type that covers the rule, and test the
error before turning it into a value. Never catch `SomeException`: it
also catches async exceptions such as `ThreadKilled` and timeouts. Where
a handler has to catch `SomeException`, it re-raises `AsyncException`
first.

## Pitfalls

* `ioeGetErrorString` returns the description only for `UserError`; for
  any other type it returns the name of the type. Use `ioe_description`
  from `GHC.IO.Exception`.
* `InvalidArgument`, `InappropriateType` and `UnsupportedOperation` are
  exported only by `GHC.IO.Exception`, not by `System.IO.Error`.
* `errnoToIOError` works on Windows: the C runtime defines the errno
  values and `strerror`.

## Tests

Assert on the error type or the errno, not on the message text. Check
the description only when the description is the behaviour under test.

## Adding a failure

* Find the GNU message and errno for the condition.
* Raise the error with the errno, or with the closest `IOErrorType`.
* Say in the Haddock which error is raised, e.g. "Raises an `IOError`
  with errno `EEXIST` (`isAlreadyExistsError`) if ...".
* Test the type or the errno.
