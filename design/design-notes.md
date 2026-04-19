## Goals

* Replace shell scripting with Haskell
* Use Haskell library instead of installing and using coreutils package
* Platform independent (Linux/MacOS/Windows)
* Concurrency support

## Input and output objects

The utilities must be written as Haskell functions that take an input object
and produce an output object. The input is usually the "config" for the request
and output is the serialized data (e.g. json or Haskell structure).

For raw haskell functions, the config would be a Haskell structure and the
output would also be a Haskell structure.

Traditionally the config is specified using the CLI arguments or a config file.
And output is more of a human readable format.

We can write adaptors for the Haskell functions to:

* parse the CLI arguments or config files into a Haskell input structure
* render the Haskell output to json or human readable format of traditional
  utilities.

The parsing can be done statically when possible e.g. using the TH quotes as
sugegsted above.

We can basically wrap the Haskell functions in CLI parser and an output
renderer to make a Unix like utility.

## Module Naming

We can either keep each utility in its own module or bundle them into a smaller
set of modules. The latter would help reduce the imports. But it may be easier
to remember the module names if they are based on the utility name.

We should keep the naming non-conflicting such that if some wants to bundle
them in a single module and re-export it should be possible. In fact we can
provide a single coreutils module exporting everything.

Each command must have its own module e.g. Coreutils.Cp for the cp command.

The command options could be called "Options" in each module but then
we may not be able to export all commands from a single module which is
desirable. So we can choose to call the options of a command by the same
name as the command itself e.g. "CpOptions".  We would not expose the
CpOptions record outside the module but we would still need to export
the type. Using a unique type for each command would help us use it
unqualified.

The command runner would be called "cp" so it would be "cp :: CpOptions -> IO ()".

## API for utilities

There are two ways of writing the API, (1) use separate functions
for each combination of options e.g. rm, rmForce, rmRecursive,
rmRecursiveForce (2) use options to control the behavior e.g. rm id,
rm force, rm recursive, rm (recursive . force). We choose the latter
because the number of functions can explode with more combinations,
and in the latter we need to remember only one function and use any
combination of options. That keeps the organization better.

The downside of using a single multiway function with options is
that we dynamically select a branch which we could have selected
statically. However depending on the function and usecase this may be
insignficant, in cases where it makes sense we can also select different
functions rather than a single multiway function.

## Directory Operations

Summary of important dir and file operations:
* dir read operations (readdir, readdir with stat)
* dir modify operations (file create, delete, rename)
* file inode read operations:
  * stat, lstat, fstatat
* file inode modify operations:
  * permissions (chmod)
  * ownership (chown)
  * timestamps (utime)
* file access control (access, acls, getxattr)

## Directory Package

The directory package provides a potpourri of operations:
* dir modify operations (mkdir, rmdir, rm, recursive rm, mv, cp),
* readdir operations, recursive traversal (listDirectory, findFiles),
* file stat (getFileSize, permissions, timestamps, isSymlink),
* symlink read,
* path (makeAbsolute, canonicalize),
* process environment (pwd, cd, PATH, findExecutable),
* OS environment (home, xdg, temp).

## Classification

We can define the following categories based on underlying filesystem
functionality:
* readdir: FileSystem.DirIO module
* Dir traversal: ls, find, cp -r, rm -r
* Dir modify: mkdir, rmdir, rm, mv, cp
* stat: test, testl
* symlink read: readlink
* path: makeAbsolute, canonicalize
* environment: pwd, cd, findInPATH, home, xdg, temp
* access control: acls, getxattr
