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
* file stat: test, testl
* file access control: acls, getxattr
* symlinks: readlink, resolvePath
* path: makeAbsolute, canonicalize, resolvePath
* Process/OS environment: pwd, cd, id, findInPATH, home, xdg, temp
* UserDB: id, home
