# GNU Coreutils for Haskell Scripting

Port useful commands from the GNU `coreutils` to Haskell functions using
streamly.

GNU `coreutils` are commonly used, well known interfaces to perform common
tasks on POSIX systems. It will be useful to have Haskell implementations of
these utilities in general and also to utilize the knowledge of these utilities
gained by programmers, admins, script writers for writing Haskell scripts.

Some of these, but not all, are available in some existing Haskell libraries in
some form or other. However, there is no comprehensive collection available,
performance may not be at par and implementations may not be streaming. The
goal of this project is to have a comprehensive streaming collection using
Streamly with performance competitive with that of the C implementations
where it matters.

One may ask why not just use fork the installed binaries and use those. In many
cases it may make sense to fork the installed  utilities to achieve the task.
However, in many other cases it may be better to use a streaming function
instead of forking a process and parsing the output.

## Scope of the Project

The goals of the project are:

1. Start with the most commonly used utilities, rather than implementing all of
   them indiscriminately. See the list of all utilities in references. Some of
   them may not even make sense to implement as Haskell functions, but unless
   it really does not make sense we can still implement the utilities to have a
   complete set for those who are used to this interface for scripting. We can
   make a decision regarding this on a case to case basis.
2. Performance of the Haskell implementation must be competitive with the C
   implementation.
3. We should try to emulate the interface as far as possible, however we can
   diverge and make sensible changes to the interface to adapt it to
   programmatic use where it makes sense and where the diversion is worth it.

The design invariant is to have the utilities composable using streaming input
and output just the way Unix commands work, reading from stdin and writing to
stdout and composed with pipes.  We should be able to chain the functions in a
stream processing pipeline.

Use the Haskell `path` package for file system path representation where needed.

## References

* https://github.com/coreutils/coreutils
* http://hackage.haskell.org/package/turtle
* https://github.com/mrak/coreutils.hs

## Prerequisites

Working knowledge of the Haskell programming language is needed, advanced
knowledge is not required. If you have a good knowledge of basics and can work
with Haskell lists that should be enough. Familiarity with POSIX systems calls
and Haskell C FFI may be useful.

## Difficulty Level

Normal

## Mentors

* Adithya Kumar
* Harendra Kumar
