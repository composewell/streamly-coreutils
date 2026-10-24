# streamly-coreutils

Haskell functions that do the work of the GNU coreutils commands: `cp`,
`mv`, `rm`, `ln`, `mkdir`, `touch`, `chmod`, `ls`, `find`, `stat`,
`test`, `readlink`, `realpath`, `which`, `tail` and others. Use them in
Haskell scripts and programs instead of calling the commands through a
shell, or instead of assembling the same behaviour from `directory`,
`filepath` and `unix`.

* Each function is named after its command and its options after the
  command's flags: `rm (recursive True . force True)` is `rm -rf`,
  `mkdir (makeParents True)` is `mkdir -p`. Translating a shell script is
  mostly mechanical.
* Defaults and error behaviour follow GNU coreutils, including symbolic
  links, trailing slashes, write-protected files and missing paths.
  Where a function differs from GNU, for example `rm -r` on a symbolic
  link with a trailing slash, which here deletes only the link, the
  difference is documented.
* The utilities run no subprocesses: no quoting of arguments, no parsing
  of text output. Errors are Haskell exceptions, and the code also runs on
  Windows, where the commands are not installed. `Streamly.Coreutils.Sh` is
  the exception: it runs shell commands.
* `ls`, `find` and `tail` return [streamly](https://github.com/composewell/streamly)
  streams. You can filter, map, take the first few entries or fold them as
  they are produced, in constant memory, even for a directory tree with
  millions of files.
* `find` lists a directory tree faster than GNU `find` and Rust `fd`,
  even with a serial traversal (see the table below).
* Where a utility's work can be done concurrently, it is, or will be in a
  later release. `find` already has concurrent traversals, which read
  several directories at the same time: `find (traversal Parallel)`
  instead of `find id`. Other utilities that walk a directory tree, such as
  recursive `ls` and `rm`, will get concurrent versions. The results are
  streams, so your own processing of them can be made concurrent too,
  e.g. with `parMapM` from streamly.
* File tests compose like the operands of `test`, and a combined test
  makes a single `stat` call: ``test p (isFile `and_` size (> 10240))``.
* When you do need a shell command, `Streamly.Coreutils.Sh` runs it and
  connects its input and output to Haskell streams. It uses `/bin/sh`, and
  on Windows `sh` from the `PATH`, e.g. from Git for Windows.

## Example

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.Function ((&))
import Streamly.FileSystem.Path (path)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

import Streamly.Coreutils.Cp (cp)
import Streamly.Coreutils.FileTest (test, isFile, size, and_)
import Streamly.Coreutils.Find (Traversal(..), find, traversal)
import Streamly.Coreutils.Mkdir (mkdir, makeParents)
import Streamly.Coreutils.Rm (rm, recursive, force)

main :: IO ()
main = do
    -- mkdir -p backup/src
    mkdir (makeParents True) [path|backup/src|]

    -- cp README.md backup/README.md
    cp id [path|README.md|] [path|backup/README.md|]

    -- find src -type f -size +10k | wc -l
    -- with directories read concurrently
    n <- find (traversal Parallel) [path|src|]
        & Stream.filterM (\p -> test p (isFile `and_` size (> 10240)))
        & Stream.fold Fold.length
    print n

    -- rm -rf backup
    rm (recursive True . force True) [path|backup|]
```

Every function takes its options as a modifier: pass `id` for the GNU
defaults, or compose modifiers with `.` as above. The Haddock of each
modifier gives the default it changes.

Shell commands can be used in the same way when there is no Haskell
equivalent:

```haskell
import Data.Function ((&))
import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.Data.Stream as Stream
import Streamly.Coreutils.Sh (toBytes, pipeBytes)

-- echo hello | tr '[a-z]' '[A-Z]'
main :: IO ()
main =
    toBytes "echo hello"
        & pipeBytes "tr '[a-z]' '[A-Z]'"
        & Stream.fold Stdio.write
```

## Performance

Time to list a directory tree of 258,310 entries, best of 5 runs, with
output to `/dev/null`. Symbolic links are not followed. The machine has
2 CPUs, the file system is ext4 and the directory cache is warm.

| Command | Traversal | Time |
|---------|-----------|------|
| `find .` (GNU findutils 4.10.0) | serial | 0.68 s |
| `fd -u -j1 .` (fd 10.5.0) | serial | 0.88 s |
| `fd -u .` | 2 threads | 0.48 s |
| `hfd --dfs .` | serial | 0.41 s |
| `hfd --parallel .` | concurrent | 0.34 s |

`hfd` is a small command line driver for `Streamly.Coreutils.Find`,
included in this package. Build it with `cabal build -f hfd hfd`.

`find` has eight traversals, selected with the `traversal` modifier, e.g.
`find (traversal BreadthFirst)`. Five are serial: depth-first (the GNU
`find` order and the default), depth-first using stream append, wide
depth-first, breadth-first and interleaved. Three read directories
concurrently: unordered, interleaved, and ordered, which gives the
depth-first order. Streamly's stream combinators make each of them a few
lines of code, so other orders can be added the same way.

## Commands

| Command | Module |
|---------|--------|
| `cp` | `Streamly.Coreutils.Cp` |
| `mv` | `Streamly.Coreutils.Mv` |
| `rm` | `Streamly.Coreutils.Rm` |
| `ln` | `Streamly.Coreutils.Ln` |
| `mkdir` | `Streamly.Coreutils.Mkdir` |
| `touch` | `Streamly.Coreutils.Touch` |
| `chmod` | `Streamly.Coreutils.Chmod` |
| `ls` | `Streamly.Coreutils.Ls` |
| `find` | `Streamly.Coreutils.Find` |
| `stat` | `Streamly.Coreutils.Stat` |
| `test`, `[ ... ]` | `Streamly.Coreutils.FileTest` |
| `readlink` | `Streamly.Coreutils.ReadLink` |
| `dirname` | `Streamly.Coreutils.Dirname` |
| `which` | `Streamly.Coreutils.Which` |
| `tail` | `Streamly.Coreutils.Tail` |
| `cut` | `Streamly.Coreutils.Cut` |
| `cd`, `pwd`, `$HOME` | `Streamly.Coreutils.Directory` |
| `id` | `Streamly.Coreutils.Id` (not on Windows) |
| `sleep` | `Streamly.Coreutils.Sleep` |
| `sh -c`, pipes | `Streamly.Coreutils.Sh` |

Not every flag of every command is implemented yet. The module
documentation lists the ones that are.

The package builds on Linux, macOS, FreeBSD and Windows, with GHC 8.10.7
to 9.14.1.

## API stability

The API is still changing, and later releases will have breaking
changes: function signatures, option names and module organization may
all change.
