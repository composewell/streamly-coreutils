# find

Design of `Streamly.Coreutils.Find` and its relation to
`Streamly.Coreutils.Ls`.

Status: find is implemented except as noted; ls is built on it.

## Current state

Implemented:

* `find`, `findEither`, `findChunked` and `findByteChunked` list the root
  and every entry under it, directories included, like GNU `find`. A root
  that is not a directory is listed by itself; a missing root raises
  ENOENT. `findEither` returns directories as `Left`.
* One `traversal :: Traversal -> ...` modifier selects one of six
  traversals: `DepthFirst` (default), `WideDepthFirstReverse`,
  `BreadthFirst`, `DepthFirstParallel`, `BreadthFirstParallel`,
  `DepthFirstParallelOrdered`. The Haddock of `Traversal` gives the order
  each one guarantees.
* `minDepth` and `maxDepth`, like GNU `-mindepth` / `-maxdepth`, with the
  root at depth 0. Each element carries its depth. `findByteChunked`
  cannot apply them to its chunks, which mix depths, and is built from
  `findChunked` when either is set.
* `maxResults` applies to all variants.
* `followSymlinks` (GNU `find -L`) and `followRootSymlink` (GNU
  `find -H`).
* A root that ends with a separator gives `dir/name`, like GNU `find`;
  the chunked readers would give `dir//name` (see
  `design/upstream-fixes.md`).
* Tests: `test/Streamly/Test/Coreutils/Find.hs`. The output of `hfd` was
  compared with GNU `find` 4.10.0 on a 21,118 entry tree, for every
  traversal and several depth limits.

Not implemented:

* Predicates (`-name`, `-type`, ...).
* Paths relative to the root (see below).
* Continuing past an unreadable directory (see Errors below).

## find and ls

GNU `find` and GNU `ls` have different jobs:

* `find` walks a tree and filters it with predicates, producing paths for
  further processing.
* `ls` presents the entries of a directory: it sorts by name, hides
  dotfiles by default (`-a` / `-A` show them), and has the `-l` long
  format, columns and `-d`.

Both are kept as library functions. Sorting and dotfile hiding are useful
programmatically ("the sorted, visible entries of this directory"), so
`ls` is not made executable-only. Only terminal rendering (`-l` text,
columns) belongs in an executable or a separate rendering function.

Split:

* `Find` owns traversal: strategies, symlink following, depth limits,
  predicates.
* `Ls` is a thin layer over `find`: depth 1, hide dotfiles by default
  with an `all` / `almostAll` modifier, sort by name. `Ls` keeps no
  traversal code of its own.
* `Ls.recursive` and `Ls.traversal` stay, as `findEither` with the Find
  traversal of the same name. Ls offers three of the seven traversals
  (`WideDepthFirstReverse`, the default, `DepthFirst`, `BreadthFirst`). A
  GNU `ls -R` equivalent also needs sorting within each directory.
* Rendering (`-l`, columns) waits until an `hls` executable is wanted.

`ls` is `findEither (followRootSymlink True . maxDepth 1)` without its
first element when that element is a directory (`Left`). This gives the
GNU `ls` operand rules:

* A directory: its entries. The root is dropped.
* A symlink to a directory: the entries of the target, as GNU `ls`
  follows a symlink given on the command line. `followRootSymlink` is
  GNU `find -H`.
* Any other path: the path itself, as `Right`, like `ls file`.

`findEither (minDepth 1 . maxDepth 1)` would give nothing for the last two.

## Remaining changes to find

### Paths relative to the root

Options:

* A `find` option that emits paths relative to the root, or emits
  `(root, relative)` pairs.
* The consumer strips the root with `stripPrefix` from
  `Streamly.Internal.FileSystem.PosixPath`.

The first keeps the prefix handling in one place and avoids joining and
then splitting every path.

### Entry types

Without `followSymlinks`, a symlink is reported as `Right`, together with
regular files. `readdir` returns `d_type`, which distinguishes them, but
`DirIO` discards it. Exposing `d_type` would need a streamly-core change.
Until then a consumer that needs the type does an `lstat` per
non-directory entry.

### Errors

A failure to open a directory (for example EACCES) ends the stream. The
`ignoreMissing`, `ignoreInaccessible` and `ignoreSymlinkLoops` options
apply only to the `stat` of a symlink target with `followSymlinks`.
Continuing past an unreadable directory, as GNU `find` does, needs a
streamly-core change: an option to skip the directory, or to report the
error in the stream. Record this in `design/upstream-fixes.md`.

### Pre-visit action

The parallel traversals do not guarantee that a directory reaches the
consumer before its contents. A traversal with a pre-visit action (run an
action in the step that reads a directory, before its children are
emitted) would let a parallel consumer rely on that.

## Open questions

1. Relative paths: a `find` option, or `stripPrefix` in the consumer?
