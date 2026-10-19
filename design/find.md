# find

Design of `Streamly.Coreutils.Find` and its relation to
`Streamly.Coreutils.Ls`.

Status: proposal, not implemented.

## Current state

* `find` and `findChunked` do not output directories. `readEitherPaths`
  and `readEitherChunks` (streamly-core, `ReadDir.hsc`) emit a directory
  only as `Left`, and `Find.hs` applies `Stream.catRights` to the output
  of `concatIterate`, which drops every `Left`. The root and empty
  directories are dropped as well. GNU `find` outputs the root and every
  directory.
* `findByteChunked` (used by `hfd`) does output directories: the `Right`
  buffer of `readEitherByteChunks` holds directories and files separated
  by newlines, and `Left` carries only the directories still to be
  traversed. It produces raw bytes, carries no entry types, and is not
  exported on Windows.
* `maxResults` is applied only in `findByteChunked`; `find` and
  `findChunked` ignore it (see `TODO.md`).
* There is no depth limit (GNU `-maxdepth`, `-mindepth`) and no
  predicates (`-name`, `-type`, ...).
* `ls (recursive True)` duplicates `find serialBfs`: both expand
  directories with `Dir.readEitherPaths`. `ls` returns `Either Path Path`
  with directories as `Left`, has none of the traversal options, and took
  1.09 s against 0.36 s for `find` on a 247,029 entry tree.
* `ls id dir` (not recursive) is a single `Dir.readEitherPaths`. `find`
  cannot express it because it has no depth limit.

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
* Remove `Ls.recursive`; a recursive walk is `find`. A GNU `ls -R`
  equivalent, if wanted later, is `find serialDfs` with sorting within
  each directory.
* Rendering (`-l`, columns) waits until an `hls` executable is wanted.

Building `ls` on `find` requires `find` to output directories and to
support `maxDepth`; otherwise depth-1 `ls` would lose its
subdirectories.

## Changes to find

### Output directories

Make `find` and `findChunked` output the root and all directories, like
`findByteChunked` and GNU `find`.

Add a variant that keeps the directory / non-directory distinction:

```haskell
findEither :: (FindOptions -> FindOptions) -> Path -> Stream IO (Either Path Path)
```

`Left` is a directory, `Right` is anything else. This is the
`concatIterate` output without `catRights`. `find` is then
`fmap (either id id) . findEither`. Consumers use the `Either` to tell
directories apart without a `stat` of their own.

### Depth limits

Add `maxDepth :: Int -> FindOptions -> FindOptions` and `minDepth`, like
GNU `-maxdepth` / `-mindepth`. The root is depth 0. `ls` uses
`minDepth 1 . maxDepth 1`.

### maxResults

Apply `maxResults` in `find` and `findChunked` too, or restrict it to
`findByteChunked` and stop exporting it (see `TODO.md`).

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

### Traversal order

The serial DFS and BFS traversals emit a directory before its contents.
The parallel traversals do not guarantee this order to the consumer.
Document the order each traversal guarantees on its modifier.

A traversal with a pre-visit action (run an action in the step that reads
a directory, before its children are emitted) would let a parallel
consumer rely on a directory being handled before its contents.

## Open questions

1. Does `find` return directories as plain `Path`s with a separate
   `findEither`, or is `findEither` the only change?
2. Relative paths: a `find` option, or `stripPrefix` in the consumer?
3. `maxResults`: implement for all variants, or drop it from the export
   list?
4. Should the traversal strategy modifiers (`serialDfs`, ...,
   `parallelOrdered`) stay exported, or be reduced to the few with
   distinct guarantees (ordered serial DFS, unordered parallel)?
