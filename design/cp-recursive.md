# Recursive cp using find

Plan for `cp -r` in `Streamly.Coreutils.Cp`, built on
`Streamly.Coreutils.Find`.

Status: proposal, not implemented.

## Current state

* The `Cp` module header says "Copy a file or directory", but `cp` raises
  "cp: -r not specified; omitting directory" for a directory source.
* `find` does not provide what `cp -r` needs (it drops directories, among
  other things). The `find` side is designed in `design/find.md`.

## What cp -r needs from find

1. Directory entries, emitted before their contents (pre-order), so that
   each destination directory can be created before the files in it, and
   empty directories are copied. `cp` uses the serial DFS traversal, which
   guarantees this order. Requires `find` to output directories, as
   `findEither` with directories as `Left`, so that `cp` tells them apart
   without a `stat` of its own.
2. The path of each entry relative to the source root, to build the
   destination path: either a `find` option or `stripPrefix` in `cp`.
3. The entry type: directory, regular file, symlink, other (fifo, socket,
   device). With `-r`, GNU `cp` copies a symlink as a symlink (`-P` is the
   default with `-R`), also for a symlink given as the source. `find`
   reports symlinks together with regular files; this does not block
   `cp`, which needs the mode of each source file anyway
   (`createWithModeOf`) and so does one `lstat` per non-directory entry.
4. Errors that do not stop the traversal. GNU `cp -r` reports an
   unreadable directory, continues, and exits with a failure status.
   `find` currently ends the stream at the first unreadable directory,
   and fixing that needs a streamly-core change. Until then `cp -r` fails
   at the first unreadable directory.

## Changes to cp

### API

Add a modifier:

```haskell
recursive :: Bool -> CpOptions -> CpOptions
```

Default (without this modifier): `False`, like GNU `cp`; a directory
source raises an exception.

### Destination of the copy

With `src` a directory:

* `dest` is an existing directory, or a symlink to one, and
  `intoDirectory True`: the copy is `dest/<last component of src>`.
* Otherwise the copy is `dest`.
* The copy path exists and is not a directory: raise "cannot overwrite
  non-directory with directory".
* The copy path is an existing directory: copy into it, merging with its
  contents, like GNU `cp -r`.

### Checks before the traversal

* The copy path is `src` or is inside `src` (compare canonical paths):
  raise "cannot copy a directory into itself". Without this check the
  traversal also walks the directories it creates.

### Copying the tree

Fold over `findEither serialDfs src`. For each entry, with `rel` its path
relative to `src` and `out = copyPath </> rel`:

* `Left d` (directory): create `out` with mode `u+rwx`, so that it can be
  written even when the source directory is read-only. If `out` exists
  and is a directory, keep it. Record `(d, out)` in a list.
* `Right f`: `lstat f`.
  * Regular file: copy with `cpCopy`, applying the `Overwrite` option to
    each file.
  * Symlink: read the link target and create a symlink at `out` with the
    same target.
  * Other types: raise an exception (not supported in the first version).

After the traversal, set the mode of each recorded directory to the
source directory's mode, without special bits, minus the umask, like GNU
`cp` without `--preserve`. Walk the list in reverse, so that a directory
is changed after its subdirectories; changing a parent first could remove
the write or search permission needed to change its children.

### Symlinks

The existing `followSymlinks` behaviour of non-recursive `cp` stays as
it is. With `recursive True`, symlinks are copied as symlinks, including
a symlink given as `src`, like GNU `cp -r`. A later modifier can add
GNU `-L` and `-H`.

The `SymbolicLink` copy method (`cp -s`, create a symlink to the source)
is a different feature and stays unimplemented.

### Tests

* Tree with nested directories, an empty directory, regular files and a
  symlink: the copy has the same structure, contents and symlink targets.
* `dest` is an existing directory: the tree is copied under
  `dest/<name>`.
* `dest` is an existing directory with the same name: contents merge;
  `Overwrite` applies to each file.
* `dest` is an existing file: raises.
* Copy into a subdirectory of the source: raises.
* Read-only source directory: the copy is populated and ends up
  read-only.
* `recursive False` with a directory source: raises, as now.

## Later

* Parallel copy. The parallel traversals of `find` do not emit a
  directory before its contents to the consumer. A parallel `cp` needs a
  `find` traversal with a pre-visit action: create the destination
  directory in the step that reads the source directory, before its
  children are emitted.
* Fifos, sockets, devices.
* `--preserve` (mode, ownership, timestamps) and hard link preservation.
* GNU `-L` / `-H` symlink handling.

## Open questions

1. Fail at the first unreadable directory for now, or make the
   streamly-core change first?
2. The build does not resolve in the current nix shell: the shell has
   streamly 0.11.0, the cabal file requires `>= 0.11.1 && < 0.12`, and
   the `master` overlay selects streamly 0.12.0. Fix this before
   implementing.
