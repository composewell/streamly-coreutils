# Upstream fixes and requests

This file lists the places where streamly-coreutils works around a
limitation of the streamly libraries, and the things we implement locally
that belong in, or would be easier with, an upstream library. When an
upstream fix is released, remove the local workaround and the entry here.

Versions in use when this was written: streamly-core 0.3.0, streamly
0.11, streamly-process 0.4.1.

Each entry gives the problem, what we do about it here, and what we need
upstream.

## streamly-core

### FileSystem: no rename that refuses to replace the destination

* Problem: neither streamly-core nor `directory` has a rename that fails if
  the destination exists. Checking first and then renaming replaces a
  destination created in between.
* Here: `Streamly.Coreutils.Rename.renameNoReplace`, with a C stub
  (`cbits/rename_noreplace.c`) calling `renameat2` with
  `RENAME_NOREPLACE` on Linux, `renamex_np` with `RENAME_EXCL` on
  macOS, and `MoveFileExW` without `MOVEFILE_REPLACE_EXISTING` on
  Windows.
* Upstream: a no-replace rename in streamly-core's file system API.

### Path: asOsCString is not exported for Path

* Problem: `asOsCString` is defined for the portable `Path` but not
  exported, so a `Path` cannot be passed to a C function directly.
* Here: `renameNoReplace` uses `Array.asCStringUnsafe (Path.toArray p)`
  from `Streamly.Internal.Data.Array`.
* Upstream: export `asOsCString` from `Streamly.FileSystem.Path`.

### Path: takeFileName returns Nothing with trailing separators

* Problem: `Path.takeFileName` (via `splitFile`) returns `Nothing` for
  `x/`. GNU `mv x/ dir` moves `x` to `dir/x`, using the last component
  with the trailing separators dropped, as `basename` does.
* Here: `targetInDir` in `Streamly.Coreutils.Overwrite`, used by `cp` and
  `mv`, calls `dropTrailingSeparators` from the internal module
  `Streamly.Internal.FileSystem.Path` before `Path.takeFileName`.
  `checkTrailingSeparator` in `Mv.hs` uses `hasTrailingSeparator` and
  `dropTrailingSeparators` from the same module to test whether `x/`
  names a symlink.
* Upstream: export `hasTrailingSeparator` and `dropTrailingSeparators`
  from `Streamly.FileSystem.Path`, and a variant of `takeFileName` with
  `basename` semantics.

### Path: splitRoot leaves the UNC share name out of the root

* Problem: in streamly-core 0.3, `splitRoot` on Windows gives `\\srv\`
  as the root of `\\srv\share\a`, and `\\?\UNC\srv\` as the root of
  `\\?\UNC\srv\share\a`. The share name belongs to the root, as in
  filepath's `splitDrive`. `dirname \\srv\share\` gave `\\srv`.
* Here: `splitRoot` in the internal module `Streamly.Coreutils.Path`,
  for `streamly-core < 0.4`, moves the share name and the separators
  after it into the root. Used by `Dirname` and `ResolvePath`.
* Upstream: fixed on master by "Make UNC share name part of root in
  splitRoot" (cce03c5e0), not yet released. Remove the workaround when
  the minimum streamly-core is 0.4.

### Path: no lexical collapsing of `..` and no prefix stripping

* Problem: streamly-core 0.3 has no function to collapse `..` components
  lexically (`realpath -s`, `realpath -L`), or to strip a leading path
  from another at component boundaries (`realpath --relative-to`,
  `--relative-base`).
* Here: `ResolvePath.hs` implements `lexicalCollapse` with `splitRoot`,
  `Path.splitPath_` and `Path.join`, and `stripComponents`, `isPathUnder`
  and `makeRelative` by comparing the components with `eqPathBytes`.
* Upstream: added on master as `collapseDotDots`, `stripPrefix` and
  `takeCommonPrefix`, not yet released. `stripPrefix` gives `Nothing` for
  equal paths as well as for a path that is not under the prefix, so
  `makeRelative` still has to return `"."` for equal paths. Replace the
  local functions when the minimum streamly-core is 0.4.

### Path: no path resolution on `Path`

* Problem: streamly-core 0.3 has no function that makes a `Path`
  absolute or resolves its symlinks and `..` components, like
  `realpath`.
* Here: `resolvePath` in `ResolvePath.hs` converts with `Path.toString`
  and calls `makeAbsolute` and `canonicalizePath` from `directory`.
  `canonicalizePath` resolves the longest existing prefix of the path and
  appends the missing components unchanged, which `RequireParents` (the
  default) and `RequireNone` depend on.
* Upstream: master has the internal module
  `Streamly.Internal.FileSystem.PathIO` with `getCurrentDirectory`,
  `setCurrentDirectory` and `makeAbsolute` implemented, and `resolve` and
  `resolveDotDots` defined as `undefined`. `resolve` is documented as
  POSIX `realpath(3)`, which fails if any component is missing. We need:
  * `resolve` implemented, with an option, or a variant, that resolves
    the longest existing prefix and appends the missing components, like
    `canonicalizePath` and GNU `realpath -m`.
  * `PathIO` exported as a public module.

### Path: no search path splitting

* Problem: streamly-core has no function to split the `PATH` environment
  variable into paths. filepath's `getSearchPath` does this, including
  the platform separator (`:` or `;`), an empty entry meaning `.` on
  POSIX, and quoted entries on Windows.
* Here: `whichAll` in `Which.hs` uses `getSearchPath` from filepath.
* Upstream: a `getSearchPath` returning `[Path]`, or a function that
  splits a search path string into paths.

### Path: no way to write paths as lines

* Problem: printing a stream of `Path` one per line, as `find` does,
  takes `fmap Path.toArray`, then `unfoldEachEndBy 10 Array.reader`
  (internal) to add the newlines, then `Handle.writeWith`. On Windows
  `Path.toArray` gives UTF-16LE `Word16` units, so the stream also needs
  `encodeUtf8 . decodeUtf16le` before it can be written to a byte handle.
  The pipeline differs between platforms.
* Here: the Haddock of `find` and `findChunked` in `Find.hs` shows the
  pipeline and the Windows step. `findByteChunked` produces
  newline-terminated UTF-8 bytes directly, but only on POSIX.
* Upstream: one of the following.
  * A `Path` to UTF-8 bytes conversion that is a no-op on POSIX, e.g.
    `Path.toUtf8 :: Path -> Array Word8`.
  * A fold that writes `Path`s to a handle as newline-terminated lines.
  * Export `unfoldEachEndBy` from `Streamly.Data.Stream`.

### DirIO: broken symlinks are dropped when following symlinks

* Problem: with `followSymlinks True` and `ignoreMissing True`, a broken
  symlink is left out of the output of `readEitherPaths` and the chunked
  readers. The documentation of `ignoreMissing` says the entry is reported
  as not a directory, so it should still be listed. Observed with `hfd -L`
  on a tree containing `broken -> nowhere`: `find -L` lists it, `hfd -L`
  does not.
* Cause: `statEntryType` in `Streamly/Internal/Syscall/Posix/ReadDir.hsc`
  returns `EntryIgnored` when `stat` fails with ENOENT, EACCES or ELOOP
  and the corresponding option (`ignoreMissing`, `ignoreInaccessible`,
  `ignoreSymlinkLoops`) is set, and the readers skip `EntryIgnored`
  entries. The documentation of all three options says the entry is
  reported as not a directory, so a symlink to an inaccessible target or
  a symlink loop is dropped too.
* Here: documented as a caveat in the Haddock of
  `Streamly.Coreutils.Find.followSymlinks`.
* Upstream: return `EntryIsNotDir` instead of `EntryIgnored` for these
  errors, so that the entry is listed as a non-directory, as documented.

### DirIO: no directory cycle detection when following symlinks

* Problem: when symlinks are followed, a symlink to an ancestor directory
  (`d/up -> ..`) makes a recursive traversal descend until path resolution
  fails with ELOOP, about 40 levels deep, listing the same entries at each
  level. With `ignoreSymlinkLoops True`, the setting `Find` uses, the
  `stat` of the link at that depth fails, the link is dropped, and the
  traversal ends there. GNU `find -L` remembers the device and inode of
  each directory it enters and skips a directory it has already visited.
* Here: `followSymlinks` in `Find` documents that there is no loop
  detection. The default does not follow symlinks.
* Upstream: either an option in the directory reading or traversal API to
  skip directories already visited, or return the device and inode from
  the `stat` that `followSymlinks` already does, so that the caller can
  keep the visited set.
* Reported: https://github.com/composewell/streamly/issues/3131

### DirIO: chunked readers double the separator after a directory

* Problem: `readEitherChunks` and `readEitherByteChunks` always put a
  separator between the directory and the entry name, so a directory
  `dir/` gives `dir//name` and `/` gives `//name`. GNU `find` gives
  `dir/name` and `/name`, and `readEitherPaths` (through `Path.join`) does
  the same.
* Here: `findChunked` and `findByteChunked` read a root that ends with a
  separator with `readEitherPaths`, and use the chunked readers from depth
  1 on, where directory paths never end with a separator.
* Upstream: do not add a separator after a directory path that ends with
  one.

### DirIO: readEitherByteChunks mixes depths in a chunk

* Problem: `readEitherByteChunks` reads the directories it is given and
  also the subdirectories it finds in them. Each subdirectory found goes
  on the reader's own pending list, and only when the 32 KB buffer is full
  does the reader return half of that list as `Left`. One call can
  therefore read far below the depth of the directories it was given, and
  a chunk can hold entries at several depths.
* Counting path components does not fix it. The depth of a line is the
  number of `/` bytes in it minus the number in the root, so the caller
  can drop the lines outside the depth limits, at the cost of a scan of
  every chunk and of splitting the chunks it keeps part of. This is
  enough for `minDepth`. It is not enough for `maxDepth`: the reader has
  already read the directories below `maxDepth`. With `maxDepth 1` on
  `/`, it would read most of the file system and the caller would discard
  the lines.
* Here: with `minDepth` or `maxDepth` set, `findByteChunked` is built from
  `findChunked` instead, which is about 22% slower.
* Upstream: an option (the `XXX` note on `readEitherByteChunks` already
  lists "recursive traversal" as a config option) to read only the
  directories given and return every subdirectory found as `Left`,
  without reading it. When all the input directories are at depth `d`,
  every entry in a `Right` chunk and every directory in a `Left` is then
  at depth `d + 1`, so the caller can tag each chunk and each `Left` with
  its depth, drop chunks below `minDepth`, and not read the `Left`
  directories at `maxDepth`. The reader can still fill one buffer from
  several of the input directories. A maximum depth passed to the reader
  would serve as well.

### DirIO: an unreadable directory ends the traversal

* Problem: a failure to open or read a directory, e.g. EACCES, raises an
  exception from the directory readers and ends the stream. GNU `find`
  and `ls -R` report the directory and continue with the rest of the tree.
* Cause: the `ReadOptions` `ignoreInaccessible`, `ignoreMissing` and
  `ignoreSymlinkLoops` apply only to the `stat` that `statEntryType` in
  `Streamly/Internal/Syscall/Posix/ReadDir.hsc` does to classify an entry
  (a symlink with `followSymlinks True`, or an entry of type
  `DT_UNKNOWN`). `openDirStream` calls `opendir` through
  `throwErrnoPathIfNullRetry`, which raises the exception whatever the
  options are. `readEitherPaths`, `readEitherChunks` and
  `readEitherByteChunks` all open directories this way.
* A caller cannot work around this well. `readEitherPaths` reads one
  directory, so the exception can be caught around it. `readEitherChunks`
  takes a list of directories and `readEitherByteChunks` opens the
  subdirectories it finds in its own loop, so an exception from one
  directory loses the rest of the batch; catching it per directory needs
  one reader call per directory, which gives up batching.
* Here: `Find` sets all three ignore options to `True`, so that `stat`
  errors on entries do not end the stream, and documents the unreadable
  directory caveat. `find` and `ls` end with the exception (see `TODO.md`,
  Error semantics).
* Upstream: make `ignoreInaccessible` (or a new option) apply to
  `opendir` and `readdir` failures too, skipping the directory and
  continuing with the rest of the batch. Optionally return the error in
  the stream, e.g. as a third kind of element, so that a caller can report
  it like GNU `find`.

### DirIO: chunked directory readers are POSIX only

* Problem: `readEitherChunks` has a fast custom implementation only on
  POSIX, and `readEitherByteChunks` does not exist on Windows (see the
  `XXX` note in `Streamly/Internal/FileSystem/DirIO.hs`).
* Here: `findByteChunked`, the fastest `find` variant, is not exported on
  Windows. `hfd` is not buildable on Windows.
* Upstream: Windows implementations of `readEitherChunks` and
  `readEitherByteChunks`.

### Stream: unfold tree iterations open every pending directory

* Problem: in streamly-core 0.3, the unfold tree iterations
  (`unfoldIterateDfs`, `altBfsUnfoldIterate`, `bfsUnfoldIterate`) run the
  unfold's inject as soon as an element is emitted and keep the result.
  For `DirIO.eitherReaderPaths` the inject is `opendir`, so every pending
  directory holds a descriptor. For a directory with 51 subdirectories,
  `bfsUnfoldIterate` had 52 descriptors open; a wide tree exceeds the open
  file limit (EMFILE). streamly master keeps the seeds instead and runs
  the inject only when the directory is read.
* Here: `Find`, and `Ls` through it, use the concat iterations
  (`StreamK.concatIterateWith`, `altBfsConcatIterate`,
  `bfsConcatIterate`), which store unstarted streams. `bfsConcatIterate`
  and `altBfsConcatIterate` keep one descriptor open.
* Upstream: fixed on master. Once released, the unfold iterations can be
  used where fusion matters.

### Stream: wide DFS only in reverse order

* Problem: streamly-core 0.3 has wide DFS only as `altBfsConcatIterate`,
  which descends into the subdirectories of a directory in the reverse of
  the order they are listed. streamly master renames it
  `wideDfsConcatIterateRev` (keeping `altBfsConcatIterate` as a deprecated
  name) and adds `wideDfsConcatIterate`, which descends in listing order.
* Here: `Find` uses `altBfsConcatIterate` for `WideDepthFirstReverse`.
* Upstream: fixed on master. Once released, use `wideDfsConcatIterate`.

### FileIO: no way to give a new file a mode

* Problem: the `FileIO` write functions (`fromChunks`, `withFile`) open the
  file with the default creation mode. `cp` must create the destination
  with the permission bits of the source, minus the umask, like GNU `cp`.
  Setting the mode after writing is not enough: a read-only source would
  give a read-only destination that cannot be written.
* Here: `copyToNew` in `Cp.hs` reads the source mode with
  `System.Posix.Files.getFileStatus`, creates the file with
  `System.Posix.IO.createFile`, converts the `Fd` to a `Handle` with
  `fdToHandle`, and writes with `Streamly.FileSystem.Handle.writeChunks`.
  Both calls convert the paths with `Path.toString`. On Windows it uses
  `File.fromChunks`, which creates the file with the default attributes.
* Upstream: write functions in `FileIO` that take creation options: the
  mode of a new file, and flags such as exclusive create (`O_EXCL`) and
  no-follow (`O_NOFOLLOW`).

### FileIO: no public withFile that runs an IO action

* Problem: the public `Streamly.FileSystem.FileIO.withFile` takes a
  `Handle -> Stream m a` action and returns a stream. Opening a file for
  an `IO` action, for example to create an empty file, needs a dummy
  stream and a fold to drain it.
* Here: `touch` in `Touch.hs` uses `withFile :: Path -> IOMode -> (Handle
  -> IO r) -> IO r` from `Streamly.Internal.FileSystem.Posix.File` or
  `Streamly.Internal.FileSystem.Windows.File`, chosen with CPP.
  `copyToNew` in `Cp.hs` must open the source before it creates the
  destination, so that a missing source leaves no destination behind.
  The stream returned by `FileIO.withFile` opens the file only when the
  stream is run, after `fromChunks` has created the destination, so
  `copyToNew` opens the source with `withBinaryFile` from the same
  internal modules.
* Upstream: a public, cross-platform `withFile` (and `openFile`) on
  `Path` that runs an `IO` action, like `System.IO.withFile`.

### File system operations on `Path`

* Problem: streamly-core has no `Path` based functions for `stat`,
  `lstat`, `chmod`, `utimes`, `link`, `symlink`, `readlink`, `unlink`,
  `rename`, `mkdir` and `rmdir`. We call `directory`, `unix` and
  `unix-compat`, which take `FilePath`, so every call converts with
  `Path.toString`: about 60 calls in 17 modules. The conversion decodes
  the path, which can fail or change a path that is not valid in the
  current encoding. `Chmod.hs` has `XXX do not use toString` notes.
* Here: `Path.toString` and `Path.fromString` at each call.
* Upstream: one of the following.
  * `Path` based versions of these operations in streamly-core.
  * Zero cost conversion between `Path` and `OsPath`, so that we can use
    the `OsPath` API of `directory` and `unix`. The `Path` documentation
    says that `streamly-filepath` provides this conversion; we do not use
    it yet.

### Reading the end of a file

* Problem: there is no function to read the last N lines or bytes of a
  seekable file without reading all of it, to read a file backwards in
  chunks, or to skip the first N lines or bytes of a stream of chunks.
* Here: `Tail.hs` implements these locally: `lastLinesOffset` reads 64 KiB
  blocks backwards from the end with `hSeek` and counts newlines;
  `tailStream` keeps the last chunks of a pipe in a bounded queue;
  `dropLines` and `dropBytes` skip a prefix of a chunk stream.
* Upstream: a reverse chunk reader for seekable files (`readChunksRev`),
  reading from a byte offset (`pread`), and stream combinators to take or
  drop the first or last N separator terminated records or N bytes of a
  stream of arrays.

### Fold: no sliding window bounded by a measure

* Problem: there is no fold that keeps the shortest suffix of a stream
  whose total measure is at least n. The `RingArray` sliding window holds
  a fixed number of `Unbox` elements, which does not fit chunks of
  different sizes or a count of records that can span any number of
  chunks.
* Here: `Tail.hs` implements it for a non-seekable input with `Queue`,
  `pushQueue`, `queueChunks` and `queueTotal`: a two-list FIFO of chunks,
  each with its measure (`Array.length` for `tail -c`, `countNewlines`
  for `tail -n`), dropping chunks from the front while the rest still
  measure at least n.
* Upstream: a fold such as
  `takeEndByMeasure :: Monad m => (a -> Int) -> Int -> Fold m a [a]`.
  `tail -c n` and `tail -n n` on a stream of arrays are this fold
  followed by trimming the first chunk.

### Array: no search for the n-th occurrence of an element

* Problem: there is no direct way to find the index of the n-th
  occurrence of an element in an array, counted from the start or from
  the end. `Array.revBreakEndBy` finds only the last match, and its
  result is the same when nothing matches and when the last element
  matches. Composing `Array.readRev`, `Scanl.findIndices`,
  `Stream.take n` and `Fold.tee Fold.length Fold.latest` works but is
  hard to read. `breakEndByWord8_` uses `memchr` and searches forward
  only; there is no `memrchr` based reverse search.
* Here: `afterNewline` and `afterNewlineRev` in `Tail.hs` loop over the
  indices with `unsafeGetIndex` and `unsafeGetIndexRev` and count
  newlines.
* Upstream: functions that return the index of the n-th match from the
  start and from the end, or the number of matches when there are fewer
  than n, e.g. `findIndexN` and `findIndexRevN`, with a `memchr` /
  `memrchr` fast path for `Word8`.

### Handle: no read of an exact number of bytes

* Problem: `Streamly.Internal.FileSystem.Handle.getChunkOf`, documented
  as reading exactly the given number of bytes, is `undefined`.
  `getChunk` uses `hGetBufSome` and may return fewer bytes. The stream
  alternative, `MutArray.compactExact`, is also `undefined`. Folding
  `Handle.read` into `Array.createOf len` works but goes through a
  `Word8` stream.
* Here: `readBlock` in `Tail.hs` allocates the array with the internal
  `MutArray.unsafeCreateWithPtr'`, fills it with `System.IO.hGetBuf`
  (which reads until it has the requested bytes or reaches EOF) and
  freezes it with the internal `unsafeFreezeWithShrink`.
* Upstream: implement `getChunkOf` and export it from
  `Streamly.FileSystem.Handle`.

### Array slicing in the public API

* Problem: the public `Streamly.Data.Array` has no slicing function.
  `sliceOffLen` and `unsafeSliceOffLen` exist only in
  `Streamly.Internal.Data.Array`.
* Here: `dropArray` in `Tail.hs` copies the remaining part of an array
  through a stream (`Array.read`, `Stream.drop`, `Array.create`) to avoid
  the internal module. `Find.hs` uses the internal `unsafeSliceOffLen`.
* Upstream: export a slicing function (e.g. `sliceOffLen`, or `splitAt`)
  from `Streamly.Data.Array`.

### Take N lines from a stream of byte chunks

* Problem: `findByteChunked` produces byte chunks with several newline
  separated paths in each chunk. To stop after N paths, `maxResults` must
  count newlines across chunks and cut the last chunk.
* Here: `takeLines` in `Find.hs`, built from `Scanl`, `postscanlMaybe`, a
  counting fold and `Array.unsafeSliceOffLen`. A comment there notes that
  `Scanl` is awkward for this and that a stateful map (`smapM`) would fit
  better.
* Upstream: a combinator to take the first N separator terminated records
  from a stream of arrays, or `smapM`.

### Text helpers implemented in `Cut`

* Problem: `Cut.hs` implements generic helpers and marks them as belonging
  elsewhere:
  * `decodeLatin1 :: Array Word8 -> [Char]` ("XXX should go in
    Unicode.Array.decodeLatin1?").
  * `readLines` ("XXX this should go in unicode text file reading module -
    Unicode.FileIO").
  * `splitByWord8`, which uses `Array.splitEndBy_` and notes that it
    should use `splitSepBy_` to get the right number of fields.
  * `splitLine`, which splits a line on the UTF-8 bytes of the delimiter.
    There is no `Array.splitSepBy_`: `Array.splitEndBy_` drops the empty
    field after a final delimiter and gives no field for an empty line,
    so `splitLine` adds them back. There is no array split on a sequence
    of elements either: a multi-byte delimiter uses
    `Stream.splitSepBySeq_` on `Array.read` with `Array.create`, which
    copies each field instead of slicing the line.
  * `encodeChar :: Char -> Array Word8`, the UTF-8 encoding of a
    character, as `Array.fromPureStream . Stream.unfold
    Unicode.readCharUtf8`.
* Here: implemented locally; all but `splitLine` and `encodeChar` are
  exported from `Cut`.
* Upstream: a `Unicode.Array` module with `decodeLatin1` and a UTF-8
  encoding of a `Char` to an array, `Array.splitSepBy_` and an
  `Array.splitSepBySeq_` that return slices, and a text file reading
  module with `readLines` and field splitting. The sequence splits need
  `Stream.indexEndBySeq_` and `Stream.indexSepBySeq_`, which yield the
  (offset, length) of each field, and `Array.splitEndBySeq_` and
  `Array.splitSepBySeq_` that slice the array with these pairs, as
  `Array.splitEndBy_` does with `indexEndBy_`.

### Array: no infix separator split that returns slices

* Problem: there is no `Array.splitSepBy_`; it is commented out of the
  export list of `Streamly.Internal.Data.MutArray.Type`. The array
  splitters are `splitEndBy_` and `splitEndBy`, which return slices, and
  the single break functions `breakEndBy_` and `revBreakEndBy_`. The
  result of `breakEndBy_` is the same when the separator is the last
  element and when there is no separator: the second array is empty in
  both cases. So `breakEndBy_` cannot be used to build an infix split.
* `Stream.splitSepBy_` on `Array.read` gives the right number of fields,
  but each field is copied into a new array by the `Array.create` fold.
  Measured with the `Streamly.Benchmark.Coreutils.Cut` benchmark (100,000
  lines of 10 tab separated fields, streamly-core 0.3.2, GHC 9.10.3,
  without the fusion plugin), time per run of `cut -f 1` to `cut -f 2-`:
  * `Array.splitEndBy_`, slices, plus the empty last field added when the
    line ends in a separator: 41 to 69 ms.
  * `Stream.splitSepBy_ (== d) Array.create . Array.read`, copies: 99 to
    134 ms.
  * `Stream.foldMany (Fold.takeEndBy_ (== d) Array.create) . Array.read`,
    copies, plus the same empty last field: 232 to 270 ms.
* The difference comes from copying, not from suffix versus infix
  splitting. Among the copying versions, the infix `Stream.splitSepBy_`
  is faster than the suffix split built from `foldMany` and
  `takeEndBy_`. `Array.splitEndBy_` produces no copies: it runs
  `indexEndBy_` on the element stream, which yields an (offset, length)
  pair per field, and slices the array with each pair. An infix array
  split needs one more step than the suffix split: if the last element is
  a separator, add an empty field. An array does this in O(1) time by
  checking the last element with `getIndex`. A stream has to remember
  whether the last element it saw was a separator.
* So building an infix split from a suffix split is not faster for
  streams in general, and the stream docs need no advice to do that.
  Whether `foldMany` with `takeEndBy_` is slower than `splitSepBy_`
  because of a fusion problem has not been checked.
* Here: `splitLine` in `Cut.hs` uses `Array.splitEndBy_` and adds the
  empty last field.
* Upstream: `Array.splitSepBy_`, built from `indexEndBy_` with the last
  element check, or from an `indexSepBy_` on streams, returning slices.

### Time units

* Problem: `FileTest` exports `seconds`, `minutes`, `hours` and `days` for
  writing durations, and uses `NominalDiffTime` from `time`. Notes in
  `FileTest.hs` and `FileTest/Common.hs` say these belong in the streamly
  time module.
* Here: defined in `FileTest`.
* Upstream: duration units and conversions in the public streamly time
  module.

### Internal modules used

The library imports these internal modules. A function used from an
internal module can change without a major version bump, so each of these
should get a public equivalent:

* `Streamly.Internal.Data.Stream`: `bfsConcatIterate`,
  `altBfsConcatIterate`, `takeEndBy`, `postscanlMaybe`,
  `unfoldEachEndBy`, `splitSepBySeq_` (`Find`, `Cut`).
* `Streamly.Internal.Data.StreamK`: `concatIterateWith`,
  `mergeIterateWith` (`Find`).
* `Streamly.Internal.FileSystem.DirIO`: `readEitherPaths`,
  `readEitherChunks` (`Find`).
* `Streamly.Internal.FileSystem.Posix.ReadDir` (streamly-core 0.3),
  `Streamly.Internal.Syscall.Posix.ReadDir` (0.4): `readEitherByteChunks`
  (`Find`).
* `Streamly.Internal.FileSystem.FileIO`: `fromChunks` (`Cp`).
* `Streamly.Internal.FileSystem.Path`: `dropTrailingSeparators`,
  `hasTrailingSeparator` (`Overwrite`, `Mv`, `Find`), `splitPath_`,
  `eqPathBytes` (`ResolvePath`).
* `Streamly.Internal.FileSystem.Posix.File`,
  `Streamly.Internal.FileSystem.Windows.File`: `withFile` (`Touch`),
  `withBinaryFile` (`Cp`).
* `Streamly.Internal.Data.Array`: `compactSepByByte_`, `splitEndBy_`,
  `concat`, `empty`, `fromPureStream`, `unsafeSliceOffLen`,
  `unsafeFreezeWithShrink` (`Cut`, `Find`, `Tail`).
* `Streamly.Internal.Data.MutArray`: `unsafeCreateWithPtr'` (`Tail`).
* `Streamly.Internal.Data.Scanl`, `Streamly.Internal.Data.Fold`
  (`foldtM'`) (`Find`).
* `Streamly.Internal.Unicode.Stream`: `decodeLatin1`, `readCharUtf8`
  (`Cut`).
* `Streamly.Internal.Data.Time.Clock`, `Streamly.Internal.Data.Time.Units`
  (`FileTest`).
* `Streamly.Internal.System.Process` (`Sh`, from streamly-process).

### Console output of byte streams

* Problem: the public `Streamly.Console.Stdio` has no `putBytes` or
  `putChunks`; they exist only in `Streamly.Internal.Console.Stdio`. The
  README example writes `Stream.fold Stdio.write` instead, and the `Sh`
  doctests import the internal module.
* Upstream: export `putBytes` and `putChunks` from the public module.

### Manipulating line buffers

The planned ArrayStream modules from streamly could be useful for line oriented
programs as well. LineStream is just ArrayStream with array buffers either
being byte arrays or unicode code point arrays or text arrays. For example, in
tail dropBytes, dropLines etc. We can represent a stream of paras where each
buffer is a "para" i.e. bunch of lines, and it records the number lines in the
header so that we can navigate by lines efficiently.

## streamly-process

### Running a command through a shell

* Problem: streamly-process has no function to run a command line through
  a shell, so `Sh` chooses the shell itself.
* Here: `shell` in `Sh.hs` is `/bin/sh` on POSIX and `sh` from the `PATH`
  on Windows (Git for Windows, MSYS2, Cygwin), passed with `-c` to the
  `Streamly.System.Process` functions in `streamWith`, `runWith` and
  `pipeWith`.
* Upstream: shell variants of the process functions in streamly-process
  that take a command line and pick the POSIX shell for the platform, and
  report a clear error when no shell is found. A separate `cmd.exe /c`
  variant for Windows-native commands.
