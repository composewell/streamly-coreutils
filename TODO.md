# TODO

## Before the Hackage release

### tail: follow

should we use streamly-fsevents for follow?

### CHANGELOG release date

The 0.1.0 entry says "Unreleased". Put the upload date there.

## After streamly-core 0.4

### Expose ResolvePath

`Streamly.Coreutils.ResolvePath` is an internal module until we require
streamly-core 0.4, which may provide path resolution on `Path`
(`Streamly.Internal.FileSystem.PathIO`: `makeAbsolute`, `resolve`). Review
the API against what 0.4 provides, then:

* move the module from `other-modules` to `exposed-modules` in the cabal
  file,
* make the `Streamly.Coreutils.ResolvePath` test suite depend on the
  library again instead of building the module from `src`,
* restore the `realpath` row in the README command table,
* remove the "not exposed yet" bullet from the design notes in the module.

Exposing the module also makes cabal-docspec run its doctests again.

## Differences from GNU behaviour

### cp

* The Haddock says "Copy a file or directory", but directories are not
  supported.

### ls

* Find vs ls: do we need both? Or make ls an executable instead of lib
  function.
* Lists dotfiles, like `ls -A`. GNU `ls` hides them by default.
* Does not sort. GNU `ls` sorts by name.
* `ls (recursive True)` descends into the subdirectories of a directory in
  the reverse of the order they are listed (`altBfsConcatIterate`). Switch
  to `wideDfsConcatIterate` from streamly-core 0.4. GNU `ls -R` descends in
  sorted order.

## FileTest

* `newerThanFile` and `olderThanFile` with a missing file. GNU `test`:
  `file1 -nt file2` is true if file1 is newer than file2, or if file1
  exists and file2 does not; `file1 -ot file2` is true if file1 is older
  than file2, or if file2 exists and file1 does not. Here a missing file2
  raises an exception, and a missing file1 gives `False`, also for
  `olderThanFile`. Decide whether to follow GNU, and whether the size
  comparisons (`largerThanFile`, ...) should do the same.
* On Windows, `isReadableNow`, `isWritableNow` and `isExecutableNow`
  catch every `IOException` after the status check and return `False`, so
  an error other than a missing file, e.g. EIO, gives `False` instead of
  raising an exception like the other predicates. `pathAccess`, used by
  `isReadable`, `isWritable` and `isExecutable`, does the same with every
  `SomeException` raised after it gets the impersonation token.
* On Windows, the real access tests for `isReadable`, `isWritable` and
  `isExecutable` (the "real access" group in the FileTest tests) are not
  run. Add Windows tests for them.

## Error semantics

* Streams (`ls`, `find`, `tail`) raise the `IOError` of the first failing
  element. GNU `find` and `ls -R` report an unreadable directory and
  continue. Add an explicit option for that, e.g. an `ignoreErrors`
  modifier or a stream of `Either IOError a`, instead of dropping entries
  silently.
* Unimplemented functions are exported and are `undefined`:
  `FileTest.Posix.testHandle`, and `isOwnedByUserId` / `isOwnedByGroupId`
  on Windows. Stop exporting them, like the unimplemented `cp` methods.

## Naming

* rmContents => rmDirContents
* Prefixed modifiers: `cpMethod` in `Cp`; `idReal`,
  `idEffective`, `idUser`, `idGroup` in `Id`. The other modules use
  unprefixed names such as `recursive` and `makeParents`.
* Runner names that are not the command name: `readLink` (`readlink`),
  `foldIndicesBy` (`cut`). `resolvePath` (`realpath`) is defensible.
* Symlink spelling: `isSymLink` and `isDirSymLink` in `FileTest`;
  `followSymlinks` everywhere else and in streamly-core.
* Deprecated exports in the first release: `Mkdir.Mkdir`, `Mkdir.parents`,
  `FileTest.isExisting`, and the "Deprecated" section of `Cut`.
  Nothing has been released, so they can be removed.
* Stale documentation: the export list comment in `Id` says "Start from
  'defaultConfig'", which is not exported. CLAUDE.md gives `parents` as an
  example of good naming, but `Mkdir` deprecates it for `makeParents`.
* `Tail.lines` and `Tail.bytes` shadow the Prelude names.

* Should FileTest and Stat go to Streamly.FileSystem ? because they can also be
  used in DirIO etc?

## API review

Before the first release, things that have long term impact and cost should be
reviewed e.g. API signatures, implementation can be changed later.
* API/options naming
* API semantics
* What can be made better in API surface?
* What can we learn from other such projects e.g. powershell
* What can we learn from rust uutils?
* doc review/reflow that adds real value, not trivial changes
* remove as many filepath/directory API usage as possible
