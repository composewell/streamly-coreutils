# Streamly Coreutils (Fast, Concurrent and Powerful)

This repository provides Haskell functions that reimplement common GNU
`coreutils` commands, utilizing the `streamly` library for efficient,
and concurrent streaming data processing where applicable. The goal is
to offer a highly composable and performant alternative to traditional
shell commands within Haskell applications, enabling complex data
transformations, system programming and scripting using a pure
functional paradigm. Where applicable, the implementation is designed
to be concurrent, for example, the `find` equivalent can list directory
contents concurrently for improved performance.

# Fast, Concurrent and Powerful

How is it fast? For example, the serial implementation of `find` is
faster than the fastest yet find implementation which is rust `fd`. How
is it concurrent? Concurrency comes for free using the Haskell streamly
library, so wherever possible the implementation is concurrent and if
you need concurrency somewhere it can be made concurrent trivially. How
is it powerful? For example, the find implementation has many choices
like bfs, dfs, interleaved, concurrent unordered, concurrent ordered,
concurrent interleaved, all these are trivial to implement thanks to
Haskell streamly.

## Implemented Commands

Currently, this library provides implementations for the
following coreutils-inspired as well as some additional commands:

* Dir traversal: `ls`, `cp -r`, `rm -r`
* Dir modify: `touch`, `ln`, `cp`, `mkdir`, `rm`, `mv`
* File stat: `test`, `stat`, `touch`
* File read/write: `cp`
* Symlinks: `readlink`, `realpath`
* Processes: `cd`, `pwd`, `sleep`, `id`
* UserDB: `id`, `home`
* Text Processing: `cut`, `tail`
* Shell: streaming composition of shell commands
* Paths: `dirname`, `which`, `realpath`, `home`

## Important API Notice

**Please be aware that the API of this library is subject to heavy
change in future releases.** This project is under active development,
and function signatures, module organization, and overall design may
evolve significantly. Users should expect breaking changes and plan
accordingly.
