# Streamly Coreutils

This repository provides Haskell functions that reimplement common
GNU `coreutils` commands, utilizing the `streamly` library for
efficient, streaming data processing where applicable. The goal is to
offer a functional and highly performant alternative to traditional
shell commands within Haskell applications, enabling complex data
transformations, system programming and scripting using a pure functional
paradigm. Where applicable, the implementation is designed to be
highly concurrent, for example, the `ls` equivalent can list directory
contents concurrently for improved performance.

## Implemented Commands

Currently, this library provides implementations for the
following coreutils-inspired as well as some additional commands:

* Dir traversal: `ls`, `cp -r`, `rm -r`
* Dir modify: `touch`, `ln`, `cp`, `mkdir`, `rm`, `mv`
* File stat: `test`, `stat`, `touch`
* File read/write: `cp`
* Symlink read: `readlink`
* Processes: `cd`, `pwd`, `sleep`
* Environment: `home`
* Text Processing: `cut`, `tail`
* Shell: streaming composition of shell commands
* Paths: `dirname`, `which`

## Important API Notice

**Please be aware that the API of this library is subject to heavy
change in future releases.** This project is under active development,
and function signatures, module organization, and overall design may
evolve significantly. Users should expect breaking changes and plan
accordingly.
