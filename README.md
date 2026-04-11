# Streamly Coreutils

This repository provides Haskell functions that reimplement common
GNU `coreutils` commands, leveraging the `streamly` library for
efficient, streaming data processing where applicable. The goal is to
offer a functional and highly performant alternative to traditional
shell commands within Haskell applications, enabling complex data
transformations and system interactions using a pure functional
paradigm. Where applicable, these implementations are designed to be
highly concurrent, for example, the `ls` equivalent can list directory
contents concurrently for improved performance.

## Implemented Commands

Currently, this library provides implementations for the
following coreutils-inspired as well as some additional commands:

* Filesystem: `cp`, `rm`, `mv`, `ln`, `readlink`, `test`, `stat`, `touch`
* Directories: `ls`, `dirname`, `mkdir`, `cd`, `pwd`, `home` and others
* Text Processing: `cut`, `tail`
* Processes: `sleep`
* Shell: `which`, executing shell commands with streaming

## Important API Notice

**Please be aware that the API of this library is subject to heavy
change in future releases.** This project is under active development,
and function signatures, module organization, and overall design may
evolve significantly. Users should expect breaking changes and plan
accordingly.
