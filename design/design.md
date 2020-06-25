# Design for Streamly Coreutils package

* cp :: SomeBase t ->
* echo
* cat
* wc
* yes
* uniq
* head
* tail
* sort


## Utilities for which I currently have ~no ideas
Primarily because they access/modify file structure

* mv
* pwd
* ls
* mkdir
* rm
* rmdir

Other packages have used the FileSystem package for the same.

## Points to remember while making the writeup

* Module structure
* Function signatures
* Implementation details (brief) and important points
* Interesting questions in mind


## End Goal

* Clean and simple API for `coreutils`
* Trying to avoid separate functions for a utility just because
  of differing file paths (absolute or relative)

## References

* MaiZure's projects
* Turtle
* Shelly
* `mrak/coreutils`
