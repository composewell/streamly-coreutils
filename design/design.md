# Design for Streamly Coreutils package

* cp
   ```
      data OptsDict = OptsDict {
         attributesOnly :: Bool,
         interactive :: Bool,
         parents :: Bool,
         symbolicLink :: Bool,
         noTargetDir :: Bool,
         verbose :: Bool,
         noTargetDir :: Bool
         ...
      }

  cpFile :: OptsDict -> SomeBase File -> SomeBase Dir -> SomeBase File -> IO ()

  Similar to srcFile -> destDir -> destFile
  ```
  Copies the source file to a dest directory with dest file name
  If dest file name is empty, then it becomes File -> Dir case (with same file name as source)

  For File -> File within same directory, current directory should be passed as destDir argument
  Else we can also say that an empty Dest Dir argument is equivalent to copying in the same directory

  ```
  cpFiletoDir :: (IsStream t, Monad m) => OptsDict -> t m (SomeBase File) -> SomeBase Dir -> IO ()

    -- uses cpFile
    (stream of input files so that copying can be done concurrently with use of appropriate streams)
    -- or map can be used as well

  cpDirToDir :: (IsStream t, Monad m) => OptsDict -> SomeBase Dir -> SomeBase Dir -> IO ()

  ```

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
* cd ( should it change the current working directory of the process? )

Other packages have used the FileSystem package for the same.


## Points to remember while making the writeup

* Module structure
* Function signatures
* Implementation details (brief) and important points
  1. Try to keep the implementation streaming in nature.
  2. Use streams over lists wherever it makes sense.

* Interesting questions in mind


## End Goal

* Clean and simple API for `coreutils`
* Trying to avoid separate functions for a utility just because
  of differing paths (absolute or relative)

## References

* MaiZure's projects
* Turtle
* Shelly
* `mrak/coreutils`
