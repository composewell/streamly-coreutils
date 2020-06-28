# Design for Streamly Coreutils package

   ```
    import qualified S as S
    import qualified Streamly.Array as A
    import qualified Streamly.Internal.FileSystem.File as File
    import qualified Streamly.Internal.FileSystem.Handle as FH
    import qualified Streamly.Internal.Data.Fold as FL

    import qualified Streamly.Data.Unicode.Stream as U
    someFileToFP :: SomeBase File -> FilePath
    someFileToFP some =
                      case some of
                         Abs x -> fromAbsFile x
                         Rel x -> fromRelFile x


    someDirToFP :: SomeBase Dir -> FilePath
    someDirToFP some =
                       case some of
                         Abs x -> fromAbsDir x
                         Rel x -> fromRelDir x
   ```

* cp

  ```
      data CpOptions = CpOptions {
         attributesOnly :: Bool,
         interactive :: Bool,
         parents :: Bool,
         symbolicLink :: Bool,
         noTargetDir :: Bool,
         verbose :: Bool,
         noTargetDir :: Bool
         ...
      }

      cpFile :: CpOptions -> SomeBase File -> SomeBase Dir -> SomeBase File -> IO ()
      -- File.fromChunks, File.toChunksWithBufferOf

  ```

  1. Similar to srcFile -> destDir -> destFile
  2. Copies the source file to a dest directory with dest file name
  3. If dest file name is empty, then it becomes File -> Dir case (with same file name as source)

  4. For File -> File within same directory, current directory should be passed as destDir argument
   Else we can also say that an empty Dest Dir argument is equivalent to copying in the same directory

  ```
      cpFiletoDir :: (IsStream t, Monad m) => CpOptions -> t m (SomeBase File) -> SomeBase Dir -> IO ()
         uses cpFile
         (stream of input files so that copying can be done concurrently with use of appropriate streams)
         or map can be used as well

      cpDirToDir :: (IsStream t, Monad m) => CpOptions -> SomeBase Dir -> SomeBase Dir -> IO ()
         should create a stream of files (t m (SomeBase File)) and call cpFileToDir

  ```
* echo

  ```
      data EchoOptions = CpOptions {
         trailingLine :: Bool,
         interpretBackSlash :: Bool,
         ...
      }

      echo :: (MonadAsync m, MonadCatch m, MonadIO m) => EchoOptions -> SerialT m Word8 -> Handle -> IO ()


  ```

   1. writes the bytes in the stream to the handle
   (as echo's output can be redirected to a file)

* cat
   1. concatenates files to standard output
   2. [ if no file -> reads from std input ]

  ```
      data CatOptions = CpOptions {
         showAll :: Bool,
         numberNonEmptyLines :: Bool,
         showEnds :: Bool,
         numberAllLines :: Bool,
         suppressRepeatedEmpty :: Bool,
         ...
      }

      cat :: (IsStream t, Monad m) => CatOptions -> t m (SomeBase File) -> SomeBase File -> IO ()
         for stdout, second arg should be "/dev/stdout"
         reads the files, concatenates the output and writes in the destination file

         if empty stream, accept input indefinitely

         ( can be made streaming by not concatenating the output ; reading and writing parallely )

      TODO: Redirection
      Ex: cat *.md | sort

  ```

* wc
      The wc utility code can be taken from the examples directory, WordCount.hs

* yes

  ```
         yes :: (IsStream t, MonadAsync m) => m a -> t m a

         Generate an infinite stream using S.repeatM

  ```

* uniq

  ```
      data UniqOptions = UniqOptions {
         count :: Bool,
         repeated :: Bool,      -- display only duplicate lines once for each group
         duplicate :: Bool,     -- print all duplicate lines
         skipFields :: Int,
         ignoreCase :: Bool,
         unique :: Bool,        -- print only unique lines
         zeroTerminated :: Bool,
         checkChar :: Int,
         ...
      }

      splitOnNewLine :: (IsStream t, Monad m) => UniqOptions -> t m Word8 -> t m (Array Char)
         -- U.decodeLatin1, S.splitOnSuffix, A.write

      uniqCount :: t m (Array Char) -> t m (Int * String)
         -- to count occurences of each array of characters after splitOnNewLine
         -- S.groupsBy

  ```
   1. Utility function to read the stream and process according to `UniqOptions`

  ```
      uniq :: (IsStream t, Monad m) => UniqOptions -> SomeBase File -> Handle -> IO ()
      -- prints the output to the file or "/dev/stdout"

  ```
* head
  ```
      data HeadOptions = HeadOptions {
         firstNbytes :: Int,
         exceptLastNbytes :: Int,
         lines :: Int,      -- default 10
         quiet :: Bool,     -- never print headers
         verbose :: Bool,   -- always print headers
         zeroTerminated :: Bool,
         ...
      }

  ```

   1. Invalid option is one like :
   HeadOptions {quiet = True, verbose = True}

   2. Maybe we should check for validity of option arguments in
   all utilities

  ```
      head :: (IsStream t, Monad m) => HeadOptions -> t m (SomeBase File) -> t m String

  ```
   The header can be interleaved in the String stream if verbose is True.
   If input stream of files is empty, read and print from /dev/stdin 10 times

* tail
  ```
      data TailOptions = TailOptions {
         bytes :: Int,
         follow :: Bool,
         lines :: Int,              -- default 10
         maxUnchangedStats :: Int,
         pid :: Int,
         sleepInterval :: Int,       -- seconds
         verbose :: Bool,
         ...
      }

      tail :: (IsStream t, Monad m) => TailOptions -> t m (SomeBase File) -> t m String
  ```
   If input stream is empty, keep reading from /dev/stdin indefinitely.

* sort

  ```
      data SortOptions = SortOptions {
         ignoreLeadingBlanks :: Bool,
         ignoreCase :: Bool,
         dictionaryOrder :: Bool,
         generalNumericSort :: Bool,
         monthSort :: Bool,
         humanNumericSort :: Bool,
         numericSort :: Bool,
         randomSource :: SomeBase File,
         reverse :: Bool,
         check :: Bool,
         merge :: Bool,
         debug :: Bool,
         stable :: Bool,
         bufferSize :: SIZE,        -- which type to use for sizes
         separator :: Char,
         parallel :: Int,
         unique :: Bool,
         randomSort :: Bool,        -- shuffle but group same keys
         ...
      }

      sort :: (IsStream t, Monad m) => t m (SomeBase File) -> t m String
         -- a very basic version

  ```

## Utilities for which I currently have ~no ideas
Primarily because they access/modify the directory structure

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
  1. Try to keep the implementation streaming in nature.
  2. Use streams over lists wherever it makes sense.

* Interesting questions in mind


## End Goal

* Clean and simple API for `coreutils`
* Enable use of Haskell instead of shell scripting
* No dependency on the installation of coreutils package
* Platform independent (Linux/MacOS/Windows)
* Concurrency support
* Trying to avoid separate functions for a utility just because
  of differing paths (absolute or relative)

## References

* MaiZure's projects - GNU coreutils Decoded
* Turtle
* Shelly
* `mrak/coreutils`
* Linux man pages
