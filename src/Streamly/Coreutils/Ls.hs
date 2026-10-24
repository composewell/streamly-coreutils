-- |
-- Module      : Streamly.Coreutils.Ls
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- List directory contents.
--
-- Usage:
--
-- >> ls id dir                                    -- ls -A dir
-- >> ls (recursive True) dir                      -- ls -AR dir
-- >> ls (recursive True . traversal BreadthFirst) dir
--
-- Caveats, compared to GNU @ls@:
--
-- * Entries whose names start with a dot are listed, like @ls -A@. GNU @ls@
--   hides them by default.
-- * Entries are not sorted. Within a directory they are in the order the
--   operating system returns them. GNU @ls@ sorts by name.
-- * With @recursive True@ the output is a single stream that starts with
--   the given directory, not one block per directory with a heading.
--
-- The API and the defaults of this module are likely to change. Dot files
-- may be hidden and entries sorted by name by default, like GNU @ls@.

module Streamly.Coreutils.Ls
    (
      ls

    -- * Options
    , LsOptions
    , Traversal (..)
    , recursive
    , traversal
    )
where

import Streamly.Data.Stream (Stream)
import Streamly.FileSystem.Path (Path)

import qualified Streamly.Data.Stream as Stream

import Streamly.Coreutils.Error (relocateErrors)
import Streamly.Coreutils.Find
    (FindOptions, findEither, followRootSymlink, maxDepth)

import qualified Streamly.Coreutils.Find as Find

-- = Design notes
--
-- * ls is built on findEither and has no traversal code of its own; see
--   design/find.md. Each Ls traversal is a combination of the Find
--   traversal options that gives the same order. Ls keeps its own Traversal
--   type so that its default (WideDepthFirstReverse, like ls -R) and its
--   documentation stay independent of Find, and so that it offers only the
--   orders that suit a listing.
--
-- * The root symlink is followed (find -H), like GNU ls follows a symlink
--   given on the command line. A directory root is the first element of
--   findEither (maxDepth 1); ls drops it. A non-directory root is the only
--   element and is kept, like GNU ls lists a file operand. minDepth 1 would
--   drop a non-directory root as well.
--
-- * Errors that findEither raises itself have the location "find"; ls
--   changes it to "ls", the command name. Errors from the directory readers
--   carry their own location and propagate unchanged.
--
-- * ls is more of a printing to console tool. We can return chunked arrays
--   with newlines and short or long info in this API which are directly
--   printable. In the "find" API we can return Path and structured Stat data
--   instead for programmatic control.
--
-- * We can also have options to follow symlinks and other directory
--   traversal options.

-- | The order in which @ls (recursive True)@ visits a directory tree. For a
-- tree in which directory @1@ contains @2@ and @3@, @2@ contains @4@, @3@
-- contains @5@, @4@ contains @7@ and @5@ contains @6@, with entries listed
-- in that order:
--
-- +-------------------------+---------------------+-----------------------+
-- | Traversal               | Output              | Directories open      |
-- +=========================+=====================+=======================+
-- | 'WideDepthFirstReverse' | 1, 2, 3, 5, 6, 4, 7 | one                   |
-- +-------------------------+---------------------+-----------------------+
-- | 'DepthFirst'            | 1, 2, 4, 7, 3, 5, 6 | one per level of path |
-- +-------------------------+---------------------+-----------------------+
-- | 'BreadthFirst'          | 1, 2, 3, 4, 5, 7, 6 | one                   |
-- +-------------------------+---------------------+-----------------------+
data Traversal =
      -- XXX Replace with WideDepthFirst, using wideDfsConcatIterate from
      -- streamly-core 0.4, which descends in the order listed, like ls -R.
      WideDepthFirstReverse
      -- ^ List all entries of a directory, then list each of its
      -- subdirectories in turn, recursively, the last listed first. Like GNU
      -- @ls -R@, the entries of a directory are contiguous and so are those
      -- of a subtree; GNU @ls -R@ descends in sorted order. Holds the
      -- subdirectories not yet listed along the current path in memory.
    | DepthFirst
      -- ^ Pre-order depth first, the order of GNU @find@. Descends into a
      -- subdirectory as soon as it is listed, keeping the directories on the
      -- current path open; a very deep tree can exceed the open file limit.
    | BreadthFirst
      -- ^ Level by level: all entries at depth one, then all at depth two,
      -- and so on. Holds the directories of the next level in memory, which
      -- can be large for a wide tree.

-- | Options for 'ls'. Modify the defaults with 'recursive' and 'traversal'.
data LsOptions = LsOptions
    { lsRecursive :: Bool
    , lsTraversal :: Traversal
    }

defaultOptions :: LsOptions
defaultOptions = LsOptions
    { lsRecursive = False
    , lsTraversal = WideDepthFirstReverse
    }

-- | List the subdirectories recursively, like @ls -R@.
--
-- Default (without this modifier): only the given directory is listed.
recursive :: Bool -> LsOptions -> LsOptions
recursive opt cfg = cfg {lsRecursive = opt}

-- | The order in which a recursive listing visits the tree. Has no effect
-- without @recursive True@.
--
-- Default (without this modifier): 'WideDepthFirstReverse', which groups
-- entries like GNU @ls -R@.
traversal :: Traversal -> LsOptions -> LsOptions
traversal t cfg = cfg {lsTraversal = t}

-- | The Find traversal options that give the same order.
findTraversal :: Traversal -> FindOptions -> FindOptions
findTraversal t =
    case t of
        WideDepthFirstReverse -> Find.batched True . Find.reverseSiblings True
        DepthFirst -> id
        BreadthFirst -> Find.traversal Find.BreadthFirst . Find.batched True

-- | Drop the first element of a stream if it is a 'Left'.
dropLeftHead :: Monad m => Stream m (Either a b) -> Stream m (Either a b)
dropLeftHead s =
    Stream.concatEffect $ do
        r <- Stream.uncons s
        pure $ case r of
            Nothing -> Stream.nil
            Just (Left _, rest) -> rest
            Just (x, rest) -> Stream.cons x rest

-- | List the entries of a directory, like @ls -A@. Directories are 'Left'
-- and other entries, including symlinks to directories, are 'Right'. Each
-- entry is prefixed with the given directory path. @.@ and @..@ are not
-- listed. A path that is not a directory is listed by itself, as 'Right',
-- like @ls file@. A symlink to a directory given as the path is followed,
-- like @ls link@.
--
-- With @recursive True@ the output starts with the given directory itself,
-- as 'Left', followed by the entries of the whole tree. Symlinks under the
-- given directory are not followed.
--
-- Pass 'id' for the defaults; each modifier documents its own default.
--
-- Raises an exception if the path does not exist or a directory cannot be
-- read.
ls :: (LsOptions -> LsOptions) -> Path -> Stream IO (Either Path Path)
ls f dir = relocateErrors "find" "ls" entries

    where

    opts = f defaultOptions
    follow = followRootSymlink True
    order = findTraversal (lsTraversal opts)

    entries
        | lsRecursive opts = findEither (follow . order) dir
        | otherwise = dropLeftHead (findEither (follow . maxDepth 1) dir)
