{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Coreutils.Dirname
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Strip the last component from file name.

module Streamly.Coreutils.Dirname
    (dirname)
where

import Streamly.FileSystem.Path (Path)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.FileSystem.Path as Path

-- $setup
-- >>> import qualified Streamly.FileSystem.Path as Path
-- >>> dirnameS p = Path.fromString p >>= dirname >>= return . Path.toString

-- = Design notes
--
-- * 'System.FilePath.takeDirectory' is not used because it differs from GNU
--   @dirname@ on trailing separators: @takeDirectory "a/b/"@ is @"a/b"@,
--   GNU gives @"a"@.
--
-- * 'Path.takeDirectory' is not used because it gives 'Nothing' when the
--   last component is @.@ or @..@, GNU gives the part before it.
--
-- * The leading root (POSIX leading separators, Windows drive or UNC share)
--   is split off with 'Path.splitRoot' and kept as is, except that when
--   nothing follows it, its trailing separators are dropped with
--   'Path.dropTrailingSeparators'. This matches GNU: @dirname //a@ is @/@ but
--   @dirname //a//b@ is @//a@.
--
-- * A Windows share root keeps its trailing separator, as a drive root
--   does: \\srv\share\a gives \\srv\share\ and C:\a gives C:\. Python's
--   ntpath.dirname gives the same results.

-- Components of a root that starts with two separators, e.g. ["srv",
-- "share"] for "\\srv\share\" or ["?", "UNC", "srv"] for "\\?\UNC\srv\".
-- Empty for any other root.
uncRootComponents :: Path -> [String]
uncRootComponents root =
    case Path.toString root of
        a : b : rest | isSep a && isSep b -> components rest
        _ -> []

    where

    isSep = Path.isSeparator . Path.charToWord

    components s =
        case dropWhile isSep s of
            "" -> []
            s1 -> let (c, s2) = break isSep s1 in c : components s2

-- "\\srv\share\" or "\\?\UNC\srv\share\", not a device path such as
-- "\\.\x" or "\\?\C:\".
isShareRoot :: Path -> Bool
isShareRoot root =
    case uncRootComponents root of
        ["?", "UNC", _, _] -> True
        [server, _] -> server `notElem` ["?", "."]
        _ -> False

#if MIN_VERSION_streamly_core(0,4,0)
splitRoot :: Path -> Maybe (Path, Maybe Path)
splitRoot = Path.splitRoot
#else
-- streamly-core < 0.4 leaves the share name out of a UNC root: the root of
-- "\\srv\share\a" is "\\srv\". Move the share name into the root.
splitRoot :: Path -> Maybe (Path, Maybe Path)
splitRoot path =
    case Path.splitRoot path of
        Just (root, Just rest) | isServerRoot root ->
            let after =
                      Array.dropWhile Path.isSeparator
                    $ Array.dropWhile (not . Path.isSeparator)
                    $ Path.toArray rest
                rootLen = Array.length arr - Array.length after
                root1 =
                    Path.unsafeFromArray
                        (Array.unsafeSliceOffLen 0 rootLen arr)
             in if Array.null after
                then Just (root1, Nothing)
                else Just (root1, Just (Path.unsafeFromArray after))
        r -> r

    where

    arr = Path.toArray path

    isServerRoot root =
        case uncRootComponents root of
            ["?", "UNC", _] -> True
            [server] -> server `notElem` ["?", "."]
            _ -> False
#endif

-- The result when nothing follows the root.
rootOnly :: Path -> Path
rootOnly root
    | isShareRoot root = root
    | otherwise = Path.dropTrailingSeparators root

-- | Strip the last component of a path and the separators before it, like
-- GNU @dirname@. Trailing separators are ignored. A path with no directory
-- part gives @"."@; a path whose directory part is the root gives the root.
--
-- >>> dirnameS "a/b/"
-- "a"
-- >>> dirnameS "a"
-- "."
--
-- Other examples (POSIX), matching GNU @dirname@:
--
-- > "/a/b"   -> "/a"
-- > "a//b//" -> "a"
-- > "/a"     -> "/"
-- > "//a"    -> "/"
-- > "//a//b" -> "//a"
-- > "/"      -> "/"
-- > "a/./b"  -> "a/."
-- > ".."     -> "."
--
-- Examples (Windows):
--
-- > "C:\\a"              -> "C:\\"
-- > "C:a"                -> "C:"
-- > "\\a"                -> "\\"
-- > "\\\\srv\\share\\a"  -> "\\\\srv\\share\\"
--
dirname :: Path -> IO Path
dirname path =
    pure $ case splitRoot path of
        Nothing -> parentOf Nothing (Path.toArray path)
        Just (root, Nothing) -> rootOnly root
        Just (root, Just rest) -> parentOf (Just root) (Path.toArray rest)

    where

    dropTrailingSeps = Array.revDropWhile Path.isSeparator
    dropLastName = Array.revDropWhile (not . Path.isSeparator)

    -- "rest" is a suffix of "path", the result is a prefix of "path".
    parentOf root rest =
        let parent = dropTrailingSeps (dropLastName (dropTrailingSeps rest))
            arr = Path.toArray path
            len =
                Array.length arr - Array.length rest + Array.length parent
         in if Array.null parent
            then maybe (Path.fromString_ ".") rootOnly root
            else Path.unsafeFromArray (Array.unsafeSliceOffLen 0 len arr)
