{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Coreutils.Path
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Path operations shared by the commands in this package, on top of
-- "Streamly.FileSystem.Path".

module Streamly.Coreutils.Path
    ( uncRootComponents
    , splitRoot
    , components
    )
where

import Data.Functor.Identity (runIdentity)
import Streamly.FileSystem.Path (Path)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path as Path
#if !MIN_VERSION_streamly_core(0,4,0)
import qualified Streamly.Internal.Data.Array as Array
#endif

-- Components of a root that starts with two separators, e.g. ["srv",
-- "share"] for "\\srv\share\" or ["?", "UNC", "srv"] for "\\?\UNC\srv\".
-- Empty for any other root.
uncRootComponents :: Path -> [String]
uncRootComponents root =
    case Path.toString root of
        a : b : rest | isSep a && isSep b -> go rest
        _ -> []

    where

    isSep = Path.isSeparator . Path.charToWord

    go s =
        case dropWhile isSep s of
            "" -> []
            s1 -> let (c, s2) = break isSep s1 in c : go s2

-- | Like 'Path.splitRoot', but on streamly-core < 0.4 the UNC share name is
-- made part of the root, as streamly-core 0.4 does.
splitRoot :: Path -> Maybe (Path, Maybe Path)
#if MIN_VERSION_streamly_core(0,4,0)
splitRoot = Path.splitRoot
#else
-- streamly-core < 0.4 leaves the share name out of a UNC root: the root of
-- "\\srv\share\a" is "\\srv\". Move the share name into the root.
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

-- | The components of a path, like filepath's @splitDirectories@: the root,
-- if any, then the names between separators. @.@ components are dropped
-- except in the leading position, and trailing separators are dropped.
components :: Path -> [Path]
components = runIdentity . Stream.toList . Path.splitPath_
