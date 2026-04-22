-- |
-- Module      : Streamly.Coreutils.Chmod
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Change file mode bits. Mirrors GNU @chmod@. Recursive mode not supported
-- yet.
--
-- == Shell equivalents
--
-- >>> _ = chmod id (ownerRead True . ownerWrite True)                     -- chmod u=rw FILE
-- >>> _ = chmod id (ownerRead True . groupRead True . otherRead True)     -- chmod a=r FILE
-- >>> _ = chmod (additive True) (ownerExec True)                          -- chmod u+x FILE
-- >>> _ = chmod (additive True) (groupWrite False)                        -- chmod g-w FILE
-- >>> _ = chmod (modeFrom file) (groupWrite True)                         -- chmod --reference=ref FILE

module Streamly.Coreutils.Chmod
    ( -- * Runner
      chmod

    -- * Options
    , ChmodOptions
    , additive
    , modeFrom

    -- * Mode
    , Mode

    -- ** Owner bits
    , ownerRead
    , ownerWrite
    , ownerExec

    -- ** Group bits
    , groupRead
    , groupWrite
    , groupExec

    -- ** Other bits
    , otherRead
    , otherWrite
    , otherExec

    -- ** Special bits
    , setUid
    , setGid
    , sticky
    )
where

import Data.Bits (complement, (.&.), (.|.))
import Streamly.FileSystem.Path (Path)
import System.PosixCompat.Files (fileMode, getFileStatus, setFileMode)
import System.PosixCompat.Types (FileMode)

import qualified Streamly.FileSystem.Path as Path

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Streamly.Coreutils.Chmod
-- >>> import Streamly.FileSystem.Path (path)
-- >>> file = [path|a.txt|]

-- = Design notes
--
-- TODO: add recursive mode.
-- TODO: add @followSymlinks@ option.
-- TODO: make windows behavior consistent with filetest.
--
-- Mode builders:
-- -------------
--
-- Mode builders can be common to chmod and the file test utility and anything
-- else in coreutils. We can have a common mode builder (FileMode) module
-- exposing the mode builders to all consumers.
--
-- The simplest is one function for each bit e.g. "ownerRead True" and compose
-- them all to create a mode. There can be canned ownerRWX, ownerRX, ownerRW,
-- ownerWX, that will complete all possible combinations, but does not include
-- the sticky bit. Another supplementary mechanism could be a quasiquote
-- [mode|a=rwx|] this can include sticky bit as well without exploding.
--
-- We can also have a quasiquoter to build the chmod options directly e.g.
-- @chmod [chmodOpt|a=rwx]@.
--
-- Quasiquoter format:
-- The format of a symbolic mode is [roles][-+=][perms...], where roles is
-- either zero or more letters from the set "ugoa". perms is either zero or
-- more letters from the set "rwxXst". Multiple symbolic modes can be given,
-- separated by commas.
--
-- Examples:
--
-- @
-- -
-- -rwx
-- g-rx
-- g-x+r
-- go-x+rw
-- go-x+rw,u+r
-- @
--

-------------------------------------------------------------------------------
-- Mode
-------------------------------------------------------------------------------

-- TODO: Should we directly use FileMode here, so that we do not have to export
-- one more type which might conflict with others.? This is in "base" so should
-- be fine.

-- | File mode.
--
-- This is an opaque type, construct values only by composing modifier
-- functions and passing the result to 'chmod'.
newtype Mode = Mode FileMode

toggle :: FileMode -> Bool -> Mode -> Mode
toggle bit True (Mode m) = Mode (m .|. bit)
toggle bit False (Mode m) = Mode (m .&. complement bit)

-- | Toggle the owner read bit (@0o400@).
--
ownerRead :: Bool -> Mode -> Mode
ownerRead = toggle 0o400

-- | Toggle the owner write bit (@0o200@).
--
ownerWrite :: Bool -> Mode -> Mode
ownerWrite = toggle 0o200

-- | Toggle the owner execute bit (@0o100@).
--
ownerExec :: Bool -> Mode -> Mode
ownerExec = toggle 0o100

-- | Toggle the group read bit (@0o040@).
--
groupRead :: Bool -> Mode -> Mode
groupRead = toggle 0o040

-- | Toggle the group write bit (@0o020@).
--
groupWrite :: Bool -> Mode -> Mode
groupWrite = toggle 0o020

-- | Toggle the group execute bit (@0o010@).
--
groupExec :: Bool -> Mode -> Mode
groupExec = toggle 0o010

-- | Toggle the other read bit (@0o004@).
--
otherRead :: Bool -> Mode -> Mode
otherRead = toggle 0o004

-- | Toggle the other write bit (@0o002@).
--
otherWrite :: Bool -> Mode -> Mode
otherWrite = toggle 0o002

-- | Toggle the other execute bit (@0o001@).
--
otherExec :: Bool -> Mode -> Mode
otherExec = toggle 0o001

-- | Toggle the set-user-ID bit (@0o4000@).
--
setUid :: Bool -> Mode -> Mode
setUid = toggle 0o4000

-- | Toggle the set-group-ID bit (@0o2000@).
--
setGid :: Bool -> Mode -> Mode
setGid = toggle 0o2000

-- | Toggle the sticky bit (@0o1000@).
--
sticky :: Bool -> Mode -> Mode
sticky = toggle 0o1000

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data SeedSource = SeedZero | SeedSelf | SeedRef Path

-- | 'chmod' configuration. Build options by composing modifiers with @(.)@ and
-- pass the composition to 'chmod'; pass @id@ for defaults.
newtype ChmodOptions = ChmodOptions { chmodSeed :: SeedSource }

defaultOptions :: ChmodOptions
defaultOptions = ChmodOptions SeedZero

-- | When 'True', mode is added to the existing mode of the file instead of
-- resetting it.
--
-- Default: 'False'.
additive :: Bool -> ChmodOptions -> ChmodOptions
additive True opts = opts { chmodSeed = SeedSelf }
additive False opts = opts { chmodSeed = SeedZero }

-- NOTE: instead of having modeFrom option modifier, we could use a mode
-- builder from file e.g. "modeFrom :: Path -> Mode -> IO Mode", but that is
-- awkward to compose with pure "Mode -> Mode" builders. One way is to use a
-- combinator like f :: Path -> Mode -> Mode -> IO (Mode -> Mode)". Or lift
-- pure "Mode -> Mode" to "Mode -> IO Mode" and compose all with kliesli, but
-- then we will need a chmod (or variant) taking "Mode -> IO Mode" as argument.
-- It is much simpler to have "modeFrom" as option modifier compared to all
-- these options.

-- | Use the current mode of the given reference path as the starting mode, the
-- specified mode is added to the reference mode.
--
-- Default: no reference.
modeFrom :: Path -> ChmodOptions -> ChmodOptions
modeFrom ref opts = opts { chmodSeed = SeedRef ref }

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------

-- XXX Do not use toString
modeOf :: Path -> IO FileMode
modeOf p = fileMode <$> getFileStatus (Path.toString p)

resolveSeed :: SeedSource -> Path -> IO FileMode
resolveSeed seed target = case seed of
    SeedZero -> pure 0
    SeedSelf -> modeOf target
    SeedRef ref -> modeOf ref

-- | Change the mode bits of a file.
--
-- The desired mode is built by composing mode setter functions. By default the
-- mode of the file is set to the supplied mode, the 'additive' modifier can be
-- used to add to the existing mode.
--
-- Symlinks are followed by default.
--
-- Pass @id@ for default options and the @Mode -> Mode@ composition for the
-- mode; each modifier documents its own default.
--
-- Note: @chmod id id@ would clear all modes.
chmod
    :: (ChmodOptions -> ChmodOptions)
    -> (Mode -> Mode)
    -> Path
    -> IO ()
chmod optF modeF path = do
    seed <- resolveSeed (chmodSeed (optF defaultOptions)) path
    let Mode bits = modeF (Mode seed)

    -- XXX do not use toString.
    setFileMode (Path.toString path) bits
