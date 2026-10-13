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
-- The mode is changed by composing class modifiers, one for each GNU symbolic
-- clause: 'setOwner' is @u=@, 'addOwner' is @u+@, 'removeOwner' is @u-@, and
-- likewise for the group (@g@), other (@o@) and all (@a@) classes. Modifiers
-- apply to the current mode of the file. A numeric mode is 'setOwner',
-- 'setGroup' and 'setOther' together.
--
-- == Shell equivalents
--
-- >>> _ = chmod id (setOwner [Read, Write] . setGroup [] . setOther [])   -- chmod 600 FILE
-- >>> _ = chmod id (setAll [Read])                                         -- chmod 444 FILE
-- >>> _ = chmod id (setOwner [Read, Write])                                -- chmod u=rw FILE
-- >>> _ = chmod id (addOwner [Exec])                                       -- chmod u+x FILE
-- >>> _ = chmod id (removeGroup [Write])                                   -- chmod g-w FILE
-- >>> _ = chmod id (addOwner [Exec] . removeGroup [Write])                 -- chmod u+x,g-w FILE
-- >>> _ = chmod id (addOwner [SetId])                                      -- chmod u+s FILE
-- >>> _ = chmod id (addOther [Sticky])                                     -- chmod +t FILE
-- >>> _ = chmod (modeFrom ref) id                                          -- chmod --reference=REF FILE

module Streamly.Coreutils.Chmod
    ( -- * Runner
      chmod

    -- * Options
    , ChmodOptions
    , modeFrom

    -- * Mode
    , Mode
    , Perm (..)

    -- ** Owner
    , setOwner
    , addOwner
    , removeOwner

    -- ** Group
    , setGroup
    , addGroup
    , removeGroup

    -- ** Other
    , setOther
    , addOther
    , removeOther

    -- ** All
    , setAll
    , addAll
    , removeAll
    )
where

import Data.Bits (complement, (.&.), (.|.))
import Streamly.FileSystem.Path (Path)
import System.PosixCompat.Files
    (fileMode, getFileStatus, isDirectory, setFileMode)
import System.PosixCompat.Types (FileMode)

import qualified Streamly.FileSystem.Path as Path

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Streamly.Coreutils.Chmod
-- >>> import Streamly.FileSystem.Path (path)
-- >>> ref = [path|ref.txt|]

-- = Design notes
--
-- * One way to write each bit. The modifiers are the GNU symbolic clauses
--   @[ugoa][=+-][rwxst]@, one function per class and operator. Per-bit
--   toggles such as @ownerRead True@ were removed: they gave a second way to
--   write the same mode.
--
-- * Modifiers always apply to the current mode; there is no option to start
--   from zero. A numeric mode is 'setOwner', 'setGroup' and 'setOther'
--   together, and this also gives the GNU numeric behavior for special bits:
--   @u=@ \/ @g=@ clear the set-ID bits of a regular file and keep those of a
--   directory, @o=@ clears the sticky bit. @chmod 600 dir@ keeps the set-ID
--   bits of the directory in GNU too. A @clearAll@ is not needed, it is
--   @setAll []@.
--
-- * As in GNU, 'SetId' is ignored for the other class and 'Sticky' is ignored
--   for the owner and group classes (@chmod o+s@, @chmod u+t@ do nothing).
--
-- * Not supported yet: @X@ (execute if a directory or already executable by
--   some class), copying a class (@u=g@), and clauses without a class
--   (@+x@), which GNU filters through the umask.
--
-- * @+x@ is not @a+x@ ('addAll' ['Exec']): GNU does not change the bits that
--   are set in the umask. With umask 077, @+x@ adds only @u+x@, @a+x@ adds
--   execute for all classes. Supporting @+x@ requires a modifier that applies
--   the umask, for example a umask-filtered variant of 'addAll', which can be
--   implemented later.
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
-- A supplementary mechanism could be a quasiquote [mode|a=rwx|] translating
-- to the class modifiers.
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

-- Mode cannot be a plain FileMode: 'setOwner' and 'setGroup' treat the set-ID
-- bits of a directory differently from those of a regular file. Fields: the
-- mode bits, whether the target is a directory.

-- | File mode.
--
-- This is an opaque type, construct values only by composing modifier
-- functions and passing the result to 'chmod'.
data Mode = Mode !FileMode !Bool

-- | A permission, the @rwxst@ of a GNU symbolic mode.
data Perm
    = Read
    | Write
    | Exec
    | SetId -- ^ set-user-ID for the owner, set-group-ID for the group,
            -- ignored for other
    | Sticky -- ^ the sticky bit for other, ignored for owner and group

data Class = Owner | Group | Other

-- The read, write and execute bits of a class.
rwxBits :: Class -> FileMode
rwxBits Owner = 0o700
rwxBits Group = 0o070
rwxBits Other = 0o007

-- The special bit that belongs to a class.
specialBit :: Class -> FileMode
specialBit Owner = 0o4000
specialBit Group = 0o2000
specialBit Other = 0o1000

-- The bits that the given permissions denote in a class.
permBits :: Class -> [Perm] -> FileMode
permBits cls = foldr ((.|.) . bit) 0

    where

    bit Read = 0o444 .&. rwxBits cls
    bit Write = 0o222 .&. rwxBits cls
    bit Exec = 0o111 .&. rwxBits cls
    bit SetId =
        case cls of
            Other -> 0
            _ -> specialBit cls
    bit Sticky =
        case cls of
            Other -> specialBit cls
            _ -> 0

-- The special bit is cleared unless listed, except for the set-ID bits of a
-- directory which are kept, see the design notes.
assignClass :: Class -> [Perm] -> Mode -> Mode
assignClass cls perms (Mode m isDir) =
    let keepSpecial =
            case cls of
                Other -> False
                _ -> isDir
        cleared =
            rwxBits cls .|. (if keepSpecial then 0 else specialBit cls)
     in Mode ((m .&. complement cleared) .|. permBits cls perms) isDir

addClass :: Class -> [Perm] -> Mode -> Mode
addClass cls perms (Mode m isDir) = Mode (m .|. permBits cls perms) isDir

removeClass :: Class -> [Perm] -> Mode -> Mode
removeClass cls perms (Mode m isDir) =
    Mode (m .&. complement (permBits cls perms)) isDir

-- | Set the owner permissions to exactly the given list, like @chmod u=...@.
-- The set-user-ID bit is cleared on a regular file and kept on a directory
-- unless 'SetId' is listed.
--
-- >>> _ = chmod id (setOwner [Read, Write])  -- chmod u=rw FILE
--
setOwner :: [Perm] -> Mode -> Mode
setOwner = assignClass Owner

-- | Add the given owner permissions, like @chmod u+...@.
--
addOwner :: [Perm] -> Mode -> Mode
addOwner = addClass Owner

-- | Remove the given owner permissions, like @chmod u-...@.
--
removeOwner :: [Perm] -> Mode -> Mode
removeOwner = removeClass Owner

-- | Set the group permissions to exactly the given list, like @chmod g=...@.
-- The set-group-ID bit is cleared on a regular file and kept on a directory
-- unless 'SetId' is listed.
--
setGroup :: [Perm] -> Mode -> Mode
setGroup = assignClass Group

-- | Add the given group permissions, like @chmod g+...@.
--
addGroup :: [Perm] -> Mode -> Mode
addGroup = addClass Group

-- | Remove the given group permissions, like @chmod g-...@.
--
removeGroup :: [Perm] -> Mode -> Mode
removeGroup = removeClass Group

-- | Set the other permissions to exactly the given list, like @chmod o=...@.
-- The sticky bit is cleared unless 'Sticky' is listed, also on a directory.
--
setOther :: [Perm] -> Mode -> Mode
setOther = assignClass Other

-- | Add the given other permissions, like @chmod o+...@.
--
addOther :: [Perm] -> Mode -> Mode
addOther = addClass Other

-- | Remove the given other permissions, like @chmod o-...@.
--
removeOther :: [Perm] -> Mode -> Mode
removeOther = removeClass Other

-- | 'setOwner', 'setGroup' and 'setOther' with the same list, like
-- @chmod a=...@.
--
-- >>> _ = chmod id (setAll [Read])  -- chmod 444 FILE
--
setAll :: [Perm] -> Mode -> Mode
setAll perms = setOwner perms . setGroup perms . setOther perms

-- | 'addOwner', 'addGroup' and 'addOther' with the same list, like
-- @chmod a+...@.
--
addAll :: [Perm] -> Mode -> Mode
addAll perms = addOwner perms . addGroup perms . addOther perms

-- | 'removeOwner', 'removeGroup' and 'removeOther' with the same list, like
-- @chmod a-...@.
--
removeAll :: [Perm] -> Mode -> Mode
removeAll perms = removeOwner perms . removeGroup perms . removeOther perms

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

-- | 'chmod' configuration. Build options by composing modifiers with @(.)@ and
-- pass the composition to 'chmod'; pass @id@ for defaults.
newtype ChmodOptions = ChmodOptions { chmodReference :: Maybe Path }

defaultOptions :: ChmodOptions
defaultOptions = ChmodOptions Nothing

-- NOTE: instead of having modeFrom option modifier, we could use a mode
-- builder from file e.g. "modeFrom :: Path -> Mode -> IO Mode", but that is
-- awkward to compose with pure "Mode -> Mode" builders. One way is to use a
-- combinator like f :: Path -> Mode -> Mode -> IO (Mode -> Mode)". Or lift
-- pure "Mode -> Mode" to "Mode -> IO Mode" and compose all with kliesli, but
-- then we will need a chmod (or variant) taking "Mode -> IO Mode" as argument.
-- It is much simpler to have "modeFrom" as option modifier compared to all
-- these options.

-- | Apply the mode modifiers to the mode of the given reference path instead
-- of the current mode of the target. With @id@ as the mode this is
-- @chmod --reference=REF@: the mode, including the special bits, is copied
-- exactly.
--
-- Default (without this modifier): the modifiers apply to the current mode of
-- the target.
modeFrom :: Path -> ChmodOptions -> ChmodOptions
modeFrom ref opts = opts { chmodReference = Just ref }

-------------------------------------------------------------------------------
-- Runner
-------------------------------------------------------------------------------

-- XXX Do not use toString
modeOf :: Path -> IO FileMode
modeOf p = fileMode <$> getFileStatus (Path.toString p)

-- The permission and special bits of a mode, without the file type bits.
modeMask :: FileMode
modeMask = 0o7777

-- | Change the mode bits of a file.
--
-- The mode modifiers apply to the current mode of the file, or to the mode of
-- the 'modeFrom' reference. Modifiers composed with @(.)@ apply right to left,
-- so the leftmost one wins: @addOwner [SetId] . setOwner [Read]@ is
-- @chmod u=r,u+s@.
--
-- Symlinks are followed.
--
-- Pass @id@ for default options and the @Mode -> Mode@ composition for the
-- mode; each modifier documents its own default. @chmod id id@ leaves the
-- mode unchanged.
chmod
    :: (ChmodOptions -> ChmodOptions)
    -> (Mode -> Mode)
    -> Path
    -> IO ()
chmod optF modeF path = do
    -- XXX do not use toString.
    st <- getFileStatus (Path.toString path)
    seed <-
        case chmodReference (optF defaultOptions) of
            Nothing -> pure (fileMode st)
            Just ref -> modeOf ref
    let Mode bits _ = modeF (Mode (seed .&. modeMask) (isDirectory st))
    setFileMode (Path.toString path) bits
