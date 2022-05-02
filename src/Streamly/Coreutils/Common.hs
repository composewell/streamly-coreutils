module Streamly.Coreutils.Common
    ( Switch (..))
where

import Data.Default.Class (Default(..))

data Switch = On | Off deriving (Show, Eq)

instance Default Switch
    where
    def = Off
