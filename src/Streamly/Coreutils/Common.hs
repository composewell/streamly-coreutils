{-# LANGUAGE PatternSynonyms #-}

-- | This module is deprecated. Use 'Data.Bool' and 'Bool' instead.
module Streamly.Coreutils.Common
    {-# DEPRECATED "This module is deprecated. Please use 'Bool' from 'Data.Bool' instead." #-}
    ( Switch
    , pattern On
    , pattern Off
    ) where

-- Define Switch as a Bool alias
type Switch = Bool
{-# DEPRECATED Switch "Use 'Bool' instead" #-}

-- Alias On with True
pattern On :: Switch
pattern On = True
{-# DEPRECATED On "Use 'True' instead" #-}

-- Alias Off with False
pattern Off :: Switch
pattern Off = False
{-# DEPRECATED Off "Use 'False' instead" #-}

-- Ensure GHC knows these two patterns cover all Bool cases
{-# COMPLETE On, Off #-}
