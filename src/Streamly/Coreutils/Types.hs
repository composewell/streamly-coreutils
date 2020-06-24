module Streamly.Coreutils.Types (
      defaultOptsDict
    , OptsDict (..)
   )
where
-------------------------------------------------------------------------------
-- Record for options used with cp
-------------------------------------------------------------------------------


data OptsDict = OptsDict {
                  verbose :: Bool
                }


defaultOptsDict :: OptsDict
defaultOptsDict = OptsDict True         -- set to False later
