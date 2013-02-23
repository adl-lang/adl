module ADL.Core.Value where

import qualified Data.Aeson as JSON

class ADLValue a where
  defaultv :: a

  atoJSON :: ToJSONFlags -> a -> JSON.Value

  afromJSON :: FromJSONFlags -> JSON.Value -> Maybe a

data ToJSONFlags = ToJSONFlags
  { tjf_writeTypeNames :: Bool
  }

data FromJSONFlags = FromJSONFlags
  { fjf_checkTypeNames :: Bool
  }


