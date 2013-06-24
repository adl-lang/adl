{-# LANGUAGE OverloadedStrings #-}
module ADL.Test where

import ADL.Core
import Control.Applicative( (<$>), (<*>) )
import Data.Time.Calendar
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

-- CustomDate excluded due to custom definition

type Date = T.Text

data S = S
    { s_v1 :: Date
    , s_v2 :: Day
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S where
    atype _ = "test.S"
    
    defaultv = S
        defaultv
        defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("v1",aToJSON f (s_v1 v))
        , ("v2",aToJSON f (s_v2 v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S
        <$> fieldFromJSON f "v1" defaultv hm
        <*> fieldFromJSON f "v2" defaultv hm
    aFromJSON _ _ = Prelude.Nothing