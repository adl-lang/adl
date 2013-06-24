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

-- CustomDate2 overridden by custom definition
data CustomDate2_ = CustomDate2_
    { customDate2__date :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue CustomDate2_ where
    atype _ = "test.CustomDate2_"
    
    defaultv = CustomDate2_
        "1900-01-01"
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("date",aToJSON f (customDate2__date v))
        ] )
    
    aFromJSON f (JSON.Object hm) = CustomDate2_
        <$> fieldFromJSON f "date" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

type Date = T.Text

data S = S
    { s_v1 :: Date
    , s_v2 :: Day
    , s_v3 :: Day
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S where
    atype _ = "test.S"
    
    defaultv = S
        defaultv
        defaultv
        defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("v1",aToJSON f (s_v1 v))
        , ("v2",aToJSON f (s_v2 v))
        , ("v3",aToJSON f (s_v3 v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S
        <$> fieldFromJSON f "v1" defaultv hm
        <*> fieldFromJSON f "v2" defaultv hm
        <*> fieldFromJSON f "v3" defaultv hm
    aFromJSON _ _ = Prelude.Nothing