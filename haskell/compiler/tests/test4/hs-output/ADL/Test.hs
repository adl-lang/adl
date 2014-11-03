{-# LANGUAGE OverloadedStrings #-}
module ADL.Test(
    Date,
    DateO(..),
    S(..),
    S2(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import Data.Text(pack,unpack)
import Data.Time.Calendar(Day)
import Data.Time.Format(parseTime,formatTime)
import System.Locale(defaultTimeLocale)
import qualified ADL.Sys.Types
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude


type Date = Day

toDate :: DateO -> Prelude.Maybe Day
toDate (DateO s) = parseTime defaultTimeLocale "%F" (Data.Text.unpack s)

fromDate :: Day -> DateO
fromDate d = DateO (Data.Text.pack (formatTime defaultTimeLocale "%F" d))

instance ADLValue Day where
  atype _ = atype (defaultv :: DateO)
  defaultv = let (Prelude.Just d) = toDate defaultv in d
  jsonSerialiser jf = JSONSerialiser aTo aFrom
    where
      js = jsonSerialiser jf
      aTo d = aToJSON js (fromDate d)
      aFrom jv = (Prelude.>>=) (aFromJSON js jv) toDate

newtype DateO = DateO { unDateO :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue DateO where
    atype _ = "test.Date"
    
    defaultv = DateO "1900-01-01"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (DateO v) = aToJSON js v
            from o = Prelude.fmap DateO (aFromJSON js o)

data S = S
    { s_v1 :: Date
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S where
    atype _ = "test.S"
    
    defaultv = S
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v1_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("v1",aToJSON v1_js (s_v1 v))
                ] )
            
            from (JSON.Object hm) = S 
                <$> fieldFromJSON v1_js "v1" defaultv hm
            from _ = Prelude.Nothing

data S2 = S2
    { s2_f1 :: (ADL.Sys.Types.Maybe Data.Int.Int32)
    , s2_f2 :: (ADL.Sys.Types.Maybe Data.Int.Int32)
    , s2_f3 :: (ADL.Sys.Types.Either Data.Int.Int32 T.Text)
    , s2_f4 :: (ADL.Sys.Types.Either Data.Int.Int32 T.Text)
    , s2_f5 :: (ADL.Sys.Types.Pair Data.Int.Int32 Prelude.Double)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S2 where
    atype _ = "test.S2"
    
    defaultv = S2
        (fromJSONLiteral "{\"just\":5}")
        (fromJSONLiteral "{\"nothing\":null}")
        (fromJSONLiteral "{\"left\":7}")
        (fromJSONLiteral "{\"right\":\"go\"}")
        (fromJSONLiteral "{\"v1\":5,\"v2\":7.8}")
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            f3_js = jsonSerialiser jf
            f4_js = jsonSerialiser jf
            f5_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s2_f1 v))
                , ("f2",aToJSON f2_js (s2_f2 v))
                , ("f3",aToJSON f3_js (s2_f3 v))
                , ("f4",aToJSON f4_js (s2_f4 v))
                , ("f5",aToJSON f5_js (s2_f5 v))
                ] )
            
            from (JSON.Object hm) = S2 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
                <*> fieldFromJSON f3_js "f3" defaultv hm
                <*> fieldFromJSON f4_js "f4" defaultv hm
                <*> fieldFromJSON f5_js "f5" defaultv hm
            from _ = Prelude.Nothing