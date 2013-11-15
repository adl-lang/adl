{-# LANGUAGE OverloadedStrings #-}
module ADL.Test(
    Date,
    DateO(..),
    S(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import Data.Time.Calendar(Day)
import Data.Time.Format(parseTime,formatTime)
import System.Locale(defaultTimeLocale)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude


type Date = Day

toDate :: DateO -> Maybe Day
toDate (Date s) = parseTime defaultTimeLocale "%F" s

fromDate :: Day -> Date_ADL
fromDate d = Date (formatTime defaultTimeLocale "%F" d)

instance ADLValue Day where
  atype _ = atype (defaultv :: ODate)
  defaultv = let (Just d) = toDate defaultv in d
  aToJSON jf d = aToJSON jf (fromDate d)
  afromJSON jf jv = (aFromJSON jf jv) >>= toDate

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