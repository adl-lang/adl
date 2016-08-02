{-# LANGUAGE OverloadedStrings #-}
module ADL.Test(
    CDate(..),
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
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude

data CDate = CDate
    { cDate_year :: Data.Int.Int16
    , cDate_month :: Data.Int.Int16
    , cDate_day :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue CDate where
    atype _ = "test.CDate"
    
    defaultv = CDate
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            year_js = jsonSerialiser jf
            month_js = jsonSerialiser jf
            day_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("year",aToJSON year_js (cDate_year v))
                , ("month",aToJSON month_js (cDate_month v))
                , ("day",aToJSON day_js (cDate_day v))
                ] )
            
            from (JSON.Object hm) = CDate 
                <$> fieldFromJSON year_js "year" defaultv hm
                <*> fieldFromJSON month_js "month" defaultv hm
                <*> fieldFromJSON day_js "day" defaultv hm
            from _ = Prelude.Nothing


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
    , s_v2 :: Date
    , s_v3 :: CDate
    , s_v4 :: CDate
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S where
    atype _ = "test.S"
    
    defaultv = S
        defaultv
        (Date "2000-01-01")
        defaultv
        defaultv { cDate_day = 1, cDate_month = 1, cDate_year = 2000 }
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            v1_js = jsonSerialiser jf
            v2_js = jsonSerialiser jf
            v3_js = jsonSerialiser jf
            v4_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("v1",aToJSON v1_js (s_v1 v))
                , ("v2",aToJSON v2_js (s_v2 v))
                , ("v3",aToJSON v3_js (s_v3 v))
                , ("v4",aToJSON v4_js (s_v4 v))
                ] )
            
            from (JSON.Object hm) = S 
                <$> fieldFromJSON v1_js "v1" defaultv hm
                <*> fieldFromJSON v2_js "v2" defaultv hm
                <*> fieldFromJSON v3_js "v3" defaultv hm
                <*> fieldFromJSON v4_js "v4" defaultv hm
            from _ = Prelude.Nothing