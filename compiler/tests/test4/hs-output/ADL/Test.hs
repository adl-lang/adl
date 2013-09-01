{-# LANGUAGE OverloadedStrings #-}
module ADL.Test(
    Date,
    DateO(..),
    S(..),
) where

import ADL.Core
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

data DateO = DateO
    { date_date :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue DateO where
    atype _ = "test.Date"
    
    defaultv = DateO
        "1900-01-01"
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("date",aToJSON f (date_date v))
        ] )
    
    aFromJSON f (JSON.Object hm) = DateO
        <$> fieldFromJSON f "date" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

data S = S
    { s_v1 :: Date
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S where
    atype _ = "test.S"
    
    defaultv = S
        defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("v1",aToJSON f (s_v1 v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S
        <$> fieldFromJSON f "v1" defaultv hm
    aFromJSON _ _ = Prelude.Nothing