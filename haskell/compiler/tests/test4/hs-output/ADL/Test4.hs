{-# LANGUAGE OverloadedStrings #-}
module ADL.Test4(
    CDate(..),
    Date,
    DateO(..),
    S(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Data.Time.Calendar(Day)
import Data.Time.Format(parseTime,formatTime)
import System.Locale(defaultTimeLocale)
import qualified ADL.Sys.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data CDate = CDate
    { cDate_year :: Data.Int.Int16
    , cDate_month :: Data.Int.Int16
    , cDate_day :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue CDate where
    atype _ = "test4.CDate"
    
    defaultv = CDate
        defaultv
        defaultv
        defaultv
    
    jsonGen = genObject
        [ genField "year" cDate_year
        , genField "month" cDate_month
        , genField "day" cDate_day
        ]
    
    jsonParser = CDate
        <$> parseField "year"
        <*> parseField "month"
        <*> parseField "day"


type Date = Day

fromDateO :: DateO -> Prelude.Maybe Day
fromDateO (DateO s) = parseTime defaultTimeLocale "%F" s

toDateO :: Day -> DateO
toDateO d = DateO (formatTime defaultTimeLocale "%F" d)

instance AdlValue Day where
  atype _ = atype (Data.Proxy.Proxy :: Data.Proxy.Proxy DateO)
  defaultv = let (Prelude.Just d) = toDate defaultv in d
  jsonGen = JsonGen (adlToJson . toDateO)
  jsonParser = jsonParser undefined

newtype DateO = DateO { unDateO :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue DateO where
    atype _ = "test4.Date"
    
    defaultv = DateO "1900-01-01"
    
    jsonGen = JsonGen (\(DateO v) -> adlToJson v)
    
    jsonParser = DateO <$> jsonParser

data S = S
    { s_v1 :: Date
    , s_v2 :: Date
    , s_v3 :: CDate
    , s_v4 :: CDate
    , s_v5 :: (ADL.Sys.Types.Maybe T.Text)
    , s_v5a :: (ADL.Sys.Types.Maybe T.Text)
    , s_v5b :: (ADL.Sys.Types.Maybe T.Text)
    , s_v6 :: (ADL.Sys.Types.Pair T.Text Data.Int.Int32)
    , s_v7 :: (ADL.Sys.Types.Set Data.Int.Int32)
    , s_v7a :: (ADL.Sys.Types.Set Data.Int.Int32)
    , s_v8 :: (ADL.Sys.Types.Map T.Text Data.Int.Int32)
    , s_v8a :: (ADL.Sys.Types.Map T.Text Data.Int.Int32)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue S where
    atype _ = "test4.S"
    
    defaultv = S
        defaultv
        (Date "2000-01-01")
        defaultv
        defaultv { cDate_day = 1, cDate_month = 1, cDate_year = 2000 }
        defaultv
        (Maybe_nothing ())
        (Maybe_just "hello")
        defaultv
        (Set [ 1, 2, 3 ])
        defaultv
        defaultv
        (Map [ (defaultv :: (ADL.Sys.Types.Pair T.Text Data.Int.Int32)) { pair_v1 = "X", pair_v2 = 1 }, (defaultv :: (ADL.Sys.Types.Pair T.Text Data.Int.Int32)) { pair_v1 = "Y", pair_v2 = 2 } ])
    
    jsonGen = genObject
        [ genField "v1" s_v1
        , genField "v2" s_v2
        , genField "v3" s_v3
        , genField "v4" s_v4
        , genField "v5" s_v5
        , genField "v5a" s_v5a
        , genField "v5b" s_v5b
        , genField "v6" s_v6
        , genField "v7" s_v7
        , genField "v7a" s_v7a
        , genField "v8" s_v8
        , genField "v8a" s_v8a
        ]
    
    jsonParser = S
        <$> parseField "v1"
        <*> parseFieldDef "v2" (Date "2000-01-01")
        <*> parseField "v3"
        <*> parseFieldDef "v4" defaultv { cDate_day = 1, cDate_month = 1, cDate_year = 2000 }
        <*> parseField "v5"
        <*> parseFieldDef "v5a" (Maybe_nothing ())
        <*> parseFieldDef "v5b" (Maybe_just "hello")
        <*> parseField "v6"
        <*> parseFieldDef "v7" (Set [ 1, 2, 3 ])
        <*> parseField "v7a"
        <*> parseField "v8"
        <*> parseFieldDef "v8a" (Map [ (defaultv :: (ADL.Sys.Types.Pair T.Text Data.Int.Int32)) { pair_v1 = "X", pair_v2 = 1 }, (defaultv :: (ADL.Sys.Types.Pair T.Text Data.Int.Int32)) { pair_v1 = "Y", pair_v2 = 2 } ])