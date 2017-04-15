{-# LANGUAGE OverloadedStrings #-}
module ADL.Test4(
    CDate(..),
    Date,
    S(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Data.Time.Calendar(Day)
import Data.Time.Format(parseTimeM,formatTime,defaultTimeLocale)
import Prelude((.))
import qualified ADL.Sys.Types
import qualified Data.Aeson
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Proxy
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text as T
import qualified Prelude

data CDate = CDate
    { cDate_year :: Data.Int.Int16
    , cDate_month :: Data.Int.Int16
    , cDate_day :: Data.Int.Int16
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkCDate :: Data.Int.Int16 -> Data.Int.Int16 -> Data.Int.Int16 -> CDate
mkCDate year month day = CDate year month day

instance AdlValue CDate where
    atype _ = "test4.CDate"
    
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

dateFromText :: Data.Text.Text -> Prelude.Maybe Day
dateFromText s = parseTimeM Prelude.True defaultTimeLocale "%F" (T.unpack s)

dateToText :: Day -> Data.Text.Text
dateToText d = T.pack (formatTime defaultTimeLocale "%F" d)

instance AdlValue Day where
  atype _ = "test4.Date"
  jsonGen = JsonGen (adlToJson . dateToText)
  jsonParser = JsonParser (\ctx jv -> case jv of 
    Data.Aeson.String s -> case dateFromText s of
      Prelude.Nothing -> ParseFailure "expected a date" ctx
      (Prelude.Just d) -> ParseSuccess d
    _ -> ParseFailure "expected a date" ctx
    )

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

mkS :: Date -> CDate -> (ADL.Sys.Types.Maybe T.Text) -> (ADL.Sys.Types.Pair T.Text Data.Int.Int32) -> (ADL.Sys.Types.Set Data.Int.Int32) -> (ADL.Sys.Types.Map T.Text Data.Int.Int32) -> S
mkS v1 v3 v5 v6 v7a v8 = S v1 ((Data.Maybe.fromJust . dateFromText) "2000-01-01") v3 (CDate 2000 1 1) v5 Prelude.Nothing (Prelude.Just "hello") v6 (Data.Set.fromList [ 1, 2, 3 ]) v7a v8 (Data.Map.fromList [ ((,) "X" 1), ((,) "Y" 2) ])

instance AdlValue S where
    atype _ = "test4.S"
    
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
        <*> parseFieldDef "v2" ((Data.Maybe.fromJust . dateFromText) "2000-01-01")
        <*> parseField "v3"
        <*> parseFieldDef "v4" (CDate 2000 1 1)
        <*> parseField "v5"
        <*> parseFieldDef "v5a" Prelude.Nothing
        <*> parseFieldDef "v5b" (Prelude.Just "hello")
        <*> parseField "v6"
        <*> parseFieldDef "v7" (Data.Set.fromList [ 1, 2, 3 ])
        <*> parseField "v7a"
        <*> parseField "v8"
        <*> parseFieldDef "v8a" (Data.Map.fromList [ ((,) "X" 1), ((,) "Y" 2) ])