{-# LANGUAGE OverloadedStrings #-}
module Date(
  Date(..),
  fromText
  ) where

import qualified Data.Text as T

import ADL.Core
import Data.Maybe(fromJust)
import Data.Time.Calendar(Day)
import Data.Time.Format(parseTimeM,formatTime,defaultTimeLocale)
import Data.Aeson

newtype Date = Date Day
  deriving (Eq,Ord,Show)

instance AdlValue Date where
  atype _ = "test4.Date"
  jsonGen = JsonGen (adlToJson . dateToText)
  jsonParser = JsonParser $ \ctx jv -> case jv of
    Data.Aeson.String s -> case dateFromText s of
      Prelude.Nothing -> ParseFailure "expected a date" ctx
      (Prelude.Just d) -> ParseSuccess d
    _ -> ParseFailure "expected a date" ctx

dateFromText :: T.Text -> Prelude.Maybe Date
dateFromText s = Date <$> parseTimeM Prelude.True defaultTimeLocale "%F" (T.unpack s)

dateToText :: Date -> T.Text
dateToText (Date d) = T.pack (formatTime defaultTimeLocale "%F" d)

fromText :: T.Text -> Date
fromText = fromJust . dateFromText
