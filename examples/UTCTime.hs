module UTCTime(
  UTCTime
  ) where

import qualified Data.Aeson as JSON

import qualified Data.Text as T

import ADL.Core.Value
import ADL.Core.Primitives
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

instance ADLValue UTCTime where
  atype _ = atype (defaultv :: T.Text)
  defaultv = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)
  aToJSON jf ut = aToJSON jf (textFromUTCTime ut)
  aFromJSON jf jv = aFromJSON jf jv >>= utcTimeFromText

utcTimeFromText :: T.Text ->  Maybe UTCTime
utcTimeFromText t =  if T.null t then (Just defaultv)
                                 else parseTime defaultTimeLocale isoFormat (T.unpack t)

textFromUTCTime :: UTCTime -> T.Text
textFromUTCTime ut = T.pack (formatTime defaultTimeLocale isoFormat ut)

isoFormat = "%FT%XZ" 
