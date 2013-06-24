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

import ADL.Examples.Pubsub(UTCTime_)

instance ADLValue UTCTime where
  atype _ = atype (defaultv :: UTCTime_)
  defaultv = (defaultv :: UTCTime_)
  aToJSON jf ut = aToJSON jf (fromUTCTime ut)
  aFromJSON jf jv = aFromJSON jf jv >>= toUTCTime

toUTCTime :: UTCTime_ ->  Maybe UTCTime
toUTCTime ut_ =  parseTime defaultTimeLocale isoFormat (uTCTime__utctime ut_)

fromUTCTime :: UTCTime -> UTCTime_
fromUTCTime ut = UTCTime_ (formatTime defaultTimeLocale isoFormat ut)

isoFormat = "%FT%XZ" 
