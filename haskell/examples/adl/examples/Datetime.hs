{-# LANGUAGE OverloadedStrings #-}
module ADL.Examples.Datetime(
    Date,
    Date0(..),
    UTCTime,
    UTCTime0(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import Data.Time.Calendar(Day)
import Data.Time.Clock(UTCTime)
import Data.Time.Format(parseTime,formatTime,defaultTimeLocale)
import Prelude(Maybe(..),(>>=))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude


type Date = Day

toDay :: Date0 -> Maybe Day
toDay (Date0 s) = parseTime defaultTimeLocale "%FT%XZ" (T.unpack s)

fromDay :: Day -> Date0
fromDay d = Date0 (T.pack (formatTime defaultTimeLocale "%FT%XZ" d))

instance ADLValue Day where
  atype _ = atype (defaultv :: Date0)
  defaultv = let (Just d) = toDay defaultv in d
  jsonSerialiser jf = JSONSerialiser to from
     where
       js = jsonSerialiser jf
       to d = aToJSON js (fromDay d) 
       from o = aFromJSON js o >>= toDay

newtype Date0 = Date0 { unDate0 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Date0 where
    atype _ = "examples.datetime.Date"
    
    defaultv = Date0 "1900-01-01"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (Date0 v) = aToJSON js v
            from o = Prelude.fmap Date0 (aFromJSON js o)


toUTCTime :: UTCTime0 -> Maybe UTCTime
toUTCTime (UTCTime0 s) = parseTime defaultTimeLocale "%FT%XZ" (T.unpack s)

fromUTCTime :: UTCTime -> UTCTime0
fromUTCTime d = UTCTime0 (T.pack (formatTime defaultTimeLocale "%FT%XZ" d))

instance ADLValue UTCTime where
  atype _ = atype (defaultv :: UTCTime0)
  defaultv = let (Just d) = toUTCTime defaultv in d
  jsonSerialiser jf = JSONSerialiser to from
     where
       js = jsonSerialiser jf
       to d = aToJSON js (fromUTCTime d) 
       from o = aFromJSON js o >>= toUTCTime

newtype UTCTime0 = UTCTime0 { unUTCTime0 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue UTCTime0 where
    atype _ = "examples.datetime.UTCTime"
    
    defaultv = UTCTime0 "1900-01-01T00:00:00Z"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (UTCTime0 v) = aToJSON js v
            from o = Prelude.fmap UTCTime0 (aFromJSON js o)