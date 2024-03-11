{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test7(
    Int1,
    Int2(..),
    Int3(..),
    Int4,
    Int5(..),
    Int6(..),
    IntPoint1,
    IntPoint1A,
    IntPoint2(..),
    IntPoint3(..),
    Point(..),
    Point1,
    Point2(..),
    S(..),
    String1,
    String2(..),
    String3(..),
    String4,
    String5(..),
    String6(..),
    mkPoint,
    mkS,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type Int1 = Data.Int.Int64

newtype Int2 = Int2 { unInt2 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Int2 where
    atype _ = "test7.Int2"
    
    jsonGen = JsonGen (\(Int2 v) -> adlToJson v)
    
    jsonParser = Int2 <$> jsonParser

newtype Int3 = Int3 { unInt3 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Int3 where
    atype _ = "test7.Int3"
    
    jsonGen = JsonGen (\(Int3 v) -> adlToJson v)
    
    jsonParser = Int3 <$> jsonParser

type Int4 x = Data.Int.Int64

newtype Int5 x = Int5 { unInt5 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (Int5 x) where
    atype _ = T.concat
        [ "test7.Int5"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    jsonGen = JsonGen (\(Int5 v) -> adlToJson v)
    
    jsonParser = Int5 <$> jsonParser

newtype Int6 x = Int6 { unInt6 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (Int6 x) where
    atype _ = T.concat
        [ "test7.Int6"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    jsonGen = JsonGen (\(Int6 v) -> adlToJson v)
    
    jsonParser = Int6 <$> jsonParser

type IntPoint1 = (Point Data.Int.Int64)

type IntPoint1A = IntPoint1

newtype IntPoint2 = IntPoint2 { unIntPoint2 :: (Point Data.Int.Int64) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue IntPoint2 where
    atype _ = "test7.IntPoint2"
    
    jsonGen = JsonGen (\(IntPoint2 v) -> adlToJson v)
    
    jsonParser = IntPoint2 <$> jsonParser

newtype IntPoint3 = IntPoint3 { unIntPoint3 :: (Point Data.Int.Int64) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue IntPoint3 where
    atype _ = "test7.IntPoint3"
    
    jsonGen = JsonGen (\(IntPoint3 v) -> adlToJson v)
    
    jsonParser = IntPoint3 <$> jsonParser

data Point t = Point
    { point_x :: t
    , point_y :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkPoint :: t -> t -> Point t
mkPoint x y = Point x y

instance (AdlValue t) => AdlValue (Point t) where
    atype _ = T.concat
        [ "test7.Point"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    jsonGen = genObject
        [ genField "x" point_x
        , genField "y" point_y
        ]
    
    jsonParser = Point
        <$> parseField "x"
        <*> parseField "y"

type Point1 x = (Point x)

newtype Point2 x = Point2 { unPoint2 :: (Point x) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (Point2 x) where
    atype _ = T.concat
        [ "test7.Point2"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    jsonGen = JsonGen (\(Point2 v) -> adlToJson v)
    
    jsonParser = Point2 <$> jsonParser

data S = S
    { s_f1 :: IntPoint1A
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkS :: IntPoint1A -> S
mkS f1 = S f1

instance AdlValue S where
    atype _ = "test7.S"
    
    jsonGen = genObject
        [ genField "f1" s_f1
        ]
    
    jsonParser = S
        <$> parseField "f1"

type String1 = T.Text

newtype String2 = String2 { unString2 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue String2 where
    atype _ = "test7.String2"
    
    jsonGen = JsonGen (\(String2 v) -> adlToJson v)
    
    jsonParser = String2 <$> jsonParser

newtype String3 = String3 { unString3 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue String3 where
    atype _ = "test7.String3"
    
    jsonGen = JsonGen (\(String3 v) -> adlToJson v)
    
    jsonParser = String3 <$> jsonParser

type String4 x = T.Text

newtype String5 x = String5 { unString5 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (String5 x) where
    atype _ = T.concat
        [ "test7.String5"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    jsonGen = JsonGen (\(String5 v) -> adlToJson v)
    
    jsonParser = String5 <$> jsonParser

newtype String6 x = String6 { unString6 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (String6 x) where
    atype _ = T.concat
        [ "test7.String6"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    jsonGen = JsonGen (\(String6 v) -> adlToJson v)
    
    jsonParser = String6 <$> jsonParser