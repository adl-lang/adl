{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test(
    Int1,
    Int2(..),
    Int3(..),
    Int4,
    Int5(..),
    Int6(..),
    IntPoint1,
    IntPoint2(..),
    IntPoint3(..),
    Point(..),
    Point1,
    Point2(..),
    String1,
    String2(..),
    String3(..),
    String4,
    String5(..),
    String6(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type Int1 = Data.Int.Int64

newtype Int2 = Int2 { unInt2 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Int2 where
    atype _ = "test.Int2"
    
    defaultv = Int2 defaultv
    
    jsonGen = JsonGen (\(Int2 v) -> adlToJson v)
    
    jsonParser = Int2 <$> jsonParser

newtype Int3 = Int3 { unInt3 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Int3 where
    atype _ = "test.Int3"
    
    defaultv = Int3 42
    
    jsonGen = JsonGen (\(Int3 v) -> adlToJson v)
    
    jsonParser = Int3 <$> jsonParser

type Int4 x = Data.Int.Int64

newtype Int5 x = Int5 { unInt5 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (Int5 x) where
    atype _ = T.concat
        [ "test.Int5"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = Int5 defaultv
    
    jsonGen = JsonGen (\(Int5 v) -> adlToJson v)
    
    jsonParser = Int5 <$> jsonParser

newtype Int6 x = Int6 { unInt6 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (Int6 x) where
    atype _ = T.concat
        [ "test.Int6"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = Int6 43
    
    jsonGen = JsonGen (\(Int6 v) -> adlToJson v)
    
    jsonParser = Int6 <$> jsonParser

type IntPoint1 = (Point Data.Int.Int64)

newtype IntPoint2 = IntPoint2 { unIntPoint2 :: (Point Data.Int.Int64) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue IntPoint2 where
    atype _ = "test.IntPoint2"
    
    defaultv = IntPoint2 defaultv
    
    jsonGen = JsonGen (\(IntPoint2 v) -> adlToJson v)
    
    jsonParser = IntPoint2 <$> jsonParser

newtype IntPoint3 = IntPoint3 { unIntPoint3 :: (Point Data.Int.Int64) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue IntPoint3 where
    atype _ = "test.IntPoint3"
    
    defaultv = IntPoint3 (Point 5 27)
    
    jsonGen = JsonGen (\(IntPoint3 v) -> adlToJson v)
    
    jsonParser = IntPoint3 <$> jsonParser

data Point t = Point
    { point_x :: t
    , point_y :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue t) => AdlValue (Point t) where
    atype _ = T.concat
        [ "test.Point"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = Point
        defaultv
        defaultv
    
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
        [ "test.Point2"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = Point2 defaultv
    
    jsonGen = JsonGen (\(Point2 v) -> adlToJson v)
    
    jsonParser = Point2 <$> jsonParser

type String1 = T.Text

newtype String2 = String2 { unString2 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue String2 where
    atype _ = "test.String2"
    
    defaultv = String2 defaultv
    
    jsonGen = JsonGen (\(String2 v) -> adlToJson v)
    
    jsonParser = String2 <$> jsonParser

newtype String3 = String3 { unString3 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue String3 where
    atype _ = "test.String3"
    
    defaultv = String3 "hello"
    
    jsonGen = JsonGen (\(String3 v) -> adlToJson v)
    
    jsonParser = String3 <$> jsonParser

type String4 x = T.Text

newtype String5 x = String5 { unString5 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (String5 x) where
    atype _ = T.concat
        [ "test.String5"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = String5 defaultv
    
    jsonGen = JsonGen (\(String5 v) -> adlToJson v)
    
    jsonParser = String5 <$> jsonParser

newtype String6 x = String6 { unString6 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (AdlValue x) => AdlValue (String6 x) where
    atype _ = T.concat
        [ "test.String6"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = String6 "goodbye"
    
    jsonGen = JsonGen (\(String6 v) -> adlToJson v)
    
    jsonParser = String6 <$> jsonParser