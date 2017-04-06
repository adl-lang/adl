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

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type Int1 = Data.Int.Int64

newtype Int2 = Int2 { unInt2 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Int2 where
    atype _ = "test.Int2"
    
    defaultv = Int2 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (Int2 v) = aToJSON js v
            from o = Prelude.fmap Int2 (aFromJSON js o)

newtype Int3 = Int3 { unInt3 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Int3 where
    atype _ = "test.Int3"
    
    defaultv = Int3 42
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (Int3 v) = aToJSON js v
            from o = Prelude.fmap Int3 (aFromJSON js o)

type Int4 x = Data.Int.Int64

newtype Int5 x = Int5 { unInt5 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue x) => ADLValue (Int5 x) where
    atype _ = T.concat
        [ "test.Int5"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = Int5 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (Int5 v) = aToJSON js v
            from o = Prelude.fmap Int5 (aFromJSON js o)

newtype Int6 x = Int6 { unInt6 :: Data.Int.Int64 }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue x) => ADLValue (Int6 x) where
    atype _ = T.concat
        [ "test.Int6"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = Int6 43
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (Int6 v) = aToJSON js v
            from o = Prelude.fmap Int6 (aFromJSON js o)

type IntPoint1 = (Point Data.Int.Int64)

newtype IntPoint2 = IntPoint2 { unIntPoint2 :: (Point Data.Int.Int64) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue IntPoint2 where
    atype _ = "test.IntPoint2"
    
    defaultv = IntPoint2 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (IntPoint2 v) = aToJSON js v
            from o = Prelude.fmap IntPoint2 (aFromJSON js o)

newtype IntPoint3 = IntPoint3 { unIntPoint3 :: (Point Data.Int.Int64) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue IntPoint3 where
    atype _ = "test.IntPoint3"
    
    defaultv = IntPoint3 (defaultv :: (Point Data.Int.Int64)) { point_x = 5, point_y = 27 }
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (IntPoint3 v) = aToJSON js v
            from o = Prelude.fmap IntPoint3 (aFromJSON js o)

data Point t = Point
    { point_x :: t
    , point_y :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (Point t) where
    atype _ = T.concat
        [ "test.Point"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy t)
        , ">" ]
    
    defaultv = Point
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            x_js = jsonSerialiser jf
            y_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("x",aToJSON x_js (point_x v))
                , ("y",aToJSON y_js (point_y v))
                ] )
            
            from (JSON.Object hm) = Point 
                <$> fieldFromJSON x_js "x" defaultv hm
                <*> fieldFromJSON y_js "y" defaultv hm
            from _ = Prelude.Nothing

type Point1 x = (Point x)

newtype Point2 x = Point2 { unPoint2 :: (Point x) }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue x) => ADLValue (Point2 x) where
    atype _ = T.concat
        [ "test.Point2"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = Point2 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (Point2 v) = aToJSON js v
            from o = Prelude.fmap Point2 (aFromJSON js o)

type String1 = T.Text

newtype String2 = String2 { unString2 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue String2 where
    atype _ = "test.String2"
    
    defaultv = String2 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (String2 v) = aToJSON js v
            from o = Prelude.fmap String2 (aFromJSON js o)

newtype String3 = String3 { unString3 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue String3 where
    atype _ = "test.String3"
    
    defaultv = String3 "hello"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (String3 v) = aToJSON js v
            from o = Prelude.fmap String3 (aFromJSON js o)

type String4 x = T.Text

newtype String5 x = String5 { unString5 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue x) => ADLValue (String5 x) where
    atype _ = T.concat
        [ "test.String5"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = String5 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (String5 v) = aToJSON js v
            from o = Prelude.fmap String5 (aFromJSON js o)

newtype String6 x = String6 { unString6 :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue x) => ADLValue (String6 x) where
    atype _ = T.concat
        [ "test.String6"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy x)
        , ">" ]
    
    defaultv = String6 "goodbye"
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            js = jsonSerialiser jf
            to (String6 v) = aToJSON js v
            from o = Prelude.fmap String6 (aFromJSON js o)