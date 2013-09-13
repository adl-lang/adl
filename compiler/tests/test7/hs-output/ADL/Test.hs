{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test(
    Int1,
    Int2,
    Int3,
    Int4,
    Int5,
    Int6,
    IntPoint1,
    IntPoint2,
    IntPoint3,
    Point(..),
    Point1,
    Point2,
    String1,
    String2,
    String3,
    String4,
    String5,
    String6,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude

type Int1 = Data.Int.Int64

newtype Int2 = Int2 Data.Int.Int64

instance ADLValue Int2 where
    atype _ = "test.Int2"
    
    defaultv = Int2 defaultv
    aToJSON f (Int2 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap Int2 (aFromJSON f o)

newtype Int3 = Int3 Data.Int.Int64

instance ADLValue Int3 where
    atype _ = "test.Int3"
    
    defaultv = Int3 42
    aToJSON f (Int3 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap Int3 (aFromJSON f o)

type Int4 x = Data.Int.Int64

newtype Int5 x = Int5 Data.Int.Int64

instance (ADLValue x) => ADLValue (Int5 x) where
    atype _ = T.concat
        [ "test.Int5"
        , "<", atype (Prelude.undefined ::x)
        , ">" ]
    
    defaultv = Int5 defaultv
    aToJSON f (Int5 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap Int5 (aFromJSON f o)

newtype Int6 x = Int6 Data.Int.Int64

instance (ADLValue x) => ADLValue (Int6 x) where
    atype _ = T.concat
        [ "test.Int6"
        , "<", atype (Prelude.undefined ::x)
        , ">" ]
    
    defaultv = Int6 43
    aToJSON f (Int6 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap Int6 (aFromJSON f o)

type IntPoint1 = (Point Data.Int.Int64)

newtype IntPoint2 = IntPoint2 (Point Data.Int.Int64)

instance ADLValue IntPoint2 where
    atype _ = "test.IntPoint2"
    
    defaultv = IntPoint2 defaultv
    aToJSON f (IntPoint2 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap IntPoint2 (aFromJSON f o)

newtype IntPoint3 = IntPoint3 (Point Data.Int.Int64)

instance ADLValue IntPoint3 where
    atype _ = "test.IntPoint3"
    
    defaultv = IntPoint3 (defaultv :: (Point Data.Int.Int64)) { point_x = 5, point_y = 27 }
    aToJSON f (IntPoint3 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap IntPoint3 (aFromJSON f o)

data Point t = Point
    { point_x :: t
    , point_y :: t
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (Point t) where
    atype _ = T.concat
        [ "test.Point"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = Point
        defaultv
        defaultv
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("x",aToJSON f (point_x v))
        , ("y",aToJSON f (point_y v))
        ] )
    
    aFromJSON f (JSON.Object hm) = Point
        <$> fieldFromJSON f "x" defaultv hm
        <*> fieldFromJSON f "y" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

type Point1 x = (Point x)

newtype Point2 x = Point2 (Point x)

instance (ADLValue x) => ADLValue (Point2 x) where
    atype _ = T.concat
        [ "test.Point2"
        , "<", atype (Prelude.undefined ::x)
        , ">" ]
    
    defaultv = Point2 defaultv
    aToJSON f (Point2 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap Point2 (aFromJSON f o)

type String1 = T.Text

newtype String2 = String2 T.Text

instance ADLValue String2 where
    atype _ = "test.String2"
    
    defaultv = String2 defaultv
    aToJSON f (String2 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap String2 (aFromJSON f o)

newtype String3 = String3 T.Text

instance ADLValue String3 where
    atype _ = "test.String3"
    
    defaultv = String3 "hello"
    aToJSON f (String3 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap String3 (aFromJSON f o)

type String4 x = T.Text

newtype String5 x = String5 T.Text

instance (ADLValue x) => ADLValue (String5 x) where
    atype _ = T.concat
        [ "test.String5"
        , "<", atype (Prelude.undefined ::x)
        , ">" ]
    
    defaultv = String5 defaultv
    aToJSON f (String5 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap String5 (aFromJSON f o)

newtype String6 x = String6 T.Text

instance (ADLValue x) => ADLValue (String6 x) where
    atype _ = T.concat
        [ "test.String6"
        , "<", atype (Prelude.undefined ::x)
        , ">" ]
    
    defaultv = String6 "goodbye"
    aToJSON f (String6 v) = aToJSON f v
    aFromJSON f o = Prelude.fmap String6 (aFromJSON f o)