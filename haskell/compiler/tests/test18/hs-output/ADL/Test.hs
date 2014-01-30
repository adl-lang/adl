{-# LANGUAGE OverloadedStrings #-}
module ADL.Test(
    X1(..),
    X2(..),
    Y1(..),
    Y2(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Prelude

data X1
    = X1_f1 Prelude.Double
    | X1_f2 Y1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue X1 where
    atype _ = "test.X1"
    
    defaultv = X1_f1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to (X1_f1 v) = JSON.Object (HM.singleton "f1" (aToJSON f1_js v))
            to (X1_f2 v) = JSON.Object (HM.singleton "f2" (aToJSON f2_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "f1" -> Prelude.fmap X1_f1 (aFromJSON f1_js v)
                    "f2" -> Prelude.fmap X1_f2 (aFromJSON f2_js v)

data X2 = X2
    { x2_f1 :: Prelude.Double
    , x2_f2 :: [Y2]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue X2 where
    atype _ = "test.X2"
    
    defaultv = X2
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (x2_f1 v))
                , ("f2",aToJSON f2_js (x2_f2 v))
                ] )
            
            from (JSON.Object hm) = X2 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
            from _ = Prelude.Nothing

data Y1
    = Y1_f1 T.Text
    | Y1_f2 X1
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Y1 where
    atype _ = "test.Y1"
    
    defaultv = Y1_f1 defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to (Y1_f1 v) = JSON.Object (HM.singleton "f1" (aToJSON f1_js v))
            to (Y1_f2 v) = JSON.Object (HM.singleton "f2" (aToJSON f2_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "f1" -> Prelude.fmap Y1_f1 (aFromJSON f1_js v)
                    "f2" -> Prelude.fmap Y1_f2 (aFromJSON f2_js v)

data Y2 = Y2
    { y2_f1 :: T.Text
    , y2_f2 :: [X2]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Y2 where
    atype _ = "test.Y2"
    
    defaultv = Y2
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (y2_f1 v))
                , ("f2",aToJSON f2_js (y2_f2 v))
                ] )
            
            from (JSON.Object hm) = Y2 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
            from _ = Prelude.Nothing