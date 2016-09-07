{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test2(
    IntTree,
    S0(..),
    S1(..),
    S2(..),
    S3(..),
    S4(..),
    Tree(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Prelude

type IntTree = (Tree Data.Int.Int32)

data S0 = S0
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S0 where
    atype _ = "test2.S0"
    
    defaultv = S0
    jsonSerialiser jf = JSONSerialiser to from
        where
            to v = JSON.Object HM.empty
            from (JSON.Object hm) = Prelude.Just S0 
            from _ = Prelude.Nothing

data S1 = S1
    { s1_x :: Data.Int.Int32
    , s1_y :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S1 where
    atype _ = "test2.S1"
    
    defaultv = S1
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            x_js = jsonSerialiser jf
            y_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("x",aToJSON x_js (s1_x v))
                , ("y",aToJSON y_js (s1_y v))
                ] )
            
            from (JSON.Object hm) = S1 
                <$> fieldFromJSON x_js "x" defaultv hm
                <*> fieldFromJSON y_js "y" defaultv hm
            from _ = Prelude.Nothing

data S2 = S2
    { s2_f1 :: T.Text
    , s2_f2 :: Prelude.Double
    , s2_f3 :: [Data.Int.Int32]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S2 where
    atype _ = "test2.S2"
    
    defaultv = S2
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            f3_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s2_f1 v))
                , ("f2",aToJSON f2_js (s2_f2 v))
                , ("f3",aToJSON f3_js (s2_f3 v))
                ] )
            
            from (JSON.Object hm) = S2 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
                <*> fieldFromJSON f3_js "f3" defaultv hm
            from _ = Prelude.Nothing

data S3 t = S3
    { s3_f1 :: T.Text
    , s3_f2 :: Prelude.Double
    , s3_f3 :: t
    , s3_f4 :: [t]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (S3 t) where
    atype _ = T.concat
        [ "test2.S3"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = S3
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            f3_js = jsonSerialiser jf
            f4_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s3_f1 v))
                , ("f2",aToJSON f2_js (s3_f2 v))
                , ("f3",aToJSON f3_js (s3_f3 v))
                , ("f4",aToJSON f4_js (s3_f4 v))
                ] )
            
            from (JSON.Object hm) = S3 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
                <*> fieldFromJSON f3_js "f3" defaultv hm
                <*> fieldFromJSON f4_js "f4" defaultv hm
            from _ = Prelude.Nothing

data S4 t = S4
    { s4_f1 :: (S3 T.Text)
    , s4_f2 :: (S3 t)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (S4 t) where
    atype _ = T.concat
        [ "test2.S4"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = S4
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s4_f1 v))
                , ("f2",aToJSON f2_js (s4_f2 v))
                ] )
            
            from (JSON.Object hm) = S4 
                <$> fieldFromJSON f1_js "f1" defaultv hm
                <*> fieldFromJSON f2_js "f2" defaultv hm
            from _ = Prelude.Nothing

data Tree t = Tree
    { tree_value :: t
    , tree_children :: [(Tree t)]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance (ADLValue t) => ADLValue (Tree t) where
    atype _ = T.concat
        [ "test2.Tree"
        , "<", atype (Prelude.undefined ::t)
        , ">" ]
    
    defaultv = Tree
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            value_js = jsonSerialiser jf
            children_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("value",aToJSON value_js (tree_value v))
                , ("children",aToJSON children_js (tree_children v))
                ] )
            
            from (JSON.Object hm) = Tree 
                <$> fieldFromJSON value_js "value" defaultv hm
                <*> fieldFromJSON children_js "children" defaultv hm
            from _ = Prelude.Nothing