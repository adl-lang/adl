{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test(
    IntTree,
    S1(..),
    S2(..),
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

data S1 = S1
    { s1_x :: Data.Int.Int32
    , s1_y :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S1 where
    atype _ = "test.S1"
    
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
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue S2 where
    atype _ = "test.S2"
    
    defaultv = S2
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            f1_js = jsonSerialiser jf
            f2_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("f1",aToJSON f1_js (s2_f1 v))
                , ("f2",aToJSON f2_js (s2_f2 v))
                ] )
            
            from (JSON.Object hm) = S2 
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
        [ "test.Tree"
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