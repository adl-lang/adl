{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Test where

import ADL.Core
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
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("x",aToJSON f (s1_x v))
        , ("y",aToJSON f (s1_y v))
        ] )
    
    aFromJSON f (JSON.Object hm) = S1
        <$> fieldFromJSON f "x" defaultv hm
        <*> fieldFromJSON f "y" defaultv hm
    aFromJSON _ _ = Prelude.Nothing

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
    
    aToJSON f v = toJSONObject f (atype v) (
        [ ("value",aToJSON f (tree_value v))
        , ("children",aToJSON f (tree_children v))
        ] )
    
    aFromJSON f (JSON.Object hm) = Tree
        <$> fieldFromJSON f "value" defaultv hm
        <*> fieldFromJSON f "children" defaultv hm
    aFromJSON _ _ = Prelude.Nothing