{-# LANGUAGE OverloadedStrings #-}
module BootstrapCustomTypes where

import qualified Data.Map as Map
import qualified Data.Text as T

import ADL.Compiler.AST
import ADL.Compiler.Backends.Haskell
import ADL.Compiler.Processing

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType scopedName _ = Map.lookup scopedName customTypes
  where
    customTypes = Map.fromList
      [ (ScopedName (ModuleName ["sys","types"]) "Maybe",
         CustomType "Prelude.Maybe" []
         ["type Maybe = Prelude.Maybe" ]
         ""
         (Map.fromList [("nothing", "Prelude.Nothing"), ("just", "Prelude.Just")])
         Nothing )
      , (ScopedName (ModuleName ["sys","types"]) "Either",
         CustomType "Prelude.Either" []
         ["type Either = Prelude.Either"]
         ""
         (Map.fromList [("left", "Prelude.Left"), ("right", "Prelude.Right")])
         Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Error",
         CustomType "Error" [HaskellModule "qualified Data.Text as T"]
         ["type Error a  = Prelude.Either T.Text a"]
         ""
         (Map.fromList [("error", "Prelude.Left"), ("value", "Prelude.Right")])
         Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Pair",
         CustomType "Pair" []
         ["type Pair a b = (a,b)"]
         "(\a b -> (a,b))"
         Map.empty
         Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Map",
         CustomType "Map" [HaskellModule "qualified Data.Map as Map"]
         ["type Map k v = Map.Map k v" ]
         "Map.fromList"
         Map.empty
         Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Set",
         CustomType "Set" [HaskellModule "qualified Data.Set as Set"]
         ["type Set v = Set.Set v"]
         "Set.fromList"
         Map.empty
         Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Nullable",
         CustomType "Nullable" []
         []
         ""
         (Map.fromList [("left", "Prelude.Left"), ("right", "Prelude.Right")])
         Nothing )
      ]
    
    


