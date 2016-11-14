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
         CustomType "Prelude.Maybe" [HaskellModule "ADL.Core.CustomTypes"]
         ["type Maybe = Prelude.Maybe" ] Nothing )
      , (ScopedName (ModuleName ["sys","types"]) "Either",
         CustomType "Prelude.Either" [HaskellModule "ADL.Core.CustomTypes"]
         ["type Either = Prelude.Either"] Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Error",
         CustomType "Error" [HaskellModule "ADL.Core.CustomTypes",
                             HaskellModule "qualified Data.Text as T"]
         ["type Error a  = Prelude.Either T.Text a"] Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Pair",
         CustomType "Pair" [HaskellModule "ADL.Core.CustomTypes"]
         ["type Pair a b = (a,b)"] Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Map",
         CustomType "Map" [HaskellModule "ADL.Core.CustomTypes",
                           HaskellModule "qualified Data.Map as Map"]
         ["type Map k v = Map.Map k v" ] Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Set",
         CustomType "Set" [HaskellModule "ADL.Core.CustomTypes",
                           HaskellModule "qualified Data.Set as Set"]
         ["type Set v = Set.Set v"] Nothing)
      , (ScopedName (ModuleName ["sys","types"]) "Nullable",
         CustomType "Nullable" [HaskellModule "ADL.Core.CustomTypes"]
         [] Nothing )
      ]
    
    


