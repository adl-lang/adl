{-# LANGUAGE OverloadedStrings #-}
module HaskellCustomTypes where

import qualified Data.Map as Map
import qualified Data.Text as T

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Backends.Haskell

getCustomTypes :: [FilePath] -> EIO T.Text CustomTypeMap
getCustomTypes srcdirs = return $ Map.fromList
    [ (ScopedName (ModuleName ["sys","types"]) "maybe",
       CustomType "Prelude.Maybe" [HaskellModule "ADL.Core.CustomTypes"] )
    , (ScopedName (ModuleName ["sys","types"]) "either",
       CustomType "Prelude.Either" [HaskellModule "ADL.Core.CustomTypes"] )
    , (ScopedName (ModuleName ["sys","types"]) "error",
       CustomType "Error" [HaskellModule "ADL.Core.CustomTypes"] )
    , (ScopedName (ModuleName ["sys","types"]) "pair",
       CustomType "Pair" [HaskellModule "ADL.Core.CustomTypes"] )
    , (ScopedName (ModuleName ["sys","types"]) "map",
       CustomType "Map" [HaskellModule "ADL.Core.CustomTypes"] )
    , (ScopedName (ModuleName ["sys","types"]) "set",
       CustomType "Set" [HaskellModule "ADL.Core.CustomTypes"] )
    ]
    
    


