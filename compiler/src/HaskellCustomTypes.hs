{-# LANGUAGE OverloadedStrings #-}
module HaskellCustomTypes where

import Control.Monad.Trans

import qualified Data.Map as Map
import qualified Data.Text as T

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Utils.Format

import ADL.Compiler.Backends.Haskell

import qualified Text.Parsec as P
import qualified ADL.Compiler.ParserP as P

import ADL.Core.Value
import qualified ADL.Adlc.Config.Haskell as HC

type EIOT = EIO T.Text

getCustomTypes :: [FilePath] -> EIOT CustomTypeMap
getCustomTypes fps = fmap Map.unions (mapM get0 fps)
  where
    get0 :: FilePath -> EIOT CustomTypeMap
    get0 fp = do
      mv <- liftIO $ aFromJSONFile jsflags fp
      case mv of
        Nothing -> eioError (template "Unable to read haskell custom types from  $1" [T.pack fp])
        Just v -> convert (HC.config_customTypes v)
    jsflags = JSONFlags True

    convert :: [HC.CustomType] -> EIOT CustomTypeMap
    convert cs = fmap Map.fromList (mapM convert1 cs)

    convert1 :: HC.CustomType -> EIOT (ScopedName,CustomType)
    convert1 c = do
        sn <- case P.parse P.scopedName "" adlname of
          (Right sn) -> return sn
          _ -> eioError (template "Unable to parse adl name $1" [adlname])
        return (sn,CustomType tn imports gc)
      where
        adlname = HC.customType_adlname c
        tn = HC.customType_haskellname c
        gc = HC.customType_generateCode c
        imports = map HaskellModule (HC.customType_haskellimports c)
    
    


