{-# LANGUAGE OverloadedStrings #-}
module Backends.Haskell where

import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Format
import AST
import Processing
import Primitive

data MState = MState {
   ms_name :: ModuleName,
   ms_indent :: T.Text,
   ms_imports :: Set.Set T.Text,
   ms_lines :: [T.Text]
}

initialMState mn = MState mn T.empty Set.empty []

type HGen = State MState

updateMState :: (MState->MState) -> HGen ()
updateMState = modify

writeLine :: T.Text -> HGen ()
writeLine t = updateMState addLine
  where
    addLine ms = ms{ms_lines=(ms_indent ms) `T.append` t:ms_lines ms}

-- | `template src substs` will replace all occurences the string $i
-- in src with `substs !! i`
template :: T.Text -> [T.Text] -> T.Text
template t substs = foldr replace t (zip [1,2..] substs)
  where
    replace (i,s) t = T.replace (T.pack ('$':show i)) s t

withIndent :: HGen a -> HGen a
withIndent g = do
    updateMState
      (\ms -> ms{ ms_indent=T.append is (ms_indent ms)})
    a <- g
    updateMState
      (\ms -> ms{ ms_indent=T.drop (T.length is) (ms_indent ms) } )
    return a
  where
    is = "    "

newtype HaskellModule = HaskellModule T.Text

instance Format HaskellModule where
  formatText (HaskellModule hm) = hm

importModule :: HaskellModule -> HGen ()
importModule (HaskellModule t) =  updateMState
  (\ms -> ms{ ms_imports=Set.insert s (ms_imports ms)} )
  where
    s = template "import qualified $1" [t]

importQualifiedModule :: HaskellModule -> T.Text -> HGen ()
importQualifiedModule (HaskellModule t) n =  updateMState
  (\ms -> ms{ ms_imports=Set.insert s (ms_imports ms)} )
  where
    s = template "import qualified $1 as $2" [t,n]

haskellModule :: ModuleName -> HGen HaskellModule
haskellModule mn = do
  let prefix = ["ADL","Compiled"]
      path = map upper1 (unModuleName mn)
      hm = HaskellModule (T.intercalate "." (prefix ++path) )
  return hm

importADLModule :: ModuleName -> HGen HaskellModule
importADLModule mn = do
  hm <- haskellModule mn
  importModule hm
  return hm

helpers = ( writeLine
          , writeLine ""  
          , \t ss -> writeLine (template t ss)
          , withIndent
          )

upper1,lower1 :: T.Text -> T.Text
upper1 t = T.toUpper (T.pack [(T.head t)]) `T.append` T.tail t
lower1 t = T.toLower (T.pack [(T.head t)]) `T.append` T.tail t

hTypeName :: Ident -> Ident
hTypeName sn = upper1 sn

hTypeParamName :: Ident -> Ident
hTypeParamName sn = lower1 sn

hFieldName :: Ident -> Ident -> Ident
hFieldName sn fn = T.concat [lower1 sn,"_",fn]

hDiscName :: Ident -> Ident -> Ident
hDiscName sn fn = T.concat [upper1 sn,"_",fn]

hPrimitiveType :: PrimitiveType -> HGen T.Text
hPrimitiveType P_Void = return "()"
hPrimitiveType P_Int = return "Int"
hPrimitiveType P_Double = return "Double"
hPrimitiveType P_ByteVector = do
  importQualifiedModule (HaskellModule "Data.ByteString")  "B"
  return "B.ByteString"
hPrimitiveType P_Vector = return "[]" -- never called
hPrimitiveType P_String = do
  importQualifiedModule (HaskellModule "Data.Text") "T"
  return "T.Text"
hPrimitiveType P_Sink = do
  importQualifiedModule (HaskellModule "ADL.IO.Sink") "S"
  return "S.Sink"

hTypeExpr :: TypeExpr ResolvedType -> HGen T.Text
hTypeExpr (TE_Ref rt) = hTypeExpr1 rt
hTypeExpr (TE_Apply (RT_Primitive P_Vector) args) = do
  argt <- hTypeExpr (head args)
  return (template "[$1]" [argt])
  
hTypeExpr (TE_Apply c args) = do
  ct <- hTypeExpr1 c
  argst <- mapM hTypeExpr args
  return (T.concat $ ["(", ct, " "] ++ L.intersperse " " argst ++ [")"])

hTypeExpr1 :: ResolvedType -> HGen T.Text
hTypeExpr1 (RT_Named (sn,d)) = do
  case sn_moduleName sn of
      ModuleName [] -> return (hTypeName (sn_name sn))
      mn -> do
        hm <- importADLModule mn
        return (T.intercalate "." [formatText hm,hTypeName (sn_name sn)])
        
hTypeExpr1 (RT_Param i) = return (hTypeParamName i)
hTypeExpr1 (RT_Primitive pt) = hPrimitiveType pt

hTParams :: [Ident] -> T.Text
hTParams [] = T.empty
hTParams ts = T.cons ' ' $ T.intercalate " " (map hTypeParamName ts)


generateDecl :: Decl ResolvedType -> HGen ()
generateDecl d@(Decl{d_type=(Decl_Struct s)}) = do
    let (wl,nl,wt,indent) = helpers
        sname = hTypeName (d_name d)
        prefixes = ["{"] ++ repeat ","

    wt "data $1$2 = $1" [sname,hTParams (s_typeParams s)]
    indent $ do
        forM_ (zip prefixes (s_fields s)) $ \(fp,f) -> do
          t <- hTypeExpr (f_type f)
          wt "$1 $2 :: $3" [fp,
                            hFieldName (d_name d) (f_name f),
                            t ]
        wl "}"
        wl "deriving (Eq,Ord,Show)"
    nl
    wt "instance DefaultV $1 where" [sname]
    indent $ do
        wl "defaultv = undefined"
    nl
    wt "instance WriteJSON $1 where" [sname]
    indent $ do
        wl "writeJSON = undefined"
    nl
    wt "instance ReadJSON $1 where" [sname]
    indent $ do
        wl "readJSON = undefined"

generateDecl d@(Decl{d_type=(Decl_Union u)}) = do
    let (wl,nl,wt,indent) = helpers
        sname = hTypeName (d_name d)
        prefixes = ["="] ++ repeat "|"

    wt "data $1$2" [sname,hTParams (u_typeParams u)]
    indent $ do
      forM_ (zip prefixes (u_fields u)) $ \(fp,f) -> do
        t <- hTypeExpr (f_type f)
        wt "$1 $2 $3" [fp,hDiscName (d_name d) (f_name f),t]
      wl "deriving (Eq,Ord,Show)"
    nl
    wt "instance DefaultV $1 where" [sname]
    indent $ do
        wl "defaultv = undefined"
    nl
    wt "instance WriteJSON $1 where" [sname]
    indent $ do
        wl "writeJSON = undefined"
    nl
    wt "instance ReadJSON $1 where" [sname]
    indent $ do
        wl "readJSON = undefined"

generateDecl d@(Decl{d_type=(Decl_Typedef t)}) = do
    let (wl,nl,wt,indent) = helpers
        sname = hTypeName (d_name d)

    ts <- hTypeExpr (t_typeExpr t)
    wt "type $1$2 = $3" [sname,hTParams (t_typeParams t),ts]

generateModule :: Module ResolvedType -> HGen ()
generateModule m = mapM_ genDecl (Map.elems $ m_decls m)
  where
    genDecl d = writeLine "" >> generateDecl d

moduleText :: Module ResolvedType -> HGen T.Text
moduleText m = do
  generateModule m
  ms <- get
  hm <- haskellModule (ms_name ms)
  let header = [template "module $1 where" [formatText hm]]
      imports = case Set.toList (ms_imports ms) of
        [] -> []
        lines -> "" : lines
      body = reverse (ms_lines ms)
  return (T.intercalate "\n" (header ++ imports ++ body))
  
generateHaskell rm = do
      let s0 = initialMState (m_name rm)
          t = evalState (moduleText rm) s0
      liftIO $ T.putStrLn t                  

