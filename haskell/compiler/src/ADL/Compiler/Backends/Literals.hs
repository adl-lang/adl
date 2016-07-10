{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Literals(
  Literal(..),
  MGen(..),
  mkDefaultLiteral,
  mkLiteral,
  literalIsDefault,
  litNumber
  ) where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Control.Monad

import ADL.Utils.Format
import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Core.Value

data Literal = LDefault Ident (TypeExpr ResolvedType)
             | LCtor Ident (TypeExpr ResolvedType) [Literal]
             | LUnion Ident T.Text Literal
             | LVector Ident [Literal]
             | LPrimitive Ident T.Text

type TypeBindingMap = Map.Map Ident (TypeExpr ResolvedType)

class (Monad m) => MGen m where
  getPrimitiveType :: PrimitiveType -> m T.Text
  getPrimitiveDefault :: PrimitiveType -> m (Maybe T.Text)
  getPrimitiveLiteral :: PrimitiveType -> JSON.Value -> m T.Text
  getTypeExpr :: Bool -> TypeExpr ResolvedType -> m T.Text
  getTypeExprB :: Bool -> TypeBindingMap -> TypeExpr ResolvedType -> m T.Text
  getUnionConstructorName :: Decl t -> Field t -> m Ident

mkDefaultLiteral :: (MGen m) => TypeExpr ResolvedType -> m Literal
mkDefaultLiteral te@(TypeExpr (RT_Primitive pt) []) = do
  mdef <- getPrimitiveDefault pt
  case mdef of
    Nothing -> do
      t <- getTypeExpr False te
      return (LDefault t te)
    Just l -> do
      ct <- getPrimitiveType pt
      return (LPrimitive ct l)
mkDefaultLiteral te = do
  t <- getTypeExpr False te
  return (LDefault t te)

mkLiteral :: (MGen m) => TypeExpr ResolvedType -> JSON.Value -> m Literal
mkLiteral te jv = mk Map.empty te jv
  where
    mk :: (MGen m) => TypeBindingMap -> TypeExpr ResolvedType -> JSON.Value -> m Literal
    mk _ (TypeExpr (RT_Primitive pt) []) v = do
      ct <- getPrimitiveType pt
      cl <- getPrimitiveLiteral pt v
      return (LPrimitive ct cl)
    mk  m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
     (Just te) -> mk m te v
     Nothing -> error "BUG: Failed to find type binding in mkLiteral"
    mk m (TypeExpr (RT_Primitive P_Vector) [te]) jv = mkVec m te jv
    mk m te0@(TypeExpr (RT_Named (_,decl)) tes) jv = case d_type decl of
      (Decl_Struct s) -> mkStruct m te0 decl s tes jv
      (Decl_Union u) -> mkUnion m te0 decl u tes jv 
      (Decl_Typedef t) -> mkTypedef m decl t tes jv
      (Decl_Newtype n) -> mkNewType m te0 decl n tes jv

    mkVec m te (JSON.Array v) = do
      t <- getTypeExprB False m te
      vals <- mapM (mk m te) (V.toList v)
      return (LVector t vals)
      
    mkStruct m te0 d s tes (JSON.Object hm) = do
      t <- getTypeExprB False m te0
      fields1 <- forM (s_fields s) $ \f -> do
        case HM.lookup (f_name f) hm of
          Nothing -> mkDefaultLiteral (f_type f) 
          (Just jv) -> mk m2 (f_type f) jv
      return (LCtor t te0 fields1)
      where
        m2 = m `Map.union` Map.fromList (zip (s_typeParams s) tes)
      
    mkUnion  m te0 d u tes (JSON.Object hm) = do
      t <- getTypeExprB False m te0
      let [(fname,jv)] = HM.toList hm
          f = getF fname
      lv <- mk m (f_type f) jv
      ctorName <- getUnionConstructorName d f
      return (LUnion t ctorName lv)
      where
        getF fname = case L.find (\f -> f_name f == fname) (u_fields u) of
          Just f -> f
        m2 = m `Map.union` Map.fromList (zip (u_typeParams u) tes)

    mkTypedef m d t tes v = mk m2 (t_typeExpr t) v
      where
        m2 = m `Map.union` Map.fromList (zip (t_typeParams t) tes)

    mkNewType m te0 d n tes v = do
      t <- getTypeExprB False m te0
      lv <- mk m2 (n_typeExpr n) v
      return (LCtor t te0 [lv])
      where
        m2 = m `Map.union` Map.fromList (zip (n_typeParams n) tes)

literalIsDefault :: Literal -> Bool
literalIsDefault (LDefault _ _) = True
literalIsDefault _ = False

litNumber :: S.Scientific -> T.Text
litNumber n = T.pack s
  where
    s = case S.floatingOrInteger n of
      (Left r) -> show n
      (Right i) -> show (i::Integer)
