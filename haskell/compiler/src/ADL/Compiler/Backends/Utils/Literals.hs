{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Utils.Literals(
  Literal(..),
  LGen(..),
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
import ADL.Compiler.Processing hiding (Literal(..))
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Core.Value

data Literal c
  = LDefault Ident (TypeExpr (ResolvedTypeT c))
  | LCtor Ident (TypeExpr (ResolvedTypeT c)) [Literal c]
  | LUnion Ident T.Text (TypeExpr (ResolvedTypeT c)) (Literal c)
  | LVector Ident [Literal c]
  | LPrimitive Ident T.Text

type TypeBindingMap c = Map.Map Ident (TypeExpr (ResolvedTypeT c))

-- Record of helper functions required to generated a literal expression.
data LGen m c = LGen {
  getPrimitiveType :: PrimitiveType -> m T.Text,
  getPrimitiveDefault :: PrimitiveType -> m (Maybe T.Text),
  getPrimitiveLiteral :: PrimitiveType -> JSON.Value -> m T.Text,
  getTypeExpr :: Bool -> TypeExpr (ResolvedTypeT c) -> m T.Text,
  getTypeExprB :: Bool -> (TypeBindingMap c) -> TypeExpr (ResolvedTypeT c) -> m T.Text,
  getUnionConstructorName :: Decl (ResolvedTypeT c) -> Field (ResolvedTypeT c) -> m Ident
}

mkDefaultLiteral :: (Monad m) => LGen m c -> TypeExpr (ResolvedTypeT c) -> m (Literal c)
mkDefaultLiteral lg te@(TypeExpr (RT_Primitive pt) []) = do
  mdef <- getPrimitiveDefault lg pt
  case mdef of
    Nothing -> do
      t <- getTypeExpr lg False te
      return (LDefault t te)
    Just l -> do
      ct <- getPrimitiveType lg pt
      return (LPrimitive ct l)
mkDefaultLiteral lg te = do
  t <- getTypeExpr lg False te
  return (LDefault t te)

mkLiteral :: (Monad m) => LGen m c -> TypeExpr (ResolvedTypeT c) -> JSON.Value -> m (Literal c)
mkLiteral lg te jv = mk Map.empty te jv
  where
    mk _ (TypeExpr (RT_Primitive pt) []) v = do
      ct <- getPrimitiveType lg pt
      cl <- getPrimitiveLiteral lg pt v
      return (LPrimitive ct cl)
    mk  m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
     (Just te) -> mk m te v
     Nothing -> error "BUG: Failed to find type binding in mkLiteral"
    mk m (TypeExpr (RT_Primitive P_Vector) [te]) jv = mkVec m te jv
    mk m te0@(TypeExpr (RT_Named (_,decl,_)) tes) jv = case d_type decl of
      (Decl_Struct s) -> mkStruct m te0 decl s tes jv
      (Decl_Union u) -> mkUnion m te0 decl u tes jv 
      (Decl_Typedef t) -> mkTypedef m decl t tes jv
      (Decl_Newtype n) -> mkNewType m te0 decl n tes jv

    mkVec m te (JSON.Array v) = do
      t <- getTypeExprB lg False m te
      vals <- mapM (mk m te) (V.toList v)
      return (LVector t vals)
      
    mkStruct m te0 d s tes (JSON.Object hm) = do
      t <- getTypeExprB lg False m te0
      fields1 <- forM (s_fields s) $ \f -> do
        case HM.lookup (f_name f) hm of
          Nothing -> mkDefaultLiteral lg (f_type f) 
          (Just jv) -> mk m2 (f_type f) jv
      return (LCtor t te0 fields1)
      where
        m2 = m `Map.union` Map.fromList (zip (s_typeParams s) tes)
      
    mkUnion  m te0 d u tes (JSON.Object hm) = do
      t <- getTypeExprB lg False m2 te0
      let [(fname,jv)] = HM.toList hm
          f = getF fname
      lv <- mk m2 (f_type f) jv
      ctorName <- getUnionConstructorName lg d f
      return (LUnion t ctorName te0 lv)
      where
        getF fname = case L.find (\f -> f_name f == fname) (u_fields u) of
          Just f -> f
        m2 = m `Map.union` Map.fromList (zip (u_typeParams u) tes)

    mkTypedef m d t tes v = mk m2 (t_typeExpr t) v
      where
        m2 = m `Map.union` Map.fromList (zip (t_typeParams t) tes)

    mkNewType m te0 d n tes v = do
      t <- getTypeExprB lg False m te0
      lv <- mk m2 (n_typeExpr n) v
      return (LCtor t te0 [lv])
      where
        m2 = m `Map.union` Map.fromList (zip (n_typeParams n) tes)

literalIsDefault :: (Literal c) -> Bool
literalIsDefault (LDefault _ _) = True
literalIsDefault _ = False

litNumber :: S.Scientific -> T.Text
litNumber n = T.pack s
  where
    s = case S.floatingOrInteger n of
      (Left r) -> show n
      (Right i) -> show (i::Integer)
