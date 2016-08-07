{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Utils.Literals2(
  Literal(..),
  mkLiteral,
  litNumber,
  literalIsDefault,
  ) where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Monoid
import Control.Monad

import ADL.Utils.Format
import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Core.Value

-- | Represent the construction of a literal value in a
-- language independent way

data Literal te
  = LDefault te
  | LCtor te [Literal te]
  | LUnion te Ident (Literal te)
  | LVector te [Literal te]
  | LPrimitive PrimitiveType JSON.Value

type ErrOr t = Either T.Text t
    
eFromJSON :: (JSON.FromJSON a) => JSON.Value -> ErrOr a
eFromJSON jv = case JSON.fromJSON jv of
  (JSON.Error msg) -> Left (T.pack msg)
  (JSON.Success v) -> Right v

type TypeExprRT c = TypeExpr (ResolvedTypeT c)

mkLiteral :: TypeExprRT c -> JSON.Value -> ErrOr (Literal (TypeExprRT c))
mkLiteral te jv = mk Map.empty te jv
  where
    mk :: Map.Map Ident (TypeExprRT c)
       -> TypeExprRT c
       -> JSON.Value
       -> ErrOr (Literal (TypeExprRT c))
       
    mk _ (TypeExpr (RT_Primitive pt) []) jv = return (LPrimitive pt jv)
        
    mk m te@(TypeExpr (RT_Primitive P_Vector) [te1]) jv = do
      vs <- eFromJSON jv
      lits <- mapM (mk m te1) vs
      return (LVector te lits)
    
    mk  m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
      (Just te) -> mk m te v
      Nothing -> Left ("Failed to find type binding in mkLiteral: " <> id <> ", " <> T.pack (show (Map.keys m)))
    
    mk m te0@(TypeExpr (RT_Named (_,decl,_)) tes) jv = case d_type decl of
      (Decl_Struct s) -> mkStruct m' te0 s tes jv
      (Decl_Union u) -> mkUnion m' te0 decl u tes jv 
      (Decl_Typedef t) -> mkTypedef m' decl t tes jv
      (Decl_Newtype n) -> mkNewType m' te0 decl n tes jv
      where
        m' = m `Map.union` Map.fromList (zip (getTypeParams (d_type decl)) tes)

    mkStruct m te0 s tes jv = do
      hm <- eFromJSON jv
      fields1 <- forM (s_fields s) $ \f -> do
        case HM.lookup (f_name f) hm of
          Nothing -> return (LDefault (f_type f))
          (Just jv) -> mk m (f_type f) jv
      return (LCtor te0 fields1)
      
    mkTypedef m d t tes v = mk m(t_typeExpr t) v

    mkNewType m te0 d n tes v = do
      lv <- mk m (n_typeExpr n) jv
      return (LCtor te0 [lv])

    mkUnion  m te0 d u tes jv0 = do
      hm <- eFromJSON jv0
      (fname,jv) <- case HM.toList hm of
        [pair] -> Right pair
        _ -> Left "Malformed union literal value"
      f <- case L.find (\f -> f_name f == fname) (u_fields u) of
        (Just f) -> Right f
        _ -> Left "Union literal with unnknown field name"
      lv <- mk m (f_type f) jv
      return (LUnion te0 (f_name f) lv)

literalIsDefault :: (Literal c) -> Bool
literalIsDefault (LDefault _) = True
literalIsDefault _ = False

litNumber :: S.Scientific -> T.Text
litNumber n = T.pack s
  where
    s = case S.floatingOrInteger n of
      (Left r) -> show n
      (Right i) -> show (i::Integer)
