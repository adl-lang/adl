{-# LANGUAGE OverloadedStrings #-}
module Backends.Haskell where

import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L

import System.Directory(createDirectoryIfMissing)
import System.FilePath(takeDirectory,joinPath,addExtension)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Format
import AST
import EIO
import Processing
import Primitive

data MState = MState {
   ms_name :: ModuleName,
   ms_moduleMapper :: ModuleName -> HaskellModule,
   ms_indent :: T.Text,
   ms_imports :: Set.Set T.Text,
   ms_lines :: [T.Text]
}

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


addImport :: T.Text -> HGen ()
addImport t = updateMState
  (\ms -> ms{ ms_imports=Set.insert t (ms_imports ms)} )

importModule :: HaskellModule -> HGen ()
importModule (HaskellModule t) =
  addImport (template "import $1" [t])

importQualifiedModule :: HaskellModule -> HGen ()
importQualifiedModule (HaskellModule t) =
  addImport (template "import qualified $1" [t])

importQualifiedModuleAs :: HaskellModule -> T.Text -> HGen ()
importQualifiedModuleAs (HaskellModule t) n =
  addImport (template "import qualified $1 as $2" [t,n])

haskellModule :: ModuleName -> HGen HaskellModule
haskellModule mn = do
  ms <- get
  return (ms_moduleMapper ms mn)

importADLModule :: ModuleName -> HGen HaskellModule
importADLModule mn = do
  hm <- haskellModule mn
  importQualifiedModule hm
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
  importQualifiedModuleAs (HaskellModule "Data.ByteString")  "B"
  return "B.ByteString"
hPrimitiveType P_Vector = return "[]" -- never called
hPrimitiveType P_String = do
  importQualifiedModuleAs (HaskellModule "Data.Text") "T"
  return "T.Text"
hPrimitiveType P_Sink = do
  return "Sink"

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

hInstanceHeader klass sname [] =
  template "instance $1 $2 where" [klass,sname]
hInstanceHeader klass sname tps =
  template "instance ($1) => $2 ($3$4) where"
           [constraints,klass,sname,hTParams tps]
  where
    constraints = T.intercalate ", " [template "$1 $2" [klass, hTypeParamName tp]
                                     | tp <- tps ]
  


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
    wl $ hInstanceHeader "DefaultV" sname (s_typeParams s)
    indent $ do
        wt "defaultv = $1 $2" [sname, T.intercalate " " ["defaultv" | f <- s_fields s]]
    nl
    wl $ hInstanceHeader "AToJSON" sname (s_typeParams s)
    indent $ do
        wl "atoJSON = undefined"
    nl
    wl $ hInstanceHeader "AFromJSON" sname (s_typeParams s)
    indent $ do
        wl "afromJSON = undefined"

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
    wl $ hInstanceHeader "DefaultV" sname (u_typeParams u)
    indent $ do
        wt "defaultv = $1 defaultv"
           [hDiscName (d_name d) (f_name (head (u_fields u)))]
    nl
    wl $ hInstanceHeader "AToJSON" sname (u_typeParams u)
    indent $ do
        wl "atoJSON = undefined"
    nl
    wl $ hInstanceHeader "AFromJSON" sname (u_typeParams u)
    indent $ do
        wl "afromJSON = undefined"

generateDecl d@(Decl{d_type=(Decl_Typedef t)}) = do
    let (wl,nl,wt,indent) = helpers
        sname = hTypeName (d_name d)

    ts <- hTypeExpr (t_typeExpr t)
    wt "type $1$2 = $3" [sname,hTParams (t_typeParams t),ts]

generateModule :: Module ResolvedType -> HGen T.Text
generateModule m = do
  addImport "import Prelude(Show,Eq,Ord,Int,Double,undefined)"
  importModule (HaskellModule "ADL.Core")
  importQualifiedModuleAs (HaskellModule "Data.Aeson") "JSON"
  mapM_ genDecl (Map.elems $ m_decls m)
  ms <- get
  hm <- haskellModule (ms_name ms)
  let header = [template "module $1 where" [formatText hm]]
      imports = case Set.toList (ms_imports ms) of
        [] -> []
        lines -> "" : lines
      body = reverse (ms_lines ms)
  return (T.intercalate "\n" (header ++ imports ++ body))
  where
    genDecl d = writeLine "" >> generateDecl d

-- | Generate he haskell code for a module into a file. The mappings
-- from adl module names to haskell modules, and from haskell module
-- name to the written file.
writeModuleFile :: (ModuleName -> HaskellModule) ->
                   (HaskellModule -> FilePath) ->
                   Module ResolvedType ->
                   EIO a ()
writeModuleFile hmf fpf m = do
  let s0 = MState (m_name m) hmf "" Set.empty []
      t = evalState (generateModule m) s0
      fpath = fpf (hmf (m_name m))
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory fpath)
    T.writeFile fpath t
    putStrLn ("writing " ++ fpath ++ "...")
    

moduleMapper :: String -> ModuleName -> HaskellModule
moduleMapper sprefix mn = HaskellModule (T.intercalate "." (prefix ++path) )
  where
    prefix = T.splitOn "." (T.pack sprefix)
    path = map upper1 (unModuleName mn)

fileMapper :: FilePath -> HaskellModule -> FilePath
fileMapper root (HaskellModule t) = addExtension (joinPath (root:ps)) "hs"
  where
    ps = map T.unpack (T.splitOn "." t)
      

