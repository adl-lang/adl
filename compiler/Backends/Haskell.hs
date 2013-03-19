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

newtype HaskellModule = HaskellModule T.Text

instance Format HaskellModule where
  formatText (HaskellModule hm) = hm

data CustomType = CustomType {
   ct_hTypeName :: Ident,
   ct_hImports :: [HaskellModule]
}

data MState = MState {
   ms_name :: ModuleName,
   ms_moduleMapper :: ModuleName -> HaskellModule,
   ms_customTypes :: Map.Map ScopedName CustomType,
   ms_indent :: T.Text,
   ms_imports :: Set.Set T.Text,
   ms_languageFeatures :: Set.Set T.Text,
   ms_lines :: [T.Text]
}

type HGen = State MState

updateMState :: (MState->MState) -> HGen ()
updateMState = modify

wl :: T.Text -> HGen ()
wl t = updateMState addLine
  where
    addLine ms = ms{ms_lines=(ms_indent ms) `T.append` t:ms_lines ms}

nl :: HGen ()
nl = wl ""

wt :: T.Text -> [T.Text] -> HGen ()
wt pattern args = wl (template pattern args)

indent :: HGen a -> HGen a
indent g = do
    updateMState
      (\ms -> ms{ ms_indent=T.append is (ms_indent ms)})
    a <- g
    updateMState
      (\ms -> ms{ ms_indent=T.drop (T.length is) (ms_indent ms) } )
    return a
  where
    is = "    "


addLanguageFeature :: T.Text -> HGen ()
addLanguageFeature t = updateMState
  (\ms -> ms{ ms_languageFeatures=Set.insert t (ms_languageFeatures ms)} )

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
hPrimitiveType P_Bool = return "Prelude.Bool"
hPrimitiveType P_Int = return "Prelude.Int"
hPrimitiveType P_Double = return "Prelude.Double"
hPrimitiveType P_ByteVector = do
  importByteString
  return "B.ByteString"
hPrimitiveType P_Vector = return "[]" -- never called
hPrimitiveType P_String = do
  importText
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
  ms <- get
  let isLocalName = case sn_moduleName sn of
        ModuleName [] -> True
        _ -> False
      fullyScopedName = if isLocalName then sn{sn_moduleName=ms_name ms} else sn

  case Map.lookup fullyScopedName (ms_customTypes ms) of
    (Just ct) -> do
      -- custom type in an imported module
      mapM_ importModule (ct_hImports ct)
      return (ct_hTypeName ct)
    Nothing -> case isLocalName of
      True ->
        -- ADL type defined in this module
        return (hTypeName (sn_name sn))
      False -> do
        -- ADL type defined in an imported module
        hm <- importADLModule (sn_moduleName sn)
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
  

enableScopedTypeVariables :: [Ident] -> HGen ()
enableScopedTypeVariables [] = return ()
enableScopedTypeVariables _ = addLanguageFeature "ScopedTypeVariables"

importText = importQualifiedModuleAs (HaskellModule "Data.Text") "T"
importByteString = importQualifiedModuleAs (HaskellModule "Data.ByteString")  "B"

declareAType :: ScopedName -> [Ident] -> HGen ()
declareAType gname [] = wt "atype _ = \"$1\"" [formatText gname]
declareAType gname tvars = do
  importText
  wl "atype _ = T.concat"
  indent $ do
    wt "[ \"$1\"" [formatText gname]
    forM_ (zip (", \"<\",": repeat ", \",\",") tvars) $ \(p,tv) -> do
      wt "$1 atype (Prelude.undefined ::$2)" [p, hTypeParamName tv]
    wl ", \">\" ]"

derivingStdClasses = wl "deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)"

generateDecl :: Decl ResolvedType -> HGen ()
generateDecl d@(Decl{d_type=(Decl_Struct s)}) = do
    enableScopedTypeVariables (s_typeParams s)
    mn <- fmap ms_name get    

    let lname = hTypeName (d_name d)
        commas = repeat ","

    wt "data $1$2 = $1" [lname,hTParams (s_typeParams s)]
    indent $ do
        forM_ (zip ("{":commas) (s_fields s)) $ \(fp,f) -> do
          t <- hTypeExpr (f_type f)
          wt "$1 $2 :: $3" [fp,
                            hFieldName (d_name d) (f_name f),
                            t ]
        wl "}"
        derivingStdClasses
    nl
    wl $ hInstanceHeader "ADLValue" lname (s_typeParams s)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (s_typeParams s)
        nl
        wt "defaultv = $1 $2" [lname, T.intercalate " " ["defaultv" | f <- s_fields s]]
        nl
        wl "aToJSON f v = toJSONObject f (atype v) ("
        indent $ do
          forM_ (zip ("[":commas) (s_fields s)) $ \(fp,f) -> do
            wt "$1 (\"$2\",aToJSON f ($3 v))"
               [fp,(f_name f),hFieldName (d_name d) (f_name f)]
          wl "] )"
        nl
        wt "aFromJSON f (JSON.Object hm) = $1" [lname]
        indent $ do
          forM_ (zip ("<$>":repeat "<*>") (s_fields s)) $ \(p,f) -> do
            wt "$1 fieldFromJSON f \"$2\" defaultv hm" [p, (f_name f)]
        wl "aFromJSON _ _ = Prelude.Nothing"

generateDecl d@(Decl{d_type=(Decl_Union u)}) = do
    enableScopedTypeVariables (u_typeParams u)
    mn <- fmap ms_name get    

    let lname = hTypeName (d_name d)
        gname = ScopedName mn (d_name d)
        prefixes = ["="] ++ repeat "|"

    wt "data $1$2" [lname,hTParams (u_typeParams u)]
    indent $ do
      forM_ (zip prefixes (u_fields u)) $ \(fp,f) -> do
        t <- hTypeExpr (f_type f)
        wt "$1 $2 $3" [fp,hDiscName (d_name d) (f_name f),t]
      derivingStdClasses
    nl
    wl $ hInstanceHeader "ADLValue" lname (u_typeParams u)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (u_typeParams u)
        nl
        wt "defaultv = $1 defaultv"
           [hDiscName (d_name d) (f_name (head (u_fields u)))]
        nl
        wl "aToJSON f v = toJSONObject f (atype v) [case v of"
        indent $ do
          forM_ (u_fields u) $ \f -> do
            wt "($1 v) -> (\"$2\",aToJSON f v)"
               [hDiscName (d_name d) (f_name f),(f_name f)]
          wl "]"
        nl
        wl "aFromJSON f o = "
        indent $ do
          wl "let umap = HM.fromList"
          indent $ indent $ do
              forM_ (zip ("[":repeat ",") (u_fields u)) $ \(fp,f) -> do
                wt "$1 (\"$2\", \\f v -> $3 <$> aFromJSON f v)" [fp,f_name f,hDiscName (d_name d) (f_name f)]
              wl "]"
          wl "in unionFromJSON f umap o"

generateDecl d@(Decl{d_type=(Decl_Typedef t)}) = do
    let lname = hTypeName (d_name d)

    ts <- hTypeExpr (t_typeExpr t)
    wt "type $1$2 = $3" [lname,hTParams (t_typeParams t),ts]

generateModule :: Module ResolvedType -> HGen T.Text
generateModule m = do
  addLanguageFeature "OverloadedStrings"
  addImport "import qualified Prelude"
  addImport "import Control.Applicative( (<$>), (<*>) )"
  importModule (HaskellModule "ADL.Core")
  importQualifiedModuleAs (HaskellModule "Data.Aeson") "JSON"
  importQualifiedModuleAs (HaskellModule "Data.HashMap.Strict") "HM"

  ms <- get
  let mname = ms_name ms
      hasCustomDefinition n = Map.member (ScopedName mname n) (ms_customTypes ms)
      genDecl (n,d) = do
          wl ""
          if hasCustomDefinition n
            then wt "-- $1 excluded due to custom definition" [n]
            else generateDecl d

  mapM_ genDecl (Map.toList (m_decls m))
  ms <- get
  hm <- haskellModule mname

  let lang = case Set.toList (ms_languageFeatures ms) of
        [] -> []
        fs ->  [template "{-# LANGUAGE $1 #-}" [T.intercalate ", " fs]]
      header = [ template "module $1 where" [formatText hm]
               ]
      imports = case Set.toList (ms_imports ms) of
        [] -> []
        lines -> "" : lines
      body = reverse (ms_lines ms)

  return (T.intercalate "\n" (lang ++ header ++ imports ++ body))

-- | Generate he haskell code for a module into a file. The mappings
-- from adl module names to haskell modules, and from haskell module
-- name to the written file.
writeModuleFile :: (ModuleName -> HaskellModule) ->
                   (HaskellModule -> FilePath) ->
                   Module ResolvedType ->
                   EIO a ()
writeModuleFile hmf fpf m = do
  let s0 = MState (m_name m) hmf customTypes "" Set.empty Set.empty []
      t = evalState (generateModule m) s0
      fpath = fpf (hmf (m_name m))
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory fpath)
    T.writeFile fpath t
    putStrLn ("writing " ++ fpath ++ "...")

customTypes = Map.fromList
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
    
    

moduleMapper :: String -> ModuleName -> HaskellModule
moduleMapper sprefix mn = HaskellModule (T.intercalate "." (prefix ++path) )
  where
    prefix = T.splitOn "." (T.pack sprefix)
    path = map upper1 (unModuleName mn)

fileMapper :: FilePath -> HaskellModule -> FilePath
fileMapper root (HaskellModule t) = addExtension (joinPath (root:ps)) "hs"
  where
    ps = map T.unpack (T.splitOn "." t)
      

