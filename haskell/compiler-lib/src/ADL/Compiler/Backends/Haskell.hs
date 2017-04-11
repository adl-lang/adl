{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Haskell(
  HaskellFlags(..),
  HaskellModule(..),
  CustomType(..),
  generate, 
  )where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Ord (comparing)

import System.FilePath(takeDirectory,joinPath,addExtension)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JS
import qualified Data.Scientific as S
import Data.Attoparsec.Number

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS

import ADL.Utils.Format
import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing hiding (litNumber)
import ADL.Compiler.Primitive
import ADL.Compiler.Utils

newtype HaskellModule = HaskellModule T.Text
  deriving Show

instance Format HaskellModule where
  formatText (HaskellModule hm) = hm

data CustomType = CustomType {
   -- The name of the custom haskell type to use in lieu of original
   -- adl type
   ct_hTypeName :: Ident,

   -- Any imports required to support the custom haskell type.
   ct_hImports :: [HaskellModule],

   -- Lines of helper code. This must implement the AdlValue typeclass
   -- for the custom haskell type
   ct_insertCode :: [T.Text],

   -- Whether to generate the original ADL Type. If required the name
   -- to be used is supplied.
   ct_generateOrigADLType :: Maybe Ident
} deriving Show

-- A variant of the AST that carries custom type
-- information.

type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CModule = Module (Maybe CustomType) CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType

data MState = MState {
   ms_name :: ModuleName,
   ms_moduleMapper :: ModuleName -> HaskellModule,
   ms_indent :: T.Text,
   ms_exports :: Set.Set T.Text,
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

addExport :: T.Text -> HGen ()
addExport t = updateMState
  (\ms -> ms{ ms_exports=Set.insert t (ms_exports ms)} )

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

intType, wordType :: T.Text -> HGen T.Text
intType s = importQualifiedModule (HaskellModule "Data.Int") >> return s
wordType s = importQualifiedModule (HaskellModule "Data.Word") >> return s

hPrimitiveType :: PrimitiveType -> HGen T.Text
hPrimitiveType P_Void = return "()"
hPrimitiveType P_Bool = return "Prelude.Bool"
hPrimitiveType P_Int8 = intType "Data.Int.Int8"
hPrimitiveType P_Int16 = intType "Data.Int.Int16"
hPrimitiveType P_Int32 = intType "Data.Int.Int32"
hPrimitiveType P_Int64 = intType "Data.Int.Int64"
hPrimitiveType P_Word8 = wordType "Data.Word.Word8"
hPrimitiveType P_Word16 = wordType "Data.Word.Word16"
hPrimitiveType P_Word32 = wordType "Data.Word.Word32"
hPrimitiveType P_Word64 = wordType "Data.Word.Word64"
hPrimitiveType P_Float = return "Prelude.Float"
hPrimitiveType P_Double = return "Prelude.Double"
hPrimitiveType P_ByteVector = importByteString >> return "B.ByteString"
hPrimitiveType P_Vector = return "[]" -- never called
hPrimitiveType P_StringMap = return "???" -- never called
hPrimitiveType P_String = importText >> return "T.Text"
hPrimitiveType P_Sink = do
  importModule (HaskellModule "ADL.Core.Sink")
  return "Sink"

hPrimitiveLiteral :: PrimitiveType -> JS.Value -> T.Text
hPrimitiveLiteral P_Void JS.Null = "()"
hPrimitiveLiteral P_Bool (JS.Bool True) = "Prelude.True"
hPrimitiveLiteral P_Bool (JS.Bool False) = "Prelude.False"
hPrimitiveLiteral P_Int8 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Int16 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Int32 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Int64 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Word8 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Word16 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Word32 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Word64 (JS.Number n) = litNumber n
hPrimitiveLiteral P_Float (JS.Number n) = litNumber n
hPrimitiveLiteral P_Double (JS.Number n) = litNumber n
hPrimitiveLiteral P_ByteVector (JS.String s) = T.pack (show (decode s))
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
hPrimitiveLiteral P_Vector _ = "defaultv" -- never called
hPrimitiveLiteral P_String (JS.String s) = T.pack (show s)
hPrimitiveLiteral P_Sink _ = "defaultv" -- never called

litNumber :: S.Scientific -> T.Text
litNumber n = T.pack (if n < 0 then "(" ++ s ++ ")" else s)
  where
    s = case S.floatingOrInteger n of
      (Left r) -> show n
      (Right i) -> show (i::Integer)

type TypeBindingMap = Map.Map Ident (TypeExpr CResolvedType)

hTypeExpr :: TypeExpr CResolvedType -> HGen T.Text
hTypeExpr = hTypeExprB Map.empty

hTypeExprB :: TypeBindingMap -> TypeExpr CResolvedType -> HGen T.Text
hTypeExprB m (TypeExpr rt []) = hTypeExprB1 m rt
hTypeExprB m (TypeExpr (RT_Primitive P_Vector) [te]) = do
  argt <- hTypeExprB m te
  return (template "[$1]" [argt])
hTypeExprB m (TypeExpr (RT_Primitive P_StringMap) [te]) = do
  argt <- hTypeExprB m te
  importMap
  importText
  return (template "StringMap ($1)" [argt])
  
hTypeExprB m (TypeExpr c args) = do
  ct <- hTypeExprB1 m c
  argst <- mapM (hTypeExprB m) args
  return (T.concat $ ["(", ct, " "] ++ L.intersperse " " argst ++ [")"])

hTypeExprB1 :: TypeBindingMap -> CResolvedType -> HGen T.Text
hTypeExprB1 m (RT_Named (sn,d)) = do
  ms <- get
  let isLocalName = case sn_moduleName sn of
        ModuleName [] -> True
        _ -> False
      fullyScopedName = if isLocalName then sn{sn_moduleName=ms_name ms} else sn

  case isLocalName of
    True ->
      -- ADL type defined in this module
      return (hTypeName (sn_name sn))
    False -> do
      -- ADL type defined in an imported module
      hm <- importADLModule (sn_moduleName sn)
      return (T.intercalate "." [formatText hm,hTypeName (sn_name sn)])

hTypeExprB1 m (RT_Param i) = case Map.lookup i m of
    (Just te) -> hTypeExprB m te
    Nothing -> return (hTypeParamName i)
hTypeExprB1 m (RT_Primitive pt) = hPrimitiveType pt

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
importMap = importQualifiedModuleAs (HaskellModule "Data.Map") "M"
importByteString = importQualifiedModuleAs (HaskellModule "Data.ByteString")  "B"

declareAType :: ScopedName -> [Ident] -> HGen ()
declareAType gname [] = wt "atype _ = \"$1\"" [formatText gname]
declareAType gname tvars = do
  importText
  wl "atype _ = T.concat"
  indent $ do
    wt "[ \"$1\"" [formatText gname]
    forM_ (zip (", \"<\",": repeat ", \",\",") tvars) $ \(p,tv) -> do
      wt "$1 atype (Data.Proxy.Proxy :: Data.Proxy.Proxy $2)" [p, hTypeParamName tv]
    wl ", \">\" ]"

derivingStdClasses = wl "deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)"

generateDecl :: Ident -> CDecl -> HGen ()

generateDecl lname d@(Decl{d_type=(Decl_Struct s)}) = do
    addExport (template "$1(..)" [lname])
    enableScopedTypeVariables (s_typeParams s)
    mn <- fmap ms_name get
    if (null (s_fields s))
      then generateNullStructDataType lname mn d s
      else generateStructDataType lname mn d s
    nl
    if (null (s_fields s))
      then generateNullStructADLInstance lname mn d s
      else generateStructADLInstance lname mn d s

generateDecl lname d@(Decl{d_type=(Decl_Union u)}) = do
    addExport (template "$1(..)" [lname])
    enableScopedTypeVariables (u_typeParams u)
    mn <- fmap ms_name get
    generateUnionDataType lname mn d u
    nl
    generateUnionADLInstance lname mn d u

generateDecl lname d@(Decl{d_type=(Decl_Typedef t)}) = do
    addExport lname
    ts <- hTypeExpr (t_typeExpr t)
    wt "type $1$2 = $3" [lname,hTParams (t_typeParams t),ts]

generateDecl lname d@(Decl{d_type=(Decl_Newtype n)}) = do
    addExport (template "$1(..)" [lname])
    mn <- fmap ms_name get
    ts <- hTypeExpr (n_typeExpr n)
    wt "newtype $1$2 = $1 { un$1 :: $3 }" [lname,hTParams (n_typeParams n),ts]
    indent $ derivingStdClasses
    nl
    generateNewtypeADLInstance lname mn d n

commas :: [T.Text]
commas = repeat ","

listPrefixes :: [T.Text]
listPrefixes = "[":repeat ","

appPrefixes :: [T.Text]
appPrefixes = "<$>":repeat "<*>"

altPrefixes :: [T.Text]
altPrefixes = "=  ":repeat "<|>"

generateStructDataType :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateStructDataType lname mn d s = do
    wt "data $1$2 = $1" [lname,hTParams (s_typeParams s)]
    indent $ do
        forM_ (zip ("{":commas) (s_fields s)) $ \(fp,f) -> do
          t <- hTypeExpr (f_type f)
          wt "$1 $2 :: $3" [fp,
                            hFieldName (d_name d) (f_name f),
                           t ]
        wl "}"
        derivingStdClasses

generateNullStructDataType :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateNullStructDataType lname mn d s = do
    wt "data $1$2 = $1" [lname,hTParams (s_typeParams s)]
    indent derivingStdClasses
        

generateStructADLInstance :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateStructADLInstance lname mn d s = do
    wl $ hInstanceHeader "AdlValue" lname (s_typeParams s)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (s_typeParams s)
        nl
        wt "defaultv = $1" [lname]
        indent $ do
          forM_ (s_fields s) $ \f -> do
            defv <- generateDefaultValue (f_type f) (f_default f) 
            wl $ defv
        nl
        wl "jsonGen = genObject"
        indent $ do
          forM_ (zip listPrefixes (s_fields s)) $ \(prefix,f) -> do
            wt "$1 genField \"$2\" $3" [prefix, f_serializedName f, hFieldName (d_name d) (f_name f)]
          wl "]"
        nl
        wt "jsonParser = $1" [lname]
        indent $ do
          forM_ (zip appPrefixes (s_fields s)) $ \(prefix,f) -> do
            case f_default f of
             Nothing -> wt "$1 parseField \"$2\"" [prefix, f_serializedName f]
             Just def -> do
               defv <- generateDefaultValue (f_type f) (f_default f) 
               wt "$1 parseFieldDef \"$2\" $3" [prefix, f_serializedName f, defv]

generateNullStructADLInstance :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateNullStructADLInstance lname mn d s = do
    wl $ hInstanceHeader "AdlValue" lname (s_typeParams s)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (s_typeParams s)
        nl
        wt "defaultv = $1" [lname]
        wl "jsonGen = genObject []"
        wt "jsonParser = Prelude.pure $1" [lname]

generateUnionDataType :: Ident -> ModuleName -> CDecl -> Union CResolvedType -> HGen ()
generateUnionDataType lname mn d u = do
    let prefixes = ["="] ++ repeat "|"

    wt "data $1$2" [lname,hTParams (u_typeParams u)]
    indent $ do
      forM_ (zip prefixes (u_fields u)) $ \(fp,f) -> do
        if isVoidType (f_type f)
          then wt "$1 $2" [fp,hDiscName (d_name d) (f_name f)]
          else do
            t <- hTypeExpr (f_type f)
            wt "$1 $2 $3" [fp,hDiscName (d_name d) (f_name f),t]
      derivingStdClasses

generateUnionADLInstance :: Ident -> ModuleName -> CDecl -> Union CResolvedType -> HGen ()
generateUnionADLInstance lname mn d u = do

    wl $ hInstanceHeader "AdlValue" lname (u_typeParams u)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (u_typeParams u)
        nl
        let f0 = head (u_fields u)
            dn0 = hDiscName (d_name d) (f_name f0)
        if isVoidType (f_type f0)
          then wt "defaultv = $1" [dn0]
          else wt "defaultv = $1 defaultv" [dn0]
        nl
        wl "jsonGen = genUnion (\\jv -> case jv of"
        indent $ do
            forM_ (u_fields u) $ \f -> do
              let dn = hDiscName (d_name d) (f_name f)
              if isVoidType (f_type f)
                then wt "$1 -> genUnionVoid \"$2\"" [dn, f_serializedName f]
                else wt "$1 v -> genUnionValue \"$2\" v" [dn, f_serializedName f]
            wl ")"
        nl
        wl "jsonParser"
        indent $ do
            forM_ (zip altPrefixes (u_fields u)) $ \(prefix,f) -> do
              let dn = hDiscName (d_name d) (f_name f)
              if isVoidType (f_type f)
                then wt "$1 parseUnionVoid \"$2\" $3" [prefix, f_serializedName f, dn]
                else wt "$1 parseUnionValue \"$2\" $3" [prefix, f_serializedName f, dn]

generateNewtypeADLInstance :: Ident -> ModuleName -> CDecl -> Newtype CResolvedType -> HGen ()
generateNewtypeADLInstance lname mn d n = do

    wl $ hInstanceHeader "AdlValue" lname (n_typeParams n)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (n_typeParams n)
        nl
        defv <- generateDefaultValue (n_typeExpr n) (n_default n) 
        wt "defaultv = $1 $2" [lname, defv]
        nl
        wt "jsonGen = JsonGen (\\($1 v) -> adlToJson v)" [lname]
        nl
        wt "jsonParser = $1 <$> jsonParser" [lname]

generateDefaultValue :: TypeExpr CResolvedType -> (Maybe JS.Value) -> HGen T.Text
generateDefaultValue _ Nothing = return "defaultv"
generateDefaultValue te (Just v) = generateLiteral te v

generateLiteral :: TypeExpr CResolvedType -> JS.Value -> HGen T.Text
generateLiteral te v =  generateLV Map.empty te v
  where
    -- We only need to match the appropriate JSON cases here, as the JSON value
    -- has already been validated by the compiler
    generateLV :: TypeBindingMap -> TypeExpr CResolvedType -> JS.Value -> HGen T.Text
    generateLV m (TypeExpr (RT_Primitive pt) []) v = return (hPrimitiveLiteral pt v)
    generateLV m (TypeExpr (RT_Primitive P_Vector) [te]) v = generateVec m te v
    generateLV m (TypeExpr (RT_Primitive P_StringMap) [te]) v = generateStringMap m te v
    generateLV m te0@(TypeExpr (RT_Named (sn,decl)) tes) v = case d_type decl of
      (Decl_Struct s) -> generateStruct m te0 decl s tes v
      (Decl_Union u) -> generateUnion m decl u tes v 
      (Decl_Typedef t) -> generateTypedef m decl t tes v
      (Decl_Newtype n) -> generateNewtype m decl n tes v
    generateLV m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
         (Just te) -> generateLV m te v

    generateVec m te (JS.Array v) = do
      vals <- mapM (generateLV m te) (V.toList v) 
      return (template "[ $1 ]" [T.intercalate ", " vals])

    generateStringMap m te (JS.Object hm) = do
      pairs <- mapM genPair (HM.toList hm) 
      return (template "(stringMapFromList [$1])" [T.intercalate ", " pairs])
      where
        genPair (k,jv) = do
          v <- generateLV m te jv
          return (template "(\"$1\", $2)" [k,v])

    generateStruct m te0 d s tes (JS.Object hm) = do
      fields <- forM (L.sortBy (comparing fst) $ HM.toList hm) $ \(fname,v) -> do
        lit <- generateLV m2 (getTE s fname) v
        return (template "$1 = $2" [hFieldName (d_name d) fname, lit])
      let fields1 = T.intercalate ", " fields
      case tes of
        [] -> do
          return (template "defaultv { $1 }" [fields1])
        _  -> do
          -- If the type has parameters, then we may need to specify the type
          -- of defaultv... so do it just in case
          hte0 <- hTypeExprB m te0
          return (template "(defaultv :: $1) { $2 }" [hte0,fields1])
      where
        getTE s fname = case L.find (\f -> f_name f == fname) (s_fields s) of
          Just f -> f_type f
        m2 = m `Map.union` Map.fromList (zip (s_typeParams s) tes)

    generateUnion m d u tes (JS.Object hm) = do
      lit <- generateLV m2 te v
      return (template "($1 $2)" [hDiscName (d_name d) fname,lit])
      where
        (fname,v) = case HM.toList hm of
          [v] -> v
        (name,te) = case L.find (\f -> f_name f == fname) (u_fields u) of
          Just f -> (f_name f,f_type f)
        m2 = m `Map.union` Map.fromList (zip (u_typeParams u) tes)

    generateTypedef m d t tes v = generateLV m2 (t_typeExpr t) v
      where
        m2 = m `Map.union` Map.fromList (zip (t_typeParams t) tes)

    generateNewtype m d n tes v = do
      lit <- generateLV m2 (n_typeExpr n) v
      return (template "($1 $2)" [hTypeName (d_name d),lit])
      where
        m2 = m `Map.union` Map.fromList (zip (n_typeParams n) tes)

generateCustomType :: Ident -> CDecl -> CustomType -> HGen ()
generateCustomType n d ct = do
  -- imports and exports
  addExport (hTypeName n)
  mapM_ importModule (ct_hImports ct)

  -- Insert the user supplied code
  when (not (null (ct_insertCode ct))) $ do
    nl
    mapM_ wl (ct_insertCode ct)

  -- If required, generate the original ADL type under a different
  -- name.
  case ct_generateOrigADLType ct of
    Nothing -> return ()
    Just i -> do
      nl
      generateDecl i d


generateModule :: CModule -> HGen T.Text
generateModule m = do
  addLanguageFeature "OverloadedStrings"
  addImport "import qualified Prelude"
  addImport "import qualified Data.Proxy"
  addImport "import Control.Applicative( (<$>), (<*>), (<|>) )"
  importModule (HaskellModule "ADL.Core")
  importQualifiedModuleAs (HaskellModule "Data.Aeson") "JS"
  importQualifiedModuleAs (HaskellModule "Data.HashMap.Strict") "HM"

  ms <- get
  let mname = ms_name ms
      genDecl (n,d) = do
          nl
          case d_customType d of
            Nothing -> generateDecl (hTypeName (d_name d)) d
            (Just ct) -> generateCustomType n d ct

  mapM_ genDecl (Map.toList (m_decls m))
  ms <- get
  hm <- haskellModule mname

  let lang = case Set.toList (ms_languageFeatures ms) of
        [] -> []
        fs ->  [template "{-# LANGUAGE $1 #-}" [T.intercalate ", " fs]]

      header = [ template "module $1(" [formatText hm] ]
               ++ exports ++
               [ ") where" ]

      exports = [template "    $1," [e] | e <- Set.toList (ms_exports ms)]

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
                   (ScopedName -> RDecl -> Maybe CustomType) ->
                   (FilePath -> LBS.ByteString -> IO ()) ->
                   RModule ->
                   EIOT ()
writeModuleFile hmf fpf getCustomType fileWriter m0 = do
  let moduleName = m_name m
      s0 = MState moduleName hmf "" Set.empty Set.empty Set.empty []
      m = associateCustomTypes getCustomType moduleName m0
      t = evalState (generateModule m) s0
      fpath = fpf (hmf (m_name m))
  checkCustomSerializations m
  liftIO $ fileWriter fpath (LBS.fromStrict (T.encodeUtf8 t))

moduleMapper :: String -> ModuleName -> HaskellModule
moduleMapper sprefix mn = HaskellModule (T.intercalate "." (prefix ++path) )
  where
    prefix = T.splitOn "." (T.pack sprefix)
    path = map upper1 (unModuleName mn)

fileMapper :: HaskellModule -> FilePath
fileMapper (HaskellModule t) = addExtension (joinPath ps) "hs"
  where
    ps = map T.unpack (T.splitOn "." t)
      
----------------------------------------------------------------------

data HaskellFlags = HaskellFlags {
  hf_modulePrefix :: String
}

generate :: AdlFlags -> HaskellFlags -> FileWriter -> (ScopedName -> RDecl -> Maybe CustomType) -> [FilePath] -> EIOT ()
generate af hf fileWriter getCustomType modulePaths = catchAllExceptions $ forM_ modulePaths $ \modulePath -> do
  rm <- loadAndCheckModule af modulePath
  writeModuleFile (moduleMapper (hf_modulePrefix hf))
                  fileMapper
                  getCustomType
                  fileWriter
                  rm
