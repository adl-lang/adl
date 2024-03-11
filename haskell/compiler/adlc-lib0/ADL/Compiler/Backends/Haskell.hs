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

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Foldable(for_)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Ord (comparing)
import System.FilePath(takeDirectory,joinPath,addExtension, splitDirectories, splitExtension, (</>))

import qualified Data.Vector as V
import qualified Data.Aeson as JS
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as AKey
import qualified Data.Scientific as S

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
import ADL.Utils.FileDiff(dirContents)
import ADL.Utils.IndentedCode(doubleQuote)

newtype HaskellModule = HaskellModule {unHaskellModule :: T.Text}
  deriving Show

instance Format HaskellModule where
  formatText (HaskellModule hm) = hm

data CustomType = CustomType {
   -- The name of the custom haskell type to use in lieu of original
   -- adl type
   ct_hTypeName :: Ident,

   -- Any imports required to support the custom haskell type.
   ct_hImports :: [HaskellModule],

   -- Any extra exports required from the module where the custom type
   -- was declared
   ct_hExtraExports:: [Ident],

   -- Lines of helper code. This must implement the AdlValue typeclass
   -- for the custom haskell type
   ct_insertCode :: [T.Text],

   -- The function to call to construct struct values
   ct_structConstructor :: T.Text,

   -- The functions to call to construct union values
   ct_unionConstructors :: Map.Map Ident T.Text,

   -- Whether to generate the original ADL Type. If required the name
   -- to be used is supplied.
   ct_generateOrigADLType :: Maybe Ident
} deriving Show

-- A variant of the AST that carries custom type
-- information.

type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CModule = Module (Maybe CustomType) CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType
type CField = Field CResolvedType

data MState = MState {
   ms_name :: ModuleName,
   ms_moduleMapper :: ModuleName -> HaskellModule,
   ms_runtimePackage :: T.Text,
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

hFieldName :: CDecl -> CField -> Ident
hFieldName decl field = T.concat [lower1 (getFieldPrefix decl field), f_name field]

hDiscName :: CDecl -> CField -> Ident
hDiscName decl field = T.concat [upper1 (getFieldPrefix decl field), f_name field]

getFieldPrefix :: CDecl -> CField -> Ident
getFieldPrefix decl field =  fromMaybe (d_name decl <> "_")
  (   getStringAnnotation fieldPrefixAnn (f_annotations field)
  <|> getStringAnnotation fieldPrefixAnn (d_annotations decl)
  )
  where
    fieldPrefixAnn = ScopedName (ModuleName ["adlc", "config", "haskell"]) "HaskellFieldPrefix"
    getStringAnnotation scopedName anns = case Map.lookup scopedName anns of
      Just (_,JS.String s) -> Just s
      _ -> Nothing

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
hPrimitiveType P_Json = return "JS.Value"
hPrimitiveType P_ByteVector = importByteString >> return "B.ByteString"
hPrimitiveType P_Vector = return "[]" -- never called
hPrimitiveType P_StringMap = return "???" -- never called
hPrimitiveType P_Nullable = return "???" -- never called
hPrimitiveType P_TypeToken = return "???" -- never called
hPrimitiveType P_String = importText >> return "T.Text"

hPrimitiveLiteral :: PrimitiveType -> JS.Value -> HGen T.Text
hPrimitiveLiteral P_Void JS.Null = return "()"
hPrimitiveLiteral P_Bool (JS.Bool True) = return "Prelude.True"
hPrimitiveLiteral P_Bool (JS.Bool False) = return "Prelude.False"
hPrimitiveLiteral P_Int8 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Int16 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Int32 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Int64 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Word8 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Word16 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Word32 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Word64 (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Float (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Double (JS.Number n) = return (litNumber n)
hPrimitiveLiteral P_Json js = do
  importQualifiedModule (HaskellModule "Data.Maybe")
  return (template "(Data.Maybe.fromJust (JS.decode $1))" [T.pack (show (JS.encode js))])
hPrimitiveLiteral P_ByteVector (JS.String s) = return (T.pack (show (decode s)))
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
hPrimitiveLiteral P_Vector _ = return "undefined" -- never called
hPrimitiveLiteral P_String (JS.String s) = return (doubleQuote s)
hPrimitiveLiteral p _ = error ("BUG: invalid literal type for primitive " ++ show p)

litNumber :: S.Scientific -> T.Text
litNumber n = T.pack (if n < 0 then "(" ++ s ++ ")" else s)
  where
    s = case S.floatingOrInteger n of
      (Left r) -> show n
      (Right i) -> show (i::Integer)

type TypeBindingMap = Map.Map Ident (TypeExpr CResolvedType)

withTypeBindings :: [Ident] -> [TypeExpr CResolvedType] -> TypeBindingMap -> TypeBindingMap
withTypeBindings ids tes m = Map.union m (Map.fromList (zip ids tes))

hTypeExpr :: TypeExpr CResolvedType -> HGen T.Text
hTypeExpr = hTypeExprB Map.empty

hTypeExprB :: TypeBindingMap -> TypeExpr CResolvedType -> HGen T.Text
hTypeExprB m (TypeExpr rt []) = hTypeExprB1 m rt
hTypeExprB m (TypeExpr (RT_Primitive P_Vector) [te]) = do
  argt <- hTypeExprB m te
  return (template "[$1]" [argt])
hTypeExprB m (TypeExpr (RT_Primitive P_Nullable) [te]) = do
  argt <- hTypeExprB m te
  nmod <- nullableModule
  importQualifiedModule nmod
  return (template "($1.Nullable $2)" [unHaskellModule nmod, argt])
hTypeExprB m (TypeExpr (RT_Primitive P_TypeToken) [te]) = do
  argt <- hTypeExprB m te
  tmod <- typeTokenModule
  importQualifiedModule tmod
  return (template "($1.TypeToken $2)" [unHaskellModule tmod, argt])
hTypeExprB m (TypeExpr (RT_Primitive P_StringMap) [te]) = do
  argt <- hTypeExprB m te
  importMap
  importText
  return (template "(StringMap $1)" [argt])
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


nullableModule :: HGen HaskellModule
nullableModule = do
  ms <- get
  return (HaskellModule (ms_runtimePackage ms <> ".Nullable"))

typeTokenModule :: HGen HaskellModule
typeTokenModule = do
  ms <- get
  return (HaskellModule (ms_runtimePackage ms <> ".TypeToken"))

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

type StdTypeClasses = Set.Set T.Text

defaultStdClasses :: StdTypeClasses
defaultStdClasses = Set.fromList ["Eq","Ord","Show"]

-- | Determine which standard typeclasses instances exist for
-- a given type expression
--
stdClassesFor :: TypeExpr CResolvedType -> StdTypeClasses
stdClassesFor = stdClassesFor1 Set.empty Map.empty

stdClassesFor1 :: Set.Set ScopedName -> TypeBindingMap -> TypeExpr CResolvedType -> StdTypeClasses
stdClassesFor1 sns tbmap (TypeExpr (RT_Primitive P_Vector) [te]) = stdClassesFor1 sns tbmap te
stdClassesFor1 sns tbmap (TypeExpr (RT_Primitive P_StringMap) [te]) = stdClassesFor1 sns tbmap te
stdClassesFor1 sns tbmap (TypeExpr (RT_Primitive P_Nullable) [te]) = stdClassesFor1 sns tbmap te
stdClassesFor1 sns tbmap (TypeExpr (RT_Primitive P_TypeToken) [te]) = stdClassesFor1 sns tbmap te
stdClassesFor1 sns tbmap (TypeExpr (RT_Primitive P_Json) []) = Set.fromList ["Eq","Show"]
stdClassesFor1 sns tbmap (TypeExpr (RT_Primitive _) _) = defaultStdClasses
stdClassesFor1 sns tbmap (TypeExpr (RT_Param tp) _) = case Map.lookup tp tbmap of
   Nothing -> defaultStdClasses
   (Just te) -> stdClassesFor1 sns (Map.delete tp tbmap) te
stdClassesFor1 sns tbmap (TypeExpr (RT_Named (sn,decl)) tes) = case Set.member sn sns of
  True -> defaultStdClasses
  False ->
    let sns' = Set.insert sn sns in
    case decl of
      Decl{d_type=Decl_Struct s} -> stdClassesForFields1 sns' (withTypeBindings (s_typeParams s) tes tbmap) (s_fields s)
      Decl{d_type=Decl_Union u} -> stdClassesForFields1 sns' (withTypeBindings (u_typeParams u) tes tbmap) (u_fields u)
      Decl{d_type=Decl_Typedef t} -> stdClassesFor1 sns' (withTypeBindings (t_typeParams t) tes tbmap) (t_typeExpr t)
      Decl{d_type=Decl_Newtype n} -> stdClassesFor1 sns' (withTypeBindings (n_typeParams n) tes tbmap) (n_typeExpr n)

stdClassesForFields1 :: Set.Set ScopedName -> TypeBindingMap -> [Field CResolvedType] -> StdTypeClasses
stdClassesForFields1 sns tbmap fields = foldr Set.intersection defaultStdClasses [stdClassesFor1 sns tbmap (f_type f) | f <- fields]

stdClassesForFields :: [Field CResolvedType] -> StdTypeClasses
stdClassesForFields = stdClassesForFields1 Set.empty Map.empty

derivingStdClasses ::  StdTypeClasses -> HGen ()
derivingStdClasses stdClasses = wt "deriving ($1)" [T.intercalate "," (map ("Prelude." <>) (Set.toList stdClasses))]


generateDecl :: Ident -> CDecl -> HGen ()

generateDecl lname d@(Decl{d_type=(Decl_Struct s)}) = do
    addExport (template "$1(..)" [lname])
    enableScopedTypeVariables (s_typeParams s)
    mn <- fmap ms_name get
    if (null (s_fields s))
      then generateNullStructDataType lname mn d s
      else do
        generateStructDataType lname mn d s
    nl
    mkfn <- generateMkStructFunction lname mn d s
    addExport mkfn
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
    let stdClasses = stdClassesFor (n_typeExpr n)
    indent $ derivingStdClasses stdClasses
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
                            hFieldName d f,
                           t ]
        wl "}"
        derivingStdClasses (stdClassesForFields (s_fields s))

generateMkStructFunction :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen T.Text
generateMkStructFunction lname mn d s = do
  args <- forM (s_fields s) $ \f -> do
    case f_default f of
     Nothing -> do
       paramtype <- hTypeExpr (f_type f)
       return (let param = unreserveWord (f_name f) in (Just (param,paramtype),param))
     (Just jv) -> do
       lit <- generateLiteral (f_type f) jv
       return (Nothing, lit)

  let fnname = "mk" <> lname
      params = [param | (Just (param,_),_) <- args]
      paramtypes = [ptype | (Just (_,ptype),_) <- args]
      ctorArgs = [arg | (_,arg) <- args]

  wt "$1 :: $2 $3$4" [fnname, T.intercalate " " [p <> " ->" | p <- paramtypes], lname, hTParams (s_typeParams s)]
  wt "$1 $2 = $3 $4" [fnname, T.intercalate " "  params, lname, T.intercalate " " ctorArgs]
  return fnname
  where

generateNullStructDataType :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateNullStructDataType lname mn d s = do
    wt "data $1$2 = $1" [lname,hTParams (s_typeParams s)]
    indent (derivingStdClasses defaultStdClasses)


generateStructADLInstance :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateStructADLInstance lname mn d s = do
    wl $ hInstanceHeader "AdlValue" lname (s_typeParams s)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (s_typeParams s)
        nl
        wl "jsonGen = genObject"
        indent $ do
          forM_ (zip listPrefixes (s_fields s)) $ \(prefix,f) -> do
            wt "$1 genField \"$2\" $3" [prefix, f_serializedName f, hFieldName d f]
          wl "]"
        nl
        wt "jsonParser = $1" [lname]
        indent $ do
          forM_ (zip appPrefixes (s_fields s)) $ \(prefix,f) -> do
            case f_default f of
             Nothing -> wt "$1 parseField \"$2\"" [prefix, f_serializedName f]
             Just def -> do
               defv <- generateLiteral (f_type f) def
               wt "$1 parseFieldDef \"$2\" $3" [prefix, f_serializedName f, defv]

generateNullStructADLInstance :: Ident -> ModuleName -> CDecl -> Struct CResolvedType -> HGen ()
generateNullStructADLInstance lname mn d s = do
    wl $ hInstanceHeader "AdlValue" lname (s_typeParams s)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (s_typeParams s)
        nl
        wl "jsonGen = genObject []"
        wt "jsonParser = Prelude.pure $1" [lname]

generateUnionDataType :: Ident -> ModuleName -> CDecl -> Union CResolvedType -> HGen ()
generateUnionDataType lname mn d u = do
    let prefixes = ["="] ++ repeat "|"

    wt "data $1$2" [lname,hTParams (u_typeParams u)]
    indent $ do
      forM_ (zip prefixes (u_fields u)) $ \(fp,f) -> do
        if isVoidType (f_type f)
          then wt "$1 $2" [fp,hDiscName d f]
          else do
            t <- hTypeExpr (f_type f)
            wt "$1 $2 $3" [fp,hDiscName d f,t]
      derivingStdClasses (stdClassesForFields (u_fields u))

generateUnionADLInstance :: Ident -> ModuleName -> CDecl -> Union CResolvedType -> HGen ()
generateUnionADLInstance lname mn d u = do

    wl $ hInstanceHeader "AdlValue" lname (u_typeParams u)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (u_typeParams u)
        nl
        wl "jsonGen = genUnion (\\jv -> case jv of"
        indent $ do
            forM_ (u_fields u) $ \f -> do
              let dn = hDiscName d f
              if isVoidType (f_type f)
                then wt "$1 -> genUnionVoid \"$2\"" [dn, f_serializedName f]
                else wt "$1 v -> genUnionValue \"$2\" v" [dn, f_serializedName f]
            wl ")"
        nl
        wl "jsonParser = parseUnion $ \\disc -> case disc of"
        indent $ do
            forM_ (u_fields u) $ \f -> do
              let dn = hDiscName d f
              if isVoidType (f_type f)
                then wt "\"$1\" -> parseUnionVoid $2" [f_serializedName f, dn]
                else wt "\"$1\" ->  parseUnionValue $2" [f_serializedName f, dn]
            wt "_ -> parseFail \"expected a discriminator for $1 ($2)\" "
                      [d_name d, T.intercalate "," (map f_serializedName (u_fields u))]

generateNewtypeADLInstance :: Ident -> ModuleName -> CDecl -> Newtype CResolvedType -> HGen ()
generateNewtypeADLInstance lname mn d n = do

    wl $ hInstanceHeader "AdlValue" lname (n_typeParams n)
    indent $ do
        declareAType (ScopedName mn (d_name d)) (n_typeParams n)
        nl
        wt "jsonGen = JsonGen (\\($1 v) -> adlToJson v)" [lname]
        nl
        wt "jsonParser = $1 <$> jsonParser" [lname]

generateLiteral :: TypeExpr CResolvedType -> JS.Value -> HGen T.Text
generateLiteral te v =  generateLV Map.empty te v
  where
    -- We only need to match the appropriate JSON cases here, as the JSON value
    -- has already been validated by the compiler
    generateLV :: TypeBindingMap -> TypeExpr CResolvedType -> JS.Value -> HGen T.Text
    generateLV m (TypeExpr (RT_Primitive P_Vector) [te]) v = generateVec m te v
    generateLV m (TypeExpr (RT_Primitive P_StringMap) [te]) v = generateStringMap m te v
    generateLV m (TypeExpr (RT_Primitive P_Nullable) [te]) v = generateNullable m te v
    generateLV m (TypeExpr (RT_Primitive P_TypeToken) [te]) v = generateType m te v
    generateLV m (TypeExpr (RT_Primitive pt) []) v = hPrimitiveLiteral pt v
    generateLV m (TypeExpr (RT_Primitive pt) _) v = error "BUG: primitive literal with unexpected type params"
    generateLV m te0@(TypeExpr (RT_Named (sn,decl)) tes) v = case d_type decl of
      (Decl_Struct s) -> generateStruct m sn decl s tes v
      (Decl_Union u) -> generateUnion m sn decl u tes v
      (Decl_Typedef t) -> generateTypedef m decl t tes v
      (Decl_Newtype n) -> generateNewtype m sn decl n tes v
    generateLV m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
         (Just te) -> generateLV m te v
         Nothing -> error "BUG: unresolved type variable in literal"
    generateVec m te (JS.Array v) = do
      vals <- mapM (generateLV m te) (V.toList v)
      return (template "[ $1 ]" [T.intercalate ", " vals])
    generateVec m te _ = error "BUG: vector literal requires a json array"

    generateStringMap m te (JS.Object hm) = do
      pairs <- mapM genPair (KM.toList hm)
      return (template "(stringMapFromList [$1])" [T.intercalate ", " pairs])
      where
        genPair (k,jv) = do
          v <- generateLV m te jv
          return (template "($1, $2)" [doubleQuote (AKey.toText k),v])
    generateStringMap m te _ = error "BUG: stringmap literal requires a json object"

    generateNullable m te JS.Null = do
      nmod <- nullableModule
      return (template "($1.null)" [unHaskellModule nmod])
    generateNullable m te js = do
      nmod <- nullableModule
      v <- generateLV m te js
      return (template "($1.fromValue ($2))" [unHaskellModule nmod, v])

    generateType m te _ = do
      tmod <- typeTokenModule
      importQualifiedModule tmod
      return (template "($1.TypeToken)" [unHaskellModule tmod])

    generateStruct m sn d s tes (JS.Object hm) = do
      fields <- forM (s_fields s) $ \f -> do
        let mjv = KM.lookup (serializedNameKey f) hm <|> f_default f
            jv = case mjv of
              Just jv -> jv
              Nothing -> error ("BUG: missing default value for field " <> T.unpack (f_name f))
        generateLV m2 (f_type f) jv
      ctor <- structCtor sn d
      return (template "($1 $2)" [ctor,T.intercalate " " fields])
      where
        m2 = withTypeBindings (s_typeParams s) tes m
    generateStruct m sn d s tes _ = error "BUG: struct literal is not an object"

    generateUnion m sn d u tes (JS.String fname) = do
      unionCtor sn d u fname
    generateUnion m sn d u tes (JS.Object hm) = do
      ctor <- unionCtor sn d u (AKey.toText fname)
      if isVoidType te
        then return ctor
        else do
          lit <- generateLV m2 te v
          return (template "($1 $2)" [ctor,lit])
      where
        (fname,v) = case KM.toList hm of
          [v] -> v
          _ -> error "BUG: union literal must have a single key"
        te = case L.find (\f -> serializedNameKey f == fname) (u_fields u) of
          Just f -> f_type f
          Nothing -> error "BUG: union literal key must be a field"
        m2 = withTypeBindings (u_typeParams u) tes m
    generateUnion m sn d u tes _ = error "BUG: union literal expects a json string or object"

    generateTypedef m d t tes v = generateLV m2 (t_typeExpr t) v
      where
        m2 = withTypeBindings (t_typeParams t) tes m

    generateNewtype m sn d n tes v = do
      lit <- generateLV m2 (n_typeExpr n) v
      ctor <- structCtor sn d
      return (template "($1 $2)" [ctor,lit])
      where
        m2 = withTypeBindings (n_typeParams n) tes m

    unionCtor :: ScopedName -> CDecl -> Union CResolvedType -> T.Text -> HGen Ident
    unionCtor sn decl union fname = case d_customType decl of
      Nothing -> do
        let field :: CField
            field = case L.find ((==fname) . f_serializedName) (u_fields union) of
              Just field -> field
              Nothing -> undefined
        scopedName (sn_moduleName sn) (hDiscName decl field)
      Just ct -> do
        importsForCustomType ct
        case Map.lookup fname (ct_unionConstructors ct) of
          Just fctor -> return fctor
          Nothing -> error ("Missing custom constructor for " ++  T.unpack fname)

    structCtor sn decl = case d_customType decl of
      Nothing -> scopedName (sn_moduleName sn) (hTypeName (d_name decl))
      (Just ct) -> do
        importsForCustomType ct
        let ctor = ct_structConstructor ct
        if T.null ctor then error "Missing custom constructor for struct" else return ctor

scopedName :: ModuleName -> Ident -> HGen T.Text
scopedName mn name = do
  genmn <- fmap ms_name get
  if unModuleName mn == [] || mn == genmn
     then return name
     else do
       hm <- importADLModule mn
       return (T.intercalate "." [formatText hm,name])

serializedNameKey:: Field a -> KM.Key
serializedNameKey f = AKey.fromText (f_serializedName f)

generateCustomType :: Ident -> CDecl -> CustomType -> HGen ()
generateCustomType n d ct = do
  -- imports and exports
  addExport (hTypeName n)
  mapM addExport (ct_hExtraExports ct)
  importsForCustomType ct

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

importsForCustomType :: CustomType -> HGen ()
importsForCustomType ct = mapM_ importModule (ct_hImports ct)

generateModule :: CModule -> HGen T.Text
generateModule m = do
  ms <- get
  addLanguageFeature "OverloadedStrings"
  addImport "import qualified Prelude"
  addImport "import Prelude( ($) )"
  addImport "import qualified Data.Proxy"
  addImport "import Control.Applicative( (<$>), (<*>), (<|>) )"
  importModule (HaskellModule (ms_runtimePackage ms))
  importQualifiedModuleAs (HaskellModule "Data.Aeson") "JS"

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
                   T.Text ->
                   (HaskellModule -> FilePath) ->
                   (ScopedName -> RDecl -> Maybe CustomType) ->
                   (FilePath -> LBS.ByteString -> IO ()) ->
                   RModule ->
                   EIOT ()
writeModuleFile hmf runtimePackage fpf getCustomType fileWriter m0 = do
  let moduleName = m_name m
      s0 = MState moduleName hmf runtimePackage "" Set.empty Set.empty Set.empty []
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

unreserveWord :: T.Text -> T.Text
unreserveWord n | Set.member n reservedWords = T.append n "_"
                | otherwise = n

reservedWords :: Set.Set T.Text
reservedWords = Set.fromList
  [ "as"
  , "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "family"
  , "forall"
  , "foreign"
  , "hiding"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "mdo"
  , "module"
  , "newtype"
  , "of"
  , "proc"
  , "qualified"
  , "rec"
  , "then"
  , "type"
  , "where"
  ]

 ----------------------------------------------------------------------

data HaskellFlags = HaskellFlags {
  hf_modulePrefix :: String,
  hf_includeRuntime :: Maybe FilePath,
  hf_runtimePackage :: T.Text
}

generate :: AdlFlags -> HaskellFlags -> FileWriter -> (ScopedName -> RDecl -> Maybe CustomType) -> [FilePath] -> EIOT ()
generate af hf fileWriter getCustomType modulePaths = catchAllExceptions $ do
  (ms0,tms) <- loadAndCheckModules af modulePaths
  let rms = if (af_generateTransitive af) then tms else ms0
  forM_ rms $ \rm -> do
    writeModuleFile (moduleMapper (hf_modulePrefix hf))
                    (hf_runtimePackage hf)
                    fileMapper
                    getCustomType
                    fileWriter
                    rm
  case hf_includeRuntime hf of
    (Just runtimedir) -> liftIO $ generateRuntime hf fileWriter runtimedir
    Nothing -> return ()

generateRuntime :: HaskellFlags -> FileWriter -> FilePath  -> IO ()
generateRuntime hf fileWriter runtimedir = do
  files <- dirContents runtimedir
  for_ files $ \inpath -> do
    content <- LBS.readFile (runtimedir </> inpath)
    fileWriter (fileMapper (newModuleName inpath)) (adjustContent content)
  where
    newModuleName :: FilePath -> HaskellModule
    newModuleName path = HaskellModule (T.intercalate "." (map T.pack components1))
      where
        (path1,ext) = splitExtension path
        components = splitDirectories path1
        components1 = case L.stripPrefix ["ADL","Core"] components of
          Nothing -> components
          (Just cs) -> map T.unpack (T.splitOn "." (hf_runtimePackage hf)) <> cs

    adjustContent :: LBS.ByteString -> LBS.ByteString
    adjustContent origLBS = LBS.fromStrict (T.encodeUtf8 newT)
      where origT = T.decodeUtf8 (LBS.toStrict origLBS)
            newT = T.replace "ADL.Core"  (hf_runtimePackage hf) origT
