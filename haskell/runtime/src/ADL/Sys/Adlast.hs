{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Adlast(
    Annotations,
    Decl(..),
    DeclType(..),
    DeclVersions,
    Field(..),
    Ident,
    Import(..),
    Module(..),
    ModuleName,
    NewType(..),
    ScopedDecl(..),
    ScopedName(..),
    Struct(..),
    TypeDef(..),
    TypeExpr(..),
    TypeRef(..),
    Union(..),
    mkDecl,
    mkField,
    mkModule,
    mkNewType,
    mkScopedDecl,
    mkScopedName,
    mkStruct,
    mkTypeDef,
    mkTypeExpr,
    mkUnion,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Sys.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

type Annotations = (ADL.Sys.Types.Map ScopedName JS.Value)

data Decl = Decl
    { decl_name :: Ident
    , decl_version :: (ADL.Sys.Types.Maybe Data.Word.Word32)
    , decl_type_ :: DeclType
    , decl_annotations :: Annotations
    }
    deriving (Prelude.Eq,Prelude.Show)

mkDecl :: Ident -> (ADL.Sys.Types.Maybe Data.Word.Word32) -> DeclType -> Annotations -> Decl
mkDecl name version type_ annotations = Decl name version type_ annotations

instance AdlValue Decl where
    atype _ = "sys.adlast.Decl"
    
    jsonGen = genObject
        [ genField "name" decl_name
        , genField "version" decl_version
        , genField "type_" decl_type_
        , genField "annotations" decl_annotations
        ]
    
    jsonParser = Decl
        <$> parseField "name"
        <*> parseField "version"
        <*> parseField "type_"
        <*> parseField "annotations"

data DeclType
    = DeclType_struct_ Struct
    | DeclType_union_ Union
    | DeclType_type_ TypeDef
    | DeclType_newtype_ NewType
    deriving (Prelude.Eq,Prelude.Show)

instance AdlValue DeclType where
    atype _ = "sys.adlast.DeclType"
    
    jsonGen = genUnion (\jv -> case jv of
        DeclType_struct_ v -> genUnionValue "struct_" v
        DeclType_union_ v -> genUnionValue "union_" v
        DeclType_type_ v -> genUnionValue "type_" v
        DeclType_newtype_ v -> genUnionValue "newtype_" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "struct_" ->  parseUnionValue DeclType_struct_
        "union_" ->  parseUnionValue DeclType_union_
        "type_" ->  parseUnionValue DeclType_type_
        "newtype_" ->  parseUnionValue DeclType_newtype_
        _ -> parseFail "expected a discriminator for DeclType (struct_,union_,type_,newtype_)" 

type DeclVersions = [Decl]

data Field = Field
    { field_name :: Ident
    , field_serializedName :: Ident
    , field_typeExpr :: TypeExpr
    , field_default :: (ADL.Sys.Types.Maybe JS.Value)
    , field_annotations :: Annotations
    }
    deriving (Prelude.Eq,Prelude.Show)

mkField :: Ident -> Ident -> TypeExpr -> (ADL.Sys.Types.Maybe JS.Value) -> Annotations -> Field
mkField name serializedName typeExpr default_ annotations = Field name serializedName typeExpr default_ annotations

instance AdlValue Field where
    atype _ = "sys.adlast.Field"
    
    jsonGen = genObject
        [ genField "name" field_name
        , genField "serializedName" field_serializedName
        , genField "typeExpr" field_typeExpr
        , genField "default" field_default
        , genField "annotations" field_annotations
        ]
    
    jsonParser = Field
        <$> parseField "name"
        <*> parseField "serializedName"
        <*> parseField "typeExpr"
        <*> parseField "default"
        <*> parseField "annotations"

type Ident = T.Text

data Import
    = Import_moduleName ModuleName
    | Import_scopedName ScopedName
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Import where
    atype _ = "sys.adlast.Import"
    
    jsonGen = genUnion (\jv -> case jv of
        Import_moduleName v -> genUnionValue "moduleName" v
        Import_scopedName v -> genUnionValue "scopedName" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "moduleName" ->  parseUnionValue Import_moduleName
        "scopedName" ->  parseUnionValue Import_scopedName
        _ -> parseFail "expected a discriminator for Import (moduleName,scopedName)" 

data Module = Module
    { module_name :: ModuleName
    , module_imports :: [Import]
    , module_decls :: StringMap (Decl)
    , module_annotations :: Annotations
    }
    deriving (Prelude.Eq,Prelude.Show)

mkModule :: ModuleName -> [Import] -> StringMap (Decl) -> Annotations -> Module
mkModule name imports decls annotations = Module name imports decls annotations

instance AdlValue Module where
    atype _ = "sys.adlast.Module"
    
    jsonGen = genObject
        [ genField "name" module_name
        , genField "imports" module_imports
        , genField "decls" module_decls
        , genField "annotations" module_annotations
        ]
    
    jsonParser = Module
        <$> parseField "name"
        <*> parseField "imports"
        <*> parseField "decls"
        <*> parseField "annotations"

type ModuleName = T.Text

data NewType = NewType
    { newType_typeParams :: [Ident]
    , newType_typeExpr :: TypeExpr
    , newType_default :: (ADL.Sys.Types.Maybe JS.Value)
    }
    deriving (Prelude.Eq,Prelude.Show)

mkNewType :: [Ident] -> TypeExpr -> (ADL.Sys.Types.Maybe JS.Value) -> NewType
mkNewType typeParams typeExpr default_ = NewType typeParams typeExpr default_

instance AdlValue NewType where
    atype _ = "sys.adlast.NewType"
    
    jsonGen = genObject
        [ genField "typeParams" newType_typeParams
        , genField "typeExpr" newType_typeExpr
        , genField "default" newType_default
        ]
    
    jsonParser = NewType
        <$> parseField "typeParams"
        <*> parseField "typeExpr"
        <*> parseField "default"

data ScopedDecl = ScopedDecl
    { scopedDecl_moduleName :: ModuleName
    , scopedDecl_decl :: Decl
    }
    deriving (Prelude.Eq,Prelude.Show)

mkScopedDecl :: ModuleName -> Decl -> ScopedDecl
mkScopedDecl moduleName decl = ScopedDecl moduleName decl

instance AdlValue ScopedDecl where
    atype _ = "sys.adlast.ScopedDecl"
    
    jsonGen = genObject
        [ genField "moduleName" scopedDecl_moduleName
        , genField "decl" scopedDecl_decl
        ]
    
    jsonParser = ScopedDecl
        <$> parseField "moduleName"
        <*> parseField "decl"

data ScopedName = ScopedName
    { scopedName_moduleName :: ModuleName
    , scopedName_name :: Ident
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkScopedName :: ModuleName -> Ident -> ScopedName
mkScopedName moduleName name = ScopedName moduleName name

instance AdlValue ScopedName where
    atype _ = "sys.adlast.ScopedName"
    
    jsonGen = genObject
        [ genField "moduleName" scopedName_moduleName
        , genField "name" scopedName_name
        ]
    
    jsonParser = ScopedName
        <$> parseField "moduleName"
        <*> parseField "name"

data Struct = Struct
    { struct_typeParams :: [Ident]
    , struct_fields :: [Field]
    }
    deriving (Prelude.Eq,Prelude.Show)

mkStruct :: [Ident] -> [Field] -> Struct
mkStruct typeParams fields = Struct typeParams fields

instance AdlValue Struct where
    atype _ = "sys.adlast.Struct"
    
    jsonGen = genObject
        [ genField "typeParams" struct_typeParams
        , genField "fields" struct_fields
        ]
    
    jsonParser = Struct
        <$> parseField "typeParams"
        <*> parseField "fields"

data TypeDef = TypeDef
    { typeDef_typeParams :: [Ident]
    , typeDef_typeExpr :: TypeExpr
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkTypeDef :: [Ident] -> TypeExpr -> TypeDef
mkTypeDef typeParams typeExpr = TypeDef typeParams typeExpr

instance AdlValue TypeDef where
    atype _ = "sys.adlast.TypeDef"
    
    jsonGen = genObject
        [ genField "typeParams" typeDef_typeParams
        , genField "typeExpr" typeDef_typeExpr
        ]
    
    jsonParser = TypeDef
        <$> parseField "typeParams"
        <*> parseField "typeExpr"

data TypeExpr = TypeExpr
    { typeExpr_typeRef :: TypeRef
    , typeExpr_parameters :: [TypeExpr]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkTypeExpr :: TypeRef -> [TypeExpr] -> TypeExpr
mkTypeExpr typeRef parameters = TypeExpr typeRef parameters

instance AdlValue TypeExpr where
    atype _ = "sys.adlast.TypeExpr"
    
    jsonGen = genObject
        [ genField "typeRef" typeExpr_typeRef
        , genField "parameters" typeExpr_parameters
        ]
    
    jsonParser = TypeExpr
        <$> parseField "typeRef"
        <*> parseField "parameters"

data TypeRef
    = TypeRef_primitive Ident
    | TypeRef_typeParam Ident
    | TypeRef_reference ScopedName
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue TypeRef where
    atype _ = "sys.adlast.TypeRef"
    
    jsonGen = genUnion (\jv -> case jv of
        TypeRef_primitive v -> genUnionValue "primitive" v
        TypeRef_typeParam v -> genUnionValue "typeParam" v
        TypeRef_reference v -> genUnionValue "reference" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "primitive" ->  parseUnionValue TypeRef_primitive
        "typeParam" ->  parseUnionValue TypeRef_typeParam
        "reference" ->  parseUnionValue TypeRef_reference
        _ -> parseFail "expected a discriminator for TypeRef (primitive,typeParam,reference)" 

data Union = Union
    { union_typeParams :: [Ident]
    , union_fields :: [Field]
    }
    deriving (Prelude.Eq,Prelude.Show)

mkUnion :: [Ident] -> [Field] -> Union
mkUnion typeParams fields = Union typeParams fields

instance AdlValue Union where
    atype _ = "sys.adlast.Union"
    
    jsonGen = genObject
        [ genField "typeParams" union_typeParams
        , genField "fields" union_fields
        ]
    
    jsonParser = Union
        <$> parseField "typeParams"
        <*> parseField "fields"