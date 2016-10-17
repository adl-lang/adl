{-# LANGUAGE OverloadedStrings #-}
module ADL.Sys.Adlast(
    Annotations,
    Decl(..),
    DeclType(..),
    DeclVersions,
    Field(..),
    Ident,
    Import(..),
    Literal(..),
    Module(..),
    ModuleName,
    NewType(..),
    ScopedName(..),
    Struct(..),
    TypeDef(..),
    TypeExpr(..),
    TypeRef(..),
    Union(..),
) where

import ADL.Core.Primitives
import ADL.Core.Value
import Control.Applicative( (<$>), (<*>) )
import qualified ADL.Sys.Types
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

type Annotations = (ADL.Sys.Types.Map ScopedName Literal)

data Decl = Decl
    { decl_name :: Ident
    , decl_version :: (ADL.Sys.Types.Maybe Data.Word.Word32)
    , decl_type_ :: DeclType
    , decl_annotations :: Annotations
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Decl where
    atype _ = "sys.adlast.Decl"
    
    defaultv = Decl
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            name_js = jsonSerialiser jf
            version_js = jsonSerialiser jf
            type__js = jsonSerialiser jf
            annotations_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("name",aToJSON name_js (decl_name v))
                , ("version",aToJSON version_js (decl_version v))
                , ("type_",aToJSON type__js (decl_type_ v))
                , ("annotations",aToJSON annotations_js (decl_annotations v))
                ] )
            
            from (JSON.Object hm) = Decl 
                <$> fieldFromJSON name_js "name" defaultv hm
                <*> fieldFromJSON version_js "version" defaultv hm
                <*> fieldFromJSON type__js "type_" defaultv hm
                <*> fieldFromJSON annotations_js "annotations" defaultv hm
            from _ = Prelude.Nothing

data DeclType
    = DeclType_struct_ Struct
    | DeclType_union_ Union
    | DeclType_type_ TypeDef
    | DeclType_newtype_ NewType
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue DeclType where
    atype _ = "sys.adlast.DeclType"
    
    defaultv = DeclType_struct_ defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            struct__js = jsonSerialiser jf
            union__js = jsonSerialiser jf
            type__js = jsonSerialiser jf
            newtype__js = jsonSerialiser jf
            
            to (DeclType_struct_ v) = JSON.Object (HM.singleton "struct_" (aToJSON struct__js v))
            to (DeclType_union_ v) = JSON.Object (HM.singleton "union_" (aToJSON union__js v))
            to (DeclType_type_ v) = JSON.Object (HM.singleton "type_" (aToJSON type__js v))
            to (DeclType_newtype_ v) = JSON.Object (HM.singleton "newtype_" (aToJSON newtype__js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "struct_" -> Prelude.fmap DeclType_struct_ (aFromJSON struct__js v)
                    "union_" -> Prelude.fmap DeclType_union_ (aFromJSON union__js v)
                    "type_" -> Prelude.fmap DeclType_type_ (aFromJSON type__js v)
                    "newtype_" -> Prelude.fmap DeclType_newtype_ (aFromJSON newtype__js v)

type DeclVersions = [Decl]

data Field = Field
    { field_name :: Ident
    , field_typeExpr :: TypeExpr
    , field_default :: (ADL.Sys.Types.Maybe Literal)
    , field_annotations :: Annotations
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Field where
    atype _ = "sys.adlast.Field"
    
    defaultv = Field
        defaultv
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            name_js = jsonSerialiser jf
            typeExpr_js = jsonSerialiser jf
            default_js = jsonSerialiser jf
            annotations_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("name",aToJSON name_js (field_name v))
                , ("typeExpr",aToJSON typeExpr_js (field_typeExpr v))
                , ("default",aToJSON default_js (field_default v))
                , ("annotations",aToJSON annotations_js (field_annotations v))
                ] )
            
            from (JSON.Object hm) = Field 
                <$> fieldFromJSON name_js "name" defaultv hm
                <*> fieldFromJSON typeExpr_js "typeExpr" defaultv hm
                <*> fieldFromJSON default_js "default" defaultv hm
                <*> fieldFromJSON annotations_js "annotations" defaultv hm
            from _ = Prelude.Nothing

type Ident = T.Text

data Import
    = Import_moduleName ModuleName
    | Import_scopedName ScopedName
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Import where
    atype _ = "sys.adlast.Import"
    
    defaultv = Import_moduleName defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            moduleName_js = jsonSerialiser jf
            scopedName_js = jsonSerialiser jf
            
            to (Import_moduleName v) = JSON.Object (HM.singleton "moduleName" (aToJSON moduleName_js v))
            to (Import_scopedName v) = JSON.Object (HM.singleton "scopedName" (aToJSON scopedName_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "moduleName" -> Prelude.fmap Import_moduleName (aFromJSON moduleName_js v)
                    "scopedName" -> Prelude.fmap Import_scopedName (aFromJSON scopedName_js v)

data Literal
    = Literal_null
    | Literal_integer Data.Int.Int64
    | Literal_double Prelude.Double
    | Literal_string T.Text
    | Literal_boolean Prelude.Bool
    | Literal_array [Literal]
    | Literal_object (ADL.Sys.Types.Map T.Text Literal)
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Literal where
    atype _ = "sys.adlast.Literal"
    
    defaultv = Literal_null
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            integer_js = jsonSerialiser jf
            double_js = jsonSerialiser jf
            string_js = jsonSerialiser jf
            boolean_js = jsonSerialiser jf
            array_js = jsonSerialiser jf
            object_js = jsonSerialiser jf
            
            to Literal_null = JSON.Object (HM.singleton "null" JSON.Null)
            to (Literal_integer v) = JSON.Object (HM.singleton "integer" (aToJSON integer_js v))
            to (Literal_double v) = JSON.Object (HM.singleton "double" (aToJSON double_js v))
            to (Literal_string v) = JSON.Object (HM.singleton "string" (aToJSON string_js v))
            to (Literal_boolean v) = JSON.Object (HM.singleton "boolean" (aToJSON boolean_js v))
            to (Literal_array v) = JSON.Object (HM.singleton "array" (aToJSON array_js v))
            to (Literal_object v) = JSON.Object (HM.singleton "object" (aToJSON object_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "null" -> Prelude.Just Literal_null
                    "integer" -> Prelude.fmap Literal_integer (aFromJSON integer_js v)
                    "double" -> Prelude.fmap Literal_double (aFromJSON double_js v)
                    "string" -> Prelude.fmap Literal_string (aFromJSON string_js v)
                    "boolean" -> Prelude.fmap Literal_boolean (aFromJSON boolean_js v)
                    "array" -> Prelude.fmap Literal_array (aFromJSON array_js v)
                    "object" -> Prelude.fmap Literal_object (aFromJSON object_js v)

data Module = Module
    { module_name :: ModuleName
    , module_imports :: [Import]
    , module_decls :: (ADL.Sys.Types.Map Ident Decl)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Module where
    atype _ = "sys.adlast.Module"
    
    defaultv = Module
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            name_js = jsonSerialiser jf
            imports_js = jsonSerialiser jf
            decls_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("name",aToJSON name_js (module_name v))
                , ("imports",aToJSON imports_js (module_imports v))
                , ("decls",aToJSON decls_js (module_decls v))
                ] )
            
            from (JSON.Object hm) = Module 
                <$> fieldFromJSON name_js "name" defaultv hm
                <*> fieldFromJSON imports_js "imports" defaultv hm
                <*> fieldFromJSON decls_js "decls" defaultv hm
            from _ = Prelude.Nothing

type ModuleName = T.Text

data NewType = NewType
    { newType_typeParams :: [Ident]
    , newType_typeExpr :: TypeExpr
    , newType_default :: (ADL.Sys.Types.Maybe Literal)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue NewType where
    atype _ = "sys.adlast.NewType"
    
    defaultv = NewType
        defaultv
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            typeParams_js = jsonSerialiser jf
            typeExpr_js = jsonSerialiser jf
            default_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("typeParams",aToJSON typeParams_js (newType_typeParams v))
                , ("typeExpr",aToJSON typeExpr_js (newType_typeExpr v))
                , ("default",aToJSON default_js (newType_default v))
                ] )
            
            from (JSON.Object hm) = NewType 
                <$> fieldFromJSON typeParams_js "typeParams" defaultv hm
                <*> fieldFromJSON typeExpr_js "typeExpr" defaultv hm
                <*> fieldFromJSON default_js "default" defaultv hm
            from _ = Prelude.Nothing

data ScopedName = ScopedName
    { scopedName_moduleName :: ModuleName
    , scopedName_name :: Ident
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue ScopedName where
    atype _ = "sys.adlast.ScopedName"
    
    defaultv = ScopedName
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            moduleName_js = jsonSerialiser jf
            name_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("moduleName",aToJSON moduleName_js (scopedName_moduleName v))
                , ("name",aToJSON name_js (scopedName_name v))
                ] )
            
            from (JSON.Object hm) = ScopedName 
                <$> fieldFromJSON moduleName_js "moduleName" defaultv hm
                <*> fieldFromJSON name_js "name" defaultv hm
            from _ = Prelude.Nothing

data Struct = Struct
    { struct_typeParams :: [Ident]
    , struct_fields :: [Field]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Struct where
    atype _ = "sys.adlast.Struct"
    
    defaultv = Struct
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            typeParams_js = jsonSerialiser jf
            fields_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("typeParams",aToJSON typeParams_js (struct_typeParams v))
                , ("fields",aToJSON fields_js (struct_fields v))
                ] )
            
            from (JSON.Object hm) = Struct 
                <$> fieldFromJSON typeParams_js "typeParams" defaultv hm
                <*> fieldFromJSON fields_js "fields" defaultv hm
            from _ = Prelude.Nothing

data TypeDef = TypeDef
    { typeDef_typeParams :: [Ident]
    , typeDef_typeExpr :: TypeExpr
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TypeDef where
    atype _ = "sys.adlast.TypeDef"
    
    defaultv = TypeDef
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            typeParams_js = jsonSerialiser jf
            typeExpr_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("typeParams",aToJSON typeParams_js (typeDef_typeParams v))
                , ("typeExpr",aToJSON typeExpr_js (typeDef_typeExpr v))
                ] )
            
            from (JSON.Object hm) = TypeDef 
                <$> fieldFromJSON typeParams_js "typeParams" defaultv hm
                <*> fieldFromJSON typeExpr_js "typeExpr" defaultv hm
            from _ = Prelude.Nothing

data TypeExpr = TypeExpr
    { typeExpr_typeRef :: TypeRef
    , typeExpr_parameters :: [TypeExpr]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TypeExpr where
    atype _ = "sys.adlast.TypeExpr"
    
    defaultv = TypeExpr
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            typeRef_js = jsonSerialiser jf
            parameters_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("typeRef",aToJSON typeRef_js (typeExpr_typeRef v))
                , ("parameters",aToJSON parameters_js (typeExpr_parameters v))
                ] )
            
            from (JSON.Object hm) = TypeExpr 
                <$> fieldFromJSON typeRef_js "typeRef" defaultv hm
                <*> fieldFromJSON parameters_js "parameters" defaultv hm
            from _ = Prelude.Nothing

data TypeRef
    = TypeRef_primitive Ident
    | TypeRef_typeParam Ident
    | TypeRef_reference ScopedName
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue TypeRef where
    atype _ = "sys.adlast.TypeRef"
    
    defaultv = TypeRef_primitive defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            primitive_js = jsonSerialiser jf
            typeParam_js = jsonSerialiser jf
            reference_js = jsonSerialiser jf
            
            to (TypeRef_primitive v) = JSON.Object (HM.singleton "primitive" (aToJSON primitive_js v))
            to (TypeRef_typeParam v) = JSON.Object (HM.singleton "typeParam" (aToJSON typeParam_js v))
            to (TypeRef_reference v) = JSON.Object (HM.singleton "reference" (aToJSON reference_js v))
            
            from o = do
                (key, v) <- splitUnion o
                case key of
                    "primitive" -> Prelude.fmap TypeRef_primitive (aFromJSON primitive_js v)
                    "typeParam" -> Prelude.fmap TypeRef_typeParam (aFromJSON typeParam_js v)
                    "reference" -> Prelude.fmap TypeRef_reference (aFromJSON reference_js v)

data Union = Union
    { union_typeParams :: [Ident]
    , union_fields :: [Field]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance ADLValue Union where
    atype _ = "sys.adlast.Union"
    
    defaultv = Union
        defaultv
        defaultv
    
    jsonSerialiser jf = JSONSerialiser to from
        where
            typeParams_js = jsonSerialiser jf
            fields_js = jsonSerialiser jf
            
            to v = JSON.Object ( HM.fromList
                [ ("typeParams",aToJSON typeParams_js (union_typeParams v))
                , ("fields",aToJSON fields_js (union_fields v))
                ] )
            
            from (JSON.Object hm) = Union 
                <$> fieldFromJSON typeParams_js "typeParams" defaultv hm
                <*> fieldFromJSON fields_js "fields" defaultv hm
            from _ = Prelude.Nothing