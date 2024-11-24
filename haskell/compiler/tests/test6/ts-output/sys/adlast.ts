/* @generated from adl module sys.adlast */

import * as ADL from '@adllang/adl-runtime';
import * as sys_types from './types';

export type ModuleName = string;

const ModuleName_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"ModuleName","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snModuleName: ADL.ScopedName = {moduleName:"sys.adlast", name:"ModuleName"};

export function texprModuleName(): ADL.ATypeExpr<ModuleName> {
  return {value : {typeRef : {kind: "reference", value : snModuleName}, parameters : []}};
}

export type Ident = string;

const Ident_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Ident","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snIdent: ADL.ScopedName = {moduleName:"sys.adlast", name:"Ident"};

export function texprIdent(): ADL.ATypeExpr<Ident> {
  return {value : {typeRef : {kind: "reference", value : snIdent}, parameters : []}};
}

export type Annotations = sys_types.Map<ScopedName, {}|null>;

const Annotations_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Annotations","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}}},{"parameters":[],"typeRef":{"kind":"primitive","value":"Json"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Map"}}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snAnnotations: ADL.ScopedName = {moduleName:"sys.adlast", name:"Annotations"};

export function texprAnnotations(): ADL.ATypeExpr<Annotations> {
  return {value : {typeRef : {kind: "reference", value : snAnnotations}, parameters : []}};
}

export interface ScopedName {
  moduleName: ModuleName;
  name: Ident;
}

export function makeScopedName(
  input: {
    moduleName: ModuleName,
    name: Ident,
  }
): ScopedName {
  return {
    moduleName: input.moduleName,
    name: input.name,
  };
}

const ScopedName_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"ScopedName","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"moduleName","serializedName":"moduleName","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"name","serializedName":"name","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snScopedName: ADL.ScopedName = {moduleName:"sys.adlast", name:"ScopedName"};

export function texprScopedName(): ADL.ATypeExpr<ScopedName> {
  return {value : {typeRef : {kind: "reference", value : snScopedName}, parameters : []}};
}

export interface TypeRef_Primitive {
  kind: 'primitive';
  value: Ident;
}
export interface TypeRef_TypeParam {
  kind: 'typeParam';
  value: Ident;
}
export interface TypeRef_Reference {
  kind: 'reference';
  value: ScopedName;
}

export type TypeRef = TypeRef_Primitive | TypeRef_TypeParam | TypeRef_Reference;

export interface TypeRefOpts {
  primitive: Ident;
  typeParam: Ident;
  reference: ScopedName;
}

export function makeTypeRef<K extends keyof TypeRefOpts>(kind: K, value: TypeRefOpts[K]) { return {kind, value}; }

const TypeRef_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"TypeRef","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"primitive","serializedName":"primitive","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"typeParam","serializedName":"typeParam","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"reference","serializedName":"reference","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snTypeRef: ADL.ScopedName = {moduleName:"sys.adlast", name:"TypeRef"};

export function texprTypeRef(): ADL.ATypeExpr<TypeRef> {
  return {value : {typeRef : {kind: "reference", value : snTypeRef}, parameters : []}};
}

export interface TypeExpr {
  typeRef: TypeRef;
  parameters: TypeExpr[];
}

export function makeTypeExpr(
  input: {
    typeRef: TypeRef,
    parameters: TypeExpr[],
  }
): TypeExpr {
  return {
    typeRef: input.typeRef,
    parameters: input.parameters,
  };
}

const TypeExpr_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"TypeExpr","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"typeRef","serializedName":"typeRef","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeRef"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"parameters","serializedName":"parameters","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snTypeExpr: ADL.ScopedName = {moduleName:"sys.adlast", name:"TypeExpr"};

export function texprTypeExpr(): ADL.ATypeExpr<TypeExpr> {
  return {value : {typeRef : {kind: "reference", value : snTypeExpr}, parameters : []}};
}

export interface Field {
  name: Ident;
  serializedName: Ident;
  typeExpr: TypeExpr;
  default: sys_types.Maybe<{}|null>;
  annotations: Annotations;
}

export function makeField(
  input: {
    name: Ident,
    serializedName: Ident,
    typeExpr: TypeExpr,
    default: sys_types.Maybe<{}|null>,
    annotations: Annotations,
  }
): Field {
  return {
    name: input.name,
    serializedName: input.serializedName,
    typeExpr: input.typeExpr,
    default: input.default,
    annotations: input.annotations,
  };
}

const Field_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Field","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"name","serializedName":"name","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"serializedName","serializedName":"serializedName","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"typeExpr","serializedName":"typeExpr","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"default","serializedName":"default","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Json"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"annotations","serializedName":"annotations","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Annotations"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snField: ADL.ScopedName = {moduleName:"sys.adlast", name:"Field"};

export function texprField(): ADL.ATypeExpr<Field> {
  return {value : {typeRef : {kind: "reference", value : snField}, parameters : []}};
}

export interface Struct {
  typeParams: Ident[];
  fields: Field[];
}

export function makeStruct(
  input: {
    typeParams: Ident[],
    fields: Field[],
  }
): Struct {
  return {
    typeParams: input.typeParams,
    fields: input.fields,
  };
}

const Struct_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Struct","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"typeParams","serializedName":"typeParams","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"fields","serializedName":"fields","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Field"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snStruct: ADL.ScopedName = {moduleName:"sys.adlast", name:"Struct"};

export function texprStruct(): ADL.ATypeExpr<Struct> {
  return {value : {typeRef : {kind: "reference", value : snStruct}, parameters : []}};
}

export interface Union {
  typeParams: Ident[];
  fields: Field[];
}

export function makeUnion(
  input: {
    typeParams: Ident[],
    fields: Field[],
  }
): Union {
  return {
    typeParams: input.typeParams,
    fields: input.fields,
  };
}

const Union_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Union","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"typeParams","serializedName":"typeParams","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"fields","serializedName":"fields","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Field"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snUnion: ADL.ScopedName = {moduleName:"sys.adlast", name:"Union"};

export function texprUnion(): ADL.ATypeExpr<Union> {
  return {value : {typeRef : {kind: "reference", value : snUnion}, parameters : []}};
}

export interface TypeDef {
  typeParams: Ident[];
  typeExpr: TypeExpr;
}

export function makeTypeDef(
  input: {
    typeParams: Ident[],
    typeExpr: TypeExpr,
  }
): TypeDef {
  return {
    typeParams: input.typeParams,
    typeExpr: input.typeExpr,
  };
}

const TypeDef_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"TypeDef","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"typeParams","serializedName":"typeParams","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"typeExpr","serializedName":"typeExpr","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snTypeDef: ADL.ScopedName = {moduleName:"sys.adlast", name:"TypeDef"};

export function texprTypeDef(): ADL.ATypeExpr<TypeDef> {
  return {value : {typeRef : {kind: "reference", value : snTypeDef}, parameters : []}};
}

export interface NewType {
  typeParams: Ident[];
  typeExpr: TypeExpr;
  default: sys_types.Maybe<{}|null>;
}

export function makeNewType(
  input: {
    typeParams: Ident[],
    typeExpr: TypeExpr,
    default: sys_types.Maybe<{}|null>,
  }
): NewType {
  return {
    typeParams: input.typeParams,
    typeExpr: input.typeExpr,
    default: input.default,
  };
}

const NewType_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"NewType","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"typeParams","serializedName":"typeParams","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"typeExpr","serializedName":"typeExpr","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"default","serializedName":"default","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Json"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snNewType: ADL.ScopedName = {moduleName:"sys.adlast", name:"NewType"};

export function texprNewType(): ADL.ATypeExpr<NewType> {
  return {value : {typeRef : {kind: "reference", value : snNewType}, parameters : []}};
}

export interface DeclType_Struct_ {
  kind: 'struct_';
  value: Struct;
}
export interface DeclType_Union_ {
  kind: 'union_';
  value: Union;
}
export interface DeclType_Type_ {
  kind: 'type_';
  value: TypeDef;
}
export interface DeclType_Newtype_ {
  kind: 'newtype_';
  value: NewType;
}

export type DeclType = DeclType_Struct_ | DeclType_Union_ | DeclType_Type_ | DeclType_Newtype_;

export interface DeclTypeOpts {
  struct_: Struct;
  union_: Union;
  type_: TypeDef;
  newtype_: NewType;
}

export function makeDeclType<K extends keyof DeclTypeOpts>(kind: K, value: DeclTypeOpts[K]) { return {kind, value}; }

const DeclType_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"DeclType","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"struct_","serializedName":"struct_","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Struct"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"union_","serializedName":"union_","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Union"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"type_","serializedName":"type_","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeDef"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"newtype_","serializedName":"newtype_","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"NewType"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snDeclType: ADL.ScopedName = {moduleName:"sys.adlast", name:"DeclType"};

export function texprDeclType(): ADL.ATypeExpr<DeclType> {
  return {value : {typeRef : {kind: "reference", value : snDeclType}, parameters : []}};
}

export interface Decl {
  name: Ident;
  version: sys_types.Maybe<number>;
  type_: DeclType;
  annotations: Annotations;
}

export function makeDecl(
  input: {
    name: Ident,
    version: sys_types.Maybe<number>,
    type_: DeclType,
    annotations: Annotations,
  }
): Decl {
  return {
    name: input.name,
    version: input.version,
    type_: input.type_,
    annotations: input.annotations,
  };
}

const Decl_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Decl","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"name","serializedName":"name","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"version","serializedName":"version","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Word32"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"type_","serializedName":"type_","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"DeclType"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"annotations","serializedName":"annotations","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Annotations"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snDecl: ADL.ScopedName = {moduleName:"sys.adlast", name:"Decl"};

export function texprDecl(): ADL.ATypeExpr<Decl> {
  return {value : {typeRef : {kind: "reference", value : snDecl}, parameters : []}};
}

export interface ScopedDecl {
  moduleName: ModuleName;
  decl: Decl;
}

export function makeScopedDecl(
  input: {
    moduleName: ModuleName,
    decl: Decl,
  }
): ScopedDecl {
  return {
    moduleName: input.moduleName,
    decl: input.decl,
  };
}

const ScopedDecl_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"ScopedDecl","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"moduleName","serializedName":"moduleName","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"decl","serializedName":"decl","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snScopedDecl: ADL.ScopedName = {moduleName:"sys.adlast", name:"ScopedDecl"};

export function texprScopedDecl(): ADL.ATypeExpr<ScopedDecl> {
  return {value : {typeRef : {kind: "reference", value : snScopedDecl}, parameters : []}};
}

export type DeclVersions = Decl[];

const DeclVersions_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"DeclVersions","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}}}],"typeRef":{"kind":"primitive","value":"Vector"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snDeclVersions: ADL.ScopedName = {moduleName:"sys.adlast", name:"DeclVersions"};

export function texprDeclVersions(): ADL.ATypeExpr<DeclVersions> {
  return {value : {typeRef : {kind: "reference", value : snDeclVersions}, parameters : []}};
}

export interface Import_ModuleName {
  kind: 'moduleName';
  value: ModuleName;
}
export interface Import_ScopedName {
  kind: 'scopedName';
  value: ScopedName;
}

export type Import = Import_ModuleName | Import_ScopedName;

export interface ImportOpts {
  moduleName: ModuleName;
  scopedName: ScopedName;
}

export function makeImport<K extends keyof ImportOpts>(kind: K, value: ImportOpts[K]) { return {kind, value}; }

const Import_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Import","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"moduleName","serializedName":"moduleName","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"scopedName","serializedName":"scopedName","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snImport: ADL.ScopedName = {moduleName:"sys.adlast", name:"Import"};

export function texprImport(): ADL.ATypeExpr<Import> {
  return {value : {typeRef : {kind: "reference", value : snImport}, parameters : []}};
}

export interface Module {
  name: ModuleName;
  imports: Import[];
  decls: {[key: string]: Decl};
  annotations: Annotations;
}

export function makeModule(
  input: {
    name: ModuleName,
    imports: Import[],
    decls: {[key: string]: Decl},
    annotations: Annotations,
  }
): Module {
  return {
    name: input.name,
    imports: input.imports,
    decls: input.decls,
    annotations: input.annotations,
  };
}

const Module_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Module","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"name","serializedName":"name","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"imports","serializedName":"imports","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Import"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"decls","serializedName":"decls","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}}}],"typeRef":{"kind":"primitive","value":"StringMap"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"annotations","serializedName":"annotations","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Annotations"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.adlast"};

export const snModule: ADL.ScopedName = {moduleName:"sys.adlast", name:"Module"};

export function texprModule(): ADL.ATypeExpr<Module> {
  return {value : {typeRef : {kind: "reference", value : snModule}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.adlast.ModuleName" : ModuleName_AST,
  "sys.adlast.Ident" : Ident_AST,
  "sys.adlast.Annotations" : Annotations_AST,
  "sys.adlast.ScopedName" : ScopedName_AST,
  "sys.adlast.TypeRef" : TypeRef_AST,
  "sys.adlast.TypeExpr" : TypeExpr_AST,
  "sys.adlast.Field" : Field_AST,
  "sys.adlast.Struct" : Struct_AST,
  "sys.adlast.Union" : Union_AST,
  "sys.adlast.TypeDef" : TypeDef_AST,
  "sys.adlast.NewType" : NewType_AST,
  "sys.adlast.DeclType" : DeclType_AST,
  "sys.adlast.Decl" : Decl_AST,
  "sys.adlast.ScopedDecl" : ScopedDecl_AST,
  "sys.adlast.DeclVersions" : DeclVersions_AST,
  "sys.adlast.Import" : Import_AST,
  "sys.adlast.Module" : Module_AST
};
