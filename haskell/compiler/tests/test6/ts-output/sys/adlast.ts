/* @generated from adl module sys.adlast */

import * as ADL from './../runtime/adl';
import * as sys_types from './types';

export type ModuleName = string;

const ModuleName_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"ModuleName","version":{"kind":"nothing"}}};

export const snModuleName: ADL.ScopedName = {moduleName:"sys.adlast", name:"ModuleName"};

export function texprModuleName(): ADL.ATypeExpr<ModuleName> {
  return {value : {typeRef : {kind: "reference", value : snModuleName}, parameters : []}};
}

export type Ident = string;

const Ident_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Ident","version":{"kind":"nothing"}}};

export const snIdent: ADL.ScopedName = {moduleName:"sys.adlast", name:"Ident"};

export function texprIdent(): ADL.ATypeExpr<Ident> {
  return {value : {typeRef : {kind: "reference", value : snIdent}, parameters : []}};
}

export type Annotations = sys_types.Map<ScopedName, {}|null>;

const Annotations_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Map"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}},"parameters":[]},{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}}},"name":"Annotations","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}}]}},"name":"ScopedName","version":{"kind":"nothing"}}};

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

const TypeRef_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"primitive","default":{"kind":"nothing"},"name":"primitive","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"typeParam","default":{"kind":"nothing"},"name":"typeParam","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"reference","default":{"kind":"nothing"},"name":"reference","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}},"parameters":[]}}]}},"name":"TypeRef","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"typeRef","default":{"kind":"nothing"},"name":"typeRef","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeRef"}},"parameters":[]}},{"annotations":[],"serializedName":"parameters","default":{"kind":"nothing"},"name":"parameters","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}},"parameters":[]}]}}]}},"name":"TypeExpr","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"serializedName","default":{"kind":"nothing"},"name":"serializedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}},"parameters":[]}},{"annotations":[],"serializedName":"default","default":{"kind":"nothing"},"name":"default","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Annotations"}},"parameters":[]}}]}},"name":"Field","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"fields","default":{"kind":"nothing"},"name":"fields","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Field"}},"parameters":[]}]}}]}},"name":"Struct","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"fields","default":{"kind":"nothing"},"name":"fields","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Field"}},"parameters":[]}]}}]}},"name":"Union","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}},"parameters":[]}}]}},"name":"TypeDef","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}},"parameters":[]}},{"annotations":[],"serializedName":"default","default":{"kind":"nothing"},"name":"default","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}}]}},"name":"NewType","version":{"kind":"nothing"}}};

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

const DeclType_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"struct_","default":{"kind":"nothing"},"name":"struct_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Struct"}},"parameters":[]}},{"annotations":[],"serializedName":"union_","default":{"kind":"nothing"},"name":"union_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Union"}},"parameters":[]}},{"annotations":[],"serializedName":"type_","default":{"kind":"nothing"},"name":"type_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeDef"}},"parameters":[]}},{"annotations":[],"serializedName":"newtype_","default":{"kind":"nothing"},"name":"newtype_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"NewType"}},"parameters":[]}}]}},"name":"DeclType","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"version","default":{"kind":"nothing"},"name":"version","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Word32"},"parameters":[]}]}},{"annotations":[],"serializedName":"type_","default":{"kind":"nothing"},"name":"type_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"DeclType"}},"parameters":[]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Annotations"}},"parameters":[]}}]}},"name":"Decl","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"decl","default":{"kind":"nothing"},"name":"decl","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}},"parameters":[]}}]}},"name":"ScopedDecl","version":{"kind":"nothing"}}};

export const snScopedDecl: ADL.ScopedName = {moduleName:"sys.adlast", name:"ScopedDecl"};

export function texprScopedDecl(): ADL.ATypeExpr<ScopedDecl> {
  return {value : {typeRef : {kind: "reference", value : snScopedDecl}, parameters : []}};
}

export type DeclVersions = Decl[];

const DeclVersions_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}},"parameters":[]}]}}},"name":"DeclVersions","version":{"kind":"nothing"}}};

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

const Import_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"scopedName","default":{"kind":"nothing"},"name":"scopedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}},"parameters":[]}}]}},"name":"Import","version":{"kind":"nothing"}}};

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
  {"moduleName":"sys.adlast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"imports","default":{"kind":"nothing"},"name":"imports","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Import"}},"parameters":[]}]}},{"annotations":[],"serializedName":"decls","default":{"kind":"nothing"},"name":"decls","typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}},"parameters":[]}]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Annotations"}},"parameters":[]}}]}},"name":"Module","version":{"kind":"nothing"}}};

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
