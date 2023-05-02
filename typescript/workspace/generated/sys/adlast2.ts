import * as sys_types from "./types";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module sys.adlast2 */

export type ModuleName = string;

const ModuleName_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"ModuleName","version":{"kind":"nothing"}}};

export const snModuleName: ADL.ScopedName = {moduleName:"sys.adlast2", name:"ModuleName"};

export function texprModuleName(): ADL.ATypeExpr<ModuleName> {
  return {value:{typeRef:{kind:"reference",value:snModuleName},parameters:[]}};
}

export type Ident = string;

const Ident_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Ident","version":{"kind":"nothing"}}};

export const snIdent: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Ident"};

export function texprIdent(): ADL.ATypeExpr<Ident> {
  return {value:{typeRef:{kind:"reference",value:snIdent},parameters:[]}};
}

export type Annotations = sys_types.Map<ScopedName, {}|null>;

const Annotations_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Map"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ScopedName"}},"parameters":[]},{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}}},"name":"Annotations","version":{"kind":"nothing"}}};

export const snAnnotations: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Annotations"};

export function texprAnnotations(): ADL.ATypeExpr<Annotations> {
  return {value:{typeRef:{kind:"reference",value:snAnnotations},parameters:[]}};
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
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}}]}},"name":"ScopedName","version":{"kind":"nothing"}}};

export const snScopedName: ADL.ScopedName = {moduleName:"sys.adlast2", name:"ScopedName"};

export function texprScopedName(): ADL.ATypeExpr<ScopedName> {
  return {value:{typeRef:{kind:"reference",value:snScopedName},parameters:[]}};
}

export interface TypeExpr<R> {
  typeRef: R;
  parameters: TypeExpr<R>[];
}

export function makeTypeExpr<R>(
  input: {
    typeRef: R,
    parameters: TypeExpr<R>[],
  }
): TypeExpr<R> {
  return {
    typeRef: input.typeRef,
    parameters: input.parameters,
  };
}

const TypeExpr_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["R"],"fields":[{"annotations":[],"serializedName":"typeRef","default":{"kind":"nothing"},"name":"typeRef","typeExpr":{"typeRef":{"kind":"typeParam","value":"R"},"parameters":[]}},{"annotations":[],"serializedName":"parameters","default":{"kind":"nothing"},"name":"parameters","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"R"},"parameters":[]}]}]}}]}},"name":"TypeExpr","version":{"kind":"nothing"}}};

export const snTypeExpr: ADL.ScopedName = {moduleName:"sys.adlast2", name:"TypeExpr"};

export function texprTypeExpr<R>(texprR : ADL.ATypeExpr<R>): ADL.ATypeExpr<TypeExpr<R>> {
  return {value:{typeRef:{kind:"reference",value:snTypeExpr},parameters:[texprR.value]}};
}

export interface Field<TE> {
  name: Ident;
  serializedName: Ident;
  typeExpr: TE;
  default: sys_types.Maybe<{}|null>;
  annotations: Annotations;
}

export function makeField<TE>(
  input: {
    name: Ident,
    serializedName: Ident,
    typeExpr: TE,
    default: sys_types.Maybe<{}|null>,
    annotations: Annotations,
  }
): Field<TE> {
  return {
    name: input.name,
    serializedName: input.serializedName,
    typeExpr: input.typeExpr,
    default: input.default,
    annotations: input.annotations,
  };
}

const Field_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"serializedName","default":{"kind":"nothing"},"name":"serializedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}},{"annotations":[],"serializedName":"default","default":{"kind":"nothing"},"name":"default","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Annotations"}},"parameters":[]}}]}},"name":"Field","version":{"kind":"nothing"}}};

export const snField: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Field"};

export function texprField<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<Field<TE>> {
  return {value:{typeRef:{kind:"reference",value:snField},parameters:[texprTE.value]}};
}

export interface Struct<TE> {
  typeParams: Ident[];
  fields: Field<TE>[];
}

export function makeStruct<TE>(
  input: {
    typeParams: Ident[],
    fields: Field<TE>[],
  }
): Struct<TE> {
  return {
    typeParams: input.typeParams,
    fields: input.fields,
  };
}

const Struct_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"fields","default":{"kind":"nothing"},"name":"fields","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Field"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}]}}]}},"name":"Struct","version":{"kind":"nothing"}}};

export const snStruct: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Struct"};

export function texprStruct<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<Struct<TE>> {
  return {value:{typeRef:{kind:"reference",value:snStruct},parameters:[texprTE.value]}};
}

export interface Union<TE> {
  typeParams: Ident[];
  fields: Field<TE>[];
}

export function makeUnion<TE>(
  input: {
    typeParams: Ident[],
    fields: Field<TE>[],
  }
): Union<TE> {
  return {
    typeParams: input.typeParams,
    fields: input.fields,
  };
}

const Union_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"fields","default":{"kind":"nothing"},"name":"fields","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Field"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}]}}]}},"name":"Union","version":{"kind":"nothing"}}};

export const snUnion: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Union"};

export function texprUnion<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<Union<TE>> {
  return {value:{typeRef:{kind:"reference",value:snUnion},parameters:[texprTE.value]}};
}

export interface TypeDef<TE> {
  typeParams: Ident[];
  typeExpr: TE;
}

export function makeTypeDef<TE>(
  input: {
    typeParams: Ident[],
    typeExpr: TE,
  }
): TypeDef<TE> {
  return {
    typeParams: input.typeParams,
    typeExpr: input.typeExpr,
  };
}

const TypeDef_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}}]}},"name":"TypeDef","version":{"kind":"nothing"}}};

export const snTypeDef: ADL.ScopedName = {moduleName:"sys.adlast2", name:"TypeDef"};

export function texprTypeDef<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<TypeDef<TE>> {
  return {value:{typeRef:{kind:"reference",value:snTypeDef},parameters:[texprTE.value]}};
}

export interface NewType<TE> {
  typeParams: Ident[];
  typeExpr: TE;
  default: sys_types.Maybe<{}|null>;
}

export function makeNewType<TE>(
  input: {
    typeParams: Ident[],
    typeExpr: TE,
    default: sys_types.Maybe<{}|null>,
  }
): NewType<TE> {
  return {
    typeParams: input.typeParams,
    typeExpr: input.typeExpr,
    default: input.default,
  };
}

const NewType_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"typeParams","default":{"kind":"nothing"},"name":"typeParams","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}},{"annotations":[],"serializedName":"default","default":{"kind":"nothing"},"name":"default","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}}]}},"name":"NewType","version":{"kind":"nothing"}}};

export const snNewType: ADL.ScopedName = {moduleName:"sys.adlast2", name:"NewType"};

export function texprNewType<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<NewType<TE>> {
  return {value:{typeRef:{kind:"reference",value:snNewType},parameters:[texprTE.value]}};
}

export interface DeclType_Struct_<TE> {
  kind: 'struct_';
  value: Struct<TE>;
}
export interface DeclType_Union_<TE> {
  kind: 'union_';
  value: Union<TE>;
}
export interface DeclType_Type_<TE> {
  kind: 'type_';
  value: TypeDef<TE>;
}
export interface DeclType_Newtype_<TE> {
  kind: 'newtype_';
  value: NewType<TE>;
}

export type DeclType<TE> = DeclType_Struct_<TE> | DeclType_Union_<TE> | DeclType_Type_<TE> | DeclType_Newtype_<TE>;

export interface DeclTypeOpts<TE> {
  struct_: Struct<TE>;
  union_: Union<TE>;
  type_: TypeDef<TE>;
  newtype_: NewType<TE>;
}

export function makeDeclType<TE, K extends keyof DeclTypeOpts<TE>>(kind: K, value: DeclTypeOpts<TE>[K]) { return {kind, value}; }

const DeclType_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"struct_","default":{"kind":"nothing"},"name":"struct_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Struct"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}},{"annotations":[],"serializedName":"union_","default":{"kind":"nothing"},"name":"union_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Union"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}},{"annotations":[],"serializedName":"type_","default":{"kind":"nothing"},"name":"type_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeDef"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}},{"annotations":[],"serializedName":"newtype_","default":{"kind":"nothing"},"name":"newtype_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"NewType"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}}]}},"name":"DeclType","version":{"kind":"nothing"}}};

export const snDeclType: ADL.ScopedName = {moduleName:"sys.adlast2", name:"DeclType"};

export function texprDeclType<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<DeclType<TE>> {
  return {value:{typeRef:{kind:"reference",value:snDeclType},parameters:[texprTE.value]}};
}

export interface Decl<TE> {
  name: Ident;
  version: sys_types.Maybe<number>;
  type_: DeclType<TE>;
  annotations: Annotations;
}

export function makeDecl<TE>(
  input: {
    name: Ident,
    version: sys_types.Maybe<number>,
    type_: DeclType<TE>,
    annotations: Annotations,
  }
): Decl<TE> {
  return {
    name: input.name,
    version: input.version,
    type_: input.type_,
    annotations: input.annotations,
  };
}

const Decl_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"version","default":{"kind":"nothing"},"name":"version","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Word32"},"parameters":[]}]}},{"annotations":[],"serializedName":"type_","default":{"kind":"nothing"},"name":"type_","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"DeclType"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Annotations"}},"parameters":[]}}]}},"name":"Decl","version":{"kind":"nothing"}}};

export const snDecl: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Decl"};

export function texprDecl<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<Decl<TE>> {
  return {value:{typeRef:{kind:"reference",value:snDecl},parameters:[texprTE.value]}};
}

export interface ScopedDecl<TE> {
  moduleName: ModuleName;
  decl: Decl<TE>;
}

export function makeScopedDecl<TE>(
  input: {
    moduleName: ModuleName,
    decl: Decl<TE>,
  }
): ScopedDecl<TE> {
  return {
    moduleName: input.moduleName,
    decl: input.decl,
  };
}

const ScopedDecl_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"decl","default":{"kind":"nothing"},"name":"decl","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Decl"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}}]}},"name":"ScopedDecl","version":{"kind":"nothing"}}};

export const snScopedDecl: ADL.ScopedName = {moduleName:"sys.adlast2", name:"ScopedDecl"};

export function texprScopedDecl<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<ScopedDecl<TE>> {
  return {value:{typeRef:{kind:"reference",value:snScopedDecl},parameters:[texprTE.value]}};
}

export type DeclVersions<TE> = Decl<TE>[];

const DeclVersions_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["TE"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Decl"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}]}}},"name":"DeclVersions","version":{"kind":"nothing"}}};

export const snDeclVersions: ADL.ScopedName = {moduleName:"sys.adlast2", name:"DeclVersions"};

export function texprDeclVersions<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<DeclVersions<TE>> {
  return {value:{typeRef:{kind:"reference",value:snDeclVersions},parameters:[texprTE.value]}};
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
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"scopedName","default":{"kind":"nothing"},"name":"scopedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ScopedName"}},"parameters":[]}}]}},"name":"Import","version":{"kind":"nothing"}}};

export const snImport: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Import"};

export function texprImport(): ADL.ATypeExpr<Import> {
  return {value:{typeRef:{kind:"reference",value:snImport},parameters:[]}};
}

export interface Module<TE> {
  name: ModuleName;
  imports: Import[];
  decls: Decl<TE>[];
  annotations: Annotations;
}

export function makeModule<TE>(
  input: {
    name: ModuleName,
    imports: Import[],
    decls: Decl<TE>[],
    annotations: Annotations,
  }
): Module<TE> {
  return {
    name: input.name,
    imports: input.imports,
    decls: input.decls,
    annotations: input.annotations,
  };
}

const Module_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["TE"],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"imports","default":{"kind":"nothing"},"name":"imports","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Import"}},"parameters":[]}]}},{"annotations":[],"serializedName":"decls","default":{"kind":"nothing"},"name":"decls","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Decl"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"TE"},"parameters":[]}]}]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Annotations"}},"parameters":[]}}]}},"name":"Module","version":{"kind":"nothing"}}};

export const snModule: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Module"};

export function texprModule<TE>(texprTE : ADL.ATypeExpr<TE>): ADL.ATypeExpr<Module<TE>> {
  return {value:{typeRef:{kind:"reference",value:snModule},parameters:[texprTE.value]}};
}

/**
 * The Span start..end contains all values with start <= x < end. It is empty if start >= end.
 */
export interface Span {
  start: number;
  end: number;
}

export function makeSpan(
  input: {
    start: number,
    end: number,
  }
): Span {
  return {
    start: input.start,
    end: input.end,
  };
}

const Span_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"start","default":{"kind":"nothing"},"name":"start","typeExpr":{"typeRef":{"kind":"primitive","value":"Word64"},"parameters":[]}},{"annotations":[],"serializedName":"end","default":{"kind":"nothing"},"name":"end","typeExpr":{"typeRef":{"kind":"primitive","value":"Word64"},"parameters":[]}}]}},"name":"Span","version":{"kind":"nothing"}}};

export const snSpan: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Span"};

export function texprSpan(): ADL.ATypeExpr<Span> {
  return {value:{typeRef:{kind:"reference",value:snSpan},parameters:[]}};
}

export interface Spanned<T> {
  value: T;
  span: Span;
}

export function makeSpanned<T>(
  input: {
    value: T,
    span: Span,
  }
): Spanned<T> {
  return {
    value: input.value,
    span: input.span,
  };
}

const Spanned_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"span","default":{"kind":"nothing"},"name":"span","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Span"}},"parameters":[]}}]}},"name":"Spanned","version":{"kind":"nothing"}}};

export const snSpanned: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Spanned"};

export function texprSpanned<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Spanned<T>> {
  return {value:{typeRef:{kind:"reference",value:snSpanned},parameters:[texprT.value]}};
}

export type PrimitiveType = 'Void' | 'Bool' | 'Int8' | 'Int16' | 'Int32' | 'Int64' | 'Word8' | 'Word16' | 'Word32' | 'Word64' | 'Float' | 'Double' | 'Json' | 'ByteVector' | 'String' | 'Vector' | 'StringMap' | 'Nullable' | 'TypeToken';
export const valuesPrimitiveType : PrimitiveType[] = ['Void', 'Bool', 'Int8', 'Int16', 'Int32', 'Int64', 'Word8', 'Word16', 'Word32', 'Word64', 'Float', 'Double', 'Json', 'ByteVector', 'String', 'Vector', 'StringMap', 'Nullable', 'TypeToken'];

const PrimitiveType_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"Void","default":{"kind":"nothing"},"name":"Void","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Bool","default":{"kind":"nothing"},"name":"Bool","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Int8","default":{"kind":"nothing"},"name":"Int8","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Int16","default":{"kind":"nothing"},"name":"Int16","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Int32","default":{"kind":"nothing"},"name":"Int32","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Int64","default":{"kind":"nothing"},"name":"Int64","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Word8","default":{"kind":"nothing"},"name":"Word8","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Word16","default":{"kind":"nothing"},"name":"Word16","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Word32","default":{"kind":"nothing"},"name":"Word32","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Word64","default":{"kind":"nothing"},"name":"Word64","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Float","default":{"kind":"nothing"},"name":"Float","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Double","default":{"kind":"nothing"},"name":"Double","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Json","default":{"kind":"nothing"},"name":"Json","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"ByteVector","default":{"kind":"nothing"},"name":"ByteVector","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"String","default":{"kind":"nothing"},"name":"String","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Vector","default":{"kind":"nothing"},"name":"Vector","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"StringMap","default":{"kind":"nothing"},"name":"StringMap","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"Nullable","default":{"kind":"nothing"},"name":"Nullable","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"TypeToken","default":{"kind":"nothing"},"name":"TypeToken","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"PrimitiveType","version":{"kind":"nothing"}}};

export const snPrimitiveType: ADL.ScopedName = {moduleName:"sys.adlast2", name:"PrimitiveType"};

export function texprPrimitiveType(): ADL.ATypeExpr<PrimitiveType> {
  return {value:{typeRef:{kind:"reference",value:snPrimitiveType},parameters:[]}};
}

export interface TypeRef_ScopedName {
  kind: 'scopedName';
  value: ScopedName;
}
export interface TypeRef_LocalName {
  kind: 'localName';
  value: Ident;
}
export interface TypeRef_Primitive {
  kind: 'primitive';
  value: PrimitiveType;
}
export interface TypeRef_TypeParam {
  kind: 'typeParam';
  value: Ident;
}

export type TypeRef = TypeRef_ScopedName | TypeRef_LocalName | TypeRef_Primitive | TypeRef_TypeParam;

export interface TypeRefOpts {
  scopedName: ScopedName;
  localName: Ident;
  primitive: PrimitiveType;
  typeParam: Ident;
}

export function makeTypeRef<K extends keyof TypeRefOpts>(kind: K, value: TypeRefOpts[K]) { return {kind, value}; }

const TypeRef_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"scopedName","default":{"kind":"nothing"},"name":"scopedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ScopedName"}},"parameters":[]}},{"annotations":[],"serializedName":"localName","default":{"kind":"nothing"},"name":"localName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}},{"annotations":[],"serializedName":"primitive","default":{"kind":"nothing"},"name":"primitive","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"PrimitiveType"}},"parameters":[]}},{"annotations":[],"serializedName":"typeParam","default":{"kind":"nothing"},"name":"typeParam","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Ident"}},"parameters":[]}}]}},"name":"TypeRef","version":{"kind":"nothing"}}};

export const snTypeRef: ADL.ScopedName = {moduleName:"sys.adlast2", name:"TypeRef"};

export function texprTypeRef(): ADL.ATypeExpr<TypeRef> {
  return {value:{typeRef:{kind:"reference",value:snTypeRef},parameters:[]}};
}

export type TypeExpr0 = TypeExpr<ScopedName>;

const TypeExpr0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ScopedName"}},"parameters":[]}]}}},"name":"TypeExpr0","version":{"kind":"nothing"}}};

export const snTypeExpr0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"TypeExpr0"};

export function texprTypeExpr0(): ADL.ATypeExpr<TypeExpr0> {
  return {value:{typeRef:{kind:"reference",value:snTypeExpr0},parameters:[]}};
}

export type Field0 = Field<TypeExpr0>;

const Field0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Field"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr0"}},"parameters":[]}]}}},"name":"Field0","version":{"kind":"nothing"}}};

export const snField0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Field0"};

export function texprField0(): ADL.ATypeExpr<Field0> {
  return {value:{typeRef:{kind:"reference",value:snField0},parameters:[]}};
}

export type Struct0 = Struct<TypeExpr0>;

const Struct0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Struct"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr0"}},"parameters":[]}]}}},"name":"Struct0","version":{"kind":"nothing"}}};

export const snStruct0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Struct0"};

export function texprStruct0(): ADL.ATypeExpr<Struct0> {
  return {value:{typeRef:{kind:"reference",value:snStruct0},parameters:[]}};
}

export type Union0 = Union<TypeExpr0>;

const Union0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Union"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr0"}},"parameters":[]}]}}},"name":"Union0","version":{"kind":"nothing"}}};

export const snUnion0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Union0"};

export function texprUnion0(): ADL.ATypeExpr<Union0> {
  return {value:{typeRef:{kind:"reference",value:snUnion0},parameters:[]}};
}

export type DeclType0 = DeclType<TypeExpr0>;

const DeclType0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"DeclType"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr0"}},"parameters":[]}]}}},"name":"DeclType0","version":{"kind":"nothing"}}};

export const snDeclType0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"DeclType0"};

export function texprDeclType0(): ADL.ATypeExpr<DeclType0> {
  return {value:{typeRef:{kind:"reference",value:snDeclType0},parameters:[]}};
}

export type Decl0 = Decl<TypeExpr0>;

const Decl0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Decl"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr0"}},"parameters":[]}]}}},"name":"Decl0","version":{"kind":"nothing"}}};

export const snDecl0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Decl0"};

export function texprDecl0(): ADL.ATypeExpr<Decl0> {
  return {value:{typeRef:{kind:"reference",value:snDecl0},parameters:[]}};
}

export type Module0 = Module<TypeExpr0>;

const Module0_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Module"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr0"}},"parameters":[]}]}}},"name":"Module0","version":{"kind":"nothing"}}};

export const snModule0: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Module0"};

export function texprModule0(): ADL.ATypeExpr<Module0> {
  return {value:{typeRef:{kind:"reference",value:snModule0},parameters:[]}};
}

export type TypeExpr1 = TypeExpr<TypeRef>;

const TypeExpr1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeRef"}},"parameters":[]}]}}},"name":"TypeExpr1","version":{"kind":"nothing"}}};

export const snTypeExpr1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"TypeExpr1"};

export function texprTypeExpr1(): ADL.ATypeExpr<TypeExpr1> {
  return {value:{typeRef:{kind:"reference",value:snTypeExpr1},parameters:[]}};
}

export type Field1 = Field<TypeExpr1>;

const Field1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Field"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}}},"name":"Field1","version":{"kind":"nothing"}}};

export const snField1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Field1"};

export function texprField1(): ADL.ATypeExpr<Field1> {
  return {value:{typeRef:{kind:"reference",value:snField1},parameters:[]}};
}

export type Struct1 = Struct<TypeExpr1>;

const Struct1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Struct"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}}},"name":"Struct1","version":{"kind":"nothing"}}};

export const snStruct1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Struct1"};

export function texprStruct1(): ADL.ATypeExpr<Struct1> {
  return {value:{typeRef:{kind:"reference",value:snStruct1},parameters:[]}};
}

export type Union1 = Union<TypeExpr1>;

const Union1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Union"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}}},"name":"Union1","version":{"kind":"nothing"}}};

export const snUnion1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Union1"};

export function texprUnion1(): ADL.ATypeExpr<Union1> {
  return {value:{typeRef:{kind:"reference",value:snUnion1},parameters:[]}};
}

export type DeclType1 = DeclType<TypeExpr1>;

const DeclType1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"DeclType"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}}},"name":"DeclType1","version":{"kind":"nothing"}}};

export const snDeclType1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"DeclType1"};

export function texprDeclType1(): ADL.ATypeExpr<DeclType1> {
  return {value:{typeRef:{kind:"reference",value:snDeclType1},parameters:[]}};
}

export type Decl1 = Decl<TypeExpr1>;

const Decl1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Decl"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}}},"name":"Decl1","version":{"kind":"nothing"}}};

export const snDecl1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Decl1"};

export function texprDecl1(): ADL.ATypeExpr<Decl1> {
  return {value:{typeRef:{kind:"reference",value:snDecl1},parameters:[]}};
}

export type Module1 = Module<TypeExpr1>;

const Module1_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Module"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}}},"name":"Module1","version":{"kind":"nothing"}}};

export const snModule1: ADL.ScopedName = {moduleName:"sys.adlast2", name:"Module1"};

export function texprModule1(): ADL.ATypeExpr<Module1> {
  return {value:{typeRef:{kind:"reference",value:snModule1},parameters:[]}};
}

export type AdlAst = {[key: string]: AstModule};

const AdlAst_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"AstModule"}},"parameters":[]}]}}},"name":"AdlAst","version":{"kind":"nothing"}}};

export const snAdlAst: ADL.ScopedName = {moduleName:"sys.adlast2", name:"AdlAst"};

export function texprAdlAst(): ADL.ATypeExpr<AdlAst> {
  return {value:{typeRef:{kind:"reference",value:snAdlAst},parameters:[]}};
}

export interface AstModule {
  name: ModuleName;
  imports: Import[];
  decls: {[key: string]: Decl<TypeExpr1>};
  annotations: Annotations;
}

export function makeAstModule(
  input: {
    name: ModuleName,
    imports: Import[],
    decls: {[key: string]: Decl<TypeExpr1>},
    annotations: Annotations,
  }
): AstModule {
  return {
    name: input.name,
    imports: input.imports,
    decls: input.decls,
    annotations: input.annotations,
  };
}

const AstModule_AST : ADL.ScopedDecl =
  {"moduleName":"sys.adlast2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"imports","default":{"kind":"nothing"},"name":"imports","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Import"}},"parameters":[]}]}},{"annotations":[],"serializedName":"decls","default":{"kind":"nothing"},"name":"decls","typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Decl"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"TypeExpr1"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"annotations","default":{"kind":"nothing"},"name":"annotations","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast2","name":"Annotations"}},"parameters":[]}}]}},"name":"AstModule","version":{"kind":"nothing"}}};

export const snAstModule: ADL.ScopedName = {moduleName:"sys.adlast2", name:"AstModule"};

export function texprAstModule(): ADL.ATypeExpr<AstModule> {
  return {value:{typeRef:{kind:"reference",value:snAstModule},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.adlast2.ModuleName" : ModuleName_AST,
  "sys.adlast2.Ident" : Ident_AST,
  "sys.adlast2.Annotations" : Annotations_AST,
  "sys.adlast2.ScopedName" : ScopedName_AST,
  "sys.adlast2.TypeExpr" : TypeExpr_AST,
  "sys.adlast2.Field" : Field_AST,
  "sys.adlast2.Struct" : Struct_AST,
  "sys.adlast2.Union" : Union_AST,
  "sys.adlast2.TypeDef" : TypeDef_AST,
  "sys.adlast2.NewType" : NewType_AST,
  "sys.adlast2.DeclType" : DeclType_AST,
  "sys.adlast2.Decl" : Decl_AST,
  "sys.adlast2.ScopedDecl" : ScopedDecl_AST,
  "sys.adlast2.DeclVersions" : DeclVersions_AST,
  "sys.adlast2.Import" : Import_AST,
  "sys.adlast2.Module" : Module_AST,
  "sys.adlast2.Span" : Span_AST,
  "sys.adlast2.Spanned" : Spanned_AST,
  "sys.adlast2.PrimitiveType" : PrimitiveType_AST,
  "sys.adlast2.TypeRef" : TypeRef_AST,
  "sys.adlast2.TypeExpr0" : TypeExpr0_AST,
  "sys.adlast2.Field0" : Field0_AST,
  "sys.adlast2.Struct0" : Struct0_AST,
  "sys.adlast2.Union0" : Union0_AST,
  "sys.adlast2.DeclType0" : DeclType0_AST,
  "sys.adlast2.Decl0" : Decl0_AST,
  "sys.adlast2.Module0" : Module0_AST,
  "sys.adlast2.TypeExpr1" : TypeExpr1_AST,
  "sys.adlast2.Field1" : Field1_AST,
  "sys.adlast2.Struct1" : Struct1_AST,
  "sys.adlast2.Union1" : Union1_AST,
  "sys.adlast2.DeclType1" : DeclType1_AST,
  "sys.adlast2.Decl1" : Decl1_AST,
  "sys.adlast2.Module1" : Module1_AST,
  "sys.adlast2.AdlAst" : AdlAst_AST,
  "sys.adlast2.AstModule" : AstModule_AST
};
