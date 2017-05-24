/* Automatically generated by adlc */

import * as sys_types from './types';

export type Annotations = sys_types.Map<ScopedName, Literal>;

export interface Decl {
  annotations: Annotations;
  name: Ident;
  type_: DeclType;
  version: sys_types.Maybe<number>;
}

export function makeDecl(
  input: {
    annotations: Annotations,
    name: Ident,
    type_: DeclType,
    version: sys_types.Maybe<number>,
  }
): Decl {
  return {
    annotations: input.annotations,
    name: input.name,
    type_: input.type_,
    version: input.version,
  };
}

interface DeclType_Newtype_ {
  kind: 'newtype_';
  value: NewType;
}
interface DeclType_Struct_ {
  kind: 'struct_';
  value: Struct;
}
interface DeclType_Type_ {
  kind: 'type_';
  value: TypeDef;
}
interface DeclType_Union_ {
  kind: 'union_';
  value: Union;
}

export function makeDeclType_Newtype_(
  input: {
    kind: 'newtype_',
    value: NewType,
  }
): DeclType_Newtype_ {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeDeclType_Struct_(
  input: {
    kind: 'struct_',
    value: Struct,
  }
): DeclType_Struct_ {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeDeclType_Type_(
  input: {
    kind: 'type_',
    value: TypeDef,
  }
): DeclType_Type_ {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeDeclType_Union_(
  input: {
    kind: 'union_',
    value: Union,
  }
): DeclType_Union_ {
  return {
    kind: input.kind,
    value: input.value,
  };
}

export type DeclType = DeclType_Newtype_ | DeclType_Struct_ | DeclType_Type_ | DeclType_Union_;

export type DeclVersions = Decl[];

export interface Field {
  annotations: Annotations;
  default: sys_types.Maybe<Literal>;
  name: Ident;
  typeExpr: TypeExpr;
}

export function makeField(
  input: {
    annotations: Annotations,
    default: sys_types.Maybe<Literal>,
    name: Ident,
    typeExpr: TypeExpr,
  }
): Field {
  return {
    annotations: input.annotations,
    default: input.default,
    name: input.name,
    typeExpr: input.typeExpr,
  };
}

export type Ident = string;

interface Import_ModuleName {
  kind: 'moduleName';
  value: ModuleName;
}
interface Import_ScopedName {
  kind: 'scopedName';
  value: ScopedName;
}

export function makeImport_ModuleName(
  input: {
    kind: 'moduleName',
    value: ModuleName,
  }
): Import_ModuleName {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeImport_ScopedName(
  input: {
    kind: 'scopedName',
    value: ScopedName,
  }
): Import_ScopedName {
  return {
    kind: input.kind,
    value: input.value,
  };
}

export type Import = Import_ModuleName | Import_ScopedName;

interface Literal_Array {
  kind: 'array';
  value: Literal[];
}
interface Literal_Boolean {
  kind: 'boolean';
  value: boolean;
}
interface Literal_Double {
  kind: 'double';
  value: number;
}
interface Literal_Integer {
  kind: 'integer';
  value: number;
}
interface Literal_Null {
  kind: 'null';
}
interface Literal_Object {
  kind: 'object';
  value: sys_types.Map<string, Literal>;
}
interface Literal_String {
  kind: 'string';
  value: string;
}

export function makeLiteral_Array(
  input: {
    kind: 'array',
    value: Literal[],
  }
): Literal_Array {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeLiteral_Boolean(
  input: {
    kind: 'boolean',
    value: boolean,
  }
): Literal_Boolean {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeLiteral_Double(
  input: {
    kind: 'double',
    value: number,
  }
): Literal_Double {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeLiteral_Integer(
  input: {
    kind: 'integer',
    value: number,
  }
): Literal_Integer {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeLiteral_Null(
  input: {
    kind: 'null',
  }
): Literal_Null {
  return {
    kind: input.kind,
  };
}
export function makeLiteral_Object(
  input: {
    kind: 'object',
    value: sys_types.Map<string, Literal>,
  }
): Literal_Object {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeLiteral_String(
  input: {
    kind: 'string',
    value: string,
  }
): Literal_String {
  return {
    kind: input.kind,
    value: input.value,
  };
}

export type Literal = Literal_Array | Literal_Boolean | Literal_Double | Literal_Integer | Literal_Null | Literal_Object | Literal_String;

export interface Module {
  decls: sys_types.Map<Ident, Decl>;
  imports: Import[];
  name: ModuleName;
}

export function makeModule(
  input: {
    decls: sys_types.Map<Ident, Decl>,
    imports: Import[],
    name: ModuleName,
  }
): Module {
  return {
    decls: input.decls,
    imports: input.imports,
    name: input.name,
  };
}

export type ModuleName = string;

export interface NewType {
  default: sys_types.Maybe<Literal>;
  typeExpr: TypeExpr;
  typeParams: Ident[];
}

export function makeNewType(
  input: {
    default: sys_types.Maybe<Literal>,
    typeExpr: TypeExpr,
    typeParams: Ident[],
  }
): NewType {
  return {
    default: input.default,
    typeExpr: input.typeExpr,
    typeParams: input.typeParams,
  };
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

export interface Struct {
  fields: Field[];
  typeParams: Ident[];
}

export function makeStruct(
  input: {
    fields: Field[],
    typeParams: Ident[],
  }
): Struct {
  return {
    fields: input.fields,
    typeParams: input.typeParams,
  };
}

export interface TypeDef {
  typeExpr: TypeExpr;
  typeParams: Ident[];
}

export function makeTypeDef(
  input: {
    typeExpr: TypeExpr,
    typeParams: Ident[],
  }
): TypeDef {
  return {
    typeExpr: input.typeExpr,
    typeParams: input.typeParams,
  };
}

export interface TypeExpr {
  parameters: TypeExpr[];
  typeRef: TypeRef;
}

export function makeTypeExpr(
  input: {
    parameters: TypeExpr[],
    typeRef: TypeRef,
  }
): TypeExpr {
  return {
    parameters: input.parameters,
    typeRef: input.typeRef,
  };
}

interface TypeRef_Primitive {
  kind: 'primitive';
  value: Ident;
}
interface TypeRef_Reference {
  kind: 'reference';
  value: ScopedName;
}
interface TypeRef_TypeParam {
  kind: 'typeParam';
  value: Ident;
}

export function makeTypeRef_Primitive(
  input: {
    kind: 'primitive',
    value: Ident,
  }
): TypeRef_Primitive {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeTypeRef_Reference(
  input: {
    kind: 'reference',
    value: ScopedName,
  }
): TypeRef_Reference {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makeTypeRef_TypeParam(
  input: {
    kind: 'typeParam',
    value: Ident,
  }
): TypeRef_TypeParam {
  return {
    kind: input.kind,
    value: input.value,
  };
}

export type TypeRef = TypeRef_Primitive | TypeRef_Reference | TypeRef_TypeParam;

export interface Union {
  fields: Field[];
  typeParams: Ident[];
}

export function makeUnion(
  input: {
    fields: Field[],
    typeParams: Ident[],
  }
): Union {
  return {
    fields: input.fields,
    typeParams: input.typeParams,
  };
}
