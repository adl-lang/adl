/* @generated from adl module test5 */

import * as ADL from './runtime/adl';

export type U1 = 'v';
export const valuesU1 : U1[] = ['v'];

const U1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U1","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU1: ADL.ScopedName = {moduleName:"test5", name:"U1"};

export function texprU1(): ADL.ATypeExpr<U1> {
  return {value : {typeRef : {kind: "reference", value : snU1}, parameters : []}};
}

export interface U2_V {
  kind: 'v';
  value: number;
}

export type U2 = U2_V;

export interface U2Opts {
  v: number;
}

export function makeU2<K extends keyof U2Opts>(kind: K, value: U2Opts[K]) { return {kind, value}; }

const U2_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U2","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU2: ADL.ScopedName = {moduleName:"test5", name:"U2"};

export function texprU2(): ADL.ATypeExpr<U2> {
  return {value : {typeRef : {kind: "reference", value : snU2}, parameters : []}};
}

export interface U3_V {
  kind: 'v';
  value: number;
}

export type U3 = U3_V;

export interface U3Opts {
  v: number;
}

export function makeU3<K extends keyof U3Opts>(kind: K, value: U3Opts[K]) { return {kind, value}; }

const U3_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U3","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":100},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU3: ADL.ScopedName = {moduleName:"test5", name:"U3"};

export function texprU3(): ADL.ATypeExpr<U3> {
  return {value : {typeRef : {kind: "reference", value : snU3}, parameters : []}};
}

export interface S1 {
  f: number;
}

export function makeS1(
  input: {
    f?: number,
  }
): S1 {
  return {
    f: input.f === undefined ? 100 : input.f,
  };
}

const S1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S1","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":100},"name":"f","serializedName":"f","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snS1: ADL.ScopedName = {moduleName:"test5", name:"S1"};

export function texprS1(): ADL.ATypeExpr<S1> {
  return {value : {typeRef : {kind: "reference", value : snS1}, parameters : []}};
}

export interface U4_V {
  kind: 'v';
  value: S1;
}

export type U4 = U4_V;

export interface U4Opts {
  v: S1;
}

export function makeU4<K extends keyof U4Opts>(kind: K, value: U4Opts[K]) { return {kind, value}; }

const U4_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U4","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"S1"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU4: ADL.ScopedName = {moduleName:"test5", name:"U4"};

export function texprU4(): ADL.ATypeExpr<U4> {
  return {value : {typeRef : {kind: "reference", value : snU4}, parameters : []}};
}

export interface U5_V {
  kind: 'v';
  value: S1;
}

export type U5 = U5_V;

export interface U5Opts {
  v: S1;
}

export function makeU5<K extends keyof U5Opts>(kind: K, value: U5Opts[K]) { return {kind, value}; }

const U5_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U5","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":{"f":200}},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"S1"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU5: ADL.ScopedName = {moduleName:"test5", name:"U5"};

export function texprU5(): ADL.ATypeExpr<U5> {
  return {value : {typeRef : {kind: "reference", value : snU5}, parameters : []}};
}

export interface U6_V {
  kind: 'v';
  value: U3;
}

export type U6 = U6_V;

export interface U6Opts {
  v: U3;
}

export function makeU6<K extends keyof U6Opts>(kind: K, value: U6Opts[K]) { return {kind, value}; }

const U6_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U6","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U3"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU6: ADL.ScopedName = {moduleName:"test5", name:"U6"};

export function texprU6(): ADL.ATypeExpr<U6> {
  return {value : {typeRef : {kind: "reference", value : snU6}, parameters : []}};
}

export interface U7_V {
  kind: 'v';
  value: U3;
}

export type U7 = U7_V;

export interface U7Opts {
  v: U3;
}

export function makeU7<K extends keyof U7Opts>(kind: K, value: U7Opts[K]) { return {kind, value}; }

const U7_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U7","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":{"v":75}},"name":"v","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U3"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU7: ADL.ScopedName = {moduleName:"test5", name:"U7"};

export function texprU7(): ADL.ATypeExpr<U7> {
  return {value : {typeRef : {kind: "reference", value : snU7}, parameters : []}};
}

export interface U8_V1 {
  kind: 'v1';
  value: S1;
}
export interface U8_V2 {
  kind: 'v2';
  value: number;
}

export type U8 = U8_V1 | U8_V2;

export interface U8Opts {
  v1: S1;
  v2: number;
}

export function makeU8<K extends keyof U8Opts>(kind: K, value: U8Opts[K]) { return {kind, value}; }

const U8_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U8","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v1","serializedName":"v1","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"S1"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"v2","serializedName":"v2","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU8: ADL.ScopedName = {moduleName:"test5", name:"U8"};

export function texprU8(): ADL.ATypeExpr<U8> {
  return {value : {typeRef : {kind: "reference", value : snU8}, parameters : []}};
}

export interface U9_V1<T> {
  kind: 'v1';
  value: T;
}
export interface U9_V2<_T> {
  kind: 'v2';
  value: number;
}
export interface U9_V3<_T> {
  kind: 'v3';
}

export type U9<T> = U9_V1<T> | U9_V2<T> | U9_V3<T>;

export interface U9Opts<T> {
  v1: T;
  v2: number;
  v3: null;
}

export function makeU9<T, K extends keyof U9Opts<T>>(kind: K, value: U9Opts<T>[K]) { return {kind, value}; }

const U9_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U9","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v1","serializedName":"v1","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"v2","serializedName":"v2","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"v3","serializedName":"v3","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU9: ADL.ScopedName = {moduleName:"test5", name:"U9"};

export function texprU9<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<U9<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test5",name : "U9"}}, parameters : [texprT.value]}};
}

export interface S {
  f1: U9<string>;
  f2: U9<string>;
  f3: U9<string>;
}

export function makeS(
  input: {
    f1?: U9<string>,
    f2?: U9<string>,
    f3?: U9<string>,
  }
): S {
  return {
    f1: input.f1 === undefined ? {kind : "v1", value : "xx"} : input.f1,
    f2: input.f2 === undefined ? {kind : "v2", value : 100} : input.f2,
    f3: input.f3 === undefined ? {kind : "v3"} : input.f3,
  };
}

const S_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":{"v1":"xx"}},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U9"}}}},{"annotations":[],"default":{"kind":"just","value":{"v2":100}},"name":"f2","serializedName":"f2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U9"}}}},{"annotations":[],"default":{"kind":"just","value":"v3"},"name":"f3","serializedName":"f3","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U9"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snS: ADL.ScopedName = {moduleName:"test5", name:"S"};

export function texprS(): ADL.ATypeExpr<S> {
  return {value : {typeRef : {kind: "reference", value : snS}, parameters : []}};
}

export interface List_Null<_T> {
  kind: 'null';
}
export interface List_Cell<T> {
  kind: 'cell';
  value: Cell<T>;
}

export type List<T> = List_Null<T> | List_Cell<T>;

export interface ListOpts<T> {
  null: null;
  cell: Cell<T>;
}

export function makeList<T, K extends keyof ListOpts<T>>(kind: K, value: ListOpts<T>[K]) { return {kind, value}; }

const List_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"List","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"null","serializedName":"null","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"cell","serializedName":"cell","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"Cell"}}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snList: ADL.ScopedName = {moduleName:"test5", name:"List"};

export function texprList<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<List<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test5",name : "List"}}, parameters : [texprT.value]}};
}

export interface Cell<T> {
  head: T;
  tail: List<T>;
}

export function makeCell<T>(
  input: {
    head: T,
    tail: List<T>,
  }
): Cell<T> {
  return {
    head: input.head,
    tail: input.tail,
  };
}

const Cell_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Cell","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"head","serializedName":"head","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"tail","serializedName":"tail","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"List"}}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snCell: ADL.ScopedName = {moduleName:"test5", name:"Cell"};

export function texprCell<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Cell<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test5",name : "Cell"}}, parameters : [texprT.value]}};
}

export interface U10_V1 {
  kind: 'v1';
  value: number;
}
export interface U10_V2 {
  kind: 'v2';
}

export type U10 = U10_V1 | U10_V2;

export interface U10Opts {
  v1: number;
  v2: null;
}

export function makeU10<K extends keyof U10Opts>(kind: K, value: U10Opts[K]) { return {kind, value}; }

const U10_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U10","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v1","serializedName":"v1","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"v2","serializedName":"v2","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU10: ADL.ScopedName = {moduleName:"test5", name:"U10"};

export function texprU10(): ADL.ATypeExpr<U10> {
  return {value : {typeRef : {kind: "reference", value : snU10}, parameters : []}};
}

export interface S10 {
  f1: U10;
  f2: (U10|null);
  f3: U10;
  f4: (U10|null);
}

export function makeS10(
  input: {
    f1?: U10,
    f2?: (U10|null),
    f3?: U10,
    f4?: (U10|null),
  }
): S10 {
  return {
    f1: input.f1 === undefined ? {kind : "v2"} : input.f1,
    f2: input.f2 === undefined ? {kind : "v2"} : input.f2,
    f3: input.f3 === undefined ? {kind : "v1", value : 17} : input.f3,
    f4: input.f4 === undefined ? {kind : "v1", value : 17} : input.f4,
  };
}

const S10_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S10","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":"v2"},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U10"}}}},{"annotations":[],"default":{"kind":"just","value":"v2"},"name":"f2","serializedName":"f2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U10"}}}],"typeRef":{"kind":"primitive","value":"Nullable"}}},{"annotations":[],"default":{"kind":"just","value":{"v1":17}},"name":"f3","serializedName":"f3","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U10"}}}},{"annotations":[],"default":{"kind":"just","value":{"v1":17}},"name":"f4","serializedName":"f4","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U10"}}}],"typeRef":{"kind":"primitive","value":"Nullable"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snS10: ADL.ScopedName = {moduleName:"test5", name:"S10"};

export function texprS10(): ADL.ATypeExpr<S10> {
  return {value : {typeRef : {kind: "reference", value : snS10}, parameters : []}};
}

export interface U11_V1 {
  kind: 'v1';
  value: number;
}
export interface U11_V2 {
  kind: 'v2';
}

export type U11 = U11_V1 | U11_V2;

export interface U11Opts {
  v1: number;
  v2: null;
}

export function makeU11<K extends keyof U11Opts>(kind: K, value: U11Opts[K]) { return {kind, value}; }

const U11_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"U11","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v1","serializedName":"VALUE1","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int16"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"v2","serializedName":"VALUE2","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snU11: ADL.ScopedName = {moduleName:"test5", name:"U11"};

export function texprU11(): ADL.ATypeExpr<U11> {
  return {value : {typeRef : {kind: "reference", value : snU11}, parameters : []}};
}

export interface S11 {
  f1: U11;
  f2: (U11|null);
  f3: U11;
  f4: (U11|null);
}

export function makeS11(
  input: {
    f1?: U11,
    f2?: (U11|null),
    f3?: U11,
    f4?: (U11|null),
  }
): S11 {
  return {
    f1: input.f1 === undefined ? {kind : "v2"} : input.f1,
    f2: input.f2 === undefined ? {kind : "v2"} : input.f2,
    f3: input.f3 === undefined ? {kind : "v1", value : 17} : input.f3,
    f4: input.f4 === undefined ? {kind : "v1", value : 17} : input.f4,
  };
}

const S11_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S11","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":"VALUE2"},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U11"}}}},{"annotations":[],"default":{"kind":"just","value":"VALUE2"},"name":"f2","serializedName":"f2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U11"}}}],"typeRef":{"kind":"primitive","value":"Nullable"}}},{"annotations":[],"default":{"kind":"just","value":{"VALUE1":17}},"name":"f3","serializedName":"f3","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U11"}}}},{"annotations":[],"default":{"kind":"just","value":{"VALUE1":17}},"name":"f4","serializedName":"f4","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U11"}}}],"typeRef":{"kind":"primitive","value":"Nullable"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test5"};

export const snS11: ADL.ScopedName = {moduleName:"test5", name:"S11"};

export function texprS11(): ADL.ATypeExpr<S11> {
  return {value : {typeRef : {kind: "reference", value : snS11}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test5.U1" : U1_AST,
  "test5.U2" : U2_AST,
  "test5.U3" : U3_AST,
  "test5.S1" : S1_AST,
  "test5.U4" : U4_AST,
  "test5.U5" : U5_AST,
  "test5.U6" : U6_AST,
  "test5.U7" : U7_AST,
  "test5.U8" : U8_AST,
  "test5.U9" : U9_AST,
  "test5.S" : S_AST,
  "test5.List" : List_AST,
  "test5.Cell" : Cell_AST,
  "test5.U10" : U10_AST,
  "test5.S10" : S10_AST,
  "test5.U11" : U11_AST,
  "test5.S11" : S11_AST
};
