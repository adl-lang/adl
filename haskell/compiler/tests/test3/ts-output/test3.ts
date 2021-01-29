/* @generated from adl module test3 */

import * as ADL from './runtime/adl';

export interface A {
  f_int: number;
  f_string: string;
  f_bool: boolean;
}

export function makeA(
  input: {
    f_int: number,
    f_string: string,
    f_bool?: boolean,
  }
): A {
  return {
    f_int: input.f_int,
    f_string: input.f_string,
    f_bool: input.f_bool === undefined ? false : input.f_bool,
  };
}

const A_AST : ADL.ScopedDecl =
  {"moduleName":"test3","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"f_int","default":{"kind":"nothing"},"name":"f_int","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}},{"annotations":[],"serializedName":"f_string","default":{"kind":"nothing"},"name":"f_string","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"f_bool","default":{"kind":"just","value":false},"name":"f_bool","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"A","version":{"kind":"nothing"}}};

export const snA: ADL.ScopedName = {moduleName:"test3", name:"A"};

export function texprA(): ADL.ATypeExpr<A> {
  return {value : {typeRef : {kind: "reference", value : snA}, parameters : []}};
}

export interface XY<T> {
  x: T;
  y: T;
}

export function makeXY<T>(
  input: {
    x: T,
    y: T,
  }
): XY<T> {
  return {
    x: input.x,
    y: input.y,
  };
}

const XY_AST : ADL.ScopedDecl =
  {"moduleName":"test3","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"x","default":{"kind":"nothing"},"name":"x","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"y","default":{"kind":"nothing"},"name":"y","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"XY","version":{"kind":"nothing"}}};

export const snXY: ADL.ScopedName = {moduleName:"test3", name:"XY"};

export function texprXY<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<XY<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test3",name : "XY"}}, parameters : [texprT.value]}};
}

export interface B<T> {
  f_t: T;
  f_string: string;
  f_tvec: T[];
  f_xy: XY<T>;
}

export function makeB<T>(
  input: {
    f_t: T,
    f_string: string,
    f_tvec: T[],
    f_xy: XY<T>,
  }
): B<T> {
  return {
    f_t: input.f_t,
    f_string: input.f_string,
    f_tvec: input.f_tvec,
    f_xy: input.f_xy,
  };
}

const B_AST : ADL.ScopedDecl =
  {"moduleName":"test3","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"f_t","default":{"kind":"nothing"},"name":"f_t","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"f_string","default":{"kind":"nothing"},"name":"f_string","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"f_tvec","default":{"kind":"nothing"},"name":"f_tvec","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"f_xy","default":{"kind":"nothing"},"name":"f_xy","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test3","name":"XY"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}]}},"name":"B","version":{"kind":"nothing"}}};

export const snB: ADL.ScopedName = {moduleName:"test3", name:"B"};

export function texprB<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<B<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test3",name : "B"}}, parameters : [texprT.value]}};
}

export interface U_F_int {
  kind: 'f_int';
  value: number;
}
export interface U_F_string {
  kind: 'f_string';
  value: string;
}
export interface U_F_void {
  kind: 'f_void';
}

export type U = U_F_int | U_F_string | U_F_void;

export interface UOpts {
  f_int: number;
  f_string: string;
  f_void: null;
}

export function makeU<K extends keyof UOpts>(kind: K, value: UOpts[K]) { return {kind, value}; }

const U_AST : ADL.ScopedDecl =
  {"moduleName":"test3","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"f_int","default":{"kind":"nothing"},"name":"f_int","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}},{"annotations":[],"serializedName":"f_string","default":{"kind":"nothing"},"name":"f_string","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"f_void","default":{"kind":"nothing"},"name":"f_void","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"U","version":{"kind":"nothing"}}};

export const snU: ADL.ScopedName = {moduleName:"test3", name:"U"};

export function texprU(): ADL.ATypeExpr<U> {
  return {value : {typeRef : {kind: "reference", value : snU}, parameters : []}};
}

export type E = 'v1' | 'v2';
export const valuesE : E[] = ['v1', 'v2'];

const E_AST : ADL.ScopedDecl =
  {"moduleName":"test3","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v1","default":{"kind":"nothing"},"name":"v1","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"v2","default":{"kind":"nothing"},"name":"v2","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"E","version":{"kind":"nothing"}}};

export const snE: ADL.ScopedName = {moduleName:"test3", name:"E"};

export function texprE(): ADL.ATypeExpr<E> {
  return {value : {typeRef : {kind: "reference", value : snE}, parameters : []}};
}

export interface S<T> {
  f_void: null;
  f_bool: boolean;
  f_int8: number;
  f_int16: number;
  f_int32: number;
  f_int64: number;
  f_word8: number;
  f_word16: number;
  f_word32: number;
  f_word64: number;
  f_float: number;
  f_double: number;
  f_bytes: Uint8Array;
  f_string: string;
  f_vstring: string[];
  f_a: A;
  f_u: U;
  f_u1: U;
  f_e: E;
  f_t: T;
  f_bint16: B<number>;
  f_smap: {[key: string]: number};
  f_json1: {}|null;
  f_json2: {}|null;
}

export function makeS<T>(
  input: {
    f_void?: null,
    f_bool?: boolean,
    f_int8?: number,
    f_int16?: number,
    f_int32?: number,
    f_int64?: number,
    f_word8?: number,
    f_word16?: number,
    f_word32?: number,
    f_word64?: number,
    f_float?: number,
    f_double?: number,
    f_bytes?: Uint8Array,
    f_string?: string,
    f_vstring?: string[],
    f_a?: A,
    f_u?: U,
    f_u1?: U,
    f_e?: E,
    f_t: T,
    f_bint16?: B<number>,
    f_smap?: {[key: string]: number},
    f_json1?: {}|null,
    f_json2?: {}|null,
  }
): S<T> {
  return {
    f_void: input.f_void === undefined ? null : input.f_void,
    f_bool: input.f_bool === undefined ? true : input.f_bool,
    f_int8: input.f_int8 === undefined ? -5 : input.f_int8,
    f_int16: input.f_int16 === undefined ? -10000 : input.f_int16,
    f_int32: input.f_int32 === undefined ? 56 : input.f_int32,
    f_int64: input.f_int64 === undefined ? 40000 : input.f_int64,
    f_word8: input.f_word8 === undefined ? 32 : input.f_word8,
    f_word16: input.f_word16 === undefined ? 50000 : input.f_word16,
    f_word32: input.f_word32 === undefined ? 124456 : input.f_word32,
    f_word64: input.f_word64 === undefined ? 2344 : input.f_word64,
    f_float: input.f_float === undefined ? 0.5 : input.f_float,
    f_double: input.f_double === undefined ? 0.45 : input.f_double,
    f_bytes: input.f_bytes === undefined ? b64.toByteArray("aGVsbG8=") : input.f_bytes,
    f_string: input.f_string === undefined ? "abcd" : input.f_string,
    f_vstring: input.f_vstring === undefined ? ["xy", "ab"] : input.f_vstring,
    f_a: input.f_a === undefined ? {f_int : 0, f_string : "xyz", f_bool : false} : input.f_a,
    f_u: input.f_u === undefined ? {kind : "f_int", value : 45} : input.f_u,
    f_u1: input.f_u1 === undefined ? {kind : "f_void"} : input.f_u1,
    f_e: input.f_e === undefined ? "v2" : input.f_e,
    f_t: input.f_t,
    f_bint16: input.f_bint16 === undefined ? {f_t : 56, f_string : "yikes", f_tvec : [1, 2, 3], f_xy : {x : 5, y : 5}} : input.f_bint16,
    f_smap: input.f_smap === undefined ? {"a" : 45, "b" : 47} : input.f_smap,
    f_json1: input.f_json1 === undefined ? null : input.f_json1,
    f_json2: input.f_json2 === undefined ? [{"v1":27,"v2":"abcde"},true] : input.f_json2,
  };
}

const S_AST : ADL.ScopedDecl =
  {"moduleName":"test3","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"f_void","default":{"kind":"just","value":null},"name":"f_void","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"f_bool","default":{"kind":"just","value":true},"name":"f_bool","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"f_int8","default":{"kind":"just","value":-5},"name":"f_int8","typeExpr":{"typeRef":{"kind":"primitive","value":"Int8"},"parameters":[]}},{"annotations":[],"serializedName":"f_int16","default":{"kind":"just","value":-10000},"name":"f_int16","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}},{"annotations":[],"serializedName":"f_int32","default":{"kind":"just","value":56},"name":"f_int32","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"f_int64","default":{"kind":"just","value":40000},"name":"f_int64","typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}},{"annotations":[],"serializedName":"f_word8","default":{"kind":"just","value":32},"name":"f_word8","typeExpr":{"typeRef":{"kind":"primitive","value":"Word8"},"parameters":[]}},{"annotations":[],"serializedName":"f_word16","default":{"kind":"just","value":50000},"name":"f_word16","typeExpr":{"typeRef":{"kind":"primitive","value":"Word16"},"parameters":[]}},{"annotations":[],"serializedName":"f_word32","default":{"kind":"just","value":124456},"name":"f_word32","typeExpr":{"typeRef":{"kind":"primitive","value":"Word32"},"parameters":[]}},{"annotations":[],"serializedName":"f_word64","default":{"kind":"just","value":2344},"name":"f_word64","typeExpr":{"typeRef":{"kind":"primitive","value":"Word64"},"parameters":[]}},{"annotations":[],"serializedName":"f_float","default":{"kind":"just","value":0.5},"name":"f_float","typeExpr":{"typeRef":{"kind":"primitive","value":"Float"},"parameters":[]}},{"annotations":[],"serializedName":"f_double","default":{"kind":"just","value":0.45},"name":"f_double","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}},{"annotations":[],"serializedName":"f_bytes","default":{"kind":"just","value":"aGVsbG8="},"name":"f_bytes","typeExpr":{"typeRef":{"kind":"primitive","value":"Bytes"},"parameters":[]}},{"annotations":[],"serializedName":"f_string","default":{"kind":"just","value":"abcd"},"name":"f_string","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"f_vstring","default":{"kind":"just","value":["xy","ab"]},"name":"f_vstring","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"f_a","default":{"kind":"just","value":{"f_string":"xyz","f_int":0}},"name":"f_a","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test3","name":"A"}},"parameters":[]}},{"annotations":[],"serializedName":"f_u","default":{"kind":"just","value":{"f_int":45}},"name":"f_u","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test3","name":"U"}},"parameters":[]}},{"annotations":[],"serializedName":"f_u1","default":{"kind":"just","value":"f_void"},"name":"f_u1","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test3","name":"U"}},"parameters":[]}},{"annotations":[],"serializedName":"f_e","default":{"kind":"just","value":"v2"},"name":"f_e","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test3","name":"E"}},"parameters":[]}},{"annotations":[],"serializedName":"f_t","default":{"kind":"nothing"},"name":"f_t","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"f_bint16","default":{"kind":"just","value":{"f_string":"yikes","f_t":56,"f_tvec":[1,2,3],"f_xy":{"x":5,"y":5}}},"name":"f_bint16","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test3","name":"B"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}]}},{"annotations":[],"serializedName":"f_smap","default":{"kind":"just","value":{"a":45,"b":47}},"name":"f_smap","typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}]}},{"annotations":[],"serializedName":"f_json1","default":{"kind":"just","value":null},"name":"f_json1","typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}},{"annotations":[],"serializedName":"f_json2","default":{"kind":"just","value":[{"v1":27,"v2":"abcde"},true]},"name":"f_json2","typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}}]}},"name":"S","version":{"kind":"nothing"}}};

export const snS: ADL.ScopedName = {moduleName:"test3", name:"S"};

export function texprS<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<S<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test3",name : "S"}}, parameters : [texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test3.A" : A_AST,
  "test3.XY" : XY_AST,
  "test3.B" : B_AST,
  "test3.U" : U_AST,
  "test3.E" : E_AST,
  "test3.S" : S_AST
};
