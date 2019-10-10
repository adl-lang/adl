/* @generated from adl module test5 */

import * as ADL from './runtime/adl';

export enum U1 {
  v,
}

const U1_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"nothing"},"name":"v","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"U1","version":{"kind":"nothing"}}};

export const snU1: ADL.ScopedName = {moduleName:"test5", name:"U1"};

export function texprU1(): ADL.ATypeExpr<U1> {
  return {value : {typeRef : {kind: "reference", value : snU1}, parameters : []}};
}

export interface U2_V {
  kind: 'v';
  value: number;
}

export type U2 = U2_V;

const U2_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"nothing"},"name":"v","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}}]}},"name":"U2","version":{"kind":"nothing"}}};

export const snU2: ADL.ScopedName = {moduleName:"test5", name:"U2"};

export function texprU2(): ADL.ATypeExpr<U2> {
  return {value : {typeRef : {kind: "reference", value : snU2}, parameters : []}};
}

export interface U3_V {
  kind: 'v';
  value: number;
}

export type U3 = U3_V;

const U3_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"just","value":100},"name":"v","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}}]}},"name":"U3","version":{"kind":"nothing"}}};

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
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"f","default":{"kind":"just","value":100},"name":"f","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}}]}},"name":"S1","version":{"kind":"nothing"}}};

export const snS1: ADL.ScopedName = {moduleName:"test5", name:"S1"};

export function texprS1(): ADL.ATypeExpr<S1> {
  return {value : {typeRef : {kind: "reference", value : snS1}, parameters : []}};
}

export interface U4_V {
  kind: 'v';
  value: S1;
}

export type U4 = U4_V;

const U4_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"nothing"},"name":"v","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"S1"}},"parameters":[]}}]}},"name":"U4","version":{"kind":"nothing"}}};

export const snU4: ADL.ScopedName = {moduleName:"test5", name:"U4"};

export function texprU4(): ADL.ATypeExpr<U4> {
  return {value : {typeRef : {kind: "reference", value : snU4}, parameters : []}};
}

export interface U5_V {
  kind: 'v';
  value: S1;
}

export type U5 = U5_V;

const U5_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"just","value":{"f":200}},"name":"v","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"S1"}},"parameters":[]}}]}},"name":"U5","version":{"kind":"nothing"}}};

export const snU5: ADL.ScopedName = {moduleName:"test5", name:"U5"};

export function texprU5(): ADL.ATypeExpr<U5> {
  return {value : {typeRef : {kind: "reference", value : snU5}, parameters : []}};
}

export interface U6_V {
  kind: 'v';
  value: U3;
}

export type U6 = U6_V;

const U6_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"nothing"},"name":"v","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U3"}},"parameters":[]}}]}},"name":"U6","version":{"kind":"nothing"}}};

export const snU6: ADL.ScopedName = {moduleName:"test5", name:"U6"};

export function texprU6(): ADL.ATypeExpr<U6> {
  return {value : {typeRef : {kind: "reference", value : snU6}, parameters : []}};
}

export interface U7_V {
  kind: 'v';
  value: U3;
}

export type U7 = U7_V;

const U7_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v","default":{"kind":"just","value":{"v":75}},"name":"v","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"U3"}},"parameters":[]}}]}},"name":"U7","version":{"kind":"nothing"}}};

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

const U8_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"v1","default":{"kind":"nothing"},"name":"v1","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"S1"}},"parameters":[]}},{"annotations":[],"serializedName":"v2","default":{"kind":"nothing"},"name":"v2","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}}]}},"name":"U8","version":{"kind":"nothing"}}};

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

export type U9<T> = U9_V1<T> | U9_V2<T>;

const U9_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"v1","default":{"kind":"nothing"},"name":"v1","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"v2","default":{"kind":"nothing"},"name":"v2","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}}]}},"name":"U9","version":{"kind":"nothing"}}};

export const snU9: ADL.ScopedName = {moduleName:"test5", name:"U9"};

export function texprU9<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<U9<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test5",name : "U9"}}, parameters : [texprT.value]}};
}

export interface List_Null<_T> {
  kind: 'null';
}
export interface List_Cell<T> {
  kind: 'cell';
  value: Cell<T>;
}

export type List<T> = List_Null<T> | List_Cell<T>;

const List_AST : ADL.ScopedDecl =
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"null","default":{"kind":"nothing"},"name":"null","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"cell","default":{"kind":"nothing"},"name":"cell","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"Cell"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}]}},"name":"List","version":{"kind":"nothing"}}};

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
  {"moduleName":"test5","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"head","default":{"kind":"nothing"},"name":"head","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"tail","default":{"kind":"nothing"},"name":"tail","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test5","name":"List"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}]}},"name":"Cell","version":{"kind":"nothing"}}};

export const snCell: ADL.ScopedName = {moduleName:"test5", name:"Cell"};

export function texprCell<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Cell<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test5",name : "Cell"}}, parameters : [texprT.value]}};
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
  "test5.List" : List_AST,
  "test5.Cell" : Cell_AST
};
