/* @generated from adl module test7 */

import * as ADL from './runtime/adl';

export interface Point<T> {
  x: T;
  y: T;
}

export function makePoint<T>(
  input: {
    x: T,
    y: T,
  }
): Point<T> {
  return {
    x: input.x,
    y: input.y,
  };
}

const Point_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Point","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"x","serializedName":"x","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"y","serializedName":"y","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snPoint: ADL.ScopedName = {moduleName:"test7", name:"Point"};

export function texprPoint<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Point<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "Point"}}, parameters : [texprT.value]}};
}

export type Int1 = number;

const Int1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Int1","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snInt1: ADL.ScopedName = {moduleName:"test7", name:"Int1"};

export function texprInt1(): ADL.ATypeExpr<Int1> {
  return {value : {typeRef : {kind: "reference", value : snInt1}, parameters : []}};
}

export type Int2 = number;

const Int2_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Int2","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snInt2: ADL.ScopedName = {moduleName:"test7", name:"Int2"};

export function texprInt2(): ADL.ATypeExpr<Int2> {
  return {value : {typeRef : {kind: "reference", value : snInt2}, parameters : []}};
}

export type Int3 = number;

const Int3_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Int3","type_":{"kind":"newtype_","value":{"default":{"kind":"just","value":42},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snInt3: ADL.ScopedName = {moduleName:"test7", name:"Int3"};

export function texprInt3(): ADL.ATypeExpr<Int3> {
  return {value : {typeRef : {kind: "reference", value : snInt3}, parameters : []}};
}

export type Int4<_X> = number;

const Int4_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Int4","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snInt4: ADL.ScopedName = {moduleName:"test7", name:"Int4"};

export function texprInt4<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<Int4<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "Int4"}}, parameters : [texprX.value]}};
}

export type Int5<_X> = number;

const Int5_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Int5","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snInt5: ADL.ScopedName = {moduleName:"test7", name:"Int5"};

export function texprInt5<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<Int5<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "Int5"}}, parameters : [texprX.value]}};
}

export type Int6<_X> = number;

const Int6_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Int6","type_":{"kind":"newtype_","value":{"default":{"kind":"just","value":43},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snInt6: ADL.ScopedName = {moduleName:"test7", name:"Int6"};

export function texprInt6<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<Int6<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "Int6"}}, parameters : [texprX.value]}};
}

export type String1 = string;

const String1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"String1","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snString1: ADL.ScopedName = {moduleName:"test7", name:"String1"};

export function texprString1(): ADL.ATypeExpr<String1> {
  return {value : {typeRef : {kind: "reference", value : snString1}, parameters : []}};
}

export type String2 = string;

const String2_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"String2","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snString2: ADL.ScopedName = {moduleName:"test7", name:"String2"};

export function texprString2(): ADL.ATypeExpr<String2> {
  return {value : {typeRef : {kind: "reference", value : snString2}, parameters : []}};
}

export type String3 = string;

const String3_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"String3","type_":{"kind":"newtype_","value":{"default":{"kind":"just","value":"hello"},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snString3: ADL.ScopedName = {moduleName:"test7", name:"String3"};

export function texprString3(): ADL.ATypeExpr<String3> {
  return {value : {typeRef : {kind: "reference", value : snString3}, parameters : []}};
}

export type String4<_X> = string;

const String4_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"String4","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snString4: ADL.ScopedName = {moduleName:"test7", name:"String4"};

export function texprString4<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<String4<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "String4"}}, parameters : [texprX.value]}};
}

export type String5<_X> = string;

const String5_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"String5","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snString5: ADL.ScopedName = {moduleName:"test7", name:"String5"};

export function texprString5<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<String5<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "String5"}}, parameters : [texprX.value]}};
}

export type String6<_X> = string;

const String6_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"String6","type_":{"kind":"newtype_","value":{"default":{"kind":"just","value":"goodbye"},"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snString6: ADL.ScopedName = {moduleName:"test7", name:"String6"};

export function texprString6<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<String6<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "String6"}}, parameters : [texprX.value]}};
}

export type IntPoint1 = Point<number>;

const IntPoint1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"IntPoint1","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"Point"}}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snIntPoint1: ADL.ScopedName = {moduleName:"test7", name:"IntPoint1"};

export function texprIntPoint1(): ADL.ATypeExpr<IntPoint1> {
  return {value : {typeRef : {kind: "reference", value : snIntPoint1}, parameters : []}};
}

export type IntPoint2 = Point<number>;

const IntPoint2_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"IntPoint2","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"Point"}}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snIntPoint2: ADL.ScopedName = {moduleName:"test7", name:"IntPoint2"};

export function texprIntPoint2(): ADL.ATypeExpr<IntPoint2> {
  return {value : {typeRef : {kind: "reference", value : snIntPoint2}, parameters : []}};
}

export type IntPoint3 = Point<number>;

const IntPoint3_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"IntPoint3","type_":{"kind":"newtype_","value":{"default":{"kind":"just","value":{"x":5,"y":27}},"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"Point"}}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snIntPoint3: ADL.ScopedName = {moduleName:"test7", name:"IntPoint3"};

export function texprIntPoint3(): ADL.ATypeExpr<IntPoint3> {
  return {value : {typeRef : {kind: "reference", value : snIntPoint3}, parameters : []}};
}

export type Point1<X> = Point<X>;

const Point1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Point1","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"X"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"Point"}}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snPoint1: ADL.ScopedName = {moduleName:"test7", name:"Point1"};

export function texprPoint1<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<Point1<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "Point1"}}, parameters : [texprX.value]}};
}

export type Point2<X> = Point<X>;

const Point2_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Point2","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"X"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"Point"}}},"typeParams":["X"]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snPoint2: ADL.ScopedName = {moduleName:"test7", name:"Point2"};

export function texprPoint2<X>(texprX : ADL.ATypeExpr<X>): ADL.ATypeExpr<Point2<X>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test7",name : "Point2"}}, parameters : [texprX.value]}};
}

export type IntPoint1A = IntPoint1;

const IntPoint1A_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"IntPoint1A","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"IntPoint1"}}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snIntPoint1A: ADL.ScopedName = {moduleName:"test7", name:"IntPoint1A"};

export function texprIntPoint1A(): ADL.ATypeExpr<IntPoint1A> {
  return {value : {typeRef : {kind: "reference", value : snIntPoint1A}, parameters : []}};
}

export interface S {
  f1: IntPoint1A;
}

export function makeS(
  input: {
    f1: IntPoint1A,
  }
): S {
  return {
    f1: input.f1,
  };
}

const S_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"test7","name":"IntPoint1A"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test7"};

export const snS: ADL.ScopedName = {moduleName:"test7", name:"S"};

export function texprS(): ADL.ATypeExpr<S> {
  return {value : {typeRef : {kind: "reference", value : snS}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test7.Point" : Point_AST,
  "test7.Int1" : Int1_AST,
  "test7.Int2" : Int2_AST,
  "test7.Int3" : Int3_AST,
  "test7.Int4" : Int4_AST,
  "test7.Int5" : Int5_AST,
  "test7.Int6" : Int6_AST,
  "test7.String1" : String1_AST,
  "test7.String2" : String2_AST,
  "test7.String3" : String3_AST,
  "test7.String4" : String4_AST,
  "test7.String5" : String5_AST,
  "test7.String6" : String6_AST,
  "test7.IntPoint1" : IntPoint1_AST,
  "test7.IntPoint2" : IntPoint2_AST,
  "test7.IntPoint3" : IntPoint3_AST,
  "test7.Point1" : Point1_AST,
  "test7.Point2" : Point2_AST,
  "test7.IntPoint1A" : IntPoint1A_AST,
  "test7.S" : S_AST
};
