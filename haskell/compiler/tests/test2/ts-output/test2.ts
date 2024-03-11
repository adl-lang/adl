/* @generated from adl module test2 */

import * as ADL from './runtime/adl';

/**
 * An empty structure.
 */
export interface S0 {
}

export function makeS0(
  _input: {
  }
): S0 {
  return {
  };
}

const S0_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S0","type_":{"kind":"struct_","value":{"fields":[],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snS0: ADL.ScopedName = {moduleName:"test2", name:"S0"};

export function texprS0(): ADL.ATypeExpr<S0> {
  return {value : {typeRef : {kind: "reference", value : snS0}, parameters : []}};
}

/**
 * A structure containing primitives.
 * It has two fields: an integer x and a String y.
 */
export interface S1 {
  x: number;
  y: string;
}

export function makeS1(
  input: {
    x: number,
    y: string,
  }
): S1 {
  return {
    x: input.x,
    y: input.y,
  };
}

const S1_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S1","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"x","serializedName":"x","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Int32"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"y","serializedName":"y","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snS1: ADL.ScopedName = {moduleName:"test2", name:"S1"};

export function texprS1(): ADL.ATypeExpr<S1> {
  return {value : {typeRef : {kind: "reference", value : snS1}, parameters : []}};
}

/**
 * A structure containing a vector.
 */
export interface S2 {
  f1: string;
  f2: number;
  f3: number[];
}

export function makeS2(
  input: {
    f1: string,
    f2: number,
    f3: number[],
  }
): S2 {
  return {
    f1: input.f1,
    f2: input.f2,
    f3: input.f3,
  };
}

const S2_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S2","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f2","serializedName":"f2","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f3","serializedName":"f3","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int32"}}],"typeRef":{"kind":"primitive","value":"Vector"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snS2: ADL.ScopedName = {moduleName:"test2", name:"S2"};

export function texprS2(): ADL.ATypeExpr<S2> {
  return {value : {typeRef : {kind: "reference", value : snS2}, parameters : []}};
}

/**
 * A generic structure.
 */
export interface S3<T> {
  f1: string;
  f2: number;
  f3: T;
  f4: T[];
}

export function makeS3<T>(
  input: {
    f1: string,
    f2: number,
    f3: T,
    f4: T[],
  }
): S3<T> {
  return {
    f1: input.f1,
    f2: input.f2,
    f3: input.f3,
    f4: input.f4,
  };
}

const S3_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S3","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f2","serializedName":"f2","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f3","serializedName":"f3","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f4","serializedName":"f4","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"primitive","value":"Vector"}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snS3: ADL.ScopedName = {moduleName:"test2", name:"S3"};

export function texprS3<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<S3<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "S3"}}, parameters : [texprT.value]}};
}

export interface S4<T> {
  f1: S3<string>;
  f2: S3<T>;
}

export function makeS4<T>(
  input: {
    f1: S3<string>,
    f2: S3<T>,
  }
): S4<T> {
  return {
    f1: input.f1,
    f2: input.f2,
  };
}

const S4_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S4","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"f1","serializedName":"f1","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"S3"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f2","serializedName":"f2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"S3"}}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snS4: ADL.ScopedName = {moduleName:"test2", name:"S4"};

export function texprS4<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<S4<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "S4"}}, parameters : [texprT.value]}};
}

export interface Tree<T> {
  value: T;
  children: Tree<T>[];
}

export function makeTree<T>(
  input: {
    value: T,
    children: Tree<T>[],
  }
): Tree<T> {
  return {
    value: input.value,
    children: input.children,
  };
}

const Tree_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Tree","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"value","serializedName":"value","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"children","serializedName":"children","typeExpr":{"parameters":[{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"Tree"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snTree: ADL.ScopedName = {moduleName:"test2", name:"Tree"};

export function texprTree<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Tree<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "Tree"}}, parameters : [texprT.value]}};
}

export type IntTree = Tree<number>;

const IntTree_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"IntTree","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int32"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"Tree"}}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test2"};

export const snIntTree: ADL.ScopedName = {moduleName:"test2", name:"IntTree"};

export function texprIntTree(): ADL.ATypeExpr<IntTree> {
  return {value : {typeRef : {kind: "reference", value : snIntTree}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test2.S0" : S0_AST,
  "test2.S1" : S1_AST,
  "test2.S2" : S2_AST,
  "test2.S3" : S3_AST,
  "test2.S4" : S4_AST,
  "test2.Tree" : Tree_AST,
  "test2.IntTree" : IntTree_AST
};
