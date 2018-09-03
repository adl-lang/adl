/* @generated from adl module test2 */

import * as ADL from './runtime/adl';

/**
 * An empty structure.
 */
export interface S0 {
}

export function makeS0(
  input: {
  }
): S0 {
  return {
  };
}

const S0_AST : ADL.ScopedDecl =
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[]}},"name":"S0","version":{"kind":"nothing"}}};

export function texprS0(): ADL.ATypeExpr<S0> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "S0"}}, parameters : []}};
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
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"x","default":{"kind":"nothing"},"name":"x","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"y","default":{"kind":"nothing"},"name":"y","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"S1","version":{"kind":"nothing"}}};

export function texprS1(): ADL.ATypeExpr<S1> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "S1"}}, parameters : []}};
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
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"f1","default":{"kind":"nothing"},"name":"f1","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"f2","default":{"kind":"nothing"},"name":"f2","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}},{"annotations":[],"serializedName":"f3","default":{"kind":"nothing"},"name":"f3","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}]}}]}},"name":"S2","version":{"kind":"nothing"}}};

export function texprS2(): ADL.ATypeExpr<S2> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "S2"}}, parameters : []}};
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
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"f1","default":{"kind":"nothing"},"name":"f1","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"f2","default":{"kind":"nothing"},"name":"f2","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}},{"annotations":[],"serializedName":"f3","default":{"kind":"nothing"},"name":"f3","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"f4","default":{"kind":"nothing"},"name":"f4","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}]}},"name":"S3","version":{"kind":"nothing"}}};

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
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"f1","default":{"kind":"nothing"},"name":"f1","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"S3"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"f2","default":{"kind":"nothing"},"name":"f2","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"S3"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}]}},"name":"S4","version":{"kind":"nothing"}}};

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
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"children","default":{"kind":"nothing"},"name":"children","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"Tree"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}]}}]}},"name":"Tree","version":{"kind":"nothing"}}};

export function texprTree<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Tree<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "Tree"}}, parameters : [texprT.value]}};
}

export type IntTree = Tree<number>;

const IntTree_AST : ADL.ScopedDecl =
  {"moduleName":"test2","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test2","name":"Tree"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}]}}},"name":"IntTree","version":{"kind":"nothing"}}};

export function texprIntTree(): ADL.ATypeExpr<IntTree> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test2",name : "IntTree"}}, parameters : []}};
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
