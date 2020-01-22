/* @generated from adl module test23 */

import * as ADL from './runtime/adl';

/**
 * A docstring
 */
export interface S1 {
  /**
   * Another docstring
   */
  field: string;
}

export function makeS1(
  input: {
    field: string,
  }
): S1 {
  return {
    field: input.field,
  };
}

const S1_AST : ADL.ScopedDecl =
  {"moduleName":"test23","decl":{"annotations":[{"v1":{"moduleName":"test23","name":"X"},"v2":null}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[{"v1":{"moduleName":"test23","name":"X"},"v2":null}],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"S1","version":{"kind":"nothing"}}};

export const snS1: ADL.ScopedName = {moduleName:"test23", name:"S1"};

export function texprS1(): ADL.ATypeExpr<S1> {
  return {value : {typeRef : {kind: "reference", value : snS1}, parameters : []}};
}

export type X = null;

const X_AST : ADL.ScopedDecl =
  {"moduleName":"test23","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"X","version":{"kind":"nothing"}}};

export const snX: ADL.ScopedName = {moduleName:"test23", name:"X"};

export function texprX(): ADL.ATypeExpr<X> {
  return {value : {typeRef : {kind: "reference", value : snX}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test23.S1" : S1_AST,
  "test23.X" : X_AST
};
