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
  {"decl":{"annotations":[{"key":{"moduleName":"test23","name":"X"},"value":null}],"name":"S1","type_":{"kind":"struct_","value":{"fields":[{"annotations":[{"key":{"moduleName":"test23","name":"X"},"value":null}],"default":{"kind":"nothing"},"name":"field","serializedName":"field","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test23"};

export const snS1: ADL.ScopedName = {moduleName:"test23", name:"S1"};

export function texprS1(): ADL.ATypeExpr<S1> {
  return {value : {typeRef : {kind: "reference", value : snS1}, parameters : []}};
}

export type X = null;

const X_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"X","type_":{"kind":"type_","value":{"typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}},"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test23"};

export const snX: ADL.ScopedName = {moduleName:"test23", name:"X"};

export function texprX(): ADL.ATypeExpr<X> {
  return {value : {typeRef : {kind: "reference", value : snX}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test23.S1" : S1_AST,
  "test23.X" : X_AST
};
