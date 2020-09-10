/* @generated from adl module test29 */

import * as ADL from './runtime/adl';

export interface Test {
  foo: {[key: string]: string};
}

export function makeTest(
  input: {
    foo?: {[key: string]: string},
  }
): Test {
  return {
    foo: input.foo === undefined ? {" " : "baz", "\"" : "baz", "$" : "bar", "'" : "baz", "degrees" : "°"} : input.foo,
  };
}

const Test_AST : ADL.ScopedDecl =
  {"moduleName":"test29","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"foo","default":{"kind":"just","value":{"'":"baz","degrees":"°"," ":"baz","$":"bar","\"":"baz"}},"name":"foo","typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"Test","version":{"kind":"nothing"}}};

export const snTest: ADL.ScopedName = {moduleName:"test29", name:"Test"};

export function texprTest(): ADL.ATypeExpr<Test> {
  return {value : {typeRef : {kind: "reference", value : snTest}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test29.Test" : Test_AST
};
