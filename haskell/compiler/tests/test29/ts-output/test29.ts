/* @generated from adl module test29 */

import * as ADL from './runtime/adl';

/**
 * An example with weird "quoting" conventions, designed to break things
 */
export interface Test {
  /**
   * "foo" as a field
   */
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
  {"decl":{"annotations":[{"key":{"moduleName":"sys.annotations","name":"Doc"},"value":"An example with weird \"quoting\" conventions, designed to break things\n"}],"name":"Test","type_":{"kind":"struct_","value":{"fields":[{"annotations":[{"key":{"moduleName":"sys.annotations","name":"Doc"},"value":"\"foo\" as a field\n"}],"default":{"kind":"just","value":{" ":"baz","\"":"baz","$":"bar","'":"baz","degrees":"°"}},"name":"foo","serializedName":"foo","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"primitive","value":"StringMap"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test29"};

export const snTest: ADL.ScopedName = {moduleName:"test29", name:"Test"};

export function texprTest(): ADL.ATypeExpr<Test> {
  return {value : {typeRef : {kind: "reference", value : snTest}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test29.Test" : Test_AST
};
