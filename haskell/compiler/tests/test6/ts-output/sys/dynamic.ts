/* @generated from adl module sys.dynamic */

import * as ADL from './../runtime/adl';
import * as sys_adlast from './adlast';

/**
 * A serialised value along with  its type
 */
export interface Dynamic {
  typeExpr: sys_adlast.TypeExpr;
  value: {}|null;
}

export function makeDynamic(
  input: {
    typeExpr: sys_adlast.TypeExpr,
    value: {}|null,
  }
): Dynamic {
  return {
    typeExpr: input.typeExpr,
    value: input.value,
  };
}

const Dynamic_AST : ADL.ScopedDecl =
  {"moduleName":"sys.dynamic","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}},"parameters":[]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}}]}},"name":"Dynamic","version":{"kind":"nothing"}}};

export const snDynamic: ADL.ScopedName = {moduleName:"sys.dynamic", name:"Dynamic"};

export function texprDynamic(): ADL.ATypeExpr<Dynamic> {
  return {value : {typeRef : {kind: "reference", value : snDynamic}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.dynamic.Dynamic" : Dynamic_AST
};
