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
  {"decl":{"annotations":[],"name":"Dynamic","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"typeExpr","serializedName":"typeExpr","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"value","serializedName":"value","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Json"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"sys.dynamic"};

export const snDynamic: ADL.ScopedName = {moduleName:"sys.dynamic", name:"Dynamic"};

export function texprDynamic(): ADL.ATypeExpr<Dynamic> {
  return {value : {typeRef : {kind: "reference", value : snDynamic}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.dynamic.Dynamic" : Dynamic_AST
};
