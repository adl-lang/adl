/* @generated from adl module sys.dynamic */

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
