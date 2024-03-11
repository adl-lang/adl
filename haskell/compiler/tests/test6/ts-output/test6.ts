/* @generated from adl module test6 */

import * as ADL from './runtime/adl';
import * as sys_types from './sys/types';

export interface S {
  f_pair: sys_types.Pair<number, number>;
  f_either: sys_types.Either<string, number>;
  f_map: sys_types.Map<string, number>;
  f_set: sys_types.Set<string>;
  f_mstring: sys_types.Maybe<string>;
  f_mstring2: sys_types.Maybe<string>;
  f_nstring: (string|null);
  f_nstring2: (string|null);
  f_int: (number|null);
  f_int2: (number|null);
  f_int3: (number|null);
}

export function makeS(
  input: {
    f_pair: sys_types.Pair<number, number>,
    f_either: sys_types.Either<string, number>,
    f_map: sys_types.Map<string, number>,
    f_set: sys_types.Set<string>,
    f_mstring: sys_types.Maybe<string>,
    f_mstring2?: sys_types.Maybe<string>,
    f_nstring: (string|null),
    f_nstring2?: (string|null),
    f_int: (number|null),
    f_int2?: (number|null),
    f_int3?: (number|null),
  }
): S {
  return {
    f_pair: input.f_pair,
    f_either: input.f_either,
    f_map: input.f_map,
    f_set: input.f_set,
    f_mstring: input.f_mstring,
    f_mstring2: input.f_mstring2 === undefined ? {kind : "just", value : "sukpeepolup"} : input.f_mstring2,
    f_nstring: input.f_nstring,
    f_nstring2: input.f_nstring2 === undefined ? "abcde" : input.f_nstring2,
    f_int: input.f_int,
    f_int2: input.f_int2 === undefined ? 100 : input.f_int2,
    f_int3: input.f_int3 === undefined ? null : input.f_int3,
  };
}

const S_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"S","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"f_pair","serializedName":"f_pair","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int32"}},{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Pair"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f_either","serializedName":"f_either","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},{"parameters":[],"typeRef":{"kind":"primitive","value":"Int32"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Either"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f_map","serializedName":"f_map","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Map"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f_set","serializedName":"f_set","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Set"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f_mstring","serializedName":"f_mstring","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}}}},{"annotations":[],"default":{"kind":"just","value":{"just":"sukpeepolup"}},"name":"f_mstring2","serializedName":"f_mstring2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f_nstring","serializedName":"f_nstring","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"primitive","value":"Nullable"}}},{"annotations":[],"default":{"kind":"just","value":"abcde"},"name":"f_nstring2","serializedName":"f_nstring2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"primitive","value":"Nullable"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"f_int","serializedName":"f_int","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}}],"typeRef":{"kind":"primitive","value":"Nullable"}}},{"annotations":[],"default":{"kind":"just","value":100},"name":"f_int2","serializedName":"f_int2","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}}],"typeRef":{"kind":"primitive","value":"Nullable"}}},{"annotations":[],"default":{"kind":"just","value":null},"name":"f_int3","serializedName":"f_int3","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Int64"}}],"typeRef":{"kind":"primitive","value":"Nullable"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test6"};

export const snS: ADL.ScopedName = {moduleName:"test6", name:"S"};

export function texprS(): ADL.ATypeExpr<S> {
  return {value : {typeRef : {kind: "reference", value : snS}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test6.S" : S_AST
};
