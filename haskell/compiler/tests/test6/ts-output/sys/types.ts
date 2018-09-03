/* @generated from adl module sys.types */

import * as ADL from './../runtime/adl';

export interface Pair<T1, T2> {
  v1: T1;
  v2: T2;
}

export function makePair<T1, T2>(
  input: {
    v1: T1,
    v2: T2,
  }
): Pair<T1, T2> {
  return {
    v1: input.v1,
    v2: input.v2,
  };
}

const Pair_AST : ADL.ScopedDecl =
  {"moduleName":"sys.types","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T1","T2"],"fields":[{"annotations":[],"serializedName":"v1","default":{"kind":"nothing"},"name":"v1","typeExpr":{"typeRef":{"kind":"typeParam","value":"T1"},"parameters":[]}},{"annotations":[],"serializedName":"v2","default":{"kind":"nothing"},"name":"v2","typeExpr":{"typeRef":{"kind":"typeParam","value":"T2"},"parameters":[]}}]}},"name":"Pair","version":{"kind":"nothing"}}};

export function texprPair<T1, T2>(texprT1 : ADL.ATypeExpr<T1>, texprT2 : ADL.ATypeExpr<T2>): ADL.ATypeExpr<Pair<T1, T2>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Pair"}}, parameters : [texprT1.value, texprT2.value]}};
}

export interface Either_Left<T1, T2> {
  kind: 'left';
  value: T1;
}
export interface Either_Right<T1, T2> {
  kind: 'right';
  value: T2;
}

export type Either<T1, T2> = Either_Left<T1, T2> | Either_Right<T1, T2>;

const Either_AST : ADL.ScopedDecl =
  {"moduleName":"sys.types","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["T1","T2"],"fields":[{"annotations":[],"serializedName":"left","default":{"kind":"nothing"},"name":"left","typeExpr":{"typeRef":{"kind":"typeParam","value":"T1"},"parameters":[]}},{"annotations":[],"serializedName":"right","default":{"kind":"nothing"},"name":"right","typeExpr":{"typeRef":{"kind":"typeParam","value":"T2"},"parameters":[]}}]}},"name":"Either","version":{"kind":"nothing"}}};

export function texprEither<T1, T2>(texprT1 : ADL.ATypeExpr<T1>, texprT2 : ADL.ATypeExpr<T2>): ADL.ATypeExpr<Either<T1, T2>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Either"}}, parameters : [texprT1.value, texprT2.value]}};
}

export interface Maybe_Nothing<T> {
  kind: 'nothing';
}
export interface Maybe_Just<T> {
  kind: 'just';
  value: T;
}

export type Maybe<T> = Maybe_Nothing<T> | Maybe_Just<T>;

const Maybe_AST : ADL.ScopedDecl =
  {"moduleName":"sys.types","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"nothing","default":{"kind":"nothing"},"name":"nothing","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"just","default":{"kind":"nothing"},"name":"just","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"Maybe","version":{"kind":"nothing"}}};

export function texprMaybe<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Maybe<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Maybe"}}, parameters : [texprT.value]}};
}

export interface Error_Value<T> {
  kind: 'value';
  value: T;
}
export interface Error_Error<T> {
  kind: 'error';
  value: string;
}

export type Error<T> = Error_Value<T> | Error_Error<T>;

const Error_AST : ADL.ScopedDecl =
  {"moduleName":"sys.types","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"error","default":{"kind":"nothing"},"name":"error","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"Error","version":{"kind":"nothing"}}};

export function texprError<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Error<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Error"}}, parameters : [texprT.value]}};
}

export type Map<K, V> = Pair<K, V>[];

const Map_AST : ADL.ScopedDecl =
  {"moduleName":"sys.types","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":["K","V"],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Pair"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"K"},"parameters":[]},{"typeRef":{"kind":"typeParam","value":"V"},"parameters":[]}]}]}}},"name":"Map","version":{"kind":"nothing"}}};

export function texprMap<K, V>(texprK : ADL.ATypeExpr<K>, texprV : ADL.ATypeExpr<V>): ADL.ATypeExpr<Map<K, V>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Map"}}, parameters : [texprK.value, texprV.value]}};
}

export type Set<T> = T[];

const Set_AST : ADL.ScopedDecl =
  {"moduleName":"sys.types","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":["T"],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}},"name":"Set","version":{"kind":"nothing"}}};

export function texprSet<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Set<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Set"}}, parameters : [texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.types.Pair" : Pair_AST,
  "sys.types.Either" : Either_AST,
  "sys.types.Maybe" : Maybe_AST,
  "sys.types.Error" : Error_AST,
  "sys.types.Map" : Map_AST,
  "sys.types.Set" : Set_AST
};
