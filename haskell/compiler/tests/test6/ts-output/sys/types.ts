/* @generated from adl module sys.types */

import * as ADL from '@adllang/adl-runtime';

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
  {"decl":{"annotations":[],"name":"Pair","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"v1","serializedName":"v1","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T1"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"v2","serializedName":"v2","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T2"}}}],"typeParams":["T1","T2"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snPair: ADL.ScopedName = {moduleName:"sys.types", name:"Pair"};

export function texprPair<T1, T2>(texprT1 : ADL.ATypeExpr<T1>, texprT2 : ADL.ATypeExpr<T2>): ADL.ATypeExpr<Pair<T1, T2>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Pair"}}, parameters : [texprT1.value, texprT2.value]}};
}

export interface Either_Left<T1, _T2> {
  kind: 'left';
  value: T1;
}
export interface Either_Right<_T1, T2> {
  kind: 'right';
  value: T2;
}

export type Either<T1, T2> = Either_Left<T1, T2> | Either_Right<T1, T2>;

export interface EitherOpts<T1, T2> {
  left: T1;
  right: T2;
}

export function makeEither<T1, T2, K extends keyof EitherOpts<T1, T2>>(kind: K, value: EitherOpts<T1, T2>[K]) { return {kind, value}; }

const Either_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Either","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"left","serializedName":"left","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T1"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"right","serializedName":"right","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T2"}}}],"typeParams":["T1","T2"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snEither: ADL.ScopedName = {moduleName:"sys.types", name:"Either"};

export function texprEither<T1, T2>(texprT1 : ADL.ATypeExpr<T1>, texprT2 : ADL.ATypeExpr<T2>): ADL.ATypeExpr<Either<T1, T2>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Either"}}, parameters : [texprT1.value, texprT2.value]}};
}

export interface Maybe_Nothing<_T> {
  kind: 'nothing';
}
export interface Maybe_Just<T> {
  kind: 'just';
  value: T;
}

export type Maybe<T> = Maybe_Nothing<T> | Maybe_Just<T>;

export interface MaybeOpts<T> {
  nothing: null;
  just: T;
}

export function makeMaybe<T, K extends keyof MaybeOpts<T>>(kind: K, value: MaybeOpts<T>[K]) { return {kind, value}; }

const Maybe_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Maybe","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"nothing","serializedName":"nothing","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Void"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"just","serializedName":"just","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snMaybe: ADL.ScopedName = {moduleName:"sys.types", name:"Maybe"};

export function texprMaybe<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Maybe<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Maybe"}}, parameters : [texprT.value]}};
}

export interface Result_Ok<T, _E> {
  kind: 'ok';
  value: T;
}
export interface Result_Error<_T, E> {
  kind: 'error';
  value: E;
}

export type Result<T, E> = Result_Ok<T, E> | Result_Error<T, E>;

export interface ResultOpts<T, E> {
  ok: T;
  error: E;
}

export function makeResult<T, E, K extends keyof ResultOpts<T, E>>(kind: K, value: ResultOpts<T, E>[K]) { return {kind, value}; }

const Result_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Result","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"ok","serializedName":"ok","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"error","serializedName":"error","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"E"}}}],"typeParams":["T","E"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snResult: ADL.ScopedName = {moduleName:"sys.types", name:"Result"};

export function texprResult<T, E>(texprT : ADL.ATypeExpr<T>, texprE : ADL.ATypeExpr<E>): ADL.ATypeExpr<Result<T, E>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Result"}}, parameters : [texprT.value, texprE.value]}};
}

export interface MapEntry<K, V> {
  key: K;
  value: V;
}

export function makeMapEntry<K, V>(
  input: {
    key: K,
    value: V,
  }
): MapEntry<K, V> {
  return {
    key: input.key,
    value: input.value,
  };
}

const MapEntry_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"MapEntry","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"key","serializedName":"k","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"K"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"value","serializedName":"v","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"V"}}}],"typeParams":["K","V"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snMapEntry: ADL.ScopedName = {moduleName:"sys.types", name:"MapEntry"};

export function texprMapEntry<K, V>(texprK : ADL.ATypeExpr<K>, texprV : ADL.ATypeExpr<V>): ADL.ATypeExpr<MapEntry<K, V>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "MapEntry"}}, parameters : [texprK.value, texprV.value]}};
}

export type Map<K, V> = MapEntry<K, V>[];

const Map_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Map","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"K"}},{"parameters":[],"typeRef":{"kind":"typeParam","value":"V"}}],"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"MapEntry"}}}],"typeRef":{"kind":"primitive","value":"Vector"}},"typeParams":["K","V"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snMap: ADL.ScopedName = {moduleName:"sys.types", name:"Map"};

export function texprMap<K, V>(texprK : ADL.ATypeExpr<K>, texprV : ADL.ATypeExpr<V>): ADL.ATypeExpr<Map<K, V>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Map"}}, parameters : [texprK.value, texprV.value]}};
}

export type Set<T> = T[];

const Set_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Set","type_":{"kind":"newtype_","value":{"default":{"kind":"nothing"},"typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"primitive","value":"Vector"}},"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"sys.types"};

export const snSet: ADL.ScopedName = {moduleName:"sys.types", name:"Set"};

export function texprSet<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Set<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "sys.types",name : "Set"}}, parameters : [texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.types.Pair" : Pair_AST,
  "sys.types.Either" : Either_AST,
  "sys.types.Maybe" : Maybe_AST,
  "sys.types.Result" : Result_AST,
  "sys.types.MapEntry" : MapEntry_AST,
  "sys.types.Map" : Map_AST,
  "sys.types.Set" : Set_AST
};
