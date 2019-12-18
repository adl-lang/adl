/* @generated from adl module sys.types */


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

export interface Either_Left<T1, _T2> {
  kind: 'left';
  value: T1;
}
export interface Either_Right<_T1, T2> {
  kind: 'right';
  value: T2;
}

export type Either<T1, T2> = Either_Left<T1, T2> | Either_Right<T1, T2>;

export interface Maybe_Nothing<_T> {
  kind: 'nothing';
}
export interface Maybe_Just<T> {
  kind: 'just';
  value: T;
}

export type Maybe<T> = Maybe_Nothing<T> | Maybe_Just<T>;

export interface Error_Value<T> {
  kind: 'value';
  value: T;
}
export interface Error_Error<_T> {
  kind: 'error';
  value: string;
}

export type Error<T> = Error_Value<T> | Error_Error<T>;

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

export type Map<K, V> = Pair<K, V>[];

export type Set<T> = T[];
