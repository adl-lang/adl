import {typeExprsEqual} from './utils.ts';
import {JsonBinding} from './json.ts';
import {Dynamic} from './sys/dynamic.ts';

/**
 * Convert an ADL value to a dynamically typed value
 */
export function toDynamic<T>(jsonBinding : JsonBinding<T>, value : T) : Dynamic {
  return {typeExpr: jsonBinding.typeExpr, value : jsonBinding.toJson(value)};
}

/**
 * Convert an ADL value to a dynamically typed value
 */
export function fromDynamic<T>(jsonBinding : JsonBinding<T>, dynamic : Dynamic) : (T|null) {
  if (typeExprsEqual(jsonBinding.typeExpr, dynamic.typeExpr)) {
    return jsonBinding.fromJson(dynamic.value);
  }
  return null;
}
