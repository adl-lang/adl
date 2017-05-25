import * as AST from './sys/adlast';

export type ScopedDecl = AST.ScopedDecl;
export type ATypeRef<T> = {value: AST.TypeRef};
export type ATypeExpr<T> = {value : AST.TypeExpr};

/**
 * Construct a type expression from a monomorphic type reference.
 */
export function aTypeExpr0<T>(typeRef0 : ATypeRef<T>) : ATypeExpr<T> {
  return {value: {typeRef : typeRef0.value, parameters : []}};
}

/**
 * A function to obtain details on a declared type.
 */
export interface DeclResolver {
    (decl : AST.ScopedName): AST.ScopedDecl;
};

export function declResolver(...astMaps : ({[key:string] : AST.ScopedDecl})[]) {
  const astMap = {};
  for (let map of astMaps) {
    for (let scopedName in map) {
      astMap[scopedName] = map[scopedName];
    }
  }

  function resolver(scopedName : AST.ScopedName) : AST.ScopedDecl {
    const scopedNameStr = scopedName.moduleName + "." + scopedName.name;
    const result = astMap[scopedNameStr];
    if (result === undefined) {
      throw new Error("Unable to resolve ADL type " + scopedNameStr);
    }
    return result;
  }

  return resolver;
}
