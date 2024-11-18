import * as AST from './sys/adlast.ts';

/**
 * A function to obtain details on a declared type.
 */
export interface DeclResolver {
    (decl : AST.ScopedName): AST.ScopedDecl;
};

export function declResolver(...astMaps : ({[key:string] : AST.ScopedDecl})[]): DeclResolver {
  const astMap :  {[key:string] : AST.ScopedDecl} = {};
  for (const map of astMaps) {
    for (const scopedName in map) {
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

