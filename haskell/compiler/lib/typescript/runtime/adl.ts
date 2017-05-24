import * as AST from './sys/adlast';

export type TypeRef<T> = {ref: string};

export type ATypeExpr<T> = {value : AST.TypeExpr};

/**
 * A function to obtain details on a declared type.
 */
export interface DeclResolver {
    (decl : AST.ScopedName): AST.ScopedDecl;
};
