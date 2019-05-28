/* @generated from adl */
import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as test2 } from "./test2";

export const ADL: { [key: string]: ScopedDecl } = {
  ...test2,
};

export const RESOLVER = declResolver(ADL);
