/* @generated from adl */
import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as test3 } from "./test3";

export const ADL: { [key: string]: ScopedDecl } = {
  ...test3,
};

export const RESOLVER = declResolver(ADL);
