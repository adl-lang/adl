import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as test5 } from "./test5";

export const ADL: { [key: string]: ScopedDecl } = {
  ...test5,
};

export const RESOLVER = declResolver(ADL);
