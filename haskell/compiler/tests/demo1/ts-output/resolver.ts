import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as picture } from "./picture";

export const ADL: { [key: string]: ScopedDecl } = {
  ...picture,
};

export const RESOLVER = declResolver(ADL);
