import { declResolver, ScopedDecl } from "./runtime/adl";
import { _AST_MAP as sys_types } from "./sys/types";
import { _AST_MAP as sys_adlast } from "./sys/adlast";
import { _AST_MAP as sys_dynamic } from "./sys/dynamic";
import { _AST_MAP as test6 } from "./test6";

export const ADL: { [key: string]: ScopedDecl } = {
  ...sys_types,
  ...sys_adlast,
  ...sys_dynamic,
  ...test6,
};

export const RESOLVER = declResolver(ADL);
