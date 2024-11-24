/* @generated from adl */
import { declResolver, ScopedDecl } from "@adllang/adl-runtime";
import { _AST_MAP as sys_adlast } from "./sys/adlast";
import { _AST_MAP as sys_dynamic } from "./sys/dynamic";
import { _AST_MAP as sys_types } from "./sys/types";
import { _AST_MAP as test6 } from "./test6";

export const ADL: { [key: string]: ScopedDecl } = {
  ...sys_adlast,
  ...sys_dynamic,
  ...sys_types,
  ...test6,
};

export const RESOLVER = declResolver(ADL);
