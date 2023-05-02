import {_AST_MAP as sys_adlast} from "./adlast";
import {_AST_MAP as sys_adlast2} from "./adlast2";
import {_AST_MAP as sys_annotations} from "./annotations";
import {_AST_MAP as sys_dynamic} from "./dynamic";
import {_AST_MAP as sys_types} from "./types";
import {ScopedDecl, declResolver} from "@adl-lang/runtime/adl";

/* @generated from adl */

export const ADL_local: { [key: string]: ScopedDecl } = {
  ...sys_adlast,
  ...sys_adlast2,
  ...sys_annotations,
  ...sys_dynamic,
  ...sys_types,
};

export const ADL: { [key: string]: ScopedDecl } = {
  ...ADL_local,
};

export const RESOLVER = declResolver(ADL);
