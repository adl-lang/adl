/* @generated from adl */
import { declResolver, ScopedDecl } from "./runtime/adl.ts";
import { _AST_MAP as adlc_codegen_ast } from "./adlc/codegen/ast.ts";
import { _AST_MAP as adlc_codegen_batch } from "./adlc/codegen/batch.ts";
import { _AST_MAP as adlc_codegen_java } from "./adlc/codegen/java.ts";
import { _AST_MAP as adlc_codegen_types } from "./adlc/codegen/types.ts";
import { _AST_MAP as adlc_codegen_typescript } from "./adlc/codegen/typescript.ts";
import { _AST_MAP as sys_adlast } from "./sys/adlast.ts";
import { _AST_MAP as sys_annotations } from "./sys/annotations.ts";
import { _AST_MAP as sys_types } from "./sys/types.ts";

export const ADL: { [key: string]: ScopedDecl } = {
  ...adlc_codegen_ast,
  ...adlc_codegen_batch,
  ...adlc_codegen_java,
  ...adlc_codegen_types,
  ...adlc_codegen_typescript,
  ...sys_adlast,
  ...sys_annotations,
  ...sys_types,
};

export const RESOLVER = declResolver(ADL);
