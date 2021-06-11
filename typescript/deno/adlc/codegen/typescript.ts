/* @generated from adl module adlc.codegen.typescript */

import * as ADL from './../../runtime/adl.ts';
import * as adlc_codegen_types from './types.ts';
import * as sys_adlast from './../../sys/adlast.ts';

/**
 * ADL type configuring the typescript code generator
 */
export interface TypescriptParams {
  sources: adlc_codegen_types.AdlSources;
  mergeExts: string[];
  modules: sys_adlast.ModuleName[];
  style: TypescriptStyle;
  output: adlc_codegen_types.OutputParams;
  runtimePath: string;
  generateTransitive: boolean;
  includeRuntime: boolean;
  includeResolver: boolean;
  excludeAst: boolean;
  excludedAstAnnotations: sys_adlast.ScopedName[];
  verbose: boolean;
}

export function makeTypescriptParams(
  input: {
    sources: adlc_codegen_types.AdlSources,
    mergeExts?: string[],
    modules: sys_adlast.ModuleName[],
    style: TypescriptStyle,
    output: adlc_codegen_types.OutputParams,
    runtimePath?: string,
    generateTransitive?: boolean,
    includeRuntime?: boolean,
    includeResolver?: boolean,
    excludeAst?: boolean,
    excludedAstAnnotations?: sys_adlast.ScopedName[],
    verbose?: boolean,
  }
): TypescriptParams {
  return {
    sources: input.sources,
    mergeExts: input.mergeExts === undefined ? [".adl-ts"] : input.mergeExts,
    modules: input.modules,
    style: input.style,
    output: input.output,
    runtimePath: input.runtimePath === undefined ? "runtime" : input.runtimePath,
    generateTransitive: input.generateTransitive === undefined ? true : input.generateTransitive,
    includeRuntime: input.includeRuntime === undefined ? true : input.includeRuntime,
    includeResolver: input.includeResolver === undefined ? true : input.includeResolver,
    excludeAst: input.excludeAst === undefined ? false : input.excludeAst,
    excludedAstAnnotations: input.excludedAstAnnotations === undefined ? [{moduleName : "sys.annotations", name : "Doc"}] : input.excludedAstAnnotations,
    verbose: input.verbose === undefined ? false : input.verbose,
  };
}

const TypescriptParams_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.typescript","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"sources","default":{"kind":"nothing"},"name":"sources","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"AdlSources"}},"parameters":[]}},{"annotations":[],"serializedName":"mergeExts","default":{"kind":"just","value":[".adl-ts"]},"name":"mergeExts","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"modules","default":{"kind":"nothing"},"name":"modules","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}]}},{"annotations":[],"serializedName":"style","default":{"kind":"nothing"},"name":"style","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.typescript","name":"TypescriptStyle"}},"parameters":[]}},{"annotations":[],"serializedName":"output","default":{"kind":"nothing"},"name":"output","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"OutputParams"}},"parameters":[]}},{"annotations":[],"serializedName":"runtimePath","default":{"kind":"just","value":"runtime"},"name":"runtimePath","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"generateTransitive","default":{"kind":"just","value":true},"name":"generateTransitive","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"includeRuntime","default":{"kind":"just","value":true},"name":"includeRuntime","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"includeResolver","default":{"kind":"just","value":true},"name":"includeResolver","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"excludeAst","default":{"kind":"just","value":false},"name":"excludeAst","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"excludedAstAnnotations","default":{"kind":"just","value":[{"moduleName":"sys.annotations","name":"Doc"}]},"name":"excludedAstAnnotations","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}},"parameters":[]}]}},{"annotations":[],"serializedName":"verbose","default":{"kind":"just","value":false},"name":"verbose","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"TypescriptParams","version":{"kind":"nothing"}}};

export const snTypescriptParams: ADL.ScopedName = {moduleName:"adlc.codegen.typescript", name:"TypescriptParams"};

export function texprTypescriptParams(): ADL.ATypeExpr<TypescriptParams> {
  return {value : {typeRef : {kind: "reference", value : snTypescriptParams}, parameters : []}};
}

export type TypescriptStyle = 'tsc' | 'deno';
export const valuesTypescriptStyle : TypescriptStyle[] = ['tsc', 'deno'];

const TypescriptStyle_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.typescript","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"tsc","default":{"kind":"nothing"},"name":"tsc","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"deno","default":{"kind":"nothing"},"name":"deno","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"TypescriptStyle","version":{"kind":"nothing"}}};

export const snTypescriptStyle: ADL.ScopedName = {moduleName:"adlc.codegen.typescript", name:"TypescriptStyle"};

export function texprTypescriptStyle(): ADL.ATypeExpr<TypescriptStyle> {
  return {value : {typeRef : {kind: "reference", value : snTypescriptStyle}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "adlc.codegen.typescript.TypescriptParams" : TypescriptParams_AST,
  "adlc.codegen.typescript.TypescriptStyle" : TypescriptStyle_AST
};
