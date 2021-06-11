/* @generated from adl module adlc.codegen.java */

import * as ADL from './../../runtime/adl.ts';
import * as adlc_codegen_types from './types.ts';
import * as sys_adlast from './../../sys/adlast.ts';

/**
 * ADL type configuring the java code generator
 */
export interface JavaParams {
  sources: adlc_codegen_types.AdlSources;
  mergeExts: string[];
  modules: sys_adlast.ModuleName[];
  package: string;
  output: adlc_codegen_types.OutputParams;
  generateTransitive: boolean;
  includeRuntime: boolean;
  parcellable: boolean;
  hungarianNaming: boolean;
  maxLineLength: (number|null);
  headerComment: string;
  suppressWarnings: string[];
  verbose: boolean;
}

export function makeJavaParams(
  input: {
    sources: adlc_codegen_types.AdlSources,
    mergeExts?: string[],
    modules: sys_adlast.ModuleName[],
    package: string,
    output: adlc_codegen_types.OutputParams,
    generateTransitive?: boolean,
    includeRuntime?: boolean,
    parcellable?: boolean,
    hungarianNaming?: boolean,
    maxLineLength?: (number|null),
    headerComment?: string,
    suppressWarnings?: string[],
    verbose?: boolean,
  }
): JavaParams {
  return {
    sources: input.sources,
    mergeExts: input.mergeExts === undefined ? [".adl-java"] : input.mergeExts,
    modules: input.modules,
    package: input.package,
    output: input.output,
    generateTransitive: input.generateTransitive === undefined ? true : input.generateTransitive,
    includeRuntime: input.includeRuntime === undefined ? true : input.includeRuntime,
    parcellable: input.parcellable === undefined ? false : input.parcellable,
    hungarianNaming: input.hungarianNaming === undefined ? false : input.hungarianNaming,
    maxLineLength: input.maxLineLength === undefined ? null : input.maxLineLength,
    headerComment: input.headerComment === undefined ? "" : input.headerComment,
    suppressWarnings: input.suppressWarnings === undefined ? [] : input.suppressWarnings,
    verbose: input.verbose === undefined ? false : input.verbose,
  };
}

const JavaParams_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.java","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"sources","default":{"kind":"nothing"},"name":"sources","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"AdlSources"}},"parameters":[]}},{"annotations":[],"serializedName":"mergeExts","default":{"kind":"just","value":[".adl-java"]},"name":"mergeExts","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"modules","default":{"kind":"nothing"},"name":"modules","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}]}},{"annotations":[],"serializedName":"package","default":{"kind":"nothing"},"name":"package","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"output","default":{"kind":"nothing"},"name":"output","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"OutputParams"}},"parameters":[]}},{"annotations":[],"serializedName":"generateTransitive","default":{"kind":"just","value":true},"name":"generateTransitive","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"includeRuntime","default":{"kind":"just","value":true},"name":"includeRuntime","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"parcellable","default":{"kind":"just","value":false},"name":"parcellable","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"hungarianNaming","default":{"kind":"just","value":false},"name":"hungarianNaming","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"maxLineLength","default":{"kind":"just","value":null},"name":"maxLineLength","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"Word16"},"parameters":[]}]}},{"annotations":[],"serializedName":"headerComment","default":{"kind":"just","value":""},"name":"headerComment","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"suppressWarnings","default":{"kind":"just","value":[]},"name":"suppressWarnings","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"verbose","default":{"kind":"just","value":false},"name":"verbose","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"JavaParams","version":{"kind":"nothing"}}};

export const snJavaParams: ADL.ScopedName = {moduleName:"adlc.codegen.java", name:"JavaParams"};

export function texprJavaParams(): ADL.ATypeExpr<JavaParams> {
  return {value : {typeRef : {kind: "reference", value : snJavaParams}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "adlc.codegen.java.JavaParams" : JavaParams_AST
};
