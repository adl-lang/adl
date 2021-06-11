/* @generated from adl module adlc.codegen.ast */

import * as ADL from './../../runtime/adl.ts';
import * as adlc_codegen_types from './types.ts';
import * as sys_adlast from './../../sys/adlast.ts';

/**
 * ADL type configuring the ast code generator
 */
export interface AstParams {
  sources: adlc_codegen_types.AdlSources;
  mergeExts: string[];
  modules: sys_adlast.ModuleName[];
  generateTransitive: boolean;
  outputFile: adlc_codegen_types.FilePath;
  verbose: boolean;
}

export function makeAstParams(
  input: {
    sources: adlc_codegen_types.AdlSources,
    mergeExts?: string[],
    modules: sys_adlast.ModuleName[],
    generateTransitive?: boolean,
    outputFile: adlc_codegen_types.FilePath,
    verbose?: boolean,
  }
): AstParams {
  return {
    sources: input.sources,
    mergeExts: input.mergeExts === undefined ? [] : input.mergeExts,
    modules: input.modules,
    generateTransitive: input.generateTransitive === undefined ? true : input.generateTransitive,
    outputFile: input.outputFile,
    verbose: input.verbose === undefined ? false : input.verbose,
  };
}

const AstParams_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.ast","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"sources","default":{"kind":"nothing"},"name":"sources","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"AdlSources"}},"parameters":[]}},{"annotations":[],"serializedName":"mergeExts","default":{"kind":"just","value":[]},"name":"mergeExts","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"modules","default":{"kind":"nothing"},"name":"modules","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}]}},{"annotations":[],"serializedName":"generateTransitive","default":{"kind":"just","value":true},"name":"generateTransitive","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"outputFile","default":{"kind":"nothing"},"name":"outputFile","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"verbose","default":{"kind":"just","value":false},"name":"verbose","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"AstParams","version":{"kind":"nothing"}}};

export const snAstParams: ADL.ScopedName = {moduleName:"adlc.codegen.ast", name:"AstParams"};

export function texprAstParams(): ADL.ATypeExpr<AstParams> {
  return {value : {typeRef : {kind: "reference", value : snAstParams}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "adlc.codegen.ast.AstParams" : AstParams_AST
};
