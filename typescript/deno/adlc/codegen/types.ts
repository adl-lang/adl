/* @generated from adl module adlc.codegen.types */

import * as ADL from './../../runtime/adl.ts';
import * as sys_adlast from './../../sys/adlast.ts';

export type FilePath = string;

const FilePath_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"FilePath","version":{"kind":"nothing"}}};

export const snFilePath: ADL.ScopedName = {moduleName:"adlc.codegen.types", name:"FilePath"};

export function texprFilePath(): ADL.ATypeExpr<FilePath> {
  return {value : {typeRef : {kind: "reference", value : snFilePath}, parameters : []}};
}

export type AdlSources = AdlTreeSource[];

const AdlSources_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.types","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"AdlTreeSource"}},"parameters":[]}]}}},"name":"AdlSources","version":{"kind":"nothing"}}};

export const snAdlSources: ADL.ScopedName = {moduleName:"adlc.codegen.types", name:"AdlSources"};

export function texprAdlSources(): ADL.ATypeExpr<AdlSources> {
  return {value : {typeRef : {kind: "reference", value : snAdlSources}, parameters : []}};
}

export interface AdlTreeSource_LocalPath {
  kind: 'localPath';
  value: FilePath;
}
export interface AdlTreeSource_Modules {
  kind: 'modules';
  value: {[key: string]: sys_adlast.Module};
}

export type AdlTreeSource = AdlTreeSource_LocalPath | AdlTreeSource_Modules;

export interface AdlTreeSourceOpts {
  localPath: FilePath;
  modules: {[key: string]: sys_adlast.Module};
}

export function makeAdlTreeSource<K extends keyof AdlTreeSourceOpts>(kind: K, value: AdlTreeSourceOpts[K]) { return {kind, value}; }

const AdlTreeSource_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.types","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"localPath","default":{"kind":"nothing"},"name":"localPath","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"modules","default":{"kind":"nothing"},"name":"modules","typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Module"}},"parameters":[]}]}}]}},"name":"AdlTreeSource","version":{"kind":"nothing"}}};

export const snAdlTreeSource: ADL.ScopedName = {moduleName:"adlc.codegen.types", name:"AdlTreeSource"};

export function texprAdlTreeSource(): ADL.ATypeExpr<AdlTreeSource> {
  return {value : {typeRef : {kind: "reference", value : snAdlTreeSource}, parameters : []}};
}

/**
 * Config for the writing of output files
 */
export interface OutputParams {
  path: FilePath;
  noOverwrite: boolean;
  manifest: (FilePath|null);
}

export function makeOutputParams(
  input: {
    path: FilePath,
    noOverwrite?: boolean,
    manifest?: (FilePath|null),
  }
): OutputParams {
  return {
    path: input.path,
    noOverwrite: input.noOverwrite === undefined ? true : input.noOverwrite,
    manifest: input.manifest === undefined ? ".adl-manifest" : input.manifest,
  };
}

const OutputParams_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.types","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"FilePath"}},"parameters":[]}},{"annotations":[],"serializedName":"noOverwrite","default":{"kind":"just","value":true},"name":"noOverwrite","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"manifest","default":{"kind":"just","value":".adl-manifest"},"name":"manifest","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.types","name":"FilePath"}},"parameters":[]}]}}]}},"name":"OutputParams","version":{"kind":"nothing"}}};

export const snOutputParams: ADL.ScopedName = {moduleName:"adlc.codegen.types", name:"OutputParams"};

export function texprOutputParams(): ADL.ATypeExpr<OutputParams> {
  return {value : {typeRef : {kind: "reference", value : snOutputParams}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "adlc.codegen.types.FilePath" : FilePath_AST,
  "adlc.codegen.types.AdlSources" : AdlSources_AST,
  "adlc.codegen.types.AdlTreeSource" : AdlTreeSource_AST,
  "adlc.codegen.types.OutputParams" : OutputParams_AST
};
