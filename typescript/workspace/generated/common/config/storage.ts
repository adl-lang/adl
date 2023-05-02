import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.storage */

export interface LocalBlobstoreConfig {
/**
 * The directory in which the blobstore files will be stored
 */
  directory: string;
/**
 * The host of the server to be used for serving public files, e.g. "http://localhost"
 */
  publicHost: string;
/**
 * The path on which blobstore objects are publically accessible e.g. "/files"
 */
  publicPath: string;
}

export function makeLocalBlobstoreConfig(
  input: {
    directory: string,
    publicHost: string,
    publicPath: string,
  }
): LocalBlobstoreConfig {
  return {
    directory: input.directory,
    publicHost: input.publicHost,
    publicPath: input.publicPath,
  };
}

const LocalBlobstoreConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.storage","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"directory","default":{"kind":"nothing"},"name":"directory","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"publicHost","default":{"kind":"nothing"},"name":"publicHost","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"publicPath","default":{"kind":"nothing"},"name":"publicPath","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"LocalBlobstoreConfig","version":{"kind":"nothing"}}};

export const snLocalBlobstoreConfig: ADL.ScopedName = {moduleName:"common.config.storage", name:"LocalBlobstoreConfig"};

export function texprLocalBlobstoreConfig(): ADL.ATypeExpr<LocalBlobstoreConfig> {
  return {value:{typeRef:{kind:"reference",value:snLocalBlobstoreConfig},parameters:[]}};
}

export interface LocalFileStoreConfig {
/**
 * The directory in which the blobstore files will be stored
 */
  directory: string;
/**
 * The host of the server to be used for serving public files, e.g. "http://localhost"
 */
  publicHost: string;
/**
 * The path on which blobstore objects are publically accessible e.g. "/files"
 */
  publicPath: string;
/**
 * False if the data in the FileStore is to be immutable
 */
  allowOverwrite: boolean;
}

export function makeLocalFileStoreConfig(
  input: {
    directory: string,
    publicHost: string,
    publicPath: string,
    allowOverwrite: boolean,
  }
): LocalFileStoreConfig {
  return {
    directory: input.directory,
    publicHost: input.publicHost,
    publicPath: input.publicPath,
    allowOverwrite: input.allowOverwrite,
  };
}

const LocalFileStoreConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.storage","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"directory","default":{"kind":"nothing"},"name":"directory","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"publicHost","default":{"kind":"nothing"},"name":"publicHost","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"publicPath","default":{"kind":"nothing"},"name":"publicPath","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"allowOverwrite","default":{"kind":"nothing"},"name":"allowOverwrite","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"LocalFileStoreConfig","version":{"kind":"nothing"}}};

export const snLocalFileStoreConfig: ADL.ScopedName = {moduleName:"common.config.storage", name:"LocalFileStoreConfig"};

export function texprLocalFileStoreConfig(): ADL.ATypeExpr<LocalFileStoreConfig> {
  return {value:{typeRef:{kind:"reference",value:snLocalFileStoreConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.storage.LocalBlobstoreConfig" : LocalBlobstoreConfig_AST,
  "common.config.storage.LocalFileStoreConfig" : LocalFileStoreConfig_AST
};
