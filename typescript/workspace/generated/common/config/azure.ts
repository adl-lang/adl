import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.azure */

export interface AzureCredentialsProvider_SharedAccessSignature {
  kind: 'sharedAccessSignature';
  value: string;
}

/**
 * Methods available to obtain Azure credentials
 */
export type AzureCredentialsProvider = AzureCredentialsProvider_SharedAccessSignature;

export interface AzureCredentialsProviderOpts {
  /**
 * A shared access signature string
   */
sharedAccessSignature: string;
}

export function makeAzureCredentialsProvider<K extends keyof AzureCredentialsProviderOpts>(kind: K, value: AzureCredentialsProviderOpts[K]) { return {kind, value}; }

const AzureCredentialsProvider_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.azure","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"sharedAccessSignature","default":{"kind":"nothing"},"name":"sharedAccessSignature","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"AzureCredentialsProvider","version":{"kind":"nothing"}}};

export const snAzureCredentialsProvider: ADL.ScopedName = {moduleName:"common.config.azure", name:"AzureCredentialsProvider"};

export function texprAzureCredentialsProvider(): ADL.ATypeExpr<AzureCredentialsProvider> {
  return {value:{typeRef:{kind:"reference",value:snAzureCredentialsProvider},parameters:[]}};
}

/**
 * Config parameters for an Azure Blob Client
 */
export interface AzureBlobClientConfig {
  blobstoreUri: string;
  credentials: AzureCredentialsProvider;
  container: string;
}

export function makeAzureBlobClientConfig(
  input: {
    blobstoreUri: string,
    credentials: AzureCredentialsProvider,
    container: string,
  }
): AzureBlobClientConfig {
  return {
    blobstoreUri: input.blobstoreUri,
    credentials: input.credentials,
    container: input.container,
  };
}

const AzureBlobClientConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.azure","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"blobstoreUri","default":{"kind":"nothing"},"name":"blobstoreUri","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"credentials","default":{"kind":"nothing"},"name":"credentials","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.azure","name":"AzureCredentialsProvider"}},"parameters":[]}},{"annotations":[],"serializedName":"container","default":{"kind":"nothing"},"name":"container","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"AzureBlobClientConfig","version":{"kind":"nothing"}}};

export const snAzureBlobClientConfig: ADL.ScopedName = {moduleName:"common.config.azure", name:"AzureBlobClientConfig"};

export function texprAzureBlobClientConfig(): ADL.ATypeExpr<AzureBlobClientConfig> {
  return {value:{typeRef:{kind:"reference",value:snAzureBlobClientConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.azure.AzureCredentialsProvider" : AzureCredentialsProvider_AST,
  "common.config.azure.AzureBlobClientConfig" : AzureBlobClientConfig_AST
};
