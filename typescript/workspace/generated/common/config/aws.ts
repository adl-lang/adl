import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.aws */

export interface AwsCredentials {
  accessKey: string;
  secretKey: string;
}

export function makeAwsCredentials(
  input: {
    accessKey: string,
    secretKey: string,
  }
): AwsCredentials {
  return {
    accessKey: input.accessKey,
    secretKey: input.secretKey,
  };
}

const AwsCredentials_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.aws","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"accessKey","default":{"kind":"nothing"},"name":"accessKey","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"secretKey","default":{"kind":"nothing"},"name":"secretKey","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"AwsCredentials","version":{"kind":"nothing"}}};

export const snAwsCredentials: ADL.ScopedName = {moduleName:"common.config.aws", name:"AwsCredentials"};

export function texprAwsCredentials(): ADL.ATypeExpr<AwsCredentials> {
  return {value:{typeRef:{kind:"reference",value:snAwsCredentials},parameters:[]}};
}

export interface AwsCredentialsProvider_UseEnvVariables {
  kind: 'useEnvVariables';
}
export interface AwsCredentialsProvider_UseInstanceProfile {
  kind: 'useInstanceProfile';
}
export interface AwsCredentialsProvider_Value {
  kind: 'value';
  value: AwsCredentials;
}

/**
 * The methods available to obtain AWS credentials
 */
export type AwsCredentialsProvider = AwsCredentialsProvider_UseEnvVariables | AwsCredentialsProvider_UseInstanceProfile | AwsCredentialsProvider_Value;

export interface AwsCredentialsProviderOpts {
  useEnvVariables: null;
  useInstanceProfile: null;
  value: AwsCredentials;
}

export function makeAwsCredentialsProvider<K extends keyof AwsCredentialsProviderOpts>(kind: K, value: AwsCredentialsProviderOpts[K]) { return {kind, value}; }

const AwsCredentialsProvider_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.aws","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"useEnvVariables","default":{"kind":"nothing"},"name":"useEnvVariables","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"useInstanceProfile","default":{"kind":"nothing"},"name":"useInstanceProfile","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsCredentials"}},"parameters":[]}}]}},"name":"AwsCredentialsProvider","version":{"kind":"nothing"}}};

export const snAwsCredentialsProvider: ADL.ScopedName = {moduleName:"common.config.aws", name:"AwsCredentialsProvider"};

export function texprAwsCredentialsProvider(): ADL.ATypeExpr<AwsCredentialsProvider> {
  return {value:{typeRef:{kind:"reference",value:snAwsCredentialsProvider},parameters:[]}};
}

export interface AwsRegionProvider_FromInstance {
  kind: 'fromInstance';
}
export interface AwsRegionProvider_Value {
  kind: 'value';
  value: string;
}

/**
 * The methods available to determine the AWS region
 */
export type AwsRegionProvider = AwsRegionProvider_FromInstance | AwsRegionProvider_Value;

export interface AwsRegionProviderOpts {
  fromInstance: null;
  value: string;
}

export function makeAwsRegionProvider<K extends keyof AwsRegionProviderOpts>(kind: K, value: AwsRegionProviderOpts[K]) { return {kind, value}; }

const AwsRegionProvider_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.aws","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"fromInstance","default":{"kind":"nothing"},"name":"fromInstance","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"AwsRegionProvider","version":{"kind":"nothing"}}};

export const snAwsRegionProvider: ADL.ScopedName = {moduleName:"common.config.aws", name:"AwsRegionProvider"};

export function texprAwsRegionProvider(): ADL.ATypeExpr<AwsRegionProvider> {
  return {value:{typeRef:{kind:"reference",value:snAwsRegionProvider},parameters:[]}};
}

export interface S3Location {
/**
 * The name on of an S3 bucket
 */
  s3Bucket: string;
/**
 * The S3 path prefix for the storage
 */
  s3Prefix: string;
}

export function makeS3Location(
  input: {
    s3Bucket: string,
    s3Prefix: string,
  }
): S3Location {
  return {
    s3Bucket: input.s3Bucket,
    s3Prefix: input.s3Prefix,
  };
}

const S3Location_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.aws","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"s3Bucket","default":{"kind":"nothing"},"name":"s3Bucket","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"s3Prefix","default":{"kind":"nothing"},"name":"s3Prefix","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"S3Location","version":{"kind":"nothing"}}};

export const snS3Location: ADL.ScopedName = {moduleName:"common.config.aws", name:"S3Location"};

export function texprS3Location(): ADL.ATypeExpr<S3Location> {
  return {value:{typeRef:{kind:"reference",value:snS3Location},parameters:[]}};
}

/**
 * Description of an AWS SQS queue
 */
export interface QueueDetails {
/**
 * The amount of time to keep the connection open while waiting for queue messages.
 */
  waitTimeSeconds: number;
  queueUrl: string;
}

export function makeQueueDetails(
  input: {
    waitTimeSeconds?: number,
    queueUrl?: string,
  }
): QueueDetails {
  return {
    waitTimeSeconds: input.waitTimeSeconds === undefined ? 10 : input.waitTimeSeconds,
    queueUrl: input.queueUrl === undefined ? "" : input.queueUrl,
  };
}

const QueueDetails_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.aws","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"waitTimeSeconds","default":{"kind":"just","value":10},"name":"waitTimeSeconds","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"queueUrl","default":{"kind":"just","value":""},"name":"queueUrl","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"QueueDetails","version":{"kind":"nothing"}}};

export const snQueueDetails: ADL.ScopedName = {moduleName:"common.config.aws", name:"QueueDetails"};

export function texprQueueDetails(): ADL.ATypeExpr<QueueDetails> {
  return {value:{typeRef:{kind:"reference",value:snQueueDetails},parameters:[]}};
}

/**
 * Configuration of an S3 client
 */
export interface S3ClientConfig {
  location: S3Location;
  region: AwsRegionProvider;
  credentials: AwsCredentialsProvider;
}

export function makeS3ClientConfig(
  input: {
    location: S3Location,
    region: AwsRegionProvider,
    credentials: AwsCredentialsProvider,
  }
): S3ClientConfig {
  return {
    location: input.location,
    region: input.region,
    credentials: input.credentials,
  };
}

const S3ClientConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.aws","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"location","default":{"kind":"nothing"},"name":"location","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"S3Location"}},"parameters":[]}},{"annotations":[],"serializedName":"region","default":{"kind":"nothing"},"name":"region","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsRegionProvider"}},"parameters":[]}},{"annotations":[],"serializedName":"credentials","default":{"kind":"nothing"},"name":"credentials","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsCredentialsProvider"}},"parameters":[]}}]}},"name":"S3ClientConfig","version":{"kind":"nothing"}}};

export const snS3ClientConfig: ADL.ScopedName = {moduleName:"common.config.aws", name:"S3ClientConfig"};

export function texprS3ClientConfig(): ADL.ATypeExpr<S3ClientConfig> {
  return {value:{typeRef:{kind:"reference",value:snS3ClientConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.aws.AwsCredentials" : AwsCredentials_AST,
  "common.config.aws.AwsCredentialsProvider" : AwsCredentialsProvider_AST,
  "common.config.aws.AwsRegionProvider" : AwsRegionProvider_AST,
  "common.config.aws.S3Location" : S3Location_AST,
  "common.config.aws.QueueDetails" : QueueDetails_AST,
  "common.config.aws.S3ClientConfig" : S3ClientConfig_AST
};
