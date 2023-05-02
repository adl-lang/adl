import * as common_config_aws from "./aws";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.log */

export type LogLevel = 'DEBUG' | 'INFO' | 'WARN' | 'ERROR' | 'NONE' | 'TRACE';
export const valuesLogLevel : LogLevel[] = ['DEBUG', 'INFO', 'WARN', 'ERROR', 'NONE', 'TRACE'];

const LogLevel_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"DEBUG","default":{"kind":"nothing"},"name":"DEBUG","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"INFO","default":{"kind":"nothing"},"name":"INFO","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"WARN","default":{"kind":"nothing"},"name":"WARN","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"ERROR","default":{"kind":"nothing"},"name":"ERROR","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"NONE","default":{"kind":"nothing"},"name":"NONE","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"TRACE","default":{"kind":"nothing"},"name":"TRACE","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"LogLevel","version":{"kind":"nothing"}}};

export const snLogLevel: ADL.ScopedName = {moduleName:"common.config.log", name:"LogLevel"};

export function texprLogLevel(): ADL.ATypeExpr<LogLevel> {
  return {value:{typeRef:{kind:"reference",value:snLogLevel},parameters:[]}};
}

export interface StdoutConfig {
  level: LogLevel;
}

export function makeStdoutConfig(
  input: {
    level: LogLevel,
  }
): StdoutConfig {
  return {
    level: input.level,
  };
}

const StdoutConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}}]}},"name":"StdoutConfig","version":{"kind":"nothing"}}};

export const snStdoutConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"StdoutConfig"};

export function texprStdoutConfig(): ADL.ATypeExpr<StdoutConfig> {
  return {value:{typeRef:{kind:"reference",value:snStdoutConfig},parameters:[]}};
}

export interface FluentdConfig {
  level: LogLevel;
  hostname: string;
  port: number;
  tag: string;
}

export function makeFluentdConfig(
  input: {
    level: LogLevel,
    hostname: string,
    port: number,
    tag: string,
  }
): FluentdConfig {
  return {
    level: input.level,
    hostname: input.hostname,
    port: input.port,
    tag: input.tag,
  };
}

const FluentdConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}},{"annotations":[],"serializedName":"hostname","default":{"kind":"nothing"},"name":"hostname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"tag","default":{"kind":"nothing"},"name":"tag","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"FluentdConfig","version":{"kind":"nothing"}}};

export const snFluentdConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"FluentdConfig"};

export function texprFluentdConfig(): ADL.ATypeExpr<FluentdConfig> {
  return {value:{typeRef:{kind:"reference",value:snFluentdConfig},parameters:[]}};
}

/**
 * Server side Rollbar config
 * NOTE: Name is `ServerRollbarConfig` due to being named prior to adding
 * `ClientRollbarConfig`
 */
export interface RollbarConfig {
  level: LogLevel;
  serverToken: string;
  env: string;
}

export function makeRollbarConfig(
  input: {
    level: LogLevel,
    serverToken: string,
    env?: string,
  }
): RollbarConfig {
  return {
    level: input.level,
    serverToken: input.serverToken,
    env: input.env === undefined ? "" : input.env,
  };
}

const RollbarConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}},{"annotations":[],"serializedName":"serverToken","default":{"kind":"nothing"},"name":"serverToken","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"env","default":{"kind":"just","value":""},"name":"env","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"RollbarConfig","version":{"kind":"nothing"}}};

export const snRollbarConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"RollbarConfig"};

export function texprRollbarConfig(): ADL.ATypeExpr<RollbarConfig> {
  return {value:{typeRef:{kind:"reference",value:snRollbarConfig},parameters:[]}};
}

/**
 * Server side logging, i.e. reporting errors originating from the server
 * NOTE: Name is not `ServerLogConfig` due to being named prior to adding
 * `ClientLogConfig`
 */
export interface LogConfig {
  stdout: (StdoutConfig|null);
  fluentd: (FluentdConfig|null);
  rollbar: (RollbarConfig|null);
  cloudwatch: (CloudWatchMetricLogConfig|null);
}

export function makeLogConfig(
  input: {
    stdout?: (StdoutConfig|null),
    fluentd?: (FluentdConfig|null),
    rollbar?: (RollbarConfig|null),
    cloudwatch?: (CloudWatchMetricLogConfig|null),
  }
): LogConfig {
  return {
    stdout: input.stdout === undefined ? {level : "INFO"} : input.stdout,
    fluentd: input.fluentd === undefined ? null : input.fluentd,
    rollbar: input.rollbar === undefined ? null : input.rollbar,
    cloudwatch: input.cloudwatch === undefined ? null : input.cloudwatch,
  };
}

const LogConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"stdout","default":{"kind":"just","value":{"level":"INFO"}},"name":"stdout","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"StdoutConfig"}},"parameters":[]}]}},{"annotations":[],"serializedName":"fluentd","default":{"kind":"just","value":null},"name":"fluentd","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"FluentdConfig"}},"parameters":[]}]}},{"annotations":[],"serializedName":"rollbar","default":{"kind":"just","value":null},"name":"rollbar","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"RollbarConfig"}},"parameters":[]}]}},{"annotations":[],"serializedName":"cloudwatch","default":{"kind":"just","value":null},"name":"cloudwatch","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"CloudWatchMetricLogConfig"}},"parameters":[]}]}}]}},"name":"LogConfig","version":{"kind":"nothing"}}};

export const snLogConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"LogConfig"};

export function texprLogConfig(): ADL.ATypeExpr<LogConfig> {
  return {value:{typeRef:{kind:"reference",value:snLogConfig},parameters:[]}};
}

/**
 * Client side Rollbar config, i.e. from Rollbar JS
 */
export interface ClientRollbarConfig {
  accessToken: string;
  env: string;
}

export function makeClientRollbarConfig(
  input: {
    accessToken: string,
    env?: string,
  }
): ClientRollbarConfig {
  return {
    accessToken: input.accessToken,
    env: input.env === undefined ? "" : input.env,
  };
}

const ClientRollbarConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"accessToken","default":{"kind":"nothing"},"name":"accessToken","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"env","default":{"kind":"just","value":""},"name":"env","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"ClientRollbarConfig","version":{"kind":"nothing"}}};

export const snClientRollbarConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"ClientRollbarConfig"};

export function texprClientRollbarConfig(): ADL.ATypeExpr<ClientRollbarConfig> {
  return {value:{typeRef:{kind:"reference",value:snClientRollbarConfig},parameters:[]}};
}

/**
 * Client side logging, i.e. reporting errors originating from the client
 */
export interface ClientLogConfig {
  rollbar: (ClientRollbarConfig|null);
}

export function makeClientLogConfig(
  input: {
    rollbar?: (ClientRollbarConfig|null),
  }
): ClientLogConfig {
  return {
    rollbar: input.rollbar === undefined ? null : input.rollbar,
  };
}

const ClientLogConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"rollbar","default":{"kind":"just","value":null},"name":"rollbar","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"ClientRollbarConfig"}},"parameters":[]}]}}]}},"name":"ClientLogConfig","version":{"kind":"nothing"}}};

export const snClientLogConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"ClientLogConfig"};

export function texprClientLogConfig(): ADL.ATypeExpr<ClientLogConfig> {
  return {value:{typeRef:{kind:"reference",value:snClientLogConfig},parameters:[]}};
}

/**
 * Logging of metrics to cloudwatch
 */
export interface CloudWatchMetricLogConfig {
  credentials: common_config_aws.AwsCredentialsProvider;
  region: common_config_aws.AwsRegionProvider;
  env: string;
  namespace: string;
/**
 * A whitelist of metrics that should be sent to CloudWatch. Users should be
 * aware that making this list too big can quickly cause costs to increase
 */
  metricsToSend: string[];
}

export function makeCloudWatchMetricLogConfig(
  input: {
    credentials: common_config_aws.AwsCredentialsProvider,
    region: common_config_aws.AwsRegionProvider,
    env: string,
    namespace: string,
    metricsToSend: string[],
  }
): CloudWatchMetricLogConfig {
  return {
    credentials: input.credentials,
    region: input.region,
    env: input.env,
    namespace: input.namespace,
    metricsToSend: input.metricsToSend,
  };
}

const CloudWatchMetricLogConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.log","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"credentials","default":{"kind":"nothing"},"name":"credentials","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsCredentialsProvider"}},"parameters":[]}},{"annotations":[],"serializedName":"region","default":{"kind":"nothing"},"name":"region","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.aws","name":"AwsRegionProvider"}},"parameters":[]}},{"annotations":[],"serializedName":"env","default":{"kind":"nothing"},"name":"env","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"namespace","default":{"kind":"nothing"},"name":"namespace","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"metricsToSend","default":{"kind":"nothing"},"name":"metricsToSend","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"CloudWatchMetricLogConfig","version":{"kind":"nothing"}}};

export const snCloudWatchMetricLogConfig: ADL.ScopedName = {moduleName:"common.config.log", name:"CloudWatchMetricLogConfig"};

export function texprCloudWatchMetricLogConfig(): ADL.ATypeExpr<CloudWatchMetricLogConfig> {
  return {value:{typeRef:{kind:"reference",value:snCloudWatchMetricLogConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.log.LogLevel" : LogLevel_AST,
  "common.config.log.StdoutConfig" : StdoutConfig_AST,
  "common.config.log.FluentdConfig" : FluentdConfig_AST,
  "common.config.log.RollbarConfig" : RollbarConfig_AST,
  "common.config.log.LogConfig" : LogConfig_AST,
  "common.config.log.ClientRollbarConfig" : ClientRollbarConfig_AST,
  "common.config.log.ClientLogConfig" : ClientLogConfig_AST,
  "common.config.log.CloudWatchMetricLogConfig" : CloudWatchMetricLogConfig_AST
};
