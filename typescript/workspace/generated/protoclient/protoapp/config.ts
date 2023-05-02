import * as common_config_db from "@adl-lang/common/config/db";
import * as common_config_frontend from "@adl-lang/common/config/frontend";
import * as common_config_log from "@adl-lang/common/config/log";
import * as common_flyway_internals from "@adl-lang/common/flyway/internals";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module protoclient.protoapp.config */

/**
 * Configuration for the server
 */
export interface ServerConfig {
/**
 * Port to listen for requests
 */
  port: number;
/**
 * Environment name, e.g. dev, uat, prod
 */
  environment: string;
/**
 * Name of the release / code version
 */
  releaseName: string;
/**
 * The shared secret that will be present on requests
 * made by the cron webhook system.
 */
  cronSharedSecret: string;
/**
 * Secret used to sign JWT tokens
 */
  jwtSecret: string;
/**
 * Database config
 */
  db: common_config_db.PostgreSqlConfig;
/**
 * Server-side logging config
 */
  logging: common_config_log.LogConfig;
/**
 * Client-side logging config
 * If not provided all client logs will appear in the server logs
 */
  clientLogging: (ClientLogConfig|null);
/**
 * Frontend threadpool configuration
 */
  threadPool: common_config_frontend.ThreadPoolConfig;
  flyway: (LocalFlywayConfig|null);
}

export function makeServerConfig(
  input: {
    port: number,
    environment: string,
    releaseName: string,
    cronSharedSecret: string,
    jwtSecret: string,
    db: common_config_db.PostgreSqlConfig,
    logging: common_config_log.LogConfig,
    clientLogging?: (ClientLogConfig|null),
    threadPool: common_config_frontend.ThreadPoolConfig,
    flyway?: (LocalFlywayConfig|null),
  }
): ServerConfig {
  return {
    port: input.port,
    environment: input.environment,
    releaseName: input.releaseName,
    cronSharedSecret: input.cronSharedSecret,
    jwtSecret: input.jwtSecret,
    db: input.db,
    logging: input.logging,
    clientLogging: input.clientLogging === undefined ? null : input.clientLogging,
    threadPool: input.threadPool,
    flyway: input.flyway === undefined ? null : input.flyway,
  };
}

const ServerConfig_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}},{"annotations":[],"serializedName":"environment","default":{"kind":"nothing"},"name":"environment","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"releaseName","default":{"kind":"nothing"},"name":"releaseName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"cronSharedSecret","default":{"kind":"nothing"},"name":"cronSharedSecret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"jwtSecret","default":{"kind":"nothing"},"name":"jwtSecret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"db","default":{"kind":"nothing"},"name":"db","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.db","name":"PostgreSqlConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"logging","default":{"kind":"nothing"},"name":"logging","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"clientLogging","default":{"kind":"just","value":null},"name":"clientLogging","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.config","name":"ClientLogConfig"}},"parameters":[]}]}},{"annotations":[],"serializedName":"threadPool","default":{"kind":"nothing"},"name":"threadPool","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.frontend","name":"ThreadPoolConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"flyway","default":{"kind":"just","value":null},"name":"flyway","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.config","name":"LocalFlywayConfig"}},"parameters":[]}]}}]}},"name":"ServerConfig","version":{"kind":"nothing"}}};

export const snServerConfig: ADL.ScopedName = {moduleName:"protoclient.protoapp.config", name:"ServerConfig"};

export function texprServerConfig(): ADL.ATypeExpr<ServerConfig> {
  return {value:{typeRef:{kind:"reference",value:snServerConfig},parameters:[]}};
}

export type ClientLogConfig = common_config_log.FluentdConfig;

const ClientLogConfig_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.config","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"FluentdConfig"}},"parameters":[]}}},"name":"ClientLogConfig","version":{"kind":"nothing"}}};

export const snClientLogConfig: ADL.ScopedName = {moduleName:"protoclient.protoapp.config", name:"ClientLogConfig"};

export function texprClientLogConfig(): ADL.ATypeExpr<ClientLogConfig> {
  return {value:{typeRef:{kind:"reference",value:snClientLogConfig},parameters:[]}};
}

export interface LocalFlywayConfig_Fastdev {
  kind: 'fastdev';
}
export interface LocalFlywayConfig_Deployed {
  kind: 'deployed';
}
export interface LocalFlywayConfig_Advanced {
  kind: 'advanced';
  value: common_flyway_internals.FlywayCommand;
}

export type LocalFlywayConfig = LocalFlywayConfig_Fastdev | LocalFlywayConfig_Deployed | LocalFlywayConfig_Advanced;

export interface LocalFlywayConfigOpts {
  fastdev: null;
  deployed: null;
  advanced: common_flyway_internals.FlywayCommand;
}

export function makeLocalFlywayConfig<K extends keyof LocalFlywayConfigOpts>(kind: K, value: LocalFlywayConfigOpts[K]) { return {kind, value}; }

const LocalFlywayConfig_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.config","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"fastdev","default":{"kind":"nothing"},"name":"fastdev","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"deployed","default":{"kind":"nothing"},"name":"deployed","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"advanced","default":{"kind":"nothing"},"name":"advanced","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayCommand"}},"parameters":[]}}]}},"name":"LocalFlywayConfig","version":{"kind":"nothing"}}};

export const snLocalFlywayConfig: ADL.ScopedName = {moduleName:"protoclient.protoapp.config", name:"LocalFlywayConfig"};

export function texprLocalFlywayConfig(): ADL.ATypeExpr<LocalFlywayConfig> {
  return {value:{typeRef:{kind:"reference",value:snLocalFlywayConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "protoclient.protoapp.config.ServerConfig" : ServerConfig_AST,
  "protoclient.protoapp.config.ClientLogConfig" : ClientLogConfig_AST,
  "protoclient.protoapp.config.LocalFlywayConfig" : LocalFlywayConfig_AST
};
