import * as common_config_db from "./../config/db";
import * as common_config_frontend from "./../config/frontend";
import * as common_config_log from "./../config/log";
import * as common_flyway_example_patterns from "./../flyway/example_patterns";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.adminui.config */

/**
 * Configuration for the adminui server
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
 * Frontend threadpool configuration
 */
  threadPool: common_config_frontend.ThreadPoolConfig;
  flyway: (common_flyway_example_patterns.FlywayConfig|null);
}

export function makeServerConfig(
  input: {
    port: number,
    environment: string,
    releaseName: string,
    jwtSecret: string,
    db: common_config_db.PostgreSqlConfig,
    logging: common_config_log.LogConfig,
    threadPool: common_config_frontend.ThreadPoolConfig,
    flyway?: (common_flyway_example_patterns.FlywayConfig|null),
  }
): ServerConfig {
  return {
    port: input.port,
    environment: input.environment,
    releaseName: input.releaseName,
    jwtSecret: input.jwtSecret,
    db: input.db,
    logging: input.logging,
    threadPool: input.threadPool,
    flyway: input.flyway === undefined ? null : input.flyway,
  };
}

const ServerConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.config","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int16"},"parameters":[]}},{"annotations":[],"serializedName":"environment","default":{"kind":"nothing"},"name":"environment","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"releaseName","default":{"kind":"nothing"},"name":"releaseName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"jwtSecret","default":{"kind":"nothing"},"name":"jwtSecret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"db","default":{"kind":"nothing"},"name":"db","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.db","name":"PostgreSqlConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"logging","default":{"kind":"nothing"},"name":"logging","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"threadPool","default":{"kind":"nothing"},"name":"threadPool","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.frontend","name":"ThreadPoolConfig"}},"parameters":[]}},{"annotations":[],"serializedName":"flyway","default":{"kind":"just","value":null},"name":"flyway","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.example_patterns","name":"FlywayConfig"}},"parameters":[]}]}}]}},"name":"ServerConfig","version":{"kind":"nothing"}}};

export const snServerConfig: ADL.ScopedName = {moduleName:"common.adminui.config", name:"ServerConfig"};

export function texprServerConfig(): ADL.ATypeExpr<ServerConfig> {
  return {value:{typeRef:{kind:"reference",value:snServerConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.adminui.config.ServerConfig" : ServerConfig_AST
};
