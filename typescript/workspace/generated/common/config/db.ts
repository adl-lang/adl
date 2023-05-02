import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.db */

export interface PostgreSqlConfig {
  host: string;
  port: number;
  dbname: string;
  user: string;
  password: string;
  poolSize: number;
  postgisEnabled: boolean;
/**
 * The amount of time to wait before refreshing a connection
 */
  minRefreshDelayMillis: number;
/**
 * The amount of time to wait for a free connection before timing out
 */
  connectionWaitMillis: number;
/**
 * The root cert to be used for ssl connections
 */
  sslRootCert: (string|null);
/**
 * The ssl connection mode
 */
  sslMode: PostgresqlSslMode;
}

export function makePostgreSqlConfig(
  input: {
    host: string,
    port: number,
    dbname: string,
    user: string,
    password: string,
    poolSize?: number,
    postgisEnabled?: boolean,
    minRefreshDelayMillis?: number,
    connectionWaitMillis?: number,
    sslRootCert?: (string|null),
    sslMode?: PostgresqlSslMode,
  }
): PostgreSqlConfig {
  return {
    host: input.host,
    port: input.port,
    dbname: input.dbname,
    user: input.user,
    password: input.password,
    poolSize: input.poolSize === undefined ? 5 : input.poolSize,
    postgisEnabled: input.postgisEnabled === undefined ? false : input.postgisEnabled,
    minRefreshDelayMillis: input.minRefreshDelayMillis === undefined ? 60000 : input.minRefreshDelayMillis,
    connectionWaitMillis: input.connectionWaitMillis === undefined ? 5000 : input.connectionWaitMillis,
    sslRootCert: input.sslRootCert === undefined ? null : input.sslRootCert,
    sslMode: input.sslMode === undefined ? "prefer" : input.sslMode,
  };
}

const PostgreSqlConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"host","default":{"kind":"nothing"},"name":"host","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"dbname","default":{"kind":"nothing"},"name":"dbname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"user","default":{"kind":"nothing"},"name":"user","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"poolSize","default":{"kind":"just","value":5},"name":"poolSize","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"postgisEnabled","default":{"kind":"just","value":false},"name":"postgisEnabled","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"minRefreshDelayMillis","default":{"kind":"just","value":60000},"name":"minRefreshDelayMillis","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"connectionWaitMillis","default":{"kind":"just","value":5000},"name":"connectionWaitMillis","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"sslRootCert","default":{"kind":"just","value":null},"name":"sslRootCert","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"sslMode","default":{"kind":"just","value":"prefer"},"name":"sslMode","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.db","name":"PostgresqlSslMode"}},"parameters":[]}}]}},"name":"PostgreSqlConfig","version":{"kind":"nothing"}}};

export const snPostgreSqlConfig: ADL.ScopedName = {moduleName:"common.config.db", name:"PostgreSqlConfig"};

export function texprPostgreSqlConfig(): ADL.ATypeExpr<PostgreSqlConfig> {
  return {value:{typeRef:{kind:"reference",value:snPostgreSqlConfig},parameters:[]}};
}

export type PostgresqlSslMode = 'disable' | 'allow' | 'prefer' | 'require' | 'verifyCa' | 'verifyFull';
export const valuesPostgresqlSslMode : PostgresqlSslMode[] = ['disable', 'allow', 'prefer', 'require', 'verifyCa', 'verifyFull'];

const PostgresqlSslMode_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.db","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"disable","default":{"kind":"nothing"},"name":"disable","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"allow","default":{"kind":"nothing"},"name":"allow","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"prefer","default":{"kind":"nothing"},"name":"prefer","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"require","default":{"kind":"nothing"},"name":"require","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"verify-ca","default":{"kind":"nothing"},"name":"verifyCa","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"verify-full","default":{"kind":"nothing"},"name":"verifyFull","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"PostgresqlSslMode","version":{"kind":"nothing"}}};

export const snPostgresqlSslMode: ADL.ScopedName = {moduleName:"common.config.db", name:"PostgresqlSslMode"};

export function texprPostgresqlSslMode(): ADL.ATypeExpr<PostgresqlSslMode> {
  return {value:{typeRef:{kind:"reference",value:snPostgresqlSslMode},parameters:[]}};
}

export interface SqlServerConfig {
  host: string;
  port: number;
  dbname: string;
  user: string;
  password: string;
  poolSize: number;
}

export function makeSqlServerConfig(
  input: {
    host: string,
    port: number,
    dbname: string,
    user: string,
    password: string,
    poolSize?: number,
  }
): SqlServerConfig {
  return {
    host: input.host,
    port: input.port,
    dbname: input.dbname,
    user: input.user,
    password: input.password,
    poolSize: input.poolSize === undefined ? 5 : input.poolSize,
  };
}

const SqlServerConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"host","default":{"kind":"nothing"},"name":"host","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"port","default":{"kind":"nothing"},"name":"port","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"dbname","default":{"kind":"nothing"},"name":"dbname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"user","default":{"kind":"nothing"},"name":"user","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"poolSize","default":{"kind":"just","value":5},"name":"poolSize","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"SqlServerConfig","version":{"kind":"nothing"}}};

export const snSqlServerConfig: ADL.ScopedName = {moduleName:"common.config.db", name:"SqlServerConfig"};

export function texprSqlServerConfig(): ADL.ATypeExpr<SqlServerConfig> {
  return {value:{typeRef:{kind:"reference",value:snSqlServerConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.db.PostgreSqlConfig" : PostgreSqlConfig_AST,
  "common.config.db.PostgresqlSslMode" : PostgresqlSslMode_AST,
  "common.config.db.SqlServerConfig" : SqlServerConfig_AST
};
