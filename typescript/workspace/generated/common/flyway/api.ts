import * as common_http from "./../http";
import * as common_flyway_internals from "./internals";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.flyway.api */

export type FlywayApi = common_http.HttpPost<FlywayReq, FlywayResp>;

const FlywayApi_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayResp"}},"parameters":[]}]}}},"name":"FlywayApi","version":{"kind":"nothing"}}};

export const snFlywayApi: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayApi"};

export function texprFlywayApi(): ADL.ATypeExpr<FlywayApi> {
  return {value:{typeRef:{kind:"reference",value:snFlywayApi},parameters:[]}};
}

export interface FlywayReq_Plan {
  kind: 'plan';
}
export interface FlywayReq_Apply {
  kind: 'apply';
}
export interface FlywayReq_Advanced {
  kind: 'advanced';
  value: AdvancedFlywayReq;
}

export type FlywayReq = FlywayReq_Plan | FlywayReq_Apply | FlywayReq_Advanced;

export interface FlywayReqOpts {
  /**
 * Show pending migrations
   */
plan: null;
  /**
 * Apply pending migrations
   */
apply: null;
  /**
 * Something customized
   */
advanced: AdvancedFlywayReq;
}

export function makeFlywayReq<K extends keyof FlywayReqOpts>(kind: K, value: FlywayReqOpts[K]) { return {kind, value}; }

const FlywayReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"plan","default":{"kind":"nothing"},"name":"plan","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"apply","default":{"kind":"nothing"},"name":"apply","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"advanced","default":{"kind":"nothing"},"name":"advanced","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"AdvancedFlywayReq"}},"parameters":[]}}]}},"name":"FlywayReq","version":{"kind":"nothing"}}};

export const snFlywayReq: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayReq"};

export function texprFlywayReq(): ADL.ATypeExpr<FlywayReq> {
  return {value:{typeRef:{kind:"reference",value:snFlywayReq},parameters:[]}};
}

/**
 * Information passed when access via API
 */
export interface AdvancedFlywayReq {
  context: FlywayReqContextOpt;
  action: common_flyway_internals.FlywayAction;
}

export function makeAdvancedFlywayReq(
  input: {
    context?: FlywayReqContextOpt,
    action: common_flyway_internals.FlywayAction,
  }
): AdvancedFlywayReq {
  return {
    context: input.context === undefined ? {kind : "application"} : input.context,
    action: input.action,
  };
}

const AdvancedFlywayReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"context","default":{"kind":"just","value":"application"},"name":"context","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayReqContextOpt"}},"parameters":[]}},{"annotations":[],"serializedName":"action","default":{"kind":"nothing"},"name":"action","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayAction"}},"parameters":[]}}]}},"name":"AdvancedFlywayReq","version":{"kind":"nothing"}}};

export const snAdvancedFlywayReq: ADL.ScopedName = {moduleName:"common.flyway.api", name:"AdvancedFlywayReq"};

export function texprAdvancedFlywayReq(): ADL.ATypeExpr<AdvancedFlywayReq> {
  return {value:{typeRef:{kind:"reference",value:snAdvancedFlywayReq},parameters:[]}};
}

export interface FlywayReqContextOpt_Application {
  kind: 'application';
}
export interface FlywayReqContextOpt_Bootstrap {
  kind: 'bootstrap';
}
export interface FlywayReqContextOpt_CustomCtx {
  kind: 'customCtx';
  value: common_flyway_internals.FlywayContext;
}

export type FlywayReqContextOpt = FlywayReqContextOpt_Application | FlywayReqContextOpt_Bootstrap | FlywayReqContextOpt_CustomCtx;

export interface FlywayReqContextOptOpts {
  /**
 * Derive the context from the app config
   * Throw exception if it app is not configured with a flyway context
   */
application: null;
  /**
 * The first ctx in bootstrapable->bootstrap.
   * Throw exception if it doesn't exist
   */
bootstrap: null;
  /**
 * Overide the application flyway config
   */
customCtx: common_flyway_internals.FlywayContext;
}

export function makeFlywayReqContextOpt<K extends keyof FlywayReqContextOptOpts>(kind: K, value: FlywayReqContextOptOpts[K]) { return {kind, value}; }

const FlywayReqContextOpt_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"application","default":{"kind":"nothing"},"name":"application","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"bootstrap","default":{"kind":"nothing"},"name":"bootstrap","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"customCtx","default":{"kind":"nothing"},"name":"customCtx","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayContext"}},"parameters":[]}}]}},"name":"FlywayReqContextOpt","version":{"kind":"nothing"}}};

export const snFlywayReqContextOpt: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayReqContextOpt"};

export function texprFlywayReqContextOpt(): ADL.ATypeExpr<FlywayReqContextOpt> {
  return {value:{typeRef:{kind:"reference",value:snFlywayReqContextOpt},parameters:[]}};
}

export interface FlywayResp_Error {
  kind: 'error';
  value: string;
}
export interface FlywayResp_Msg {
  kind: 'msg';
  value: string;
}
export interface FlywayResp_Clean {
  kind: 'clean';
  value: FlywayCleanResult;
}
export interface FlywayResp_Info {
  kind: 'info';
  value: FlywayInfo;
}
export interface FlywayResp_MigrationResult {
  kind: 'migrationResult';
  value: FlywayMigrateResult;
}
export interface FlywayResp_BaselineResult {
  kind: 'baselineResult';
  value: FlywayBaselineResult;
}
export interface FlywayResp_Sequence {
  kind: 'sequence';
  value: FlywayResp[];
}

export type FlywayResp = FlywayResp_Error | FlywayResp_Msg | FlywayResp_Clean | FlywayResp_Info | FlywayResp_MigrationResult | FlywayResp_BaselineResult | FlywayResp_Sequence;

export interface FlywayRespOpts {
  error: string;
  msg: string;
  clean: FlywayCleanResult;
  info: FlywayInfo;
  migrationResult: FlywayMigrateResult;
  baselineResult: FlywayBaselineResult;
  sequence: FlywayResp[];
}

export function makeFlywayResp<K extends keyof FlywayRespOpts>(kind: K, value: FlywayRespOpts[K]) { return {kind, value}; }

const FlywayResp_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"error","default":{"kind":"nothing"},"name":"error","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"msg","default":{"kind":"nothing"},"name":"msg","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"clean","default":{"kind":"nothing"},"name":"clean","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayCleanResult"}},"parameters":[]}},{"annotations":[],"serializedName":"info","default":{"kind":"nothing"},"name":"info","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayInfo"}},"parameters":[]}},{"annotations":[],"serializedName":"migrationResult","default":{"kind":"nothing"},"name":"migrationResult","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayMigrateResult"}},"parameters":[]}},{"annotations":[],"serializedName":"baselineResult","default":{"kind":"nothing"},"name":"baselineResult","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayBaselineResult"}},"parameters":[]}},{"annotations":[],"serializedName":"sequence","default":{"kind":"nothing"},"name":"sequence","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayResp"}},"parameters":[]}]}}]}},"name":"FlywayResp","version":{"kind":"nothing"}}};

export const snFlywayResp: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayResp"};

export function texprFlywayResp(): ADL.ATypeExpr<FlywayResp> {
  return {value:{typeRef:{kind:"reference",value:snFlywayResp},parameters:[]}};
}

export interface FlywayInfo {
  schemaVersion: string;
  migrations: FlywayMigrationInfo[];
}

export function makeFlywayInfo(
  input: {
    schemaVersion: string,
    migrations: FlywayMigrationInfo[],
  }
): FlywayInfo {
  return {
    schemaVersion: input.schemaVersion,
    migrations: input.migrations,
  };
}

const FlywayInfo_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"schemaVersion","default":{"kind":"nothing"},"name":"schemaVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"migrations","default":{"kind":"nothing"},"name":"migrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayMigrationInfo"}},"parameters":[]}]}}]}},"name":"FlywayInfo","version":{"kind":"nothing"}}};

export const snFlywayInfo: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayInfo"};

export function texprFlywayInfo(): ADL.ATypeExpr<FlywayInfo> {
  return {value:{typeRef:{kind:"reference",value:snFlywayInfo},parameters:[]}};
}

export interface FlywayMigrationInfo {
  category: string;
  version: string;
  description: string;
  type_: string;
  installed_on: string;
  state: string;
}

export function makeFlywayMigrationInfo(
  input: {
    category: string,
    version: string,
    description: string,
    type_: string,
    installed_on: string,
    state: string,
  }
): FlywayMigrationInfo {
  return {
    category: input.category,
    version: input.version,
    description: input.description,
    type_: input.type_,
    installed_on: input.installed_on,
    state: input.state,
  };
}

const FlywayMigrationInfo_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"category","default":{"kind":"nothing"},"name":"category","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"version","default":{"kind":"nothing"},"name":"version","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"type_","default":{"kind":"nothing"},"name":"type_","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"installed_on","default":{"kind":"nothing"},"name":"installed_on","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"state","default":{"kind":"nothing"},"name":"state","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"FlywayMigrationInfo","version":{"kind":"nothing"}}};

export const snFlywayMigrationInfo: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayMigrationInfo"};

export function texprFlywayMigrationInfo(): ADL.ATypeExpr<FlywayMigrationInfo> {
  return {value:{typeRef:{kind:"reference",value:snFlywayMigrationInfo},parameters:[]}};
}

export interface FlywayMigrateResult {
  message: string;
  flywayVersion: string;
  database: string;
  warnings: string[];
  operation: string;
  initialSchemaVersion: (string|null);
  targetSchemaVersion: (string|null);
  schemaName: (string|null);
  migrations: FlywayMigrateOutput[];
  migrationsExecuted: (number|null);
}

export function makeFlywayMigrateResult(
  input: {
    message: string,
    flywayVersion: string,
    database: string,
    warnings: string[],
    operation: string,
    initialSchemaVersion: (string|null),
    targetSchemaVersion: (string|null),
    schemaName: (string|null),
    migrations: FlywayMigrateOutput[],
    migrationsExecuted: (number|null),
  }
): FlywayMigrateResult {
  return {
    message: input.message,
    flywayVersion: input.flywayVersion,
    database: input.database,
    warnings: input.warnings,
    operation: input.operation,
    initialSchemaVersion: input.initialSchemaVersion,
    targetSchemaVersion: input.targetSchemaVersion,
    schemaName: input.schemaName,
    migrations: input.migrations,
    migrationsExecuted: input.migrationsExecuted,
  };
}

const FlywayMigrateResult_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"message","default":{"kind":"nothing"},"name":"message","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"flywayVersion","default":{"kind":"nothing"},"name":"flywayVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"database","default":{"kind":"nothing"},"name":"database","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"warnings","default":{"kind":"nothing"},"name":"warnings","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"operation","default":{"kind":"nothing"},"name":"operation","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"initialSchemaVersion","default":{"kind":"nothing"},"name":"initialSchemaVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"targetSchemaVersion","default":{"kind":"nothing"},"name":"targetSchemaVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"schemaName","default":{"kind":"nothing"},"name":"schemaName","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"migrations","default":{"kind":"nothing"},"name":"migrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayMigrateOutput"}},"parameters":[]}]}},{"annotations":[],"serializedName":"migrationsExecuted","default":{"kind":"nothing"},"name":"migrationsExecuted","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}]}}]}},"name":"FlywayMigrateResult","version":{"kind":"nothing"}}};

export const snFlywayMigrateResult: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayMigrateResult"};

export function texprFlywayMigrateResult(): ADL.ATypeExpr<FlywayMigrateResult> {
  return {value:{typeRef:{kind:"reference",value:snFlywayMigrateResult},parameters:[]}};
}

export interface FlywayBaselineResult {
  flywayVersion: string;
  database: string;
  warnings: string[];
  operation: string;
  successfullyBaselined: boolean;
  baselineVersion: (string|null);
}

export function makeFlywayBaselineResult(
  input: {
    flywayVersion: string,
    database: string,
    warnings: string[],
    operation: string,
    successfullyBaselined: boolean,
    baselineVersion: (string|null),
  }
): FlywayBaselineResult {
  return {
    flywayVersion: input.flywayVersion,
    database: input.database,
    warnings: input.warnings,
    operation: input.operation,
    successfullyBaselined: input.successfullyBaselined,
    baselineVersion: input.baselineVersion,
  };
}

const FlywayBaselineResult_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"flywayVersion","default":{"kind":"nothing"},"name":"flywayVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"database","default":{"kind":"nothing"},"name":"database","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"warnings","default":{"kind":"nothing"},"name":"warnings","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"operation","default":{"kind":"nothing"},"name":"operation","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"successfullyBaselined","default":{"kind":"nothing"},"name":"successfullyBaselined","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"baselineVersion","default":{"kind":"nothing"},"name":"baselineVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"FlywayBaselineResult","version":{"kind":"nothing"}}};

export const snFlywayBaselineResult: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayBaselineResult"};

export function texprFlywayBaselineResult(): ADL.ATypeExpr<FlywayBaselineResult> {
  return {value:{typeRef:{kind:"reference",value:snFlywayBaselineResult},parameters:[]}};
}

export interface FlywayCleanResult {
  flywayVersion: string;
  database: string;
  warnings: string[];
  operation: string;
  schemasCleaned: string[];
  schemasDropped: string[];
}

export function makeFlywayCleanResult(
  input: {
    flywayVersion: string,
    database: string,
    warnings: string[],
    operation: string,
    schemasCleaned: string[],
    schemasDropped: string[],
  }
): FlywayCleanResult {
  return {
    flywayVersion: input.flywayVersion,
    database: input.database,
    warnings: input.warnings,
    operation: input.operation,
    schemasCleaned: input.schemasCleaned,
    schemasDropped: input.schemasDropped,
  };
}

const FlywayCleanResult_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"flywayVersion","default":{"kind":"nothing"},"name":"flywayVersion","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"database","default":{"kind":"nothing"},"name":"database","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"warnings","default":{"kind":"nothing"},"name":"warnings","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"operation","default":{"kind":"nothing"},"name":"operation","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"schemasCleaned","default":{"kind":"nothing"},"name":"schemasCleaned","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"schemasDropped","default":{"kind":"nothing"},"name":"schemasDropped","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"FlywayCleanResult","version":{"kind":"nothing"}}};

export const snFlywayCleanResult: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayCleanResult"};

export function texprFlywayCleanResult(): ADL.ATypeExpr<FlywayCleanResult> {
  return {value:{typeRef:{kind:"reference",value:snFlywayCleanResult},parameters:[]}};
}

export interface FlywayMigrateOutput {
  category: string;
  version: string;
  description: string;
  type_: string;
  filepath: string;
  executionTime: number;
}

export function makeFlywayMigrateOutput(
  input: {
    category: string,
    version: string,
    description: string,
    type_: string,
    filepath: string,
    executionTime: number,
  }
): FlywayMigrateOutput {
  return {
    category: input.category,
    version: input.version,
    description: input.description,
    type_: input.type_,
    filepath: input.filepath,
    executionTime: input.executionTime,
  };
}

const FlywayMigrateOutput_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"category","default":{"kind":"nothing"},"name":"category","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"version","default":{"kind":"nothing"},"name":"version","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"type_","default":{"kind":"nothing"},"name":"type_","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"filepath","default":{"kind":"nothing"},"name":"filepath","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"executionTime","default":{"kind":"nothing"},"name":"executionTime","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"FlywayMigrateOutput","version":{"kind":"nothing"}}};

export const snFlywayMigrateOutput: ADL.ScopedName = {moduleName:"common.flyway.api", name:"FlywayMigrateOutput"};

export function texprFlywayMigrateOutput(): ADL.ATypeExpr<FlywayMigrateOutput> {
  return {value:{typeRef:{kind:"reference",value:snFlywayMigrateOutput},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.flyway.api.FlywayApi" : FlywayApi_AST,
  "common.flyway.api.FlywayReq" : FlywayReq_AST,
  "common.flyway.api.AdvancedFlywayReq" : AdvancedFlywayReq_AST,
  "common.flyway.api.FlywayReqContextOpt" : FlywayReqContextOpt_AST,
  "common.flyway.api.FlywayResp" : FlywayResp_AST,
  "common.flyway.api.FlywayInfo" : FlywayInfo_AST,
  "common.flyway.api.FlywayMigrationInfo" : FlywayMigrationInfo_AST,
  "common.flyway.api.FlywayMigrateResult" : FlywayMigrateResult_AST,
  "common.flyway.api.FlywayBaselineResult" : FlywayBaselineResult_AST,
  "common.flyway.api.FlywayCleanResult" : FlywayCleanResult_AST,
  "common.flyway.api.FlywayMigrateOutput" : FlywayMigrateOutput_AST
};
