import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.flyway.internals */

export interface FlywayCommand {
  ctx: FlywayContext;
/**
 * The action to take on application startup
 */
  bootstrap: FlywayAction[];
  action: (FlywayAction|null);
}

export function makeFlywayCommand(
  input: {
    ctx: FlywayContext,
    bootstrap?: FlywayAction[],
    action: (FlywayAction|null),
  }
): FlywayCommand {
  return {
    ctx: input.ctx,
    bootstrap: input.bootstrap === undefined ? [] : input.bootstrap,
    action: input.action,
  };
}

const FlywayCommand_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ctx","default":{"kind":"nothing"},"name":"ctx","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayContext"}},"parameters":[]}},{"annotations":[],"serializedName":"bootstrap","default":{"kind":"just","value":[]},"name":"bootstrap","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayAction"}},"parameters":[]}]}},{"annotations":[],"serializedName":"action","default":{"kind":"nothing"},"name":"action","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayAction"}},"parameters":[]}]}}]}},"name":"FlywayCommand","version":{"kind":"nothing"}}};

export const snFlywayCommand: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"FlywayCommand"};

export function texprFlywayCommand(): ADL.ATypeExpr<FlywayCommand> {
  return {value:{typeRef:{kind:"reference",value:snFlywayCommand},parameters:[]}};
}

/**
 * Contextual information required to run flyway
 */
export interface FlywayContext {
/**
 * The name of the flyway history table.
 * Useful to override for bootstraping.
 */
  schemaHistoryTable: string;
/**
 * The location from which db migrations are loaded
 */
  sqlMigrationDir: string;
/**
 * The package from which java based db migrations are loaded
 */
  javaMigrationPackage: (string|null);
/**
 * DB connection retries, with 1 second between retries
 */
  connect_retries: number;
}

export function makeFlywayContext(
  input: {
    schemaHistoryTable?: string,
    sqlMigrationDir?: string,
    javaMigrationPackage?: (string|null),
    connect_retries?: number,
  }
): FlywayContext {
  return {
    schemaHistoryTable: input.schemaHistoryTable === undefined ? "flyway_schema_history" : input.schemaHistoryTable,
    sqlMigrationDir: input.sqlMigrationDir === undefined ? "/app/sql/migrations" : input.sqlMigrationDir,
    javaMigrationPackage: input.javaMigrationPackage === undefined ? "app/java/migrations" : input.javaMigrationPackage,
    connect_retries: input.connect_retries === undefined ? 3 : input.connect_retries,
  };
}

const FlywayContext_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"schemaHistoryTable","default":{"kind":"just","value":"flyway_schema_history"},"name":"schemaHistoryTable","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"sqlMigrationDir","default":{"kind":"just","value":"/app/sql/migrations"},"name":"sqlMigrationDir","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"javaMigrationPackage","default":{"kind":"just","value":"app/java/migrations"},"name":"javaMigrationPackage","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"connect_retries","default":{"kind":"just","value":3},"name":"connect_retries","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"FlywayContext","version":{"kind":"nothing"}}};

export const snFlywayContext: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"FlywayContext"};

export function texprFlywayContext(): ADL.ATypeExpr<FlywayContext> {
  return {value:{typeRef:{kind:"reference",value:snFlywayContext},parameters:[]}};
}

export interface FlywayAction_Info {
  kind: 'info';
}
export interface FlywayAction_Baseline_zero {
  kind: 'baseline_zero';
}
export interface FlywayAction_Baseline_one {
  kind: 'baseline_one';
}
export interface FlywayAction_Baseline_latest {
  kind: 'baseline_latest';
}
export interface FlywayAction_Baseline_version {
  kind: 'baseline_version';
  value: string;
}
export interface FlywayAction_Migrate {
  kind: 'migrate';
}
export interface FlywayAction_Migrate_latest {
  kind: 'migrate_latest';
  value: MigrateLatestAction;
}
export interface FlywayAction_Migrate_to_version {
  kind: 'migrate_to_version';
  value: MigrateAction;
}
export interface FlywayAction_Sequence {
  kind: 'sequence';
  value: FlywayAction[];
}
export interface FlywayAction_WithContext {
  kind: 'withContext';
  value: FlywayCtxAction;
}

export type FlywayAction = FlywayAction_Info | FlywayAction_Baseline_zero | FlywayAction_Baseline_one | FlywayAction_Baseline_latest | FlywayAction_Baseline_version | FlywayAction_Migrate | FlywayAction_Migrate_latest | FlywayAction_Migrate_to_version | FlywayAction_Sequence | FlywayAction_WithContext;

export interface FlywayActionOpts {
  /**
 * Get migration info, the a combintation of the flyway schema history and future migrations.
   */
info: null;
  baseline_zero: null;
  baseline_one: null;
  baseline_latest: null;
  baseline_version: string;
  /**
 * Migrate to lastest with default options
   */
migrate: null;
  migrate_latest: MigrateLatestAction;
  migrate_to_version: MigrateAction;
  sequence: FlywayAction[];
  withContext: FlywayCtxAction;
}

export function makeFlywayAction<K extends keyof FlywayActionOpts>(kind: K, value: FlywayActionOpts[K]) { return {kind, value}; }

const FlywayAction_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"info","default":{"kind":"nothing"},"name":"info","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"baseline_zero","default":{"kind":"nothing"},"name":"baseline_zero","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"baseline_one","default":{"kind":"nothing"},"name":"baseline_one","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"baseline_latest","default":{"kind":"nothing"},"name":"baseline_latest","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"baseline_version","default":{"kind":"nothing"},"name":"baseline_version","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"migrate","default":{"kind":"nothing"},"name":"migrate","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"migrate_latest","default":{"kind":"nothing"},"name":"migrate_latest","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"MigrateLatestAction"}},"parameters":[]}},{"annotations":[],"serializedName":"migrate_to_version","default":{"kind":"nothing"},"name":"migrate_to_version","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"MigrateAction"}},"parameters":[]}},{"annotations":[],"serializedName":"sequence","default":{"kind":"nothing"},"name":"sequence","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayAction"}},"parameters":[]}]}},{"annotations":[],"serializedName":"withContext","default":{"kind":"nothing"},"name":"withContext","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayCtxAction"}},"parameters":[]}}]}},"name":"FlywayAction","version":{"kind":"nothing"}}};

export const snFlywayAction: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"FlywayAction"};

export function texprFlywayAction(): ADL.ATypeExpr<FlywayAction> {
  return {value:{typeRef:{kind:"reference",value:snFlywayAction},parameters:[]}};
}

export interface FlywayCtxAction {
/**
 * The context in normal (non-bootstrap) mode
 */
  ctx: FlywayContext;
/**
 * Array of actions to run on bootstrap
 */
  action: FlywayAction;
}

export function makeFlywayCtxAction(
  input: {
    ctx: FlywayContext,
    action: FlywayAction,
  }
): FlywayCtxAction {
  return {
    ctx: input.ctx,
    action: input.action,
  };
}

const FlywayCtxAction_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ctx","default":{"kind":"nothing"},"name":"ctx","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayContext"}},"parameters":[]}},{"annotations":[],"serializedName":"action","default":{"kind":"nothing"},"name":"action","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayAction"}},"parameters":[]}}]}},"name":"FlywayCtxAction","version":{"kind":"nothing"}}};

export const snFlywayCtxAction: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"FlywayCtxAction"};

export function texprFlywayCtxAction(): ADL.ATypeExpr<FlywayCtxAction> {
  return {value:{typeRef:{kind:"reference",value:snFlywayCtxAction},parameters:[]}};
}

export interface MigrateLatestAction {
/**
 * migration options
 */
  options: MigrateOptionsOptions;
}

export function makeMigrateLatestAction(
  input: {
    options?: MigrateOptionsOptions,
  }
): MigrateLatestAction {
  return {
    options: input.options === undefined ? {kind : "defaults"} : input.options,
  };
}

const MigrateLatestAction_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"options","default":{"kind":"just","value":"defaults"},"name":"options","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"MigrateOptionsOptions"}},"parameters":[]}}]}},"name":"MigrateLatestAction","version":{"kind":"nothing"}}};

export const snMigrateLatestAction: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"MigrateLatestAction"};

export function texprMigrateLatestAction(): ADL.ATypeExpr<MigrateLatestAction> {
  return {value:{typeRef:{kind:"reference",value:snMigrateLatestAction},parameters:[]}};
}

export interface MigrateAction {
/**
 * migration options
 */
  options: MigrateOptionsOptions;
  version: string;
}

export function makeMigrateAction(
  input: {
    options?: MigrateOptionsOptions,
    version: string,
  }
): MigrateAction {
  return {
    options: input.options === undefined ? {kind : "defaults"} : input.options,
    version: input.version,
  };
}

const MigrateAction_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"options","default":{"kind":"just","value":"defaults"},"name":"options","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"MigrateOptionsOptions"}},"parameters":[]}},{"annotations":[],"serializedName":"version","default":{"kind":"nothing"},"name":"version","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"MigrateAction","version":{"kind":"nothing"}}};

export const snMigrateAction: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"MigrateAction"};

export function texprMigrateAction(): ADL.ATypeExpr<MigrateAction> {
  return {value:{typeRef:{kind:"reference",value:snMigrateAction},parameters:[]}};
}

export interface MigrateOptionsOptions_Defaults {
  kind: 'defaults';
}
export interface MigrateOptionsOptions_IgnoreMissingMigrations {
  kind: 'ignoreMissingMigrations';
}
export interface MigrateOptionsOptions_IgnoreIgnoredMigrations {
  kind: 'ignoreIgnoredMigrations';
}
export interface MigrateOptionsOptions_OutOfOrder {
  kind: 'outOfOrder';
}
export interface MigrateOptionsOptions_DontValidateOnMigrate {
  kind: 'dontValidateOnMigrate';
}
export interface MigrateOptionsOptions_Custom {
  kind: 'custom';
  value: MigrateOptions;
}

export type MigrateOptionsOptions = MigrateOptionsOptions_Defaults | MigrateOptionsOptions_IgnoreMissingMigrations | MigrateOptionsOptions_IgnoreIgnoredMigrations | MigrateOptionsOptions_OutOfOrder | MigrateOptionsOptions_DontValidateOnMigrate | MigrateOptionsOptions_Custom;

export interface MigrateOptionsOptionsOpts {
  defaults: null;
  ignoreMissingMigrations: null;
  ignoreIgnoredMigrations: null;
  outOfOrder: null;
  dontValidateOnMigrate: null;
  custom: MigrateOptions;
}

export function makeMigrateOptionsOptions<K extends keyof MigrateOptionsOptionsOpts>(kind: K, value: MigrateOptionsOptionsOpts[K]) { return {kind, value}; }

const MigrateOptionsOptions_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"defaults","default":{"kind":"nothing"},"name":"defaults","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"ignoreMissingMigrations","default":{"kind":"nothing"},"name":"ignoreMissingMigrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"ignoreIgnoredMigrations","default":{"kind":"nothing"},"name":"ignoreIgnoredMigrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"outOfOrder","default":{"kind":"nothing"},"name":"outOfOrder","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"dontValidateOnMigrate","default":{"kind":"nothing"},"name":"dontValidateOnMigrate","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"custom","default":{"kind":"just","value":{}},"name":"custom","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"MigrateOptions"}},"parameters":[]}}]}},"name":"MigrateOptionsOptions","version":{"kind":"nothing"}}};

export const snMigrateOptionsOptions: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"MigrateOptionsOptions"};

export function texprMigrateOptionsOptions(): ADL.ATypeExpr<MigrateOptionsOptions> {
  return {value:{typeRef:{kind:"reference",value:snMigrateOptionsOptions},parameters:[]}};
}

export interface MigrateOptions {
/**
 * Ignore missing migrations when reading the schema history table. These are migrations that were performed by an
 * older deployment of the application that are no longer available in this version.
 */
  ignoreMissingMigrations: boolean;
/**
 * Ignore ignored migrations when reading the schema history table. These are migrations that were added in between
 * already migrated migrations in this version.
 */
  ignoreIgnoredMigrations: boolean;
/**
 * Ignore pending migrations when reading the schema history table. These are migrations that are available on the
 * classpath but have not yet been performed by an application deployment.
 * This can be useful for verifying that in-development migration changes don't contain any validation-breaking changes
 * of migrations that have already been applied to a production environment, e.g. as part of a CI/CD process, without
 * failing because of the existence of new migration versions.
 */
  ignorePendingMigrations: boolean;
/**
 * Ignore future migrations when reading the schema history table. These are migrations that were performed by a
 * newer deployment of the application that are not yet available in this version. For example: we have migrations
 * available on the classpath up to version 3.0. The schema history table indicates that a migration to version 4.0
 * (unknown to us) has already been applied. Instead of bombing out (fail fast) with an exception, a
 * warning is logged and Flyway continues normally. This is useful for situations where one must be able to redeploy
 * an older version of the application after the database has been migrated by a newer one.
 */
  ignoreFutureMigrations: boolean;
/**
 * Whether to validate migrations and callbacks whose scripts do not obey the correct naming convention. A failure can be
 * useful to check that errors such as case sensitivity in migration prefixes have been corrected.
 */
  validateMigrationNaming: boolean;
/**
 * Whether to automatically call validate or not when running migrate. (default: {@code true})
 */
  validateOnMigrate: boolean;
/**
 * Whether to automatically call clean or not when a validation error occurs. (default: {@code false})
 * <p> This is exclusively intended as a convenience for development. even though we
 * strongly recommend not to change migration scripts once they have been checked into SCM and run, this provides a
 * way of dealing with this case in a smooth manner. The database will be wiped clean automatically, ensuring that
 * the next migration will bring you back to the state checked into SCM.</p>
 * <p><b>Warning ! Do not enable in production !</b></p>
 */
  cleanOnValidationError: boolean;
/**
 * Whether to disable clean. (default: {@code false})
 * <p>This is especially useful for production environments where running clean can be quite a career limiting move.</p>
 */
  cleanDisabled: boolean;
/**
 * Allows migrations to be run "out of order".
 * <p>If you already have versions 1 and 3 applied, and now a version 2 is found,
 * it will be applied too instead of being ignored.</p>
 */
  outOfOrder: boolean;
}

export function makeMigrateOptions(
  input: {
    ignoreMissingMigrations?: boolean,
    ignoreIgnoredMigrations?: boolean,
    ignorePendingMigrations?: boolean,
    ignoreFutureMigrations?: boolean,
    validateMigrationNaming?: boolean,
    validateOnMigrate?: boolean,
    cleanOnValidationError?: boolean,
    cleanDisabled?: boolean,
    outOfOrder?: boolean,
  }
): MigrateOptions {
  return {
    ignoreMissingMigrations: input.ignoreMissingMigrations === undefined ? true : input.ignoreMissingMigrations,
    ignoreIgnoredMigrations: input.ignoreIgnoredMigrations === undefined ? true : input.ignoreIgnoredMigrations,
    ignorePendingMigrations: input.ignorePendingMigrations === undefined ? false : input.ignorePendingMigrations,
    ignoreFutureMigrations: input.ignoreFutureMigrations === undefined ? true : input.ignoreFutureMigrations,
    validateMigrationNaming: input.validateMigrationNaming === undefined ? false : input.validateMigrationNaming,
    validateOnMigrate: input.validateOnMigrate === undefined ? true : input.validateOnMigrate,
    cleanOnValidationError: input.cleanOnValidationError === undefined ? false : input.cleanOnValidationError,
    cleanDisabled: input.cleanDisabled === undefined ? false : input.cleanDisabled,
    outOfOrder: input.outOfOrder === undefined ? false : input.outOfOrder,
  };
}

const MigrateOptions_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.internals","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ignoreMissingMigrations","default":{"kind":"just","value":true},"name":"ignoreMissingMigrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"ignoreIgnoredMigrations","default":{"kind":"just","value":true},"name":"ignoreIgnoredMigrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"ignorePendingMigrations","default":{"kind":"just","value":false},"name":"ignorePendingMigrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"ignoreFutureMigrations","default":{"kind":"just","value":true},"name":"ignoreFutureMigrations","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"validateMigrationNaming","default":{"kind":"just","value":false},"name":"validateMigrationNaming","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"validateOnMigrate","default":{"kind":"just","value":true},"name":"validateOnMigrate","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"cleanOnValidationError","default":{"kind":"just","value":false},"name":"cleanOnValidationError","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"cleanDisabled","default":{"kind":"just","value":false},"name":"cleanDisabled","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"outOfOrder","default":{"kind":"just","value":false},"name":"outOfOrder","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"MigrateOptions","version":{"kind":"nothing"}}};

export const snMigrateOptions: ADL.ScopedName = {moduleName:"common.flyway.internals", name:"MigrateOptions"};

export function texprMigrateOptions(): ADL.ATypeExpr<MigrateOptions> {
  return {value:{typeRef:{kind:"reference",value:snMigrateOptions},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.flyway.internals.FlywayCommand" : FlywayCommand_AST,
  "common.flyway.internals.FlywayContext" : FlywayContext_AST,
  "common.flyway.internals.FlywayAction" : FlywayAction_AST,
  "common.flyway.internals.FlywayCtxAction" : FlywayCtxAction_AST,
  "common.flyway.internals.MigrateLatestAction" : MigrateLatestAction_AST,
  "common.flyway.internals.MigrateAction" : MigrateAction_AST,
  "common.flyway.internals.MigrateOptionsOptions" : MigrateOptionsOptions_AST,
  "common.flyway.internals.MigrateOptions" : MigrateOptions_AST
};
