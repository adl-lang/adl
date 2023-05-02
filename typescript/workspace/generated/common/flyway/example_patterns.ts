import * as common_flyway_internals from "./internals";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.flyway.example_patterns */

export interface FlywayConfig_Mixed_mode_latest {
  kind: 'mixed_mode_latest';
}
export interface FlywayConfig_Mixed_mode_manual {
  kind: 'mixed_mode_manual';
}
export interface FlywayConfig_Migration_mode_latest {
  kind: 'migration_mode_latest';
}
export interface FlywayConfig_Advanced {
  kind: 'advanced';
  value: common_flyway_internals.FlywayCommand;
}

export type FlywayConfig = FlywayConfig_Mixed_mode_latest | FlywayConfig_Mixed_mode_manual | FlywayConfig_Migration_mode_latest | FlywayConfig_Advanced;

export interface FlywayConfigOpts {
  mixed_mode_latest: null;
  mixed_mode_manual: null;
  migration_mode_latest: null;
  advanced: common_flyway_internals.FlywayCommand;
}

export function makeFlywayConfig<K extends keyof FlywayConfigOpts>(kind: K, value: FlywayConfigOpts[K]) { return {kind, value}; }

const FlywayConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.example_patterns","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"mixed_mode_latest","default":{"kind":"nothing"},"name":"mixed_mode_latest","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"mixed_mode_manual","default":{"kind":"nothing"},"name":"mixed_mode_manual","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"migration_mode_latest","default":{"kind":"nothing"},"name":"migration_mode_latest","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"advanced","default":{"kind":"nothing"},"name":"advanced","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayCommand"}},"parameters":[]}}]}},"name":"FlywayConfig","version":{"kind":"nothing"}}};

export const snFlywayConfig: ADL.ScopedName = {moduleName:"common.flyway.example_patterns", name:"FlywayConfig"};

export function texprFlywayConfig(): ADL.ATypeExpr<FlywayConfig> {
  return {value:{typeRef:{kind:"reference",value:snFlywayConfig},parameters:[]}};
}

export type ExampleFlywayCommand = {[key: string]: common_flyway_internals.FlywayCommand};

const ExampleFlywayCommand_AST : ADL.ScopedDecl =
  {"moduleName":"common.flyway.example_patterns","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.internals","name":"FlywayCommand"}},"parameters":[]}]}}},"name":"ExampleFlywayCommand","version":{"kind":"nothing"}}};

export const snExampleFlywayCommand: ADL.ScopedName = {moduleName:"common.flyway.example_patterns", name:"ExampleFlywayCommand"};

export function texprExampleFlywayCommand(): ADL.ATypeExpr<ExampleFlywayCommand> {
  return {value:{typeRef:{kind:"reference",value:snExampleFlywayCommand},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.flyway.example_patterns.FlywayConfig" : FlywayConfig_AST,
  "common.flyway.example_patterns.ExampleFlywayCommand" : ExampleFlywayCommand_AST
};
