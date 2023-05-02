import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module protoclient.protoapp.uiconfig */

/**
 * Configuration for the web frontend.
 */
export interface UiConfig {
/**
 * Application title
 */
  title: string;
/**
 * Environment name, e.g. dev, uat, prod
 */
  environment: string;
/**
 * Name of the release / code version
 */
  releaseName: string;
}

export function makeUiConfig(
  input: {
    title?: string,
    environment: string,
    releaseName: string,
  }
): UiConfig {
  return {
    title: input.title === undefined ? "" : input.title,
    environment: input.environment,
    releaseName: input.releaseName,
  };
}

const UiConfig_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.uiconfig","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"title","default":{"kind":"just","value":""},"name":"title","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"environment","default":{"kind":"nothing"},"name":"environment","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"releaseName","default":{"kind":"nothing"},"name":"releaseName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"UiConfig","version":{"kind":"nothing"}}};

export const snUiConfig: ADL.ScopedName = {moduleName:"protoclient.protoapp.uiconfig", name:"UiConfig"};

export function texprUiConfig(): ADL.ATypeExpr<UiConfig> {
  return {value:{typeRef:{kind:"reference",value:snUiConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "protoclient.protoapp.uiconfig.UiConfig" : UiConfig_AST
};
