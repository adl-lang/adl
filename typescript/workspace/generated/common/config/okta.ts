import * as ADL from "@adl-lang/runtime/adl";
import * as sys_types from "@adl-lang/sys/types";

/* @generated from adl module common.config.okta */

/**
 * Config parameters for setting up open ID authentication using Okta
 */
export interface OktaConfig {
/**
 * ID of the client application
 */
  clientId: string;
/**
 * Secret of the client application
 */
  clientSecret: string;
/**
 * Hostname of the okta instance
 */
  clientHost: string;
/**
 * Endpoint which users should be redirected to to complete authorization
 */
  authorizeEndpoint: string;
/**
 * The URL supplied to okta to redirect the user to after authorization is
 * complete
 */
  redirectUrl: string;
/**
 * API key used for performing actions in okta, not required if the app does
 * not use this functionality
 */
  apiKey: sys_types.Maybe<string>;
}

export function makeOktaConfig(
  input: {
    clientId: string,
    clientSecret: string,
    clientHost: string,
    authorizeEndpoint: string,
    redirectUrl: string,
    apiKey: sys_types.Maybe<string>,
  }
): OktaConfig {
  return {
    clientId: input.clientId,
    clientSecret: input.clientSecret,
    clientHost: input.clientHost,
    authorizeEndpoint: input.authorizeEndpoint,
    redirectUrl: input.redirectUrl,
    apiKey: input.apiKey,
  };
}

const OktaConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.okta","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"clientId","default":{"kind":"nothing"},"name":"clientId","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"clientSecret","default":{"kind":"nothing"},"name":"clientSecret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"clientHost","default":{"kind":"nothing"},"name":"clientHost","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"authorizeEndpoint","default":{"kind":"nothing"},"name":"authorizeEndpoint","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"redirectUrl","default":{"kind":"nothing"},"name":"redirectUrl","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"apiKey","default":{"kind":"nothing"},"name":"apiKey","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"OktaConfig","version":{"kind":"nothing"}}};

export const snOktaConfig: ADL.ScopedName = {moduleName:"common.config.okta", name:"OktaConfig"};

export function texprOktaConfig(): ADL.ATypeExpr<OktaConfig> {
  return {value:{typeRef:{kind:"reference",value:snOktaConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.okta.OktaConfig" : OktaConfig_AST
};
