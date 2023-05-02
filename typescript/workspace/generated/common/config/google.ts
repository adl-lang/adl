import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.google */

/**
 * Connection parameters for google services
 */
export interface GoogleConfig {
  clientId: string;
  clientSecret: string;
  accessToken: string;
  refreshToken: string;
}

export function makeGoogleConfig(
  input: {
    clientId: string,
    clientSecret: string,
    accessToken: string,
    refreshToken: string,
  }
): GoogleConfig {
  return {
    clientId: input.clientId,
    clientSecret: input.clientSecret,
    accessToken: input.accessToken,
    refreshToken: input.refreshToken,
  };
}

const GoogleConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.google","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"clientId","default":{"kind":"nothing"},"name":"clientId","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"clientSecret","default":{"kind":"nothing"},"name":"clientSecret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"accessToken","default":{"kind":"nothing"},"name":"accessToken","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"refreshToken","default":{"kind":"nothing"},"name":"refreshToken","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"GoogleConfig","version":{"kind":"nothing"}}};

export const snGoogleConfig: ADL.ScopedName = {moduleName:"common.config.google", name:"GoogleConfig"};

export function texprGoogleConfig(): ADL.ATypeExpr<GoogleConfig> {
  return {value:{typeRef:{kind:"reference",value:snGoogleConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.google.GoogleConfig" : GoogleConfig_AST
};
