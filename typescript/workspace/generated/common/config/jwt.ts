import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.config.jwt */

/**
 * Secret and expiry of the JWT token
 */
export interface JwtConfig {
  secret: string;
  expiryInMinutes: number;
}

export function makeJwtConfig(
  input: {
    secret: string,
    expiryInMinutes: number,
  }
): JwtConfig {
  return {
    secret: input.secret,
    expiryInMinutes: input.expiryInMinutes,
  };
}

const JwtConfig_AST : ADL.ScopedDecl =
  {"moduleName":"common.config.jwt","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"secret","default":{"kind":"nothing"},"name":"secret","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"expiryInMinutes","default":{"kind":"nothing"},"name":"expiryInMinutes","typeExpr":{"typeRef":{"kind":"primitive","value":"Double"},"parameters":[]}}]}},"name":"JwtConfig","version":{"kind":"nothing"}}};

export const snJwtConfig: ADL.ScopedName = {moduleName:"common.config.jwt", name:"JwtConfig"};

export function texprJwtConfig(): ADL.ATypeExpr<JwtConfig> {
  return {value:{typeRef:{kind:"reference",value:snJwtConfig},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.config.jwt.JwtConfig" : JwtConfig_AST
};
