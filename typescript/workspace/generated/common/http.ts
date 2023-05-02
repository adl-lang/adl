import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.http */

/**
 * A marker type to associate the output response
 * type for an http Get request.
 */
export type Get<_I> = null;

const Get_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["I"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Get","version":{"kind":"nothing"}}};

export const snGet: ADL.ScopedName = {moduleName:"common.http", name:"Get"};

export function texprGet<I>(texprI : ADL.ATypeExpr<I>): ADL.ATypeExpr<Get<I>> {
  return {value:{typeRef:{kind:"reference",value:snGet},parameters:[texprI.value]}};
}

/**
 * A marker type to associate the input request and output response
 * types for an http put request.
 */
export type Put<_I, _O> = null;

const Put_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["I","O"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Put","version":{"kind":"nothing"}}};

export const snPut: ADL.ScopedName = {moduleName:"common.http", name:"Put"};

export function texprPut<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<Put<I, O>> {
  return {value:{typeRef:{kind:"reference",value:snPut},parameters:[texprI.value,texprO.value]}};
}

/**
 * A marker type to associate the input request and output response
 * types for an http post request.
 */
export type Post<_I, _O> = null;

const Post_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["I","O"],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Post","version":{"kind":"nothing"}}};

export const snPost: ADL.ScopedName = {moduleName:"common.http", name:"Post"};

export function texprPost<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<Post<I, O>> {
  return {value:{typeRef:{kind:"reference",value:snPost},parameters:[texprI.value,texprO.value]}};
}

/**
 * An annotation indicating the URL path for a request
 */
export type Path = string;

const Path_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Path","version":{"kind":"nothing"}}};

export const snPath: ADL.ScopedName = {moduleName:"common.http", name:"Path"};

export function texprPath(): ADL.ATypeExpr<Path> {
  return {value:{typeRef:{kind:"reference",value:snPath},parameters:[]}};
}

/**
 * An annotation indicating that a request requires an Authorization http header
 */
export type AuthHeader = null;

const AuthHeader_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"AuthHeader","version":{"kind":"nothing"}}};

export const snAuthHeader: ADL.ScopedName = {moduleName:"common.http", name:"AuthHeader"};

export function texprAuthHeader(): ADL.ATypeExpr<AuthHeader> {
  return {value:{typeRef:{kind:"reference",value:snAuthHeader},parameters:[]}};
}

/**
 * An annotation indicating that a request is idemponent, and can hence be safely
 * retried on failure.
 */
export type Idempotent = null;

const Idempotent_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"Idempotent","version":{"kind":"nothing"}}};

export const snIdempotent: ADL.ScopedName = {moduleName:"common.http", name:"Idempotent"};

export function texprIdempotent(): ADL.ATypeExpr<Idempotent> {
  return {value:{typeRef:{kind:"reference",value:snIdempotent},parameters:[]}};
}

/**
 * An annotation indicating that a request can be made idempotent if the caller
 * provides an Idempotency-Key header with a unique value.
 */
export type IdempotentWithKey = null;

const IdempotentWithKey_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"IdempotentWithKey","version":{"kind":"nothing"}}};

export const snIdempotentWithKey: ADL.ScopedName = {moduleName:"common.http", name:"IdempotentWithKey"};

export function texprIdempotentWithKey(): ADL.ATypeExpr<IdempotentWithKey> {
  return {value:{typeRef:{kind:"reference",value:snIdempotentWithKey},parameters:[]}};
}

/**
 * The standard message body for errors
 */
export interface PublicErrorData {
  publicMessage: string;
}

export function makePublicErrorData(
  input: {
    publicMessage: string,
  }
): PublicErrorData {
  return {
    publicMessage: input.publicMessage,
  };
}

const PublicErrorData_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"publicMessage","default":{"kind":"nothing"},"name":"publicMessage","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"PublicErrorData","version":{"kind":"nothing"}}};

export const snPublicErrorData: ADL.ScopedName = {moduleName:"common.http", name:"PublicErrorData"};

export function texprPublicErrorData(): ADL.ATypeExpr<PublicErrorData> {
  return {value:{typeRef:{kind:"reference",value:snPublicErrorData},parameters:[]}};
}

/**
 * New request types
 */
export interface HttpGet<O> {
  path: string;
  security: HttpSecurity;
  rateLimit: (HttpRateLimit|null);
  respType: ADL.ATypeExpr<O>;
}

const HttpGet_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"rateLimit","default":{"kind":"just","value":null},"name":"rateLimit","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpRateLimit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpGet","version":{"kind":"nothing"}}};

export const snHttpGet: ADL.ScopedName = {moduleName:"common.http", name:"HttpGet"};

export function texprHttpGet<O>(texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpGet<O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpGet},parameters:[texprO.value]}};
}

export interface HttpGetStream<O> {
  path: string;
  security: HttpSecurity;
  respItemType: ADL.ATypeExpr<O>;
}

const HttpGetStream_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"respItemType","default":{"kind":"just","value":null},"name":"respItemType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpGetStream","version":{"kind":"nothing"}}};

export const snHttpGetStream: ADL.ScopedName = {moduleName:"common.http", name:"HttpGetStream"};

export function texprHttpGetStream<O>(texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpGetStream<O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpGetStream},parameters:[texprO.value]}};
}

export interface HttpGet2<P, O> {
  path: string;
  security: HttpSecurity;
  rateLimit: (HttpRateLimit|null);
  paramsType: ADL.ATypeExpr<P>;
  respType: ADL.ATypeExpr<O>;
}

const HttpGet2_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["P","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"rateLimit","default":{"kind":"just","value":null},"name":"rateLimit","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpRateLimit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"paramsType","default":{"kind":"just","value":null},"name":"paramsType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"P"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpGet2","version":{"kind":"nothing"}}};

export const snHttpGet2: ADL.ScopedName = {moduleName:"common.http", name:"HttpGet2"};

export function texprHttpGet2<P, O>(texprP : ADL.ATypeExpr<P>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpGet2<P, O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpGet2},parameters:[texprP.value,texprO.value]}};
}

export interface HttpPut<I, O> {
  path: string;
  security: HttpSecurity;
  rateLimit: (HttpRateLimit|null);
  reqType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

const HttpPut_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["I","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"rateLimit","default":{"kind":"just","value":null},"name":"rateLimit","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpRateLimit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"reqType","default":{"kind":"just","value":null},"name":"reqType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"I"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpPut","version":{"kind":"nothing"}}};

export const snHttpPut: ADL.ScopedName = {moduleName:"common.http", name:"HttpPut"};

export function texprHttpPut<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpPut<I, O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpPut},parameters:[texprI.value,texprO.value]}};
}

export interface HttpPost<I, O> {
  path: string;
  security: HttpSecurity;
  rateLimit: (HttpRateLimit|null);
  reqType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

const HttpPost_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["I","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"rateLimit","default":{"kind":"just","value":null},"name":"rateLimit","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpRateLimit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"reqType","default":{"kind":"just","value":null},"name":"reqType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"I"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpPost","version":{"kind":"nothing"}}};

export const snHttpPost: ADL.ScopedName = {moduleName:"common.http", name:"HttpPost"};

export function texprHttpPost<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpPost<I, O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpPost},parameters:[texprI.value,texprO.value]}};
}

export interface HttpPost2<P, I, O> {
  path: string;
  security: HttpSecurity;
  rateLimit: (HttpRateLimit|null);
  paramsType: ADL.ATypeExpr<P>;
  reqType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

const HttpPost2_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["P","I","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"rateLimit","default":{"kind":"just","value":null},"name":"rateLimit","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpRateLimit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"paramsType","default":{"kind":"just","value":null},"name":"paramsType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"P"},"parameters":[]}]}},{"annotations":[],"serializedName":"reqType","default":{"kind":"just","value":null},"name":"reqType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"I"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpPost2","version":{"kind":"nothing"}}};

export const snHttpPost2: ADL.ScopedName = {moduleName:"common.http", name:"HttpPost2"};

export function texprHttpPost2<P, I, O>(texprP : ADL.ATypeExpr<P>, texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpPost2<P, I, O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpPost2},parameters:[texprP.value,texprI.value,texprO.value]}};
}

export interface HttpDelete<P, O> {
  path: string;
  security: HttpSecurity;
  paramsType: ADL.ATypeExpr<P>;
  respType: ADL.ATypeExpr<O>;
}

const HttpDelete_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["P","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"security","default":{"kind":"nothing"},"name":"security","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpSecurity"}},"parameters":[]}},{"annotations":[],"serializedName":"paramsType","default":{"kind":"just","value":null},"name":"paramsType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"P"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"HttpDelete","version":{"kind":"nothing"}}};

export const snHttpDelete: ADL.ScopedName = {moduleName:"common.http", name:"HttpDelete"};

export function texprHttpDelete<P, O>(texprP : ADL.ATypeExpr<P>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<HttpDelete<P, O>> {
  return {value:{typeRef:{kind:"reference",value:snHttpDelete},parameters:[texprP.value,texprO.value]}};
}

export interface HttpSecurity_Public {
  kind: 'public';
}
export interface HttpSecurity_Token {
  kind: 'token';
}
export interface HttpSecurity_TokenWithRole {
  kind: 'tokenWithRole';
  value: string;
}

export type HttpSecurity = HttpSecurity_Public | HttpSecurity_Token | HttpSecurity_TokenWithRole;

export interface HttpSecurityOpts {
  public: null;
  token: null;
  tokenWithRole: string;
}

export function makeHttpSecurity<K extends keyof HttpSecurityOpts>(kind: K, value: HttpSecurityOpts[K]) { return {kind, value}; }

const HttpSecurity_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"public","default":{"kind":"nothing"},"name":"public","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"token","default":{"kind":"nothing"},"name":"token","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"tokenWithRole","default":{"kind":"nothing"},"name":"tokenWithRole","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"HttpSecurity","version":{"kind":"nothing"}}};

export const snHttpSecurity: ADL.ScopedName = {moduleName:"common.http", name:"HttpSecurity"};

export function texprHttpSecurity(): ADL.ATypeExpr<HttpSecurity> {
  return {value:{typeRef:{kind:"reference",value:snHttpSecurity},parameters:[]}};
}

export interface HttpRateLimit {
  maxRequests: number;
  perTimeUnit: RateLimitTimeUnit;
}

export function makeHttpRateLimit(
  input: {
    maxRequests: number,
    perTimeUnit: RateLimitTimeUnit,
  }
): HttpRateLimit {
  return {
    maxRequests: input.maxRequests,
    perTimeUnit: input.perTimeUnit,
  };
}

const HttpRateLimit_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"maxRequests","default":{"kind":"nothing"},"name":"maxRequests","typeExpr":{"typeRef":{"kind":"primitive","value":"Word32"},"parameters":[]}},{"annotations":[],"serializedName":"perTimeUnit","default":{"kind":"nothing"},"name":"perTimeUnit","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"RateLimitTimeUnit"}},"parameters":[]}}]}},"name":"HttpRateLimit","version":{"kind":"nothing"}}};

export const snHttpRateLimit: ADL.ScopedName = {moduleName:"common.http", name:"HttpRateLimit"};

export function texprHttpRateLimit(): ADL.ATypeExpr<HttpRateLimit> {
  return {value:{typeRef:{kind:"reference",value:snHttpRateLimit},parameters:[]}};
}

export type RateLimitTimeUnit = 'second' | 'minute' | 'hour';
export const valuesRateLimitTimeUnit : RateLimitTimeUnit[] = ['second', 'minute', 'hour'];

const RateLimitTimeUnit_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"second","default":{"kind":"nothing"},"name":"second","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"minute","default":{"kind":"nothing"},"name":"minute","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"hour","default":{"kind":"nothing"},"name":"hour","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"RateLimitTimeUnit","version":{"kind":"nothing"}}};

export const snRateLimitTimeUnit: ADL.ScopedName = {moduleName:"common.http", name:"RateLimitTimeUnit"};

export function texprRateLimitTimeUnit(): ADL.ATypeExpr<RateLimitTimeUnit> {
  return {value:{typeRef:{kind:"reference",value:snRateLimitTimeUnit},parameters:[]}};
}

/**
 * API decl or request annotation to specify
 * the non-success response codes we want
 * included in the generated open API. The
 * Json should have exactly the structure of
 * the OpenApi responses map
 */
export type OpenApiOtherResponses = {}|null;

const OpenApiOtherResponses_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}}},"name":"OpenApiOtherResponses","version":{"kind":"nothing"}}};

export const snOpenApiOtherResponses: ADL.ScopedName = {moduleName:"common.http", name:"OpenApiOtherResponses"};

export function texprOpenApiOtherResponses(): ADL.ATypeExpr<OpenApiOtherResponses> {
  return {value:{typeRef:{kind:"reference",value:snOpenApiOtherResponses},parameters:[]}};
}

/**
 * API annotation to specify the available
 * server endpoints. The Json should have
 * exactly the structure of the OpenApi
 * servers map
 */
export type OpenApiServers = {}|null;

const OpenApiServers_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}}},"name":"OpenApiServers","version":{"kind":"nothing"}}};

export const snOpenApiServers: ADL.ScopedName = {moduleName:"common.http", name:"OpenApiServers"};

export function texprOpenApiServers(): ADL.ATypeExpr<OpenApiServers> {
  return {value:{typeRef:{kind:"reference",value:snOpenApiServers},parameters:[]}};
}

/**
 * Marker annotation to indicate that an endpoint
 * or (defaulted) field should be left out of the
 * generated openapi
 */
export type OpenApiExclude = boolean;

const OpenApiExclude_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}},"name":"OpenApiExclude","version":{"kind":"nothing"}}};

export const snOpenApiExclude: ADL.ScopedName = {moduleName:"common.http", name:"OpenApiExclude"};

export function texprOpenApiExclude(): ADL.ATypeExpr<OpenApiExclude> {
  return {value:{typeRef:{kind:"reference",value:snOpenApiExclude},parameters:[]}};
}

/**
 * API annotation to specify additional api
 * information. The Json should have
 * exactly the structure of the OpenApi
 * info map
 */
export type OpenApiInfo = {}|null;

const OpenApiInfo_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}}},"name":"OpenApiInfo","version":{"kind":"nothing"}}};

export const snOpenApiInfo: ADL.ScopedName = {moduleName:"common.http", name:"OpenApiInfo"};

export function texprOpenApiInfo(): ADL.ATypeExpr<OpenApiInfo> {
  return {value:{typeRef:{kind:"reference",value:snOpenApiInfo},parameters:[]}};
}

export interface SecurityScheme_HttpBearer {
  kind: 'httpBearer';
}
export interface SecurityScheme_ApiKey {
  kind: 'apiKey';
  value: HeaderApiKeyScheme;
}

/**
 * API decl annotation to specify
 * the security schema in use
 */
export type SecurityScheme = SecurityScheme_HttpBearer | SecurityScheme_ApiKey;

export interface SecuritySchemeOpts {
  httpBearer: null;
  apiKey: HeaderApiKeyScheme;
}

export function makeSecurityScheme<K extends keyof SecuritySchemeOpts>(kind: K, value: SecuritySchemeOpts[K]) { return {kind, value}; }

const SecurityScheme_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"httpBearer","default":{"kind":"nothing"},"name":"httpBearer","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"apiKey","default":{"kind":"nothing"},"name":"apiKey","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HeaderApiKeyScheme"}},"parameters":[]}}]}},"name":"SecurityScheme","version":{"kind":"nothing"}}};

export const snSecurityScheme: ADL.ScopedName = {moduleName:"common.http", name:"SecurityScheme"};

export function texprSecurityScheme(): ADL.ATypeExpr<SecurityScheme> {
  return {value:{typeRef:{kind:"reference",value:snSecurityScheme},parameters:[]}};
}

export interface HeaderApiKeyScheme {
  headerName: string;
}

export function makeHeaderApiKeyScheme(
  input: {
    headerName: string,
  }
): HeaderApiKeyScheme {
  return {
    headerName: input.headerName,
  };
}

const HeaderApiKeyScheme_AST : ADL.ScopedDecl =
  {"moduleName":"common.http","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"headerName","default":{"kind":"nothing"},"name":"headerName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"HeaderApiKeyScheme","version":{"kind":"nothing"}}};

export const snHeaderApiKeyScheme: ADL.ScopedName = {moduleName:"common.http", name:"HeaderApiKeyScheme"};

export function texprHeaderApiKeyScheme(): ADL.ATypeExpr<HeaderApiKeyScheme> {
  return {value:{typeRef:{kind:"reference",value:snHeaderApiKeyScheme},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.http.Get" : Get_AST,
  "common.http.Put" : Put_AST,
  "common.http.Post" : Post_AST,
  "common.http.Path" : Path_AST,
  "common.http.AuthHeader" : AuthHeader_AST,
  "common.http.Idempotent" : Idempotent_AST,
  "common.http.IdempotentWithKey" : IdempotentWithKey_AST,
  "common.http.PublicErrorData" : PublicErrorData_AST,
  "common.http.HttpGet" : HttpGet_AST,
  "common.http.HttpGetStream" : HttpGetStream_AST,
  "common.http.HttpGet2" : HttpGet2_AST,
  "common.http.HttpPut" : HttpPut_AST,
  "common.http.HttpPost" : HttpPost_AST,
  "common.http.HttpPost2" : HttpPost2_AST,
  "common.http.HttpDelete" : HttpDelete_AST,
  "common.http.HttpSecurity" : HttpSecurity_AST,
  "common.http.HttpRateLimit" : HttpRateLimit_AST,
  "common.http.RateLimitTimeUnit" : RateLimitTimeUnit_AST,
  "common.http.OpenApiOtherResponses" : OpenApiOtherResponses_AST,
  "common.http.OpenApiServers" : OpenApiServers_AST,
  "common.http.OpenApiExclude" : OpenApiExclude_AST,
  "common.http.OpenApiInfo" : OpenApiInfo_AST,
  "common.http.SecurityScheme" : SecurityScheme_AST,
  "common.http.HeaderApiKeyScheme" : HeaderApiKeyScheme_AST
};
