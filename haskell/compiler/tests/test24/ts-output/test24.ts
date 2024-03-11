/* @generated from adl module test24 */

import * as ADL from './runtime/adl';

export interface PostReq<I, O> {
  path: string;
  reqBodyType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

const PostReq_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"PostReq","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"path","serializedName":"path","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}},{"annotations":[],"default":{"kind":"just","value":null},"name":"reqBodyType","serializedName":"reqBodyType","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"I"}}],"typeRef":{"kind":"primitive","value":"TypeToken"}}},{"annotations":[],"default":{"kind":"just","value":null},"name":"respType","serializedName":"respType","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"O"}}],"typeRef":{"kind":"primitive","value":"TypeToken"}}}],"typeParams":["I","O"]}},"version":{"kind":"nothing"}},"moduleName":"test24"};

export const snPostReq: ADL.ScopedName = {moduleName:"test24", name:"PostReq"};

export function texprPostReq<I, O>(texprI : ADL.ATypeExpr<I>, texprO : ADL.ATypeExpr<O>): ADL.ATypeExpr<PostReq<I, O>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test24",name : "PostReq"}}, parameters : [texprI.value, texprO.value]}};
}

export interface CrudReqs<T> {
  create: PostReq<T, string>;
  read: PostReq<string, T>;
}

export function makeCrudReqs<T>(
  input: {
    create: PostReq<T, string>,
    read: PostReq<string, T>,
  }
): CrudReqs<T> {
  return {
    create: input.create,
    read: input.read,
  };
}

const CrudReqs_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"CrudReqs","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"create","serializedName":"create","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}},{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"read","serializedName":"read","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"test24"};

export const snCrudReqs: ADL.ScopedName = {moduleName:"test24", name:"CrudReqs"};

export function texprCrudReqs<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<CrudReqs<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "test24",name : "CrudReqs"}}, parameters : [texprT.value]}};
}

export interface Service {
  hello: PostReq<string, string>;
  farewell: PostReq<(string|null), (string|null)>;
  blobs: CrudReqs<{}|null>;
}

export function makeService(
  input: {
    hello?: PostReq<string, string>,
    farewell?: PostReq<(string|null), (string|null)>,
    blobs?: CrudReqs<{}|null>,
  }
): Service {
  return {
    hello: input.hello === undefined ? {path : "/hello", reqBodyType : ADL.texprString(), respType : ADL.texprString()} : input.hello,
    farewell: input.farewell === undefined ? {path : "/farewell", reqBodyType : ADL.texprNullable(ADL.texprString()), respType : ADL.texprNullable(ADL.texprString())} : input.farewell,
    blobs: input.blobs === undefined ? {create : {path : "/blobs/create", reqBodyType : ADL.texprJson(), respType : ADL.texprString()}, read : {path : "/blobs/read", reqBodyType : ADL.texprString(), respType : ADL.texprJson()}} : input.blobs,
  };
}

const Service_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Service","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":{"path":"/hello"}},"name":"hello","serializedName":"hello","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}},{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}}}},{"annotations":[],"default":{"kind":"just","value":{"path":"/farewell"}},"name":"farewell","serializedName":"farewell","typeExpr":{"parameters":[{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"primitive","value":"Nullable"}},{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"String"}}],"typeRef":{"kind":"primitive","value":"Nullable"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}}}},{"annotations":[],"default":{"kind":"just","value":{"create":{"path":"/blobs/create"},"read":{"path":"/blobs/read"}}},"name":"blobs","serializedName":"blobs","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"primitive","value":"Json"}}],"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"CrudReqs"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"test24"};

export const snService: ADL.ScopedName = {moduleName:"test24", name:"Service"};

export function texprService(): ADL.ATypeExpr<Service> {
  return {value : {typeRef : {kind: "reference", value : snService}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test24.PostReq" : PostReq_AST,
  "test24.CrudReqs" : CrudReqs_AST,
  "test24.Service" : Service_AST
};
