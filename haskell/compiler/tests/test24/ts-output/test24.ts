/* @generated from adl module test24 */

import * as ADL from './runtime/adl';

export interface PostReq<I, O> {
  path: string;
  reqBodyType: ADL.ATypeExpr<I>;
  respType: ADL.ATypeExpr<O>;
}

const PostReq_AST : ADL.ScopedDecl =
  {"moduleName":"test24","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["I","O"],"fields":[{"annotations":[],"serializedName":"path","default":{"kind":"nothing"},"name":"path","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"reqBodyType","default":{"kind":"just","value":null},"name":"reqBodyType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"I"},"parameters":[]}]}},{"annotations":[],"serializedName":"respType","default":{"kind":"just","value":null},"name":"respType","typeExpr":{"typeRef":{"kind":"primitive","value":"TypeToken"},"parameters":[{"typeRef":{"kind":"typeParam","value":"O"},"parameters":[]}]}}]}},"name":"PostReq","version":{"kind":"nothing"}}};

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
  {"moduleName":"test24","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"create","default":{"kind":"nothing"},"name":"create","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]},{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"read","default":{"kind":"nothing"},"name":"read","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]},{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}}]}},"name":"CrudReqs","version":{"kind":"nothing"}}};

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
  {"moduleName":"test24","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"hello","default":{"kind":"just","value":{"path":"/hello"}},"name":"hello","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]},{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"farewell","default":{"kind":"just","value":{"path":"/farewell"}},"name":"farewell","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"PostReq"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]},{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}]}},{"annotations":[],"serializedName":"blobs","default":{"kind":"just","value":{"read":{"path":"/blobs/read"},"create":{"path":"/blobs/create"}}},"name":"blobs","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"test24","name":"CrudReqs"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}}]}},"name":"Service","version":{"kind":"nothing"}}};

export const snService: ADL.ScopedName = {moduleName:"test24", name:"Service"};

export function texprService(): ADL.ATypeExpr<Service> {
  return {value : {typeRef : {kind: "reference", value : snService}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "test24.PostReq" : PostReq_AST,
  "test24.CrudReqs" : CrudReqs_AST,
  "test24.Service" : Service_AST
};
