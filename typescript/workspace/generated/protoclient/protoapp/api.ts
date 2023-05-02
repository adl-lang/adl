import * as protoclient_protoapp_db from "./db";
import * as common from "@adl-lang/common/";
import * as common_config_log from "@adl-lang/common/config/log";
import * as common_db from "@adl-lang/common/db";
import * as common_http from "@adl-lang/common/http";
import * as common_strings from "@adl-lang/common/strings";
import * as common_tabular from "@adl-lang/common/tabular";
import * as ADL from "@adl-lang/runtime/adl";
import * as sys_types from "@adl-lang/sys/types";

/* @generated from adl module protoclient.protoapp.api */

export interface X<_A> {
}

export function makeX<_A>(
  _input: {}
): X<_A> {
  return {};
}

const X_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["A"],"fields":[]}},"name":"X","version":{"kind":"nothing"}}};

export const snX: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"X"};

export function texprX<A>(texprA : ADL.ATypeExpr<A>): ADL.ATypeExpr<X<A>> {
  return {value:{typeRef:{kind:"reference",value:snX},parameters:[texprA.value]}};
}

/**
 * The app API
 */
export interface ApiRequests {
  x: X<string>;
  imamap: sys_types.Map<string, string>;
/**
 * Login a user
 */
  with_prim: common_http.HttpPost<string, {[key: string]: number}[]>;
/**
 * Login a user
 */
  login: common_http.HttpPost<LoginReq, LoginResp>;
/**
 * Post a message to the noticeboard
 */
  newMessage: common_http.HttpPost<NewMessageReq, common.Unit>;
/**
 * Get recent noticeboard messages
 */
  recentMessages: common_http.HttpPost<RecentMessagesReq, common.Paginated<Message>>;
/**
 * Gets the logged in user details
 */
  whoAmI: common_http.HttpGet<UserProfile>;
/**
 * Create a new user
 */
  createUser: common_http.HttpPost<UserReq, protoclient_protoapp_db.AppUserId>;
/**
 * Update an existing user
 */
  updateUser: common_http.HttpPost<common_db.WithDbId<UserReq>, common.Unit>;
/**
 * Delete an existing user
 */
  deleteUser: common_http.HttpPost<protoclient_protoapp_db.AppUserId, common.Unit>;
/**
 * Query existing users sorted and filters according to the
 * TableQuery request.
 */
  queryUsers: common_http.HttpPost<common_tabular.TableQuery, common.Paginated<common_db.WithDbId<protoclient_protoapp_db.AppUser>>>;
/**
 * Logs an error from the client app without user information, i.e. when the
 * user has not logged in
 */
  logClientErrorPublic: common_http.HttpPost<ClientLogReq, common.Unit>;
/**
 * Logs an error from the client app and includes user information
 */
  logClientErrorUser: common_http.HttpPost<ClientLogReq, common.Unit>;
}

export function makeApiRequests(
  input: {
    x: X<string>,
    imamap: sys_types.Map<string, string>,
    with_prim?: common_http.HttpPost<string, {[key: string]: number}[]>,
    login?: common_http.HttpPost<LoginReq, LoginResp>,
    newMessage?: common_http.HttpPost<NewMessageReq, common.Unit>,
    recentMessages?: common_http.HttpPost<RecentMessagesReq, common.Paginated<Message>>,
    whoAmI?: common_http.HttpGet<UserProfile>,
    createUser?: common_http.HttpPost<UserReq, protoclient_protoapp_db.AppUserId>,
    updateUser?: common_http.HttpPost<common_db.WithDbId<UserReq>, common.Unit>,
    deleteUser?: common_http.HttpPost<protoclient_protoapp_db.AppUserId, common.Unit>,
    queryUsers?: common_http.HttpPost<common_tabular.TableQuery, common.Paginated<common_db.WithDbId<protoclient_protoapp_db.AppUser>>>,
    logClientErrorPublic?: common_http.HttpPost<ClientLogReq, common.Unit>,
    logClientErrorUser?: common_http.HttpPost<ClientLogReq, common.Unit>,
  }
): ApiRequests {
  return {
    x: input.x,
    imamap: input.imamap,
    with_prim: input.with_prim === undefined ? {path : "/login", security : {kind : "public"}, rateLimit : null, reqType : ADL.texprString(), respType : ADL.texprVector(ADL.texprStringMap(ADL.texprInt64()))} : input.with_prim,
    login: input.login === undefined ? {path : "/login", security : {kind : "public"}, rateLimit : null, reqType : texprLoginReq(), respType : texprLoginResp()} : input.login,
    newMessage: input.newMessage === undefined ? {path : "/messages/new", security : {kind : "token"}, rateLimit : null, reqType : texprNewMessageReq(), respType : common.texprUnit()} : input.newMessage,
    recentMessages: input.recentMessages === undefined ? {path : "/messages/recent", security : {kind : "token"}, rateLimit : null, reqType : texprRecentMessagesReq(), respType : common.texprPaginated(texprMessage())} : input.recentMessages,
    whoAmI: input.whoAmI === undefined ? {path : "/whoami", security : {kind : "token"}, rateLimit : null, respType : texprUserProfile()} : input.whoAmI,
    createUser: input.createUser === undefined ? {path : "/users/create", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : texprUserReq(), respType : protoclient_protoapp_db.texprAppUserId()} : input.createUser,
    updateUser: input.updateUser === undefined ? {path : "/users/update", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : common_db.texprWithDbId(texprUserReq()), respType : common.texprUnit()} : input.updateUser,
    deleteUser: input.deleteUser === undefined ? {path : "/users/delete", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : protoclient_protoapp_db.texprAppUserId(), respType : common.texprUnit()} : input.deleteUser,
    queryUsers: input.queryUsers === undefined ? {path : "/users/query", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : common_tabular.texprTableQuery(), respType : common.texprPaginated(common_db.texprWithDbId(protoclient_protoapp_db.texprAppUser()))} : input.queryUsers,
    logClientErrorPublic: input.logClientErrorPublic === undefined ? {path : "/client/log/public", security : {kind : "public"}, rateLimit : {maxRequests : 6, perTimeUnit : "minute"}, reqType : texprClientLogReq(), respType : common.texprUnit()} : input.logClientErrorPublic,
    logClientErrorUser: input.logClientErrorUser === undefined ? {path : "/client/log/user", security : {kind : "token"}, rateLimit : null, reqType : texprClientLogReq(), respType : common.texprUnit()} : input.logClientErrorUser,
  };
}

const ApiRequests_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"x","default":{"kind":"nothing"},"name":"x","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"X"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"imamap","default":{"kind":"nothing"},"name":"imamap","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Map"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]},{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"with_prim","default":{"kind":"just","value":{"path":"/login","security":"public"}},"name":"with_prim","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]},{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}]}]}]}},{"annotations":[],"serializedName":"login","default":{"kind":"just","value":{"path":"/login","security":"public"}},"name":"login","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"LoginReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"LoginResp"}},"parameters":[]}]}},{"annotations":[],"serializedName":"newMessage","default":{"kind":"just","value":{"path":"/messages/new","security":"token"}},"name":"newMessage","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"NewMessageReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"recentMessages","default":{"kind":"just","value":{"path":"/messages/recent","security":"token"}},"name":"recentMessages","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"RecentMessagesReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"Message"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"whoAmI","default":{"kind":"just","value":{"path":"/whoami","security":"token"}},"name":"whoAmI","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpGet"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"UserProfile"}},"parameters":[]}]}},{"annotations":[],"serializedName":"createUser","default":{"kind":"just","value":{"path":"/users/create","security":{"tokenWithRole":"admin"}}},"name":"createUser","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"UserReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"AppUserId"}},"parameters":[]}]}},{"annotations":[],"serializedName":"updateUser","default":{"kind":"just","value":{"path":"/users/update","security":{"tokenWithRole":"admin"}}},"name":"updateUser","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"UserReq"}},"parameters":[]}]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"deleteUser","default":{"kind":"just","value":{"path":"/users/delete","security":{"tokenWithRole":"admin"}}},"name":"deleteUser","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"AppUserId"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"queryUsers","default":{"kind":"just","value":{"path":"/users/query","security":{"tokenWithRole":"admin"}}},"name":"queryUsers","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"TableQuery"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"AppUser"}},"parameters":[]}]}]}]}},{"annotations":[],"serializedName":"logClientErrorPublic","default":{"kind":"just","value":{"path":"/client/log/public","rateLimit":{"maxRequests":6,"perTimeUnit":"minute"},"security":"public"}},"name":"logClientErrorPublic","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"ClientLogReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}},{"annotations":[],"serializedName":"logClientErrorUser","default":{"kind":"just","value":{"path":"/client/log/user","security":"token"}},"name":"logClientErrorUser","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.api","name":"ClientLogReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}}]}},"name":"ApiRequests","version":{"kind":"nothing"}}};

export const snApiRequests: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"ApiRequests"};

export function texprApiRequests(): ADL.ATypeExpr<ApiRequests> {
  return {value:{typeRef:{kind:"reference",value:snApiRequests},parameters:[]}};
}

export interface UserProfile {
  id: protoclient_protoapp_db.AppUserId;
  fullname: string;
  email: string;
  isAdmin: boolean;
}

export function makeUserProfile(
  input: {
    id: protoclient_protoapp_db.AppUserId,
    fullname: string,
    email: string,
    isAdmin: boolean,
  }
): UserProfile {
  return {
    id: input.id,
    fullname: input.fullname,
    email: input.email,
    isAdmin: input.isAdmin,
  };
}

const UserProfile_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"AppUserId"}},"parameters":[]}},{"annotations":[],"serializedName":"fullname","default":{"kind":"nothing"},"name":"fullname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"isAdmin","default":{"kind":"nothing"},"name":"isAdmin","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"UserProfile","version":{"kind":"nothing"}}};

export const snUserProfile: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"UserProfile"};

export function texprUserProfile(): ADL.ATypeExpr<UserProfile> {
  return {value:{typeRef:{kind:"reference",value:snUserProfile},parameters:[]}};
}

/**
 * Details for a user
 */
export interface UserReq {
  fullname: string;
  email: string;
  password: common_strings.Password;
}

export function makeUserReq(
  input: {
    fullname: string,
    email: string,
    password: common_strings.Password,
  }
): UserReq {
  return {
    fullname: input.fullname,
    email: input.email,
    password: input.password,
  };
}

const UserReq_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"fullname","default":{"kind":"nothing"},"name":"fullname","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"Password"}},"parameters":[]}}]}},"name":"UserReq","version":{"kind":"nothing"}}};

export const snUserReq: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"UserReq"};

export function texprUserReq(): ADL.ATypeExpr<UserReq> {
  return {value:{typeRef:{kind:"reference",value:snUserReq},parameters:[]}};
}

export interface LoginReq {
  email: common_strings.StringNE;
  password: common_strings.Password;
}

export function makeLoginReq(
  input: {
    email: common_strings.StringNE,
    password: common_strings.Password,
  }
): LoginReq {
  return {
    email: input.email,
    password: input.password,
  };
}

const LoginReq_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"password","default":{"kind":"nothing"},"name":"password","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"Password"}},"parameters":[]}}]}},"name":"LoginReq","version":{"kind":"nothing"}}};

export const snLoginReq: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"LoginReq"};

export function texprLoginReq(): ADL.ATypeExpr<LoginReq> {
  return {value:{typeRef:{kind:"reference",value:snLoginReq},parameters:[]}};
}

export interface LoginResp_AccessToken {
  kind: 'accessToken';
  value: common_strings.StringNE;
}
export interface LoginResp_InvalidCredentials {
  kind: 'invalidCredentials';
}

export type LoginResp = LoginResp_AccessToken | LoginResp_InvalidCredentials;

export interface LoginRespOpts {
  accessToken: common_strings.StringNE;
  invalidCredentials: null;
}

export function makeLoginResp<K extends keyof LoginRespOpts>(kind: K, value: LoginRespOpts[K]) { return {kind, value}; }

const LoginResp_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"accessToken","default":{"kind":"nothing"},"name":"accessToken","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"invalidCredentials","default":{"kind":"nothing"},"name":"invalidCredentials","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"LoginResp","version":{"kind":"nothing"}}};

export const snLoginResp: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"LoginResp"};

export function texprLoginResp(): ADL.ATypeExpr<LoginResp> {
  return {value:{typeRef:{kind:"reference",value:snLoginResp},parameters:[]}};
}

export interface NewMessageReq {
  message: common_strings.StringML;
}

export function makeNewMessageReq(
  input: {
    message: common_strings.StringML,
  }
): NewMessageReq {
  return {
    message: input.message,
  };
}

const NewMessageReq_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"message","default":{"kind":"nothing"},"name":"message","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringML"}},"parameters":[]}}]}},"name":"NewMessageReq","version":{"kind":"nothing"}}};

export const snNewMessageReq: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"NewMessageReq"};

export function texprNewMessageReq(): ADL.ATypeExpr<NewMessageReq> {
  return {value:{typeRef:{kind:"reference",value:snNewMessageReq},parameters:[]}};
}

export interface RecentMessagesReq {
  offset: number;
  count: number;
}

export function makeRecentMessagesReq(
  input: {
    offset?: number,
    count?: number,
  }
): RecentMessagesReq {
  return {
    offset: input.offset === undefined ? 0 : input.offset,
    count: input.count === undefined ? 20 : input.count,
  };
}

const RecentMessagesReq_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"offset","default":{"kind":"just","value":0},"name":"offset","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"count","default":{"kind":"just","value":20},"name":"count","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"RecentMessagesReq","version":{"kind":"nothing"}}};

export const snRecentMessagesReq: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"RecentMessagesReq"};

export function texprRecentMessagesReq(): ADL.ATypeExpr<RecentMessagesReq> {
  return {value:{typeRef:{kind:"reference",value:snRecentMessagesReq},parameters:[]}};
}

export interface Message {
  id: protoclient_protoapp_db.MessageId;
  postedAt: common.Instant;
  userFullName: string;
  message: common_strings.StringML;
}

export function makeMessage(
  input: {
    id: protoclient_protoapp_db.MessageId,
    postedAt: common.Instant,
    userFullName: string,
    message: common_strings.StringML,
  }
): Message {
  return {
    id: input.id,
    postedAt: input.postedAt,
    userFullName: input.userFullName,
    message: input.message,
  };
}

const Message_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"MessageId"}},"parameters":[]}},{"annotations":[],"serializedName":"postedAt","default":{"kind":"nothing"},"name":"postedAt","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Instant"}},"parameters":[]}},{"annotations":[],"serializedName":"userFullName","default":{"kind":"nothing"},"name":"userFullName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"message","default":{"kind":"nothing"},"name":"message","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringML"}},"parameters":[]}}]}},"name":"Message","version":{"kind":"nothing"}}};

export const snMessage: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"Message"};

export function texprMessage(): ADL.ATypeExpr<Message> {
  return {value:{typeRef:{kind:"reference",value:snMessage},parameters:[]}};
}

export interface ClientLogReq {
  level: common_config_log.LogLevel;
  error: string;
  stacktrace: (string|null);
}

export function makeClientLogReq(
  input: {
    level: common_config_log.LogLevel,
    error: string,
    stacktrace: (string|null),
  }
): ClientLogReq {
  return {
    level: input.level,
    error: input.error,
    stacktrace: input.stacktrace,
  };
}

const ClientLogReq_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"level","default":{"kind":"nothing"},"name":"level","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.config.log","name":"LogLevel"}},"parameters":[]}},{"annotations":[],"serializedName":"error","default":{"kind":"nothing"},"name":"error","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"stacktrace","default":{"kind":"nothing"},"name":"stacktrace","typeExpr":{"typeRef":{"kind":"primitive","value":"Nullable"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"ClientLogReq","version":{"kind":"nothing"}}};

export const snClientLogReq: ADL.ScopedName = {moduleName:"protoclient.protoapp.api", name:"ClientLogReq"};

export function texprClientLogReq(): ADL.ATypeExpr<ClientLogReq> {
  return {value:{typeRef:{kind:"reference",value:snClientLogReq},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "protoclient.protoapp.api.X" : X_AST,
  "protoclient.protoapp.api.ApiRequests" : ApiRequests_AST,
  "protoclient.protoapp.api.UserProfile" : UserProfile_AST,
  "protoclient.protoapp.api.UserReq" : UserReq_AST,
  "protoclient.protoapp.api.LoginReq" : LoginReq_AST,
  "protoclient.protoapp.api.LoginResp" : LoginResp_AST,
  "protoclient.protoapp.api.NewMessageReq" : NewMessageReq_AST,
  "protoclient.protoapp.api.RecentMessagesReq" : RecentMessagesReq_AST,
  "protoclient.protoapp.api.Message" : Message_AST,
  "protoclient.protoapp.api.ClientLogReq" : ClientLogReq_AST
};
