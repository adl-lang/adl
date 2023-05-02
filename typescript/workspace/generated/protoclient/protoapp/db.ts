import * as common from "@adl-lang/common/";
import * as common_db from "@adl-lang/common/db";
import * as common_strings from "@adl-lang/common/strings";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module protoclient.protoapp.db */

export interface Trade {
  commodities: Commodity[];
  date: common.Instant;
}

export function makeTrade(
  input: {
    commodities: Commodity[],
    date: common.Instant,
  }
): Trade {
  return {
    commodities: input.commodities,
    date: input.date,
  };
}

const Trade_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.db","decl":{"annotations":[{"value":{"withIdPrimaryKey":true},"key":{"moduleName":"common.db","name":"DbTable"}}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"commodities","default":{"kind":"nothing"},"name":"commodities","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"Commodity"}},"parameters":[]}]}},{"annotations":[],"serializedName":"date","default":{"kind":"nothing"},"name":"date","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Instant"}},"parameters":[]}}]}},"name":"Trade","version":{"kind":"nothing"}}};

export const snTrade: ADL.ScopedName = {moduleName:"protoclient.protoapp.db", name:"Trade"};

export function texprTrade(): ADL.ATypeExpr<Trade> {
  return {value:{typeRef:{kind:"reference",value:snTrade},parameters:[]}};
}

export interface Commodity {
  code: string;
}

export function makeCommodity(
  input: {
    code: string,
  }
): Commodity {
  return {
    code: input.code,
  };
}

const Commodity_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"code","default":{"kind":"nothing"},"name":"code","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"Commodity","version":{"kind":"nothing"}}};

export const snCommodity: ADL.ScopedName = {moduleName:"protoclient.protoapp.db", name:"Commodity"};

export function texprCommodity(): ADL.ATypeExpr<Commodity> {
  return {value:{typeRef:{kind:"reference",value:snCommodity},parameters:[]}};
}

/**
 * Details for a user
 */
export interface AppUser {
  fullname: common_strings.StringNE;
  email: common_strings.StringNE;
  isAdmin: boolean;
  hashedPassword: common_strings.StringNE;
}

export function makeAppUser(
  input: {
    fullname: common_strings.StringNE,
    email: common_strings.StringNE,
    isAdmin: boolean,
    hashedPassword?: common_strings.StringNE,
  }
): AppUser {
  return {
    fullname: input.fullname,
    email: input.email,
    isAdmin: input.isAdmin,
    hashedPassword: input.hashedPassword === undefined ? "" : input.hashedPassword,
  };
}

const AppUser_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.db","decl":{"annotations":[{"value":{"indexes":[["email"]],"label":["fullname"],"withIdPrimaryKey":true},"key":{"moduleName":"common.db","name":"DbTable"}}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"fullname","default":{"kind":"nothing"},"name":"fullname","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"email","default":{"kind":"nothing"},"name":"email","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}},{"annotations":[],"serializedName":"isAdmin","default":{"kind":"nothing"},"name":"isAdmin","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"hashedPassword","default":{"kind":"just","value":""},"name":"hashedPassword","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}]}},"name":"AppUser","version":{"kind":"nothing"}}};

export const snAppUser: ADL.ScopedName = {moduleName:"protoclient.protoapp.db", name:"AppUser"};

export function texprAppUser(): ADL.ATypeExpr<AppUser> {
  return {value:{typeRef:{kind:"reference",value:snAppUser},parameters:[]}};
}

export type AppUserId = common_db.DbKey<AppUser>;

const AppUserId_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"AppUser"}},"parameters":[]}]}}},"name":"AppUserId","version":{"kind":"nothing"}}};

export const snAppUserId: ADL.ScopedName = {moduleName:"protoclient.protoapp.db", name:"AppUserId"};

export function texprAppUserId(): ADL.ATypeExpr<AppUserId> {
  return {value:{typeRef:{kind:"reference",value:snAppUserId},parameters:[]}};
}

/**
 * messages posted on the noticeboard
 */
export interface Message {
  postedAt: common.Instant;
  postedBy: common_db.DbKey<AppUser>;
  message: common_strings.StringML;
}

export function makeMessage(
  input: {
    postedAt: common.Instant,
    postedBy: common_db.DbKey<AppUser>,
    message: common_strings.StringML,
  }
): Message {
  return {
    postedAt: input.postedAt,
    postedBy: input.postedBy,
    message: input.message,
  };
}

const Message_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.db","decl":{"annotations":[{"value":{"indexes":[["posted_at"]],"withIdPrimaryKey":true},"key":{"moduleName":"common.db","name":"DbTable"}}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"postedAt","default":{"kind":"nothing"},"name":"postedAt","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Instant"}},"parameters":[]}},{"annotations":[],"serializedName":"postedBy","default":{"kind":"nothing"},"name":"postedBy","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"AppUser"}},"parameters":[]}]}},{"annotations":[],"serializedName":"message","default":{"kind":"nothing"},"name":"message","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringML"}},"parameters":[]}}]}},"name":"Message","version":{"kind":"nothing"}}};

export const snMessage: ADL.ScopedName = {moduleName:"protoclient.protoapp.db", name:"Message"};

export function texprMessage(): ADL.ATypeExpr<Message> {
  return {value:{typeRef:{kind:"reference",value:snMessage},parameters:[]}};
}

export type MessageId = common_db.DbKey<Message>;

const MessageId_AST : ADL.ScopedDecl =
  {"moduleName":"protoclient.protoapp.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"protoclient.protoapp.db","name":"Message"}},"parameters":[]}]}}},"name":"MessageId","version":{"kind":"nothing"}}};

export const snMessageId: ADL.ScopedName = {moduleName:"protoclient.protoapp.db", name:"MessageId"};

export function texprMessageId(): ADL.ATypeExpr<MessageId> {
  return {value:{typeRef:{kind:"reference",value:snMessageId},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "protoclient.protoapp.db.Trade" : Trade_AST,
  "protoclient.protoapp.db.Commodity" : Commodity_AST,
  "protoclient.protoapp.db.AppUser" : AppUser_AST,
  "protoclient.protoapp.db.AppUserId" : AppUserId_AST,
  "protoclient.protoapp.db.Message" : Message_AST,
  "protoclient.protoapp.db.MessageId" : MessageId_AST
};
