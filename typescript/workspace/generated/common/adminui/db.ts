import * as ADL from "@adl-lang/runtime/adl";
import * as sys_adlast from "@adl-lang/sys/adlast";

/* @generated from adl module common.adminui.db */

export interface MetaTable {
  name: string;
  description: string;
  declModuleName: sys_adlast.ModuleName;
  declName: string;
}

export function makeMetaTable(
  input: {
    name: string,
    description: string,
    declModuleName: sys_adlast.ModuleName,
    declName: string,
  }
): MetaTable {
  return {
    name: input.name,
    description: input.description,
    declModuleName: input.declModuleName,
    declName: input.declName,
  };
}

const MetaTable_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.db","decl":{"annotations":[{"value":{"uniquenessConstraints":[["name"]],"withPrimaryKey":["name"]},"key":{"moduleName":"common.db","name":"DbTable"}}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"declModuleName","default":{"kind":"nothing"},"name":"declModuleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"declName","default":{"kind":"nothing"},"name":"declName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"MetaTable","version":{"kind":"nothing"}}};

export const snMetaTable: ADL.ScopedName = {moduleName:"common.adminui.db", name:"MetaTable"};

export function texprMetaTable(): ADL.ATypeExpr<MetaTable> {
  return {value:{typeRef:{kind:"reference",value:snMetaTable},parameters:[]}};
}

export interface MetaAdlDecl {
  moduleName: sys_adlast.ModuleName;
  name: string;
  decl: sys_adlast.Decl;
}

export function makeMetaAdlDecl(
  input: {
    moduleName: sys_adlast.ModuleName,
    name: string,
    decl: sys_adlast.Decl,
  }
): MetaAdlDecl {
  return {
    moduleName: input.moduleName,
    name: input.name,
    decl: input.decl,
  };
}

const MetaAdlDecl_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.db","decl":{"annotations":[{"value":{"uniquenessConstraints":[["moduleName","name"]],"withPrimaryKey":["moduleName","name"]},"key":{"moduleName":"common.db","name":"DbTable"}}],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"moduleName","default":{"kind":"nothing"},"name":"moduleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"decl","default":{"kind":"nothing"},"name":"decl","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"Decl"}},"parameters":[]}}]}},"name":"MetaAdlDecl","version":{"kind":"nothing"}}};

export const snMetaAdlDecl: ADL.ScopedName = {moduleName:"common.adminui.db", name:"MetaAdlDecl"};

export function texprMetaAdlDecl(): ADL.ATypeExpr<MetaAdlDecl> {
  return {value:{typeRef:{kind:"reference",value:snMetaAdlDecl},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.adminui.db.MetaTable" : MetaTable_AST,
  "common.adminui.db.MetaAdlDecl" : MetaAdlDecl_AST
};
