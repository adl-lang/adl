import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.db */

export interface DbTable {
  tableName: string;
  withIdPrimaryKey: boolean;
  withPrimaryKey: string[];
  indexes: string[][];
  uniquenessConstraints: string[][];
  extraSql: string[];
  label: string[];
}

export function makeDbTable(
  input: {
    tableName?: string,
    withIdPrimaryKey?: boolean,
    withPrimaryKey?: string[],
    indexes?: string[][],
    uniquenessConstraints?: string[][],
    extraSql?: string[],
    label?: string[],
  }
): DbTable {
  return {
    tableName: input.tableName === undefined ? "" : input.tableName,
    withIdPrimaryKey: input.withIdPrimaryKey === undefined ? false : input.withIdPrimaryKey,
    withPrimaryKey: input.withPrimaryKey === undefined ? [] : input.withPrimaryKey,
    indexes: input.indexes === undefined ? [] : input.indexes,
    uniquenessConstraints: input.uniquenessConstraints === undefined ? [] : input.uniquenessConstraints,
    extraSql: input.extraSql === undefined ? [] : input.extraSql,
    label: input.label === undefined ? [] : input.label,
  };
}

const DbTable_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"tableName","default":{"kind":"just","value":""},"name":"tableName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"withIdPrimaryKey","default":{"kind":"just","value":false},"name":"withIdPrimaryKey","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"withPrimaryKey","default":{"kind":"just","value":[]},"name":"withPrimaryKey","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"indexes","default":{"kind":"just","value":[]},"name":"indexes","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}]}},{"annotations":[],"serializedName":"uniquenessConstraints","default":{"kind":"just","value":[]},"name":"uniquenessConstraints","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}]}},{"annotations":[],"serializedName":"extraSql","default":{"kind":"just","value":[]},"name":"extraSql","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"label","default":{"kind":"just","value":[]},"name":"label","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"DbTable","version":{"kind":"nothing"}}};

export const snDbTable: ADL.ScopedName = {moduleName:"common.db", name:"DbTable"};

export function texprDbTable(): ADL.ATypeExpr<DbTable> {
  return {value:{typeRef:{kind:"reference",value:snDbTable},parameters:[]}};
}

export interface DbView {
  viewName: string;
  withIdPrimaryKey: boolean;
  viewSql: string[];
}

export function makeDbView(
  input: {
    viewName?: string,
    withIdPrimaryKey?: boolean,
    viewSql?: string[],
  }
): DbView {
  return {
    viewName: input.viewName === undefined ? "" : input.viewName,
    withIdPrimaryKey: input.withIdPrimaryKey === undefined ? false : input.withIdPrimaryKey,
    viewSql: input.viewSql === undefined ? [] : input.viewSql,
  };
}

const DbView_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"viewName","default":{"kind":"just","value":""},"name":"viewName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"withIdPrimaryKey","default":{"kind":"just","value":false},"name":"withIdPrimaryKey","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"viewSql","default":{"kind":"just","value":[]},"name":"viewSql","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}]}},"name":"DbView","version":{"kind":"nothing"}}};

export const snDbView: ADL.ScopedName = {moduleName:"common.db", name:"DbView"};

export function texprDbView(): ADL.ATypeExpr<DbView> {
  return {value:{typeRef:{kind:"reference",value:snDbView},parameters:[]}};
}

/**
 * Field level annotation to override the name of the
 * database column.
 */
export type DbColumnName = string;

const DbColumnName_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DbColumnName","version":{"kind":"nothing"}}};

export const snDbColumnName: ADL.ScopedName = {moduleName:"common.db", name:"DbColumnName"};

export function texprDbColumnName(): ADL.ATypeExpr<DbColumnName> {
  return {value:{typeRef:{kind:"reference",value:snDbColumnName},parameters:[]}};
}

/**
 * Field or type level annotation to override the type of the
 * database column.
 */
export type DbColumnType = string;

const DbColumnType_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DbColumnType","version":{"kind":"nothing"}}};

export const snDbColumnType: ADL.ScopedName = {moduleName:"common.db", name:"DbColumnType"};

export function texprDbColumnType(): ADL.ATypeExpr<DbColumnType> {
  return {value:{typeRef:{kind:"reference",value:snDbColumnType},parameters:[]}};
}

/**
 * Field level annotation to indicate that a column value
 * is generated, and hence need not be requested from or edited
 * by a user.
 */
export type DbColumnGenerated = null;

const DbColumnGenerated_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}},"name":"DbColumnGenerated","version":{"kind":"nothing"}}};

export const snDbColumnGenerated: ADL.ScopedName = {moduleName:"common.db", name:"DbColumnGenerated"};

export function texprDbColumnGenerated(): ADL.ATypeExpr<DbColumnGenerated> {
  return {value:{typeRef:{kind:"reference",value:snDbColumnGenerated},parameters:[]}};
}

/**
 * A reference for a database stored value, referenced by a
 * string primary key.
 */
export type DbKey<_T> = string;

const DbKey_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":["T"],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"DbKey","version":{"kind":"nothing"}}};

export const snDbKey: ADL.ScopedName = {moduleName:"common.db", name:"DbKey"};

export function texprDbKey<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<DbKey<T>> {
  return {value:{typeRef:{kind:"reference",value:snDbKey},parameters:[texprT.value]}};
}

/**
 * A value of type T along with a unique db identifier
 */
export interface WithDbId<T> {
  id: DbKey<T>;
  value: T;
}

export function makeWithDbId<T>(
  input: {
    id: DbKey<T>,
    value: T,
  }
): WithDbId<T> {
  return {
    id: input.id,
    value: input.value,
  };
}

const WithDbId_AST : ADL.ScopedDecl =
  {"moduleName":"common.db","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"WithDbId","version":{"kind":"nothing"}}};

export const snWithDbId: ADL.ScopedName = {moduleName:"common.db", name:"WithDbId"};

export function texprWithDbId<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<WithDbId<T>> {
  return {value:{typeRef:{kind:"reference",value:snWithDbId},parameters:[texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.db.DbTable" : DbTable_AST,
  "common.db.DbView" : DbView_AST,
  "common.db.DbColumnName" : DbColumnName_AST,
  "common.db.DbColumnType" : DbColumnType_AST,
  "common.db.DbColumnGenerated" : DbColumnGenerated_AST,
  "common.db.DbKey" : DbKey_AST,
  "common.db.WithDbId" : WithDbId_AST
};
