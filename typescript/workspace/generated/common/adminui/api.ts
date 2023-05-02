import * as common from "./../_/common";
import * as common_db from "./../db";
import * as common_flyway_api from "./../flyway/api";
import * as common_http from "./../http";
import * as common_tabular from "./../tabular";
import * as common_adminui_db from "./db";
import * as ADL from "@adl-lang/runtime/adl";
import * as sys_adlast from "@adl-lang/sys/adlast";
import * as sys_types from "@adl-lang/sys/types";

/* @generated from adl module common.adminui.api */

export type AdlValue = {}|null;

const AdlValue_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}}},"name":"AdlValue","version":{"kind":"nothing"}}};

export const snAdlValue: ADL.ScopedName = {moduleName:"common.adminui.api", name:"AdlValue"};

export function texprAdlValue(): ADL.ATypeExpr<AdlValue> {
  return {value:{typeRef:{kind:"reference",value:snAdlValue},parameters:[]}};
}

export type DbRow = {[key: string]: AdlValue};

const DbRow_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"AdlValue"}},"parameters":[]}]}}},"name":"DbRow","version":{"kind":"nothing"}}};

export const snDbRow: ADL.ScopedName = {moduleName:"common.adminui.api", name:"DbRow"};

export function texprDbRow(): ADL.ATypeExpr<DbRow> {
  return {value:{typeRef:{kind:"reference",value:snDbRow},parameters:[]}};
}

export type DbId = common_db.DbKey<DbRow>;

const DbId_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"DbKey"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbRow"}},"parameters":[]}]}}},"name":"DbId","version":{"kind":"nothing"}}};

export const snDbId: ADL.ScopedName = {moduleName:"common.adminui.api", name:"DbId"};

export function texprDbId(): ADL.ATypeExpr<DbId> {
  return {value:{typeRef:{kind:"reference",value:snDbId},parameters:[]}};
}

/**
 * Endpoints supporting CRUD for the Admin UI,
 * along with metadata access.
 */
export interface AdminApiRequests {
/**
 * Query the tables available for admin access
 */
  queryTables: common_http.HttpPost<common_tabular.TableQuery, common.Paginated<Table>>;
/**
 * Query the ADL declarations used in the column
 * type declarations
 */
  queryDecls: common_http.HttpPost<common_tabular.TableQuery, common.Paginated<common_adminui_db.MetaAdlDecl>>;
/**
 * Query rows from a table, specifying pagination,
 * filtering and sorting. As per it's return type, this method
 * can only be used for tables with a string primary key
 * called `id`.
 */
  query: common_http.HttpPost<QueryReq, common.Paginated<common_db.WithDbId<DbRow>>>;
/**
 * Query rows from a table without requiring a primary key, specifying pagination,
 * filtering and sorting.
 */
  queryWithoutIds: common_http.HttpPost<QueryReq, common.Paginated<DbRow>>;
/**
 * Create a new row in a table
 */
  create: common_http.HttpPost<CreateReq, DbResult<DbId>>;
/**
 * Update a single existing row in a table
 */
  update: common_http.HttpPost<UpdateReq, DbResult<common.Unit>>;
/**
 * Delete a single existing row in a table
 */
  delete: common_http.HttpPost<DeleteReq, DbResult<common.Unit>>;
/**
 * Fetch descriptive strings for db keys
 */
  dbKeyLabels: common_http.HttpPost<DbKeyLabelReq[], DbKeyLabelResp[]>;
/**
 * Flyway migration handled by adminui
 */
  flyway: common_http.HttpPost<common_flyway_api.FlywayReq, common_flyway_api.FlywayResp>;
}

export function makeAdminApiRequests(
  input: {
    queryTables?: common_http.HttpPost<common_tabular.TableQuery, common.Paginated<Table>>,
    queryDecls?: common_http.HttpPost<common_tabular.TableQuery, common.Paginated<common_adminui_db.MetaAdlDecl>>,
    query?: common_http.HttpPost<QueryReq, common.Paginated<common_db.WithDbId<DbRow>>>,
    queryWithoutIds?: common_http.HttpPost<QueryReq, common.Paginated<DbRow>>,
    create?: common_http.HttpPost<CreateReq, DbResult<DbId>>,
    update?: common_http.HttpPost<UpdateReq, DbResult<common.Unit>>,
    delete?: common_http.HttpPost<DeleteReq, DbResult<common.Unit>>,
    dbKeyLabels?: common_http.HttpPost<DbKeyLabelReq[], DbKeyLabelResp[]>,
    flyway?: common_http.HttpPost<common_flyway_api.FlywayReq, common_flyway_api.FlywayResp>,
  }
): AdminApiRequests {
  return {
    queryTables: input.queryTables === undefined ? {path : "/admin/meta/tables", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : common_tabular.texprTableQuery(), respType : common.texprPaginated(texprTable())} : input.queryTables,
    queryDecls: input.queryDecls === undefined ? {path : "/admin/meta/decls", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : common_tabular.texprTableQuery(), respType : common.texprPaginated(common_adminui_db.texprMetaAdlDecl())} : input.queryDecls,
    query: input.query === undefined ? {path : "/admin/query", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : texprQueryReq(), respType : common.texprPaginated(common_db.texprWithDbId(texprDbRow()))} : input.query,
    queryWithoutIds: input.queryWithoutIds === undefined ? {path : "/admin/query-without-ids", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : texprQueryReq(), respType : common.texprPaginated(texprDbRow())} : input.queryWithoutIds,
    create: input.create === undefined ? {path : "/admin/create", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : texprCreateReq(), respType : texprDbResult(texprDbId())} : input.create,
    update: input.update === undefined ? {path : "/admin/update", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : texprUpdateReq(), respType : texprDbResult(common.texprUnit())} : input.update,
    delete: input.delete === undefined ? {path : "/admin/delete", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : texprDeleteReq(), respType : texprDbResult(common.texprUnit())} : input.delete,
    dbKeyLabels: input.dbKeyLabels === undefined ? {path : "/admin/dbkeylabels", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : ADL.texprVector(texprDbKeyLabelReq()), respType : ADL.texprVector(texprDbKeyLabelResp())} : input.dbKeyLabels,
    flyway: input.flyway === undefined ? {path : "/admin/flyway", security : {kind : "tokenWithRole", value : "admin"}, rateLimit : null, reqType : common_flyway_api.texprFlywayReq(), respType : common_flyway_api.texprFlywayResp()} : input.flyway,
  };
}

const AdminApiRequests_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"queryTables","default":{"kind":"just","value":{"path":"/admin/meta/tables","security":{"tokenWithRole":"admin"}}},"name":"queryTables","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"TableQuery"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"Table"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"queryDecls","default":{"kind":"just","value":{"path":"/admin/meta/decls","security":{"tokenWithRole":"admin"}}},"name":"queryDecls","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"TableQuery"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.db","name":"MetaAdlDecl"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"query","default":{"kind":"just","value":{"path":"/admin/query","security":{"tokenWithRole":"admin"}}},"name":"query","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"QueryReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbRow"}},"parameters":[]}]}]}]}},{"annotations":[],"serializedName":"queryWithoutIds","default":{"kind":"just","value":{"path":"/admin/query-without-ids","security":{"tokenWithRole":"admin"}}},"name":"queryWithoutIds","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"QueryReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Paginated"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbRow"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"create","default":{"kind":"just","value":{"path":"/admin/create","security":{"tokenWithRole":"admin"}}},"name":"create","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"CreateReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbResult"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbId"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"update","default":{"kind":"just","value":{"path":"/admin/update","security":{"tokenWithRole":"admin"}}},"name":"update","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"UpdateReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbResult"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"delete","default":{"kind":"just","value":{"path":"/admin/delete","security":{"tokenWithRole":"admin"}}},"name":"delete","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DeleteReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbResult"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Unit"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"dbKeyLabels","default":{"kind":"just","value":{"path":"/admin/dbkeylabels","security":{"tokenWithRole":"admin"}}},"name":"dbKeyLabels","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbKeyLabelReq"}},"parameters":[]}]},{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbKeyLabelResp"}},"parameters":[]}]}]}},{"annotations":[],"serializedName":"flyway","default":{"kind":"just","value":{"path":"/admin/flyway","security":{"tokenWithRole":"admin"}}},"name":"flyway","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.http","name":"HttpPost"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayReq"}},"parameters":[]},{"typeRef":{"kind":"reference","value":{"moduleName":"common.flyway.api","name":"FlywayResp"}},"parameters":[]}]}}]}},"name":"AdminApiRequests","version":{"kind":"nothing"}}};

export const snAdminApiRequests: ADL.ScopedName = {moduleName:"common.adminui.api", name:"AdminApiRequests"};

export function texprAdminApiRequests(): ADL.ATypeExpr<AdminApiRequests> {
  return {value:{typeRef:{kind:"reference",value:snAdminApiRequests},parameters:[]}};
}

export interface QueryReq {
  table: string;
/**
 * The columns to be included in the result. If empty, all
 * columns will be included.
 */
  columns: string[];
  query: common_tabular.TableQuery;
}

export function makeQueryReq(
  input: {
    table: string,
    columns: string[],
    query: common_tabular.TableQuery,
  }
): QueryReq {
  return {
    table: input.table,
    columns: input.columns,
    query: input.query,
  };
}

const QueryReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"table","default":{"kind":"nothing"},"name":"table","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"columns","default":{"kind":"nothing"},"name":"columns","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"query","default":{"kind":"nothing"},"name":"query","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"TableQuery"}},"parameters":[]}}]}},"name":"QueryReq","version":{"kind":"nothing"}}};

export const snQueryReq: ADL.ScopedName = {moduleName:"common.adminui.api", name:"QueryReq"};

export function texprQueryReq(): ADL.ATypeExpr<QueryReq> {
  return {value:{typeRef:{kind:"reference",value:snQueryReq},parameters:[]}};
}

export interface Table {
  name: string;
  label: string;
  description: string;
  hasIdPrimaryKey: boolean;
/**
 * If true, the current user is allowed to query existingRows
 */
  allowQuery: boolean;
/**
 * If true, the current user is allowed to insert new rows
 */
  allowInsert: boolean;
/**
 * If true, the current user is allowed to delete rows
 */
  allowDelete: boolean;
/**
 * If true, the current user is allowed to update rows
 */
  allowUpdate: boolean;
  columns: TableColumn[];
  declModuleName: sys_adlast.ModuleName;
  declName: string;
}

export function makeTable(
  input: {
    name: string,
    label: string,
    description: string,
    hasIdPrimaryKey: boolean,
    allowQuery: boolean,
    allowInsert: boolean,
    allowDelete: boolean,
    allowUpdate: boolean,
    columns: TableColumn[],
    declModuleName: sys_adlast.ModuleName,
    declName: string,
  }
): Table {
  return {
    name: input.name,
    label: input.label,
    description: input.description,
    hasIdPrimaryKey: input.hasIdPrimaryKey,
    allowQuery: input.allowQuery,
    allowInsert: input.allowInsert,
    allowDelete: input.allowDelete,
    allowUpdate: input.allowUpdate,
    columns: input.columns,
    declModuleName: input.declModuleName,
    declName: input.declName,
  };
}

const Table_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"label","default":{"kind":"nothing"},"name":"label","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"hasIdPrimaryKey","default":{"kind":"nothing"},"name":"hasIdPrimaryKey","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"allowQuery","default":{"kind":"nothing"},"name":"allowQuery","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"allowInsert","default":{"kind":"nothing"},"name":"allowInsert","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"allowDelete","default":{"kind":"nothing"},"name":"allowDelete","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"allowUpdate","default":{"kind":"nothing"},"name":"allowUpdate","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"columns","default":{"kind":"nothing"},"name":"columns","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"TableColumn"}},"parameters":[]}]}},{"annotations":[],"serializedName":"declModuleName","default":{"kind":"nothing"},"name":"declModuleName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ModuleName"}},"parameters":[]}},{"annotations":[],"serializedName":"declName","default":{"kind":"nothing"},"name":"declName","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"Table","version":{"kind":"nothing"}}};

export const snTable: ADL.ScopedName = {moduleName:"common.adminui.api", name:"Table"};

export function texprTable(): ADL.ATypeExpr<Table> {
  return {value:{typeRef:{kind:"reference",value:snTable},parameters:[]}};
}

export type Access = 'readOnly' | 'readWrite';
export const valuesAccess : Access[] = ['readOnly', 'readWrite'];

const Access_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"readOnly","default":{"kind":"nothing"},"name":"readOnly","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"readWrite","default":{"kind":"nothing"},"name":"readWrite","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"Access","version":{"kind":"nothing"}}};

export const snAccess: ADL.ScopedName = {moduleName:"common.adminui.api", name:"Access"};

export function texprAccess(): ADL.ATypeExpr<Access> {
  return {value:{typeRef:{kind:"reference",value:snAccess},parameters:[]}};
}

export interface TableColumn {
  name: string;
  label: string;
  description: string;
  typeExpr: sys_adlast.TypeExpr;
  defaultValue: sys_types.Maybe<{}|null>;
  generated: boolean;
  access: Access;
}

export function makeTableColumn(
  input: {
    name: string,
    label: string,
    description: string,
    typeExpr: sys_adlast.TypeExpr,
    defaultValue: sys_types.Maybe<{}|null>,
    generated: boolean,
    access: Access,
  }
): TableColumn {
  return {
    name: input.name,
    label: input.label,
    description: input.description,
    typeExpr: input.typeExpr,
    defaultValue: input.defaultValue,
    generated: input.generated,
    access: input.access,
  };
}

const TableColumn_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"name","default":{"kind":"nothing"},"name":"name","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"label","default":{"kind":"nothing"},"name":"label","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"typeExpr","default":{"kind":"nothing"},"name":"typeExpr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"TypeExpr"}},"parameters":[]}},{"annotations":[],"serializedName":"defaultValue","default":{"kind":"nothing"},"name":"defaultValue","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Maybe"}},"parameters":[{"typeRef":{"kind":"primitive","value":"Json"},"parameters":[]}]}},{"annotations":[],"serializedName":"generated","default":{"kind":"nothing"},"name":"generated","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"access","default":{"kind":"nothing"},"name":"access","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"Access"}},"parameters":[]}}]}},"name":"TableColumn","version":{"kind":"nothing"}}};

export const snTableColumn: ADL.ScopedName = {moduleName:"common.adminui.api", name:"TableColumn"};

export function texprTableColumn(): ADL.ATypeExpr<TableColumn> {
  return {value:{typeRef:{kind:"reference",value:snTableColumn},parameters:[]}};
}

export interface CreateReq {
  table: string;
  values: DbRow;
}

export function makeCreateReq(
  input: {
    table: string,
    values: DbRow,
  }
): CreateReq {
  return {
    table: input.table,
    values: input.values,
  };
}

const CreateReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"table","default":{"kind":"nothing"},"name":"table","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"values","default":{"kind":"nothing"},"name":"values","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbRow"}},"parameters":[]}}]}},"name":"CreateReq","version":{"kind":"nothing"}}};

export const snCreateReq: ADL.ScopedName = {moduleName:"common.adminui.api", name:"CreateReq"};

export function texprCreateReq(): ADL.ATypeExpr<CreateReq> {
  return {value:{typeRef:{kind:"reference",value:snCreateReq},parameters:[]}};
}

export interface UpdateReq {
  table: string;
  values: common_db.WithDbId<DbRow>;
}

export function makeUpdateReq(
  input: {
    table: string,
    values: common_db.WithDbId<DbRow>,
  }
): UpdateReq {
  return {
    table: input.table,
    values: input.values,
  };
}

const UpdateReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"table","default":{"kind":"nothing"},"name":"table","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"values","default":{"kind":"nothing"},"name":"values","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.db","name":"WithDbId"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbRow"}},"parameters":[]}]}}]}},"name":"UpdateReq","version":{"kind":"nothing"}}};

export const snUpdateReq: ADL.ScopedName = {moduleName:"common.adminui.api", name:"UpdateReq"};

export function texprUpdateReq(): ADL.ATypeExpr<UpdateReq> {
  return {value:{typeRef:{kind:"reference",value:snUpdateReq},parameters:[]}};
}

export interface DeleteReq {
  table: string;
  id: DbId;
}

export function makeDeleteReq(
  input: {
    table: string,
    id: DbId,
  }
): DeleteReq {
  return {
    table: input.table,
    id: input.id,
  };
}

const DeleteReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"table","default":{"kind":"nothing"},"name":"table","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.adminui.api","name":"DbId"}},"parameters":[]}}]}},"name":"DeleteReq","version":{"kind":"nothing"}}};

export const snDeleteReq: ADL.ScopedName = {moduleName:"common.adminui.api", name:"DeleteReq"};

export function texprDeleteReq(): ADL.ATypeExpr<DeleteReq> {
  return {value:{typeRef:{kind:"reference",value:snDeleteReq},parameters:[]}};
}

export interface DbResult_Ok<T> {
  kind: 'ok';
  value: T;
}
export interface DbResult_DbError<_T> {
  kind: 'dbError';
  value: string;
}

export type DbResult<T> = DbResult_Ok<T> | DbResult_DbError<T>;

export interface DbResultOpts<T> {
  ok: T;
  dbError: string;
}

export function makeDbResult<T, K extends keyof DbResultOpts<T>>(kind: K, value: DbResultOpts<T>[K]) { return {kind, value}; }

const DbResult_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"ok","default":{"kind":"nothing"},"name":"ok","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}},{"annotations":[],"serializedName":"dbError","default":{"kind":"nothing"},"name":"dbError","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"DbResult","version":{"kind":"nothing"}}};

export const snDbResult: ADL.ScopedName = {moduleName:"common.adminui.api", name:"DbResult"};

export function texprDbResult<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<DbResult<T>> {
  return {value:{typeRef:{kind:"reference",value:snDbResult},parameters:[texprT.value]}};
}

export interface DbKeyLabelReq {
  scopedName: sys_adlast.ScopedName;
  id: string;
}

export function makeDbKeyLabelReq(
  input: {
    scopedName: sys_adlast.ScopedName,
    id: string,
  }
): DbKeyLabelReq {
  return {
    scopedName: input.scopedName,
    id: input.id,
  };
}

const DbKeyLabelReq_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"scopedName","default":{"kind":"nothing"},"name":"scopedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}},"parameters":[]}},{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"DbKeyLabelReq","version":{"kind":"nothing"}}};

export const snDbKeyLabelReq: ADL.ScopedName = {moduleName:"common.adminui.api", name:"DbKeyLabelReq"};

export function texprDbKeyLabelReq(): ADL.ATypeExpr<DbKeyLabelReq> {
  return {value:{typeRef:{kind:"reference",value:snDbKeyLabelReq},parameters:[]}};
}

export interface DbKeyLabelResp {
  scopedName: sys_adlast.ScopedName;
  id: string;
  label: string;
}

export function makeDbKeyLabelResp(
  input: {
    scopedName: sys_adlast.ScopedName,
    id: string,
    label: string,
  }
): DbKeyLabelResp {
  return {
    scopedName: input.scopedName,
    id: input.id,
    label: input.label,
  };
}

const DbKeyLabelResp_AST : ADL.ScopedDecl =
  {"moduleName":"common.adminui.api","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"scopedName","default":{"kind":"nothing"},"name":"scopedName","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"sys.adlast","name":"ScopedName"}},"parameters":[]}},{"annotations":[],"serializedName":"id","default":{"kind":"nothing"},"name":"id","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"label","default":{"kind":"nothing"},"name":"label","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"DbKeyLabelResp","version":{"kind":"nothing"}}};

export const snDbKeyLabelResp: ADL.ScopedName = {moduleName:"common.adminui.api", name:"DbKeyLabelResp"};

export function texprDbKeyLabelResp(): ADL.ATypeExpr<DbKeyLabelResp> {
  return {value:{typeRef:{kind:"reference",value:snDbKeyLabelResp},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.adminui.api.AdlValue" : AdlValue_AST,
  "common.adminui.api.DbRow" : DbRow_AST,
  "common.adminui.api.DbId" : DbId_AST,
  "common.adminui.api.AdminApiRequests" : AdminApiRequests_AST,
  "common.adminui.api.QueryReq" : QueryReq_AST,
  "common.adminui.api.Table" : Table_AST,
  "common.adminui.api.Access" : Access_AST,
  "common.adminui.api.TableColumn" : TableColumn_AST,
  "common.adminui.api.CreateReq" : CreateReq_AST,
  "common.adminui.api.UpdateReq" : UpdateReq_AST,
  "common.adminui.api.DeleteReq" : DeleteReq_AST,
  "common.adminui.api.DbResult" : DbResult_AST,
  "common.adminui.api.DbKeyLabelReq" : DbKeyLabelReq_AST,
  "common.adminui.api.DbKeyLabelResp" : DbKeyLabelResp_AST
};
