import * as common from "./_/common";
import * as common_strings from "./strings";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common.tabular */

/**
 * Common definitions for querying tabular data
 */
export type FieldName = common_strings.StringNE;

const FieldName_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}},"name":"FieldName","version":{"kind":"nothing"}}};

export const snFieldName: ADL.ScopedName = {moduleName:"common.tabular", name:"FieldName"};

export function texprFieldName(): ADL.ATypeExpr<FieldName> {
  return {value:{typeRef:{kind:"reference",value:snFieldName},parameters:[]}};
}

export interface ExprLike {
  expr: Expr;
  pattern: string;
  caseSensitive: boolean;
}

export function makeExprLike(
  input: {
    expr: Expr,
    pattern: string,
    caseSensitive?: boolean,
  }
): ExprLike {
  return {
    expr: input.expr,
    pattern: input.pattern,
    caseSensitive: input.caseSensitive === undefined ? true : input.caseSensitive,
  };
}

const ExprLike_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"expr","default":{"kind":"nothing"},"name":"expr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}},{"annotations":[],"serializedName":"pattern","default":{"kind":"nothing"},"name":"pattern","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"caseSensitive","default":{"kind":"just","value":true},"name":"caseSensitive","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"ExprLike","version":{"kind":"nothing"}}};

export const snExprLike: ADL.ScopedName = {moduleName:"common.tabular", name:"ExprLike"};

export function texprExprLike(): ADL.ATypeExpr<ExprLike> {
  return {value:{typeRef:{kind:"reference",value:snExprLike},parameters:[]}};
}

export interface ExprIn {
  expr: Expr;
  exprs: Expr[];
}

export function makeExprIn(
  input: {
    expr: Expr,
    exprs: Expr[],
  }
): ExprIn {
  return {
    expr: input.expr,
    exprs: input.exprs,
  };
}

const ExprIn_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"expr","default":{"kind":"nothing"},"name":"expr","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}},{"annotations":[],"serializedName":"exprs","default":{"kind":"nothing"},"name":"exprs","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}]}}]}},"name":"ExprIn","version":{"kind":"nothing"}}};

export const snExprIn: ADL.ScopedName = {moduleName:"common.tabular", name:"ExprIn"};

export function texprExprIn(): ADL.ATypeExpr<ExprIn> {
  return {value:{typeRef:{kind:"reference",value:snExprIn},parameters:[]}};
}

export interface ExprComparison {
  expr1: Expr;
  expr2: Expr;
}

export function makeExprComparison(
  input: {
    expr1: Expr,
    expr2: Expr,
  }
): ExprComparison {
  return {
    expr1: input.expr1,
    expr2: input.expr2,
  };
}

const ExprComparison_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"expr1","default":{"kind":"nothing"},"name":"expr1","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}},{"annotations":[],"serializedName":"expr2","default":{"kind":"nothing"},"name":"expr2","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}}]}},"name":"ExprComparison","version":{"kind":"nothing"}}};

export const snExprComparison: ADL.ScopedName = {moduleName:"common.tabular", name:"ExprComparison"};

export function texprExprComparison(): ADL.ATypeExpr<ExprComparison> {
  return {value:{typeRef:{kind:"reference",value:snExprComparison},parameters:[]}};
}

export interface FieldPredicate_EqualTo {
  kind: 'equalTo';
  value: ExprComparison;
}
export interface FieldPredicate_In {
  kind: 'in';
  value: ExprIn;
}
export interface FieldPredicate_Like {
  kind: 'like';
  value: ExprLike;
}
export interface FieldPredicate_Isnull {
  kind: 'isnull';
  value: Expr;
}
export interface FieldPredicate_Not {
  kind: 'not';
  value: FieldPredicate;
}
export interface FieldPredicate_GreaterThan {
  kind: 'greaterThan';
  value: ExprComparison;
}
export interface FieldPredicate_LessThan {
  kind: 'lessThan';
  value: ExprComparison;
}
export interface FieldPredicate_And {
  kind: 'and';
  value: FieldPredicate[];
}
export interface FieldPredicate_Or {
  kind: 'or';
  value: FieldPredicate[];
}
export interface FieldPredicate_Literal {
  kind: 'literal';
  value: boolean;
}

export type FieldPredicate = FieldPredicate_EqualTo | FieldPredicate_In | FieldPredicate_Like | FieldPredicate_Isnull | FieldPredicate_Not | FieldPredicate_GreaterThan | FieldPredicate_LessThan | FieldPredicate_And | FieldPredicate_Or | FieldPredicate_Literal;

export interface FieldPredicateOpts {
  equalTo: ExprComparison;
  in: ExprIn;
  like: ExprLike;
  isnull: Expr;
  not: FieldPredicate;
  greaterThan: ExprComparison;
  lessThan: ExprComparison;
  and: FieldPredicate[];
  or: FieldPredicate[];
  literal: boolean;
}

export function makeFieldPredicate<K extends keyof FieldPredicateOpts>(kind: K, value: FieldPredicateOpts[K]) { return {kind, value}; }

const FieldPredicate_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"equalTo","default":{"kind":"nothing"},"name":"equalTo","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ExprComparison"}},"parameters":[]}},{"annotations":[],"serializedName":"in","default":{"kind":"nothing"},"name":"in","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ExprIn"}},"parameters":[]}},{"annotations":[],"serializedName":"like","default":{"kind":"nothing"},"name":"like","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ExprLike"}},"parameters":[]}},{"annotations":[],"serializedName":"isnull","default":{"kind":"nothing"},"name":"isnull","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}},{"annotations":[],"serializedName":"not","default":{"kind":"nothing"},"name":"not","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}},{"annotations":[],"serializedName":"greaterThan","default":{"kind":"nothing"},"name":"greaterThan","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ExprComparison"}},"parameters":[]}},{"annotations":[],"serializedName":"lessThan","default":{"kind":"nothing"},"name":"lessThan","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"ExprComparison"}},"parameters":[]}},{"annotations":[],"serializedName":"and","default":{"kind":"nothing"},"name":"and","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}]}},{"annotations":[],"serializedName":"or","default":{"kind":"nothing"},"name":"or","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}]}},{"annotations":[],"serializedName":"literal","default":{"kind":"nothing"},"name":"literal","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}]}},"name":"FieldPredicate","version":{"kind":"nothing"}}};

export const snFieldPredicate: ADL.ScopedName = {moduleName:"common.tabular", name:"FieldPredicate"};

export function texprFieldPredicate(): ADL.ATypeExpr<FieldPredicate> {
  return {value:{typeRef:{kind:"reference",value:snFieldPredicate},parameters:[]}};
}

export interface Expr_String {
  kind: 'string';
  value: string;
}
export interface Expr_Int {
  kind: 'int';
  value: number;
}
export interface Expr_Bool {
  kind: 'bool';
  value: boolean;
}
export interface Expr_Date {
  kind: 'date';
  value: common.LocalDate;
}
export interface Expr_Instant {
  kind: 'instant';
  value: common.Instant;
}
export interface Expr_Field {
  kind: 'field';
  value: FieldName;
}
export interface Expr_CurrentDate {
  kind: 'currentDate';
}
export interface Expr_Concat {
  kind: 'concat';
  value: Expr[];
}

export type Expr = Expr_String | Expr_Int | Expr_Bool | Expr_Date | Expr_Instant | Expr_Field | Expr_CurrentDate | Expr_Concat;

export interface ExprOpts {
  string: string;
  int: number;
  bool: boolean;
  date: common.LocalDate;
  instant: common.Instant;
  field: FieldName;
  currentDate: null;
  concat: Expr[];
}

export function makeExpr<K extends keyof ExprOpts>(kind: K, value: ExprOpts[K]) { return {kind, value}; }

const Expr_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"string","default":{"kind":"nothing"},"name":"string","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"int","default":{"kind":"nothing"},"name":"int","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"bool","default":{"kind":"nothing"},"name":"bool","typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}},{"annotations":[],"serializedName":"date","default":{"kind":"nothing"},"name":"date","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"LocalDate"}},"parameters":[]}},{"annotations":[],"serializedName":"instant","default":{"kind":"nothing"},"name":"instant","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Instant"}},"parameters":[]}},{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"currentDate","default":{"kind":"nothing"},"name":"currentDate","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"concat","default":{"kind":"nothing"},"name":"concat","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"Expr"}},"parameters":[]}]}}]}},"name":"Expr","version":{"kind":"nothing"}}};

export const snExpr: ADL.ScopedName = {moduleName:"common.tabular", name:"Expr"};

export function texprExpr(): ADL.ATypeExpr<Expr> {
  return {value:{typeRef:{kind:"reference",value:snExpr},parameters:[]}};
}

export type SortDirection = 'ascending' | 'descending';
export const valuesSortDirection : SortDirection[] = ['ascending', 'descending'];

const SortDirection_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ascending","default":{"kind":"nothing"},"name":"ascending","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"descending","default":{"kind":"nothing"},"name":"descending","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"SortDirection","version":{"kind":"nothing"}}};

export const snSortDirection: ADL.ScopedName = {moduleName:"common.tabular", name:"SortDirection"};

export function texprSortDirection(): ADL.ATypeExpr<SortDirection> {
  return {value:{typeRef:{kind:"reference",value:snSortDirection},parameters:[]}};
}

export interface SortField {
  field: FieldName;
  direction: SortDirection;
}

export function makeSortField(
  input: {
    field: FieldName,
    direction: SortDirection,
  }
): SortField {
  return {
    field: input.field,
    direction: input.direction,
  };
}

const SortField_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"field","default":{"kind":"nothing"},"name":"field","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}},{"annotations":[],"serializedName":"direction","default":{"kind":"nothing"},"name":"direction","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"SortDirection"}},"parameters":[]}}]}},"name":"SortField","version":{"kind":"nothing"}}};

export const snSortField: ADL.ScopedName = {moduleName:"common.tabular", name:"SortField"};

export function texprSortField(): ADL.ATypeExpr<SortField> {
  return {value:{typeRef:{kind:"reference",value:snSortField},parameters:[]}};
}

export interface TableView {
  columns: FieldName[];
  filter: FieldPredicate;
  sorting: SortField[];
}

export function makeTableView(
  input: {
    columns: FieldName[],
    filter?: FieldPredicate,
    sorting?: SortField[],
  }
): TableView {
  return {
    columns: input.columns,
    filter: input.filter === undefined ? {kind : "literal", value : true} : input.filter,
    sorting: input.sorting === undefined ? [] : input.sorting,
  };
}

const TableView_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"columns","default":{"kind":"nothing"},"name":"columns","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldName"}},"parameters":[]}]}},{"annotations":[],"serializedName":"filter","default":{"kind":"just","value":{"literal":true}},"name":"filter","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}},{"annotations":[],"serializedName":"sorting","default":{"kind":"just","value":[]},"name":"sorting","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"SortField"}},"parameters":[]}]}}]}},"name":"TableView","version":{"kind":"nothing"}}};

export const snTableView: ADL.ScopedName = {moduleName:"common.tabular", name:"TableView"};

export function texprTableView(): ADL.ATypeExpr<TableView> {
  return {value:{typeRef:{kind:"reference",value:snTableView},parameters:[]}};
}

export interface TableQuery {
  filter: FieldPredicate;
  sorting: SortField[];
  offset: number;
  count: number;
}

export function makeTableQuery(
  input: {
    filter?: FieldPredicate,
    sorting?: SortField[],
    offset?: number,
    count?: number,
  }
): TableQuery {
  return {
    filter: input.filter === undefined ? {kind : "literal", value : true} : input.filter,
    sorting: input.sorting === undefined ? [] : input.sorting,
    offset: input.offset === undefined ? 0 : input.offset,
    count: input.count === undefined ? -1 : input.count,
  };
}

const TableQuery_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"filter","default":{"kind":"just","value":{"literal":true}},"name":"filter","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"FieldPredicate"}},"parameters":[]}},{"annotations":[],"serializedName":"sorting","default":{"kind":"just","value":[]},"name":"sorting","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.tabular","name":"SortField"}},"parameters":[]}]}},{"annotations":[],"serializedName":"offset","default":{"kind":"just","value":0},"name":"offset","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}},{"annotations":[],"serializedName":"count","default":{"kind":"just","value":-1},"name":"count","typeExpr":{"typeRef":{"kind":"primitive","value":"Int32"},"parameters":[]}}]}},"name":"TableQuery","version":{"kind":"nothing"}}};

export const snTableQuery: ADL.ScopedName = {moduleName:"common.tabular", name:"TableQuery"};

export function texprTableQuery(): ADL.ATypeExpr<TableQuery> {
  return {value:{typeRef:{kind:"reference",value:snTableQuery},parameters:[]}};
}

export interface SingleField<T> {
  value: T;
}

export function makeSingleField<T>(
  input: {
    value: T,
  }
): SingleField<T> {
  return {
    value: input.value,
  };
}

const SingleField_AST : ADL.ScopedDecl =
  {"moduleName":"common.tabular","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"SingleField","version":{"kind":"nothing"}}};

export const snSingleField: ADL.ScopedName = {moduleName:"common.tabular", name:"SingleField"};

export function texprSingleField<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<SingleField<T>> {
  return {value:{typeRef:{kind:"reference",value:snSingleField},parameters:[texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.tabular.FieldName" : FieldName_AST,
  "common.tabular.ExprLike" : ExprLike_AST,
  "common.tabular.ExprIn" : ExprIn_AST,
  "common.tabular.ExprComparison" : ExprComparison_AST,
  "common.tabular.FieldPredicate" : FieldPredicate_AST,
  "common.tabular.Expr" : Expr_AST,
  "common.tabular.SortDirection" : SortDirection_AST,
  "common.tabular.SortField" : SortField_AST,
  "common.tabular.TableView" : TableView_AST,
  "common.tabular.TableQuery" : TableQuery_AST,
  "common.tabular.SingleField" : SingleField_AST
};
