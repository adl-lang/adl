/* @generated from adl module adlc.codegen.batch */

import * as ADL from './../../runtime/adl.ts';
import * as adlc_codegen_ast from './ast.ts';
import * as adlc_codegen_java from './java.ts';
import * as adlc_codegen_typescript from './typescript.ts';

export interface BatchItem_Ast {
  kind: 'ast';
  value: adlc_codegen_ast.AstParams;
}
export interface BatchItem_Java {
  kind: 'java';
  value: adlc_codegen_java.JavaParams;
}
export interface BatchItem_Typescript {
  kind: 'typescript';
  value: adlc_codegen_typescript.TypescriptParams;
}

export type BatchItem = BatchItem_Ast | BatchItem_Java | BatchItem_Typescript;

export interface BatchItemOpts {
  ast: adlc_codegen_ast.AstParams;
  java: adlc_codegen_java.JavaParams;
  typescript: adlc_codegen_typescript.TypescriptParams;
}

export function makeBatchItem<K extends keyof BatchItemOpts>(kind: K, value: BatchItemOpts[K]) { return {kind, value}; }

const BatchItem_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.batch","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"ast","default":{"kind":"nothing"},"name":"ast","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.ast","name":"AstParams"}},"parameters":[]}},{"annotations":[],"serializedName":"java","default":{"kind":"nothing"},"name":"java","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.java","name":"JavaParams"}},"parameters":[]}},{"annotations":[],"serializedName":"typescript","default":{"kind":"nothing"},"name":"typescript","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.typescript","name":"TypescriptParams"}},"parameters":[]}}]}},"name":"BatchItem","version":{"kind":"nothing"}}};

export const snBatchItem: ADL.ScopedName = {moduleName:"adlc.codegen.batch", name:"BatchItem"};

export function texprBatchItem(): ADL.ATypeExpr<BatchItem> {
  return {value : {typeRef : {kind: "reference", value : snBatchItem}, parameters : []}};
}

export type Batch = BatchItem[];

const Batch_AST : ADL.ScopedDecl =
  {"moduleName":"adlc.codegen.batch","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"adlc.codegen.batch","name":"BatchItem"}},"parameters":[]}]}}},"name":"Batch","version":{"kind":"nothing"}}};

export const snBatch: ADL.ScopedName = {moduleName:"adlc.codegen.batch", name:"Batch"};

export function texprBatch(): ADL.ATypeExpr<Batch> {
  return {value : {typeRef : {kind: "reference", value : snBatch}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "adlc.codegen.batch.BatchItem" : BatchItem_AST,
  "adlc.codegen.batch.Batch" : Batch_AST
};
