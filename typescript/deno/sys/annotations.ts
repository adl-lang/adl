/* @generated from adl module sys.annotations */

import * as ADL from './../runtime/adl.ts';

export type Doc = string;

const Doc_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Doc","version":{"kind":"nothing"}}};

export const snDoc: ADL.ScopedName = {moduleName:"sys.annotations", name:"Doc"};

export function texprDoc(): ADL.ATypeExpr<Doc> {
  return {value : {typeRef : {kind: "reference", value : snDoc}, parameters : []}};
}

export type SerializedName = string;

const SerializedName_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"SerializedName","version":{"kind":"nothing"}}};

export const snSerializedName: ADL.ScopedName = {moduleName:"sys.annotations", name:"SerializedName"};

export function texprSerializedName(): ADL.ATypeExpr<SerializedName> {
  return {value : {typeRef : {kind: "reference", value : snSerializedName}, parameters : []}};
}

export interface SerializedWithInternalTag {
  tag: string;
}

export function makeSerializedWithInternalTag(
  input: {
    tag: string,
  }
): SerializedWithInternalTag {
  return {
    tag: input.tag,
  };
}

const SerializedWithInternalTag_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"tag","default":{"kind":"nothing"},"name":"tag","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"SerializedWithInternalTag","version":{"kind":"nothing"}}};

export const snSerializedWithInternalTag: ADL.ScopedName = {moduleName:"sys.annotations", name:"SerializedWithInternalTag"};

export function texprSerializedWithInternalTag(): ADL.ATypeExpr<SerializedWithInternalTag> {
  return {value : {typeRef : {kind: "reference", value : snSerializedWithInternalTag}, parameters : []}};
}

export type CustomSerialization = boolean;

const CustomSerialization_AST : ADL.ScopedDecl =
  {"moduleName":"sys.annotations","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"Bool"},"parameters":[]}}},"name":"CustomSerialization","version":{"kind":"nothing"}}};

export const snCustomSerialization: ADL.ScopedName = {moduleName:"sys.annotations", name:"CustomSerialization"};

export function texprCustomSerialization(): ADL.ATypeExpr<CustomSerialization> {
  return {value : {typeRef : {kind: "reference", value : snCustomSerialization}, parameters : []}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "sys.annotations.Doc" : Doc_AST,
  "sys.annotations.SerializedName" : SerializedName_AST,
  "sys.annotations.SerializedWithInternalTag" : SerializedWithInternalTag_AST,
  "sys.annotations.CustomSerialization" : CustomSerialization_AST
};
