import * as ADL from "@adl-lang/runtime/adl";
import * as sys_types from "@adl-lang/sys/types";

/* @generated from adl module common.ui */

export type FormLabel = string;

const FormLabel_AST : ADL.ScopedDecl =
  {"moduleName":"common.ui","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"FormLabel","version":{"kind":"nothing"}}};

export const snFormLabel: ADL.ScopedName = {moduleName:"common.ui", name:"FormLabel"};

export function texprFormLabel(): ADL.ATypeExpr<FormLabel> {
  return {value:{typeRef:{kind:"reference",value:snFormLabel},parameters:[]}};
}

export type FormGroupKey = string;

const FormGroupKey_AST : ADL.ScopedDecl =
  {"moduleName":"common.ui","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":[],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"FormGroupKey","version":{"kind":"nothing"}}};

export const snFormGroupKey: ADL.ScopedName = {moduleName:"common.ui", name:"FormGroupKey"};

export function texprFormGroupKey(): ADL.ATypeExpr<FormGroupKey> {
  return {value:{typeRef:{kind:"reference",value:snFormGroupKey},parameters:[]}};
}

export interface FormGroups {
  defaultKey: FormGroupKey;
  labels: sys_types.Pair<FormGroupKey, string>[];
}

export function makeFormGroups(
  input: {
    defaultKey: FormGroupKey,
    labels: sys_types.Pair<FormGroupKey, string>[],
  }
): FormGroups {
  return {
    defaultKey: input.defaultKey,
    labels: input.labels,
  };
}

const FormGroups_AST : ADL.ScopedDecl =
  {"moduleName":"common.ui","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"defaultKey","default":{"kind":"nothing"},"name":"defaultKey","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.ui","name":"FormGroupKey"}},"parameters":[]}},{"annotations":[],"serializedName":"labels","default":{"kind":"nothing"},"name":"labels","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"sys.types","name":"Pair"}},"parameters":[{"typeRef":{"kind":"reference","value":{"moduleName":"common.ui","name":"FormGroupKey"}},"parameters":[]},{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}]}}]}},"name":"FormGroups","version":{"kind":"nothing"}}};

export const snFormGroups: ADL.ScopedName = {moduleName:"common.ui", name:"FormGroups"};

export function texprFormGroups(): ADL.ATypeExpr<FormGroups> {
  return {value:{typeRef:{kind:"reference",value:snFormGroups},parameters:[]}};
}

/**
 * An field/type alias annotation to constrain the
 * values allowed by a string to the enumerated values
 */
export interface ValidValues {
/**
 * The allowed values
 */
  values: string[];
/**
 * A (short) user readable string describing the
 * expected text.
 */
  description: string;
}

export function makeValidValues(
  input: {
    values: string[],
    description: string,
  }
): ValidValues {
  return {
    values: input.values,
    description: input.description,
  };
}

const ValidValues_AST : ADL.ScopedDecl =
  {"moduleName":"common.ui","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"values","default":{"kind":"nothing"},"name":"values","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}]}},"name":"ValidValues","version":{"kind":"nothing"}}};

export const snValidValues: ADL.ScopedName = {moduleName:"common.ui", name:"ValidValues"};

export function texprValidValues(): ADL.ATypeExpr<ValidValues> {
  return {value:{typeRef:{kind:"reference",value:snValidValues},parameters:[]}};
}

/**
 * An field/type alias annotation to constrain the
 * values allowed by a string to a regular expression
 */
export interface ValidRegex {
/**
 * The regexp that must be matched
 */
  regex: string;
/**
 * A (short) user readable string describing the
 * expected text.
 */
  description: string;
/**
 * The regex group index to return if matches
 * 0 is the entire string
 */
  returnGroup: number;
}

export function makeValidRegex(
  input: {
    regex: string,
    description: string,
    returnGroup?: number,
  }
): ValidRegex {
  return {
    regex: input.regex,
    description: input.description,
    returnGroup: input.returnGroup === undefined ? 0 : input.returnGroup,
  };
}

const ValidRegex_AST : ADL.ScopedDecl =
  {"moduleName":"common.ui","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"regex","default":{"kind":"nothing"},"name":"regex","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"description","default":{"kind":"nothing"},"name":"description","typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}},{"annotations":[],"serializedName":"returnGroup","default":{"kind":"just","value":0},"name":"returnGroup","typeExpr":{"typeRef":{"kind":"primitive","value":"Int8"},"parameters":[]}}]}},"name":"ValidRegex","version":{"kind":"nothing"}}};

export const snValidRegex: ADL.ScopedName = {moduleName:"common.ui", name:"ValidRegex"};

export function texprValidRegex(): ADL.ATypeExpr<ValidRegex> {
  return {value:{typeRef:{kind:"reference",value:snValidRegex},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.ui.FormLabel" : FormLabel_AST,
  "common.ui.FormGroupKey" : FormGroupKey_AST,
  "common.ui.FormGroups" : FormGroups_AST,
  "common.ui.ValidValues" : ValidValues_AST,
  "common.ui.ValidRegex" : ValidRegex_AST
};
