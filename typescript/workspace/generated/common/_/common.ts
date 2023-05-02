import * as common_strings from "../strings";
import * as ADL from "@adl-lang/runtime/adl";

/* @generated from adl module common */

/**
 * A instant in time, represented as milliseconds from
 * the epoch of "1970-01-01T00:00:00Z
 */
export type Instant = number;

const Instant_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}}},"name":"Instant","version":{"kind":"nothing"}}};

export const snInstant: ADL.ScopedName = {moduleName:"common", name:"Instant"};

export function texprInstant(): ADL.ATypeExpr<Instant> {
  return {value:{typeRef:{kind:"reference",value:snInstant},parameters:[]}};
}

/**
 * A date in ISO8601 format
 */
export type LocalDate = string;

const LocalDate_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"1970-01-01"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"LocalDate","version":{"kind":"nothing"}}};

export const snLocalDate: ADL.ScopedName = {moduleName:"common", name:"LocalDate"};

export function texprLocalDate(): ADL.ATypeExpr<LocalDate> {
  return {value:{typeRef:{kind:"reference",value:snLocalDate},parameters:[]}};
}

/**
 * A time in ISO8601 format
 */
export type LocalTime = string;

const LocalTime_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"00:00:00"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"LocalTime","version":{"kind":"nothing"}}};

export const snLocalTime: ADL.ScopedName = {moduleName:"common", name:"LocalTime"};

export function texprLocalTime(): ADL.ATypeExpr<LocalTime> {
  return {value:{typeRef:{kind:"reference",value:snLocalTime},parameters:[]}};
}

/**
 * A datetime in ISO8601 format
 */
export type LocalDateTime = string;

const LocalDateTime_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"1970-01-01T00:00:00"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"LocalDateTime","version":{"kind":"nothing"}}};

export const snLocalDateTime: ADL.ScopedName = {moduleName:"common", name:"LocalDateTime"};

export function texprLocalDateTime(): ADL.ATypeExpr<LocalDateTime> {
  return {value:{typeRef:{kind:"reference",value:snLocalDateTime},parameters:[]}};
}

/**
 * The day of the week
 */
export type DayOfWeek = 'monday' | 'tuesday' | 'wednesday' | 'thursday' | 'friday' | 'saturday' | 'sunday';
export const valuesDayOfWeek : DayOfWeek[] = ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'];

const DayOfWeek_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"union_","value":{"typeParams":[],"fields":[{"annotations":[],"serializedName":"monday","default":{"kind":"nothing"},"name":"monday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"tuesday","default":{"kind":"nothing"},"name":"tuesday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"wednesday","default":{"kind":"nothing"},"name":"wednesday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"thursday","default":{"kind":"nothing"},"name":"thursday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"friday","default":{"kind":"nothing"},"name":"friday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"saturday","default":{"kind":"nothing"},"name":"saturday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}},{"annotations":[],"serializedName":"sunday","default":{"kind":"nothing"},"name":"sunday","typeExpr":{"typeRef":{"kind":"primitive","value":"Void"},"parameters":[]}}]}},"name":"DayOfWeek","version":{"kind":"nothing"}}};

export const snDayOfWeek: ADL.ScopedName = {moduleName:"common", name:"DayOfWeek"};

export function texprDayOfWeek(): ADL.ATypeExpr<DayOfWeek> {
  return {value:{typeRef:{kind:"reference",value:snDayOfWeek},parameters:[]}};
}

/**
 * A duration in ISO8601 format
 */
export type Duration = string;

const Duration_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[{"value":"interval","key":{"moduleName":"common.db","name":"DbColumnType"}}],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"just","value":"P1D"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Duration","version":{"kind":"nothing"}}};

export const snDuration: ADL.ScopedName = {moduleName:"common", name:"Duration"};

export function texprDuration(): ADL.ATypeExpr<Duration> {
  return {value:{typeRef:{kind:"reference",value:snDuration},parameters:[]}};
}

/**
 * An IANA timezone
 */
export type Timezone = common_strings.StringNE;

const Timezone_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common.strings","name":"StringNE"}},"parameters":[]}}},"name":"Timezone","version":{"kind":"nothing"}}};

export const snTimezone: ADL.ScopedName = {moduleName:"common", name:"Timezone"};

export function texprTimezone(): ADL.ATypeExpr<Timezone> {
  return {value:{typeRef:{kind:"reference",value:snTimezone},parameters:[]}};
}

/**
 * A holder for paginated results
 */
export interface Paginated<T> {
/**
 * The paginated items
 */
  items: T[];
/**
 * The offset used for this query
 */
  current_offset: number;
/**
 * The size of the entire date set
 */
  total_size: number;
}

export function makePaginated<T>(
  input: {
    items: T[],
    current_offset: number,
    total_size: number,
  }
): Paginated<T> {
  return {
    items: input.items,
    current_offset: input.current_offset,
    total_size: input.total_size,
  };
}

const Paginated_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"items","default":{"kind":"nothing"},"name":"items","typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"current_offset","default":{"kind":"nothing"},"name":"current_offset","typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}},{"annotations":[],"serializedName":"total_size","default":{"kind":"nothing"},"name":"total_size","typeExpr":{"typeRef":{"kind":"primitive","value":"Int64"},"parameters":[]}}]}},"name":"Paginated","version":{"kind":"nothing"}}};

export const snPaginated: ADL.ScopedName = {moduleName:"common", name:"Paginated"};

export function texprPaginated<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Paginated<T>> {
  return {value:{typeRef:{kind:"reference",value:snPaginated},parameters:[texprT.value]}};
}

/**
 * Empty Struct (Used mostly for Void RPC responses)
 */
export interface Unit {
}

export function makeUnit(
  _input: {}
): Unit {
  return {};
}

const Unit_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":[],"fields":[]}},"name":"Unit","version":{"kind":"nothing"}}};

export const snUnit: ADL.ScopedName = {moduleName:"common", name:"Unit"};

export function texprUnit(): ADL.ATypeExpr<Unit> {
  return {value:{typeRef:{kind:"reference",value:snUnit},parameters:[]}};
}

/**
 * Phantom type to capture a StringMap with a named string key type:
 */
export type StringKeyMap<_K, V> = {[key: string]: V};

const StringKeyMap_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["K","V"],"typeExpr":{"typeRef":{"kind":"primitive","value":"StringMap"},"parameters":[{"typeRef":{"kind":"typeParam","value":"V"},"parameters":[]}]}}},"name":"StringKeyMap","version":{"kind":"nothing"}}};

export const snStringKeyMap: ADL.ScopedName = {moduleName:"common", name:"StringKeyMap"};

export function texprStringKeyMap<K, V>(texprK : ADL.ATypeExpr<K>, texprV : ADL.ATypeExpr<V>): ADL.ATypeExpr<StringKeyMap<K, V>> {
  return {value:{typeRef:{kind:"reference",value:snStringKeyMap},parameters:[texprK.value,texprV.value]}};
}

/**
 * Naming aid for strings used as keys
 */
export type Key<_T> = string;

const Key_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"type_","value":{"typeParams":["T"],"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"Key","version":{"kind":"nothing"}}};

export const snKey: ADL.ScopedName = {moduleName:"common", name:"Key"};

export function texprKey<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Key<T>> {
  return {value:{typeRef:{kind:"reference",value:snKey},parameters:[texprT.value]}};
}

/**
 * A value of type T along with the Key<T>
 */
export interface WithKey<T> {
  key: Key<T>;
  value: T;
}

export function makeWithKey<T>(
  input: {
    key: Key<T>,
    value: T,
  }
): WithKey<T> {
  return {
    key: input.key,
    value: input.value,
  };
}

const WithKey_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[],"type_":{"kind":"struct_","value":{"typeParams":["T"],"fields":[{"annotations":[],"serializedName":"key","default":{"kind":"nothing"},"name":"key","typeExpr":{"typeRef":{"kind":"reference","value":{"moduleName":"common","name":"Key"}},"parameters":[{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}]}},{"annotations":[],"serializedName":"value","default":{"kind":"nothing"},"name":"value","typeExpr":{"typeRef":{"kind":"typeParam","value":"T"},"parameters":[]}}]}},"name":"WithKey","version":{"kind":"nothing"}}};

export const snWithKey: ADL.ScopedName = {moduleName:"common", name:"WithKey"};

export function texprWithKey<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<WithKey<T>> {
  return {value:{typeRef:{kind:"reference",value:snWithKey},parameters:[texprT.value]}};
}

/**
 * Postgres array of strings type that is serialized in to a list of Strings
 */
export type StringList = string[];

const StringList_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[{"value":"text[]","key":{"moduleName":"common.db","name":"DbColumnType"}}],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"Vector"},"parameters":[{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}]}}},"name":"StringList","version":{"kind":"nothing"}}};

export const snStringList: ADL.ScopedName = {moduleName:"common", name:"StringList"};

export function texprStringList(): ADL.ATypeExpr<StringList> {
  return {value:{typeRef:{kind:"reference",value:snStringList},parameters:[]}};
}

export type TSVector = string;

const TSVector_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[{"value":"tsvector","key":{"moduleName":"common.db","name":"DbColumnType"}}],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"TSVector","version":{"kind":"nothing"}}};

export const snTSVector: ADL.ScopedName = {moduleName:"common", name:"TSVector"};

export function texprTSVector(): ADL.ATypeExpr<TSVector> {
  return {value:{typeRef:{kind:"reference",value:snTSVector},parameters:[]}};
}

/**
 * Postgres Geography type that is serialized using GeoJson
 */
export type GeographyGeoJson = string;

const GeographyGeoJson_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[{"value":"geography","key":{"moduleName":"common.db","name":"DbColumnType"}}],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"GeographyGeoJson","version":{"kind":"nothing"}}};

export const snGeographyGeoJson: ADL.ScopedName = {moduleName:"common", name:"GeographyGeoJson"};

export function texprGeographyGeoJson(): ADL.ATypeExpr<GeographyGeoJson> {
  return {value:{typeRef:{kind:"reference",value:snGeographyGeoJson},parameters:[]}};
}

/**
 * Postgres Geometry type
 */
export type GeometryWKT = string;

const GeometryWKT_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[{"value":"geometry","key":{"moduleName":"common.db","name":"DbColumnType"}}],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"GeometryWKT","version":{"kind":"nothing"}}};

export const snGeometryWKT: ADL.ScopedName = {moduleName:"common", name:"GeometryWKT"};

export function texprGeometryWKT(): ADL.ATypeExpr<GeometryWKT> {
  return {value:{typeRef:{kind:"reference",value:snGeometryWKT},parameters:[]}};
}

/**
 * A floating point decimal value
 */
export type BigDecimal = string;

const BigDecimal_AST : ADL.ScopedDecl =
  {"moduleName":"common","decl":{"annotations":[{"value":"numeric","key":{"moduleName":"common.db","name":"DbColumnType"}}],"type_":{"kind":"newtype_","value":{"typeParams":[],"default":{"kind":"nothing"},"typeExpr":{"typeRef":{"kind":"primitive","value":"String"},"parameters":[]}}},"name":"BigDecimal","version":{"kind":"nothing"}}};

export const snBigDecimal: ADL.ScopedName = {moduleName:"common", name:"BigDecimal"};

export function texprBigDecimal(): ADL.ATypeExpr<BigDecimal> {
  return {value:{typeRef:{kind:"reference",value:snBigDecimal},parameters:[]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "common.Instant" : Instant_AST,
  "common.LocalDate" : LocalDate_AST,
  "common.LocalTime" : LocalTime_AST,
  "common.LocalDateTime" : LocalDateTime_AST,
  "common.DayOfWeek" : DayOfWeek_AST,
  "common.Duration" : Duration_AST,
  "common.Timezone" : Timezone_AST,
  "common.Paginated" : Paginated_AST,
  "common.Unit" : Unit_AST,
  "common.StringKeyMap" : StringKeyMap_AST,
  "common.Key" : Key_AST,
  "common.WithKey" : WithKey_AST,
  "common.StringList" : StringList_AST,
  "common.TSVector" : TSVector_AST,
  "common.GeographyGeoJson" : GeographyGeoJson_AST,
  "common.GeometryWKT" : GeometryWKT_AST,
  "common.BigDecimal" : BigDecimal_AST
};
