import { DeclResolver, ATypeExpr } from './adl$TSEXT';
import * as AST from './sys/adlast$TSEXT';
import * as ANN from '../sys/annotations$TSEXT';
import {
  Json,
  JsonArray,
  JsonObject,
  asJsonObject,
  createJsonBinding,
  getAnnotation
} from './json$TSEXT';

export interface TypeDiscs {
  max_version: number,
  type_discs: TypeDisc[],
  // record of vector branches where the element has of TypeDiscs
  vector_discs: Record<string, TypeDiscs>,
}
export interface TypeDisc {
  // field: AST.Field,
  name: string,
  serializedName: string,
  typeExpr: AST.TypeExpr,
  disc: ANN.TypeDiscrimination,
}

export function liftIntoUnion<T>(
  dresolver: DeclResolver,
  texpr: ATypeExpr<T>,
  json: Json,
): Json {
  const tds = getTypeDiscriminations(dresolver, texpr.value)
  return liftTypeDiscriminations(dresolver, json, tds)
}

export function getTypeDiscriminations(
  dresolver: DeclResolver,
  texpr: AST.TypeExpr,
): TypeDiscs {
  const ret: TypeDiscs = {
    max_version: -1,
    type_discs: [],
    vector_discs: {},
  }
  if (texpr.typeRef.kind === "reference") {
    const jb = createJsonBinding(dresolver, ANN.texprTypeDiscrimination())
    const ast = dresolver(texpr.typeRef.value);
    if (ast.decl.type_.kind === "union_") {
      ret.type_discs = ast.decl.type_.value.fields.flatMap(fld => {
        if (fld.typeExpr.typeRef.kind === "primitive" && fld.typeExpr.typeRef.value === "Vector") {
          const elem_td = getTypeDiscriminations(dresolver, fld.typeExpr.parameters[0])
          // if (elem_td.)
          ret.vector_discs[fld.serializedName] = elem_td
        }
        const ann = getAnnotation(jb, fld.annotations)
        if (ann) {
          if (ann.version > ret.max_version) {
            ret.max_version = ann.version
          }
          return [{
            ...fld,
            disc: ann,
          }]
        }
        return []
      })
    }
  }
  return ret
}

export function liftTypeDiscriminations(
  dresolver: DeclResolver,
  json: Json,
  type_discs: TypeDiscs,
): Json {
  const isDisc = (type_disc: TypeDisc) => {
    const expanded_texpr = expandTypes(
      dresolver,
      type_disc.typeExpr,
      { expandNewType: true, expandTypeAliases: true },
    )
    return matchTypeDiscrimination(dresolver, json, type_disc, expanded_texpr)
  }
  for (const td of type_discs.type_discs) {
    const elem_td = type_discs.vector_discs[td.serializedName]
    if (elem_td !== undefined) {
      if (elem_td.type_discs.length !== 0) {
        throw Error(`union of vector of a union containing TypeDiscrimination branches not supported. Field ${td.name}`)
      }
    }
  }
  const mtd = type_discs.type_discs.filter(isDisc)
  if (mtd.length === 0) {
    if (json instanceof Object && !(json instanceof Array)) {
      const jobj = json as JsonObject;
      const keys = Object.keys(jobj).filter(k => k != "@v")
      if (keys.length != 1) {
        throw Error(`doesn't have the form of a union. elements ${keys} ${JSON.stringify(jobj)}`)
      }
      const json1 = jobj[keys[0]]
      if (Array.isArray(json1)) {
        const elem_tds = type_discs.vector_discs[keys[0]]
        if (elem_tds !== undefined) {
          const json0: JsonObject = {}
          json0[keys[0]] = json1.map(j => liftTypeDiscriminations(dresolver, j, elem_tds))
          const v = jobj["@v"]
          if (v !== undefined) {
            json0["@v"] = v
          }
          return json0
        }
      }
    }
    return json
  }
  if (mtd.length > 1) {
    throw Error(`ambiguous matching type discriminators ${mtd.map(el => el.name)}`)
  }
  const json1: Json = {}
  if (Array.isArray(json)) {
    json1[mtd[0].serializedName] = json.map(j => {
      const elem_tds = getTypeDiscriminations(dresolver, mtd[0].typeExpr.parameters[0])
      return liftTypeDiscriminations(dresolver, j, elem_tds)
    })
  } else {
    json1[mtd[0].serializedName] = json
  }
  json1["@v"] = type_discs.max_version
  return json1
}

function liftArray(
  dresolver: DeclResolver,
  json: JsonArray,
  texpr: AST.TypeExpr,
): JsonArray {
  const elem_tds = getTypeDiscriminations(dresolver, texpr)
  return json.map(j => {
    return liftTypeDiscriminations(dresolver, j, elem_tds)
  })
}

export function matchTypeDiscrimination(
  dresolver: DeclResolver,
  json: Json,
  type_disc: TypeDisc,
  expanded_texpr: AST.TypeExpr,
): boolean {
  let typeRef = expanded_texpr.typeRef
  if (typeRef.kind === "typeParam") {
    return false
  }
  if (typeRef.kind === "primitive" && (typeRef.value === "Json" || typeRef.value === "Void")) {
    throw Error(`cannot use Json or Void as a type discriminator`)
  }
  switch (type_disc.disc.disc_cfg.kind) {
    case "by_type": {
      if (json === null) {
        if (typeRef.kind === "primitive" && typeRef.value === "Nullable") {
          return true
        }
        throw Error(`primitive type mismatch. expected "Nullable" received ${JSON.stringify(typeRef)}`)
      }
      if (Array.isArray(json)) {
        return json.find(j => !matchTypeDiscrimination(dresolver, j, type_disc, expanded_texpr.parameters[0])) === undefined
        // throw Error("not yet implemented")
        // if (typeRef.kind === "primitive" && typeRef.value === "Vector") {
        //   return matchTypeDiscrimination(DeclResolver,)
        // }
        // throw Error(`primitive type mismatch. expected "Vector" received ${JSON.stringify(typeRef)}`)
      }
      if (typeRef.kind === "primitive" && typeRef.value === "Nullable") {
        typeRef = expanded_texpr.parameters[0].typeRef
        if (typeRef.kind === "primitive" && typeRef.value === "Vector") {
          throw Error(`lifting of Nullable<Vector<>> not implemented`)
        }
        if (typeRef.kind === "primitive" && typeRef.value === "Nullable") {
          throw Error(`lifting of Nullable<Nullable<>> not implemented`)
        }
      }
      switch (typeof json) {
        case "string":
          return isPrimitiveOf(typeRef, "String")
        case "object":
          return matchObject(dresolver, json, type_disc.name, expanded_texpr)
        case "number":
          return isPrimitiveOfs(typeRef, adlNumbers)
        case "boolean":
          return isPrimitiveOf(typeRef, "Bool")
        // case "bigint":
        default:
          break
      }
      return false
    }
    case "obj_props":
      if (json === null) {
        return isPrimitiveOf(typeRef, "Nullable")
      }
      if (Array.isArray(json)) {
        throw Error("not implemented or not implementable - TODO: figure out which")
      }
      const jobj = asJsonObject(json)
      if (jobj === undefined) {
        throw Error("expecting object")
      }
      return matchObject(dresolver, jobj, type_disc.name, expanded_texpr)
    default:
      assertNever(type_disc.disc.disc_cfg)
  }
}

function matchObject(
  dresolver: DeclResolver,
  json: JsonObject,
  fname: string,
  // type_disc: TypeDisc,
  expanded_texpr: AST.TypeExpr,
): boolean {
  const typeRef = expanded_texpr.typeRef
  if (typeRef.kind === "primitive" && typeRef.value === "StringMap") {
    return true
  }
  if (typeRef.kind === "reference") {
    const sd = dresolver(typeRef.value)
    // decl can only be struct or union, because of the earlier expandTypes(...)
    if (sd.decl.type_.kind === "struct_") {
      const names = sd.decl.type_.value.fields.filter(f => f.default.kind === "nothing").map(f => f.name)
      const keys = Object.keys(json)
      for (let i = 0; i < names.length; i++) {
        if (!keys.includes(names[i])) {
          return false
        }
      }
      return true
    }
    if (sd.decl.type_.kind === "union_") {
      const u_tds = getTypeDiscriminations(dresolver, expanded_texpr)
      if (u_tds.type_discs.length > 0) {
        throw Error(`union of union containing TypeDiscrimination branches not supported : ${fname}`)
      }
      const key = Object.keys(json).find(k => k !== "@v")
      return sd.decl.type_.value.fields.find(f => f.serializedName === key) !== undefined
    }
  }
  return false
}

function isPrimitiveOf(typeRef: AST.TypeRef, type_: string): boolean {
  if (typeRef.kind === "primitive" && typeRef.value === type_) {
    return true
  }
  throw Error(`primitive type mismatch. expected ${type_} received ${JSON.stringify(typeRef)}`)
  // return false
}

function isPrimitiveOfs(typeRef: AST.TypeRef, types: string[]): boolean {
  if (typeRef.kind === "primitive" && types.includes(typeRef.value)) {
    return true
  }
  throw Error(`primitive type mismatch. expected ${types} received ${JSON.stringify(typeRef)}`)
}

const adlNumbers = [
  "Int8",
  "Int16",
  "Int32",
  "Int64",
  "Word8",
  "Word16",
  "Word32",
  "Word64",
  "Float",
  "Double",
]

export function substituteTypeVariable(
  typeExpr: AST.TypeExpr,
  tparam: string,
  tvalue: AST.TypeExpr,
): AST.TypeExpr {
  if (
    typeExpr.typeRef.kind == "typeParam" && typeExpr.typeRef.value == tparam &&
    typeExpr.parameters.length == 0
  ) {
    return tvalue;
  }
  return {
    typeRef: typeExpr.typeRef,
    parameters: typeExpr.parameters.map((p) =>
      substituteTypeVariable(p, tparam, tvalue)
    ),
  };
}

export function substituteTypeVariables(
  typeExpr0: AST.TypeExpr,
  tparams: string[],
  tvalues: AST.TypeExpr[],
): AST.TypeExpr {
  let typeExpr = typeExpr0;
  for (let i = 0; i < tparams.length; i++) {
    typeExpr = substituteTypeVariable(typeExpr, tparams[i], tvalues[i]);
  }
  return typeExpr;
}

export function expandTypeAlias(
  resolver: DeclResolver,
  typeExpr: AST.TypeExpr,
): AST.TypeExpr | null {
  if (typeExpr.typeRef.kind == "reference") {
    const sdecl = resolver(typeExpr.typeRef.value);
    const dtype = sdecl.decl.type_;
    if (dtype.kind == "type_") {
      const tparams = dtype.value.typeParams;
      const tvalues = typeExpr.parameters;
      return substituteTypeVariables(dtype.value.typeExpr, tparams, tvalues);
    }
  }
  return null;
}

export function expandNewType(
  resolver: DeclResolver,
  typeExpr: AST.TypeExpr,
): AST.TypeExpr | null {
  if (typeExpr.typeRef.kind == "reference") {
    const sdecl = resolver(typeExpr.typeRef.value);
    const dtype = sdecl.decl.type_;
    if (dtype.kind == "newtype_") {
      const tparams = dtype.value.typeParams;
      const tvalues = typeExpr.parameters;
      return substituteTypeVariables(dtype.value.typeExpr, tparams, tvalues);
    }
  }
  return null;
}

/**
 * Recursively xpand type aliases and/or newtypes by substitution
 */
export function expandTypes(
  resolver: DeclResolver,
  typeExpr: AST.TypeExpr,
  options: ExpandTypeOptions,
): AST.TypeExpr {
  switch (typeExpr.typeRef.kind) {
    case "primitive":
      break;
    case "reference":
      let texpr2 = null;
      if (options.expandTypeAliases) {
        texpr2 = texpr2 || expandTypeAlias(resolver, typeExpr);
      }
      if (options.expandNewType) {
        texpr2 = texpr2 || expandNewType(resolver, typeExpr);
      }
      if (texpr2) {
        return expandTypes(resolver, texpr2, options);
      }
      break;
    case "typeParam":
      break;
  }
  return typeExpr;
}

interface ExpandTypeOptions {
  expandTypeAliases?: boolean;
  expandNewType?: boolean;
}

function assertNever(x: never): never {
  throw new Error('cannot reach here!');
}
