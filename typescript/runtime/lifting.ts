import { DeclResolver, ATypeExpr } from './adl$TSEXT';
import * as AST from './sys/adlast$TSEXT';
import * as ANN from '../sys/annotations$TSEXT';
import {
  Json,
  JsonArray,
  JsonBinding,
  JsonObject,
  asJsonObject,
  createJsonBinding,
  getAnnotation,
  hasAnnotation
} from './json$TSEXT';
import { isEnum } from './utils$TSEXT';

export interface Lifter {
  lift(json: Json): Json
}

function isJsonArray(jv: Json): jv is JsonArray {
  return jv instanceof Array
}
function isJsonObject(jv: Json): jv is JsonObject {
  return jv instanceof Object && !(jv instanceof Array)
}

export function createLifter(
  dresolver: DeclResolver,
  texpr: AST.TypeExpr,
): Lifter {
  return buildLifter(dresolver, texpr, {})
}

function buildLifter(
  dresolver: DeclResolver,
  texpr: AST.TypeExpr,
  boundTypeParams: Record<string, Lifter>,
): Lifter {
  switch (texpr.typeRef.kind) {
    case "primitive": {
      if (texpr.parameters.length === 0) {
        return idLifter
      }
      const elem_lifter = buildLifter(dresolver, texpr.parameters[0], boundTypeParams)
      switch (texpr.typeRef.value) {
        case "Nullable": {
          function lift(json: Json): Json {
            if (json === null) {
              return null
            }
            return elem_lifter.lift(json)
          }
          return { lift }
        }
        case "Vector": {
          function lift(json: Json): Json {
            if (isJsonArray(json)) {
              return json.map(elem_lifter.lift)
            }
            throw Error(`expected arrays got ${typeof json}`)
          }
          return { lift }
        }
        case "StringMap": {
          function lift(json: Json): Json {
            if (isJsonObject(json)) {
              const jv2: JsonObject = {}
              for (const k of Object.keys(json)) {
                jv2[k] = elem_lifter.lift(json[k])
              }
              return jv2
            }
            throw Error(`expected object got ${typeof json}`)
          }
          return { lift }
        }
        default:
          throw Error(`Unexpected primitive ${texpr.typeRef.value}`)
      }
    }
    case "typeParam":
      return boundTypeParams[texpr.typeRef.value];
    case "reference":
      const ast = dresolver(texpr.typeRef.value);
      switch (ast.decl.type_.kind) {
        case "struct_": {
          const newBoundTypeParams = createBoundTypeParams(dresolver, ast.decl.type_.value.typeParams, texpr.parameters, boundTypeParams);
          const fieldDetails: Record<string, Lifter> = {}
          ast.decl.type_.value.fields.forEach(fld => {
            fieldDetails[fld.serializedName] = buildLifter(dresolver, fld.typeExpr, newBoundTypeParams)
          })
          function lift(json: Json): Json {
            if (isJsonObject(json)) {
              const jv2: JsonObject = {}
              for (const k of Object.keys(json)) {
                const elem_lifter = fieldDetails[k]
                if (elem_lifter) {
                  jv2[k] = elem_lifter.lift(json[k])
                } else {
                  // keep field not defined in adl
                  jv2[k] = json[k]
                }
              }
              return jv2
            }
            throw Error(`expected object got ${typeof json}`)
          }
          return { lift }
        }
        case "union_":
          if (isEnum(ast.decl.type_.value)) {
            return idLifter
          } else {
            return createUnionLifter(dresolver, ast.decl.type_.value, texpr, boundTypeParams)
          }
        case "type_": {
          const newBoundTypeParams = createBoundTypeParams(dresolver, ast.decl.type_.value.typeParams, texpr.parameters, boundTypeParams);
          return buildLifter(dresolver, ast.decl.type_.value.typeExpr, newBoundTypeParams)
        }
        case "newtype_": {
          const newBoundTypeParams = createBoundTypeParams(dresolver, ast.decl.type_.value.typeParams, texpr.parameters, boundTypeParams);
          return buildLifter(dresolver, ast.decl.type_.value.typeExpr, newBoundTypeParams)
        }
      }
  }
}

interface Ancestor {
  name: string,
  max_version: number,
}
interface UnionFieldDetails {
  ancestors: Ancestor[],
  max_version: number,
  // disc?: ANN.TypeDiscrimination
  lifter: Lifter
}
interface TypeDiscs {
  max_version: number,
  type_discs: TypeDisc[],
  // record of vector branches where the element has of TypeDiscs
  // vector_discs: Record<string, TypeDiscs>,
}
interface TypeDisc {
  // field: AST.Field,
  ancestors: Ancestor[],
  name: string,
  serializedName: string,
  typeExpr: AST.TypeExpr,
  disc: ANN.TypeDiscrimination,
}


export function createUnionLifter(
  dresolver: DeclResolver,
  union: AST.Union,
  texpr: AST.TypeExpr,
  boundTypeParams: Record<string, Lifter>,
): Lifter {
  const newBoundTypeParams = createBoundTypeParams(dresolver, union.typeParams, texpr.parameters, boundTypeParams);
  const fields: Record<string, UnionFieldDetails> = {}
    const jb = createJsonBinding(dresolver, ANN.texprTypeDiscrimination())
  const type_discs: TypeDisc[] = union.fields.flatMap(fld => {
    const disc = getAnnotation(jb, fld.annotations)
    if (disc) {
      return [{ ...fld, disc, ancestors: [] }]
        }
    return []
  })
  const max_version = type_discs.reduce((p, td) => td.disc.version > p ? td.disc.version : p, -1)
  union.fields.forEach(fld => {
    fields[fld.serializedName] = {
      max_version,
      ancestors: [],
      lifter: buildLifter(dresolver, fld.typeExpr, newBoundTypeParams),
          }
  })
  union.fields.forEach(fld => {
    const anc: Ancestor = {
      name: fld.serializedName,
      max_version: max_version,
        }
    const trans_discs = transitiveTypeDisc(dresolver, fld.typeExpr, [anc], newBoundTypeParams, jb)
    for (const tdtd of trans_discs) {
      if (fields[tdtd.serializedName] !== undefined) {
        throw Error(`union of union branch ambiguity '${tdtd.serializedName}' ${JSON.stringify(texpr.typeRef)}`)
      }
      fields[tdtd.serializedName] = {
        max_version: tdtd.disc.version,
        ancestors: tdtd.ancestors,
        lifter: tdtd.lifter,
      }
      type_discs.push(tdtd)
    }
  })
  function lift(json0: Json): Json {
    let json1 = liftTypeDiscriminations(dresolver, json0, { max_version, type_discs })
    if (isJsonObject(json1)) {
      const keys = Object.keys(json1).filter(k => k != "@v")
      if (keys.length != 1) {
        throw Error("not the shape of a union")
      }
      const ufd = fields[keys[0]]
      if (ufd === undefined) {
        throw Error(`branch not defined '${keys[0]}'\n${JSON.stringify(json0)}\n${JSON.stringify(json1)}\n${JSON.stringify(texpr)}`)
      }
      if (ufd.max_version > -1) {
        json1["@v"] = ufd.max_version
      } 
      if (ufd.ancestors.length > 0) {
        for (const an of ufd.ancestors) {
          const j3: Json = {}
          j3[an.name] = json1
          j3["@v"] = an.max_version
          json1 = j3
        }
      } else {
        json1[keys[0]] = ufd.lifter.lift(json1[keys[0]])
        if (max_version != -1) {
          json1["@v"] = max_version
        }
      }
      return json1
    } else {
      throw Error(`expecting union, value isn't even an object\n${JSON.stringify(json0)}\n${JSON.stringify(json1)}\n${JSON.stringify(texpr)}`)
    }
  }
  return { lift }
}

function transitiveTypeDisc(
  dresolver: DeclResolver,
  ftexpr: AST.TypeExpr,
  ancestors: Ancestor[],
  boundTypeParams: Record<string, Lifter>,
  jb: JsonBinding<ANN.TypeDiscrimination>,
): (TypeDisc & UnionFieldDetails)[] {
  switch (ftexpr.typeRef.kind) {
    case "reference":
      const ast = dresolver(ftexpr.typeRef.value)
      switch (ast.decl.type_.kind) {
        case "newtype_":
          return transitiveTypeDisc(dresolver, ast.decl.type_.value.typeExpr, ancestors, boundTypeParams, jb)
        case "type_":
          return transitiveTypeDisc(dresolver, ast.decl.type_.value.typeExpr, ancestors, boundTypeParams, jb)
        case "struct_":
        return []
        case 'union_':
          const ret: (TypeDisc & UnionFieldDetails)[] = []
          const newBoundTypeParams2 = createBoundTypeParams(dresolver, ast.decl.type_.value.typeParams, ftexpr.parameters, boundTypeParams);
          const max_version = ast.decl.type_.value.fields.reduce((p, fld) => {
            const disc = getAnnotation(jb, fld.annotations)
            if (disc) {
              if (disc.version > p) {
                return disc.version
              }
            }
            return p
          }, -1)
          ast.decl.type_.value.fields.flatMap(fld => {
            const disc = getAnnotation(jb, fld.annotations)
            if (disc) {
              ret.push({
                ...fld,
                max_version,
                lifter: buildLifter(dresolver, fld.typeExpr, newBoundTypeParams2),
                disc,
                ancestors,
      })
    }
            ret.push(...transitiveTypeDisc(dresolver, fld.typeExpr, [{ name: fld.serializedName, max_version }, ...ancestors], newBoundTypeParams2, jb))
          })
  return ret
      }
  }
  return []
}


const idLifter = {
  lift: (json: Json) => json
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
  const mtd = type_discs.type_discs.filter(isDisc)
  if (mtd.length === 0) {
    return json
  }
  if (mtd.length > 1) {
    throw Error(`ambiguous matching type discriminators ${mtd.map(el => el.name)}`)
  }
  const json1: Json = {}
    json1[mtd[0].serializedName] = json
  return json1
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
  // throw Error(`primitive type mismatch. expected ${type_} received ${JSON.stringify(typeRef)}`)
  return false
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

function createBoundTypeParams(
  dresolver: DeclResolver,
  paramNames: string[],
  paramTypes: AST.TypeExpr[],
  boundTypeParams: Record<string, Lifter>,
): Record<string, Lifter> {
  let result: Record<string, Lifter> = {};
  paramNames.forEach((paramName, i) => {
    result[paramName] = buildLifter(dresolver, paramTypes[i], boundTypeParams);
  });
  return result;
}
