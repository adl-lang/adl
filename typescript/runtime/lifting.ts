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
          const newBoundTypeParams = bindTypeParams(ast.decl.type_.value.typeParams, texpr.parameters, (te: AST.TypeExpr) => buildLifter(dresolver, te, boundTypeParams));
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
            return buildUnionLifter(dresolver, ast.decl.type_.value, texpr, boundTypeParams)
          }
        case "type_": {
          const newBoundTypeParams = bindTypeParams(ast.decl.type_.value.typeParams, texpr.parameters, (te: AST.TypeExpr) => buildLifter(dresolver, te, boundTypeParams));
          return buildLifter(dresolver, ast.decl.type_.value.typeExpr, newBoundTypeParams)
        }
        case "newtype_": {
          const newBoundTypeParams = bindTypeParams(ast.decl.type_.value.typeParams, texpr.parameters, (te: AST.TypeExpr) => buildLifter(dresolver, te, boundTypeParams));
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
  lifter: Lifter
}
interface TypeDiscs {
  max_version: number,
  type_discs: TypeDisc[],
}
type TypeDisc = AST.Field & {
  disc: ANN.TypeDiscrimination,
  // max_version: number,
  // lifter: Lifter
}

function buildUnionLifter(
  dresolver: DeclResolver,
  union: AST.Union,
  texpr: AST.TypeExpr,
  boundTypeParams: Record<string, Lifter>,
): Lifter {
  const newBoundTypeParams = bindTypeParams(union.typeParams, texpr.parameters, (te: AST.TypeExpr) => buildLifter(dresolver, te, boundTypeParams));
  const fields: Record<string, UnionFieldDetails> = {}
  const jb = createJsonBinding(dresolver, ANN.texprTypeDiscrimination())
  const type_discs: TypeDisc[] = union.fields.flatMap(fld => {
    const disc = getAnnotation(jb, fld.annotations)
    if (disc) {
      // const lifter = createLifter(dresolver, fld.typeExpr)
      return [{
        ...fld,
        disc,
      }]
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
      if (fields[tdtd.fld.serializedName] !== undefined) {
        throw Error(`union of union branch ambiguity '${tdtd.fld.serializedName}' ${JSON.stringify(texpr.typeRef)}`)
      }
      fields[tdtd.fld.serializedName] = tdtd.ufd
      type_discs.push(tdtd.fld)
    }
  })
  function lift(json0: Json): Json {
    let json1 = json0
    const mtd = type_discs.filter((type_disc: TypeDisc) => {
      const expanded_texpr = expandTypes(dresolver, type_disc.typeExpr, {})
      return matchTypeDiscrimination(dresolver, json0, expanded_texpr)
    })
    if (mtd.length > 1) {
      throw Error(`ambiguous matching type discriminators ${mtd.map(el => el.name)}`)
    }
    if (mtd.length === 1) {
      json1 = {}
      json1[mtd[0].serializedName] = json0
      // json1["@v"] = mtd[0].disc.version
    }
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
      json1[keys[0]] = ufd.lifter.lift(json1[keys[0]])
      if (ufd.ancestors.length > 0) {
        for (const an of ufd.ancestors) {
          const j3: Json = {}
          j3[an.name] = json1
          j3["@v"] = an.max_version
          json1 = j3
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
): { fld: TypeDisc, ufd: UnionFieldDetails }[] {
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
          const ret: { fld: TypeDisc, ufd: UnionFieldDetails }[] = []
          const newBoundTypeParams2 = bindTypeParams(ast.decl.type_.value.typeParams, ftexpr.parameters, (te: AST.TypeExpr) => buildLifter(dresolver, te, boundTypeParams));
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
                fld: {
                  ...fld,
                  disc,
                },
                ufd: {
                  max_version,
                  lifter: buildLifter(dresolver, fld.typeExpr, newBoundTypeParams2),
                  ancestors,
                }
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

function matchTypeDiscrimination(
  dresolver: DeclResolver,
  json: Json,
  texpr: AST.TypeExpr,
): boolean {
  let typeRef = texpr.typeRef
  if (typeRef.kind === "typeParam") {
    return false
  }
  if (typeRef.kind === "primitive" && (typeRef.value === "Json" || typeRef.value === "Void")) {
    throw Error(`cannot use Json or Void as a type discriminator`)
  }
  if (json === null) {
    if (typeRef.kind === "primitive" && typeRef.value === "Nullable") {
      return true
    }
    throw Error(`primitive type mismatch. expected "Nullable" received ${JSON.stringify(typeRef)}`)
  }
  if (Array.isArray(json)) {
    if (typeRef.kind === "primitive" && typeRef.value !== "Vector") {
      return false
    }
    return json.find(j => !matchTypeDiscrimination(dresolver, j, texpr.parameters[0])) === undefined
  }
  if (typeRef.kind === "primitive" && typeRef.value === "Nullable") {
    typeRef = texpr.parameters[0].typeRef
    if (typeRef.kind === "primitive" && typeRef.value === "Vector") {
      throw Error(`lifting of Nullable<Vector<>> not implemented`)
    }
    if (typeRef.kind === "primitive" && typeRef.value === "Nullable") {
      throw Error(`lifting of Nullable<Nullable<>> not implemented`)
    }
  }
  switch (typeof json) {
    case "string":
      if (typeRef.kind === "primitive" && typeRef.value === "String") {
        return true
      }
      return false
    case "object":
      return matchObject(dresolver, json, texpr)
    case "number":
      if (typeRef.kind === "primitive" && adlNumbers.includes(typeRef.value)) {
        return true
      }
      return false
    // throw Error(`primitive type mismatch. expected ${adlNumbers} received ${JSON.stringify(typeRef)}`)
    case "boolean":
      if (typeRef.kind === "primitive" && typeRef.value === "Bool") {
        return true
      }
      return false
    // case "bigint":
    default:
      break
  }
  return false
}

function matchObject(
  dresolver: DeclResolver,
  json: JsonObject,
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

/**
 * Recursively xpand type aliases and/or newtypes by substitution
 */
function expandTypes(
  resolver: DeclResolver,
  texpr: AST.TypeExpr,
  boundTypeParams: Record<string, AST.TypeExpr>,
): AST.TypeExpr {
  // console.log("expandTypes", texpr, boundTypeParams)
  switch (texpr.typeRef.kind) {
    case "primitive":
      if (texpr.parameters.length == 0) {
        return texpr;
      }
      const te0 = expandTypes(resolver, texpr.parameters[0], boundTypeParams)
      return AST.makeTypeExpr({ typeRef: texpr.typeRef, parameters: [te0] })
    case "reference":
      const ast = resolver(texpr.typeRef.value);
      switch (ast.decl.type_.kind) {
        case "struct_":
          if (texpr.parameters.length == 0) {
            return texpr;
          }
          return texpr
          // const nbp = bindTypeParams(ast.decl.type_.value.typeParams, texpr.parameters, (te: AST.TypeExpr) => te);
          // const fields = ast.decl.type_.value.fields.map(fld => {
          //   return {
          //     ...fld,
          //     typeExpr: expandTypes(resolver, fld.typeExpr, nbp)
          //   }
          // })
          // return {

          // }
        case "union_":
          if (texpr.parameters.length == 0) {
            return texpr;
          }
          return texpr
        case "newtype_":
        case "type_": {
          // console.log("!!! ", texpr, boundTypeParams)
          const nbp = bindTypeParams(ast.decl.type_.value.typeParams, texpr.parameters, (te: AST.TypeExpr) => te);
          return expandTypes(resolver, ast.decl.type_.value.typeExpr, nbp);
        }
      }
    case "typeParam":
      // console.log("*** ", texpr, boundTypeParams)
      return boundTypeParams[texpr.typeRef.value];
  }
}

function assertNever(x: never): never {
  throw new Error('cannot reach here!');
}

function bindTypeParams<T>(
  // dresolver: DeclResolver,
  paramNames: string[],
  paramTypes: AST.TypeExpr[],
  // boundTypeParams: Record<string, T>,
  fn: (te: AST.TypeExpr) => T,
): Record<string, T> {
  let result: Record<string, T> = {};
  paramNames.forEach((paramName, i) => {
    result[paramName] = fn(paramTypes[i]);
  });
  return result;
}
