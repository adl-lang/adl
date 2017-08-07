import {DeclResolver,ATypeExpr} from './adl';
import * as AST from './sys/adlast';
import * as b64 from 'base64-js';
import {isVoid, isEnum} from './utils';

type Json = {}|null;

/**
 * A JsonBinding is a de/serialiser for a give ADL type
 */
export interface JsonBinding<T> {
  typeExpr : AST.TypeExpr;
  toJson (t : T): Json;
  fromJson(json : Json) : T;
};

/**
 * Construct a JsonBinding for an arbitrary type expression
 */
export function createJsonBinding<T>(dresolver : DeclResolver, texpr : ATypeExpr<T>) : JsonBinding<T> {
  const jb0 = buildJsonBinding(dresolver, texpr.value, {});
  return {typeExpr : texpr.value, toJson:jb0.toJson, fromJson:jb0.fromJson};
};

/**
 * Interface for json parsing exceptions.
 * Any implementation should properly show the parse error tree.
 *
 *  @interface JsonParseException
 */
export interface JsonParseException {
  kind: 'JsonParseException';
  getMessage(): string;
  pushField(fieldName: string): void;
  pushIndex(index: number): void;
  toString(): string;
}

/** Convenience function for generating a json parse exception.
 *  @param {string} message - Exception message.
 */
export function jsonParseException(message: string): JsonParseException {
  const context: string[] = [];
  let createContextString: () => string = () => {
    const rcontext: string[] = context.slice(0);
    rcontext.push('$');
    rcontext.reverse();
    return rcontext.join('.');
  };
  return {
    kind: 'JsonParseException',
    getMessage(): string {
      return message + ' at ' + createContextString();
    },
    pushField(fieldName: string): void {
      context.push(fieldName);
    },
    pushIndex(index: number): void {
      context.push('[' + index + ']');
    },
    toString(): string {
      return this.getMessage();
    }
  };
}

/**
 * Check if a javascript error is of the json parse exception type.
 * @param exception The exception to check.
 */
export function isJsonParseException(exception: {}): exception is JsonParseException {
  return (<JsonParseException> exception).kind === 'JsonParseException';
}

interface JsonBinding0<T> {
  toJson (t : T): any;
  fromJson(json : any) : T;
};

interface BoundTypeParams {
  [key: string]: JsonBinding0<any>;
}

function buildJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding0<any> {
  if (texpr.typeRef.kind === "primitive") {
    return primitiveJsonBinding(dresolver, texpr.typeRef.value, texpr.parameters, boundTypeParams);
  } else if (texpr.typeRef.kind === "reference") {
    const ast = dresolver(texpr.typeRef.value);
    if (ast.decl.type_.kind === "struct_") {
      return structJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    } else if (ast.decl.type_.kind === "union_") {
      const union = ast.decl.type_.value;
      if (isEnum(union)) {
        return enumJsonBinding(dresolver, union, texpr.parameters, boundTypeParams);
      } else {
        return unionJsonBinding(dresolver, union, texpr.parameters, boundTypeParams);
      }
    } else if (ast.decl.type_.kind === "newtype_") {
      return newtypeJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    } else if (ast.decl.type_.kind === "type_") {
      return typedefJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    }
  } else if (texpr.typeRef.kind === "typeParam") {
    return boundTypeParams[texpr.typeRef.value];
  }
  throw new Error("buildJsonBinding : unimplemented ADL type");
};

function primitiveJsonBinding(dresolver : DeclResolver, ptype : string, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding0<any> {
  if      (ptype === "String")     { return identityJsonBinding("a string", (v) => typeof(v) === 'string'); }
  else if (ptype === "Int8")       { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Void")       { return identityJsonBinding("a null", (v) => v === null); }
  else if (ptype === "Bool")       { return identityJsonBinding("a bool", (v) => typeof(v) === 'boolean'); }
  else if (ptype === "Int8")       { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Int16")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Int32")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Int64")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Word8")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Word16")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Word32")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Word64")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Float")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Double")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype === "Json")       { return identityJsonBinding("a json value", (_v) => true); }
  else if (ptype === "Bytes")      { return bytesJsonBinding(); }
  else if (ptype === "Vector")     { return vectorJsonBinding(dresolver, params[0], boundTypeParams); }
  else if (ptype === "StringMap")  { return stringMapJsonBinding(dresolver, params[0], boundTypeParams); }
  else if (ptype === "Nullable")   { return nullableJsonBinding(dresolver, params[0], boundTypeParams); }
  else throw new Error("Unimplemented json binding for primitive " + ptype);
};

function identityJsonBinding(expected : string, predicate : (json : any) => boolean) : JsonBinding0<any>{

  function toJson(v : any) : any {
    return v;
  }

  function fromJson(json : any) : any {
    if( !predicate(json)) {
      throw jsonParseException("expected " + expected);
    }
    return json;
  }

  return {toJson, fromJson};
}

function bytesJsonBinding() : JsonBinding0<any> {
  function toJson(v : any) : any {
    return b64.fromByteArray(v);
  }

  function fromJson(json : any) : any {
    if (typeof(json) != 'string') {
      throw jsonParseException('expected a string');
    }
    return b64.toByteArray(json);
  }

  return {toJson, fromJson};
}

function vectorJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding0<any> {
  const elementBinding = once(() => buildJsonBinding(dresolver, texpr, boundTypeParams));

  function toJson(v : any) : any {
    return v.map(elementBinding().toJson);
  }

  function fromJson(json : any) : any {
      if (!(json instanceof Array)) {
        throw jsonParseException('expected an array');
      }
      let result : any[] = [];
      json.forEach( (eljson,i) => {
        try {
          result.push(elementBinding().fromJson(eljson));
        } catch(e) {
          if (isJsonParseException(e)) {
            e.pushIndex(i);
          }
          throw e;
        }
      });
    return result;
  }

  return {toJson, fromJson};
}

function stringMapJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding0<any> {
  const elementBinding = once(() => buildJsonBinding(dresolver, texpr, boundTypeParams));

  function toJson(v : any) : any {
    const result = {};
    for (let k in v) {
      result[k] = elementBinding().toJson(v[k]);
    }
    return result;
  }

  function fromJson(json : any) : any {
    if (!(json instanceof Object)) {
      throw jsonParseException('expected an object');
    }
    let result = {};
    for (let k in json) {
      try {
        result[k] = elementBinding().fromJson(json[k]);
      } catch(e) {
        if (isJsonParseException(e)) {
          e.pushField(k);
        }
      }
    }
    return result;
  }

  return {toJson, fromJson};
}

function nullableJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding0<any> {
  const elementBinding = once(() => buildJsonBinding(dresolver, texpr, boundTypeParams));

  function toJson(v : any) : any {
    if (v === null) {
      return null;
    }
    return elementBinding().toJson(v);
  }

  function fromJson(json : any) : any {
    if (json === null) {
      return null;
    }
    return elementBinding().fromJson(json);
  }

  return {toJson,fromJson};
}

interface StructFieldDetails {
  field : AST.Field,
  jsonBinding : () => JsonBinding0<any>,
  buildDefault : () => { value : any } | null
};

function structJsonBinding(dresolver : DeclResolver, struct : AST.Struct, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding0<any> {
  const newBoundTypeParams = createBoundTypeParams(dresolver, struct.typeParams, params, boundTypeParams);
  const fieldDetails : StructFieldDetails[] = [];
  struct.fields.forEach( (field) => {
    let buildDefault = once( () => {
      if (field.default.kind === "just")  {
        const json = jsonFromLiteral(field.default.value);
        return { 'value' : buildJsonBinding(dresolver, field.typeExpr, newBoundTypeParams).fromJson(json)};
      } else {
        return null;
      }
    });

    fieldDetails.push( {
      field : field,
      jsonBinding : once(() => buildJsonBinding(dresolver, field.typeExpr, newBoundTypeParams)),
      buildDefault : buildDefault,
    });
  });

  function toJson(v) {
    const json = {};
    fieldDetails.forEach( (fd) => {
      json[fd.field.serializedName] = fd.jsonBinding().toJson(v[fd.field.name]);
    });
    return json;
  }

  function fromJson(json) {
    if (!(json instanceof Object)) {
      throw jsonParseException("expected an object");
    }

    const v = {};
    fieldDetails.forEach( (fd) => {
      if (json[fd.field.serializedName] === undefined) {
        const defaultv = fd.buildDefault();
        if (defaultv === null)  {
          throw jsonParseException("missing struct field " + fd.field.serializedName );
        } else {
          v[fd.field.name] = defaultv.value;
        }
      } else {
        try {
          v[fd.field.name] = fd.jsonBinding().fromJson(json[fd.field.serializedName]);
        } catch(e) {
          if (isJsonParseException(e)) {
            e.pushField(fd.field.serializedName);
          }
          throw e;
        }
      }
    });
    return v;
  }

  return {toJson, fromJson};
}

function enumJsonBinding(_dresolver : DeclResolver, union : AST.Union, _params : AST.TypeExpr[], _boundTypeParams : BoundTypeParams ) : JsonBinding0<any> {
  const fieldSerializedNames : string[] = [];
  const fieldNumbers = {};
  union.fields.forEach( (field,i) => {
    fieldSerializedNames.push(field.serializedName);
    fieldNumbers[field.serializedName] = i;
  });

  function toJson(v :any) : any {
    return fieldSerializedNames[v];
  }

  function fromJson(json : any) : any {
    if (typeof(json) !== 'string') {
      throw jsonParseException("expected a string for enum");
    }
    const result = fieldNumbers[json];
    if (result === undefined) {
      throw jsonParseException("invalid string for enum: " + json);
    }
    return result;
  }

  return {toJson, fromJson};
}

function unionJsonBinding(dresolver : DeclResolver, union : AST.Union, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding0<any> {


  const newBoundTypeParams = createBoundTypeParams(dresolver, union.typeParams, params, boundTypeParams);
  const detailsByName = {};
  const detailsBySerializedName = {};
  union.fields.forEach( (field) => {
    const details = {
      field : field,
      isVoid : isVoid(field.typeExpr),
      jsonBinding : buildJsonBinding(dresolver, field.typeExpr, newBoundTypeParams)
    };
    detailsByName[field.name] = details;
    detailsBySerializedName[field.serializedName] = details;
  });

  function toJson(v : any) : any {
    const details = detailsByName[v.kind];
    if (details.isVoid) {
      return details.field.serializedName;
    } else {
      const result = {};
      result[details.field.serializedName] = details.jsonBinding.toJson(v.value);
      return result;
    }
  }

  function lookupDetails(serializedName : string) {
    let details = detailsBySerializedName[serializedName];
    if (details === undefined) {
      throw jsonParseException("invalid union field " + serializedName);
    }
    return details;
  }

  function fromJson(json : any) : any {
    if (typeof(json) === "string") {
      let details = lookupDetails(json);
      if (!details.isVoid) {
        throw jsonParseException("union field " + json + "needs an associated value");
      }
      return { kind : details.field.name };
    } else if (json instanceof Object) {
      for (let k in json) {
        let details = lookupDetails(k);
        try {
          return {
            kind : details.field.name,
            value : details.jsonBinding.fromJson(json[k])
          }
        } catch(e) {
          if (isJsonParseException(e)) {
            e.pushField(k);
          }
          throw e;
        }
      }
      throw jsonParseException("union without a property");
    } else {
      throw jsonParseException("expected an object or string");
    }
  }

  return {toJson, fromJson};
}

function newtypeJsonBinding(dresolver : DeclResolver, newtype : AST.NewType, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding0<any> {
  const newBoundTypeParams = createBoundTypeParams(dresolver, newtype.typeParams, params, boundTypeParams);
  return buildJsonBinding(dresolver, newtype.typeExpr, newBoundTypeParams);
}

function typedefJsonBinding(dresolver : DeclResolver, typedef : AST.TypeDef, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding0<any> {
  const newBoundTypeParams = createBoundTypeParams(dresolver, typedef.typeParams, params, boundTypeParams);
  return buildJsonBinding(dresolver, typedef.typeExpr, newBoundTypeParams);
}

function createBoundTypeParams(dresolver : DeclResolver, paramNames : string[], paramTypes : AST.TypeExpr[], boundTypeParams : BoundTypeParams) : BoundTypeParams
{
  let result : BoundTypeParams = {};
  paramNames.forEach( (paramName,i) => {
    result[paramName] = buildJsonBinding(dresolver,paramTypes[i], boundTypeParams);
  });
  return result;
}

/**
 *  Convert a ADLAST literal to a json value.
 *
 *  This will be unnecessary when this is addressed:
 *      https://github.com/timbod7/adl/issues/42
 */
function jsonFromLiteral(literal : AST.Literal) : any {
  if (literal.kind === "null") {
    return null;
  } else if (literal.kind === "string") {
    return literal.value;
  } else if (literal.kind === "integer") {
    return literal.value;
  } else if (literal.kind === "double") {
    return literal.value;
  } else if (literal.kind === "boolean") {
    return literal.value;
  } else if (literal.kind === "array") {
    return literal.value.map(jsonFromLiteral);
  } else if (literal.kind === "object") {
    const result = {};
    literal.value.forEach( (pair) => {
      result[pair.v1] = jsonFromLiteral(pair.v2);
    });
    return result;
  }
}

/**
 * Helper function that takes a thunk, and evaluates it only on the first call. Subsequent
 * calls return the previous value
 */
function once<T>(run : () => T) : () => T {
  let result : T | null = null;
  return () => {
    if(result === null) {
      result = run();
    }
    return result;
  };
}
