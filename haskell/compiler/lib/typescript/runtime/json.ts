import {DeclResolver,ATypeExpr} from './adl';
import * as AST from './sys/adlast';

/**
 * A JsonBinding is a de/serialiser for a give ADL type
 */
interface JsonBinding<T> {
  toJson (t : T): any;
  fromJson(json : any) : T;
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

/**
 * Construct a JsonBinding for an arbitrary type expression
 */
export function createJsonBinding<T>(dresolver : DeclResolver, texpr : ATypeExpr<T>) : JsonBinding<T> {
  return buildJsonBinding(dresolver, texpr.value, {});
};

interface BoundTypeParams {
  [key: string]: JsonBinding<any>;
}

export function buildJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding<any> {
  if (texpr.typeRef.kind == "primitive") {
    return primitiveJsonBinding(dresolver, texpr.typeRef.value, texpr.parameters, boundTypeParams);
  } else if (texpr.typeRef.kind == "reference") {
    const ast = dresolver(texpr.typeRef.value);
    if (ast.decl.type_.kind === "struct_") {
      return structJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    } else if (ast.decl.type_.kind === "union_") {
      return unionJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    } else if (ast.decl.type_.kind === "newtype_") {
      return newtypeJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    } else if (ast.decl.type_.kind === "type_") {
      return typedefJsonBinding(dresolver, ast.decl.type_.value, texpr.parameters, boundTypeParams);
    }
  } else if (texpr.typeRef.kind == "typeParam") {
    return boundTypeParams[texpr.typeRef.value];
  }
};

function primitiveJsonBinding(dresolver : DeclResolver, ptype : string, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding<any> {
  if      (ptype == "String")     { return identityJsonBinding("a string", (v) => typeof(v) === 'string'); }
  else if (ptype == "Int8")       { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Void")       { return identityJsonBinding("a null", (v) => v === null); }
  else if (ptype == "Bool")       { return identityJsonBinding("a bool", (v) => typeof(v) == 'boolean'); }
  else if (ptype == "Int8")       { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Int16")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Int32")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Int64")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Word8")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Word16")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Word32")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Word64")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Float")      { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "Double")     { return identityJsonBinding("a number", (v) => typeof(v) === 'number'); }
  else if (ptype == "ByteVector") { return identityJsonBinding("a base64 string", (v) => typeof(v) === 'string'); }
  else if (ptype == "Vector")     { return vectorJsonBinding(dresolver, params[0], boundTypeParams); }
  else if (ptype == "StringMap")  { return stringMapJsonBinding(dresolver, params[0], boundTypeParams); }
  else throw new Error("Unimplemented json binding for primitive" + ptype);
};

function identityJsonBinding(expected : string, predicate : (json : any) => boolean) : JsonBinding<any>{

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

function vectorJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding<any> {
  const elementBinding = once(() => buildJsonBinding(dresolver, texpr, boundTypeParams));

  function toJson(v : any) : any {
    return v.map(elementBinding().toJson);
  }

  function fromJson(json : any) : any {
      if (!(json instanceof Array)) {
        throw jsonParseException('expected an array');
      }
      let result = [];
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

function stringMapJsonBinding(dresolver : DeclResolver, texpr : AST.TypeExpr, boundTypeParams : BoundTypeParams) : JsonBinding<any> {
  const elementBinding = once(() => buildJsonBinding(dresolver, texpr, boundTypeParams));
  throw new Error("Unimplemented json binding for stringMap");
}

function structJsonBinding(dresolver : DeclResolver, struct : AST.Struct, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding<any> {
  const newBoundTypeParams = createBoundTypeParams(dresolver, struct.typeParams, params, boundTypeParams);
  const fieldDetails = [];
  struct.fields.forEach( (field) => {
    let buildDefault = once( () => {
      if (field.default.kind == "just")  {
        return buildDefaultValue(dresolver, field.typeExpr, field.default.value);
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
    const v = {};
    fieldDetails.forEach( (fd) => {
      if (json[fd.field.serializedName] == undefined) {
        const defaultv = fd.buildDefault();
        if (defaultv == null)  {
          throw jsonParseException("missing struct field " + fd.field.serializedName );
        } else {
          v[fd.field.name] = defaultv;
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

function unionJsonBinding(dresolver : DeclResolver, union : AST.Union, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding<any> {
  return undefined;
}

function newtypeJsonBinding(dresolver : DeclResolver, newtype : AST.NewType, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding<any> {
  return undefined;
}

function typedefJsonBinding(dresolver : DeclResolver, typedef : AST.TypeDef, params : AST.TypeExpr[], boundTypeParams : BoundTypeParams ) : JsonBinding<any> {
  return undefined;
}

function createBoundTypeParams(dresolver : DeclResolver, paramNames : string[], paramTypes : AST.TypeExpr[], boundTypeParams : BoundTypeParams) : BoundTypeParams
{
  let result : BoundTypeParams = {};
  paramNames.forEach( (paramName,i) => {
    result[paramName] = buildJsonBinding(dresolver,paramTypes[i], boundTypeParams);
  });
  return result;
}

function buildDefaultValue(dresolver : DeclResolver, typeExpr : AST.TypeExpr, defaultv : AST.Literal) : any {
  throw new Error( "buildDefaultValue: not implemented" );
}

function once<T>(run : () => T) : () => T {
  let result = null;
  return () => {
    if(result == null) {
      result = run();
    }
    return result;
  };
}
