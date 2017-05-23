import {TypeDesc, isJsonParseException, jsonParseException, exceptionOn} from './runtime';

interface Ref {
  ref: string;
}

interface Primitive {
  primitive: string;
}
interface Complex {
  args: FieldType[];
  app: FieldType;
}

type FieldType = Primitive|Complex|Ref;

interface Field {
  name: string;
  type: FieldType;
  defaultv: {}|null;
}

interface JsonUnion {
  kind: string;
}

interface Just {
  just: {};
}

interface Nothing {
  nothing: null;
}

function isJust(json: {}): json is Just {
  return (<Just> json).just !== undefined;
}

function isNothing(json: {}): json is Nothing {
  return (<Nothing> json).nothing === null;
}

function isFieldPrimitive(fieldType: FieldType): fieldType is Primitive {
  return (<Primitive> fieldType).primitive !== undefined;
}

function isFieldComplex(fieldType: FieldType): fieldType is Complex {
  return (<Complex> fieldType).app !== undefined && (<Complex> fieldType).args !== undefined;
}

function isFieldRef(fieldType: FieldType): fieldType is Ref {
  return (<Ref> fieldType).ref !== undefined;
}

function isArray(json: {}): json is Array<{}> {
  return (json instanceof Array);
}

function isJsonUnion(json: {}): json is JsonUnion {
 return (<JsonUnion> json).kind !== undefined;
}

function isString(json: {}): json is String {
  return (json instanceof String) || (typeof(json) === 'string');
}

function isNumber(json: {}): json is Number {
  return (json instanceof Number) || (typeof(json) === 'number');
}

interface Union {
  kind: string;
  value: {};
}

function isUnion(value: {}): value is Union {
  return (<Union> value).kind !== undefined && (<Union> value).value !== undefined;
}

interface TypedefDecl {
  name: string;
  module: string;
  kind: 'typedef';
  type: FieldType;
  annotations: {}[];
}

interface StructDecl {
  name: string;
  module: string;
  kind: 'struct';
  fields: Field[];
  annotations: {}[];
}

interface UnionDecl {
  name: string;
  module: string;
  kind: 'union';
  typevars: {}[];
  fields: Field[];
}

type Schema = TypedefDecl|StructDecl|UnionDecl;

function isSchemaTypedef(schema: Schema): schema is TypedefDecl {
  return (<TypedefDecl> schema).kind === 'typedef';
}

function isSchemaStruct(schema: Schema): schema is StructDecl {
  return (<StructDecl> schema).kind === 'struct';
}

function isSchemaUnion(schema: Schema): schema is UnionDecl {
  return (<UnionDecl> schema).kind === 'union';
}

export class JsonBindings {
  schemaMap: Map<string, Schema>;

  constructor(schemaList: { [key: string]: Schema}[]) {
    this.schemaMap = new Map<string, Schema>();
    schemaList.forEach((schemas: { [key: string]: Schema}) => {
      Object.keys(schemas).forEach((key: string) => {
        this.schemaMap[key] = schemas[key];
      });
    });
  }

  serialise<T>(value: T, type: TypeDesc<T>): {}|null {
    return this.serializeField(value, type, type.ref);
  }

  serializeField(value: {}, fieldType: FieldType, fieldName: string, defaultv?: {}|null): {}|null {
      if (isFieldPrimitive(fieldType)) {
        return value;
      } else if (isFieldRef(fieldType)) {
        return this.serializeRefType(value, fieldType.ref, fieldName, defaultv);
      } else if (isFieldComplex(fieldType)) {
        return this.serializeComplexType(value, fieldType.app, fieldType.args, fieldName);
      } else {
        exceptionOn(() => true, `encountered unhandled field type ${fieldType} for field`);
        return null;
      }
  }

  serializeRefType(value: {}, ref: string, fieldName: string, defaultv?: {}|null): {}|null {
    let serialized: {} = {};
    let schema: Schema = this.schemaMap[ref];
    exceptionOn(() => !schema, `couldn't find schema for ${ref} while validating ${fieldName}`);

    if (isSchemaTypedef(schema)) {
      return this.serializeField(value, schema.type, fieldName);
    } else if (isSchemaStruct(schema)) {
      for (let structField of schema.fields) {
          let structFieldType = structField.type;
          let structFieldName = structField.name;
          let defaultValue = structField.defaultv;
          let fieldJson = this.serializeField(value[structFieldName], structFieldType, structFieldName, defaultValue);
          serialized[structFieldName] = fieldJson;
      }
    } else if (isSchemaUnion(schema)) {
      exceptionOn(() => !isUnion(value), 'encountered non-union object for union schema ' + ref + ' for field name ' + fieldName);
      let union = value as Union;
      let kind: string = union.kind;
      let maybeUnionField: Field|undefined = schema.fields.find((field: Field) => field.name === kind);
      exceptionOn(() => maybeUnionField === undefined, `found invalid union kind ${kind} for union ${ref}`);
      let unionField: Field = maybeUnionField!;
      serialized = {
        kind,
        [kind]: this.serializeField(union.value, unionField.type, unionField.name)
      };
    } else {
      throw jsonParseException('encountered unhandled schema kind for type ' + ref);
    }
    return serialized;
  }

  serializeComplexType(value: {}, app: FieldType, args: FieldType[], fieldName: string): {}|null {
    if (isFieldPrimitive(app)) {
      if (app.primitive === 'Vector') {
        if (isArray(value)) {
          return this.serializeArray(value, args[0], fieldName);
        }
      }
    } else if (isFieldRef(app)) {
      if (app.ref === 'sys.types.Nullable') {
        if (value === null) {
          return {nothing: null};
        } else {
          return {just: this.serializeField(value, args[0], fieldName)};
        }
      }
    }
    exceptionOn(() => true, `unhandled complex type for field ${fieldName}`);
    return null;
  }

  serializeArray(array: {}[], fieldType: FieldType, fieldName: string): Array<{}|null> {
    let validatedArray = new Array<{}|null>();
    array.forEach((arrayElement: {}) => {
      validatedArray.push(this.serializeField(arrayElement, fieldType, fieldName));
    });
    return validatedArray;
  }

  parse<T>(type: TypeDesc<T>, json: {}): T {
    let properlyParsed = this.validate(json, type, type.ref);
    return (properlyParsed as T);
  }

  validate(json: {}, fieldType: FieldType, fieldName: string, defaultv?: {}|null): {}|null {
    try {
      if (isFieldPrimitive(fieldType)) {
        return this.validatePrimitiveType(json, fieldType.primitive, defaultv);
      } else if (isFieldRef(fieldType)) {
        return this.validateRefType(json, fieldType.ref, fieldName, defaultv);
      } else if (isFieldComplex(fieldType)) {
        return this.validateComplexType(json, fieldType.app, fieldType.args, fieldName);
      } else {
        exceptionOn(() => true, `encountered unhandled field type ${fieldType} for field`);
      }
    } catch (e) {
      if (isJsonParseException(e)) {
        e.pushField('{' + fieldName + '}');
      }
      throw e;
    }
    return null;
  }

  validatePrimitiveType(json: {}, primitiveName: string, defaultv?: {}|null): {}|null {
    let jsonOrDefault = json || defaultv;
    exceptionOn(() => jsonOrDefault === undefined, 'attempting to parse undefined, mandatory primitive field');

    if (primitiveName === 'Double') {
      exceptionOn(() => !isNumber(jsonOrDefault), `primitive field ${JSON.stringify(jsonOrDefault)} is not of type number`);
    } else if (primitiveName === 'String') {
      exceptionOn(() => !isString(jsonOrDefault), `primitive field ${JSON.stringify(jsonOrDefault)} is not of type string`);
    } else {
      throw jsonParseException('encountered unhandled primitive type ' + primitiveName + ' for field ' + name);
    }
    return jsonOrDefault;
  }

  validateComplexType(json: {}, app: FieldType, args: FieldType[], fieldName: string): {}|null {
    if (isFieldPrimitive(app)) {
      if (app.primitive === 'Vector') {
        if (isArray(json)) {
          return this.validateArray(json, args[0], fieldName);
        }
      }
    } else if (isFieldRef(app)) {
      if (app.ref === 'sys.types.Nullable') {
        if (isJust(json)) {
          return this.validate(json.just, args[0], fieldName);
        } else if (isNothing(json)) {
          return null;
        }
      }
    }
    exceptionOn(() => true, `unhandled complex type for field ${fieldName}`);
    return null;
  }

  validateRefType(json: {}, ref: string, fieldName: string, defaultv?: {} | null): {}|null {
    let jsonOrDefault = json || defaultv;
    exceptionOn(() => jsonOrDefault === undefined, 'attempting to parse undefined, mandatory ref field');
    let properlyParsed: {} = {};
    let schema: Schema = this.schemaMap[ref];
    exceptionOn(() => !schema, `couldn't find schema for ${ref} while validating ${fieldName}`);

    if (isSchemaTypedef(schema)) {
      return this.validate(json, schema.type, fieldName);
    } else if (isSchemaStruct(schema)) {
      for (let structField of schema.fields) {
          let structFieldType = structField.type;
          let structFieldName = structField.name;
          let defaultValue = structField.defaultv;
          let fieldJson = this.validate(jsonOrDefault[structFieldName], structFieldType, structFieldName, defaultValue);
          properlyParsed[structFieldName] = fieldJson;
      }
    } else if (isSchemaUnion(schema)) {
      exceptionOn(() => !isJsonUnion(json), 'encountered non-union object for union schema ' + ref + ' for field name ' + fieldName);
      let unionSchema = json as JsonUnion;
      let kind: string = unionSchema.kind;
      let maybeUnionField: Field|undefined = schema.fields.find((field: Field) => field.name === kind);
      exceptionOn(() => maybeUnionField === undefined, `found invalid union kind ${kind} for union ${ref}`);
      let unionField: Field = maybeUnionField!;
      let unionFieldType = unionField.type;
      let unionFieldName = unionField.name;
      let defaultValue = unionField.defaultv;
      exceptionOn(() => json[unionFieldName] === undefined, `couldn't find union value in union for field ${fieldName}`);
      return {
        kind,
        value: this.validate(json[unionFieldName], unionFieldType, unionFieldName, defaultValue)
      };
    } else {
      throw jsonParseException('encountered unhandled schema kind for type ' + ref);
    }
    return properlyParsed;
  }

  validateArray(jsonArray: {}[], fieldType: FieldType, fieldName: string): Array<{}|null> {
    let validatedArray = new Array<{}|null>();
    jsonArray.forEach((json: {}) => {
      validatedArray.push(this.validate(json, fieldType, fieldName));
    });
    return validatedArray;
  }
}
