import * as AST from './sys/adlast.ts';
import {Json} from "./json.ts";

export * from './utils.ts';
export * from './json.ts';
export * from './dynamic.ts';
export * from './resolver.ts';

export type ScopedName = AST.ScopedName;
export type ScopedDecl = AST.ScopedDecl;

/** An ADL type reference for a value of type _T */
export type ATypeRef<_T> = {value: AST.TypeRef};

/** An ADL type expression for a value of type _T */
export type ATypeExpr<_T> = {value : AST.TypeExpr};

/* Type expressions for primitive types */

function texprPrimitive(ptype: string): ATypeExpr<unknown> {
  return {
    value: {
      typeRef: { kind: "primitive", value: ptype },
      parameters: []
    }
  };
};

function texprPrimitive1(ptype: string, etype: ATypeExpr<unknown>): ATypeExpr<unknown> {
  return {
    value: {
      typeRef: { kind: "primitive", value: ptype },
      parameters: [etype.value]
    }
  };
};

/** A type expression for the unit type */
export function texprVoid() : ATypeExpr<null> {return texprPrimitive("Void");}
/** A type expression for the boolean type */
export function texprBool() : ATypeExpr<boolean> {return texprPrimitive("Bool");}
/** A type expression for the Int8 type */
export function texprInt8() : ATypeExpr<number> {return texprPrimitive("Int8");}
/** A type expression for the Int16 type */
export function texprInt16() : ATypeExpr<number> {return texprPrimitive("Int16");}
/** A type expression for the Int32 type */
export function texprInt32() : ATypeExpr<number> {return texprPrimitive("Int32");}
/** A type expression for the Int64 type */
export function texprInt64() : ATypeExpr<number> {return texprPrimitive("Int64");}
/** A type expression for the Word8 type */
export function texprWord8() : ATypeExpr<number> {return texprPrimitive("Word8");}
/** A type expression for the Word16 type */
export function texprWord16() : ATypeExpr<number> {return texprPrimitive("Word16");}
/** A type expression for the Word32 type */
export function texprWord32() : ATypeExpr<number> {return texprPrimitive("Word32");}
/** A type expression for the Word64 type */
export function texprWord64() : ATypeExpr<number> {return texprPrimitive("Word64");}
/** A type expression for the single precision floating point type */
export function texprFloat() : ATypeExpr<number> {return texprPrimitive("Float");}
/** A type expression for the double precision floating point type */
export function texprDouble() : ATypeExpr<number> {return texprPrimitive("Double");}
/** A type expression for the JSON type */
export function texprJson() : ATypeExpr<Json> {return texprPrimitive("Json");}
/** A type expression for the ADL ByteVector type */
export function texprByteVector() : ATypeExpr<Uint8Array> {return texprPrimitive("ByteVector");}
/** A type expression for the string type */
export function texprString() : ATypeExpr<string> {return texprPrimitive("String");}

/** A type expression for a vector with elements of type T */
export function texprVector<T>(etype: ATypeExpr<T>) : ATypeExpr<T[]> {
  return texprPrimitive1("Vector", etype);
}

/** A type expression for a StringMap with elements of type T */
export function texprStringMap<T>(etype: ATypeExpr<T>) : ATypeExpr<{[key:string]:T}> {
  return texprPrimitive1("StringMap", etype);
}

/** A type expression for a value that may be type T or null */
export function texprNullable<T>(etype: ATypeExpr<T>) : ATypeExpr<T|null> {
  return texprPrimitive1("Nullable", etype);
}

export function makeATypeExpr<T>(value: AST.TypeExpr): ATypeExpr<T> {
  return {value}
}
