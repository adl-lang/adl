import * as AST from './sys/adlast';

export function isEnum(union : AST.Union) : boolean {
  for (let field of union.fields) {
    if (!isVoid(field.typeExpr)) {
      return false;
    }
  }
  return true;
}

export function isVoid(texpr : AST.TypeExpr) : boolean {
  if (texpr.typeRef.kind === "primitive") {
    return texpr.typeRef.value === "Void";
  }
  return false;
}

export function typeExprsEqual(texpr1 : AST.TypeExpr, texpr2 : AST.TypeExpr) : boolean {
  if (!typeRefsEqual(texpr1.typeRef, texpr2.typeRef)) {
    return false;
  }
  if (texpr1.parameters.length != texpr2.parameters.length) {
    return false;
  }
  for (let i = 0; i < texpr1.parameters.length; i++) {
    if(!typeExprsEqual(texpr1.parameters[i], texpr2.parameters[i])) {
      return false;
    }
  }
  return true;
}

export function typeRefsEqual(tref1 : AST.TypeRef, tref2 : AST.TypeRef) : boolean {
  if (tref1.kind === "primitive" && tref2.kind === "primitive") {
    return tref1.value === tref2.value;
  } else if (tref1.kind === "typeParam" && tref2.kind === "typeParam") {
    return tref1.value === tref2.value;
  } else if (tref1.kind === "reference" && tref2.kind === "reference") {
    return scopedNamesEqual(tref1.value, tref2.value);
  }
  return false;
}

export function scopedNamesEqual(sn1: AST.ScopedName, sn2: AST.ScopedName): boolean {
  return sn1.moduleName === sn2.moduleName && sn1.name === sn2.name;
}

function typeExprToStringImpl(te: AST.TypeExpr, withScopedNames: boolean) : string {
  let result = "";
  if (te.typeRef.kind == "primitive") {
    result = te.typeRef.value;
  } else if (te.typeRef.kind == "typeParam") {
    result = te.typeRef.value;
  } else if (te.typeRef.kind == "reference") {
    result = withScopedNames
      ? te.typeRef.value.moduleName + "." + te.typeRef.value.name
      : te.typeRef.value.name;
  }
  if (te.parameters.length > 0) {
    result = result + "<" + te.parameters.map(p => typeExprToStringImpl(p, withScopedNames)) + ">";
  }
  return result;
}

/* Convert a type expression to a string, with fully scoped names */

export function typeExprToString(te: AST.TypeExpr) : string {
  return typeExprToStringImpl(te, true);
}

/* Convert a type expression to a string, with unscoped names */

export function typeExprToStringUnscoped(te: AST.TypeExpr) : string {
  return typeExprToStringImpl(te, false);
}

