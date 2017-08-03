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
    if(typeExprsEqual(texpr1.parameters[i], texpr2.parameters[i])) {
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
    return tref1.value.moduleName === tref2.value.moduleName &&
      tref1.value.name === tref2.value.name;
  }
  return false;
}
