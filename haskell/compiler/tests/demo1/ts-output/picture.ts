/* @generated from adl module picture */

import * as ADL from './runtime/adl';

export interface Picture_Circle {
  kind: 'circle';
  value: Circle;
}
export interface Picture_Rectangle {
  kind: 'rectangle';
  value: Rectangle;
}
export interface Picture_Composed {
  kind: 'composed';
  value: Picture[];
}
export interface Picture_Translated {
  kind: 'translated';
  value: Translated<Picture>;
}

export type Picture = Picture_Circle | Picture_Rectangle | Picture_Composed | Picture_Translated;

export interface PictureOpts {
  circle: Circle;
  rectangle: Rectangle;
  composed: Picture[];
  translated: Translated<Picture>;
}

export function makePicture<K extends keyof PictureOpts>(kind: K, value: PictureOpts[K]) { return {kind, value}; }

const Picture_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Picture","type_":{"kind":"union_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"circle","serializedName":"circle","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Circle"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"rectangle","serializedName":"rectangle","typeExpr":{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Rectangle"}}}},{"annotations":[],"default":{"kind":"nothing"},"name":"composed","serializedName":"composed","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Picture"}}}],"typeRef":{"kind":"primitive","value":"Vector"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"translated","serializedName":"translated","typeExpr":{"parameters":[{"parameters":[],"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Picture"}}}],"typeRef":{"kind":"reference","value":{"moduleName":"picture","name":"Translated"}}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"picture"};

export const snPicture: ADL.ScopedName = {moduleName:"picture", name:"Picture"};

export function texprPicture(): ADL.ATypeExpr<Picture> {
  return {value : {typeRef : {kind: "reference", value : snPicture}, parameters : []}};
}

export interface Circle {
  radius: number;
}

export function makeCircle(
  input: {
    radius: number,
  }
): Circle {
  return {
    radius: input.radius,
  };
}

const Circle_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Circle","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"radius","serializedName":"radius","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"picture"};

export const snCircle: ADL.ScopedName = {moduleName:"picture", name:"Circle"};

export function texprCircle(): ADL.ATypeExpr<Circle> {
  return {value : {typeRef : {kind: "reference", value : snCircle}, parameters : []}};
}

export interface Rectangle {
  width: number;
  height: number;
}

export function makeRectangle(
  input: {
    width: number,
    height: number,
  }
): Rectangle {
  return {
    width: input.width,
    height: input.height,
  };
}

const Rectangle_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Rectangle","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"nothing"},"name":"width","serializedName":"width","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"height","serializedName":"height","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}}],"typeParams":[]}},"version":{"kind":"nothing"}},"moduleName":"picture"};

export const snRectangle: ADL.ScopedName = {moduleName:"picture", name:"Rectangle"};

export function texprRectangle(): ADL.ATypeExpr<Rectangle> {
  return {value : {typeRef : {kind: "reference", value : snRectangle}, parameters : []}};
}

export interface Translated<T> {
  xoffset: number;
  yoffset: number;
  object: T;
}

export function makeTranslated<T>(
  input: {
    xoffset?: number,
    yoffset?: number,
    object: T,
  }
): Translated<T> {
  return {
    xoffset: input.xoffset === undefined ? 0 : input.xoffset,
    yoffset: input.yoffset === undefined ? 0 : input.yoffset,
    object: input.object,
  };
}

const Translated_AST : ADL.ScopedDecl =
  {"decl":{"annotations":[],"name":"Translated","type_":{"kind":"struct_","value":{"fields":[{"annotations":[],"default":{"kind":"just","value":0},"name":"xoffset","serializedName":"xoffset","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}},{"annotations":[],"default":{"kind":"just","value":0},"name":"yoffset","serializedName":"yoffset","typeExpr":{"parameters":[],"typeRef":{"kind":"primitive","value":"Double"}}},{"annotations":[],"default":{"kind":"nothing"},"name":"object","serializedName":"object","typeExpr":{"parameters":[],"typeRef":{"kind":"typeParam","value":"T"}}}],"typeParams":["T"]}},"version":{"kind":"nothing"}},"moduleName":"picture"};

export const snTranslated: ADL.ScopedName = {moduleName:"picture", name:"Translated"};

export function texprTranslated<T>(texprT : ADL.ATypeExpr<T>): ADL.ATypeExpr<Translated<T>> {
  return {value : {typeRef : {kind: "reference", value : {moduleName : "picture",name : "Translated"}}, parameters : [texprT.value]}};
}

export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {
  "picture.Picture" : Picture_AST,
  "picture.Circle" : Circle_AST,
  "picture.Rectangle" : Rectangle_AST,
  "picture.Translated" : Translated_AST
};
