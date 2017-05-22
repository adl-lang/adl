/**
 * This is an auto generated typescript file compiled with the adl compiler.
 */
// This file requires the adl runtime typescript file to be located in the same directory.
import { TypeRef } from './runtime/adl';

/**
 * Testing multiline comment.
 * This is the second line.
 */
export interface Rectangle {
  height: number;
  width: number;
}

export function makeRectangle(
  input: {
    height: number,
    width: number,
  }
): Rectangle {
  return {
    height: input.height,
    width: input.width,
  };
}

export function refRectangle(): TypeRef<Rectangle> {
  return {ref: 'shapes.Rectangle'};
}

export interface Point {
  x: number;
  y: number;
}

export function makePoint(
  input: {
    x: number,
    y: number,
  }
): Point {
  return {
    x: input.x,
    y: input.y,
  };
}

export function refPoint(): TypeRef<Point> {
  return {ref: 'shapes.Point'};
}

export type Color = string;

export function refColor(): TypeRef<Color> {
  return {ref: 'shapes.Color'};
}

export interface Circle {
  center: Point;
  color: Color|null;
  radius: number;
}

export function makeCircle(
  input: {
    center?: Point,
    color: Color|null,
    radius: number,
  }
): Circle {
  return {
    center: input.center === undefined ? {
      x: 10,
      y: 10,
    } : input.center,
    color: input.color,
    radius: input.radius,
  };
}

export function refCircle(): TypeRef<Circle> {
  return {ref: 'shapes.Circle'};
}
