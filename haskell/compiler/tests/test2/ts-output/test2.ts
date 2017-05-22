/**
 * This is an auto generated typescript file compiled with the adl compiler.
 */
// This file requires the adl runtime typescript file to be located in the same directory.
import { TypeDesc } from './adl/runtime';

export interface Tree<T> {
  children: Tree<T>[];
  value: T;
}

export function makeTree<T>(
  input: {
    children: Tree<T>[],
    value: T,
  }
): Tree<T> {
  return {
    children: input.children,
    value: input.value,
  };
}

export function getTreeDesc<T>(): TypeDesc<Tree<T>> {
  return {ref: 'test2.Tree'};
}

export interface S4<T> {
  f1: S3<string>;
  f2: S3<T>;
}

export function makeS4<T>(
  input: {
    f1: S3<string>,
    f2: S3<T>,
  }
): S4<T> {
  return {
    f1: input.f1,
    f2: input.f2,
  };
}

export function getS4Desc<T>(): TypeDesc<S4<T>> {
  return {ref: 'test2.S4'};
}

/**
 * A generic structure.
 */
export interface S3<T> {
  f1: string;
  f2: number;
  f3: T;
  f4: T[];
}

export function makeS3<T>(
  input: {
    f1: string,
    f2: number,
    f3: T,
    f4: T[],
  }
): S3<T> {
  return {
    f1: input.f1,
    f2: input.f2,
    f3: input.f3,
    f4: input.f4,
  };
}

export function getS3Desc<T>(): TypeDesc<S3<T>> {
  return {ref: 'test2.S3'};
}

/**
 * A structure containing a vector.
 */
export interface S2 {
  f1: string;
  f2: number;
  f3: number[];
}

export function makeS2(
  input: {
    f1: string,
    f2: number,
    f3: number[],
  }
): S2 {
  return {
    f1: input.f1,
    f2: input.f2,
    f3: input.f3,
  };
}

export function getS2Desc(): TypeDesc<S2> {
  return {ref: 'test2.S2'};
}

/**
 * A structure containing primitives.
 * It has two fields: an integer x and a String y.
 */
export interface S1 {
  x: number;
  y: string;
}

export function makeS1(
  input: {
    x: number,
    y: string,
  }
): S1 {
  return {
    x: input.x,
    y: input.y,
  };
}

export function getS1Desc(): TypeDesc<S1> {
  return {ref: 'test2.S1'};
}

/**
 * An empty structure.
 */
export interface S0 {
}

export function makeS0(
  input: {
  }
): S0 {
  return {
  };
}

export function getS0Desc(): TypeDesc<S0> {
  return {ref: 'test2.S0'};
}

export type IntTree = Tree<number>;

export function getIntTreeDesc(): TypeDesc<IntTree> {
  return {ref: 'test2.IntTree'};
}
