/**
 * This is an auto generated typescript file compiled with the adl compiler.
 */
// This file requires the adl runtime typescript file to be located in the same directory.
import { TypeRef } from './runtime/adl';

import * as shapes from './shapes';
/**
 * Testing doc comment for Translated.
 */
export interface Translated<T> {
  layer: number;
  object: T;
  xoffset: number;
  yoffset: number;
}

export function makeTranslated<T>(
  input: {
    layer?: number,
    object: T,
    xoffset?: number,
    yoffset?: number,
  }
): Translated<T> {
  return {
    layer: input.layer === undefined ? 1 : input.layer,
    object: input.object,
    xoffset: input.xoffset === undefined ? 0 : input.xoffset,
    yoffset: input.yoffset === undefined ? 10.5 : input.yoffset,
  };
}

export function refTranslated<T>(): TypeRef<Translated<T>> {
  return {ref: 'picture.Translated'};
}

export enum Terminal {
  t1,
  t2,
  t3,
}

export function refTerminal(): TypeRef<Terminal> {
  return {ref: 'picture.Terminal'};
}

export type PictureRow = Picture[];

export function refPictureRow(): TypeRef<PictureRow> {
  return {ref: 'picture.PictureRow'};
}

interface Picture_Circle {
  kind: 'circle';
  value: shapes.Circle;
}
interface Picture_Composed {
  kind: 'composed';
  value: Picture[];
}
interface Picture_Empty {
  kind: 'empty';
}
interface Picture_Numbers {
  kind: 'numbers';
  value: number;
}
interface Picture_Rectangle {
  kind: 'rectangle';
  value: shapes.Rectangle;
}
interface Picture_Translated {
  kind: 'translated';
  value: Translated<Picture>;
}

export function makePicture_Circle(
  input: {
    kind: 'circle',
    value: shapes.Circle,
  }
): Picture_Circle {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makePicture_Composed(
  input: {
    kind: 'composed',
    value: Picture[],
  }
): Picture_Composed {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makePicture_Empty(
  input: {
    kind: 'empty',
  }
): Picture_Empty {
  return {
    kind: input.kind,
  };
}
export function makePicture_Numbers(
  input: {
    kind: 'numbers',
    value: number,
  }
): Picture_Numbers {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makePicture_Rectangle(
  input: {
    kind: 'rectangle',
    value: shapes.Rectangle,
  }
): Picture_Rectangle {
  return {
    kind: input.kind,
    value: input.value,
  };
}
export function makePicture_Translated(
  input: {
    kind: 'translated',
    value: Translated<Picture>,
  }
): Picture_Translated {
  return {
    kind: input.kind,
    value: input.value,
  };
}

/**
 * Testing doc comment for Picture.
 */
export type Picture = Picture_Circle | Picture_Composed | Picture_Empty | Picture_Numbers | Picture_Rectangle | Picture_Translated;

export function refPicture(): TypeRef<Picture> {
  return {ref: 'picture.Picture'};
}

export interface ModernArtGallery {
  circle: shapes.Circle;
}

export function makeModernArtGallery(
  input: {
    circle?: shapes.Circle,
  }
): ModernArtGallery {
  return {
    circle: input.circle === undefined ? {
      radius: 10,
      color: '#f00',
      center: {
        x: 10,
        y: 10,
      },
    } : input.circle,
  };
}

export function refModernArtGallery(): TypeRef<ModernArtGallery> {
  return {ref: 'picture.ModernArtGallery'};
}

export interface Gallery {
  artwork: Picture[];
  moved: Translated<Picture>;
}

export function makeGallery(
  input: {
    artwork: Picture[],
    moved: Translated<Picture>,
  }
): Gallery {
  return {
    artwork: input.artwork,
    moved: input.moved,
  };
}

export function refGallery(): TypeRef<Gallery> {
  return {ref: 'picture.Gallery'};
}

export interface ContemporaryArtGallery {
  artwork: number[];
}

export function makeContemporaryArtGallery(
  input: {
    artwork?: number[],
  }
): ContemporaryArtGallery {
  return {
    artwork: input.artwork === undefined ? [5.6, 7.8, 9] : input.artwork,
  };
}

export function refContemporaryArtGallery(): TypeRef<ContemporaryArtGallery> {
  return {ref: 'picture.ContemporaryArtGallery'};
}

export interface CircleAndBlankWall {
  circle1: shapes.Circle;
  circle2: shapes.Circle;
}

export function makeCircleAndBlankWall(
  input: {
    circle1?: shapes.Circle,
    circle2?: shapes.Circle,
  }
): CircleAndBlankWall {
  return {
    circle1: input.circle1 === undefined ? {
      radius: 5,
      color: '#f00',
      center: {
        x: 10,
        y: 10,
      },
    } : input.circle1,
    circle2: input.circle2 === undefined ? {
      radius: 5,
      color: null,
      center: {
        x: 10,
        y: 10,
      },
    } : input.circle2,
  };
}

export function refCircleAndBlankWall(): TypeRef<CircleAndBlankWall> {
  return {ref: 'picture.CircleAndBlankWall'};
}
