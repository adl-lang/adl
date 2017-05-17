
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

/**
 * Testing multiline comment.
 * This is the second line.
 */
export interface Rectangle {
  height: number;
  width: number;
}

type Color = string;

export function makeCircle(
  input: {
    color: Color|null,
    radius: number,
  }
): Circle {
  return {
    color: input.color,
    radius: input.radius,
  };
}

export interface Circle {
  color: Color|null;
  radius: number;
}
