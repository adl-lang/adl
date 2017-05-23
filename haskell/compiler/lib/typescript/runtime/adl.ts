export type TypeRef<T> = {ref: string};

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
 * Convenience sugar that throws an exception when a predicate evaluates to true.
 * @param predicate The predicate function to check.
 * @param message The message to throw.
 */
export function exceptionOn(predicate: () => boolean, message: string): boolean {
  let val: boolean = predicate();
  if (val) {
    throw jsonParseException(message);
  }
  return val;
}