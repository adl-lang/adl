export type TypeDesc<T> = {ref: string};

export interface JsonParseException {
    kind: 'JsonParseException';
    getMessage(): string;
    contextString(): string;
    pushField(fieldName: string): void;
    pushIndex(index: number): void;
    toString(): string;
}

export function jsonParseException(message: string): JsonParseException {
  const context: string[] = [];
  return {
    kind: 'JsonParseException',
    getMessage(): string {
      return message + ' at ' + this.contextString();
    },
    contextString(): string {
      const rcontext: string[] = context;
      rcontext.push('$');
      rcontext.reverse();
      return rcontext.join('.');
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

export function isJsonParseException(exception: {}): exception is JsonParseException {
    return (<JsonParseException> exception).kind === 'JsonParseException';
}