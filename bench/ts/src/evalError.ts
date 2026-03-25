export interface SourceLocation {
  line: number;
  column: number;
}

function formatMessage(message: string, location?: SourceLocation): string {
  if (location === undefined) {
    return message;
  }

  return `${location.line}:${location.column}: ${message}`;
}

export class EvalError extends Error {
  readonly detail: string;
  readonly location?: SourceLocation;

  constructor(message: string, location?: SourceLocation) {
    super(formatMessage(message, location));
    this.name = 'EvalError';
    this.detail = message;
    this.location = location;
    Object.setPrototypeOf(this, new.target.prototype);
  }

  withLocation(location: SourceLocation): EvalError {
    if (this.location !== undefined) {
      return this;
    }

    return new EvalError(this.detail, location);
  }
}
