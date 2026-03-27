export interface SourcePosition {
  line: number;
  column: number;
}

export class EvalError extends Error {
  readonly detail: string;
  readonly position?: SourcePosition;

  constructor(message: string, position?: SourcePosition) {
    super(position === undefined ? message : `${message} at ${position.line}:${position.column}`);
    this.name = 'EvalError';
    this.detail = message;
    this.position = position;
  }

  withPosition(position: SourcePosition): EvalError {
    return this.position === undefined ? new EvalError(this.detail, position) : this;
  }
}

export function attachPosition(error: unknown, position: SourcePosition): EvalError {
  if (error instanceof EvalError) {
    return error.withPosition(position);
  }

  if (error instanceof Error) {
    return new EvalError(error.message, position);
  }

  return new EvalError(String(error), position);
}
