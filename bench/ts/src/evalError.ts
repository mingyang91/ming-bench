export interface SourcePosition {
  line: number;
  column: number;
}

export class EvalError extends Error {
  readonly rawMessage: string;
  readonly position?: SourcePosition;

  constructor(message: string, position?: SourcePosition) {
    super(position ? `${position.line}:${position.column}: ${message}` : message);
    this.name = 'EvalError';
    this.rawMessage = message;
    this.position = position;
  }
}
