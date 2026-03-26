export interface SourcePosition {
  line: number;
  col: number;
}

export class EvalError extends Error {
  readonly position?: SourcePosition;

  constructor(message: string, position?: SourcePosition) {
    super(position === undefined ? message : `${position.line}:${position.col}: ${message}`);
    this.name = 'EvalError';
    this.position = position;
  }
}
