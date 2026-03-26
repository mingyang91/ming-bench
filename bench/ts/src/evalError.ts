export interface SourcePos {
  line: number;
  col: number;
}

export class EvalError extends Error {
  readonly line?: number;
  readonly col?: number;

  constructor(message: string, pos?: SourcePos) {
    super(pos === undefined ? message : `${pos.line}:${pos.col}: ${message}`);
    this.name = 'EvalError';
    this.line = pos?.line;
    this.col = pos?.col;
  }

  withPosition(pos: SourcePos): EvalError {
    if (this.line !== undefined && this.col !== undefined) {
      return this;
    }

    return new EvalError(this.message, pos);
  }
}
