export class EvalError extends Error {
    line;
    col;
    constructor(message, pos) {
        super(pos === undefined ? message : `${pos.line}:${pos.col}: ${message}`);
        this.name = 'EvalError';
        this.line = pos?.line;
        this.col = pos?.col;
    }
    withPosition(pos) {
        if (this.line !== undefined && this.col !== undefined) {
            return this;
        }
        return new EvalError(this.message, pos);
    }
}
