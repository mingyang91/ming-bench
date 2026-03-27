export class EvalError extends Error {
    detail;
    position;
    constructor(message, position) {
        super(position === undefined ? message : `${message} at ${position.line}:${position.column}`);
        this.name = 'EvalError';
        this.detail = message;
        this.position = position;
    }
    withPosition(position) {
        return this.position === undefined ? new EvalError(this.detail, position) : this;
    }
}
export function attachPosition(error, position) {
    if (error instanceof EvalError) {
        return error.withPosition(position);
    }
    if (error instanceof Error) {
        return new EvalError(error.message, position);
    }
    return new EvalError(String(error), position);
}
