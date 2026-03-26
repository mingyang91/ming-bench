export class EvalError extends Error {
    position;
    constructor(message, position) {
        super(position === undefined ? message : `${position.line}:${position.col}: ${message}`);
        this.name = 'EvalError';
        this.position = position;
    }
}
