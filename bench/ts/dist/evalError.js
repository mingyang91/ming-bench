export class EvalError extends Error {
    constructor(message) {
        super(message);
        this.name = 'EvalError';
    }
}
