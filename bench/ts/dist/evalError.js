export class EvalError extends Error {
    rawMessage;
    position;
    constructor(message, position) {
        super(position ? `${position.line}:${position.column}: ${message}` : message);
        this.name = 'EvalError';
        this.rawMessage = message;
        this.position = position;
    }
}
