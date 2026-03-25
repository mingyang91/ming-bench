export interface SourcePosition {
    line: number;
    column: number;
}
export declare class EvalError extends Error {
    readonly rawMessage: string;
    readonly position?: SourcePosition;
    constructor(message: string, position?: SourcePosition);
}
