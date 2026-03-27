export interface SourcePosition {
    line: number;
    column: number;
}
export declare class EvalError extends Error {
    readonly detail: string;
    readonly position?: SourcePosition;
    constructor(message: string, position?: SourcePosition);
    withPosition(position: SourcePosition): EvalError;
}
export declare function attachPosition(error: unknown, position: SourcePosition): EvalError;
