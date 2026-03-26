export interface SourcePosition {
    line: number;
    col: number;
}
export declare class EvalError extends Error {
    readonly position?: SourcePosition;
    constructor(message: string, position?: SourcePosition);
}
