export interface SourcePos {
    line: number;
    col: number;
}
export declare class EvalError extends Error {
    readonly line?: number;
    readonly col?: number;
    constructor(message: string, pos?: SourcePos);
    withPosition(pos: SourcePos): EvalError;
}
