/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result. All expressions are chained
 * in a single CPS chain so continuations can span across them.
 */
export declare function evalStr(input: string): string;
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export declare function evalStrWithLimit(input: string, maxSteps: number): string;
export declare function evalStrWithOutput(input: string): {
    result: string;
    output: string;
};
