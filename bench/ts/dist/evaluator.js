import { EvalError } from './evalError.js';
class Reader {
    input;
    index = 0;
    line = 1;
    col = 1;
    constructor(input) {
        this.input = input;
    }
    parseProgram() {
        const expressions = [];
        this.skipWhitespaceAndComments();
        while (!this.isEof()) {
            expressions.push(this.parseExpr());
            this.skipWhitespaceAndComments();
        }
        return expressions;
    }
    parseExpr() {
        this.skipWhitespaceAndComments();
        if (this.isEof()) {
            this.raise('unexpected end of input');
        }
        const loc = this.currentLoc();
        const ch = this.peek();
        if (ch === '(') {
            return this.parseList(loc);
        }
        if (ch === ')') {
            this.raise('unexpected )', loc);
        }
        if (ch === '"') {
            return this.parseString(loc);
        }
        return this.parseAtom(loc);
    }
    parseList(loc) {
        this.advance();
        const elements = [];
        this.skipWhitespaceAndComments();
        while (!this.isEof() && this.peek() !== ')') {
            elements.push(this.parseExpr());
            this.skipWhitespaceAndComments();
        }
        if (this.isEof()) {
            this.raise('unterminated list', loc);
        }
        this.advance();
        return { type: 'list', elements, ...loc };
    }
    parseString(loc) {
        this.advance();
        let value = '';
        let terminated = false;
        while (!this.isEof()) {
            const ch = this.advance();
            if (ch === '"') {
                terminated = true;
                break;
            }
            if (ch === '\\') {
                if (this.isEof()) {
                    this.raise('unterminated string escape', loc);
                }
                const escaped = this.advance();
                switch (escaped) {
                    case '"':
                        value += '"';
                        break;
                    case '\\':
                        value += '\\';
                        break;
                    case 'n':
                        value += '\n';
                        break;
                    case 't':
                        value += '\t';
                        break;
                    default:
                        this.raise(`unsupported escape \\${escaped}`, loc);
                }
                continue;
            }
            value += ch;
        }
        if (!terminated) {
            this.raise('unterminated string literal', loc);
        }
        return { type: 'string', value, ...loc };
    }
    parseAtom(loc) {
        let token = '';
        while (!this.isEof()) {
            const ch = this.peek();
            if (isWhitespace(ch) || ch === '(' || ch === ')' || ch === ';') {
                break;
            }
            token += this.advance();
        }
        if (token.length === 0) {
            this.raise('expected expression', loc);
        }
        if (token === '#t') {
            return { type: 'boolean', value: true, ...loc };
        }
        if (token === '#f') {
            return { type: 'boolean', value: false, ...loc };
        }
        if (/^-?\d+$/.test(token)) {
            return { type: 'number', value: Number(token), ...loc };
        }
        return { type: 'symbol', value: token, ...loc };
    }
    skipWhitespaceAndComments() {
        while (!this.isEof()) {
            const ch = this.peek();
            if (isWhitespace(ch)) {
                this.advance();
                continue;
            }
            if (ch === ';') {
                while (!this.isEof() && this.peek() !== '\n') {
                    this.advance();
                }
                continue;
            }
            break;
        }
    }
    currentLoc() {
        return { line: this.line, col: this.col };
    }
    isEof() {
        return this.index >= this.input.length;
    }
    peek() {
        return this.input[this.index] ?? '';
    }
    advance() {
        const ch = this.input[this.index] ?? '';
        this.index += 1;
        if (ch === '\n') {
            this.line += 1;
            this.col = 1;
        }
        else {
            this.col += 1;
        }
        return ch;
    }
    raise(message, loc = this.currentLoc()) {
        throw new EvalError(`${loc.line}:${loc.col}: ${message}`);
    }
}
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input) {
    const program = new Reader(input).parseProgram();
    if (program.length === 0) {
        throw new EvalError('1:1: expected expression');
    }
    let result = false;
    for (const expr of program) {
        result = evaluate(expr);
    }
    return formatValue(result);
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    return { result: evalStr(input), output: '' };
}
function evaluate(expr) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
            return expr.value;
        case 'string':
            return { kind: 'string', value: expr.value };
        case 'symbol':
            throw new EvalError(`${expr.line}:${expr.col}: unbound variable ${expr.value}`);
        case 'list':
            return evaluateList(expr);
    }
}
function evaluateList(expr) {
    if (expr.elements.length === 0) {
        throw new EvalError(`${expr.line}:${expr.col}: cannot evaluate empty list`);
    }
    const [head, ...args] = expr.elements;
    if (head.type !== 'symbol') {
        throw new EvalError(`${head.line}:${head.col}: operator must be a symbol`);
    }
    switch (head.value) {
        case '+':
            return evalNumericFold(args, head, 0, (left, right) => left + right);
        case '*':
            return evalNumericFold(args, head, 1, (left, right) => left * right);
        case '-':
            return evalSubtraction(args, head);
        case '/':
            return evalDivision(args, head);
        case '<':
            return evalComparison(args, head, (left, right) => left < right);
        case '>':
            return evalComparison(args, head, (left, right) => left > right);
        case '=':
            return evalComparison(args, head, (left, right) => left === right);
        case '<=':
            return evalComparison(args, head, (left, right) => left <= right);
        case 'not':
            return evalNot(args, head);
        case 'and':
            return evalAnd(args);
        case 'or':
            return evalOr(args);
        default:
            throw new EvalError(`${head.line}:${head.col}: unknown procedure ${head.value}`);
    }
}
function evalNumericFold(args, head, initial, op) {
    let result = initial;
    for (const arg of args) {
        result = op(result, expectNumber(evaluate(arg), arg));
    }
    return result;
}
function evalSubtraction(args, head) {
    if (args.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: - expects at least 1 argument`);
    }
    const first = expectNumber(evaluate(args[0]), args[0]);
    if (args.length === 1) {
        return -first;
    }
    let result = first;
    for (const arg of args.slice(1)) {
        result -= expectNumber(evaluate(arg), arg);
    }
    return result;
}
function evalDivision(args, head) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: / expects at least 2 arguments`);
    }
    let result = expectNumber(evaluate(args[0]), args[0]);
    for (const arg of args.slice(1)) {
        const value = expectNumber(evaluate(arg), arg);
        if (value === 0) {
            throw new EvalError(`${arg.line}:${arg.col}: division by zero`);
        }
        result /= value;
    }
    return result;
}
function evalComparison(args, head, predicate) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: ${head.value} expects at least 2 arguments`);
    }
    const values = args.map((arg) => expectNumber(evaluate(arg), arg));
    for (let index = 0; index < values.length - 1; index += 1) {
        if (!predicate(values[index], values[index + 1])) {
            return false;
        }
    }
    return true;
}
function evalNot(args, head) {
    if (args.length !== 1) {
        throw new EvalError(`${head.line}:${head.col}: not expects exactly 1 argument`);
    }
    return !isTruthy(evaluate(args[0]));
}
function evalAnd(args) {
    let result = true;
    for (const arg of args) {
        result = evaluate(arg);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalOr(args) {
    let result = false;
    for (const arg of args) {
        result = evaluate(arg);
        if (isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function expectNumber(value, expr) {
    if (typeof value !== 'number') {
        throw new EvalError(`${expr.line}:${expr.col}: expected number`);
    }
    return value;
}
function isTruthy(value) {
    return value !== false;
}
function formatValue(value) {
    if (typeof value === 'number') {
        return Object.is(value, -0) ? '0' : String(value);
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    return `"${escapeString(value.value)}"`;
}
function escapeString(value) {
    return value
        .replaceAll('\\', '\\\\')
        .replaceAll('"', '\\"')
        .replaceAll('\n', '\\n')
        .replaceAll('\t', '\\t');
}
function isWhitespace(ch) {
    return ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r';
}
