import { EvalError } from './evalError.js';
const VOID = { kind: 'void' };
class Env {
    parent;
    bindings = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, value) {
        this.bindings.set(name, value);
    }
    lookup(name) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        if (this.parent !== undefined) {
            return this.parent.lookup(name);
        }
        throw new EvalError(`unbound symbol: ${name}`);
    }
}
class Parser {
    input;
    index = 0;
    constructor(input) {
        this.input = input;
    }
    parseProgram() {
        const exprs = [];
        this.skipIgnored();
        while (!this.isAtEnd()) {
            exprs.push(this.parseExpr());
            this.skipIgnored();
        }
        return exprs;
    }
    parseExpr() {
        this.skipIgnored();
        if (this.isAtEnd()) {
            throw new EvalError('unexpected end of input');
        }
        const ch = this.peek();
        if (ch === '\'') {
            this.advance();
            return {
                kind: 'list',
                items: [
                    { kind: 'symbol', name: 'quote' },
                    this.parseExpr(),
                ],
            };
        }
        if (ch === '(') {
            return this.parseList();
        }
        if (ch === ')') {
            throw new EvalError('unexpected )');
        }
        if (ch === '"') {
            return this.parseString();
        }
        return this.parseAtom();
    }
    parseList() {
        this.advance();
        const items = [];
        this.skipIgnored();
        while (!this.isAtEnd() && this.peek() !== ')') {
            items.push(this.parseExpr());
            this.skipIgnored();
        }
        if (this.isAtEnd()) {
            throw new EvalError('unterminated list');
        }
        this.advance();
        return { kind: 'list', items };
    }
    parseString() {
        this.advance();
        let value = '';
        while (!this.isAtEnd()) {
            const ch = this.advance();
            if (ch === '"') {
                return { kind: 'string', value };
            }
            if (ch === '\\') {
                if (this.isAtEnd()) {
                    throw new EvalError('unterminated string');
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
                        value += escaped;
                        break;
                }
            }
            else {
                value += ch;
            }
        }
        throw new EvalError('unterminated string');
    }
    parseAtom() {
        const start = this.index;
        while (!this.isAtEnd() && !isDelimiter(this.peek())) {
            this.advance();
        }
        const token = this.input.slice(start, this.index);
        if (token.length === 0) {
            throw new EvalError('expected expression');
        }
        if (token === '#t') {
            return { kind: 'boolean', value: true };
        }
        if (token === '#f') {
            return { kind: 'boolean', value: false };
        }
        if (/^[+-]?\d+$/.test(token) && token !== '+' && token !== '-') {
            return { kind: 'number', value: Number.parseInt(token, 10) };
        }
        return { kind: 'symbol', name: token };
    }
    skipIgnored() {
        while (!this.isAtEnd()) {
            const ch = this.peek();
            if (isWhitespace(ch)) {
                this.advance();
                continue;
            }
            if (ch === ';') {
                while (!this.isAtEnd() && this.peek() !== '\n') {
                    this.advance();
                }
                continue;
            }
            return;
        }
    }
    isAtEnd() {
        return this.index >= this.input.length;
    }
    peek() {
        return this.input[this.index];
    }
    advance() {
        const ch = this.input[this.index];
        this.index += 1;
        return ch;
    }
}
const BUILTINS = new Map([
    builtin('+', (args) => sumNumbers('+', args, 0)),
    builtin('*', (args) => productNumbers('*', args, 1)),
    builtin('-', (args) => subtractNumbers(args)),
    builtin('/', (args) => divideNumbers(args)),
    builtin('<', (args) => compareNumbers('<', args, (left, right) => left < right)),
    builtin('>', (args) => compareNumbers('>', args, (left, right) => left > right)),
    builtin('=', (args) => compareNumbers('=', args, (left, right) => left === right)),
    builtin('<=', (args) => compareNumbers('<=', args, (left, right) => left <= right)),
    builtin('not', (args) => {
        assertExactArity('not', args, 1);
        return !isTruthy(args[0]);
    }),
]);
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input) {
    return evalStrWithOutput(input).result;
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    const program = new Parser(input).parseProgram();
    if (program.length === 0) {
        throw new EvalError('empty input');
    }
    const env = createGlobalEnv();
    let lastValue = VOID;
    for (const expr of program) {
        lastValue = evalExpr(expr, env);
    }
    return {
        result: formatValue(lastValue),
        output: '',
    };
}
function createGlobalEnv() {
    const env = new Env();
    for (const [name, value] of BUILTINS) {
        env.define(name, value);
    }
    return env;
}
function evalExpr(expr, env) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr.value;
        case 'symbol':
            return env.lookup(expr.name);
        case 'list':
            return evalList(expr.items, env);
    }
}
function evalList(items, env) {
    if (items.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const first = items[0];
    if (first.kind === 'symbol') {
        switch (first.name) {
            case 'and':
                return evalAnd(items.slice(1), env);
            case 'or':
                return evalOr(items.slice(1), env);
            case 'define':
                return evalDefine(items.slice(1), env);
            case 'if':
                return evalIf(items.slice(1), env);
            case 'lambda':
                return evalLambda(items.slice(1), env);
            case 'quote':
                return evalQuote(items.slice(1));
        }
    }
    const proc = evalExpr(first, env);
    if (!isProcedure(proc)) {
        throw new EvalError('attempted to call a non-procedure');
    }
    const args = items.slice(1).map((item) => evalExpr(item, env));
    return applyProcedure(proc, args);
}
function evalAnd(args, env) {
    let result = true;
    for (const arg of args) {
        result = evalExpr(arg, env);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalOr(args, env) {
    for (const arg of args) {
        const value = evalExpr(arg, env);
        if (isTruthy(value)) {
            return value;
        }
    }
    return false;
}
function evalDefine(args, env) {
    assertAtLeastArity('define', args, 2);
    const target = args[0];
    const body = args.slice(1);
    if (target.kind === 'symbol') {
        assertExactArity('define', body, 1);
        env.define(target.name, evalExpr(body[0], env));
        return VOID;
    }
    if (target.kind === 'list' && target.items.length > 0) {
        const nameExpr = target.items[0];
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('define expects a symbol name');
        }
        const params = parseParams(target.items.slice(1));
        assertAtLeastArity('define', body, 1);
        env.define(nameExpr.name, {
            kind: 'lambda',
            name: nameExpr.name,
            params,
            body,
            env,
        });
        return VOID;
    }
    throw new EvalError('define expects a symbol name');
}
function evalIf(args, env) {
    assertExactArity('if', args, 3);
    const [conditionExpr, thenExpr, elseExpr] = args;
    return isTruthy(evalExpr(conditionExpr, env))
        ? evalExpr(thenExpr, env)
        : evalExpr(elseExpr, env);
}
function evalLambda(args, env) {
    assertAtLeastArity('lambda', args, 2);
    const paramsExpr = args[0];
    if (paramsExpr.kind !== 'list') {
        throw new EvalError('lambda expects a parameter list');
    }
    return {
        kind: 'lambda',
        params: parseParams(paramsExpr.items),
        body: args.slice(1),
        env,
    };
}
function evalQuote(args) {
    assertExactArity('quote', args, 1);
    return quoteExpr(args[0]);
}
function parseParams(items) {
    return items.map((item) => {
        if (item.kind !== 'symbol') {
            throw new EvalError('lambda parameters must be symbols');
        }
        return item.name;
    });
}
function quoteExpr(expr) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr.value;
        case 'symbol':
            return { kind: 'symbol-value', name: expr.name };
        case 'list':
            return {
                kind: 'list-value',
                items: expr.items.map((item) => quoteExpr(item)),
            };
    }
}
function applyProcedure(proc, args) {
    if (proc.kind === 'builtin') {
        return proc.apply(args);
    }
    assertExactArity(proc.name ?? 'lambda', args, proc.params.length);
    const callEnv = new Env(proc.env);
    proc.params.forEach((param, index) => {
        callEnv.define(param, args[index]);
    });
    let result = VOID;
    for (const expr of proc.body) {
        result = evalExpr(expr, callEnv);
    }
    return result;
}
function builtin(name, apply) {
    return [name, { kind: 'builtin', name, apply }];
}
function sumNumbers(name, args, initial) {
    const numbers = expectNumbers(name, args);
    return normalizeNumber(numbers.reduce((sum, value) => sum + value, initial));
}
function productNumbers(name, args, initial) {
    const numbers = expectNumbers(name, args);
    return normalizeNumber(numbers.reduce((product, value) => product * value, initial));
}
function subtractNumbers(args) {
    const numbers = expectNumbers('-', args);
    assertAtLeastArity('-', numbers, 1);
    if (numbers.length === 1) {
        return normalizeNumber(-numbers[0]);
    }
    return normalizeNumber(numbers.slice(1).reduce((acc, value) => acc - value, numbers[0]));
}
function divideNumbers(args) {
    const numbers = expectNumbers('/', args);
    assertAtLeastArity('/', numbers, 1);
    if (numbers.length === 1) {
        if (numbers[0] === 0) {
            throw new EvalError('division by zero');
        }
        return normalizeNumber(1 / numbers[0]);
    }
    let result = numbers[0];
    for (const divisor of numbers.slice(1)) {
        if (divisor === 0) {
            throw new EvalError('division by zero');
        }
        result /= divisor;
    }
    return normalizeNumber(result);
}
function compareNumbers(name, args, compare) {
    const numbers = expectNumbers(name, args);
    assertAtLeastArity(name, numbers, 2);
    for (let index = 1; index < numbers.length; index += 1) {
        if (!compare(numbers[index - 1], numbers[index])) {
            return false;
        }
    }
    return true;
}
function expectNumbers(name, args) {
    return args.map((arg) => {
        if (typeof arg !== 'number') {
            throw new EvalError(`${name} expects number arguments`);
        }
        return arg;
    });
}
function assertExactArity(name, args, expected) {
    if (args.length !== expected) {
        throw new EvalError(`${name} expects exactly ${expected} argument(s)`);
    }
}
function assertAtLeastArity(name, args, minimum) {
    if (args.length < minimum) {
        throw new EvalError(`${name} expects at least ${minimum} argument(s)`);
    }
}
function formatValue(value) {
    if (typeof value === 'number') {
        return formatNumber(value);
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    if (typeof value === 'string') {
        return JSON.stringify(value);
    }
    switch (value.kind) {
        case 'symbol-value':
            return value.name;
        case 'list-value':
            return `(${value.items.map((item) => formatValue(item)).join(' ')})`;
        case 'void':
            return '';
        case 'builtin':
            return `#<procedure:${value.name}>`;
        case 'lambda':
            return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
    }
}
function formatNumber(value) {
    const normalized = normalizeNumber(value);
    return Number.isInteger(normalized) ? normalized.toString() : String(normalized);
}
function normalizeNumber(value) {
    return Object.is(value, -0) ? 0 : value;
}
function isTruthy(value) {
    return value !== false;
}
function isProcedure(value) {
    return typeof value === 'object' && value !== null && (value.kind === 'builtin' || value.kind === 'lambda');
}
function isWhitespace(ch) {
    return /\s/.test(ch);
}
function isDelimiter(ch) {
    return isWhitespace(ch) || ch === '(' || ch === ')' || ch === ';' || ch === '\'';
}
