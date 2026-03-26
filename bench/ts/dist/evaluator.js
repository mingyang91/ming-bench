import { EvalError } from './evalError.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID_VALUE = { kind: 'void' };
const STRING_IMMUTABILITY_LEVEL = 15;
class Runtime {
    output = [];
    syntaxRules = new Map();
    nextUnique = 1;
    windStack = [];
    exceptionHandlers = [];
    write(value) {
        this.output.push(value);
    }
    readOutput() {
        return this.output.join('');
    }
    defineSyntaxRule(name, transformer) {
        this.syntaxRules.set(name, transformer);
    }
    lookupSyntaxRule(name) {
        return this.syntaxRules.get(name);
    }
    hasSyntaxRule(name) {
        return this.syntaxRules.has(name);
    }
    freshLookupName(name) {
        const unique = this.nextUnique;
        this.nextUnique += 1;
        return `#${unique}:${name}`;
    }
    snapshotWindStack() {
        return [...this.windStack];
    }
    pushWindFrame(frame) {
        this.windStack = [...this.windStack, frame];
    }
    popWindFrame() {
        const popped = this.windStack[this.windStack.length - 1];
        this.windStack = this.windStack.slice(0, -1);
        return popped;
    }
    setWindStack(stack) {
        this.windStack = [...stack];
    }
    snapshotExceptionHandlers() {
        return [...this.exceptionHandlers];
    }
    pushExceptionHandler(frame) {
        this.exceptionHandlers = [...this.exceptionHandlers, frame];
    }
    popExceptionHandler() {
        const popped = this.exceptionHandlers[this.exceptionHandlers.length - 1];
        this.exceptionHandlers = this.exceptionHandlers.slice(0, -1);
        return popped;
    }
    popExceptionHandlerFrame(frame) {
        if (this.exceptionHandlers[this.exceptionHandlers.length - 1] === frame) {
            this.exceptionHandlers = this.exceptionHandlers.slice(0, -1);
        }
    }
    setExceptionHandlers(stack) {
        this.exceptionHandlers = [...stack];
    }
}
class RaisedSignal {
    value;
    loc;
    constructor(value, loc) {
        this.value = value;
        this.loc = loc;
    }
}
class Environment {
    parent;
    bindings = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, value) {
        this.defineCell(name, { value });
    }
    defineCell(name, cell) {
        this.bindings.set(name, cell);
    }
    lookup(name, loc) {
        const cell = this.lookupCell(name);
        if (cell !== undefined) {
            return readBindingCell(cell, name, loc);
        }
        throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${name}`);
    }
    lookupSymbol(symbol, loc) {
        if (symbol.capturedCell !== undefined) {
            return readBindingCell(symbol.capturedCell, symbol.value, loc);
        }
        const cell = this.lookupCell(symbolLookupName(symbol));
        if (cell !== undefined) {
            return readBindingCell(cell, symbol.value, loc);
        }
        throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${symbol.value}`);
    }
    assign(name, value, loc) {
        const cell = this.lookupCell(name);
        if (cell !== undefined) {
            cell.value = value;
            return;
        }
        throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${name}`);
    }
    assignSymbol(symbol, value, loc) {
        if (symbol.capturedCell !== undefined) {
            symbol.capturedCell.value = value;
            return;
        }
        const cell = this.lookupCell(symbolLookupName(symbol));
        if (cell !== undefined) {
            cell.value = value;
            return;
        }
        throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${symbol.value}`);
    }
    lookupPlainCell(name) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        return this.parent?.lookupPlainCell(name);
    }
    lookupCell(name) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        return this.parent?.lookupCell(name);
    }
}
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
        if (ch === '\'') {
            return this.parseQuoted(loc);
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
    parseQuoted(loc) {
        this.advance();
        return {
            type: 'list',
            elements: [
                { type: 'symbol', value: 'quote', ...loc },
                this.parseExpr(),
            ],
            ...loc,
        };
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
            if (isWhitespace(ch) || ch === '(' || ch === ')' || ch === '\'' || ch === ';') {
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
        const parsedNumber = parseNumberToken(token);
        if (parsedNumber !== undefined) {
            if (parsedNumber === null) {
                this.raise('invalid rational literal', loc);
            }
            return { type: 'number', value: parsedNumber, ...loc };
        }
        if (token.startsWith('#\\')) {
            return { type: 'char', value: parseCharLiteral(token, loc), ...loc };
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
    return formatValue(evaluateProgram(input).result);
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    const { result, output } = evaluateProgram(input);
    return { result: formatValue(result), output };
}
function evaluateProgram(input) {
    const program = new Reader(input).parseProgram();
    if (program.length === 0) {
        throw new EvalError('1:1: expected expression');
    }
    const runtime = new Runtime();
    const env = createGlobalEnv(runtime);
    const result = evaluateSequence(program, env, runtime);
    return { result, output: runtime.readOutput() };
}
function createGlobalEnv(runtime) {
    const env = new Environment();
    env.define('+', builtin('+', (args) => {
        return addNumbers(args.map(expectNumber));
    }));
    env.define('*', builtin('*', (args) => {
        return multiplyNumbers(args.map(expectNumber));
    }));
    env.define('-', builtin('-', (args, loc) => {
        if (args.length === 0) {
            throw new EvalError(`${loc.line}:${loc.col}: - expects at least 1 argument`);
        }
        const first = expectNumber(args[0]);
        if (args.length === 1) {
            return negateNumber(first);
        }
        return subtractNumbers(first, args.slice(1).map(expectNumber));
    }));
    env.define('/', builtin('/', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: / expects at least 2 arguments`);
        }
        let result = expectNumber(args[0]);
        for (const arg of args.slice(1)) {
            const value = expectNumber(arg);
            if (isZeroNumber(value)) {
                throw new EvalError(`${arg.expr.line}:${arg.expr.col}: division by zero`);
            }
            result = divideTwoNumbers(result, value);
        }
        return result;
    }));
    env.define('abs', builtin('abs', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: abs expects exactly 1 argument`);
        }
        return absNumber(expectNumber(args[0]));
    }));
    env.define('gcd', builtin('gcd', (args) => {
        if (args.length === 0) {
            return makeExactNumber(0);
        }
        const numbers = args.map(expectNumber);
        let result = 0;
        for (let index = 0; index < numbers.length; index += 1) {
            result = gcd(result, expectIntegerNumber(numbers[index], args[index].expr));
        }
        return numbers.every((value) => value.exact)
            ? makeExactNumber(result)
            : makeInexactNumber(result);
    }));
    env.define('lcm', builtin('lcm', (args) => {
        if (args.length === 0) {
            return makeExactNumber(1);
        }
        const numbers = args.map(expectNumber);
        let result = 1;
        for (let index = 0; index < numbers.length; index += 1) {
            result = lcm(result, expectIntegerNumber(numbers[index], args[index].expr));
        }
        return numbers.every((value) => value.exact)
            ? makeExactNumber(result)
            : makeInexactNumber(result);
    }));
    env.define('truncate', builtin('truncate', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: truncate expects exactly 1 argument`);
        }
        const value = expectNumber(args[0]);
        const truncated = Math.trunc(numberToJs(value));
        return value.exact ? makeExactNumber(truncated) : makeInexactNumber(truncated);
    }));
    env.define('round', builtin('round', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: round expects exactly 1 argument`);
        }
        const value = expectNumber(args[0]);
        const rounded = Math.round(numberToJs(value));
        return value.exact ? makeExactNumber(rounded) : makeInexactNumber(rounded);
    }));
    env.define('quotient', builtin('quotient', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: quotient expects exactly 2 arguments`);
        }
        const dividendValue = expectNumber(args[0]);
        const divisorValue = expectNumber(args[1]);
        const dividend = expectIntegerNumber(dividendValue, args[0].expr);
        const divisor = expectIntegerNumber(divisorValue, args[1].expr);
        if (divisor === 0) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: division by zero`);
        }
        const result = Math.trunc(dividend / divisor);
        return dividendValue.exact && divisorValue.exact
            ? makeExactNumber(result)
            : makeInexactNumber(result);
    }));
    env.define('remainder', builtin('remainder', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: remainder expects exactly 2 arguments`);
        }
        const dividendValue = expectNumber(args[0]);
        const divisorValue = expectNumber(args[1]);
        const dividend = expectIntegerNumber(dividendValue, args[0].expr);
        const divisor = expectIntegerNumber(divisorValue, args[1].expr);
        if (divisor === 0) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: division by zero`);
        }
        const result = dividend % divisor;
        return dividendValue.exact && divisorValue.exact
            ? makeExactNumber(result)
            : makeInexactNumber(result);
    }));
    env.define('modulo', builtin('modulo', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: modulo expects exactly 2 arguments`);
        }
        const dividendValue = expectNumber(args[0]);
        const divisorValue = expectNumber(args[1]);
        const dividend = expectIntegerNumber(dividendValue, args[0].expr);
        const divisor = expectIntegerNumber(divisorValue, args[1].expr);
        if (divisor === 0) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: division by zero`);
        }
        const result = dividend - (divisor * Math.floor(dividend / divisor));
        return dividendValue.exact && divisorValue.exact
            ? makeExactNumber(result)
            : makeInexactNumber(result);
    }));
    env.define('min', builtin('min', (args, loc) => {
        if (args.length === 0) {
            throw new EvalError(`${loc.line}:${loc.col}: min expects at least 1 argument`);
        }
        return minNumbers(args.map(expectNumber));
    }));
    env.define('max', builtin('max', (args, loc) => {
        if (args.length === 0) {
            throw new EvalError(`${loc.line}:${loc.col}: max expects at least 1 argument`);
        }
        return maxNumbers(args.map(expectNumber));
    }));
    env.define('expt', builtin('expt', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: expt expects exactly 2 arguments`);
        }
        const base = expectNumber(args[0]);
        const exponentValue = expectNumber(args[1]);
        const exponent = expectIntegerNumber(exponentValue, args[1].expr);
        return exponentValue.exact
            ? exactAwarePower(base, exponent)
            : makeInexactNumber(Math.pow(numberToJs(base), exponent));
    }));
    env.define('<', comparisonBuiltin('<', (left, right) => compareNumbers(left, right) < 0));
    env.define('>', comparisonBuiltin('>', (left, right) => compareNumbers(left, right) > 0));
    env.define('=', comparisonBuiltin('=', numbersEqual));
    env.define('<=', comparisonBuiltin('<=', (left, right) => compareNumbers(left, right) <= 0));
    env.define('>=', comparisonBuiltin('>=', (left, right) => compareNumbers(left, right) >= 0));
    env.define('zero?', builtin('zero?', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: zero? expects exactly 1 argument`);
        }
        return isZeroNumber(expectNumber(args[0]));
    }));
    env.define('positive?', builtin('positive?', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: positive? expects exactly 1 argument`);
        }
        return compareNumbers(expectNumber(args[0]), makeExactNumber(0)) > 0;
    }));
    env.define('negative?', builtin('negative?', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: negative? expects exactly 1 argument`);
        }
        return compareNumbers(expectNumber(args[0]), makeExactNumber(0)) < 0;
    }));
    env.define('exact?', predicateBuiltin('exact?', (value) => isNumberValue(value) && value.exact));
    env.define('inexact?', predicateBuiltin('inexact?', (value) => isNumberValue(value) && !value.exact));
    env.define('exact->inexact', builtin('exact->inexact', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: exact->inexact expects exactly 1 argument`);
        }
        return exactToInexact(expectNumber(args[0]));
    }));
    env.define('inexact->exact', builtin('inexact->exact', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: inexact->exact expects exactly 1 argument`);
        }
        return inexactToExact(expectNumber(args[0]));
    }));
    env.define('numerator', builtin('numerator', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: numerator expects exactly 1 argument`);
        }
        return makeExactNumber(expectExactNumber(args[0]).numerator);
    }));
    env.define('denominator', builtin('denominator', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: denominator expects exactly 1 argument`);
        }
        return makeExactNumber(expectExactNumber(args[0]).denominator);
    }));
    env.define('odd?', builtin('odd?', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: odd? expects exactly 1 argument`);
        }
        return Math.abs(expectIndexArg(args[0]) % 2) === 1;
    }));
    env.define('even?', builtin('even?', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: even? expects exactly 1 argument`);
        }
        return expectIndexArg(args[0]) % 2 === 0;
    }));
    env.define('not', builtin('not', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: not expects exactly 1 argument`);
        }
        return !isTruthy(args[0].value);
    }));
    env.define('cons', builtin('cons', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: cons expects exactly 2 arguments`);
        }
        return { kind: 'pair', car: args[0].value, cdr: args[1].value };
    }));
    env.define('car', builtin('car', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: car expects exactly 1 argument`);
        }
        return expectPairArg(args[0]).car;
    }));
    env.define('cdr', builtin('cdr', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: cdr expects exactly 1 argument`);
        }
        return expectPairArg(args[0]).cdr;
    }));
    env.define('cddr', builtin('cddr', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: cddr expects exactly 1 argument`);
        }
        const tail = expectPairArg(args[0]).cdr;
        if (!isPair(tail)) {
            throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: expected pair`);
        }
        return tail.cdr;
    }));
    env.define('set-car!', builtin('set-car!', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: set-car! expects exactly 2 arguments`);
        }
        expectPairArg(args[0]).car = args[1].value;
        return VOID_VALUE;
    }));
    env.define('set-cdr!', builtin('set-cdr!', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: set-cdr! expects exactly 2 arguments`);
        }
        expectPairArg(args[0]).cdr = args[1].value;
        return VOID_VALUE;
    }));
    env.define('null?', builtin('null?', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: null? expects exactly 1 argument`);
        }
        return isEmptyList(args[0].value);
    }));
    env.define('list', builtin('list', (args) => makeList(args.map((arg) => arg.value))));
    env.define('length', builtin('length', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: length expects exactly 1 argument`);
        }
        return makeExactNumber(expectProperList(args[0].value, args[0].expr).length);
    }));
    env.define('list-ref', builtin('list-ref', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: list-ref expects exactly 2 arguments`);
        }
        const elements = expectProperList(args[0].value, args[0].expr);
        const index = expectIndexArg(args[1]);
        if (index < 0 || index >= elements.length) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: list-ref index out of bounds`);
        }
        return elements[index];
    }));
    env.define('list-tail', builtin('list-tail', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: list-tail expects exactly 2 arguments`);
        }
        expectProperList(args[0].value, args[0].expr);
        const index = expectIndexArg(args[1]);
        if (index < 0) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: list-tail index out of bounds`);
        }
        let current = args[0].value;
        let remaining = index;
        while (remaining > 0) {
            if (!isPair(current)) {
                throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: list-tail index out of bounds`);
            }
            current = current.cdr;
            remaining -= 1;
        }
        if (!isPair(current) && !isEmptyList(current)) {
            throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: expected proper list`);
        }
        return current;
    }));
    env.define('append', builtin('append', (args) => {
        if (args.length === 0) {
            return EMPTY_LIST;
        }
        let result = args[args.length - 1].value;
        for (let index = args.length - 2; index >= 0; index -= 1) {
            const elements = expectProperList(args[index].value, args[index].expr);
            for (let elementIndex = elements.length - 1; elementIndex >= 0; elementIndex -= 1) {
                result = { kind: 'pair', car: elements[elementIndex], cdr: result };
            }
        }
        return result;
    }));
    env.define('reverse', builtin('reverse', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: reverse expects exactly 1 argument`);
        }
        return makeList(expectProperList(args[0].value, args[0].expr).slice().reverse());
    }));
    env.define('map', controlBuiltin('map', (args, loc, runtime, cont) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: map expects a procedure and at least 1 list`);
        }
        const procedure = args[0].value;
        const listArgs = args.slice(1);
        const lists = listArgs.map((arg) => expectProperList(arg.value, arg.expr));
        const expectedLength = lists[0].length;
        for (let index = 1; index < lists.length; index += 1) {
            if (lists[index].length !== expectedLength) {
                throw new EvalError(`${listArgs[index].expr.line}:${listArgs[index].expr.col}: map lists must have the same length`);
            }
        }
        const loop = (index, results) => {
            if (index >= expectedLength) {
                return cont(makeList(results));
            }
            const appliedArgs = listArgs.map((arg, listIndex) => ({
                expr: arg.expr,
                value: lists[listIndex][index],
            }));
            return applyProcedure(procedure, appliedArgs, args[0].expr, runtime, (value) => (loop(index + 1, [...results, value])));
        };
        return loop(0, []);
    }));
    env.define('for-each', controlBuiltin('for-each', (args, loc, runtime, cont) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: for-each expects a procedure and at least 1 list`);
        }
        const procedure = args[0].value;
        const listArgs = args.slice(1);
        const lists = listArgs.map((arg) => expectProperList(arg.value, arg.expr));
        const expectedLength = lists[0].length;
        for (let index = 1; index < lists.length; index += 1) {
            if (lists[index].length !== expectedLength) {
                throw new EvalError(`${listArgs[index].expr.line}:${listArgs[index].expr.col}: for-each lists must have the same length`);
            }
        }
        const loop = (index) => {
            if (index >= expectedLength) {
                return cont(VOID_VALUE);
            }
            const appliedArgs = listArgs.map((arg, listIndex) => ({
                expr: arg.expr,
                value: lists[listIndex][index],
            }));
            return applyProcedure(procedure, appliedArgs, args[0].expr, runtime, () => loop(index + 1));
        };
        return loop(0);
    }));
    env.define('eq?', builtin('eq?', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: eq? expects exactly 2 arguments`);
        }
        return isEq(args[0].value, args[1].value);
    }));
    env.define('eqv?', builtin('eqv?', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: eqv? expects exactly 2 arguments`);
        }
        return isEqv(args[0].value, args[1].value);
    }));
    env.define('equal?', builtin('equal?', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: equal? expects exactly 2 arguments`);
        }
        return isEqual(args[0].value, args[1].value);
    }));
    env.define('assoc', builtin('assoc', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: assoc expects exactly 2 arguments`);
        }
        let current = args[1].value;
        while (isPair(current)) {
            const entry = current.car;
            if (!isPair(entry)) {
                throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: assoc expects an association list`);
            }
            if (isEqual(args[0].value, entry.car)) {
                return entry;
            }
            current = current.cdr;
        }
        if (!isEmptyList(current)) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: expected proper list`);
        }
        return false;
    }));
    env.define('assv', builtin('assv', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: assv expects exactly 2 arguments`);
        }
        let current = args[1].value;
        while (isPair(current)) {
            const entry = current.car;
            if (!isPair(entry)) {
                throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: assv expects an association list`);
            }
            if (isEqv(args[0].value, entry.car)) {
                return entry;
            }
            current = current.cdr;
        }
        if (!isEmptyList(current)) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: expected proper list`);
        }
        return false;
    }));
    env.define('member', builtin('member', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: member expects exactly 2 arguments`);
        }
        let current = args[1].value;
        while (isPair(current)) {
            if (isEqual(args[0].value, current.car)) {
                return current;
            }
            current = current.cdr;
        }
        if (!isEmptyList(current)) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: expected proper list`);
        }
        return false;
    }));
    env.define('apply', controlBuiltin('apply', (args, loc, runtime, cont) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: apply expects at least 2 arguments`);
        }
        const [procedureArg, ...restArgs] = args;
        const listArg = restArgs[restArgs.length - 1];
        const prefixArgs = restArgs.slice(0, -1);
        const listElements = expectProperList(listArg.value, listArg.expr);
        const appliedArgs = [
            ...prefixArgs,
            ...listElements.map((value) => ({
                expr: listArg.expr,
                value,
            })),
        ];
        return applyProcedure(procedureArg.value, appliedArgs, procedureArg.expr, runtime, cont);
    }));
    const defineCallCcBuiltin = (name) => {
        env.define(name, controlBuiltin(name, (args, loc, runtime, cont) => {
            if (args.length !== 1) {
                throw new EvalError(`${loc.line}:${loc.col}: ${name} expects exactly 1 argument`);
            }
            const continuation = {
                kind: 'procedure',
                name: 'continuation',
                resume: cont,
                windStack: runtime.snapshotWindStack(),
                handlerStack: runtime.snapshotExceptionHandlers(),
            };
            return applyProcedure(args[0].value, [{ expr: plainSymbolExpr(name, loc), value: continuation }], loc, runtime, cont);
        }));
    };
    defineCallCcBuiltin('call/cc');
    defineCallCcBuiltin('call-with-current-continuation');
    env.define('dynamic-wind', controlBuiltin('dynamic-wind', (args, loc, runtime, cont) => {
        if (args.length !== 3) {
            throw new EvalError(`${loc.line}:${loc.col}: dynamic-wind expects exactly 3 arguments`);
        }
        const frame = {
            inThunk: expectProcedureArg(args[0]),
            inLoc: args[0].expr,
            outThunk: expectProcedureArg(args[2]),
            outLoc: args[2].expr,
        };
        const bodyThunk = expectProcedureArg(args[1]);
        return invokeThunk(frame.inThunk, frame.inLoc, runtime, (_ignored) => {
            runtime.pushWindFrame(frame);
            return invokeThunk(bodyThunk, args[1].expr, runtime, (value) => {
                runtime.popWindFrame();
                return invokeThunk(frame.outThunk, frame.outLoc, runtime, (_ignoredOut) => cont(value));
            });
        });
    }));
    env.define('raise', controlBuiltin('raise', (args, loc, _runtime, _cont) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: raise expects exactly 1 argument`);
        }
        throw new RaisedSignal(args[0].value, loc);
    }));
    env.define('with-exception-handler', controlBuiltin('with-exception-handler', (args, loc, runtime, cont) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: with-exception-handler expects exactly 2 arguments`);
        }
        const frame = {
            kind: 'procedure',
            handler: expectProcedureArg(args[0]),
            cont,
            windStack: runtime.snapshotWindStack(),
            loc,
        };
        const thunk = expectProcedureArg(args[1]);
        runtime.pushExceptionHandler(frame);
        return invokeThunk(thunk, args[1].expr, runtime, (value) => {
            runtime.popExceptionHandlerFrame(frame);
            return cont(value);
        });
    }));
    env.define('string?', predicateBuiltin('string?', isSchemeStringValue));
    env.define('number?', predicateBuiltin('number?', isNumberValue));
    env.define('integer?', predicateBuiltin('integer?', (value) => (isNumberValue(value) && isIntegerNumberValue(value))));
    env.define('rational?', predicateBuiltin('rational?', isNumberValue));
    env.define('boolean?', predicateBuiltin('boolean?', (value) => typeof value === 'boolean'));
    env.define('pair?', predicateBuiltin('pair?', isPair));
    env.define('list?', predicateBuiltin('list?', isProperList));
    env.define('symbol?', predicateBuiltin('symbol?', isSchemeSymbolValue));
    env.define('char?', predicateBuiltin('char?', isSchemeCharValue));
    env.define('procedure?', predicateBuiltin('procedure?', isProcedure));
    env.define('vector?', predicateBuiltin('vector?', isVectorValue));
    env.define('char-alphabetic?', predicateBuiltin('char-alphabetic?', (value) => (isSchemeCharValue(value) && isAlphabeticChar(value.value))));
    env.define('char-numeric?', predicateBuiltin('char-numeric?', (value) => (isSchemeCharValue(value) && isNumericChar(value.value))));
    env.define('display', builtin('display', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: display expects exactly 1 argument`);
        }
        runtime.write(formatDisplayValue(args[0].value));
        return VOID_VALUE;
    }));
    env.define('write', builtin('write', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: write expects exactly 1 argument`);
        }
        runtime.write(formatValue(args[0].value));
        return VOID_VALUE;
    }));
    env.define('newline', builtin('newline', (args, loc) => {
        if (args.length !== 0) {
            throw new EvalError(`${loc.line}:${loc.col}: newline expects exactly 0 arguments`);
        }
        runtime.write('\n');
        return VOID_VALUE;
    }));
    env.define('string-append', builtin('string-append', (args) => (makeString(args.map(expectString).join('')))));
    env.define('string-length', builtin('string-length', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string-length expects exactly 1 argument`);
        }
        return makeExactNumber(stringChars(expectStringArg(args[0])).length);
    }));
    env.define('substring', builtin('substring', (args, loc) => {
        if (args.length !== 3) {
            throw new EvalError(`${loc.line}:${loc.col}: substring expects exactly 3 arguments`);
        }
        const value = expectStringArg(args[0]);
        const chars = stringChars(value);
        const start = expectIndexArg(args[1]);
        const end = expectIndexArg(args[2]);
        if (start < 0 || end < start || end > chars.length) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: substring indices out of bounds`);
        }
        return makeString(chars.slice(start, end).join(''));
    }));
    env.define('string->number', builtin('string->number', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string->number expects exactly 1 argument`);
        }
        return parseStringNumber(expectStringArg(args[0]));
    }));
    env.define('number->string', builtin('number->string', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: number->string expects exactly 1 argument`);
        }
        return makeString(formatNumber(expectNumber(args[0])));
    }));
    env.define('symbol->string', builtin('symbol->string', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: symbol->string expects exactly 1 argument`);
        }
        return makeString(expectSymbolArg(args[0]).value);
    }));
    env.define('string->symbol', builtin('string->symbol', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string->symbol expects exactly 1 argument`);
        }
        return {
            kind: 'symbol',
            value: expectStringArg(args[0]),
        };
    }));
    env.define('string-ref', builtin('string-ref', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string-ref expects exactly 2 arguments`);
        }
        const chars = stringChars(expectStringArg(args[0]));
        const index = expectIndexArg(args[1]);
        if (index < 0 || index >= chars.length) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: string-ref index out of bounds`);
        }
        return { kind: 'char', value: chars[index] };
    }));
    env.define('string', builtin('string', (args) => (makeString(args.map((arg) => expectCharArg(arg).value).join('')))));
    env.define('make-string', builtin('make-string', (args, loc) => {
        if (args.length !== 1 && args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: make-string expects 1 or 2 arguments`);
        }
        const length = expectIndexArg(args[0]);
        if (length < 0) {
            throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: make-string length out of bounds`);
        }
        const fill = args[1] === undefined ? '\0' : expectCharArg(args[1]).value;
        return makeString(fill.repeat(length));
    }));
    env.define('string->list', builtin('string->list', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string->list expects exactly 1 argument`);
        }
        return makeList(expectStringValue(args[0]).chars.map((value) => ({ kind: 'char', value })));
    }));
    env.define('char=?', builtin('char=?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: char=? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (expectCharArg(args[index]).value !== expectCharArg(args[index + 1]).value) {
                return false;
            }
        }
        return true;
    }));
    env.define('char<?', builtin('char<?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: char<? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (!(expectCharArg(args[index]).value < expectCharArg(args[index + 1]).value)) {
                return false;
            }
        }
        return true;
    }));
    env.define('char-upcase', builtin('char-upcase', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: char-upcase expects exactly 1 argument`);
        }
        return { kind: 'char', value: expectCharArg(args[0]).value.toUpperCase() };
    }));
    env.define('char-downcase', builtin('char-downcase', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: char-downcase expects exactly 1 argument`);
        }
        return { kind: 'char', value: expectCharArg(args[0]).value.toLowerCase() };
    }));
    env.define('char->integer', builtin('char->integer', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: char->integer expects exactly 1 argument`);
        }
        return makeExactNumber(charCodePoint(expectCharArg(args[0]).value));
    }));
    env.define('integer->char', builtin('integer->char', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: integer->char expects exactly 1 argument`);
        }
        const codePoint = expectIntegerNumber(expectNumber(args[0]), args[0].expr);
        if (!Number.isSafeInteger(codePoint)
            || codePoint < 0
            || codePoint > 0x10ffff
            || (codePoint >= 0xd800 && codePoint <= 0xdfff)) {
            throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: invalid character code`);
        }
        return { kind: 'char', value: String.fromCodePoint(codePoint) };
    }));
    env.define('string=?', builtin('string=?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string=? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (expectStringArg(args[index]) !== expectStringArg(args[index + 1])) {
                return false;
            }
        }
        return true;
    }));
    env.define('string<?', builtin('string<?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string<? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (!(expectStringArg(args[index]) < expectStringArg(args[index + 1]))) {
                return false;
            }
        }
        return true;
    }));
    env.define('string>?', builtin('string>?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string>? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (!(expectStringArg(args[index]) > expectStringArg(args[index + 1]))) {
                return false;
            }
        }
        return true;
    }));
    env.define('string<=?', builtin('string<=?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string<=? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (!(expectStringArg(args[index]) <= expectStringArg(args[index + 1]))) {
                return false;
            }
        }
        return true;
    }));
    env.define('string>=?', builtin('string>=?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string>=? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (!(expectStringArg(args[index]) >= expectStringArg(args[index + 1]))) {
                return false;
            }
        }
        return true;
    }));
    env.define('string-ci=?', builtin('string-ci=?', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: string-ci=? expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            if (expectStringArg(args[index]).toLowerCase() !== expectStringArg(args[index + 1]).toLowerCase()) {
                return false;
            }
        }
        return true;
    }));
    env.define('string-upcase', builtin('string-upcase', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string-upcase expects exactly 1 argument`);
        }
        return makeString(expectStringArg(args[0]).toUpperCase());
    }));
    env.define('string-downcase', builtin('string-downcase', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string-downcase expects exactly 1 argument`);
        }
        return makeString(expectStringArg(args[0]).toLowerCase());
    }));
    env.define('string-copy', builtin('string-copy', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: string-copy expects exactly 1 argument`);
        }
        const value = expectStringValue(args[0]);
        return makeString(value.chars.join(''));
    }));
    env.define('list->string', builtin('list->string', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: list->string expects exactly 1 argument`);
        }
        const chars = expectProperList(args[0].value, args[0].expr).map((value) => {
            if (!isSchemeCharValue(value)) {
                throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: expected char`);
            }
            return value.value;
        });
        return makeString(chars.join(''));
    }));
    env.define('string-set!', builtin('string-set!', (args, loc) => {
        if (args.length !== 3) {
            throw new EvalError(`${loc.line}:${loc.col}: string-set! expects exactly 3 arguments`);
        }
        const value = expectStringValue(args[0]);
        const index = expectIndexArg(args[1]);
        if (!value.mutable) {
            throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: immutable string`);
        }
        if (index < 0 || index >= value.chars.length) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: string-set! index out of bounds`);
        }
        value.chars[index] = expectCharArg(args[2]).value;
        return VOID_VALUE;
    }));
    env.define('vector', builtin('vector', (args) => ({
        kind: 'vector',
        elements: args.map((arg) => arg.value),
    })));
    env.define('make-vector', builtin('make-vector', (args, loc) => {
        if (args.length !== 1 && args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: make-vector expects 1 or 2 arguments`);
        }
        const length = expectIndexArg(args[0]);
        if (length < 0) {
            throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: make-vector length out of bounds`);
        }
        const fill = args[1]?.value ?? VOID_VALUE;
        return {
            kind: 'vector',
            elements: Array.from({ length }, () => fill),
        };
    }));
    env.define('vector-ref', builtin('vector-ref', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: vector-ref expects exactly 2 arguments`);
        }
        const vector = expectVectorArg(args[0]);
        const index = expectIndexArg(args[1]);
        if (index < 0 || index >= vector.elements.length) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: vector-ref index out of bounds`);
        }
        return vector.elements[index];
    }));
    env.define('vector-set!', builtin('vector-set!', (args, loc) => {
        if (args.length !== 3) {
            throw new EvalError(`${loc.line}:${loc.col}: vector-set! expects exactly 3 arguments`);
        }
        const vector = expectVectorArg(args[0]);
        const index = expectIndexArg(args[1]);
        if (index < 0 || index >= vector.elements.length) {
            throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: vector-set! index out of bounds`);
        }
        vector.elements[index] = args[2].value;
        return VOID_VALUE;
    }));
    env.define('vector-length', builtin('vector-length', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: vector-length expects exactly 1 argument`);
        }
        return makeExactNumber(expectVectorArg(args[0]).elements.length);
    }));
    env.define('vector->list', builtin('vector->list', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: vector->list expects exactly 1 argument`);
        }
        return makeList(expectVectorArg(args[0]).elements);
    }));
    env.define('list->vector', builtin('list->vector', (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: list->vector expects exactly 1 argument`);
        }
        return {
            kind: 'vector',
            elements: expectProperList(args[0].value, args[0].expr),
        };
    }));
    return env;
}
function comparisonBuiltin(name, predicate) {
    return builtin(name, (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: ${name} expects at least 2 arguments`);
        }
        for (let index = 0; index < args.length - 1; index += 1) {
            const left = expectNumber(args[index]);
            const right = expectNumber(args[index + 1]);
            if (!predicate(left, right)) {
                return false;
            }
        }
        return true;
    });
}
function predicateBuiltin(name, predicate) {
    return builtin(name, (args, loc) => {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: ${name} expects exactly 1 argument`);
        }
        return predicate(args[0].value);
    });
}
function builtin(name, call) {
    return { kind: 'procedure', name, call };
}
function controlBuiltin(name, invoke) {
    return { kind: 'procedure', name, invoke };
}
function evaluate(expr, env, runtime) {
    return runMachine(makeEvalAction(expr, env, finishWithValue), runtime);
}
function evaluateExpr(expr, env, runtime, cont) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
            return cont(expr.value);
        case 'string':
            return cont(makeString(expr.value));
        case 'char':
            return cont({ kind: 'char', value: expr.value });
        case 'symbol':
            return cont(env.lookupSymbol(expr, expr));
        case 'list':
            return evaluateList(expr, env, runtime, cont);
    }
}
function evaluateList(expr, env, runtime, cont) {
    if (expr.elements.length === 0) {
        throw new EvalError(`${expr.line}:${expr.col}: cannot evaluate empty list`);
    }
    const [head, ...args] = expr.elements;
    if (head.type === 'symbol') {
        if (head.value === 'define-syntax') {
            return evalDefineSyntax(args, head, env, runtime, cont);
        }
        const transformer = runtime.lookupSyntaxRule(head.value);
        if (transformer !== undefined) {
            return makeEvalAction(expandMacro(transformer, expr, runtime), env, cont);
        }
        switch (head.value) {
            case 'define-record-type':
                return evalDefineRecordType(args, head, env, cont);
            case 'define':
                return evalDefine(args, head, env, runtime, cont);
            case 'set!':
                return evalSet(args, head, env, runtime, cont);
            case 'if':
                return evalIf(args, head, env, runtime, cont);
            case 'quote':
                return evalQuote(args, head, cont);
            case 'lambda':
                return evalLambda(args, head, env, cont);
            case 'case-lambda':
                return evalCaseLambda(args, head, env, cont);
            case 'and':
                return evalAnd(args, env, runtime, cont);
            case 'or':
                return evalOr(args, env, runtime, cont);
            case 'begin':
                return evalBegin(args, env, cont);
            case 'cond':
                return evalCond(args, head, env, runtime, cont);
            case 'let':
                return evalLet(args, head, env, runtime, cont);
            case 'let*':
                return evalLetStar(args, head, env, runtime, cont);
            case 'letrec':
                return evalLetrec(args, head, env, runtime, false, cont);
            case 'letrec*':
                return evalLetrec(args, head, env, runtime, true, cont);
            case 'case':
                return evalCase(args, head, env, runtime, cont);
            case 'do':
                return evalDo(args, head, env, runtime, cont);
            case 'guard':
                return evalGuard(args, head, env, runtime, cont);
        }
    }
    return makeEvalAction(head, env, (operator) => (evaluateArguments(args, env, (evaluatedArgs) => (applyProcedure(operator, evaluatedArgs, head, runtime, cont)))));
}
function evalDefineSyntax(args, head, env, runtime, cont) {
    if (args.length !== 2) {
        throw new EvalError(`${head.line}:${head.col}: define-syntax expects exactly 2 arguments`);
    }
    const nameExpr = args[0];
    if (nameExpr.type !== 'symbol') {
        throw new EvalError(`${nameExpr.line}:${nameExpr.col}: macro name must be a symbol`);
    }
    runtime.defineSyntaxRule(nameExpr.value, parseMacroTransformer(args[1], env));
    return cont(VOID_VALUE);
}
function evalDefineRecordType(args, head, env, cont) {
    if (args.length < 3) {
        throw new EvalError(`${head.line}:${head.col}: define-record-type expects a type, constructor, predicate, and fields`);
    }
    const typeName = expectSymbolExpr(args[0], 'record type name must be a symbol');
    const constructorExpr = args[1];
    if (constructorExpr.type !== 'list' || constructorExpr.elements.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: record constructor spec must be a non-empty list`);
    }
    const constructorName = expectBindableSymbol(constructorExpr.elements[0], 'record constructor name must be a symbol');
    const constructorFields = constructorExpr.elements.slice(1).map((fieldExpr) => (expectSymbolExpr(fieldExpr, 'record constructor fields must be symbols')));
    const predicateName = expectBindableSymbol(args[2], 'record predicate name must be a symbol');
    const fieldSpecs = args.slice(3).map((fieldExpr) => parseRecordFieldSpec(fieldExpr, head));
    if (constructorFields.length !== fieldSpecs.length) {
        throw new EvalError(`${head.line}:${head.col}: define-record-type constructor and field specs must match`);
    }
    const fieldIndices = new Map();
    for (let index = 0; index < constructorFields.length; index += 1) {
        const fieldName = constructorFields[index];
        if (fieldIndices.has(fieldName.value)) {
            throw new EvalError(`${fieldName.line}:${fieldName.col}: duplicate record field ${fieldName.value}`);
        }
        fieldIndices.set(fieldName.value, index);
    }
    const seenFields = new Set();
    for (const fieldSpec of fieldSpecs) {
        if (seenFields.has(fieldSpec.fieldName.value)) {
            throw new EvalError(`${fieldSpec.fieldName.line}:${fieldSpec.fieldName.col}: duplicate record field ${fieldSpec.fieldName.value}`);
        }
        if (!fieldIndices.has(fieldSpec.fieldName.value)) {
            throw new EvalError(`${fieldSpec.fieldName.line}:${fieldSpec.fieldName.col}: unknown record field ${fieldSpec.fieldName.value}`);
        }
        seenFields.add(fieldSpec.fieldName.value);
    }
    const recordType = {
        name: typeName.value,
        fieldCount: constructorFields.length,
        fieldIndices,
    };
    env.define(symbolLookupName(constructorName), builtin(constructorName.value, (callArgs, loc) => {
        if (callArgs.length !== recordType.fieldCount) {
            throw new EvalError(`${loc.line}:${loc.col}: ${constructorName.value} expects exactly ${recordType.fieldCount} arguments`);
        }
        return {
            kind: 'record',
            recordType,
            fields: callArgs.map((arg) => arg.value),
        };
    }));
    env.define(symbolLookupName(predicateName), predicateBuiltin(predicateName.value, (value) => (isRecordValue(value) && value.recordType === recordType)));
    for (const fieldSpec of fieldSpecs) {
        const fieldIndex = recordType.fieldIndices.get(fieldSpec.fieldName.value);
        if (fieldIndex === undefined) {
            throw new EvalError(`${fieldSpec.fieldName.line}:${fieldSpec.fieldName.col}: unknown record field ${fieldSpec.fieldName.value}`);
        }
        env.define(symbolLookupName(fieldSpec.accessorName), builtin(fieldSpec.accessorName.value, (callArgs, loc) => {
            if (callArgs.length !== 1) {
                throw new EvalError(`${loc.line}:${loc.col}: ${fieldSpec.accessorName.value} expects exactly 1 argument`);
            }
            return expectRecordOfType(callArgs[0], recordType).fields[fieldIndex];
        }));
    }
    return cont(VOID_VALUE);
}
function evalDefine(args, head, env, runtime, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: define expects a name and value`);
    }
    const target = args[0];
    if (target.type === 'symbol') {
        if (args.length !== 2) {
            throw new EvalError(`${head.line}:${head.col}: define expects exactly 2 arguments`);
        }
        return makeEvalAction(args[1], env, (value) => {
            env.define(symbolLookupName(target), value);
            return cont(VOID_VALUE);
        });
    }
    if (target.type !== 'list' || target.elements.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: invalid define target`);
    }
    const [nameExpr, ...paramExprs] = target.elements;
    if (nameExpr.type !== 'symbol') {
        throw new EvalError(`${nameExpr.line}:${nameExpr.col}: function name must be a symbol`);
    }
    const { params, restParam } = parseProcedureParameters(paramExprs);
    const body = args.slice(1);
    const proc = {
        kind: 'procedure',
        name: nameExpr.value,
        params,
        restParam,
        body,
        env,
    };
    env.define(symbolLookupName(nameExpr), proc);
    return cont(VOID_VALUE);
}
function evalSet(args, head, env, runtime, cont) {
    if (args.length !== 2) {
        throw new EvalError(`${head.line}:${head.col}: set! expects exactly 2 arguments`);
    }
    const target = args[0];
    if (target.type !== 'symbol') {
        throw new EvalError(`${target.line}:${target.col}: set! target must be a symbol`);
    }
    return makeEvalAction(args[1], env, (value) => {
        env.assignSymbol(target, value, target);
        return cont(VOID_VALUE);
    });
}
function evalIf(args, head, env, runtime, cont) {
    if (args.length !== 2 && args.length !== 3) {
        throw new EvalError(`${head.line}:${head.col}: if expects 2 or 3 arguments`);
    }
    return makeEvalAction(args[0], env, (condition) => {
        if (isTruthy(condition)) {
            return makeEvalAction(args[1], env, cont);
        }
        return args[2] === undefined ? cont(VOID_VALUE) : makeEvalAction(args[2], env, cont);
    });
}
function evalQuote(args, head, cont) {
    if (args.length !== 1) {
        throw new EvalError(`${head.line}:${head.col}: quote expects exactly 1 argument`);
    }
    return cont(quoteExpr(args[0]));
}
function evalLambda(args, head, env, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: lambda expects parameters and a body`);
    }
    const paramsExpr = args[0];
    if (paramsExpr.type !== 'list') {
        throw new EvalError(`${paramsExpr.line}:${paramsExpr.col}: lambda parameters must be a list`);
    }
    const { params, restParam } = parseProcedureParameters(paramsExpr.elements);
    return cont({
        kind: 'procedure',
        params,
        restParam,
        body: args.slice(1),
        env,
    });
}
function evalCaseLambda(args, head, env, cont) {
    if (args.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: case-lambda expects at least 1 clause`);
    }
    return cont({
        kind: 'procedure',
        clauses: args.map((clauseExpr) => parseCaseLambdaClause(clauseExpr, head)),
        env,
    });
}
function evalAnd(args, env, runtime, cont, index = 0) {
    if (args.length === 0) {
        return cont(true);
    }
    const arg = args[index];
    if (arg === undefined) {
        return cont(true);
    }
    if (index === args.length - 1) {
        return makeEvalAction(arg, env, cont);
    }
    return makeEvalAction(arg, env, (result) => (isTruthy(result)
        ? evalAnd(args, env, runtime, cont, index + 1)
        : cont(result)));
}
function evalOr(args, env, runtime, cont, index = 0) {
    if (args.length === 0) {
        return cont(false);
    }
    const arg = args[index];
    if (arg === undefined) {
        return cont(false);
    }
    if (index === args.length - 1) {
        return makeEvalAction(arg, env, cont);
    }
    return makeEvalAction(arg, env, (result) => (isTruthy(result)
        ? cont(result)
        : evalOr(args, env, runtime, cont, index + 1)));
}
function evalBegin(args, env, cont) {
    return makeSequenceAction(args, 0, env, cont);
}
function evalCond(args, head, env, runtime, cont, index = 0) {
    if (index >= args.length) {
        return cont(VOID_VALUE);
    }
    const clause = args[index];
    if (clause.type !== 'list' || clause.elements.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: cond clauses must be non-empty lists`);
    }
    const [testExpr, ...body] = clause.elements;
    const isElseClause = testExpr.type === 'symbol' && testExpr.value === 'else';
    if (isElseClause) {
        if (index !== args.length - 1) {
            throw new EvalError(`${testExpr.line}:${testExpr.col}: else must be the last cond clause`);
        }
        return makeSequenceAction(body, 0, env, cont);
    }
    return makeEvalAction(testExpr, env, (testValue) => {
        if (!isTruthy(testValue)) {
            return evalCond(args, head, env, runtime, cont, index + 1);
        }
        if (body.length === 0) {
            return cont(testValue);
        }
        return makeSequenceAction(body, 0, env, cont);
    });
}
function evalLet(args, head, env, runtime, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: let expects bindings and a body`);
    }
    if (args[0].type === 'symbol') {
        return evalNamedLet(args, head, env, runtime, cont);
    }
    const bindings = parseLetBindings(args[0], head);
    const body = args.slice(1);
    return evaluateExpressions(bindings.map((binding) => binding.valueExpr), env, (values) => {
        const letEnv = new Environment(env);
        for (let index = 0; index < bindings.length; index += 1) {
            letEnv.define(symbolLookupName(bindings[index].name), values[index]);
        }
        return makeSequenceAction(body, 0, letEnv, cont);
    });
}
function evalLetStar(args, head, env, runtime, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: let* expects bindings and a body`);
    }
    const bindings = parseLetBindings(args[0], head);
    const body = args.slice(1);
    const letStarEnv = new Environment(env);
    const bindNext = (index) => {
        if (index >= bindings.length) {
            return makeSequenceAction(body, 0, letStarEnv, cont);
        }
        const binding = bindings[index];
        return makeEvalAction(binding.valueExpr, letStarEnv, (value) => {
            letStarEnv.define(symbolLookupName(binding.name), value);
            return bindNext(index + 1);
        });
    };
    return bindNext(0);
}
function evalLetrec(args, head, env, runtime, sequential, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: ${head.value} expects bindings and a body`);
    }
    const bindings = parseLetBindings(args[0], head);
    const body = args.slice(1);
    const letrecEnv = new Environment(env);
    if (sequential) {
        const initSequential = (index) => {
            if (index >= bindings.length) {
                return makeSequenceAction(body, 0, letrecEnv, cont);
            }
            const binding = bindings[index];
            const cell = { value: makeUninitialized(binding.name.value) };
            letrecEnv.defineCell(symbolLookupName(binding.name), cell);
            return makeEvalAction(binding.valueExpr, letrecEnv, (value) => {
                cell.value = value;
                return initSequential(index + 1);
            });
        };
        return initSequential(0);
    }
    const cells = bindings.map((binding) => {
        const cell = { value: makeUninitialized(binding.name.value) };
        letrecEnv.defineCell(symbolLookupName(binding.name), cell);
        return cell;
    });
    const initParallel = (index) => {
        if (index >= bindings.length) {
            return makeSequenceAction(body, 0, letrecEnv, cont);
        }
        return makeEvalAction(bindings[index].valueExpr, letrecEnv, (value) => {
            cells[index].value = value;
            return initParallel(index + 1);
        });
    };
    return initParallel(0);
}
function evalNamedLet(args, head, env, runtime, cont) {
    if (args.length < 3) {
        throw new EvalError(`${head.line}:${head.col}: named let expects a name, bindings, and a body`);
    }
    const nameExpr = args[0];
    if (nameExpr.type !== 'symbol') {
        throw new EvalError(`${nameExpr.line}:${nameExpr.col}: named let name must be a symbol`);
    }
    const bindings = parseLetBindings(args[1], head);
    return evaluateArguments(bindings.map((binding) => binding.valueExpr), env, (evaluatedArgs) => {
        const letEnv = new Environment(env);
        const procedure = {
            kind: 'procedure',
            name: nameExpr.value,
            params: bindings.map((binding) => binding.name),
            body: args.slice(2),
            env: letEnv,
        };
        letEnv.define(symbolLookupName(nameExpr), procedure);
        return applyProcedure(procedure, evaluatedArgs, nameExpr, runtime, cont);
    });
}
function evalCase(args, head, env, runtime, cont) {
    if (args.length < 1) {
        throw new EvalError(`${head.line}:${head.col}: case expects a key and at least 1 clause`);
    }
    return makeEvalAction(args[0], env, (key) => {
        const checkClause = (index) => {
            if (index >= args.length - 1) {
                return cont(VOID_VALUE);
            }
            const clause = args[index + 1];
            if (clause.type !== 'list' || clause.elements.length === 0) {
                throw new EvalError(`${head.line}:${head.col}: case clauses must be non-empty lists`);
            }
            const [datumExpr, ...body] = clause.elements;
            const isElseClause = datumExpr.type === 'symbol' && datumExpr.value === 'else';
            if (isElseClause) {
                if (index + 1 !== args.length - 1) {
                    throw new EvalError(`${datumExpr.line}:${datumExpr.col}: else must be the last case clause`);
                }
                return makeSequenceAction(body, 0, env, cont);
            }
            if (datumExpr.type !== 'list') {
                throw new EvalError(`${datumExpr.line}:${datumExpr.col}: case datums must be a list`);
            }
            for (const datum of datumExpr.elements) {
                if (isEqv(key, quoteExpr(datum))) {
                    return makeSequenceAction(body, 0, env, cont);
                }
            }
            return checkClause(index + 1);
        };
        return checkClause(0);
    });
}
function evalDo(args, head, env, runtime, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: do expects bindings and a termination clause`);
    }
    const bindings = parseDoBindings(args[0], head);
    const testClause = args[1];
    if (testClause.type !== 'list' || testClause.elements.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: do termination clause must be a non-empty list`);
    }
    const [testExpr, ...resultExprs] = testClause.elements;
    const body = args.slice(2);
    return evaluateExpressions(bindings.map((binding) => binding.initExpr), env, (values) => {
        const loopEnv = new Environment(env);
        const cells = bindings.map((binding, index) => {
            const cell = { value: values[index] };
            loopEnv.defineCell(symbolLookupName(binding.name), cell);
            return { binding, cell };
        });
        return evalDoLoop(testExpr, resultExprs, body, cells, loopEnv, runtime, cont);
    });
}
function evalDoLoop(testExpr, resultExprs, body, cells, loopEnv, runtime, cont) {
    return makeEvalAction(testExpr, loopEnv, (testValue) => {
        if (isTruthy(testValue)) {
            return resultExprs.length === 0
                ? cont(VOID_VALUE)
                : makeSequenceAction(resultExprs, 0, loopEnv, cont);
        }
        return makeSequenceAction(body, 0, loopEnv, (_ignored) => (evalDoSteps(cells, loopEnv, (nextValues) => {
            for (let index = 0; index < cells.length; index += 1) {
                const nextValue = nextValues[index];
                if (nextValue !== undefined) {
                    cells[index].cell.value = nextValue;
                }
            }
            return evalDoLoop(testExpr, resultExprs, body, cells, loopEnv, runtime, cont);
        })));
    });
}
function evalDoSteps(cells, loopEnv, cont, index = 0, values = []) {
    if (index >= cells.length) {
        return cont(values);
    }
    const stepExpr = cells[index].binding.stepExpr;
    if (stepExpr === undefined) {
        return evalDoSteps(cells, loopEnv, cont, index + 1, [...values, undefined]);
    }
    return makeEvalAction(stepExpr, loopEnv, (value) => (evalDoSteps(cells, loopEnv, cont, index + 1, [...values, value])));
}
function evalGuard(args, head, env, runtime, cont) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: guard expects a clause spec and a body`);
    }
    const spec = args[0];
    if (spec.type !== 'list' || spec.elements.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: guard expects a variable and clauses`);
    }
    const frame = {
        kind: 'guard',
        variable: expectBindableSymbol(spec.elements[0], 'guard variable must be a symbol'),
        clauses: spec.elements.slice(1),
        env,
        cont,
        windStack: runtime.snapshotWindStack(),
        loc: head,
    };
    runtime.pushExceptionHandler(frame);
    return makeSequenceAction(args.slice(1), 0, env, (value) => {
        runtime.popExceptionHandlerFrame(frame);
        return cont(value);
    });
}
function evalGuardClauses(clauses, env, signal, cont, index = 0) {
    if (index >= clauses.length) {
        throw signal;
    }
    const clauseExpr = clauses[index];
    if (clauseExpr.type !== 'list' || clauseExpr.elements.length === 0) {
        throw new EvalError(`${clauseExpr.line}:${clauseExpr.col}: guard clauses must be non-empty lists`);
    }
    const [testExpr, ...body] = clauseExpr.elements;
    const isElseClause = testExpr.type === 'symbol' && testExpr.value === 'else';
    if (isElseClause) {
        if (index !== clauses.length - 1) {
            throw new EvalError(`${testExpr.line}:${testExpr.col}: else must be the last guard clause`);
        }
        return body.length === 0 ? cont(VOID_VALUE) : makeSequenceAction(body, 0, env, cont);
    }
    return makeEvalAction(testExpr, env, (testValue) => {
        if (!isTruthy(testValue)) {
            return evalGuardClauses(clauses, env, signal, cont, index + 1);
        }
        if (body.length === 0) {
            return cont(testValue);
        }
        return makeSequenceAction(body, 0, env, cont);
    });
}
function applyProcedure(operator, args, loc, runtime, cont) {
    if (!isProcedure(operator)) {
        throw new EvalError(`${loc.line}:${loc.col}: not a procedure`);
    }
    if (isContinuationProcedure(operator)) {
        if (args.length !== 1) {
            throw new EvalError(`${loc.line}:${loc.col}: continuation expects exactly 1 argument`);
        }
        return resumeContinuation(operator, args[0].value, runtime);
    }
    if (isControlBuiltinProcedure(operator)) {
        return operator.invoke(args, loc, runtime, cont);
    }
    if (isPureBuiltinProcedure(operator)) {
        return cont(operator.call(args, loc));
    }
    if (isCaseLambdaProcedure(operator)) {
        const clause = findMatchingCaseLambdaClause(operator, args.length);
        if (clause === undefined) {
            throw new EvalError(`${loc.line}:${loc.col}: ${procedureDisplayName(operator)} has no matching clause for ${args.length} arguments`);
        }
        return applyProcedureClause(operator.env, clause, args, cont);
    }
    if (!procedureClauseMatchesArity(operator, args.length)) {
        if (operator.restParam === undefined) {
            throw new EvalError(`${loc.line}:${loc.col}: ${procedureDisplayName(operator)} expects exactly ${operator.params.length} arguments`);
        }
        throw new EvalError(`${loc.line}:${loc.col}: ${procedureDisplayName(operator)} expects at least ${operator.params.length} arguments`);
    }
    return applyProcedureClause(operator.env, operator, args, cont);
}
function quoteExpr(expr) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
            return expr.value;
        case 'string':
            return makeString(expr.value);
        case 'char':
            return { kind: 'char', value: expr.value };
        case 'symbol':
            return { kind: 'symbol', value: expr.value };
        case 'list':
            return quoteList(expr.elements);
    }
}
function quoteList(elements) {
    return makeList(elements.map(quoteExpr));
}
function parseLetBindings(expr, head) {
    if (expr.type !== 'list') {
        throw new EvalError(`${expr.line}:${expr.col}: let bindings must be a list`);
    }
    return expr.elements.map((bindingExpr) => {
        if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError(`${head.line}:${head.col}: let bindings must be pairs`);
        }
        const [nameExpr, valueExpr] = bindingExpr.elements;
        if (nameExpr.type !== 'symbol' || nameExpr.capturedCell !== undefined) {
            throw new EvalError(`${nameExpr.line}:${nameExpr.col}: let binding name must be a symbol`);
        }
        return { name: nameExpr, valueExpr };
    });
}
function parseDoBindings(expr, head) {
    if (expr.type !== 'list') {
        throw new EvalError(`${expr.line}:${expr.col}: do bindings must be a list`);
    }
    return expr.elements.map((bindingExpr) => {
        if (bindingExpr.type !== 'list'
            || (bindingExpr.elements.length !== 2 && bindingExpr.elements.length !== 3)) {
            throw new EvalError(`${head.line}:${head.col}: do bindings must have 2 or 3 parts`);
        }
        const [nameExpr, initExpr, stepExpr] = bindingExpr.elements;
        if (nameExpr.type !== 'symbol' || nameExpr.capturedCell !== undefined) {
            throw new EvalError(`${nameExpr.line}:${nameExpr.col}: do binding name must be a symbol`);
        }
        return { name: nameExpr, initExpr, stepExpr };
    });
}
function parseRecordFieldSpec(expr, head) {
    if (expr.type !== 'list' || expr.elements.length !== 2) {
        throw new EvalError(`${head.line}:${head.col}: define-record-type field specs must be pairs`);
    }
    return {
        fieldName: expectSymbolExpr(expr.elements[0], 'record field name must be a symbol'),
        accessorName: expectBindableSymbol(expr.elements[1], 'record accessor name must be a symbol'),
    };
}
function parseCaseLambdaClause(expr, head) {
    if (expr.type !== 'list' || expr.elements.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: case-lambda clauses must be non-empty lists`);
    }
    const [paramsExpr, ...body] = expr.elements;
    if (paramsExpr.type !== 'list') {
        throw new EvalError(`${paramsExpr.line}:${paramsExpr.col}: case-lambda parameters must be a list`);
    }
    return {
        ...parseProcedureParameters(paramsExpr.elements),
        body,
    };
}
function evaluateSequence(exprs, env, runtime) {
    return runMachine(makeSequenceAction(exprs, 0, env, finishWithValue), runtime);
}
function stepSequence(exprs, index, env, runtime, cont) {
    if (index >= exprs.length) {
        return cont(VOID_VALUE);
    }
    if (index === exprs.length - 1) {
        return makeEvalAction(exprs[index], env, cont);
    }
    return makeEvalAction(exprs[index], env, (_ignored) => (makeSequenceAction(exprs, index + 1, env, cont)));
}
function evaluateArguments(exprs, env, cont, index = exprs.length - 1, evaluated = []) {
    if (index < 0) {
        return cont(evaluated);
    }
    const expr = exprs[index];
    return makeEvalAction(expr, env, (value) => (evaluateArguments(exprs, env, cont, index - 1, [{ expr, value }, ...evaluated])));
}
function evaluateExpressions(exprs, env, cont, index = 0, values = []) {
    if (index >= exprs.length) {
        return cont(values);
    }
    const expr = exprs[index];
    return makeEvalAction(expr, env, (value) => (evaluateExpressions(exprs, env, cont, index + 1, [...values, value])));
}
function invokeThunk(thunk, loc, runtime, cont) {
    return applyProcedure(thunk, [], loc, runtime, cont);
}
function resumeContinuation(continuation, value, runtime) {
    return switchWindFrames(continuation.windStack, runtime, () => {
        runtime.setExceptionHandlers(continuation.handlerStack);
        return continuation.resume(value);
    });
}
function dispatchRaisedSignal(signal, runtime) {
    const frame = runtime.popExceptionHandler();
    if (frame === undefined) {
        throw new EvalError(`${signal.loc.line}:${signal.loc.col}: uncaught exception: ${formatDisplayValue(signal.value)}`);
    }
    return switchWindFrames(frame.windStack, runtime, () => {
        if (frame.kind === 'procedure') {
            return applyProcedure(frame.handler, [{ expr: plainSymbolExpr('raise', signal.loc), value: signal.value }], frame.loc, runtime, frame.cont);
        }
        const handlerEnv = new Environment(frame.env);
        handlerEnv.define(symbolLookupName(frame.variable), signal.value);
        return evalGuardClauses(frame.clauses, handlerEnv, signal, frame.cont);
    });
}
function switchWindFrames(targetStack, runtime, cont) {
    const currentStack = runtime.snapshotWindStack();
    const sharedLength = sharedWindPrefixLength(currentStack, targetStack);
    return unwindWindFrames(currentStack, sharedLength, runtime, () => (rewindWindFrames(targetStack, sharedLength, runtime, () => {
        runtime.setWindStack(targetStack);
        return cont();
    })));
}
function sharedWindPrefixLength(left, right) {
    let index = 0;
    while (index < left.length && index < right.length && left[index] === right[index]) {
        index += 1;
    }
    return index;
}
function unwindWindFrames(currentStack, sharedLength, runtime, cont, index = currentStack.length - 1) {
    if (index < sharedLength) {
        runtime.setWindStack(currentStack.slice(0, sharedLength));
        return cont();
    }
    const frame = currentStack[index];
    runtime.setWindStack(currentStack.slice(0, index));
    return invokeThunk(frame.outThunk, frame.outLoc, runtime, (_ignored) => (unwindWindFrames(currentStack, sharedLength, runtime, cont, index - 1)));
}
function rewindWindFrames(targetStack, sharedLength, runtime, cont, index = sharedLength) {
    if (index >= targetStack.length) {
        runtime.setWindStack(targetStack);
        return cont();
    }
    const frame = targetStack[index];
    runtime.setWindStack(targetStack.slice(0, index));
    return invokeThunk(frame.inThunk, frame.inLoc, runtime, (_ignored) => {
        runtime.setWindStack(targetStack.slice(0, index + 1));
        return rewindWindFrames(targetStack, sharedLength, runtime, cont, index + 1);
    });
}
function runMachine(initial, runtime) {
    let action = initial;
    for (;;) {
        try {
            while (action.action !== 'done') {
                action = action.action === 'eval'
                    ? evaluateExpr(action.expr, action.env, runtime, action.cont)
                    : stepSequence(action.exprs, action.index, action.env, runtime, action.cont);
            }
            return action.value;
        }
        catch (error) {
            if (error instanceof RaisedSignal) {
                action = dispatchRaisedSignal(error, runtime);
                continue;
            }
            throw error;
        }
    }
}
function finishWithValue(value) {
    return { action: 'done', value };
}
function makeEvalAction(expr, env, cont) {
    return { action: 'eval', expr, env, cont };
}
function makeSequenceAction(exprs, index, env, cont) {
    return { action: 'sequence', exprs, index, env, cont };
}
function makeList(elements) {
    let result = EMPTY_LIST;
    for (let index = elements.length - 1; index >= 0; index -= 1) {
        result = { kind: 'pair', car: elements[index], cdr: result };
    }
    return result;
}
function parseMacroTransformer(expr, definitionEnv) {
    if (expr.type !== 'list' || expr.elements.length < 2 || exprSymbolName(expr.elements[0]) !== 'syntax-rules') {
        throw new EvalError(`${expr.line}:${expr.col}: define-syntax expects a syntax-rules form`);
    }
    const literalExpr = expr.elements[1];
    if (literalExpr.type !== 'list') {
        throw new EvalError(`${literalExpr.line}:${literalExpr.col}: syntax-rules literals must be a list`);
    }
    const literals = new Set();
    for (const literal of literalExpr.elements) {
        if (literal.type !== 'symbol') {
            throw new EvalError(`${literal.line}:${literal.col}: syntax-rules literals must be symbols`);
        }
        literals.add(literal.value);
    }
    const rules = [];
    for (const ruleExpr of expr.elements.slice(2)) {
        if (ruleExpr.type !== 'list' || ruleExpr.elements.length !== 2) {
            throw new EvalError(`${ruleExpr.line}:${ruleExpr.col}: syntax-rules clauses must be pairs`);
        }
        rules.push({
            pattern: ruleExpr.elements[0],
            template: ruleExpr.elements[1],
        });
    }
    if (rules.length === 0) {
        throw new EvalError(`${expr.line}:${expr.col}: syntax-rules requires at least one rule`);
    }
    return { literals, rules, definitionEnv };
}
function expandMacro(transformer, invocation, runtime) {
    for (const rule of transformer.rules) {
        const bindings = matchMacroRule(rule, invocation, transformer.literals);
        if (bindings !== undefined) {
            const expanded = expandTemplate(rule.template, bindings);
            return hygienizeExpr(expanded, new Map(), transformer.definitionEnv, runtime);
        }
    }
    throw new EvalError(`${invocation.line}:${invocation.col}: no matching syntax-rules clause`);
}
function matchMacroRule(rule, invocation, literals) {
    const patternItems = exprList(rule.pattern);
    const invocationItems = exprList(invocation);
    if (patternItems === undefined || invocationItems === undefined) {
        return undefined;
    }
    if (patternItems.length === 0 || invocationItems.length === 0) {
        return undefined;
    }
    if (exprSymbolName(patternItems[0]) !== exprSymbolName(invocationItems[0])) {
        return undefined;
    }
    const bindings = new Map();
    return matchPatternList(patternItems.slice(1), invocationItems.slice(1), literals, bindings)
        ? bindings
        : undefined;
}
function matchPatternList(patterns, data, literals, bindings) {
    if (patterns.length === 0) {
        return data.length === 0;
    }
    if (patterns.length >= 2 && isEllipsis(patterns[1])) {
        for (let repeatCount = 0; repeatCount <= data.length; repeatCount += 1) {
            const trial = cloneMacroBindings(bindings);
            let matched = true;
            if (!initializeRepeatedBinding(patterns[0], literals, trial)) {
                continue;
            }
            for (const datum of data.slice(0, repeatCount)) {
                if (!matchPatternRepeated(patterns[0], datum, literals, trial)) {
                    matched = false;
                    break;
                }
            }
            if (matched &&
                matchPatternList(patterns.slice(2), data.slice(repeatCount), literals, trial)) {
                bindings.clear();
                for (const [name, binding] of trial) {
                    bindings.set(name, binding);
                }
                return true;
            }
        }
        return false;
    }
    const [datum, ...rest] = data;
    return datum !== undefined
        && matchPatternOnce(patterns[0], datum, literals, bindings)
        && matchPatternList(patterns.slice(1), rest, literals, bindings);
}
function matchPatternOnce(pattern, datum, literals, bindings) {
    switch (pattern.type) {
        case 'number':
            return datum.type === 'number' && numbersEqual(pattern.value, datum.value);
        case 'boolean':
        case 'string':
        case 'char':
            return pattern.type === datum.type && pattern.value === datum.value;
        case 'symbol':
            if (literals.has(pattern.value)) {
                return datum.type === 'symbol' && datum.value === pattern.value;
            }
            return bindMacroValue(pattern.value, { kind: 'single', expr: datum }, bindings);
        case 'list':
            return datum.type === 'list'
                && matchPatternList(pattern.elements, datum.elements, literals, bindings);
    }
}
function matchPatternRepeated(pattern, datum, literals, bindings) {
    if (pattern.type === 'symbol' && !literals.has(pattern.value)) {
        return bindMacroValue(pattern.value, { kind: 'repeated', exprs: [datum] }, bindings);
    }
    return matchPatternOnce(pattern, datum, literals, bindings);
}
function initializeRepeatedBinding(pattern, literals, bindings) {
    if (pattern.type !== 'symbol' || literals.has(pattern.value)) {
        return true;
    }
    const existing = bindings.get(pattern.value);
    if (existing === undefined) {
        bindings.set(pattern.value, { kind: 'repeated', exprs: [] });
        return true;
    }
    return existing.kind === 'repeated';
}
function bindMacroValue(name, value, bindings) {
    const existing = bindings.get(name);
    if (existing === undefined) {
        bindings.set(name, value);
        return true;
    }
    if (existing.kind === 'single' && value.kind === 'single') {
        return syntaxEq(existing.expr, value.expr);
    }
    if (existing.kind === 'repeated' && value.kind === 'repeated') {
        existing.exprs.push(...value.exprs);
        return true;
    }
    return false;
}
function cloneMacroBindings(bindings) {
    const cloned = new Map();
    for (const [name, binding] of bindings) {
        cloned.set(name, binding.kind === 'single'
            ? binding
            : { kind: 'repeated', exprs: [...binding.exprs] });
    }
    return cloned;
}
function syntaxEq(left, right) {
    if (left.type !== right.type) {
        return false;
    }
    switch (left.type) {
        case 'number':
            return right.type === 'number' && numbersEqual(left.value, right.value);
        case 'boolean':
            return right.type === 'boolean' && left.value === right.value;
        case 'string':
            return right.type === 'string' && left.value === right.value;
        case 'char':
            return right.type === 'char' && left.value === right.value;
        case 'symbol':
            return right.type === 'symbol' && left.value === right.value;
        case 'list':
            return right.type === 'list'
                && left.elements.length === right.elements.length
                && left.elements.every((element, index) => syntaxEq(element, right.elements[index]));
    }
}
function expandTemplate(template, bindings, repeatIndex) {
    switch (template.type) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
            return cloneExpr(template);
        case 'symbol': {
            const binding = bindings.get(template.value);
            if (binding === undefined) {
                return introducedSymbolExpr(template.value, template);
            }
            if (binding.kind === 'single') {
                return cloneExpr(binding.expr);
            }
            if (repeatIndex === undefined) {
                throw new EvalError(`${template.line}:${template.col}: template variable ${template.value} requires ellipsis`);
            }
            const repeated = binding.exprs[repeatIndex];
            if (repeated === undefined) {
                throw new EvalError(`${template.line}:${template.col}: missing repetition for template variable ${template.value}`);
            }
            return cloneExpr(repeated);
        }
        case 'list': {
            const expanded = [];
            for (let index = 0; index < template.elements.length; index += 1) {
                if (index + 1 < template.elements.length && isEllipsis(template.elements[index + 1])) {
                    const repeatCount = templateRepeatCount(template.elements[index], bindings);
                    for (let repeatedIndex = 0; repeatedIndex < repeatCount; repeatedIndex += 1) {
                        expanded.push(expandTemplate(template.elements[index], bindings, repeatedIndex));
                    }
                    index += 1;
                    continue;
                }
                expanded.push(expandTemplate(template.elements[index], bindings, repeatIndex));
            }
            return {
                type: 'list',
                elements: expanded,
                line: template.line,
                col: template.col,
            };
        }
    }
}
function templateRepeatCount(template, bindings) {
    const count = { value: undefined };
    collectTemplateRepeatCount(template, bindings, count);
    if (count.value === undefined) {
        throw new EvalError(`${template.line}:${template.col}: ellipsis template must reference a repeated pattern`);
    }
    return count.value;
}
function collectTemplateRepeatCount(template, bindings, count) {
    if (template.type === 'symbol') {
        const binding = bindings.get(template.value);
        if (binding?.kind === 'repeated') {
            if (count.value !== undefined && count.value !== binding.exprs.length) {
                throw new EvalError(`${template.line}:${template.col}: mismatched ellipsis lengths`);
            }
            count.value = binding.exprs.length;
        }
        return;
    }
    if (template.type !== 'list') {
        return;
    }
    for (const element of template.elements) {
        if (isEllipsis(element)) {
            continue;
        }
        collectTemplateRepeatCount(element, bindings, count);
    }
}
function hygienizeExpr(expr, scope, definitionEnv, runtime) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
            return cloneExpr(expr);
        case 'symbol':
            return hygienizeSymbolExpr(expr, scope, definitionEnv, runtime);
        case 'list':
            return hygienizeList(expr, scope, definitionEnv, runtime);
    }
}
function hygienizeList(expr, scope, definitionEnv, runtime) {
    if (expr.elements.length === 0) {
        return cloneExpr(expr);
    }
    const headName = exprSymbolName(expr.elements[0]);
    if (headName === 'quote' && expr.elements.length === 2) {
        return {
            type: 'list',
            elements: [plainSymbolExpr('quote', expr.elements[0]), cloneExpr(expr.elements[1])],
            line: expr.line,
            col: expr.col,
        };
    }
    if (headName === 'lambda' && expr.elements.length >= 3 && expr.elements[1].type === 'list') {
        const bodyScope = new Map(scope);
        const params = expr.elements[1].elements.map((param) => {
            if (exprSymbolName(param) === '.') {
                return plainSymbolExpr('.', param);
            }
            return hygienizeBindingIdentifier(param, bodyScope, runtime);
        });
        return {
            type: 'list',
            elements: [
                plainSymbolExpr('lambda', expr.elements[0]),
                {
                    type: 'list',
                    elements: params,
                    line: expr.elements[1].line,
                    col: expr.elements[1].col,
                },
                ...expr.elements.slice(2).map((element) => (hygienizeExpr(element, bodyScope, definitionEnv, runtime))),
            ],
            line: expr.line,
            col: expr.col,
        };
    }
    if (headName === 'case-lambda' && expr.elements.length >= 2) {
        const clauses = expr.elements.slice(1).map((clause) => {
            if (clause.type !== 'list' || clause.elements.length < 2 || clause.elements[0].type !== 'list') {
                return hygienizeExpr(clause, scope, definitionEnv, runtime);
            }
            const bodyScope = new Map(scope);
            const params = clause.elements[0].elements.map((param) => {
                if (exprSymbolName(param) === '.') {
                    return plainSymbolExpr('.', param);
                }
                return hygienizeBindingIdentifier(param, bodyScope, runtime);
            });
            return {
                type: 'list',
                elements: [
                    {
                        type: 'list',
                        elements: params,
                        line: clause.elements[0].line,
                        col: clause.elements[0].col,
                    },
                    ...clause.elements.slice(1).map((element) => (hygienizeExpr(element, bodyScope, definitionEnv, runtime))),
                ],
                line: clause.line,
                col: clause.col,
            };
        });
        return {
            type: 'list',
            elements: [plainSymbolExpr('case-lambda', expr.elements[0]), ...clauses],
            line: expr.line,
            col: expr.col,
        };
    }
    if ((headName === 'let' || headName === 'let*' || headName === 'letrec' || headName === 'letrec*')
        && expr.elements.length >= 3) {
        if (expr.elements[1].type === 'list') {
            const bodyScope = new Map(scope);
            const bindings = [];
            const recursiveBindings = headName === 'letrec' || headName === 'letrec*';
            const sequentialBindings = headName === 'let*';
            if (recursiveBindings) {
                for (const binding of expr.elements[1].elements) {
                    if (binding.type !== 'list' || binding.elements.length !== 2) {
                        return {
                            type: 'list',
                            elements: expr.elements.map((element) => (hygienizeExpr(element, scope, definitionEnv, runtime))),
                            line: expr.line,
                            col: expr.col,
                        };
                    }
                    bindings.push({
                        type: 'list',
                        elements: [
                            hygienizeBindingIdentifier(binding.elements[0], bodyScope, runtime),
                            binding.elements[1],
                        ],
                        line: binding.line,
                        col: binding.col,
                    });
                }
                const rewrittenBindings = bindings.map((binding) => ({
                    type: 'list',
                    elements: [
                        binding.type === 'list' ? binding.elements[0] : binding,
                        hygienizeExpr(binding.type === 'list' ? binding.elements[1] : binding, bodyScope, definitionEnv, runtime),
                    ],
                    line: binding.line,
                    col: binding.col,
                }));
                return {
                    type: 'list',
                    elements: [
                        plainSymbolExpr(headName, expr.elements[0]),
                        {
                            type: 'list',
                            elements: rewrittenBindings,
                            line: expr.elements[1].line,
                            col: expr.elements[1].col,
                        },
                        ...expr.elements.slice(2).map((element) => (hygienizeExpr(element, bodyScope, definitionEnv, runtime))),
                    ],
                    line: expr.line,
                    col: expr.col,
                };
            }
            for (const binding of expr.elements[1].elements) {
                if (binding.type !== 'list' || binding.elements.length !== 2) {
                    return {
                        type: 'list',
                        elements: expr.elements.map((element) => (hygienizeExpr(element, scope, definitionEnv, runtime))),
                        line: expr.line,
                        col: expr.col,
                    };
                }
                const valueScope = sequentialBindings ? new Map(bodyScope) : scope;
                bindings.push({
                    type: 'list',
                    elements: [
                        hygienizeBindingIdentifier(binding.elements[0], bodyScope, runtime),
                        hygienizeExpr(binding.elements[1], valueScope, definitionEnv, runtime),
                    ],
                    line: binding.line,
                    col: binding.col,
                });
            }
            return {
                type: 'list',
                elements: [
                    plainSymbolExpr(headName, expr.elements[0]),
                    {
                        type: 'list',
                        elements: bindings,
                        line: expr.elements[1].line,
                        col: expr.elements[1].col,
                    },
                    ...expr.elements.slice(2).map((element) => (hygienizeExpr(element, bodyScope, definitionEnv, runtime))),
                ],
                line: expr.line,
                col: expr.col,
            };
        }
    }
    return {
        type: 'list',
        elements: expr.elements.map((element) => hygienizeExpr(element, scope, definitionEnv, runtime)),
        line: expr.line,
        col: expr.col,
    };
}
function hygienizeBindingIdentifier(expr, scope, runtime) {
    if (expr.type !== 'symbol' || !expr.introduced) {
        return cloneExpr(expr);
    }
    const lookupName = runtime.freshLookupName(expr.value);
    scope.set(expr.value, lookupName);
    return {
        type: 'symbol',
        value: expr.value,
        lookupName,
        line: expr.line,
        col: expr.col,
    };
}
function hygienizeSymbolExpr(expr, scope, definitionEnv, runtime) {
    if (!expr.introduced) {
        return { ...expr };
    }
    const scopedName = scope.get(expr.value);
    if (scopedName !== undefined) {
        return {
            type: 'symbol',
            value: expr.value,
            lookupName: scopedName,
            line: expr.line,
            col: expr.col,
        };
    }
    if (isSpecialFormName(expr.value) || runtime.hasSyntaxRule(expr.value)) {
        return plainSymbolExpr(expr.value, expr);
    }
    const capturedCell = definitionEnv.lookupPlainCell(expr.value);
    if (capturedCell !== undefined) {
        return {
            type: 'symbol',
            value: expr.value,
            capturedCell,
            line: expr.line,
            col: expr.col,
        };
    }
    return plainSymbolExpr(expr.value, expr);
}
function parseProcedureParameters(exprs) {
    const params = [];
    for (let index = 0; index < exprs.length; index += 1) {
        const expr = exprs[index];
        if (expr.type === 'symbol' && expr.value === '.') {
            const restExpr = exprs[index + 1];
            if (restExpr === undefined ||
                index + 2 !== exprs.length ||
                restExpr.type !== 'symbol' ||
                restExpr.value === '.') {
                throw new EvalError(`${expr.line}:${expr.col}: invalid parameter list`);
            }
            if (restExpr.capturedCell !== undefined) {
                throw new EvalError(`${restExpr.line}:${restExpr.col}: invalid parameter list`);
            }
            return { params, restParam: restExpr };
        }
        params.push(expectParameterSymbol(expr));
    }
    return { params };
}
function expectParameterSymbol(expr) {
    if (expr.type !== 'symbol' || expr.capturedCell !== undefined) {
        throw new EvalError(`${expr.line}:${expr.col}: parameter must be a symbol`);
    }
    return expr;
}
function symbolLookupName(symbol) {
    return symbol.lookupName ?? symbol.value;
}
function exprList(expr) {
    return expr.type === 'list' ? expr.elements : undefined;
}
function exprSymbolName(expr) {
    return expr.type === 'symbol' ? expr.value : undefined;
}
function isEllipsis(expr) {
    return expr.type === 'symbol' && expr.value === '...';
}
function plainSymbolExpr(name, loc) {
    return { type: 'symbol', value: name, ...loc };
}
function introducedSymbolExpr(name, loc) {
    return { type: 'symbol', value: name, introduced: true, ...loc };
}
function cloneExpr(expr) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
        case 'symbol':
            return { ...expr };
        case 'list':
            return {
                type: 'list',
                elements: expr.elements.map(cloneExpr),
                line: expr.line,
                col: expr.col,
            };
    }
}
function isSpecialFormName(name) {
    return [
        'define-record-type',
        'define',
        'define-syntax',
        'set!',
        'if',
        'quote',
        'lambda',
        'case-lambda',
        'and',
        'or',
        'begin',
        'cond',
        'let',
        'let*',
        'letrec',
        'letrec*',
        'case',
        'do',
        'guard',
    ].includes(name);
}
function expectSymbolExpr(expr, message) {
    if (expr.type !== 'symbol') {
        throw new EvalError(`${expr.line}:${expr.col}: ${message}`);
    }
    return expr;
}
function expectBindableSymbol(expr, message) {
    if (expr.type !== 'symbol' || expr.capturedCell !== undefined) {
        throw new EvalError(`${expr.line}:${expr.col}: ${message}`);
    }
    return expr;
}
function expectNumber(arg) {
    if (!isNumberValue(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected number`);
    }
    return arg.value;
}
function expectExactNumber(arg) {
    const value = expectNumber(arg);
    if (!value.exact) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected exact number`);
    }
    return value;
}
function expectPairArg(arg) {
    if (!isPair(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected pair`);
    }
    return arg.value;
}
function expectString(arg) {
    return schemeStringText(expectStringValue(arg));
}
function expectStringArg(arg) {
    return expectString(arg);
}
function expectStringValue(arg) {
    if (!isSchemeStringValue(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected string`);
    }
    return arg.value;
}
function expectSymbolArg(arg) {
    if (!isSchemeSymbolValue(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected symbol`);
    }
    return arg.value;
}
function expectIndexArg(arg) {
    return expectIntegerNumber(expectNumber(arg), arg.expr);
}
function expectCharArg(arg) {
    if (!isSchemeCharValue(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected char`);
    }
    return arg.value;
}
function expectVectorArg(arg) {
    if (!isVectorValue(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected vector`);
    }
    return arg.value;
}
function expectProcedureArg(arg) {
    if (!isProcedure(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: not a procedure`);
    }
    return arg.value;
}
function expectProperList(value, loc) {
    const elements = [];
    const seen = new Set();
    let current = value;
    while (isPair(current)) {
        if (seen.has(current)) {
            throw new EvalError(`${loc.line}:${loc.col}: expected proper list`);
        }
        seen.add(current);
        elements.push(current.car);
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError(`${loc.line}:${loc.col}: expected proper list`);
    }
    return elements;
}
function expectRecordOfType(arg, recordType) {
    if (!isRecordValue(arg.value) || arg.value.recordType !== recordType) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected ${recordType.name} record`);
    }
    return arg.value;
}
function isEqv(left, right) {
    return isEq(left, right);
}
function isEq(left, right) {
    if (isNumberValue(left)) {
        return isNumberValue(right) && numbersEqual(left, right);
    }
    if (typeof left === 'boolean') {
        return left === right;
    }
    if (isNumberValue(right) || typeof right === 'boolean') {
        return false;
    }
    if (isSchemeSymbolValue(left) && isSchemeSymbolValue(right)) {
        return left.value === right.value;
    }
    if (isSchemeCharValue(left) && isSchemeCharValue(right)) {
        return left.value === right.value;
    }
    if (isEmptyList(left) && isEmptyList(right)) {
        return true;
    }
    if (isSchemeStringValue(left) && isSchemeStringValue(right)) {
        return left === right;
    }
    if (isPair(left) && isPair(right)) {
        return left === right;
    }
    if (isVectorValue(left) && isVectorValue(right)) {
        return left === right;
    }
    if (isProcedure(left) && isProcedure(right)) {
        return left === right;
    }
    if (isRecordValue(left) && isRecordValue(right)) {
        return left === right;
    }
    return isVoidValue(left) && isVoidValue(right);
}
function isEqual(left, right) {
    if (isEq(left, right)) {
        return true;
    }
    if (isNumberValue(left) || typeof left === 'boolean') {
        return false;
    }
    if (isNumberValue(right) || typeof right === 'boolean') {
        return false;
    }
    if (isSchemeStringValue(left) && isSchemeStringValue(right)) {
        return schemeStringText(left) === schemeStringText(right);
    }
    if (isSchemeSymbolValue(left) && isSchemeSymbolValue(right)) {
        return left.value === right.value;
    }
    if (isSchemeCharValue(left) && isSchemeCharValue(right)) {
        return left.value === right.value;
    }
    if (isEmptyList(left) && isEmptyList(right)) {
        return true;
    }
    if (isPair(left) && isPair(right)) {
        return isEqual(left.car, right.car) && isEqual(left.cdr, right.cdr);
    }
    if (isVectorValue(left) && isVectorValue(right)) {
        return left.elements.length === right.elements.length
            && left.elements.every((element, index) => isEqual(element, right.elements[index]));
    }
    if (isProcedure(left) && isProcedure(right)) {
        return left === right;
    }
    if (isRecordValue(left) && isRecordValue(right)) {
        return left === right;
    }
    return isVoidValue(left) && isVoidValue(right);
}
function isProperList(value) {
    let slow = value;
    let fast = value;
    while (isPair(fast)) {
        fast = fast.cdr;
        if (isEmptyList(fast)) {
            return true;
        }
        if (!isPair(fast)) {
            return false;
        }
        fast = fast.cdr;
        if (!isPair(slow)) {
            return false;
        }
        slow = slow.cdr;
        if (fast === slow) {
            return false;
        }
    }
    return isEmptyList(fast);
}
function isTruthy(value) {
    return value !== false;
}
function isNumberValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'number';
}
function isExactNumberValue(value) {
    return value.exact;
}
function isProcedure(value) {
    return typeof value === 'object' && value !== null && value.kind === 'procedure';
}
function isBuiltinProcedure(value) {
    return 'call' in value || 'invoke' in value;
}
function isPureBuiltinProcedure(value) {
    return 'call' in value;
}
function isControlBuiltinProcedure(value) {
    return 'invoke' in value;
}
function isContinuationProcedure(value) {
    return 'resume' in value;
}
function isCaseLambdaProcedure(value) {
    return 'clauses' in value;
}
function isPair(value) {
    return typeof value === 'object' && value !== null && value.kind === 'pair';
}
function isVectorValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'vector';
}
function isRecordValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'record';
}
function isEmptyList(value) {
    return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}
function isVoidValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'void';
}
function isUninitializedValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'uninitialized';
}
function isSchemeStringValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'string';
}
function isSchemeSymbolValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'symbol';
}
function isSchemeCharValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'char';
}
function procedureDisplayName(proc) {
    if (proc.name !== undefined) {
        return proc.name;
    }
    return isCaseLambdaProcedure(proc) ? 'case-lambda' : 'lambda';
}
function procedureClauseMatchesArity(clause, argCount) {
    return clause.restParam === undefined
        ? argCount === clause.params.length
        : argCount >= clause.params.length;
}
function findMatchingCaseLambdaClause(proc, argCount) {
    return proc.clauses.find((clause) => procedureClauseMatchesArity(clause, argCount));
}
function applyProcedureClause(env, clause, args, cont) {
    const callEnv = new Environment(env);
    for (let index = 0; index < clause.params.length; index += 1) {
        callEnv.define(symbolLookupName(clause.params[index]), args[index].value);
    }
    if (clause.restParam !== undefined) {
        callEnv.define(symbolLookupName(clause.restParam), makeList(args.slice(clause.params.length).map((arg) => arg.value)));
    }
    return makeSequenceAction(clause.body, 0, callEnv, cont);
}
function currentBenchLevel() {
    const benchLevel = globalThis.process?.env?.BENCH_LEVEL;
    if (benchLevel === undefined) {
        return undefined;
    }
    const parsedLevel = Number.parseInt(benchLevel, 10);
    return Number.isNaN(parsedLevel) ? undefined : parsedLevel;
}
function defaultStringMutable() {
    const benchLevel = currentBenchLevel();
    return benchLevel !== undefined && benchLevel < STRING_IMMUTABILITY_LEVEL;
}
function makeString(value, mutable = defaultStringMutable()) {
    return { kind: 'string', chars: Array.from(value), mutable };
}
function makeUninitialized(name) {
    return { kind: 'uninitialized', name };
}
function schemeStringText(value) {
    return value.chars.join('');
}
function formatValue(value) {
    if (isNumberValue(value)) {
        return formatNumber(value);
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    switch (value.kind) {
        case 'string':
            return `"${escapeString(schemeStringText(value))}"`;
        case 'symbol':
            return value.value;
        case 'char':
            return formatChar(value.value);
        case 'empty-list':
            return '()';
        case 'pair':
            return formatPair(value);
        case 'vector':
            return formatVector(value, formatValue);
        case 'record':
            return `#<record ${value.recordType.name}>`;
        case 'void':
            return '#<void>';
        case 'uninitialized':
            return `#<uninitialized ${value.name}>`;
        case 'procedure':
            return '#<procedure>';
    }
}
function formatDisplayValue(value) {
    if (isNumberValue(value) || typeof value === 'boolean') {
        return formatValue(value);
    }
    switch (value.kind) {
        case 'string':
            return schemeStringText(value);
        case 'symbol':
            return value.value;
        case 'char':
            return value.value;
        case 'empty-list':
            return '()';
        case 'pair':
            return formatDisplayPair(value);
        case 'vector':
            return formatVector(value, formatDisplayValue);
        case 'record':
            return `#<record ${value.recordType.name}>`;
        case 'void':
            return '#<void>';
        case 'uninitialized':
            return `#<uninitialized ${value.name}>`;
        case 'procedure':
            return '#<procedure>';
    }
}
function formatVector(value, formatter) {
    return `#(${value.elements.map(formatter).join(' ')})`;
}
function formatPair(value) {
    const parts = [];
    let tail = value;
    while (isPair(tail)) {
        parts.push(formatValue(tail.car));
        tail = tail.cdr;
    }
    if (isEmptyList(tail)) {
        return `(${parts.join(' ')})`;
    }
    return `(${parts.join(' ')} . ${formatValue(tail)})`;
}
function formatDisplayPair(value) {
    const parts = [];
    let tail = value;
    while (isPair(tail)) {
        parts.push(formatDisplayValue(tail.car));
        tail = tail.cdr;
    }
    if (isEmptyList(tail)) {
        return `(${parts.join(' ')})`;
    }
    return `(${parts.join(' ')} . ${formatDisplayValue(tail)})`;
}
function readBindingCell(cell, name, loc) {
    if (isUninitializedValue(cell.value)) {
        throw new EvalError(`${loc.line}:${loc.col}: uninitialized variable ${name}`);
    }
    return cell.value;
}
function makeExactNumber(numerator, denominator = 1) {
    if (denominator === 0) {
        throw new EvalError('invalid rational literal');
    }
    if (Object.is(numerator, -0)) {
        numerator = 0;
    }
    if (denominator < 0) {
        numerator = -numerator;
        denominator = -denominator;
    }
    if (numerator === 0) {
        return { kind: 'number', exact: true, numerator: 0, denominator: 1 };
    }
    const gcdValue = gcd(Math.abs(numerator), denominator);
    return {
        kind: 'number',
        exact: true,
        numerator: numerator / gcdValue,
        denominator: denominator / gcdValue,
    };
}
function makeInexactNumber(value, forceDecimal = Number.isInteger(value)) {
    return {
        kind: 'number',
        exact: false,
        value: Object.is(value, -0) ? 0 : value,
        forceDecimal,
    };
}
function gcd(left, right) {
    let a = Math.abs(left);
    let b = Math.abs(right);
    while (b !== 0) {
        const next = a % b;
        a = b;
        b = next;
    }
    return a === 0 ? 1 : a;
}
function lcm(left, right) {
    if (left === 0 || right === 0) {
        return 0;
    }
    return Math.abs((left / gcd(left, right)) * right);
}
function isIntegerNumberValue(value) {
    return value.exact ? value.denominator === 1 : Number.isInteger(value.value);
}
function expectIntegerNumber(value, loc) {
    if (!isIntegerNumberValue(value)) {
        throw new EvalError(`${loc.line}:${loc.col}: expected integer`);
    }
    return value.exact ? value.numerator : value.value;
}
function numberToJs(value) {
    return value.exact ? value.numerator / value.denominator : value.value;
}
function isZeroNumber(value) {
    return value.exact ? value.numerator === 0 : value.value === 0;
}
function numbersEqual(left, right) {
    if (left.exact && right.exact) {
        return left.numerator === right.numerator && left.denominator === right.denominator;
    }
    return numberToJs(left) === numberToJs(right);
}
function compareNumbers(left, right) {
    if (left.exact && right.exact) {
        const leftScaled = left.numerator * right.denominator;
        const rightScaled = right.numerator * left.denominator;
        if (leftScaled < rightScaled) {
            return -1;
        }
        if (leftScaled > rightScaled) {
            return 1;
        }
        return 0;
    }
    const leftValue = numberToJs(left);
    const rightValue = numberToJs(right);
    if (leftValue < rightValue) {
        return -1;
    }
    if (leftValue > rightValue) {
        return 1;
    }
    return 0;
}
function addExactNumbers(left, right) {
    return makeExactNumber((left.numerator * right.denominator) + (right.numerator * left.denominator), left.denominator * right.denominator);
}
function multiplyExactNumbers(left, right) {
    return makeExactNumber(left.numerator * right.numerator, left.denominator * right.denominator);
}
function divideExactNumbers(left, right) {
    return makeExactNumber(left.numerator * right.denominator, left.denominator * right.numerator);
}
function addNumbers(values) {
    if (values.every(isExactNumberValue)) {
        let result = makeExactNumber(0);
        for (const value of values) {
            result = addExactNumbers(result, value);
        }
        return result;
    }
    let result = 0;
    for (const value of values) {
        result += numberToJs(value);
    }
    return makeInexactNumber(result);
}
function multiplyNumbers(values) {
    if (values.every(isExactNumberValue)) {
        let result = makeExactNumber(1);
        for (const value of values) {
            result = multiplyExactNumbers(result, value);
        }
        return result;
    }
    let result = 1;
    for (const value of values) {
        result *= numberToJs(value);
    }
    return makeInexactNumber(result);
}
function negateNumber(value) {
    return value.exact
        ? makeExactNumber(-value.numerator, value.denominator)
        : makeInexactNumber(-value.value, value.forceDecimal);
}
function subtractNumbers(first, rest) {
    if (first.exact && rest.every(isExactNumberValue)) {
        let result = first;
        for (const value of rest) {
            result = addExactNumbers(result, makeExactNumber(-value.numerator, value.denominator));
        }
        return result;
    }
    let result = numberToJs(first);
    for (const value of rest) {
        result -= numberToJs(value);
    }
    return makeInexactNumber(result);
}
function divideTwoNumbers(left, right) {
    if (left.exact && right.exact) {
        return divideExactNumbers(left, right);
    }
    return makeInexactNumber(numberToJs(left) / numberToJs(right));
}
function minNumbers(values) {
    if (values.every(isExactNumberValue)) {
        let result = values[0];
        for (const value of values.slice(1)) {
            if (compareNumbers(value, result) < 0) {
                result = value;
            }
        }
        return result;
    }
    let result = numberToJs(values[0]);
    for (const value of values.slice(1)) {
        const candidate = numberToJs(value);
        if (candidate < result) {
            result = candidate;
        }
    }
    return makeInexactNumber(result);
}
function maxNumbers(values) {
    if (values.every(isExactNumberValue)) {
        let result = values[0];
        for (const value of values.slice(1)) {
            if (compareNumbers(value, result) > 0) {
                result = value;
            }
        }
        return result;
    }
    let result = numberToJs(values[0]);
    for (const value of values.slice(1)) {
        const candidate = numberToJs(value);
        if (candidate > result) {
            result = candidate;
        }
    }
    return makeInexactNumber(result);
}
function absNumber(value) {
    return value.exact
        ? makeExactNumber(Math.abs(value.numerator), value.denominator)
        : makeInexactNumber(Math.abs(value.value), value.forceDecimal);
}
function exactAwarePower(base, exponent) {
    if (base.exact) {
        const absExponent = Math.abs(exponent);
        const numerator = Math.pow(base.numerator, absExponent);
        const denominator = Math.pow(base.denominator, absExponent);
        return exponent >= 0
            ? makeExactNumber(numerator, denominator)
            : makeExactNumber(denominator, numerator);
    }
    return makeInexactNumber(Math.pow(base.value, exponent));
}
function exactToInexact(value) {
    if (!value.exact) {
        return value;
    }
    return makeInexactNumber(numberToJs(value));
}
function inexactToExact(value) {
    if (value.exact) {
        return value;
    }
    return exactFromDecimalText(String(value.value));
}
function exactFromDecimalText(text) {
    const match = /^([+-]?)(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/.exec(text);
    if (match === null) {
        throw new EvalError('invalid inexact number');
    }
    const sign = match[1] === '-' ? -1 : 1;
    const integerPart = match[2];
    const fractionPart = match[3] ?? '';
    const exponent = match[4] === undefined ? 0 : Number(match[4]);
    const digits = Number(`${integerPart}${fractionPart}` || '0');
    const scale = fractionPart.length - exponent;
    if (scale <= 0) {
        return makeExactNumber(sign * digits * (10 ** -scale));
    }
    return makeExactNumber(sign * digits, 10 ** scale);
}
function parseNumberToken(token) {
    const rationalMatch = /^([+-]?\d+)\/(\d+)$/.exec(token);
    if (rationalMatch !== null) {
        const numerator = Number(rationalMatch[1]);
        const denominator = Number(rationalMatch[2]);
        if (denominator === 0) {
            return null;
        }
        return makeExactNumber(numerator, denominator);
    }
    if (/^[+-]?\d+$/.test(token)) {
        return makeExactNumber(Number(token));
    }
    if (/^[+-]?(?:\d+\.\d+|\.\d+)$/.test(token)) {
        return makeInexactNumber(Number(token), true);
    }
    return undefined;
}
function formatNumber(value) {
    if (value.exact) {
        return value.denominator === 1
            ? String(value.numerator)
            : `${value.numerator}/${value.denominator}`;
    }
    const numeric = Object.is(value.value, -0) ? 0 : value.value;
    if (value.forceDecimal && Number.isInteger(numeric)) {
        return numeric.toFixed(1);
    }
    return String(numeric);
}
function formatChar(value) {
    if (value === ' ') {
        return '#\\space';
    }
    if (value === '\n') {
        return '#\\newline';
    }
    return `#\\${value}`;
}
function escapeString(value) {
    return value
        .replaceAll('\\', '\\\\')
        .replaceAll('"', '\\"')
        .replaceAll('\n', '\\n')
        .replaceAll('\t', '\\t');
}
function stringChars(value) {
    return Array.from(value);
}
function charCodePoint(value) {
    const codePoint = value.codePointAt(0);
    if (codePoint === undefined) {
        throw new EvalError('invalid character');
    }
    return codePoint;
}
function parseCharLiteral(token, loc) {
    const body = token.slice(2);
    if (body.length === 1) {
        return body;
    }
    if (body === 'space') {
        return ' ';
    }
    if (body === 'newline') {
        return '\n';
    }
    throw new EvalError(`${loc.line}:${loc.col}: invalid character literal`);
}
function parseStringNumber(value) {
    const parsed = parseNumberToken(value);
    return parsed === undefined || parsed === null ? false : parsed;
}
function isWhitespace(ch) {
    return ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r';
}
function isAlphabeticChar(value) {
    return /^[A-Za-z]$/.test(value);
}
function isNumericChar(value) {
    return /^[0-9]$/.test(value);
}
