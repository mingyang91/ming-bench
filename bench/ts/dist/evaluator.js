import { EvalError } from './evalError.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID_VALUE = { kind: 'void' };
class Runtime {
    output = [];
    syntaxRules = new Map();
    nextUnique = 1;
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
            return cell.value;
        }
        throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${name}`);
    }
    lookupSymbol(symbol, loc) {
        if (symbol.capturedCell !== undefined) {
            return symbol.capturedCell.value;
        }
        const cell = this.lookupCell(symbolLookupName(symbol));
        if (cell !== undefined) {
            return cell.value;
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
    let result = VOID_VALUE;
    for (const expr of program) {
        result = evaluate(expr, env, runtime);
    }
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
    env.define('map', builtin('map', (args, loc) => {
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
        const results = [];
        for (let index = 0; index < expectedLength; index += 1) {
            const appliedArgs = listArgs.map((arg, listIndex) => ({
                expr: arg.expr,
                value: lists[listIndex][index],
            }));
            results.push(applyProcedure(procedure, appliedArgs, args[0].expr, runtime));
        }
        return makeList(results);
    }));
    env.define('eq?', builtin('eq?', (args, loc) => {
        if (args.length !== 2) {
            throw new EvalError(`${loc.line}:${loc.col}: eq? expects exactly 2 arguments`);
        }
        return isEq(args[0].value, args[1].value);
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
    env.define('apply', builtin('apply', (args, loc) => {
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
        return applyProcedure(procedureArg.value, appliedArgs, procedureArg.expr, runtime);
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
    env.define('string-append', builtin('string-append', (args) => ({
        kind: 'string',
        chars: Array.from(args.map(expectString).join('')),
        mutable: true,
    })));
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
        return { kind: 'string', chars: [...value.chars], mutable: true };
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
function evaluate(expr, env, runtime) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
            return expr.value;
        case 'string':
            return makeString(expr.value);
        case 'char':
            return { kind: 'char', value: expr.value };
        case 'symbol':
            return env.lookupSymbol(expr, expr);
        case 'list':
            return evaluateList(expr, env, runtime);
    }
}
function evaluateList(expr, env, runtime) {
    if (expr.elements.length === 0) {
        throw new EvalError(`${expr.line}:${expr.col}: cannot evaluate empty list`);
    }
    const [head, ...args] = expr.elements;
    if (head.type === 'symbol') {
        if (head.value === 'define-syntax') {
            return evalDefineSyntax(args, head, env, runtime);
        }
        const transformer = runtime.lookupSyntaxRule(head.value);
        if (transformer !== undefined) {
            return evaluate(expandMacro(transformer, expr, runtime), env, runtime);
        }
        switch (head.value) {
            case 'define-record-type':
                return evalDefineRecordType(args, head, env);
            case 'define':
                return evalDefine(args, head, env, runtime);
            case 'set!':
                return evalSet(args, head, env, runtime);
            case 'if':
                return evalIf(args, head, env, runtime);
            case 'quote':
                return evalQuote(args, head);
            case 'lambda':
                return evalLambda(args, head, env);
            case 'and':
                return evalAnd(args, env, runtime);
            case 'or':
                return evalOr(args, env, runtime);
            case 'begin':
                return evalBegin(args, env, runtime);
            case 'cond':
                return evalCond(args, head, env, runtime);
            case 'let':
                return evalLet(args, head, env, runtime);
        }
    }
    const operator = evaluate(head, env, runtime);
    const evaluatedArgs = args.map((arg) => ({ expr: arg, value: evaluate(arg, env, runtime) }));
    return applyProcedure(operator, evaluatedArgs, head, runtime);
}
function evalDefineSyntax(args, head, env, runtime) {
    if (args.length !== 2) {
        throw new EvalError(`${head.line}:${head.col}: define-syntax expects exactly 2 arguments`);
    }
    const nameExpr = args[0];
    if (nameExpr.type !== 'symbol') {
        throw new EvalError(`${nameExpr.line}:${nameExpr.col}: macro name must be a symbol`);
    }
    runtime.defineSyntaxRule(nameExpr.value, parseMacroTransformer(args[1], env));
    return VOID_VALUE;
}
function evalDefineRecordType(args, head, env) {
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
    return VOID_VALUE;
}
function evalDefine(args, head, env, runtime) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: define expects a name and value`);
    }
    const target = args[0];
    if (target.type === 'symbol') {
        if (args.length !== 2) {
            throw new EvalError(`${head.line}:${head.col}: define expects exactly 2 arguments`);
        }
        env.define(symbolLookupName(target), evaluate(args[1], env, runtime));
        return VOID_VALUE;
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
    return VOID_VALUE;
}
function evalSet(args, head, env, runtime) {
    if (args.length !== 2) {
        throw new EvalError(`${head.line}:${head.col}: set! expects exactly 2 arguments`);
    }
    const target = args[0];
    if (target.type !== 'symbol') {
        throw new EvalError(`${target.line}:${target.col}: set! target must be a symbol`);
    }
    env.assignSymbol(target, evaluate(args[1], env, runtime), target);
    return VOID_VALUE;
}
function evalIf(args, head, env, runtime) {
    if (args.length !== 3) {
        throw new EvalError(`${head.line}:${head.col}: if expects exactly 3 arguments`);
    }
    const condition = evaluate(args[0], env, runtime);
    if (isTruthy(condition)) {
        return evaluate(args[1], env, runtime);
    }
    return evaluate(args[2], env, runtime);
}
function evalQuote(args, head) {
    if (args.length !== 1) {
        throw new EvalError(`${head.line}:${head.col}: quote expects exactly 1 argument`);
    }
    return quoteExpr(args[0]);
}
function evalLambda(args, head, env) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: lambda expects parameters and a body`);
    }
    const paramsExpr = args[0];
    if (paramsExpr.type !== 'list') {
        throw new EvalError(`${paramsExpr.line}:${paramsExpr.col}: lambda parameters must be a list`);
    }
    const { params, restParam } = parseProcedureParameters(paramsExpr.elements);
    return {
        kind: 'procedure',
        params,
        restParam,
        body: args.slice(1),
        env,
    };
}
function evalAnd(args, env, runtime) {
    let result = true;
    for (const arg of args) {
        result = evaluate(arg, env, runtime);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalOr(args, env, runtime) {
    let result = false;
    for (const arg of args) {
        result = evaluate(arg, env, runtime);
        if (isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalBegin(args, env, runtime) {
    return evaluateSequence(args, env, runtime);
}
function evalCond(args, head, env, runtime) {
    for (let index = 0; index < args.length; index += 1) {
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
            return evaluateSequence(body, env, runtime);
        }
        const testValue = evaluate(testExpr, env, runtime);
        if (isTruthy(testValue)) {
            if (body.length === 0) {
                return testValue;
            }
            return evaluateSequence(body, env, runtime);
        }
    }
    return VOID_VALUE;
}
function evalLet(args, head, env, runtime) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: let expects bindings and a body`);
    }
    if (args[0].type === 'symbol') {
        return evalNamedLet(args, head, env, runtime);
    }
    const bindings = parseLetBindings(args[0], head);
    const body = args.slice(1);
    const letEnv = new Environment(env);
    for (const binding of bindings) {
        letEnv.define(symbolLookupName(binding.name), evaluate(binding.valueExpr, env, runtime));
    }
    return evaluateSequence(body, letEnv, runtime);
}
function evalNamedLet(args, head, env, runtime) {
    if (args.length < 3) {
        throw new EvalError(`${head.line}:${head.col}: named let expects a name, bindings, and a body`);
    }
    const nameExpr = args[0];
    if (nameExpr.type !== 'symbol') {
        throw new EvalError(`${nameExpr.line}:${nameExpr.col}: named let name must be a symbol`);
    }
    const bindings = parseLetBindings(args[1], head);
    const evaluatedArgs = bindings.map((binding) => ({
        expr: binding.valueExpr,
        value: evaluate(binding.valueExpr, env, runtime),
    }));
    const letEnv = new Environment(env);
    const procedure = {
        kind: 'procedure',
        name: nameExpr.value,
        params: bindings.map((binding) => binding.name),
        body: args.slice(2),
        env: letEnv,
    };
    letEnv.define(symbolLookupName(nameExpr), procedure);
    return applyProcedure(procedure, evaluatedArgs, nameExpr, runtime);
}
function applyProcedure(operator, args, loc, runtime) {
    if (!isProcedure(operator)) {
        throw new EvalError(`${loc.line}:${loc.col}: not a procedure`);
    }
    if (isBuiltinProcedure(operator)) {
        return operator.call(args, loc);
    }
    if (operator.restParam === undefined && args.length !== operator.params.length) {
        throw new EvalError(`${loc.line}:${loc.col}: ${procedureDisplayName(operator)} expects exactly ${operator.params.length} arguments`);
    }
    if (operator.restParam !== undefined && args.length < operator.params.length) {
        throw new EvalError(`${loc.line}:${loc.col}: ${procedureDisplayName(operator)} expects at least ${operator.params.length} arguments`);
    }
    const callEnv = new Environment(operator.env);
    for (let index = 0; index < operator.params.length; index += 1) {
        callEnv.define(symbolLookupName(operator.params[index]), args[index].value);
    }
    if (operator.restParam !== undefined) {
        callEnv.define(symbolLookupName(operator.restParam), makeList(args.slice(operator.params.length).map((arg) => arg.value)));
    }
    return evaluateSequence(operator.body, callEnv, runtime);
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
function parseRecordFieldSpec(expr, head) {
    if (expr.type !== 'list' || expr.elements.length !== 2) {
        throw new EvalError(`${head.line}:${head.col}: define-record-type field specs must be pairs`);
    }
    return {
        fieldName: expectSymbolExpr(expr.elements[0], 'record field name must be a symbol'),
        accessorName: expectBindableSymbol(expr.elements[1], 'record accessor name must be a symbol'),
    };
}
function evaluateSequence(exprs, env, runtime) {
    let result = VOID_VALUE;
    for (const expr of exprs) {
        result = evaluate(expr, env, runtime);
    }
    return result;
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
    if (headName === 'let' && expr.elements.length >= 3) {
        if (expr.elements[1].type === 'list') {
            const bodyScope = new Map(scope);
            const bindings = [];
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
                        hygienizeExpr(binding.elements[1], scope, definitionEnv, runtime),
                    ],
                    line: binding.line,
                    col: binding.col,
                });
            }
            return {
                type: 'list',
                elements: [
                    plainSymbolExpr('let', expr.elements[0]),
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
        'and',
        'or',
        'begin',
        'cond',
        'let',
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
function expectProperList(value, loc) {
    const elements = [];
    let current = value;
    while (isPair(current)) {
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
    if (isProcedure(left) && isProcedure(right)) {
        return left === right;
    }
    if (isRecordValue(left) && isRecordValue(right)) {
        return left === right;
    }
    return left.kind === 'void' && right.kind === 'void';
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
    if (isProcedure(left) && isProcedure(right)) {
        return left === right;
    }
    if (isRecordValue(left) && isRecordValue(right)) {
        return left === right;
    }
    return left.kind === 'void' && right.kind === 'void';
}
function isProperList(value) {
    let current = value;
    while (isPair(current)) {
        current = current.cdr;
    }
    return isEmptyList(current);
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
    return 'call' in value;
}
function isPair(value) {
    return typeof value === 'object' && value !== null && value.kind === 'pair';
}
function isRecordValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'record';
}
function isEmptyList(value) {
    return typeof value === 'object' && value !== null && value.kind === 'empty-list';
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
    return proc.name ?? 'lambda';
}
function makeString(value, mutable = true) {
    return { kind: 'string', chars: Array.from(value), mutable };
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
        case 'record':
            return `#<record ${value.recordType.name}>`;
        case 'void':
            return '#<void>';
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
        case 'record':
            return `#<record ${value.recordType.name}>`;
        case 'void':
            return '#<void>';
        case 'procedure':
            return '#<procedure>';
    }
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
