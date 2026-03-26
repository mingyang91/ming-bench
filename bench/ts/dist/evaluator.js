import { EvalError } from './evalError.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID_VALUE = { kind: 'void' };
class Environment {
    parent;
    bindings = new Map();
    constructor(parent) {
        this.parent = parent;
    }
    define(name, value) {
        this.bindings.set(name, value);
    }
    lookup(name, loc) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        if (this.parent !== undefined) {
            return this.parent.lookup(name, loc);
        }
        throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${name}`);
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
    const env = createGlobalEnv();
    let result = VOID_VALUE;
    for (const expr of program) {
        result = evaluate(expr, env);
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
function createGlobalEnv() {
    const env = new Environment();
    env.define('+', builtin('+', (args) => {
        let result = 0;
        for (const arg of args) {
            result += expectNumber(arg);
        }
        return result;
    }));
    env.define('*', builtin('*', (args) => {
        let result = 1;
        for (const arg of args) {
            result *= expectNumber(arg);
        }
        return result;
    }));
    env.define('-', builtin('-', (args, loc) => {
        if (args.length === 0) {
            throw new EvalError(`${loc.line}:${loc.col}: - expects at least 1 argument`);
        }
        const first = expectNumber(args[0]);
        if (args.length === 1) {
            return -first;
        }
        let result = first;
        for (const arg of args.slice(1)) {
            result -= expectNumber(arg);
        }
        return result;
    }));
    env.define('/', builtin('/', (args, loc) => {
        if (args.length < 2) {
            throw new EvalError(`${loc.line}:${loc.col}: / expects at least 2 arguments`);
        }
        let result = expectNumber(args[0]);
        for (const arg of args.slice(1)) {
            const value = expectNumber(arg);
            if (value === 0) {
                throw new EvalError(`${arg.expr.line}:${arg.expr.col}: division by zero`);
            }
            result /= value;
        }
        return result;
    }));
    env.define('<', comparisonBuiltin('<', (left, right) => left < right));
    env.define('>', comparisonBuiltin('>', (left, right) => left > right));
    env.define('=', comparisonBuiltin('=', (left, right) => left === right));
    env.define('<=', comparisonBuiltin('<=', (left, right) => left <= right));
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
        return expectProperList(args[0].value, args[0].expr).length;
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
    env.define('string?', predicateBuiltin('string?', isSchemeStringValue));
    env.define('number?', predicateBuiltin('number?', (value) => typeof value === 'number'));
    env.define('boolean?', predicateBuiltin('boolean?', (value) => typeof value === 'boolean'));
    env.define('pair?', predicateBuiltin('pair?', isPair));
    env.define('symbol?', predicateBuiltin('symbol?', isSchemeSymbolValue));
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
function evaluate(expr, env) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
            return expr.value;
        case 'string':
            return { kind: 'string', value: expr.value };
        case 'symbol':
            return env.lookup(expr.value, expr);
        case 'list':
            return evaluateList(expr, env);
    }
}
function evaluateList(expr, env) {
    if (expr.elements.length === 0) {
        throw new EvalError(`${expr.line}:${expr.col}: cannot evaluate empty list`);
    }
    const [head, ...args] = expr.elements;
    if (head.type === 'symbol') {
        switch (head.value) {
            case 'define':
                return evalDefine(args, head, env);
            case 'if':
                return evalIf(args, head, env);
            case 'quote':
                return evalQuote(args, head);
            case 'lambda':
                return evalLambda(args, head, env);
            case 'and':
                return evalAnd(args, env);
            case 'or':
                return evalOr(args, env);
            case 'begin':
                return evalBegin(args, env);
            case 'cond':
                return evalCond(args, head, env);
            case 'let':
                return evalLet(args, head, env);
        }
    }
    const operator = evaluate(head, env);
    const evaluatedArgs = args.map((arg) => ({ expr: arg, value: evaluate(arg, env) }));
    return applyProcedure(operator, evaluatedArgs, head);
}
function evalDefine(args, head, env) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: define expects a name and value`);
    }
    const target = args[0];
    if (target.type === 'symbol') {
        if (args.length !== 2) {
            throw new EvalError(`${head.line}:${head.col}: define expects exactly 2 arguments`);
        }
        env.define(target.value, evaluate(args[1], env));
        return VOID_VALUE;
    }
    if (target.type !== 'list' || target.elements.length === 0) {
        throw new EvalError(`${head.line}:${head.col}: invalid define target`);
    }
    const [nameExpr, ...paramExprs] = target.elements;
    if (nameExpr.type !== 'symbol') {
        throw new EvalError(`${nameExpr.line}:${nameExpr.col}: function name must be a symbol`);
    }
    const params = paramExprs.map(expectParameterSymbol);
    const body = args.slice(1);
    const proc = {
        kind: 'procedure',
        name: nameExpr.value,
        params,
        body,
        env,
    };
    env.define(nameExpr.value, proc);
    return VOID_VALUE;
}
function evalIf(args, head, env) {
    if (args.length !== 3) {
        throw new EvalError(`${head.line}:${head.col}: if expects exactly 3 arguments`);
    }
    const condition = evaluate(args[0], env);
    if (isTruthy(condition)) {
        return evaluate(args[1], env);
    }
    return evaluate(args[2], env);
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
    return {
        kind: 'procedure',
        params: paramsExpr.elements.map(expectParameterSymbol),
        body: args.slice(1),
        env,
    };
}
function evalAnd(args, env) {
    let result = true;
    for (const arg of args) {
        result = evaluate(arg, env);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalOr(args, env) {
    let result = false;
    for (const arg of args) {
        result = evaluate(arg, env);
        if (isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evalBegin(args, env) {
    return evaluateSequence(args, env);
}
function evalCond(args, head, env) {
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
            return evaluateSequence(body, env);
        }
        const testValue = evaluate(testExpr, env);
        if (isTruthy(testValue)) {
            if (body.length === 0) {
                return testValue;
            }
            return evaluateSequence(body, env);
        }
    }
    return VOID_VALUE;
}
function evalLet(args, head, env) {
    if (args.length < 2) {
        throw new EvalError(`${head.line}:${head.col}: let expects bindings and a body`);
    }
    if (args[0].type === 'symbol') {
        return evalNamedLet(args, head, env);
    }
    const bindings = parseLetBindings(args[0], head);
    const body = args.slice(1);
    const letEnv = new Environment(env);
    for (const binding of bindings) {
        letEnv.define(binding.name, evaluate(binding.valueExpr, env));
    }
    return evaluateSequence(body, letEnv);
}
function evalNamedLet(args, head, env) {
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
        value: evaluate(binding.valueExpr, env),
    }));
    const letEnv = new Environment(env);
    const procedure = {
        kind: 'procedure',
        name: nameExpr.value,
        params: bindings.map((binding) => binding.name),
        body: args.slice(2),
        env: letEnv,
    };
    letEnv.define(nameExpr.value, procedure);
    return applyProcedure(procedure, evaluatedArgs, nameExpr);
}
function applyProcedure(operator, args, loc) {
    if (!isProcedure(operator)) {
        throw new EvalError(`${loc.line}:${loc.col}: not a procedure`);
    }
    if (isBuiltinProcedure(operator)) {
        return operator.call(args, loc);
    }
    if (args.length !== operator.params.length) {
        throw new EvalError(`${loc.line}:${loc.col}: ${procedureDisplayName(operator)} expects exactly ${operator.params.length} arguments`);
    }
    const callEnv = new Environment(operator.env);
    for (let index = 0; index < operator.params.length; index += 1) {
        callEnv.define(operator.params[index], args[index].value);
    }
    return evaluateSequence(operator.body, callEnv);
}
function quoteExpr(expr) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
            return expr.value;
        case 'string':
            return { kind: 'string', value: expr.value };
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
        if (nameExpr.type !== 'symbol') {
            throw new EvalError(`${nameExpr.line}:${nameExpr.col}: let binding name must be a symbol`);
        }
        return { name: nameExpr.value, valueExpr };
    });
}
function evaluateSequence(exprs, env) {
    let result = VOID_VALUE;
    for (const expr of exprs) {
        result = evaluate(expr, env);
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
function expectParameterSymbol(expr) {
    if (expr.type !== 'symbol') {
        throw new EvalError(`${expr.line}:${expr.col}: parameter must be a symbol`);
    }
    return expr.value;
}
function expectNumber(arg) {
    if (typeof arg.value !== 'number') {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected number`);
    }
    return arg.value;
}
function expectPairArg(arg) {
    if (!isPair(arg.value)) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected pair`);
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
function isTruthy(value) {
    return value !== false;
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
function isEmptyList(value) {
    return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}
function isSchemeStringValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'string';
}
function isSchemeSymbolValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'symbol';
}
function procedureDisplayName(proc) {
    return proc.name ?? 'lambda';
}
function formatValue(value) {
    if (typeof value === 'number') {
        return Object.is(value, -0) ? '0' : String(value);
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    switch (value.kind) {
        case 'string':
            return `"${escapeString(value.value)}"`;
        case 'symbol':
            return value.value;
        case 'empty-list':
            return '()';
        case 'pair':
            return formatPair(value);
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
