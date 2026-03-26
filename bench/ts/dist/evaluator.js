import { EvalError } from './evalError.js';
const BUILTIN_NAMES = [
    '+',
    '-',
    '*',
    '/',
    '<',
    '>',
    '=',
    '<=',
    'not',
    'cons',
    'car',
    'cdr',
    'null?',
    'list',
    'append',
    'length',
    'string?',
    'number?',
    'boolean?',
    'pair?',
    'symbol?',
];
const NIL_VALUE = { kind: 'nil' };
const VOID_VALUE = { kind: 'void' };
const DEFAULT_SOURCE_POS = { line: 1, col: 1 };
class Environment {
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
    const evaluation = evaluateProgram(input);
    return {
        result: formatValue(evaluation.result),
        output: evaluation.output,
    };
}
function evaluateProgram(input) {
    const expressions = parseProgram(input);
    if (expressions.length === 0) {
        throw new EvalError('expected at least one expression');
    }
    const env = createGlobalEnv();
    let result = VOID_VALUE;
    for (const expr of expressions) {
        result = evaluateExpr(expr, env);
    }
    return {
        result,
        output: '',
    };
}
function createGlobalEnv() {
    const env = new Environment();
    for (const name of BUILTIN_NAMES) {
        env.define(name, { kind: 'builtin', name });
    }
    return env;
}
function parseProgram(input) {
    const tokens = tokenize(input);
    const expressions = [];
    let index = 0;
    while (index < tokens.length) {
        const parsed = parseExpr(tokens, index);
        expressions.push(parsed.expr);
        index = parsed.nextIndex;
    }
    return expressions;
}
function tokenize(input) {
    const tokens = [];
    let index = 0;
    let line = 1;
    let col = 1;
    const currentPos = () => ({ line, col });
    const advanceChar = (char) => {
        if (char === '\n') {
            line += 1;
            col = 1;
            return;
        }
        col += 1;
    };
    while (index < input.length) {
        const char = input[index];
        if (isWhitespace(char)) {
            advanceChar(char);
            index += 1;
            continue;
        }
        if (char === ';') {
            while (index < input.length && input[index] !== '\n') {
                advanceChar(input[index]);
                index += 1;
            }
            continue;
        }
        if (char === '(' || char === ')') {
            tokens.push({ kind: 'paren', value: char, pos: currentPos() });
            advanceChar(char);
            index += 1;
            continue;
        }
        if (char === "'") {
            tokens.push({ kind: 'quote', pos: currentPos() });
            advanceChar(char);
            index += 1;
            continue;
        }
        if (char === '"') {
            const pos = currentPos();
            const parsed = parseStringToken(input, index, pos);
            tokens.push({ kind: 'string', value: parsed.value, pos });
            for (let scan = index; scan < parsed.nextIndex; scan += 1) {
                advanceChar(input[scan]);
            }
            index = parsed.nextIndex;
            continue;
        }
        const pos = currentPos();
        let end = index;
        while (end < input.length && !isDelimiter(input[end])) {
            end += 1;
        }
        tokens.push({ kind: 'atom', value: input.slice(index, end), pos });
        col += end - index;
        index = end;
    }
    return tokens;
}
function parseStringToken(input, start, pos) {
    let index = start + 1;
    let value = '';
    while (index < input.length) {
        const char = input[index];
        if (char === '"') {
            return { value, nextIndex: index + 1 };
        }
        if (char === '\\') {
            index += 1;
            if (index >= input.length) {
                throw new EvalError('unterminated string literal', pos);
            }
            const escaped = input[index];
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
                case 'r':
                    value += '\r';
                    break;
                case 't':
                    value += '\t';
                    break;
                default:
                    value += escaped;
                    break;
            }
            index += 1;
            continue;
        }
        value += char;
        index += 1;
    }
    throw new EvalError('unterminated string literal', pos);
}
function parseExpr(tokens, index) {
    const token = tokens[index];
    if (token === undefined) {
        throw new EvalError('unexpected end of input');
    }
    if (token.kind === 'quote') {
        const parsed = parseExpr(tokens, index + 1);
        return {
            expr: {
                kind: 'list',
                pos: token.pos,
                elements: [
                    { kind: 'symbol', name: 'quote', pos: token.pos },
                    parsed.expr,
                ],
            },
            nextIndex: parsed.nextIndex,
        };
    }
    if (token.kind === 'paren') {
        if (token.value === ')') {
            throw new EvalError('unexpected )', token.pos);
        }
        const elements = [];
        let nextIndex = index + 1;
        while (nextIndex < tokens.length) {
            const nextToken = tokens[nextIndex];
            if (nextToken.kind === 'paren' && nextToken.value === ')') {
                return {
                    expr: { kind: 'list', pos: token.pos, elements },
                    nextIndex: nextIndex + 1,
                };
            }
            const parsed = parseExpr(tokens, nextIndex);
            elements.push(parsed.expr);
            nextIndex = parsed.nextIndex;
        }
        throw new EvalError('unterminated list', token.pos);
    }
    if (token.kind === 'string') {
        return {
            expr: { kind: 'string', value: token.value, pos: token.pos },
            nextIndex: index + 1,
        };
    }
    return {
        expr: parseAtom(token),
        nextIndex: index + 1,
    };
}
function parseAtom(token) {
    if (token.value === '#t') {
        return { kind: 'boolean', value: true, pos: token.pos };
    }
    if (token.value === '#f') {
        return { kind: 'boolean', value: false, pos: token.pos };
    }
    if (/^[+-]?\d+$/.test(token.value)) {
        return { kind: 'number', value: Number.parseInt(token.value, 10), pos: token.pos };
    }
    return { kind: 'symbol', name: token.value, pos: token.pos };
}
function evaluateExpr(expr, env) {
    try {
        switch (expr.kind) {
            case 'number':
            case 'boolean':
            case 'string':
                return expr;
            case 'symbol':
                return env.lookup(expr.name);
            case 'list':
                return evaluateList(expr.elements, env);
        }
    }
    catch (error) {
        throw attachPosition(error, expr.pos);
    }
}
function evaluateList(elements, env) {
    if (elements.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const [head, ...argExprs] = elements;
    if (head.kind === 'symbol') {
        switch (head.name) {
            case 'define':
                return evaluateDefine(argExprs, env);
            case 'if':
                return evaluateIf(argExprs, env);
            case 'quote':
                return evaluateQuote(argExprs);
            case 'lambda':
                return evaluateLambda(argExprs, env);
            case 'and':
                return evaluateAnd(argExprs, env);
            case 'or':
                return evaluateOr(argExprs, env);
            case 'begin':
                return evaluateBegin(argExprs, env);
            case 'let':
                return evaluateLet(argExprs, env);
            case 'cond':
                return evaluateCond(argExprs, env);
        }
    }
    const procedure = evaluateExpr(head, env);
    const args = argExprs.map((expr) => evaluateExpr(expr, env));
    return applyProcedure(procedure, args);
}
function evaluateDefine(argExprs, env) {
    if (argExprs.length < 2) {
        throw new EvalError('define expects a target and a value');
    }
    const [target, ...body] = argExprs;
    if (target.kind === 'symbol') {
        if (body.length !== 1) {
            throw new EvalError('define variable form expects exactly 1 value expression');
        }
        const value = evaluateExpr(body[0], env);
        env.define(target.name, value);
        return VOID_VALUE;
    }
    if (target.kind === 'list' && target.elements.length > 0) {
        const [nameExpr, ...paramExprs] = target.elements;
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('define function form expects a function name');
        }
        const params = readParameterList(paramExprs);
        const procedure = {
            kind: 'closure',
            params,
            body,
            env,
        };
        env.define(nameExpr.name, procedure);
        return VOID_VALUE;
    }
    throw new EvalError('invalid define form');
}
function evaluateIf(argExprs, env) {
    if (argExprs.length !== 3) {
        throw new EvalError('if expects exactly 3 arguments');
    }
    const condition = evaluateExpr(argExprs[0], env);
    return isTruthy(condition) ? evaluateExpr(argExprs[1], env) : evaluateExpr(argExprs[2], env);
}
function evaluateQuote(argExprs) {
    if (argExprs.length !== 1) {
        throw new EvalError('quote expects exactly 1 argument');
    }
    return quoteExpr(argExprs[0]);
}
function quoteExpr(expr) {
    switch (expr.kind) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr;
        case 'symbol':
            return { kind: 'symbol', name: expr.name };
        case 'list':
            return buildList(expr.elements.map((element) => quoteExpr(element)));
    }
}
function evaluateLambda(argExprs, env) {
    if (argExprs.length < 2) {
        throw new EvalError('lambda expects parameters and a body');
    }
    const [paramsExpr, ...body] = argExprs;
    if (paramsExpr.kind !== 'list') {
        throw new EvalError('lambda parameters must be a list');
    }
    return {
        kind: 'closure',
        params: readParameterList(paramsExpr.elements),
        body,
        env,
    };
}
function readParameterList(exprs) {
    const params = [];
    for (const expr of exprs) {
        if (expr.kind !== 'symbol') {
            throw new EvalError('parameter list must contain only symbols');
        }
        params.push(expr.name);
    }
    return params;
}
function evaluateAnd(argExprs, env) {
    let lastValue = makeBoolean(true);
    for (const expr of argExprs) {
        lastValue = evaluateExpr(expr, env);
        if (!isTruthy(lastValue)) {
            return lastValue;
        }
    }
    return lastValue;
}
function evaluateOr(argExprs, env) {
    for (const expr of argExprs) {
        const value = evaluateExpr(expr, env);
        if (isTruthy(value)) {
            return value;
        }
    }
    return makeBoolean(false);
}
function evaluateBegin(argExprs, env) {
    return evaluateSequence(argExprs, env);
}
function evaluateLet(argExprs, env) {
    if (argExprs.length < 2) {
        throw new EvalError('let expects bindings and a body');
    }
    let name;
    let bindingsExpr;
    let body;
    if (argExprs[0].kind === 'symbol') {
        if (argExprs.length < 3) {
            throw new EvalError('named let expects a name, bindings, and a body');
        }
        name = argExprs[0].name;
        bindingsExpr = argExprs[1];
        body = argExprs.slice(2);
    }
    else {
        bindingsExpr = argExprs[0];
        body = argExprs.slice(1);
    }
    const bindings = readLetBindings(bindingsExpr);
    const values = bindings.initExprs.map((expr) => evaluateExpr(expr, env));
    if (name === undefined) {
        const letEnv = new Environment(env);
        for (let index = 0; index < bindings.names.length; index += 1) {
            letEnv.define(bindings.names[index], values[index]);
        }
        return evaluateSequence(body, letEnv);
    }
    const letEnv = new Environment(env);
    const procedure = {
        kind: 'closure',
        params: bindings.names,
        body,
        env: letEnv,
    };
    letEnv.define(name, procedure);
    return applyClosure(procedure, values);
}
function readLetBindings(bindingsExpr) {
    if (bindingsExpr.kind !== 'list') {
        throw new EvalError('let bindings must be a list');
    }
    const names = [];
    const initExprs = [];
    for (const bindingExpr of bindingsExpr.elements) {
        if (bindingExpr.kind !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError('let bindings must contain (name value) pairs');
        }
        const [nameExpr, initExpr] = bindingExpr.elements;
        if (nameExpr.kind !== 'symbol') {
            throw new EvalError('let binding name must be a symbol');
        }
        names.push(nameExpr.name);
        initExprs.push(initExpr);
    }
    return { names, initExprs };
}
function evaluateCond(argExprs, env) {
    for (let index = 0; index < argExprs.length; index += 1) {
        const clauseExpr = argExprs[index];
        if (clauseExpr.kind !== 'list' || clauseExpr.elements.length === 0) {
            throw new EvalError('cond clauses must be non-empty lists');
        }
        const [testExpr, ...body] = clauseExpr.elements;
        const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';
        if (isElseClause) {
            if (index !== argExprs.length - 1) {
                throw new EvalError('cond else clause must be last');
            }
            if (body.length === 0) {
                throw new EvalError('cond else clause expects at least 1 expression');
            }
            return evaluateSequence(body, env);
        }
        const testValue = evaluateExpr(testExpr, env);
        if (!isTruthy(testValue)) {
            continue;
        }
        if (body.length === 0) {
            return testValue;
        }
        return evaluateSequence(body, env);
    }
    return VOID_VALUE;
}
function applyProcedure(procedure, args) {
    switch (procedure.kind) {
        case 'builtin':
            return applyBuiltin(procedure.name, args);
        case 'closure':
            return applyClosure(procedure, args);
        default:
            throw new EvalError('attempted to call a non-procedure');
    }
}
function applyClosure(procedure, args) {
    if (args.length !== procedure.params.length) {
        throw new EvalError(`expected ${procedure.params.length} arguments, got ${args.length}`);
    }
    const callEnv = new Environment(procedure.env);
    for (let index = 0; index < procedure.params.length; index += 1) {
        callEnv.define(procedure.params[index], args[index]);
    }
    return evaluateSequence(procedure.body, callEnv);
}
function evaluateSequence(exprs, env) {
    let result = VOID_VALUE;
    for (const expr of exprs) {
        result = evaluateExpr(expr, env);
    }
    return result;
}
function applyBuiltin(name, args) {
    switch (name) {
        case '+':
            return makeNumber(args.map((arg) => expectNumber(arg, '+')).reduce((sum, value) => sum + value, 0));
        case '-':
            return applySubtraction(args);
        case '*':
            return makeNumber(args.map((arg) => expectNumber(arg, '*')).reduce((product, value) => product * value, 1));
        case '/':
            return applyDivision(args);
        case '<':
            return applyComparison(args, '<', (left, right) => left < right);
        case '>':
            return applyComparison(args, '>', (left, right) => left > right);
        case '=':
            return applyComparison(args, '=', (left, right) => left === right);
        case '<=':
            return applyComparison(args, '<=', (left, right) => left <= right);
        case 'not':
            if (args.length !== 1) {
                throw new EvalError('not expects exactly 1 argument');
            }
            return makeBoolean(!isTruthy(args[0]));
        case 'cons':
            if (args.length !== 2) {
                throw new EvalError('cons expects exactly 2 arguments');
            }
            return { kind: 'pair', car: args[0], cdr: args[1] };
        case 'car':
            if (args.length !== 1) {
                throw new EvalError('car expects exactly 1 argument');
            }
            return expectPair(args[0], 'car').car;
        case 'cdr':
            if (args.length !== 1) {
                throw new EvalError('cdr expects exactly 1 argument');
            }
            return expectPair(args[0], 'cdr').cdr;
        case 'null?':
            if (args.length !== 1) {
                throw new EvalError('null? expects exactly 1 argument');
            }
            return makeBoolean(args[0].kind === 'nil');
        case 'list':
            return buildList(args);
        case 'append':
            return applyAppend(args);
        case 'length':
            if (args.length !== 1) {
                throw new EvalError('length expects exactly 1 argument');
            }
            return makeNumber(listLength(args[0]));
        case 'string?':
            return applyTypePredicate(args, 'string?', (value) => value.kind === 'string');
        case 'number?':
            return applyTypePredicate(args, 'number?', (value) => value.kind === 'number');
        case 'boolean?':
            return applyTypePredicate(args, 'boolean?', (value) => value.kind === 'boolean');
        case 'pair?':
            return applyTypePredicate(args, 'pair?', (value) => value.kind === 'pair');
        case 'symbol?':
            return applyTypePredicate(args, 'symbol?', (value) => value.kind === 'symbol');
    }
}
function applySubtraction(args) {
    if (args.length === 0) {
        throw new EvalError('- expects at least 1 argument');
    }
    const numbers = args.map((arg) => expectNumber(arg, '-'));
    if (numbers.length === 1) {
        return makeNumber(-numbers[0]);
    }
    const [first, ...rest] = numbers;
    return makeNumber(rest.reduce((result, value) => result - value, first));
}
function applyDivision(args) {
    if (args.length === 0) {
        throw new EvalError('/ expects at least 1 argument');
    }
    const numbers = args.map((arg) => expectNumber(arg, '/'));
    if (numbers.length === 1) {
        if (numbers[0] === 0) {
            throw new EvalError('division by zero');
        }
        return makeNumber(1 / numbers[0]);
    }
    const [first, ...rest] = numbers;
    let result = first;
    for (const value of rest) {
        if (value === 0) {
            throw new EvalError('division by zero');
        }
        result /= value;
    }
    return makeNumber(result);
}
function applyComparison(args, name, predicate) {
    if (args.length < 2) {
        throw new EvalError(`${name} expects at least 2 arguments`);
    }
    const numbers = args.map((arg) => expectNumber(arg, name));
    for (let index = 0; index < numbers.length - 1; index += 1) {
        if (!predicate(numbers[index], numbers[index + 1])) {
            return makeBoolean(false);
        }
    }
    return makeBoolean(true);
}
function expectNumber(value, procedure) {
    if (value.kind !== 'number') {
        throw new EvalError(`${procedure} expects numeric arguments`);
    }
    return value.value;
}
function expectPair(value, procedure) {
    if (value.kind !== 'pair') {
        throw new EvalError(`${procedure} expects a pair`);
    }
    return value;
}
function buildList(elements) {
    let list = NIL_VALUE;
    for (let index = elements.length - 1; index >= 0; index -= 1) {
        list = {
            kind: 'pair',
            car: elements[index],
            cdr: list,
        };
    }
    return list;
}
function applyAppend(args) {
    if (args.length === 0) {
        return NIL_VALUE;
    }
    let result = args[args.length - 1];
    for (let index = args.length - 2; index >= 0; index -= 1) {
        const elements = listToArray(args[index], 'append');
        for (let elementIndex = elements.length - 1; elementIndex >= 0; elementIndex -= 1) {
            result = {
                kind: 'pair',
                car: elements[elementIndex],
                cdr: result,
            };
        }
    }
    return result;
}
function listLength(value) {
    return listToArray(value, 'length').length;
}
function listToArray(value, procedure) {
    const elements = [];
    let current = value;
    while (current.kind === 'pair') {
        elements.push(current.car);
        current = current.cdr;
    }
    if (current.kind !== 'nil') {
        throw new EvalError(`${procedure} expects a proper list`);
    }
    return elements;
}
function applyTypePredicate(args, name, predicate) {
    if (args.length !== 1) {
        throw new EvalError(`${name} expects exactly 1 argument`);
    }
    return makeBoolean(predicate(args[0]));
}
function isTruthy(value) {
    return value.kind !== 'boolean' || value.value;
}
function makeNumber(value) {
    return {
        kind: 'number',
        pos: DEFAULT_SOURCE_POS,
        value: Object.is(value, -0) ? 0 : value,
    };
}
function makeBoolean(value) {
    return {
        kind: 'boolean',
        pos: DEFAULT_SOURCE_POS,
        value,
    };
}
function formatValue(value) {
    switch (value.kind) {
        case 'number':
            return formatNumber(value.value);
        case 'boolean':
            return value.value ? '#t' : '#f';
        case 'string':
            return `"${escapeString(value.value)}"`;
        case 'symbol':
            return value.name;
        case 'nil':
            return '()';
        case 'pair':
            return formatPair(value);
        case 'builtin':
        case 'closure':
            return '#<procedure>';
        case 'void':
            return '';
    }
}
function formatNumber(value) {
    if (Object.is(value, -0)) {
        return '0';
    }
    if (Number.isInteger(value)) {
        return value.toString(10);
    }
    return String(value);
}
function escapeString(value) {
    return value
        .replaceAll('\\', '\\\\')
        .replaceAll('"', '\\"')
        .replaceAll('\n', '\\n')
        .replaceAll('\r', '\\r')
        .replaceAll('\t', '\\t');
}
function isWhitespace(char) {
    return /\s/.test(char);
}
function isDelimiter(char) {
    return isWhitespace(char) || char === '(' || char === ')' || char === "'" || char === ';';
}
function formatPair(value) {
    const parts = [];
    let current = value;
    while (current.kind === 'pair') {
        parts.push(formatValue(current.car));
        current = current.cdr;
    }
    if (current.kind === 'nil') {
        return `(${parts.join(' ')})`;
    }
    return `(${parts.join(' ')} . ${formatValue(current)})`;
}
function attachPosition(error, pos) {
    if (error instanceof EvalError) {
        return error.withPosition(pos);
    }
    if (error instanceof Error) {
        return new EvalError(error.message, pos);
    }
    return new EvalError(String(error), pos);
}
