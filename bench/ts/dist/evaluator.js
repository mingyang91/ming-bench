import { EvalError } from './evalError.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID = { kind: 'void' };
const builtins = new Map([
    ['+', builtin('+', (args) => sum(args, 0))],
    ['-', builtin('-', (args) => subtract(args))],
    ['*', builtin('*', (args) => product(args, 1))],
    ['/', builtin('/', (args) => divide(args))],
    ['<', builtin('<', (args) => compareChain('<', args, (left, right) => left < right))],
    ['>', builtin('>', (args) => compareChain('>', args, (left, right) => left > right))],
    ['=', builtin('=', (args) => compareChain('=', args, (left, right) => left === right))],
    ['<=', builtin('<=', (args) => compareChain('<=', args, (left, right) => left <= right))],
    [
        'not',
        builtin('not', (args) => {
            expectArity('not', args, 1);
            return isFalse(args[0]);
        }),
    ],
    [
        'cons',
        builtin('cons', (args) => {
            expectArity('cons', args, 2);
            return { kind: 'pair', car: args[0], cdr: args[1] };
        }),
    ],
    [
        'car',
        builtin('car', (args) => {
            expectArity('car', args, 1);
            return expectPair(args[0], 'car').car;
        }),
    ],
    [
        'cdr',
        builtin('cdr', (args) => {
            expectArity('cdr', args, 1);
            return expectPair(args[0], 'cdr').cdr;
        }),
    ],
    [
        'null?',
        builtin('null?', (args) => {
            expectArity('null?', args, 1);
            return isEmptyList(args[0]);
        }),
    ],
    ['list', builtin('list', (args) => listToPairs(args))],
    [
        'length',
        builtin('length', (args) => {
            expectArity('length', args, 1);
            return listToArray(args[0], 'length').length;
        }),
    ],
    ['append', builtin('append', (args) => appendLists(args))],
    [
        'string?',
        builtin('string?', (args) => {
            expectArity('string?', args, 1);
            return typeof args[0] === 'string';
        }),
    ],
    [
        'number?',
        builtin('number?', (args) => {
            expectArity('number?', args, 1);
            return typeof args[0] === 'number';
        }),
    ],
    [
        'boolean?',
        builtin('boolean?', (args) => {
            expectArity('boolean?', args, 1);
            return typeof args[0] === 'boolean';
        }),
    ],
    [
        'pair?',
        builtin('pair?', (args) => {
            expectArity('pair?', args, 1);
            return isPair(args[0]);
        }),
    ],
    [
        'symbol?',
        builtin('symbol?', (args) => {
            expectArity('symbol?', args, 1);
            return isSymbolValue(args[0]);
        }),
    ],
]);
export function evalStr(input) {
    const parser = new Parser(tokenize(input));
    const expressions = parser.parseProgram();
    if (expressions.length === 0) {
        throw new EvalError('expected at least one expression');
    }
    const env = createGlobalEnvironment();
    let result = VOID;
    for (const expression of expressions) {
        result = evaluate(expression, env);
    }
    return formatValue(result);
}
export function evalStrWithOutput(input) {
    return {
        result: evalStr(input),
        output: '',
    };
}
class Environment {
    bindings = new Map();
    parent;
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
function createGlobalEnvironment() {
    const env = new Environment();
    for (const [name, value] of builtins) {
        env.define(name, value);
    }
    return env;
}
function evaluate(expression, env) {
    switch (expression.kind) {
        case 'number':
        case 'boolean':
        case 'string':
            return expression.value;
        case 'symbol':
            return env.lookup(expression.name);
        case 'list':
            return evaluateList(expression.elements, env);
    }
}
function evaluateList(elements, env) {
    if (elements.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const [operatorExpr, ...argumentExprs] = elements;
    if (operatorExpr.kind === 'symbol') {
        switch (operatorExpr.name) {
            case 'and':
                return evaluateAnd(argumentExprs, env);
            case 'or':
                return evaluateOr(argumentExprs, env);
            case 'if':
                return evaluateIf(argumentExprs, env);
            case 'define':
                return evaluateDefine(argumentExprs, env);
            case 'quote':
                return evaluateQuote(argumentExprs);
            case 'lambda':
                return evaluateLambda(argumentExprs, env);
            case 'begin':
                return evaluateBegin(argumentExprs, env);
            case 'cond':
                return evaluateCond(argumentExprs, env);
            case 'let':
                return evaluateLet(argumentExprs, env);
        }
    }
    const operator = evaluate(operatorExpr, env);
    const args = argumentExprs.map((argument) => evaluate(argument, env));
    return applyProcedure(operator, args);
}
function evaluateAnd(expressions, env) {
    let result = true;
    for (const expression of expressions) {
        result = evaluate(expression, env);
        if (isFalse(result)) {
            return result;
        }
    }
    return result;
}
function evaluateOr(expressions, env) {
    let result = false;
    for (const expression of expressions) {
        result = evaluate(expression, env);
        if (!isFalse(result)) {
            return result;
        }
    }
    return result;
}
function evaluateIf(expressions, env) {
    if (expressions.length !== 3) {
        throw new EvalError(`if expected 3 argument(s), got ${expressions.length}`);
    }
    const [conditionExpr, thenExpr, elseExpr] = expressions;
    const condition = evaluate(conditionExpr, env);
    if (isFalse(condition)) {
        return evaluate(elseExpr, env);
    }
    return evaluate(thenExpr, env);
}
function evaluateDefine(expressions, env) {
    if (expressions.length < 2) {
        throw new EvalError(`define expected at least 2 argument(s), got ${expressions.length}`);
    }
    const [targetExpr, ...valueExprs] = expressions;
    if (targetExpr.kind === 'symbol') {
        if (valueExprs.length !== 1) {
            throw new EvalError(`define expected 1 value expression, got ${valueExprs.length}`);
        }
        const value = evaluate(valueExprs[0], env);
        env.define(targetExpr.name, value);
        return VOID;
    }
    if (targetExpr.kind !== 'list' || targetExpr.elements.length === 0) {
        throw new EvalError('define expected a symbol or function signature');
    }
    const [nameExpr, ...paramExprs] = targetExpr.elements;
    const name = expectSymbolExpr(nameExpr, 'define');
    const params = paramExprs.map((expr) => expectSymbolExpr(expr, 'define'));
    if (valueExprs.length === 0) {
        throw new EvalError('define expected at least one function body expression');
    }
    const closure = {
        kind: 'closure',
        name,
        params,
        body: valueExprs,
        env,
    };
    env.define(name, closure);
    return VOID;
}
function evaluateQuote(expressions) {
    if (expressions.length !== 1) {
        throw new EvalError(`quote expected 1 argument(s), got ${expressions.length}`);
    }
    return quoteExpr(expressions[0]);
}
function evaluateLambda(expressions, env) {
    if (expressions.length < 2) {
        throw new EvalError(`lambda expected at least 2 argument(s), got ${expressions.length}`);
    }
    const [paramsExpr, ...body] = expressions;
    if (paramsExpr.kind !== 'list') {
        throw new EvalError('lambda expected a parameter list');
    }
    const params = paramsExpr.elements.map((expr) => expectSymbolExpr(expr, 'lambda'));
    return {
        kind: 'closure',
        params,
        body,
        env,
    };
}
function evaluateBegin(expressions, env) {
    return evaluateSequence(expressions, env);
}
function evaluateCond(clauses, env) {
    for (let index = 0; index < clauses.length; index += 1) {
        const clause = clauses[index];
        if (clause.kind !== 'list' || clause.elements.length === 0) {
            throw new EvalError('cond expected a non-empty clause');
        }
        const [testExpr, ...bodyExprs] = clause.elements;
        const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';
        if (isElseClause) {
            if (index !== clauses.length - 1) {
                throw new EvalError('cond else clause must be last');
            }
            return evaluateSequence(bodyExprs, env);
        }
        const testValue = evaluate(testExpr, env);
        if (!isFalse(testValue)) {
            if (bodyExprs.length === 0) {
                return testValue;
            }
            return evaluateSequence(bodyExprs, env);
        }
    }
    return VOID;
}
function evaluateLet(expressions, env) {
    if (expressions.length < 2) {
        throw new EvalError(`let expected at least 2 argument(s), got ${expressions.length}`);
    }
    if (expressions[0].kind === 'symbol') {
        const [nameExpr, bindingsExpr, ...bodyExprs] = expressions;
        if (bindingsExpr === undefined || bodyExprs.length === 0) {
            throw new EvalError('let expected bindings and a body');
        }
        const bindings = parseBindings(bindingsExpr, 'let');
        const values = bindings.map((binding) => evaluate(binding.valueExpr, env));
        const letEnv = new Environment(env);
        const closure = {
            kind: 'closure',
            name: nameExpr.name,
            params: bindings.map((binding) => binding.name),
            body: bodyExprs,
            env: letEnv,
        };
        letEnv.define(nameExpr.name, closure);
        return applyProcedure(closure, values);
    }
    const [bindingsExpr, ...bodyExprs] = expressions;
    const bindings = parseBindings(bindingsExpr, 'let');
    const values = bindings.map((binding) => evaluate(binding.valueExpr, env));
    const letEnv = new Environment(env);
    for (let index = 0; index < bindings.length; index += 1) {
        letEnv.define(bindings[index].name, values[index]);
    }
    return evaluateSequence(bodyExprs, letEnv);
}
function applyProcedure(value, args) {
    if (isBuiltin(value)) {
        return value.apply(args);
    }
    if (!isClosure(value)) {
        throw new EvalError('attempted to call a non-procedure value');
    }
    if (args.length !== value.params.length) {
        throw new EvalError(`${value.name ?? 'lambda'} expected ${value.params.length} argument(s), got ${args.length}`);
    }
    const callEnv = new Environment(value.env);
    for (let index = 0; index < value.params.length; index += 1) {
        callEnv.define(value.params[index], args[index]);
    }
    return evaluateSequence(value.body, callEnv);
}
function quoteExpr(expression) {
    switch (expression.kind) {
        case 'number':
        case 'boolean':
        case 'string':
            return expression.value;
        case 'symbol':
            return { kind: 'symbol', name: expression.name };
        case 'list':
            return listToPairs(expression.elements.map((element) => quoteExpr(element)));
    }
}
function listToPairs(elements) {
    let result = EMPTY_LIST;
    for (let index = elements.length - 1; index >= 0; index -= 1) {
        result = {
            kind: 'pair',
            car: elements[index],
            cdr: result,
        };
    }
    return result;
}
function tokenize(input) {
    const tokens = [];
    let index = 0;
    while (index < input.length) {
        const char = input[index];
        if (isWhitespace(char)) {
            index += 1;
            continue;
        }
        if (char === ';') {
            while (index < input.length && input[index] !== '\n') {
                index += 1;
            }
            continue;
        }
        if (char === '(') {
            tokens.push({ kind: 'lparen' });
            index += 1;
            continue;
        }
        if (char === ')') {
            tokens.push({ kind: 'rparen' });
            index += 1;
            continue;
        }
        if (char === '\'') {
            tokens.push({ kind: 'quote' });
            index += 1;
            continue;
        }
        if (char === '"') {
            const { value, nextIndex } = readStringLiteral(input, index);
            tokens.push({ kind: 'string', value });
            index = nextIndex;
            continue;
        }
        let end = index;
        while (end < input.length) {
            const next = input[end];
            if (isWhitespace(next) || next === '(' || next === ')' || next === ';') {
                break;
            }
            end += 1;
        }
        const rawToken = input.slice(index, end);
        if (rawToken.length === 0) {
            throw new EvalError('unexpected token');
        }
        if (rawToken === '#t') {
            tokens.push({ kind: 'boolean', value: true });
        }
        else if (rawToken === '#f') {
            tokens.push({ kind: 'boolean', value: false });
        }
        else if (/^[+-]?\d+$/.test(rawToken)) {
            tokens.push({ kind: 'number', value: Number(rawToken) });
        }
        else {
            tokens.push({ kind: 'symbol', value: rawToken });
        }
        index = end;
    }
    return tokens;
}
function readStringLiteral(input, startIndex) {
    let index = startIndex + 1;
    let value = '';
    while (index < input.length) {
        const char = input[index];
        if (char === '"') {
            return { value, nextIndex: index + 1 };
        }
        if (char === '\\') {
            index += 1;
            if (index >= input.length) {
                throw new EvalError('unterminated string literal');
            }
            const escaped = input[index];
            if (escaped === 'n') {
                value += '\n';
            }
            else if (escaped === 't') {
                value += '\t';
            }
            else if (escaped === '"' || escaped === '\\') {
                value += escaped;
            }
            else {
                value += escaped;
            }
            index += 1;
            continue;
        }
        value += char;
        index += 1;
    }
    throw new EvalError('unterminated string literal');
}
class Parser {
    tokens;
    index = 0;
    constructor(tokens) {
        this.tokens = tokens;
    }
    parseProgram() {
        const expressions = [];
        while (!this.isAtEnd()) {
            expressions.push(this.parseExpr());
        }
        return expressions;
    }
    parseExpr() {
        const token = this.advance();
        if (token === undefined) {
            throw new EvalError('unexpected end of input');
        }
        switch (token.kind) {
            case 'number':
                return { kind: 'number', value: token.value };
            case 'boolean':
                return { kind: 'boolean', value: token.value };
            case 'string':
                return { kind: 'string', value: token.value };
            case 'symbol':
                return { kind: 'symbol', name: token.value };
            case 'quote':
                return {
                    kind: 'list',
                    elements: [
                        { kind: 'symbol', name: 'quote' },
                        this.parseExpr(),
                    ],
                };
            case 'lparen': {
                const elements = [];
                while (true) {
                    const next = this.peek();
                    if (next === undefined) {
                        throw new EvalError('unterminated list');
                    }
                    if (next.kind === 'rparen') {
                        this.advance();
                        return { kind: 'list', elements };
                    }
                    elements.push(this.parseExpr());
                }
            }
            case 'rparen':
                throw new EvalError('unexpected )');
        }
    }
    peek() {
        return this.tokens[this.index];
    }
    advance() {
        const token = this.tokens[this.index];
        this.index += 1;
        return token;
    }
    isAtEnd() {
        return this.index >= this.tokens.length;
    }
}
function builtin(name, apply) {
    return { kind: 'builtin', name, apply };
}
function isBuiltin(value) {
    return typeof value === 'object' && value !== null && value.kind === 'builtin';
}
function isClosure(value) {
    return typeof value === 'object' && value !== null && value.kind === 'closure';
}
function isSymbolValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'symbol';
}
function isPair(value) {
    return typeof value === 'object' && value !== null && value.kind === 'pair';
}
function isEmptyList(value) {
    return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}
function expectArity(name, args, expected) {
    if (args.length !== expected) {
        throw new EvalError(`${name} expected ${expected} argument(s), got ${args.length}`);
    }
}
function expectAtLeastArity(name, args, min) {
    if (args.length < min) {
        throw new EvalError(`${name} expected at least ${min} argument(s), got ${args.length}`);
    }
}
function expectNumber(value, name) {
    if (typeof value !== 'number') {
        throw new EvalError(`${name} expected a number`);
    }
    return value;
}
function expectPair(value, name) {
    if (!isPair(value)) {
        throw new EvalError(`${name} expected a pair`);
    }
    return value;
}
function expectSymbolExpr(expression, name) {
    if (expression.kind !== 'symbol') {
        throw new EvalError(`${name} expected a symbol`);
    }
    return expression.name;
}
function parseBindings(bindingsExpr, name) {
    if (bindingsExpr.kind !== 'list') {
        throw new EvalError(`${name} expected a bindings list`);
    }
    return bindingsExpr.elements.map((bindingExpr) => {
        if (bindingExpr.kind !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError(`${name} expected bindings of the form (name value)`);
        }
        const [nameExpr, valueExpr] = bindingExpr.elements;
        return {
            name: expectSymbolExpr(nameExpr, name),
            valueExpr,
        };
    });
}
function evaluateSequence(expressions, env) {
    let result = VOID;
    for (const expression of expressions) {
        result = evaluate(expression, env);
    }
    return result;
}
function listToArray(value, name) {
    const elements = [];
    let current = value;
    while (isPair(current)) {
        elements.push(current.car);
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError(`${name} expected a list`);
    }
    return elements;
}
function appendLists(args) {
    const elements = [];
    for (const arg of args) {
        elements.push(...listToArray(arg, 'append'));
    }
    return listToPairs(elements);
}
function sum(args, identity) {
    let total = identity;
    for (const arg of args) {
        total += expectNumber(arg, '+');
    }
    return normalizeNumber(total);
}
function subtract(args) {
    expectAtLeastArity('-', args, 1);
    if (args.length === 1) {
        return normalizeNumber(-expectNumber(args[0], '-'));
    }
    let total = expectNumber(args[0], '-');
    for (const arg of args.slice(1)) {
        total -= expectNumber(arg, '-');
    }
    return normalizeNumber(total);
}
function product(args, identity) {
    let total = identity;
    for (const arg of args) {
        total *= expectNumber(arg, '*');
    }
    return normalizeNumber(total);
}
function divide(args) {
    expectAtLeastArity('/', args, 2);
    let total = expectNumber(args[0], '/');
    for (const arg of args.slice(1)) {
        const divisor = expectNumber(arg, '/');
        if (divisor === 0) {
            throw new EvalError('division by zero');
        }
        total /= divisor;
    }
    return normalizeNumber(total);
}
function compareChain(name, args, predicate) {
    expectAtLeastArity(name, args, 2);
    const numbers = args.map((arg) => expectNumber(arg, name));
    for (let index = 0; index < numbers.length - 1; index += 1) {
        if (!predicate(numbers[index], numbers[index + 1])) {
            return false;
        }
    }
    return true;
}
function normalizeNumber(value) {
    return Object.is(value, -0) ? 0 : value;
}
function isFalse(value) {
    return value === false;
}
function isWhitespace(value) {
    return /\s/.test(value);
}
function formatValue(value) {
    if (typeof value === 'number') {
        return String(normalizeNumber(value));
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    if (typeof value === 'string') {
        return JSON.stringify(value);
    }
    if (isBuiltin(value)) {
        return `#<procedure:${value.name}>`;
    }
    if (isClosure(value)) {
        return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
    }
    if (isPair(value)) {
        return formatPair(value);
    }
    if (isEmptyList(value)) {
        return '()';
    }
    if (isSymbolValue(value)) {
        return value.name;
    }
    if (typeof value === 'object' && value !== null && value.kind === 'void') {
        return '#<void>';
    }
    throw new EvalError('cannot format value');
}
function formatPair(pair) {
    const parts = [];
    let current = pair;
    while (isPair(current)) {
        parts.push(formatValue(current.car));
        current = current.cdr;
    }
    if (isEmptyList(current)) {
        return `(${parts.join(' ')})`;
    }
    return `(${parts.join(' ')} . ${formatValue(current)})`;
}
