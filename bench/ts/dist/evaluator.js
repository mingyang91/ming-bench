import { EvalError } from './evalError.js';
const EMPTY_LIST = { kind: 'empty-list' };
const VOID = { kind: 'void' };
function createBuiltins(context) {
    return new Map([
        ['+', builtin('+', (args, pos) => sum(args, 0, pos))],
        ['-', builtin('-', (args, pos) => subtract(args, pos))],
        ['*', builtin('*', (args, pos) => product(args, 1, pos))],
        ['/', builtin('/', (args, pos) => divide(args, pos))],
        [
            '<',
            builtin('<', (args, pos) => compareChain('<', args, (left, right) => left < right, pos)),
        ],
        [
            '>',
            builtin('>', (args, pos) => compareChain('>', args, (left, right) => left > right, pos)),
        ],
        [
            '=',
            builtin('=', (args, pos) => compareChain('=', args, (left, right) => left === right, pos)),
        ],
        [
            '<=',
            builtin('<=', (args, pos) => compareChain('<=', args, (left, right) => left <= right, pos)),
        ],
        [
            'not',
            builtin('not', (args, pos) => {
                expectArity('not', args, 1, pos);
                return isFalse(args[0]);
            }),
        ],
        [
            'cons',
            builtin('cons', (args, pos) => {
                expectArity('cons', args, 2, pos);
                return { kind: 'pair', car: args[0], cdr: args[1] };
            }),
        ],
        [
            'car',
            builtin('car', (args, pos) => {
                expectArity('car', args, 1, pos);
                return expectPair(args[0], 'car', pos).car;
            }),
        ],
        [
            'cdr',
            builtin('cdr', (args, pos) => {
                expectArity('cdr', args, 1, pos);
                return expectPair(args[0], 'cdr', pos).cdr;
            }),
        ],
        [
            'null?',
            builtin('null?', (args, pos) => {
                expectArity('null?', args, 1, pos);
                return isEmptyList(args[0]);
            }),
        ],
        ['list', builtin('list', (args) => listToPairs(args))],
        [
            'length',
            builtin('length', (args, pos) => {
                expectArity('length', args, 1, pos);
                return listToArray(args[0], 'length', pos).length;
            }),
        ],
        ['append', builtin('append', (args, pos) => appendLists(args, pos))],
        [
            'string?',
            builtin('string?', (args, pos) => {
                expectArity('string?', args, 1, pos);
                return isStringValue(args[0]);
            }),
        ],
        [
            'number?',
            builtin('number?', (args, pos) => {
                expectArity('number?', args, 1, pos);
                return typeof args[0] === 'number';
            }),
        ],
        [
            'boolean?',
            builtin('boolean?', (args, pos) => {
                expectArity('boolean?', args, 1, pos);
                return typeof args[0] === 'boolean';
            }),
        ],
        [
            'pair?',
            builtin('pair?', (args, pos) => {
                expectArity('pair?', args, 1, pos);
                return isPair(args[0]);
            }),
        ],
        [
            'symbol?',
            builtin('symbol?', (args, pos) => {
                expectArity('symbol?', args, 1, pos);
                return isSymbolValue(args[0]);
            }),
        ],
        [
            'display',
            builtin('display', (args, pos) => {
                expectArity('display', args, 1, pos);
                context.output.push(formatDisplayValue(args[0]));
                return VOID;
            }),
        ],
        [
            'write',
            builtin('write', (args, pos) => {
                expectArity('write', args, 1, pos);
                context.output.push(formatValue(args[0]));
                return VOID;
            }),
        ],
        [
            'newline',
            builtin('newline', (args, pos) => {
                expectArity('newline', args, 0, pos);
                context.output.push('\n');
                return VOID;
            }),
        ],
        [
            'string-append',
            builtin('string-append', (args, pos) => makeString(args.map((arg) => expectStringContent(arg, 'string-append', pos)).join(''))),
        ],
        [
            'string-length',
            builtin('string-length', (args, pos) => {
                expectArity('string-length', args, 1, pos);
                return expectStringValue(args[0], 'string-length', pos).chars.length;
            }),
        ],
        [
            'substring',
            builtin('substring', (args, pos) => {
                expectArity('substring', args, 3, pos);
                const chars = expectStringValue(args[0], 'substring', pos).chars;
                const start = expectIndex(args[1], 'substring', pos);
                const end = expectIndex(args[2], 'substring', pos);
                if (start > end || end > chars.length) {
                    throw new EvalError('substring index out of bounds', pos);
                }
                return makeString(chars.slice(start, end));
            }),
        ],
        [
            'string->number',
            builtin('string->number', (args, pos) => {
                expectArity('string->number', args, 1, pos);
                const value = expectStringContent(args[0], 'string->number', pos);
                return /^[+-]?\d+$/.test(value) ? Number(value) : false;
            }),
        ],
        [
            'number->string',
            builtin('number->string', (args, pos) => {
                expectArity('number->string', args, 1, pos);
                return makeString(String(normalizeNumber(expectNumber(args[0], 'number->string', pos))));
            }),
        ],
        [
            'symbol->string',
            builtin('symbol->string', (args, pos) => {
                expectArity('symbol->string', args, 1, pos);
                return makeString(expectSymbolValue(args[0], 'symbol->string', pos).name);
            }),
        ],
        [
            'string->symbol',
            builtin('string->symbol', (args, pos) => {
                expectArity('string->symbol', args, 1, pos);
                return {
                    kind: 'symbol',
                    name: expectStringContent(args[0], 'string->symbol', pos),
                };
            }),
        ],
        [
            'string-ref',
            builtin('string-ref', (args, pos) => {
                expectArity('string-ref', args, 2, pos);
                const chars = expectStringValue(args[0], 'string-ref', pos).chars;
                const index = expectIndex(args[1], 'string-ref', pos);
                if (index >= chars.length) {
                    throw new EvalError('string-ref index out of bounds', pos);
                }
                return { kind: 'char', value: chars[index] };
            }),
        ],
        [
            'string-copy',
            builtin('string-copy', (args, pos) => {
                expectArity('string-copy', args, 1, pos);
                return makeString(expectStringValue(args[0], 'string-copy', pos).chars, true);
            }),
        ],
        [
            'string-set!',
            builtin('string-set!', (args, pos) => {
                expectArity('string-set!', args, 3, pos);
                const target = expectMutableString(args[0], 'string-set!', pos);
                const index = expectIndex(args[1], 'string-set!', pos);
                const char = expectChar(args[2], 'string-set!', pos);
                if (index >= target.chars.length) {
                    throw new EvalError('string-set! index out of bounds', pos);
                }
                target.chars[index] = char.value;
                return VOID;
            }),
        ],
        [
            'char?',
            builtin('char?', (args, pos) => {
                expectArity('char?', args, 1, pos);
                return isChar(args[0]);
            }),
        ],
    ]);
}
export function evalStr(input) {
    return evaluateInput(input).result;
}
export function evalStrWithOutput(input) {
    return evaluateInput(input);
}
function evaluateInput(input) {
    const parser = new Parser(tokenize(input));
    const expressions = parser.parseProgram();
    if (expressions.length === 0) {
        throw new EvalError('expected at least one expression', { line: 1, col: 1 });
    }
    const context = { output: [] };
    const env = createGlobalEnvironment(context);
    let result = VOID;
    for (const expression of expressions) {
        result = evaluate(expression, env);
    }
    return {
        result: formatValue(result),
        output: context.output.join(''),
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
    lookup(name, pos) {
        if (this.bindings.has(name)) {
            return this.bindings.get(name);
        }
        if (this.parent !== undefined) {
            return this.parent.lookup(name, pos);
        }
        throw new EvalError(`unbound symbol: ${name}`, pos);
    }
}
function createGlobalEnvironment(context) {
    const env = new Environment();
    for (const [name, value] of createBuiltins(context)) {
        env.define(name, value);
    }
    return env;
}
function evaluate(expression, env) {
    try {
        switch (expression.kind) {
            case 'number':
            case 'boolean':
                return expression.value;
            case 'string':
                return makeString(expression.value);
            case 'char':
                return { kind: 'char', value: expression.value };
            case 'symbol':
                return env.lookup(expression.name, expression.pos);
            case 'list':
                return evaluateList(expression.elements, env, expression.pos);
        }
    }
    catch (error) {
        throw errorWithPosition(error, expression.pos);
    }
}
function evaluateList(elements, env, pos) {
    if (elements.length === 0) {
        throw new EvalError('cannot evaluate empty list', pos);
    }
    const [operatorExpr, ...argumentExprs] = elements;
    if (operatorExpr.kind === 'symbol') {
        switch (operatorExpr.name) {
            case 'and':
                return evaluateAnd(argumentExprs, env);
            case 'or':
                return evaluateOr(argumentExprs, env);
            case 'if':
                return evaluateIf(argumentExprs, env, operatorExpr.pos);
            case 'define':
                return evaluateDefine(argumentExprs, env, operatorExpr.pos);
            case 'quote':
                return evaluateQuote(argumentExprs, operatorExpr.pos);
            case 'lambda':
                return evaluateLambda(argumentExprs, env, operatorExpr.pos);
            case 'begin':
                return evaluateBegin(argumentExprs, env);
            case 'cond':
                return evaluateCond(argumentExprs, env, operatorExpr.pos);
            case 'let':
                return evaluateLet(argumentExprs, env, operatorExpr.pos);
        }
    }
    const operator = evaluate(operatorExpr, env);
    const args = argumentExprs.map((argument) => evaluate(argument, env));
    return applyProcedure(operator, args, operatorExpr.pos);
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
function evaluateIf(expressions, env, pos) {
    if (expressions.length !== 3) {
        throw new EvalError(`if expected 3 argument(s), got ${expressions.length}`, pos);
    }
    const [conditionExpr, thenExpr, elseExpr] = expressions;
    const condition = evaluate(conditionExpr, env);
    if (isFalse(condition)) {
        return evaluate(elseExpr, env);
    }
    return evaluate(thenExpr, env);
}
function evaluateDefine(expressions, env, pos) {
    if (expressions.length < 2) {
        throw new EvalError(`define expected at least 2 argument(s), got ${expressions.length}`, pos);
    }
    const [targetExpr, ...valueExprs] = expressions;
    if (targetExpr.kind === 'symbol') {
        if (valueExprs.length !== 1) {
            throw new EvalError(`define expected 1 value expression, got ${valueExprs.length}`, pos);
        }
        const value = evaluate(valueExprs[0], env);
        env.define(targetExpr.name, value);
        return VOID;
    }
    if (targetExpr.kind !== 'list' || targetExpr.elements.length === 0) {
        throw new EvalError('define expected a symbol or function signature', targetExpr.pos);
    }
    const [nameExpr, ...paramExprs] = targetExpr.elements;
    const name = expectSymbolExpr(nameExpr, 'define');
    const params = paramExprs.map((expr) => expectSymbolExpr(expr, 'define'));
    if (valueExprs.length === 0) {
        throw new EvalError('define expected at least one function body expression', pos);
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
function evaluateQuote(expressions, pos) {
    if (expressions.length !== 1) {
        throw new EvalError(`quote expected 1 argument(s), got ${expressions.length}`, pos);
    }
    return quoteExpr(expressions[0]);
}
function evaluateLambda(expressions, env, pos) {
    if (expressions.length < 2) {
        throw new EvalError(`lambda expected at least 2 argument(s), got ${expressions.length}`, pos);
    }
    const [paramsExpr, ...body] = expressions;
    if (paramsExpr.kind !== 'list') {
        throw new EvalError('lambda expected a parameter list', paramsExpr.pos);
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
function evaluateCond(clauses, env, pos) {
    for (let index = 0; index < clauses.length; index += 1) {
        const clause = clauses[index];
        if (clause.kind !== 'list' || clause.elements.length === 0) {
            throw new EvalError('cond expected a non-empty clause', clause.pos);
        }
        const [testExpr, ...bodyExprs] = clause.elements;
        const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';
        if (isElseClause) {
            if (index !== clauses.length - 1) {
                throw new EvalError('cond else clause must be last', clause.pos);
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
function evaluateLet(expressions, env, pos) {
    if (expressions.length < 2) {
        throw new EvalError(`let expected at least 2 argument(s), got ${expressions.length}`, pos);
    }
    if (expressions[0].kind === 'symbol') {
        const [nameExpr, bindingsExpr, ...bodyExprs] = expressions;
        if (bindingsExpr === undefined || bodyExprs.length === 0) {
            throw new EvalError('let expected bindings and a body', pos);
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
        return applyProcedure(closure, values, nameExpr.pos);
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
function applyProcedure(value, args, pos) {
    if (isBuiltin(value)) {
        return value.apply(args, pos);
    }
    if (!isClosure(value)) {
        throw new EvalError('attempted to call a non-procedure value', pos);
    }
    if (args.length !== value.params.length) {
        throw new EvalError(`${value.name ?? 'lambda'} expected ${value.params.length} argument(s), got ${args.length}`, pos);
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
            return expression.value;
        case 'string':
            return makeString(expression.value);
        case 'char':
            return { kind: 'char', value: expression.value };
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
    let line = 1;
    let col = 1;
    const currentPosition = () => ({ line, col });
    const peekChar = () => input[index];
    const advanceChar = () => {
        const char = input[index];
        index += 1;
        if (char === '\n') {
            line += 1;
            col = 1;
        }
        else {
            col += 1;
        }
        return char;
    };
    while (index < input.length) {
        const char = peekChar();
        if (isWhitespace(char)) {
            advanceChar();
            continue;
        }
        if (char === ';') {
            while (index < input.length && peekChar() !== '\n') {
                advanceChar();
            }
            continue;
        }
        const pos = currentPosition();
        if (char === '(') {
            advanceChar();
            tokens.push({ kind: 'lparen', pos });
            continue;
        }
        if (char === ')') {
            advanceChar();
            tokens.push({ kind: 'rparen', pos });
            continue;
        }
        if (char === '\'') {
            advanceChar();
            tokens.push({ kind: 'quote', pos });
            continue;
        }
        if (char === '"') {
            const value = readStringLiteral(peekChar, advanceChar, pos);
            tokens.push({ kind: 'string', value, pos });
            continue;
        }
        let rawToken = '';
        while (index < input.length) {
            const next = peekChar();
            if (isWhitespace(next) || next === '(' || next === ')' || next === ';') {
                break;
            }
            rawToken += advanceChar();
        }
        if (rawToken.length === 0) {
            throw new EvalError('unexpected token', pos);
        }
        if (rawToken === '#t') {
            tokens.push({ kind: 'boolean', value: true, pos });
        }
        else if (rawToken === '#f') {
            tokens.push({ kind: 'boolean', value: false, pos });
        }
        else if (rawToken.startsWith('#\\')) {
            tokens.push({ kind: 'char', value: parseCharLiteral(rawToken, pos), pos });
        }
        else if (/^[+-]?\d+$/.test(rawToken)) {
            tokens.push({ kind: 'number', value: Number(rawToken), pos });
        }
        else {
            tokens.push({ kind: 'symbol', value: rawToken, pos });
        }
    }
    return { tokens, eofPosition: currentPosition() };
}
function readStringLiteral(peekChar, advanceChar, startPosition) {
    advanceChar();
    let value = '';
    while (peekChar() !== undefined) {
        const char = advanceChar();
        if (char === '"') {
            return value;
        }
        if (char === '\\') {
            if (peekChar() === undefined) {
                throw new EvalError('unterminated string literal', startPosition);
            }
            const escaped = advanceChar();
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
            continue;
        }
        value += char;
    }
    throw new EvalError('unterminated string literal', startPosition);
}
class Parser {
    tokens;
    eofPosition;
    index = 0;
    constructor(tokenStream) {
        this.tokens = tokenStream.tokens;
        this.eofPosition = tokenStream.eofPosition;
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
            throw new EvalError('unexpected end of input', this.eofPosition);
        }
        switch (token.kind) {
            case 'number':
                return { kind: 'number', value: token.value, pos: token.pos };
            case 'boolean':
                return { kind: 'boolean', value: token.value, pos: token.pos };
            case 'string':
                return { kind: 'string', value: token.value, pos: token.pos };
            case 'char':
                return { kind: 'char', value: token.value, pos: token.pos };
            case 'symbol':
                return { kind: 'symbol', name: token.value, pos: token.pos };
            case 'quote':
                return {
                    kind: 'list',
                    pos: token.pos,
                    elements: [
                        { kind: 'symbol', name: 'quote', pos: token.pos },
                        this.parseExpr(),
                    ],
                };
            case 'lparen': {
                const elements = [];
                while (true) {
                    const next = this.peek();
                    if (next === undefined) {
                        throw new EvalError('unterminated list', this.eofPosition);
                    }
                    if (next.kind === 'rparen') {
                        this.advance();
                        return { kind: 'list', elements, pos: token.pos };
                    }
                    elements.push(this.parseExpr());
                }
            }
            case 'rparen':
                throw new EvalError('unexpected )', token.pos);
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
function errorWithPosition(error, pos) {
    if (error instanceof EvalError) {
        return error.position === undefined ? new EvalError(error.message, pos) : error;
    }
    if (error instanceof Error) {
        return new EvalError(error.message, pos);
    }
    return new EvalError(String(error), pos);
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
function isStringValue(value) {
    return typeof value === 'object' && value !== null && value.kind === 'string';
}
function isPair(value) {
    return typeof value === 'object' && value !== null && value.kind === 'pair';
}
function isChar(value) {
    return typeof value === 'object' && value !== null && value.kind === 'char';
}
function isEmptyList(value) {
    return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}
function expectArity(name, args, expected, pos) {
    if (args.length !== expected) {
        throw new EvalError(`${name} expected ${expected} argument(s), got ${args.length}`, pos);
    }
}
function expectAtLeastArity(name, args, min, pos) {
    if (args.length < min) {
        throw new EvalError(`${name} expected at least ${min} argument(s), got ${args.length}`, pos);
    }
}
function expectNumber(value, name, pos) {
    if (typeof value !== 'number') {
        throw new EvalError(`${name} expected a number`, pos);
    }
    return value;
}
function expectStringValue(value, name, pos) {
    if (!isStringValue(value)) {
        throw new EvalError(`${name} expected a string`, pos);
    }
    return value;
}
function expectStringContent(value, name, pos) {
    return expectStringValue(value, name, pos).chars.join('');
}
function expectMutableString(value, name, pos) {
    const stringValue = expectStringValue(value, name, pos);
    if (!stringValue.mutable) {
        throw new EvalError(`${name} expected a mutable string`, pos);
    }
    return stringValue;
}
function expectPair(value, name, pos) {
    if (!isPair(value)) {
        throw new EvalError(`${name} expected a pair`, pos);
    }
    return value;
}
function expectChar(value, name, pos) {
    if (!isChar(value)) {
        throw new EvalError(`${name} expected a character`, pos);
    }
    return value;
}
function expectSymbolValue(value, name, pos) {
    if (!isSymbolValue(value)) {
        throw new EvalError(`${name} expected a symbol`, pos);
    }
    return value;
}
function expectIndex(value, name, pos) {
    const numericValue = expectNumber(value, name, pos);
    if (!Number.isInteger(numericValue) || numericValue < 0) {
        throw new EvalError(`${name} expected a non-negative integer`, pos);
    }
    return numericValue;
}
function expectSymbolExpr(expression, name) {
    if (expression.kind !== 'symbol') {
        throw new EvalError(`${name} expected a symbol`, expression.pos);
    }
    return expression.name;
}
function parseBindings(bindingsExpr, name) {
    if (bindingsExpr.kind !== 'list') {
        throw new EvalError(`${name} expected a bindings list`, bindingsExpr.pos);
    }
    return bindingsExpr.elements.map((bindingExpr) => {
        if (bindingExpr.kind !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError(`${name} expected bindings of the form (name value)`, bindingExpr.pos);
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
function listToArray(value, name, pos) {
    const elements = [];
    let current = value;
    while (isPair(current)) {
        elements.push(current.car);
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError(`${name} expected a list`, pos);
    }
    return elements;
}
function appendLists(args, pos) {
    const elements = [];
    for (const arg of args) {
        elements.push(...listToArray(arg, 'append', pos));
    }
    return listToPairs(elements);
}
function sum(args, identity, pos) {
    let total = identity;
    for (const arg of args) {
        total += expectNumber(arg, '+', pos);
    }
    return normalizeNumber(total);
}
function subtract(args, pos) {
    expectAtLeastArity('-', args, 1, pos);
    if (args.length === 1) {
        return normalizeNumber(-expectNumber(args[0], '-', pos));
    }
    let total = expectNumber(args[0], '-', pos);
    for (const arg of args.slice(1)) {
        total -= expectNumber(arg, '-', pos);
    }
    return normalizeNumber(total);
}
function product(args, identity, pos) {
    let total = identity;
    for (const arg of args) {
        total *= expectNumber(arg, '*', pos);
    }
    return normalizeNumber(total);
}
function divide(args, pos) {
    expectAtLeastArity('/', args, 2, pos);
    let total = expectNumber(args[0], '/', pos);
    for (const arg of args.slice(1)) {
        const divisor = expectNumber(arg, '/', pos);
        if (divisor === 0) {
            throw new EvalError('division by zero', pos);
        }
        total /= divisor;
    }
    return normalizeNumber(total);
}
function compareChain(name, args, predicate, pos) {
    expectAtLeastArity(name, args, 2, pos);
    const numbers = args.map((arg) => expectNumber(arg, name, pos));
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
function stringToChars(value) {
    return Array.from(value);
}
function makeString(value, mutable = true) {
    return {
        kind: 'string',
        chars: typeof value === 'string' ? stringToChars(value) : [...value],
        mutable,
    };
}
function parseCharLiteral(rawToken, pos) {
    const literal = rawToken.slice(2);
    if (literal === 'space') {
        return ' ';
    }
    if (literal === 'newline') {
        return '\n';
    }
    if (stringToChars(literal).length === 1) {
        return literal;
    }
    throw new EvalError('invalid character literal', pos);
}
function formatValue(value) {
    if (typeof value === 'number') {
        return String(normalizeNumber(value));
    }
    if (typeof value === 'boolean') {
        return value ? '#t' : '#f';
    }
    if (isStringValue(value)) {
        return JSON.stringify(value.chars.join(''));
    }
    if (isChar(value)) {
        return formatCharLiteral(value.value);
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
function formatDisplayValue(value) {
    if (isStringValue(value)) {
        return value.chars.join('');
    }
    if (isChar(value)) {
        return value.value;
    }
    if (isPair(value)) {
        return formatDisplayPair(value);
    }
    return formatValue(value);
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
function formatDisplayPair(pair) {
    const parts = [];
    let current = pair;
    while (isPair(current)) {
        parts.push(formatDisplayValue(current.car));
        current = current.cdr;
    }
    if (isEmptyList(current)) {
        return `(${parts.join(' ')})`;
    }
    return `(${parts.join(' ')} . ${formatDisplayValue(current)})`;
}
function formatCharLiteral(value) {
    if (value === ' ') {
        return '#\\space';
    }
    if (value === '\n') {
        return '#\\newline';
    }
    return `#\\${value}`;
}
