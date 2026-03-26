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
            'eq?',
            builtin('eq?', (args, pos) => {
                expectArity('eq?', args, 2, pos);
                return isEqValue(args[0], args[1]);
            }),
        ],
        [
            'equal?',
            builtin('equal?', (args, pos) => {
                expectArity('equal?', args, 2, pos);
                return isEqualValue(args[0], args[1]);
            }),
        ],
        [
            'abs',
            builtin('abs', (args, pos) => {
                expectArity('abs', args, 1, pos);
                return normalizeNumber(Math.abs(expectNumber(args[0], 'abs', pos)));
            }),
        ],
        [
            'modulo',
            builtin('modulo', (args, pos) => {
                expectArity('modulo', args, 2, pos);
                const dividend = expectInteger(args[0], 'modulo', pos);
                const divisor = expectInteger(args[1], 'modulo', pos);
                return modulo(dividend, divisor, pos);
            }),
        ],
        [
            'remainder',
            builtin('remainder', (args, pos) => {
                expectArity('remainder', args, 2, pos);
                const dividend = expectInteger(args[0], 'remainder', pos);
                const divisor = expectInteger(args[1], 'remainder', pos);
                return remainder(dividend, divisor, pos);
            }),
        ],
        [
            'quotient',
            builtin('quotient', (args, pos) => {
                expectArity('quotient', args, 2, pos);
                const dividend = expectInteger(args[0], 'quotient', pos);
                const divisor = expectInteger(args[1], 'quotient', pos);
                return quotient(dividend, divisor, pos);
            }),
        ],
        [
            'min',
            builtin('min', (args, pos) => {
                expectAtLeastArity('min', args, 1, pos);
                const numbers = args.map((arg) => expectNumber(arg, 'min', pos));
                return normalizeNumber(Math.min(...numbers));
            }),
        ],
        [
            'max',
            builtin('max', (args, pos) => {
                expectAtLeastArity('max', args, 1, pos);
                const numbers = args.map((arg) => expectNumber(arg, 'max', pos));
                return normalizeNumber(Math.max(...numbers));
            }),
        ],
        [
            'expt',
            builtin('expt', (args, pos) => {
                expectArity('expt', args, 2, pos);
                const base = expectNumber(args[0], 'expt', pos);
                const exponent = expectInteger(args[1], 'expt', pos);
                return normalizeNumber(base ** exponent);
            }),
        ],
        [
            'zero?',
            builtin('zero?', (args, pos) => {
                expectArity('zero?', args, 1, pos);
                return expectNumber(args[0], 'zero?', pos) === 0;
            }),
        ],
        [
            'positive?',
            builtin('positive?', (args, pos) => {
                expectArity('positive?', args, 1, pos);
                return expectNumber(args[0], 'positive?', pos) > 0;
            }),
        ],
        [
            'negative?',
            builtin('negative?', (args, pos) => {
                expectArity('negative?', args, 1, pos);
                return expectNumber(args[0], 'negative?', pos) < 0;
            }),
        ],
        [
            'odd?',
            builtin('odd?', (args, pos) => {
                expectArity('odd?', args, 1, pos);
                return Math.abs(expectInteger(args[0], 'odd?', pos) % 2) === 1;
            }),
        ],
        [
            'even?',
            builtin('even?', (args, pos) => {
                expectArity('even?', args, 1, pos);
                return expectInteger(args[0], 'even?', pos) % 2 === 0;
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
        [
            'list?',
            builtin('list?', (args, pos) => {
                expectArity('list?', args, 1, pos);
                return isProperList(args[0]);
            }),
        ],
        ['list', builtin('list', (args) => listToPairs(args))],
        [
            'list-ref',
            builtin('list-ref', (args, pos) => {
                expectArity('list-ref', args, 2, pos);
                return listRef(args[0], expectIndex(args[1], 'list-ref', pos), pos);
            }),
        ],
        [
            'list-tail',
            builtin('list-tail', (args, pos) => {
                expectArity('list-tail', args, 2, pos);
                return listTail(args[0], expectIndex(args[1], 'list-tail', pos), pos);
            }),
        ],
        [
            'length',
            builtin('length', (args, pos) => {
                expectArity('length', args, 1, pos);
                return listToArray(args[0], 'length', pos).length;
            }),
        ],
        ['append', builtin('append', (args, pos) => appendLists(args, pos))],
        [
            'assoc',
            builtin('assoc', (args, pos) => {
                expectArity('assoc', args, 2, pos);
                return assoc(args[0], args[1], pos);
            }),
        ],
        [
            'map',
            builtin('map', (args, pos) => {
                expectAtLeastArity('map', args, 2, pos);
                return mapLists(args[0], args.slice(1), pos);
            }),
        ],
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
            'apply',
            builtin('apply', (args, pos) => {
                expectAtLeastArity('apply', args, 2, pos);
                const [procedure, ...rest] = args;
                const listArg = rest[rest.length - 1];
                const prefixArgs = rest.slice(0, -1);
                const listArgs = listToArray(listArg, 'apply', pos);
                return applyProcedure(procedure, [...prefixArgs, ...listArgs], pos);
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
        [
            'char-alphabetic?',
            builtin('char-alphabetic?', (args, pos) => {
                expectArity('char-alphabetic?', args, 1, pos);
                return /^[A-Za-z]$/.test(expectChar(args[0], 'char-alphabetic?', pos).value);
            }),
        ],
        [
            'char-numeric?',
            builtin('char-numeric?', (args, pos) => {
                expectArity('char-numeric?', args, 1, pos);
                return /^[0-9]$/.test(expectChar(args[0], 'char-numeric?', pos).value);
            }),
        ],
        [
            'char-upcase',
            builtin('char-upcase', (args, pos) => {
                expectArity('char-upcase', args, 1, pos);
                return { kind: 'char', value: expectChar(args[0], 'char-upcase', pos).value.toUpperCase() };
            }),
        ],
        [
            'char-downcase',
            builtin('char-downcase', (args, pos) => {
                expectArity('char-downcase', args, 1, pos);
                return { kind: 'char', value: expectChar(args[0], 'char-downcase', pos).value.toLowerCase() };
            }),
        ],
        [
            'char=?',
            builtin('char=?', (args, pos) => compareCharChain('char=?', args, (left, right) => left === right, pos)),
        ],
        [
            'char<?',
            builtin('char<?', (args, pos) => compareCharChain('char<?', args, (left, right) => left < right, pos)),
        ],
        [
            'string=?',
            builtin('string=?', (args, pos) => compareStringChain('string=?', args, (left, right) => left === right, pos)),
        ],
        [
            'string<?',
            builtin('string<?', (args, pos) => compareStringChain('string<?', args, (left, right) => left < right, pos)),
        ],
        [
            'string-ci=?',
            builtin('string-ci=?', (args, pos) => compareStringChain('string-ci=?', args, (left, right) => left.toLowerCase() === right.toLowerCase(), pos)),
        ],
        [
            'string-upcase',
            builtin('string-upcase', (args, pos) => {
                expectArity('string-upcase', args, 1, pos);
                return makeString(expectStringContent(args[0], 'string-upcase', pos).toUpperCase());
            }),
        ],
        [
            'string-downcase',
            builtin('string-downcase', (args, pos) => {
                expectArity('string-downcase', args, 1, pos);
                return makeString(expectStringContent(args[0], 'string-downcase', pos).toLowerCase());
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
    assign(name, value, pos) {
        if (this.bindings.has(name)) {
            this.bindings.set(name, value);
            return;
        }
        if (this.parent !== undefined) {
            this.parent.assign(name, value, pos);
            return;
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
            case 'set!':
                return evaluateSet(argumentExprs, env, operatorExpr.pos);
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
    const { params, restParam } = parseFormalParameters(paramExprs, 'define');
    if (valueExprs.length === 0) {
        throw new EvalError('define expected at least one function body expression', pos);
    }
    const closure = {
        kind: 'closure',
        name,
        params,
        restParam,
        body: valueExprs,
        env,
    };
    env.define(name, closure);
    return VOID;
}
function evaluateSet(expressions, env, pos) {
    if (expressions.length !== 2) {
        throw new EvalError(`set! expected 2 argument(s), got ${expressions.length}`, pos);
    }
    const [targetExpr, valueExpr] = expressions;
    const name = expectSymbolExpr(targetExpr, 'set!');
    const value = evaluate(valueExpr, env);
    env.assign(name, value, targetExpr.pos);
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
    const { params, restParam } = parseFormalParameters(paramsExpr.elements, 'lambda');
    return {
        kind: 'closure',
        params,
        restParam,
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
    if (value.restParam === undefined && args.length !== value.params.length) {
        throw new EvalError(`${value.name ?? 'lambda'} expected ${value.params.length} argument(s), got ${args.length}`, pos);
    }
    if (value.restParam !== undefined && args.length < value.params.length) {
        throw new EvalError(`${value.name ?? 'lambda'} expected at least ${value.params.length} argument(s), got ${args.length}`, pos);
    }
    const callEnv = new Environment(value.env);
    for (let index = 0; index < value.params.length; index += 1) {
        callEnv.define(value.params[index], args[index]);
    }
    if (value.restParam !== undefined) {
        callEnv.define(value.restParam, listToPairs(args.slice(value.params.length)));
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
    const numericValue = expectInteger(value, name, pos);
    if (numericValue < 0) {
        throw new EvalError(`${name} expected a non-negative integer`, pos);
    }
    return numericValue;
}
function expectInteger(value, name, pos) {
    const numericValue = expectNumber(value, name, pos);
    if (!Number.isInteger(numericValue)) {
        throw new EvalError(`${name} expected an integer`, pos);
    }
    return numericValue;
}
function expectSymbolExpr(expression, name) {
    if (expression.kind !== 'symbol') {
        throw new EvalError(`${name} expected a symbol`, expression.pos);
    }
    return expression.name;
}
function parseFormalParameters(parameterExprs, name) {
    const params = [];
    for (let index = 0; index < parameterExprs.length; index += 1) {
        const parameterExpr = parameterExprs[index];
        if (parameterExpr.kind === 'symbol' && parameterExpr.name === '.') {
            if (index !== parameterExprs.length - 2) {
                throw new EvalError(`${name} expected a valid dotted parameter list`, parameterExpr.pos);
            }
            return {
                params,
                restParam: expectSymbolExpr(parameterExprs[index + 1], name),
            };
        }
        params.push(expectSymbolExpr(parameterExpr, name));
    }
    return { params };
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
function listRef(value, index, pos) {
    let current = value;
    let remaining = index;
    while (remaining > 0) {
        if (!isPair(current)) {
            throw new EvalError('list-ref index out of bounds', pos);
        }
        current = current.cdr;
        remaining -= 1;
    }
    if (!isPair(current)) {
        throw new EvalError('list-ref index out of bounds', pos);
    }
    return current.car;
}
function listTail(value, index, pos) {
    let current = value;
    let remaining = index;
    while (remaining > 0) {
        if (!isPair(current)) {
            throw new EvalError('list-tail index out of bounds', pos);
        }
        current = current.cdr;
        remaining -= 1;
    }
    if (!isPair(current) && !isEmptyList(current)) {
        throw new EvalError('list-tail index out of bounds', pos);
    }
    return current;
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
function isProperList(value) {
    let current = value;
    while (isPair(current)) {
        current = current.cdr;
    }
    return isEmptyList(current);
}
function assoc(key, value, pos) {
    let current = value;
    while (isPair(current)) {
        const entry = expectPair(current.car, 'assoc', pos);
        if (isEqualValue(key, entry.car)) {
            return entry;
        }
        current = current.cdr;
    }
    if (!isEmptyList(current)) {
        throw new EvalError('assoc expected a list', pos);
    }
    return false;
}
function mapLists(procedure, lists, pos) {
    const arrays = lists.map((list) => listToArray(list, 'map', pos));
    const length = arrays[0].length;
    for (const array of arrays) {
        if (array.length !== length) {
            throw new EvalError('map expected lists of equal length', pos);
        }
    }
    const results = [];
    for (let index = 0; index < length; index += 1) {
        results.push(applyProcedure(procedure, arrays.map((array) => array[index]), pos));
    }
    return listToPairs(results);
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
function quotient(dividend, divisor, pos) {
    if (divisor === 0) {
        throw new EvalError('division by zero', pos);
    }
    return normalizeNumber(Math.trunc(dividend / divisor));
}
function remainder(dividend, divisor, pos) {
    if (divisor === 0) {
        throw new EvalError('division by zero', pos);
    }
    return normalizeNumber(dividend % divisor);
}
function modulo(dividend, divisor, pos) {
    const result = remainder(dividend, divisor, pos);
    if (result !== 0 && Math.sign(result) !== Math.sign(divisor)) {
        return normalizeNumber(result + divisor);
    }
    return result;
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
function compareCharChain(name, args, predicate, pos) {
    expectAtLeastArity(name, args, 2, pos);
    const codes = args.map((arg) => charCode(expectChar(arg, name, pos).value));
    for (let index = 0; index < codes.length - 1; index += 1) {
        if (!predicate(codes[index], codes[index + 1])) {
            return false;
        }
    }
    return true;
}
function compareStringChain(name, args, predicate, pos) {
    expectAtLeastArity(name, args, 2, pos);
    const values = args.map((arg) => expectStringContent(arg, name, pos));
    for (let index = 0; index < values.length - 1; index += 1) {
        if (!predicate(values[index], values[index + 1])) {
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
function isEqValue(left, right) {
    if (typeof left === 'number' || typeof left === 'boolean') {
        return left === right;
    }
    if (isEmptyList(left) && isEmptyList(right)) {
        return true;
    }
    if (isSymbolValue(left) && isSymbolValue(right)) {
        return left.name === right.name;
    }
    if (isChar(left) && isChar(right)) {
        return left.value === right.value;
    }
    return left === right;
}
function isEqualValue(left, right) {
    if (isEqValue(left, right)) {
        return true;
    }
    if (isStringValue(left) && isStringValue(right)) {
        return left.chars.join('') === right.chars.join('');
    }
    if (isPair(left) && isPair(right)) {
        return isEqualValue(left.car, right.car) && isEqualValue(left.cdr, right.cdr);
    }
    return false;
}
function charCode(value) {
    return value.codePointAt(0);
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
