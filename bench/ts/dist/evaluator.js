import { EvalError } from './evalError.js';
const VOID_VALUE = { type: 'void' };
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
        const value = this.bindings.get(name);
        if (value !== undefined) {
            return value;
        }
        if (this.parent) {
            return this.parent.lookup(name);
        }
        throw new EvalError(`unbound variable: ${name}`);
    }
}
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input) {
    const expressions = parseProgram(input);
    if (expressions.length === 0) {
        throw new EvalError('empty input');
    }
    const env = createGlobalEnv();
    let result = VOID_VALUE;
    for (const expr of expressions) {
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
function parseProgram(input) {
    const tokens = tokenize(input);
    let index = 0;
    const expressions = [];
    while (index < tokens.length) {
        expressions.push(parseExpr());
    }
    return expressions;
    function parseExpr() {
        const token = tokens[index];
        if (!token) {
            throw new EvalError('unexpected end of input');
        }
        index += 1;
        if (token.kind === 'paren') {
            if (token.value === ')') {
                throw new EvalError('unexpected )');
            }
            const elements = [];
            while (index < tokens.length) {
                const next = tokens[index];
                if (next.kind === 'paren' && next.value === ')') {
                    index += 1;
                    return { type: 'list', elements };
                }
                elements.push(parseExpr());
            }
            throw new EvalError('missing )');
        }
        if (token.kind === 'quote') {
            return {
                type: 'list',
                elements: [{ type: 'symbol', name: 'quote' }, parseExpr()],
            };
        }
        if (token.kind === 'string') {
            return { type: 'string', value: token.value };
        }
        if (token.value === '#t') {
            return { type: 'boolean', value: true };
        }
        if (token.value === '#f') {
            return { type: 'boolean', value: false };
        }
        if (/^-?\d+$/.test(token.value)) {
            return { type: 'number', value: Number(token.value) };
        }
        return { type: 'symbol', name: token.value };
    }
}
function tokenize(input) {
    const tokens = [];
    let index = 0;
    while (index < input.length) {
        const ch = input[index];
        if (/\s/.test(ch)) {
            index += 1;
            continue;
        }
        if (ch === ';') {
            while (index < input.length && input[index] !== '\n') {
                index += 1;
            }
            continue;
        }
        if (ch === '(' || ch === ')') {
            tokens.push({ kind: 'paren', value: ch });
            index += 1;
            continue;
        }
        if (ch === "'") {
            tokens.push({ kind: 'quote' });
            index += 1;
            continue;
        }
        if (ch === '"') {
            index += 1;
            let value = '';
            let terminated = false;
            while (index < input.length) {
                const current = input[index];
                if (current === '"') {
                    index += 1;
                    tokens.push({ kind: 'string', value });
                    terminated = true;
                    break;
                }
                if (current === '\\') {
                    index += 1;
                    if (index >= input.length) {
                        throw new EvalError('unterminated string literal');
                    }
                    const escaped = input[index];
                    switch (escaped) {
                        case 'n':
                            value += '\n';
                            break;
                        case 'r':
                            value += '\r';
                            break;
                        case 't':
                            value += '\t';
                            break;
                        case '"':
                            value += '"';
                            break;
                        case '\\':
                            value += '\\';
                            break;
                        default:
                            value += escaped;
                            break;
                    }
                    index += 1;
                    continue;
                }
                value += current;
                index += 1;
            }
            if (!terminated) {
                throw new EvalError('unterminated string literal');
            }
            continue;
        }
        let end = index;
        while (end < input.length) {
            const current = input[end];
            if (/\s/.test(current) || current === '(' || current === ')' || current === ';') {
                break;
            }
            end += 1;
        }
        tokens.push({ kind: 'atom', value: input.slice(index, end) });
        index = end;
    }
    return tokens;
}
function evaluate(expr, env) {
    switch (expr.type) {
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
function evaluateList(elements, env) {
    if (elements.length === 0) {
        throw new EvalError('cannot evaluate empty list');
    }
    const operator = elements[0];
    const args = elements.slice(1);
    if (operator.type === 'symbol') {
        switch (operator.name) {
            case 'define':
                return evaluateDefine(args, env);
            case 'if':
                return evaluateIf(args, env);
            case 'quote':
                return evaluateQuote(args);
            case 'lambda':
                return evaluateLambda(args, env);
            case 'and':
                return evaluateAnd(args, env);
            case 'or':
                return evaluateOr(args, env);
            case 'begin':
                return evaluateBegin(args, env);
            case 'let':
                return evaluateLet(args, env);
            case 'cond':
                return evaluateCond(args, env);
        }
    }
    const procedure = evaluate(operator, env);
    const evaluatedArgs = args.map((arg) => evaluate(arg, env));
    return applyProcedure(procedure, evaluatedArgs);
}
function evaluateDefine(args, env) {
    if (args.length < 2) {
        throw new EvalError(`define: expected at least 2 argument(s), got ${args.length}`);
    }
    const target = args[0];
    if (target.type === 'symbol') {
        requireArgCount('define', args.length, 2);
        const value = evaluate(args[1], env);
        env.define(target.name, value);
        return VOID_VALUE;
    }
    if (target.type !== 'list' || target.elements.length === 0) {
        throw new EvalError('define: invalid binding target');
    }
    const nameExpr = target.elements[0];
    if (nameExpr.type !== 'symbol') {
        throw new EvalError('define: invalid function name');
    }
    const params = parseParameterNames(target.elements.slice(1));
    const body = args.slice(1);
    const closure = { type: 'closure', params, body, env };
    env.define(nameExpr.name, closure);
    return VOID_VALUE;
}
function evaluateIf(args, env) {
    requireArgCount('if', args.length, 3);
    return isTruthy(evaluate(args[0], env)) ? evaluate(args[1], env) : evaluate(args[2], env);
}
function evaluateQuote(args) {
    requireArgCount('quote', args.length, 1);
    return quoteExpr(args[0]);
}
function evaluateLambda(args, env) {
    requireArgCountAtLeast('lambda', args.length, 2);
    const paramsExpr = args[0];
    if (paramsExpr.type !== 'list') {
        throw new EvalError('lambda: parameter list must be a list');
    }
    return {
        type: 'closure',
        params: parseParameterNames(paramsExpr.elements),
        body: args.slice(1),
        env,
    };
}
function evaluateAnd(args, env) {
    let result = booleanValue(true);
    for (const arg of args) {
        result = evaluate(arg, env);
        if (!isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evaluateOr(args, env) {
    let result = booleanValue(false);
    for (const arg of args) {
        result = evaluate(arg, env);
        if (isTruthy(result)) {
            return result;
        }
    }
    return result;
}
function evaluateBegin(args, env) {
    return evaluateSequence(args, env);
}
function evaluateLet(args, env) {
    requireArgCountAtLeast('let', args.length, 2);
    const firstArg = args[0];
    if (firstArg.type === 'symbol') {
        requireArgCountAtLeast('let', args.length, 3);
        const bindings = parseLetBindings(args[1]);
        const values = bindings.map((binding) => evaluate(binding.value, env));
        const letEnv = new Environment(env);
        const closure = {
            type: 'closure',
            params: bindings.map((binding) => binding.name),
            body: args.slice(2),
            env: letEnv,
        };
        letEnv.define(firstArg.name, closure);
        return applyProcedure(closure, values);
    }
    const bindings = parseLetBindings(firstArg);
    const letEnv = new Environment(env);
    for (const binding of bindings) {
        letEnv.define(binding.name, evaluate(binding.value, env));
    }
    return evaluateSequence(args.slice(1), letEnv);
}
function evaluateCond(args, env) {
    for (const clause of args) {
        if (clause.type !== 'list' || clause.elements.length === 0) {
            throw new EvalError('cond: expected non-empty clause');
        }
        const [testExpr, ...body] = clause.elements;
        if (testExpr.type === 'symbol' && testExpr.name === 'else') {
            return body.length === 0 ? VOID_VALUE : evaluateSequence(body, env);
        }
        const testValue = evaluate(testExpr, env);
        if (isTruthy(testValue)) {
            return body.length === 0 ? testValue : evaluateSequence(body, env);
        }
    }
    return VOID_VALUE;
}
function quoteExpr(expr) {
    switch (expr.type) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr;
        case 'symbol':
            return { type: 'symbol', name: expr.name };
        case 'list':
            return { type: 'list', elements: expr.elements.map(quoteExpr) };
    }
}
function parseParameterNames(params) {
    return params.map((param) => {
        if (param.type !== 'symbol') {
            throw new EvalError('lambda: parameter names must be symbols');
        }
        return param.name;
    });
}
function parseLetBindings(bindingsExpr) {
    if (bindingsExpr.type !== 'list') {
        throw new EvalError('let: expected binding list');
    }
    return bindingsExpr.elements.map((bindingExpr) => {
        if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
            throw new EvalError('let: expected binding pair');
        }
        const [nameExpr, valueExpr] = bindingExpr.elements;
        if (nameExpr.type !== 'symbol') {
            throw new EvalError('let: binding name must be a symbol');
        }
        return { name: nameExpr.name, value: valueExpr };
    });
}
function applyProcedure(value, args) {
    switch (value.type) {
        case 'builtin':
            return value.invoke(args);
        case 'closure': {
            requireArgCount('lambda', args.length, value.params.length);
            const callEnv = new Environment(value.env);
            for (let index = 0; index < value.params.length; index += 1) {
                callEnv.define(value.params[index], args[index]);
            }
            return evaluateSequence(value.body, callEnv);
        }
        default:
            throw new EvalError('attempted to call a non-procedure');
    }
}
function evaluateSequence(expressions, env) {
    let result = VOID_VALUE;
    for (const expr of expressions) {
        result = evaluate(expr, env);
    }
    return result;
}
function createGlobalEnv() {
    const env = new Environment();
    env.define('+', builtin('+', (args) => numberValue(evaluateNumberArgs(args).reduce((sum, value) => sum + value, 0))));
    env.define('*', builtin('*', (args) => numberValue(evaluateNumberArgs(args).reduce((product, value) => product * value, 1))));
    env.define('-', builtin('-', (args) => {
        const values = evaluateNumberArgs(args);
        requireArgCountAtLeast('-', values.length, 1);
        if (values.length === 1) {
            return numberValue(-values[0]);
        }
        return numberValue(values.slice(1).reduce((result, value) => result - value, values[0]));
    }));
    env.define('/', builtin('/', (args) => {
        const values = evaluateNumberArgs(args);
        requireArgCountAtLeast('/', values.length, 1);
        let result = values[0];
        if (values.length === 1) {
            if (result === 0) {
                throw new EvalError('division by zero');
            }
            return numberValue(1 / result);
        }
        for (const value of values.slice(1)) {
            if (value === 0) {
                throw new EvalError('division by zero');
            }
            result /= value;
        }
        return numberValue(result);
    }));
    env.define('<', builtin('<', (args) => booleanValue(compareNumberArgs('<', args, (a, b) => a < b))));
    env.define('>', builtin('>', (args) => booleanValue(compareNumberArgs('>', args, (a, b) => a > b))));
    env.define('=', builtin('=', (args) => booleanValue(compareNumberArgs('=', args, (a, b) => a === b))));
    env.define('<=', builtin('<=', (args) => booleanValue(compareNumberArgs('<=', args, (a, b) => a <= b))));
    env.define('not', builtin('not', (args) => {
        requireArgCount('not', args.length, 1);
        return booleanValue(!isTruthy(args[0]));
    }));
    env.define('cons', builtin('cons', (args) => {
        requireArgCount('cons', args.length, 2);
        const tail = expectList('cons', args[1]);
        return { type: 'list', elements: [args[0], ...tail.elements] };
    }));
    env.define('car', builtin('car', (args) => {
        requireArgCount('car', args.length, 1);
        return expectPair('car', args[0]).elements[0];
    }));
    env.define('cdr', builtin('cdr', (args) => {
        requireArgCount('cdr', args.length, 1);
        return { type: 'list', elements: expectPair('cdr', args[0]).elements.slice(1) };
    }));
    env.define('list', builtin('list', (args) => ({ type: 'list', elements: [...args] })));
    env.define('length', builtin('length', (args) => {
        requireArgCount('length', args.length, 1);
        return numberValue(expectList('length', args[0]).elements.length);
    }));
    env.define('append', builtin('append', (args) => {
        const elements = [];
        for (const arg of args) {
            elements.push(...expectList('append', arg).elements);
        }
        return { type: 'list', elements };
    }));
    env.define('null?', builtin('null?', (args) => {
        requireArgCount('null?', args.length, 1);
        return booleanValue(args[0].type === 'list' && args[0].elements.length === 0);
    }));
    env.define('pair?', builtin('pair?', (args) => {
        requireArgCount('pair?', args.length, 1);
        return booleanValue(args[0].type === 'list' && args[0].elements.length > 0);
    }));
    env.define('string?', builtin('string?', (args) => {
        requireArgCount('string?', args.length, 1);
        return booleanValue(args[0].type === 'string');
    }));
    env.define('number?', builtin('number?', (args) => {
        requireArgCount('number?', args.length, 1);
        return booleanValue(args[0].type === 'number');
    }));
    env.define('boolean?', builtin('boolean?', (args) => {
        requireArgCount('boolean?', args.length, 1);
        return booleanValue(args[0].type === 'boolean');
    }));
    env.define('symbol?', builtin('symbol?', (args) => {
        requireArgCount('symbol?', args.length, 1);
        return booleanValue(args[0].type === 'symbol');
    }));
    return env;
}
function builtin(name, invoke) {
    return { type: 'builtin', name, invoke };
}
function evaluateNumberArgs(args) {
    return args.map(expectNumber);
}
function compareNumberArgs(name, args, predicate) {
    const values = evaluateNumberArgs(args);
    requireArgCountAtLeast(name, values.length, 1);
    for (let index = 0; index < values.length - 1; index += 1) {
        if (!predicate(values[index], values[index + 1])) {
            return false;
        }
    }
    return true;
}
function requireArgCount(name, actual, expected) {
    if (actual !== expected) {
        throw new EvalError(`${name}: expected ${expected} argument(s), got ${actual}`);
    }
}
function requireArgCountAtLeast(name, actual, minimum) {
    if (actual < minimum) {
        throw new EvalError(`${name}: expected at least ${minimum} argument(s), got ${actual}`);
    }
}
function expectNumber(value) {
    if (value.type !== 'number') {
        throw new EvalError('expected number');
    }
    return value.value;
}
function expectList(name, value) {
    if (value.type !== 'list') {
        throw new EvalError(`${name}: expected list`);
    }
    return value;
}
function expectPair(name, value) {
    const list = expectList(name, value);
    if (list.elements.length === 0) {
        throw new EvalError(`${name}: expected non-empty list`);
    }
    return list;
}
function isTruthy(value) {
    return value.type !== 'boolean' || value.value;
}
function numberValue(value) {
    if (!Number.isFinite(value)) {
        throw new EvalError('invalid number');
    }
    return { type: 'number', value };
}
function booleanValue(value) {
    return { type: 'boolean', value };
}
function formatValue(value) {
    switch (value.type) {
        case 'number':
            return String(value.value);
        case 'boolean':
            return value.value ? '#t' : '#f';
        case 'string':
            return JSON.stringify(value.value);
        case 'symbol':
            return value.name;
        case 'list':
            return `(${value.elements.map(formatValue).join(' ')})`;
        case 'builtin':
        case 'closure':
            return '#<procedure>';
        case 'void':
            return '#<void>';
    }
}
