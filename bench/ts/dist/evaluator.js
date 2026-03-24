import { EvalError } from './evalError.js';
const NIL = { tag: 'nil' };
function listToConsPairs(lst) {
    if (lst.tag !== 'list')
        return lst;
    let result = NIL;
    for (let i = lst.value.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: listToConsPairs(lst.value[i]), cdr: result };
    }
    return result;
}
function tokenize(input) {
    const tokens = [];
    let i = 0;
    while (i < input.length) {
        const ch = input[i];
        // skip whitespace
        if (/\s/.test(ch)) {
            i++;
            continue;
        }
        // skip line comments
        if (ch === ';') {
            while (i < input.length && input[i] !== '\n')
                i++;
            continue;
        }
        if (ch === '(') {
            tokens.push({ type: 'lparen', value: '(' });
            i++;
            continue;
        }
        if (ch === ')') {
            tokens.push({ type: 'rparen', value: ')' });
            i++;
            continue;
        }
        if (ch === '\'') {
            tokens.push({ type: 'quote', value: '\'' });
            i++;
            continue;
        }
        if (ch === '"') {
            let s = '';
            i++; // skip opening quote
            while (i < input.length && input[i] !== '"') {
                if (input[i] === '\\') {
                    i++;
                    if (i < input.length) {
                        if (input[i] === 'n')
                            s += '\n';
                        else if (input[i] === 't')
                            s += '\t';
                        else if (input[i] === '"')
                            s += '"';
                        else if (input[i] === '\\')
                            s += '\\';
                        else
                            s += input[i];
                    }
                }
                else {
                    s += input[i];
                }
                i++;
            }
            i++; // skip closing quote
            tokens.push({ type: 'string', value: s });
            continue;
        }
        // atom
        let atom = '';
        while (i < input.length && !/[\s()";]/.test(input[i])) {
            atom += input[i];
            i++;
        }
        if (atom === '.') {
            tokens.push({ type: 'dot', value: '.' });
        }
        else {
            tokens.push({ type: 'atom', value: atom });
        }
    }
    return tokens;
}
function parse(tokens) {
    let pos = 0;
    function parseExpr() {
        if (pos >= tokens.length)
            throw new EvalError('unexpected end of input');
        const tok = tokens[pos];
        if (tok.type === 'lparen') {
            pos++; // skip (
            const elements = [];
            while (pos < tokens.length && tokens[pos].type !== 'rparen') {
                elements.push(parseExpr());
            }
            if (pos >= tokens.length)
                throw new EvalError('missing closing parenthesis');
            pos++; // skip )
            return { tag: 'list', value: elements };
        }
        if (tok.type === 'rparen') {
            throw new EvalError('unexpected )');
        }
        if (tok.type === 'quote') {
            pos++;
            const quoted = parseExpr();
            return { tag: 'list', value: [{ tag: 'symbol', value: 'quote' }, quoted] };
        }
        if (tok.type === 'string') {
            pos++;
            return { tag: 'string', value: tok.value };
        }
        // atom
        pos++;
        const v = tok.value;
        if (v === '#t')
            return { tag: 'boolean', value: true };
        if (v === '#f')
            return { tag: 'boolean', value: false };
        if (/^-?\d+$/.test(v))
            return { tag: 'number', value: parseInt(v, 10) };
        return { tag: 'symbol', value: v };
    }
    const exprs = [];
    while (pos < tokens.length) {
        exprs.push(parseExpr());
    }
    return exprs;
}
// --- Environment ---
class Env {
    bindings;
    parent;
    constructor(parent = null) {
        this.bindings = new Map();
        this.parent = parent;
    }
    get(name) {
        const val = this.bindings.get(name);
        if (val !== undefined)
            return val;
        if (this.parent)
            return this.parent.get(name);
        throw new EvalError(`unbound variable: ${name}`);
    }
    set(name, value) {
        this.bindings.set(name, value);
    }
}
function makeGlobalEnv() {
    const env = new Env();
    const numOp = (op, identity) => ({ tag: 'procedure', value: (...args) => {
            const nums = args.map(a => {
                if (a.tag !== 'number')
                    throw new EvalError('expected number');
                return a.value;
            });
            return { tag: 'number', value: nums.reduce(op, identity) };
        } });
    env.set('+', { tag: 'procedure', value: (...args) => {
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('expected number'); return a.value; });
            return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
        } });
    env.set('*', { tag: 'procedure', value: (...args) => {
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('expected number'); return a.value; });
            return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
        } });
    env.set('-', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                throw new EvalError('- requires at least one argument');
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('expected number'); return a.value; });
            if (nums.length === 1)
                return { tag: 'number', value: -nums[0] };
            return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
        } });
    env.set('/', { tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('/ requires at least two arguments');
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('expected number'); return a.value; });
            return { tag: 'number', value: nums.slice(1).reduce((a, b) => {
                    if (b === 0)
                        throw new EvalError('division by zero');
                    return Math.trunc(a / b);
                }, nums[0]) };
        } });
    const cmpOp = (op) => ({ tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('comparison requires at least two arguments');
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('expected number'); return a.value; });
            for (let i = 0; i < nums.length - 1; i++) {
                if (!op(nums[i], nums[i + 1]))
                    return { tag: 'boolean', value: false };
            }
            return { tag: 'boolean', value: true };
        } });
    env.set('<', cmpOp((a, b) => a < b));
    env.set('>', cmpOp((a, b) => a > b));
    env.set('=', cmpOp((a, b) => a === b));
    env.set('<=', cmpOp((a, b) => a <= b));
    env.set('>=', cmpOp((a, b) => a >= b));
    env.set('not', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('not requires exactly one argument');
            return { tag: 'boolean', value: isFalsy(args[0]) };
        } });
    // List primitives
    env.set('cons', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('cons requires exactly 2 arguments');
            return { tag: 'pair', car: args[0], cdr: args[1] };
        } });
    env.set('car', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('car requires exactly 1 argument');
            if (args[0].tag === 'pair')
                return args[0].car;
            throw new EvalError('car: not a pair');
        } });
    env.set('cdr', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('cdr requires exactly 1 argument');
            if (args[0].tag === 'pair')
                return args[0].cdr;
            throw new EvalError('cdr: not a pair');
        } });
    env.set('null?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('null? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'nil' };
        } });
    env.set('list', { tag: 'procedure', value: (...args) => {
            let result = NIL;
            for (let i = args.length - 1; i >= 0; i--) {
                result = { tag: 'pair', car: args[i], cdr: result };
            }
            return result;
        } });
    env.set('length', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('length requires exactly 1 argument');
            let count = 0;
            let cur = args[0];
            while (cur.tag === 'pair') {
                count++;
                cur = cur.cdr;
            }
            if (cur.tag !== 'nil')
                throw new EvalError('length: not a proper list');
            return { tag: 'number', value: count };
        } });
    env.set('append', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                return NIL;
            if (args.length === 1)
                return args[0];
            // Build result by appending all lists
            let result = args[args.length - 1];
            for (let i = args.length - 2; i >= 0; i--) {
                const elems = [];
                let cur = args[i];
                while (cur.tag === 'pair') {
                    elems.push(cur.car);
                    cur = cur.cdr;
                }
                for (let j = elems.length - 1; j >= 0; j--) {
                    result = { tag: 'pair', car: elems[j], cdr: result };
                }
            }
            return result;
        } });
    // Type predicates
    env.set('number?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('number? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'number' };
        } });
    env.set('string?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('string? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'string' };
        } });
    env.set('boolean?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('boolean? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'boolean' };
        } });
    env.set('pair?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('pair? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'pair' };
        } });
    env.set('symbol?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('symbol? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'symbol' };
        } });
    return env;
}
// --- Evaluator ---
function isFalsy(val) {
    return val.tag === 'boolean' && val.value === false;
}
function evaluate(expr, env) {
    switch (expr.tag) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr;
        case 'symbol':
            return env.get(expr.value);
        case 'list': {
            const elems = expr.value;
            if (elems.length === 0)
                throw new EvalError('empty application');
            const first = elems[0];
            // Special forms
            if (first.tag === 'symbol') {
                switch (first.value) {
                    case 'define': {
                        if (elems.length < 3)
                            throw new EvalError('define requires at least 2 arguments');
                        const target = elems[1];
                        if (target.tag === 'symbol') {
                            // (define x expr)
                            const val = evaluate(elems[2], env);
                            env.set(target.value, val);
                            return { tag: 'void' };
                        }
                        if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
                            // (define (f params...) body...)
                            const name = target.value[0].value;
                            const paramNames = target.value.slice(1).map(p => {
                                if (p.tag !== 'symbol')
                                    throw new EvalError('parameter must be a symbol');
                                return p.value;
                            });
                            const bodyExprs = elems.slice(2);
                            const proc = { tag: 'procedure', value: (...args) => {
                                    const childEnv = new Env(env);
                                    for (let i = 0; i < paramNames.length; i++) {
                                        childEnv.set(paramNames[i], args[i]);
                                    }
                                    let result = { tag: 'void' };
                                    for (const b of bodyExprs) {
                                        result = evaluate(b, childEnv);
                                    }
                                    return result;
                                } };
                            env.set(name, proc);
                            return { tag: 'void' };
                        }
                        throw new EvalError('invalid define syntax');
                    }
                    case 'if': {
                        if (elems.length < 3)
                            throw new EvalError('if requires at least 2 arguments');
                        const cond = evaluate(elems[1], env);
                        if (!isFalsy(cond)) {
                            return evaluate(elems[2], env);
                        }
                        else if (elems.length > 3) {
                            return evaluate(elems[3], env);
                        }
                        return { tag: 'void' };
                    }
                    case 'quote': {
                        if (elems.length !== 2)
                            throw new EvalError('quote requires exactly 1 argument');
                        return listToConsPairs(elems[1]);
                    }
                    case 'lambda': {
                        if (elems.length < 3)
                            throw new EvalError('lambda requires params and body');
                        const params = elems[1];
                        if (params.tag !== 'list')
                            throw new EvalError('lambda params must be a list');
                        const paramNames = params.value.map(p => {
                            if (p.tag !== 'symbol')
                                throw new EvalError('parameter must be a symbol');
                            return p.value;
                        });
                        const bodyExprs = elems.slice(2);
                        return { tag: 'procedure', value: (...args) => {
                                const childEnv = new Env(env);
                                for (let i = 0; i < paramNames.length; i++) {
                                    childEnv.set(paramNames[i], args[i]);
                                }
                                let result = { tag: 'void' };
                                for (const b of bodyExprs) {
                                    result = evaluate(b, childEnv);
                                }
                                return result;
                            } };
                    }
                    case 'and': {
                        if (elems.length === 1)
                            return { tag: 'boolean', value: true };
                        let result = { tag: 'boolean', value: true };
                        for (let i = 1; i < elems.length; i++) {
                            result = evaluate(elems[i], env);
                            if (isFalsy(result))
                                return result;
                        }
                        return result;
                    }
                    case 'or': {
                        if (elems.length === 1)
                            return { tag: 'boolean', value: false };
                        let result = { tag: 'boolean', value: false };
                        for (let i = 1; i < elems.length; i++) {
                            result = evaluate(elems[i], env);
                            if (!isFalsy(result))
                                return result;
                        }
                        return result;
                    }
                    case 'begin': {
                        let result = { tag: 'void' };
                        for (let i = 1; i < elems.length; i++) {
                            result = evaluate(elems[i], env);
                        }
                        return result;
                    }
                    case 'let': {
                        if (elems.length < 3)
                            throw new EvalError('let requires bindings and body');
                        // Named let: (let name ((var init) ...) body ...)
                        if (elems[1].tag === 'symbol') {
                            if (elems.length < 4)
                                throw new EvalError('named let requires bindings and body');
                            const loopName = elems[1].value;
                            const bindingsList = elems[2];
                            if (bindingsList.tag !== 'list')
                                throw new EvalError('let bindings must be a list');
                            const paramNames = [];
                            const initVals = [];
                            for (const binding of bindingsList.value) {
                                if (binding.tag !== 'list' || binding.value.length !== 2)
                                    throw new EvalError('invalid let binding');
                                if (binding.value[0].tag !== 'symbol')
                                    throw new EvalError('let binding name must be a symbol');
                                paramNames.push(binding.value[0].value);
                                initVals.push(evaluate(binding.value[1], env));
                            }
                            const bodyExprs = elems.slice(3);
                            const childEnv = new Env(env);
                            const loopProc = { tag: 'procedure', value: (...args) => {
                                    const innerEnv = new Env(env);
                                    for (let i = 0; i < paramNames.length; i++) {
                                        innerEnv.set(paramNames[i], args[i]);
                                    }
                                    innerEnv.set(loopName, loopProc);
                                    let result = { tag: 'void' };
                                    for (const b of bodyExprs)
                                        result = evaluate(b, innerEnv);
                                    return result;
                                } };
                            childEnv.set(loopName, loopProc);
                            for (let i = 0; i < paramNames.length; i++) {
                                childEnv.set(paramNames[i], initVals[i]);
                            }
                            let result = { tag: 'void' };
                            for (const b of bodyExprs)
                                result = evaluate(b, childEnv);
                            return result;
                        }
                        // Regular let: (let ((var init) ...) body ...)
                        const bindings = elems[1];
                        if (bindings.tag !== 'list')
                            throw new EvalError('let bindings must be a list');
                        const childEnv = new Env(env);
                        for (const binding of bindings.value) {
                            if (binding.tag !== 'list' || binding.value.length !== 2)
                                throw new EvalError('invalid let binding');
                            const name = binding.value[0];
                            if (name.tag !== 'symbol')
                                throw new EvalError('let binding name must be a symbol');
                            const val = evaluate(binding.value[1], env);
                            childEnv.set(name.value, val);
                        }
                        let result = { tag: 'void' };
                        for (let i = 2; i < elems.length; i++) {
                            result = evaluate(elems[i], childEnv);
                        }
                        return result;
                    }
                    case 'cond': {
                        for (let i = 1; i < elems.length; i++) {
                            const clause = elems[i];
                            if (clause.tag !== 'list' || clause.value.length < 1)
                                throw new EvalError('invalid cond clause');
                            const test = clause.value[0];
                            if (test.tag === 'symbol' && test.value === 'else') {
                                let result = { tag: 'void' };
                                for (let j = 1; j < clause.value.length; j++) {
                                    result = evaluate(clause.value[j], env);
                                }
                                return result;
                            }
                            const testVal = evaluate(test, env);
                            if (!isFalsy(testVal)) {
                                if (clause.value.length === 1)
                                    return testVal;
                                let result = { tag: 'void' };
                                for (let j = 1; j < clause.value.length; j++) {
                                    result = evaluate(clause.value[j], env);
                                }
                                return result;
                            }
                        }
                        return { tag: 'void' };
                    }
                }
            }
            // Function application
            const func = evaluate(first, env);
            if (func.tag !== 'procedure')
                throw new EvalError('not a procedure');
            const args = elems.slice(1).map(a => evaluate(a, env));
            return func.value(...args);
        }
        default:
            throw new EvalError('cannot evaluate');
    }
}
function display(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'nil': return '()';
        case 'list': return `(${val.value.map(display).join(' ')})`;
        case 'pair': {
            let parts = [];
            let cur = val;
            while (cur.tag === 'pair') {
                parts.push(display(cur.car));
                cur = cur.cdr;
            }
            if (cur.tag === 'nil') {
                return `(${parts.join(' ')})`;
            }
            return `(${parts.join(' ')} . ${display(cur)})`;
        }
        case 'void': return '';
        case 'procedure': return '#<procedure>';
    }
}
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input) {
    const tokens = tokenize(input);
    const exprs = parse(tokens);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const env = makeGlobalEnv();
    let result = { tag: 'void' };
    for (const expr of exprs) {
        result = evaluate(expr, env);
    }
    return display(result);
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input) {
    throw new EvalError('not implemented');
}
