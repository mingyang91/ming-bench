import { EvalError } from './evalError.js';
// ── Environment ───────────────────────────────────────────────────
class Env {
    parent;
    bindings = new Map();
    constructor(parent = null) {
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
    set(name, val) {
        this.bindings.set(name, val);
    }
}
// ── Parser ─────────────────────────────────────────────────────────
function tokenize(input) {
    const tokens = [];
    let i = 0;
    while (i < input.length) {
        const ch = input[i];
        if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
            i++;
            continue;
        }
        if (ch === ';') {
            while (i < input.length && input[i] !== '\n')
                i++;
            continue;
        }
        if (ch === '(' || ch === ')') {
            tokens.push(ch);
            i++;
            continue;
        }
        if (ch === "'") {
            tokens.push("'");
            i++;
            continue;
        }
        if (ch === '"') {
            let s = '"';
            i++;
            while (i < input.length && input[i] !== '"') {
                if (input[i] === '\\') {
                    s += input[i];
                    i++;
                    if (i < input.length) {
                        s += input[i];
                        i++;
                    }
                    continue;
                }
                s += input[i];
                i++;
            }
            if (i < input.length) {
                s += '"';
                i++;
            }
            tokens.push(s);
            continue;
        }
        let atom = '';
        while (i < input.length && !" \t\n\r();\"'".includes(input[i])) {
            atom += input[i];
            i++;
        }
        if (atom.length > 0)
            tokens.push(atom);
    }
    return tokens;
}
function parse(tokens, pos) {
    if (pos.i >= tokens.length)
        throw new EvalError('unexpected end of input');
    const token = tokens[pos.i];
    if (token === "'") {
        pos.i++;
        const quoted = parse(tokens, pos);
        return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote' }, quoted] };
    }
    if (token === '(') {
        pos.i++;
        const elements = [];
        while (pos.i < tokens.length && tokens[pos.i] !== ')')
            elements.push(parse(tokens, pos));
        if (pos.i >= tokens.length)
            throw new EvalError('missing closing paren');
        pos.i++;
        return { tag: 'list', elements };
    }
    if (token === ')')
        throw new EvalError('unexpected )');
    pos.i++;
    return parseAtom(token);
}
function parseAtom(token) {
    if (token === '#t')
        return { tag: 'boolean', value: true };
    if (token === '#f')
        return { tag: 'boolean', value: false };
    if (token.startsWith('"') && token.endsWith('"')) {
        const inner = token.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
        return { tag: 'string', value: inner };
    }
    const num = Number(token);
    if (!isNaN(num) && token !== '')
        return { tag: 'number', value: num };
    return { tag: 'symbol', value: token };
}
function parseAll(input) {
    const tokens = tokenize(input);
    const exprs = [];
    const pos = { i: 0 };
    while (pos.i < tokens.length)
        exprs.push(parse(tokens, pos));
    return exprs;
}
// ── Evaluator ──────────────────────────────────────────────────────
function isTruthy(val) {
    return !(val.tag === 'boolean' && val.value === false);
}
function expectNumber(val, op) {
    if (val.tag !== 'number')
        throw new EvalError(`${op}: expected number`);
    return val.value;
}
function evaluate(expr, env) {
    if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string')
        return expr;
    if (expr.tag === 'symbol')
        return env.get(expr.value);
    if (expr.tag !== 'list')
        return expr;
    const elems = expr.elements;
    if (elems.length === 0)
        throw new EvalError('empty application');
    const head = elems[0];
    // Special forms
    if (head.tag === 'symbol') {
        switch (head.value) {
            case 'quote':
                if (elems.length !== 2)
                    throw new EvalError('quote: need 1 argument');
                return elems[1];
            case 'if': {
                if (elems.length < 3 || elems.length > 4)
                    throw new EvalError('if: bad syntax');
                const cond = evaluate(elems[1], env);
                if (isTruthy(cond))
                    return evaluate(elems[2], env);
                if (elems.length === 4)
                    return evaluate(elems[3], env);
                return { tag: 'void' };
            }
            case 'define': {
                if (elems.length < 3)
                    throw new EvalError('define: bad syntax');
                const target = elems[1];
                if (target.tag === 'symbol') {
                    env.set(target.value, evaluate(elems[2], env));
                    return { tag: 'void' };
                }
                if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
                    const fnName = target.elements[0].value;
                    const params = target.elements.slice(1).map(p => {
                        if (p.tag !== 'symbol')
                            throw new EvalError('define: param must be symbol');
                        return p.value;
                    });
                    env.set(fnName, { tag: 'lambda', params, body: elems.slice(2), env });
                    return { tag: 'void' };
                }
                throw new EvalError('define: bad syntax');
            }
            case 'lambda': {
                if (elems.length < 3)
                    throw new EvalError('lambda: bad syntax');
                const paramList = elems[1];
                if (paramList.tag !== 'list')
                    throw new EvalError('lambda: params must be a list');
                const params = paramList.elements.map(p => {
                    if (p.tag !== 'symbol')
                        throw new EvalError('lambda: param must be symbol');
                    return p.value;
                });
                return { tag: 'lambda', params, body: elems.slice(2), env };
            }
            case 'and': {
                if (elems.length === 1)
                    return { tag: 'boolean', value: true };
                let result = { tag: 'boolean', value: true };
                for (let i = 1; i < elems.length; i++) {
                    result = evaluate(elems[i], env);
                    if (!isTruthy(result))
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
                    if (isTruthy(result))
                        return result;
                }
                return result;
            }
        }
    }
    // Function application
    const proc = evaluate(head, env);
    const args = elems.slice(1).map(e => evaluate(e, env));
    if (proc.tag === 'lambda') {
        if (args.length !== proc.params.length)
            throw new EvalError('wrong number of arguments');
        const callEnv = new Env(proc.env);
        for (let i = 0; i < proc.params.length; i++)
            callEnv.set(proc.params[i], args[i]);
        let result = { tag: 'void' };
        for (const bodyExpr of proc.body)
            result = evaluate(bodyExpr, callEnv);
        return result;
    }
    if (proc.tag === 'builtin')
        return proc.fn(args);
    throw new EvalError('not a procedure');
}
// ── Builtins ──────────────────────────────────────────────────────
function makeGlobalEnv() {
    const env = new Env();
    function defBuiltin(name, fn) {
        env.set(name, { tag: 'builtin', name, fn });
    }
    defBuiltin('+', args => { let s = 0; for (const a of args)
        s += expectNumber(a, '+'); return { tag: 'number', value: s }; });
    defBuiltin('-', args => {
        if (args.length === 0)
            throw new EvalError('-: need at least 1 argument');
        if (args.length === 1)
            return { tag: 'number', value: -expectNumber(args[0], '-') };
        let r = expectNumber(args[0], '-');
        for (let i = 1; i < args.length; i++)
            r -= expectNumber(args[i], '-');
        return { tag: 'number', value: r };
    });
    defBuiltin('*', args => { let p = 1; for (const a of args)
        p *= expectNumber(a, '*'); return { tag: 'number', value: p }; });
    defBuiltin('/', args => {
        if (args.length < 2)
            throw new EvalError('/: need at least 2 arguments');
        let r = expectNumber(args[0], '/');
        for (let i = 1; i < args.length; i++) {
            const d = expectNumber(args[i], '/');
            if (d === 0)
                throw new EvalError('division by zero');
            r = Math.trunc(r / d);
        }
        return { tag: 'number', value: r };
    });
    defBuiltin('<', args => { if (args.length !== 2)
        throw new EvalError('<: need 2 arguments'); return { tag: 'boolean', value: expectNumber(args[0], '<') < expectNumber(args[1], '<') }; });
    defBuiltin('>', args => { if (args.length !== 2)
        throw new EvalError('>: need 2 arguments'); return { tag: 'boolean', value: expectNumber(args[0], '>') > expectNumber(args[1], '>') }; });
    defBuiltin('=', args => { if (args.length !== 2)
        throw new EvalError('=: need 2 arguments'); return { tag: 'boolean', value: expectNumber(args[0], '=') === expectNumber(args[1], '=') }; });
    defBuiltin('<=', args => { if (args.length !== 2)
        throw new EvalError('<=: need 2 arguments'); return { tag: 'boolean', value: expectNumber(args[0], '<=') <= expectNumber(args[1], '<=') }; });
    defBuiltin('>=', args => { if (args.length !== 2)
        throw new EvalError('>=: need 2 arguments'); return { tag: 'boolean', value: expectNumber(args[0], '>=') >= expectNumber(args[1], '>=') }; });
    defBuiltin('not', args => { if (args.length !== 1)
        throw new EvalError('not: need 1 argument'); return { tag: 'boolean', value: !isTruthy(args[0]) }; });
    return env;
}
// ── Display ────────────────────────────────────────────────────────
function displayVal(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
        case 'void': return '';
        case 'lambda': return '#<procedure>';
        case 'builtin': return '#<procedure>';
    }
}
// ── Public API ─────────────────────────────────────────────────────
export function evalStr(input) {
    const exprs = parseAll(input);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const env = makeGlobalEnv();
    let result = { tag: 'void' };
    for (const expr of exprs)
        result = evaluate(expr, env);
    return displayVal(result);
}
export function evalStrWithOutput(input) {
    return { result: evalStr(input), output: '' };
}
