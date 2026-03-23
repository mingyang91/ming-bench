import { EvalError } from './evalError.js';
// ── Environment ────────────────────────────────────────────────────
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
    define(name, val) {
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
        while (i < input.length && !("() \t\n\r;'".includes(input[i]))) {
            atom += input[i];
            i++;
        }
        if (atom.length > 0)
            tokens.push(atom);
    }
    return tokens;
}
function parseTokens(tokens, pos) {
    if (pos >= tokens.length)
        throw new EvalError('unexpected end of input');
    const tok = tokens[pos];
    if (tok === "'") {
        const [val, next] = parseTokens(tokens, pos + 1);
        return [{ tag: 'list', value: [{ tag: 'symbol', value: 'quote' }, val] }, next];
    }
    if (tok === '(') {
        const items = [];
        pos++;
        while (pos < tokens.length && tokens[pos] !== ')') {
            const [val, next] = parseTokens(tokens, pos);
            items.push(val);
            pos = next;
        }
        if (pos >= tokens.length)
            throw new EvalError('missing closing paren');
        return [{ tag: 'list', value: items }, pos + 1];
    }
    if (tok === ')')
        throw new EvalError('unexpected )');
    return [parseAtom(tok), pos + 1];
}
function parseAtom(tok) {
    if (tok === '#t')
        return { tag: 'boolean', value: true };
    if (tok === '#f')
        return { tag: 'boolean', value: false };
    if (tok.startsWith('"') && tok.endsWith('"')) {
        const inner = tok.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
        return { tag: 'string', value: inner };
    }
    const num = Number(tok);
    if (!isNaN(num) && tok !== '') {
        return { tag: 'number', value: num };
    }
    return { tag: 'symbol', value: tok };
}
function parse(input) {
    const tokens = tokenize(input);
    const exprs = [];
    let pos = 0;
    while (pos < tokens.length) {
        const [val, next] = parseTokens(tokens, pos);
        exprs.push(val);
        pos = next;
    }
    return exprs;
}
// ── Evaluator ──────────────────────────────────────────────────────
const NIL = { tag: 'nil' };
function listToPairs(items) {
    let result = NIL;
    for (let i = items.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: items[i], cdr: result };
    }
    return result;
}
function astToPairs(val) {
    if (val.tag === 'list') {
        return listToPairs(val.value.map(astToPairs));
    }
    return val;
}
function pairsToArray(val) {
    const result = [];
    let cur = val;
    while (cur.tag === 'pair') {
        result.push(cur.car);
        cur = cur.cdr;
    }
    return result;
}
function isTruthy(val) {
    return !(val.tag === 'boolean' && val.value === false);
}
function expectNumber(val, op) {
    if (val.tag !== 'number')
        throw new EvalError(`${op}: expected number`);
    return val.value;
}
function makeGlobalEnv() {
    const env = new Env();
    function defBuiltin(name, fn) {
        env.define(name, { tag: 'builtin', name, fn });
    }
    defBuiltin('+', (args) => {
        let sum = 0;
        for (const a of args)
            sum += expectNumber(a, '+');
        return { tag: 'number', value: sum };
    });
    defBuiltin('-', (args) => {
        if (args.length < 1)
            throw new EvalError('-: need at least 1 argument');
        if (args.length === 1)
            return { tag: 'number', value: -expectNumber(args[0], '-') };
        let result = expectNumber(args[0], '-');
        for (let i = 1; i < args.length; i++)
            result -= expectNumber(args[i], '-');
        return { tag: 'number', value: result };
    });
    defBuiltin('*', (args) => {
        let prod = 1;
        for (const a of args)
            prod *= expectNumber(a, '*');
        return { tag: 'number', value: prod };
    });
    defBuiltin('/', (args) => {
        if (args.length < 2)
            throw new EvalError('/: need at least 2 arguments');
        let result = expectNumber(args[0], '/');
        for (let i = 1; i < args.length; i++) {
            const d = expectNumber(args[i], '/');
            if (d === 0)
                throw new EvalError('division by zero');
            result = Math.trunc(result / d);
        }
        return { tag: 'number', value: result };
    });
    defBuiltin('<', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '<') < expectNumber(args[1], '<') }));
    defBuiltin('>', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '>') > expectNumber(args[1], '>') }));
    defBuiltin('=', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '=') === expectNumber(args[1], '=') }));
    defBuiltin('<=', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '<=') <= expectNumber(args[1], '<=') }));
    defBuiltin('>=', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '>=') >= expectNumber(args[1], '>=') }));
    defBuiltin('not', (args) => ({ tag: 'boolean', value: !isTruthy(args[0]) }));
    defBuiltin('cons', (args) => ({ tag: 'pair', car: args[0], cdr: args[1] }));
    defBuiltin('car', (args) => {
        if (args[0].tag !== 'pair')
            throw new EvalError('car: not a pair');
        return args[0].car;
    });
    defBuiltin('cdr', (args) => {
        if (args[0].tag !== 'pair')
            throw new EvalError('cdr: not a pair');
        return args[0].cdr;
    });
    defBuiltin('null?', (args) => ({ tag: 'boolean', value: args[0].tag === 'nil' }));
    defBuiltin('list', (args) => listToPairs(args));
    defBuiltin('length', (args) => {
        let len = 0;
        let cur = args[0];
        while (cur.tag === 'pair') {
            len++;
            cur = cur.cdr;
        }
        return { tag: 'number', value: len };
    });
    defBuiltin('append', (args) => {
        if (args.length === 0)
            return NIL;
        if (args.length === 1)
            return args[0];
        let result = args[args.length - 1];
        for (let i = args.length - 2; i >= 0; i--) {
            const elems = pairsToArray(args[i]);
            for (let j = elems.length - 1; j >= 0; j--) {
                result = { tag: 'pair', car: elems[j], cdr: result };
            }
        }
        return result;
    });
    defBuiltin('number?', (args) => ({ tag: 'boolean', value: args[0].tag === 'number' }));
    defBuiltin('string?', (args) => ({ tag: 'boolean', value: args[0].tag === 'string' }));
    defBuiltin('boolean?', (args) => ({ tag: 'boolean', value: args[0].tag === 'boolean' }));
    defBuiltin('pair?', (args) => ({ tag: 'boolean', value: args[0].tag === 'pair' }));
    defBuiltin('symbol?', (args) => ({ tag: 'boolean', value: args[0].tag === 'symbol' }));
    return env;
}
function evaluate(expr, env) {
    if (expr.tag === 'symbol') {
        return env.get(expr.value);
    }
    if (expr.tag !== 'list') {
        return expr; // self-evaluating
    }
    const items = expr.value;
    if (items.length === 0)
        throw new EvalError('empty application');
    const head = items[0];
    if (head.tag === 'symbol') {
        const op = head.value;
        // Special forms
        if (op === 'quote') {
            return astToPairs(items[1]);
        }
        if (op === 'if') {
            const cond = evaluate(items[1], env);
            if (isTruthy(cond)) {
                return evaluate(items[2], env);
            }
            else if (items.length > 3) {
                return evaluate(items[3], env);
            }
            return { tag: 'void' };
        }
        if (op === 'define') {
            if (items[1].tag === 'list') {
                // (define (f params...) body...)
                const nameAndParams = items[1].value;
                const name = nameAndParams[0].value;
                const params = nameAndParams.slice(1).map(p => p.value);
                const body = items.slice(2);
                env.define(name, { tag: 'lambda', params, body, env });
                return { tag: 'void' };
            }
            // (define x expr)
            const name = items[1].value;
            const val = evaluate(items[2], env);
            env.define(name, val);
            return { tag: 'void' };
        }
        if (op === 'lambda') {
            const paramList = items[1];
            const params = paramList.value.map(p => p.value);
            const body = items.slice(2);
            return { tag: 'lambda', params, body, env };
        }
        if (op === 'and') {
            let result = { tag: 'boolean', value: true };
            for (let i = 1; i < items.length; i++) {
                result = evaluate(items[i], env);
                if (!isTruthy(result))
                    return result;
            }
            return result;
        }
        if (op === 'or') {
            let result = { tag: 'boolean', value: false };
            for (let i = 1; i < items.length; i++) {
                result = evaluate(items[i], env);
                if (isTruthy(result))
                    return result;
            }
            return result;
        }
        if (op === 'let') {
            // Named let: (let name ((var val) ...) body...)
            if (items[1].tag === 'symbol') {
                const name = items[1].value;
                const bindings = items[2].value;
                const body = items.slice(3);
                const params = [];
                const inits = [];
                for (const b of bindings) {
                    const bv = b.value;
                    params.push(bv[0].value);
                    inits.push(bv[1]);
                }
                const letEnv = new Env(env);
                const lambda = { tag: 'lambda', params, body, env: letEnv };
                letEnv.define(name, lambda);
                const args = inits.map(i => evaluate(i, env));
                const callEnv = new Env(letEnv);
                for (let i = 0; i < params.length; i++) {
                    callEnv.define(params[i], args[i]);
                }
                let result = { tag: 'void' };
                for (const bodyExpr of body) {
                    result = evaluate(bodyExpr, callEnv);
                }
                return result;
            }
            // Regular let: (let ((var val) ...) body...)
            const bindings = items[1].value;
            const body = items.slice(2);
            const letEnv = new Env(env);
            for (const b of bindings) {
                const bv = b.value;
                const name = bv[0].value;
                const val = evaluate(bv[1], env);
                letEnv.define(name, val);
            }
            let result = { tag: 'void' };
            for (const bodyExpr of body) {
                result = evaluate(bodyExpr, letEnv);
            }
            return result;
        }
        if (op === 'begin') {
            let result = { tag: 'void' };
            for (let i = 1; i < items.length; i++) {
                result = evaluate(items[i], env);
            }
            return result;
        }
        if (op === 'cond') {
            for (let i = 1; i < items.length; i++) {
                const clause = items[i].value;
                if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
                    let result = { tag: 'void' };
                    for (let j = 1; j < clause.length; j++) {
                        result = evaluate(clause[j], env);
                    }
                    return result;
                }
                const test = evaluate(clause[0], env);
                if (isTruthy(test)) {
                    let result = test;
                    for (let j = 1; j < clause.length; j++) {
                        result = evaluate(clause[j], env);
                    }
                    return result;
                }
            }
            return { tag: 'void' };
        }
    }
    // Function application
    const proc = evaluate(head, env);
    const args = items.slice(1).map(a => evaluate(a, env));
    if (proc.tag === 'builtin') {
        return proc.fn(args);
    }
    if (proc.tag === 'lambda') {
        const callEnv = new Env(proc.env);
        for (let i = 0; i < proc.params.length; i++) {
            callEnv.define(proc.params[i], args[i]);
        }
        let result = { tag: 'void' };
        for (const bodyExpr of proc.body) {
            result = evaluate(bodyExpr, callEnv);
        }
        return result;
    }
    throw new EvalError('not a procedure');
}
// ── Display ────────────────────────────────────────────────────────
function display(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'list': return `(${val.value.map(display).join(' ')})`;
        case 'nil': return '()';
        case 'pair': {
            let s = '(' + display(val.car);
            let cur = val.cdr;
            while (cur.tag === 'pair') {
                s += ' ' + display(cur.car);
                cur = cur.cdr;
            }
            if (cur.tag !== 'nil') {
                s += ' . ' + display(cur);
            }
            s += ')';
            return s;
        }
        case 'lambda': return '#<procedure>';
        case 'builtin': return `#<builtin:${val.name}>`;
        case 'void': return '';
    }
}
// ── Public API ─────────────────────────────────────────────────────
export function evalStr(input) {
    const exprs = parse(input);
    if (exprs.length === 0)
        throw new EvalError('empty input');
    const env = makeGlobalEnv();
    let result = { tag: 'void' };
    for (const expr of exprs) {
        result = evaluate(expr, env);
    }
    return display(result);
}
export function evalStrWithOutput(input) {
    throw new EvalError('not implemented');
}
