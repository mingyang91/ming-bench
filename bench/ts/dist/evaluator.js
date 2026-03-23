import { EvalError } from './evalError.js';
function posError(msg, pos) {
    if (pos)
        return new EvalError(`${pos.line}:${pos.col}: ${msg}`);
    return new EvalError(msg);
}
// ── Environment ───────────────────────────────────────────────────
class Env {
    parent;
    bindings = new Map();
    constructor(parent = null) {
        this.parent = parent;
    }
    get(name, pos) {
        const val = this.bindings.get(name);
        if (val !== undefined)
            return val;
        if (this.parent)
            return this.parent.get(name, pos);
        throw posError(`unbound variable: ${name}`, pos);
    }
    set(name, val) {
        this.bindings.set(name, val);
    }
}
function tokenize(input) {
    const tokens = [];
    let i = 0;
    let line = 1;
    let col = 1;
    while (i < input.length) {
        const ch = input[i];
        if (ch === '\n') {
            i++;
            line++;
            col = 1;
            continue;
        }
        if (ch === ' ' || ch === '\t' || ch === '\r') {
            i++;
            col++;
            continue;
        }
        if (ch === ';') {
            while (i < input.length && input[i] !== '\n') {
                i++;
                col++;
            }
            continue;
        }
        const startPos = { line, col };
        if (ch === '(' || ch === ')') {
            tokens.push({ text: ch, pos: startPos });
            i++;
            col++;
            continue;
        }
        if (ch === "'") {
            tokens.push({ text: "'", pos: startPos });
            i++;
            col++;
            continue;
        }
        if (ch === '"') {
            let s = '"';
            i++;
            col++;
            while (i < input.length && input[i] !== '"') {
                if (input[i] === '\\') {
                    s += input[i];
                    i++;
                    col++;
                    if (i < input.length) {
                        s += input[i];
                        i++;
                        col++;
                    }
                    continue;
                }
                if (input[i] === '\n') {
                    line++;
                    col = 1;
                }
                else {
                    col++;
                }
                s += input[i];
                i++;
            }
            if (i < input.length) {
                s += '"';
                i++;
                col++;
            }
            tokens.push({ text: s, pos: startPos });
            continue;
        }
        let atom = '';
        while (i < input.length && !" \t\n\r();\"'".includes(input[i])) {
            atom += input[i];
            i++;
            col++;
        }
        if (atom.length > 0)
            tokens.push({ text: atom, pos: startPos });
    }
    return tokens;
}
function parse(tokens, cur) {
    if (cur.i >= tokens.length)
        throw new EvalError('unexpected end of input');
    const tok = tokens[cur.i];
    if (tok.text === "'") {
        cur.i++;
        const quoted = parse(tokens, cur);
        return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, quoted], pos: tok.pos };
    }
    if (tok.text === '(') {
        cur.i++;
        const elements = [];
        while (cur.i < tokens.length && tokens[cur.i].text !== ')')
            elements.push(parse(tokens, cur));
        if (cur.i >= tokens.length)
            throw posError('missing closing paren', tok.pos);
        cur.i++;
        return { tag: 'list', elements, pos: tok.pos };
    }
    if (tok.text === ')')
        throw posError('unexpected )', tok.pos);
    cur.i++;
    return parseAtom(tok.text, tok.pos);
}
function parseAtom(token, pos) {
    if (token === '#t')
        return { tag: 'boolean', value: true, pos };
    if (token === '#f')
        return { tag: 'boolean', value: false, pos };
    if (token.startsWith('"') && token.endsWith('"')) {
        const inner = token.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
        return { tag: 'string', value: inner, pos };
    }
    const num = Number(token);
    if (!isNaN(num) && token !== '')
        return { tag: 'number', value: num, pos };
    return { tag: 'symbol', value: token, pos };
}
function parseAll(input) {
    const tokens = tokenize(input);
    const exprs = [];
    const cur = { i: 0 };
    while (cur.i < tokens.length)
        exprs.push(parse(tokens, cur));
    return exprs;
}
// ── Evaluator ──────────────────────────────────────────────────────
function isTruthy(val) {
    return !(val.tag === 'boolean' && val.value === false);
}
function expectNumber(val, op, p) {
    if (val.tag !== 'number')
        throw posError(`${op}: expected number`, p);
    return val.value;
}
function evaluate(expr, env) {
    if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string')
        return expr;
    if (expr.tag === 'symbol')
        return env.get(expr.value, expr.pos);
    if (expr.tag !== 'list')
        return expr;
    const elems = expr.elements;
    if (elems.length === 0)
        throw posError('empty application', expr.pos);
    const head = elems[0];
    // Special forms
    if (head.tag === 'symbol') {
        switch (head.value) {
            case 'quote':
                if (elems.length !== 2)
                    throw posError('quote: need 1 argument', expr.pos);
                return elems[1];
            case 'if': {
                if (elems.length < 3 || elems.length > 4)
                    throw posError('if: bad syntax', expr.pos);
                const cond = evaluate(elems[1], env);
                if (isTruthy(cond))
                    return evaluate(elems[2], env);
                if (elems.length === 4)
                    return evaluate(elems[3], env);
                return { tag: 'void' };
            }
            case 'define': {
                if (elems.length < 3)
                    throw posError('define: bad syntax', expr.pos);
                const target = elems[1];
                if (target.tag === 'symbol') {
                    env.set(target.value, evaluate(elems[2], env));
                    return { tag: 'void' };
                }
                if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
                    const fnName = target.elements[0].value;
                    const params = target.elements.slice(1).map(p => {
                        if (p.tag !== 'symbol')
                            throw posError('define: param must be symbol', expr.pos);
                        return p.value;
                    });
                    env.set(fnName, { tag: 'lambda', params, body: elems.slice(2), env });
                    return { tag: 'void' };
                }
                throw posError('define: bad syntax', expr.pos);
            }
            case 'lambda': {
                if (elems.length < 3)
                    throw posError('lambda: bad syntax', expr.pos);
                const paramList = elems[1];
                if (paramList.tag !== 'list')
                    throw posError('lambda: params must be a list', expr.pos);
                const params = paramList.elements.map(p => {
                    if (p.tag !== 'symbol')
                        throw posError('lambda: param must be symbol', expr.pos);
                    return p.value;
                });
                return { tag: 'lambda', params, body: elems.slice(2), env };
            }
            case 'let': {
                if (elems.length < 3)
                    throw posError('let: bad syntax', expr.pos);
                // Named let: (let name ((var init) ...) body ...)
                if (elems[1].tag === 'symbol') {
                    const loopName = elems[1].value;
                    if (elems.length < 4)
                        throw posError('let: bad syntax', expr.pos);
                    const bindingList = elems[2];
                    if (bindingList.tag !== 'list')
                        throw posError('let: bindings must be a list', expr.pos);
                    const paramNames = [];
                    const initExprs = [];
                    for (const b of bindingList.elements) {
                        if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                            throw posError('let: bad binding', expr.pos);
                        paramNames.push(b.elements[0].value);
                        initExprs.push(b.elements[1]);
                    }
                    const body = elems.slice(3);
                    const loopLambda = { tag: 'lambda', params: paramNames, body, env };
                    const letEnv = new Env(env);
                    letEnv.set(loopName, loopLambda);
                    // Update closure env so the lambda can see itself
                    loopLambda.env = letEnv;
                    const args = initExprs.map(e => evaluate(e, env));
                    const callEnv = new Env(letEnv);
                    for (let i = 0; i < paramNames.length; i++)
                        callEnv.set(paramNames[i], args[i]);
                    let result = { tag: 'void' };
                    for (const bodyExpr of body)
                        result = evaluate(bodyExpr, callEnv);
                    return result;
                }
                // Regular let: (let ((var init) ...) body ...)
                const bindings = elems[1];
                if (bindings.tag !== 'list')
                    throw posError('let: bindings must be a list', expr.pos);
                const letEnv2 = new Env(env);
                for (const b of bindings.elements) {
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                        throw posError('let: bad binding', expr.pos);
                    letEnv2.set(b.elements[0].value, evaluate(b.elements[1], env));
                }
                let letResult = { tag: 'void' };
                for (let i = 2; i < elems.length; i++)
                    letResult = evaluate(elems[i], letEnv2);
                return letResult;
            }
            case 'begin': {
                let result = { tag: 'void' };
                for (let i = 1; i < elems.length; i++)
                    result = evaluate(elems[i], env);
                return result;
            }
            case 'cond': {
                for (let i = 1; i < elems.length; i++) {
                    const clause = elems[i];
                    if (clause.tag !== 'list' || clause.elements.length < 2)
                        throw posError('cond: bad clause', expr.pos);
                    if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
                        let result = { tag: 'void' };
                        for (let j = 1; j < clause.elements.length; j++)
                            result = evaluate(clause.elements[j], env);
                        return result;
                    }
                    const test = evaluate(clause.elements[0], env);
                    if (isTruthy(test)) {
                        let result = { tag: 'void' };
                        for (let j = 1; j < clause.elements.length; j++)
                            result = evaluate(clause.elements[j], env);
                        return result;
                    }
                }
                return { tag: 'void' };
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
            throw posError('wrong number of arguments', expr.pos);
        const callEnv = new Env(proc.env);
        for (let i = 0; i < proc.params.length; i++)
            callEnv.set(proc.params[i], args[i]);
        let result = { tag: 'void' };
        for (const bodyExpr of proc.body)
            result = evaluate(bodyExpr, callEnv);
        return result;
    }
    if (proc.tag === 'builtin')
        return proc.fn(args, expr.pos);
    throw posError('not a procedure', expr.pos);
}
// ── Builtins ──────────────────────────────────────────────────────
function makeGlobalEnv() {
    const env = new Env();
    function defBuiltin(name, fn) {
        env.set(name, { tag: 'builtin', name, fn });
    }
    defBuiltin('+', (args, p) => { let s = 0; for (const a of args)
        s += expectNumber(a, '+', p); return { tag: 'number', value: s }; });
    defBuiltin('-', (args, p) => {
        if (args.length === 0)
            throw posError('-: need at least 1 argument', p);
        if (args.length === 1)
            return { tag: 'number', value: -expectNumber(args[0], '-', p) };
        let r = expectNumber(args[0], '-', p);
        for (let i = 1; i < args.length; i++)
            r -= expectNumber(args[i], '-', p);
        return { tag: 'number', value: r };
    });
    defBuiltin('*', (args, p) => { let s = 1; for (const a of args)
        s *= expectNumber(a, '*', p); return { tag: 'number', value: s }; });
    defBuiltin('/', (args, p) => {
        if (args.length < 2)
            throw posError('/: need at least 2 arguments', p);
        let r = expectNumber(args[0], '/', p);
        for (let i = 1; i < args.length; i++) {
            const d = expectNumber(args[i], '/', p);
            if (d === 0)
                throw posError('division by zero', p);
            r = Math.trunc(r / d);
        }
        return { tag: 'number', value: r };
    });
    defBuiltin('<', (args, p) => { if (args.length !== 2)
        throw posError('<: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '<', p) < expectNumber(args[1], '<', p) }; });
    defBuiltin('>', (args, p) => { if (args.length !== 2)
        throw posError('>: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '>', p) > expectNumber(args[1], '>', p) }; });
    defBuiltin('=', (args, p) => { if (args.length !== 2)
        throw posError('=: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '=', p) === expectNumber(args[1], '=', p) }; });
    defBuiltin('<=', (args, p) => { if (args.length !== 2)
        throw posError('<=: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '<=', p) <= expectNumber(args[1], '<=', p) }; });
    defBuiltin('>=', (args, p) => { if (args.length !== 2)
        throw posError('>=: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '>=', p) >= expectNumber(args[1], '>=', p) }; });
    defBuiltin('not', (args, p) => { if (args.length !== 1)
        throw posError('not: need 1 argument', p); return { tag: 'boolean', value: !isTruthy(args[0]) }; });
    // List operations
    defBuiltin('cons', (args, p) => {
        if (args.length !== 2)
            throw posError('cons: need 2 arguments', p);
        const cdr = args[1];
        if (cdr.tag === 'list')
            return { tag: 'list', elements: [args[0], ...cdr.elements] };
        return { tag: 'list', elements: [args[0], { tag: 'symbol', value: '.' }, cdr] };
    });
    defBuiltin('car', (args, p) => {
        if (args.length !== 1)
            throw posError('car: need 1 argument', p);
        if (args[0].tag !== 'list' || args[0].elements.length === 0)
            throw posError('car: not a pair', p);
        return args[0].elements[0];
    });
    defBuiltin('cdr', (args, p) => {
        if (args.length !== 1)
            throw posError('cdr: need 1 argument', p);
        if (args[0].tag !== 'list' || args[0].elements.length === 0)
            throw posError('cdr: not a pair', p);
        return { tag: 'list', elements: args[0].elements.slice(1) };
    });
    defBuiltin('null?', (args, p) => {
        if (args.length !== 1)
            throw posError('null?: need 1 argument', p);
        return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length === 0 };
    });
    defBuiltin('list', args => {
        return { tag: 'list', elements: args };
    });
    defBuiltin('length', (args, p) => {
        if (args.length !== 1 || args[0].tag !== 'list')
            throw posError('length: need a list', p);
        return { tag: 'number', value: args[0].elements.length };
    });
    defBuiltin('append', (args, p) => {
        const result = [];
        for (const a of args) {
            if (a.tag !== 'list')
                throw posError('append: not a list', p);
            result.push(...a.elements);
        }
        return { tag: 'list', elements: result };
    });
    // Type predicates
    defBuiltin('number?', (args, p) => { if (args.length !== 1)
        throw posError('number?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'number' }; });
    defBuiltin('string?', (args, p) => { if (args.length !== 1)
        throw posError('string?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'string' }; });
    defBuiltin('boolean?', (args, p) => { if (args.length !== 1)
        throw posError('boolean?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'boolean' }; });
    defBuiltin('pair?', (args, p) => { if (args.length !== 1)
        throw posError('pair?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length > 0 }; });
    defBuiltin('symbol?', (args, p) => { if (args.length !== 1)
        throw posError('symbol?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'symbol' }; });
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
