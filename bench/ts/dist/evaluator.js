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
    mutate(name, val, pos) {
        if (this.bindings.has(name)) {
            this.bindings.set(name, val);
            return;
        }
        if (this.parent) {
            this.parent.mutate(name, val, pos);
            return;
        }
        throw posError(`set!: unbound variable: ${name}`, pos);
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
    if (token.startsWith('#\\')) {
        const charName = token.slice(2);
        if (charName === 'space')
            return { tag: 'char', value: ' ', pos };
        if (charName === 'newline')
            return { tag: 'char', value: '\n', pos };
        if (charName === 'tab')
            return { tag: 'char', value: '\t', pos };
        if (charName.length === 1)
            return { tag: 'char', value: charName, pos };
        throw posError(`bad character literal: ${token}`, pos);
    }
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
function parseParams(elements, errPos) {
    const dotIdx = elements.findIndex(e => e.tag === 'symbol' && e.value === '.');
    if (dotIdx === -1) {
        return { params: elements.map(p => { if (p.tag !== 'symbol')
                throw posError('param must be symbol', errPos); return p.value; }) };
    }
    if (dotIdx !== elements.length - 2)
        throw posError('bad dot syntax in params', errPos);
    const last = elements[elements.length - 1];
    if (last.tag !== 'symbol')
        throw posError('rest param must be symbol', errPos);
    const params = elements.slice(0, dotIdx).map(p => { if (p.tag !== 'symbol')
        throw posError('param must be symbol', errPos); return p.value; });
    return { params, restParam: last.value };
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
function evaluate(initExpr, initEnv) {
    let expr = initExpr;
    let env = initEnv;
    for (;;) {
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
                    if (isTruthy(cond)) {
                        expr = elems[2];
                        continue;
                    }
                    if (elems.length === 4) {
                        expr = elems[3];
                        continue;
                    }
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
                        const { params, restParam } = parseParams(target.elements.slice(1), expr.pos);
                        env.set(fnName, { tag: 'lambda', params, restParam, body: elems.slice(2), env });
                        return { tag: 'void' };
                    }
                    throw posError('define: bad syntax', expr.pos);
                }
                case 'lambda': {
                    if (elems.length < 3)
                        throw posError('lambda: bad syntax', expr.pos);
                    const paramList = elems[1];
                    if (paramList.tag === 'symbol') {
                        // (lambda args body...) — all args as rest
                        return { tag: 'lambda', params: [], restParam: paramList.value, body: elems.slice(2), env };
                    }
                    if (paramList.tag !== 'list')
                        throw posError('lambda: params must be a list', expr.pos);
                    const { params, restParam } = parseParams(paramList.elements, expr.pos);
                    return { tag: 'lambda', params, restParam, body: elems.slice(2), env };
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
                        loopLambda.env = letEnv;
                        const args = initExprs.map(e => evaluate(e, env));
                        const callEnv = new Env(letEnv);
                        for (let i = 0; i < paramNames.length; i++)
                            callEnv.set(paramNames[i], args[i]);
                        for (let i = 0; i < body.length - 1; i++)
                            evaluate(body[i], callEnv);
                        expr = body[body.length - 1];
                        env = callEnv;
                        continue;
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
                    for (let i = 2; i < elems.length - 1; i++)
                        evaluate(elems[i], letEnv2);
                    expr = elems[elems.length - 1];
                    env = letEnv2;
                    continue;
                }
                case 'set!': {
                    if (elems.length !== 3)
                        throw posError('set!: bad syntax', expr.pos);
                    const target = elems[1];
                    if (target.tag !== 'symbol')
                        throw posError('set!: target must be a symbol', expr.pos);
                    const val = evaluate(elems[2], env);
                    env.mutate(target.value, val, expr.pos);
                    return { tag: 'void' };
                }
                case 'begin': {
                    if (elems.length === 1)
                        return { tag: 'void' };
                    for (let i = 1; i < elems.length - 1; i++)
                        evaluate(elems[i], env);
                    expr = elems[elems.length - 1];
                    continue;
                }
                case 'cond': {
                    let matched = false;
                    for (let i = 1; i < elems.length; i++) {
                        const clause = elems[i];
                        if (clause.tag !== 'list' || clause.elements.length < 2)
                            throw posError('cond: bad clause', expr.pos);
                        if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
                            for (let j = 1; j < clause.elements.length - 1; j++)
                                evaluate(clause.elements[j], env);
                            expr = clause.elements[clause.elements.length - 1];
                            matched = true;
                            break;
                        }
                        const test = evaluate(clause.elements[0], env);
                        if (isTruthy(test)) {
                            for (let j = 1; j < clause.elements.length - 1; j++)
                                evaluate(clause.elements[j], env);
                            expr = clause.elements[clause.elements.length - 1];
                            matched = true;
                            break;
                        }
                    }
                    if (matched)
                        continue;
                    return { tag: 'void' };
                }
                case 'and': {
                    if (elems.length === 1)
                        return { tag: 'boolean', value: true };
                    for (let i = 1; i < elems.length - 1; i++) {
                        const result = evaluate(elems[i], env);
                        if (!isTruthy(result))
                            return result;
                    }
                    expr = elems[elems.length - 1];
                    continue;
                }
                case 'or': {
                    if (elems.length === 1)
                        return { tag: 'boolean', value: false };
                    for (let i = 1; i < elems.length - 1; i++) {
                        const result = evaluate(elems[i], env);
                        if (isTruthy(result))
                            return result;
                    }
                    expr = elems[elems.length - 1];
                    continue;
                }
            }
        }
        // Function application
        const proc = evaluate(head, env);
        const args = elems.slice(1).map(e => evaluate(e, env));
        if (proc.tag === 'lambda') {
            if (proc.restParam) {
                if (args.length < proc.params.length)
                    throw posError('wrong number of arguments', expr.pos);
            }
            else {
                if (args.length !== proc.params.length)
                    throw posError('wrong number of arguments', expr.pos);
            }
            const callEnv = new Env(proc.env);
            for (let i = 0; i < proc.params.length; i++)
                callEnv.set(proc.params[i], args[i]);
            if (proc.restParam)
                callEnv.set(proc.restParam, { tag: 'list', elements: args.slice(proc.params.length) });
            for (let i = 0; i < proc.body.length - 1; i++)
                evaluate(proc.body[i], callEnv);
            expr = proc.body[proc.body.length - 1];
            env = callEnv;
            continue;
        }
        if (proc.tag === 'builtin')
            return proc.fn(args, expr.pos);
        throw posError('not a procedure', expr.pos);
    }
}
// ── Builtins ──────────────────────────────────────────────────────
function makeGlobalEnv(output = []) {
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
    // Apply
    defBuiltin('apply', (args, p) => {
        if (args.length < 2)
            throw posError('apply: need at least 2 arguments', p);
        const fn = args[0];
        const lastArg = args[args.length - 1];
        if (lastArg.tag !== 'list')
            throw posError('apply: last argument must be a list', p);
        const allArgs = [...args.slice(1, -1), ...lastArg.elements];
        if (fn.tag === 'lambda') {
            if (fn.restParam) {
                if (allArgs.length < fn.params.length)
                    throw posError('wrong number of arguments', p);
            }
            else {
                if (allArgs.length !== fn.params.length)
                    throw posError('wrong number of arguments', p);
            }
            const callEnv = new Env(fn.env);
            for (let i = 0; i < fn.params.length; i++)
                callEnv.set(fn.params[i], allArgs[i]);
            if (fn.restParam)
                callEnv.set(fn.restParam, { tag: 'list', elements: allArgs.slice(fn.params.length) });
            let result = { tag: 'void' };
            for (const bodyExpr of fn.body)
                result = evaluate(bodyExpr, callEnv);
            return result;
        }
        if (fn.tag === 'builtin')
            return fn.fn(allArgs, p);
        throw posError('apply: not a procedure', p);
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
    defBuiltin('char?', (args, p) => { if (args.length !== 1)
        throw posError('char?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'char' }; });
    // Output
    defBuiltin('display', (args, p) => {
        if (args.length !== 1)
            throw posError('display: need 1 argument', p);
        output.push(displayForDisplay(args[0]));
        return { tag: 'void' };
    });
    defBuiltin('write', (args, p) => {
        if (args.length !== 1)
            throw posError('write: need 1 argument', p);
        output.push(writeVal(args[0]));
        return { tag: 'void' };
    });
    defBuiltin('newline', (args, p) => {
        if (args.length !== 0)
            throw posError('newline: need 0 arguments', p);
        output.push('\n');
        return { tag: 'void' };
    });
    // String operations
    defBuiltin('string-append', (args, p) => {
        let result = '';
        for (const a of args) {
            if (a.tag !== 'string')
                throw posError('string-append: expected string', p);
            result += a.value;
        }
        return { tag: 'string', value: result };
    });
    defBuiltin('string-length', (args, p) => {
        if (args.length !== 1 || args[0].tag !== 'string')
            throw posError('string-length: expected string', p);
        return { tag: 'number', value: args[0].value.length };
    });
    defBuiltin('substring', (args, p) => {
        if (args.length !== 3)
            throw posError('substring: need 3 arguments', p);
        if (args[0].tag !== 'string')
            throw posError('substring: expected string', p);
        const start = expectNumber(args[1], 'substring', p);
        const end = expectNumber(args[2], 'substring', p);
        return { tag: 'string', value: args[0].value.slice(start, end) };
    });
    defBuiltin('string->number', (args, p) => {
        if (args.length !== 1 || args[0].tag !== 'string')
            throw posError('string->number: expected string', p);
        const n = Number(args[0].value);
        if (isNaN(n))
            return { tag: 'boolean', value: false };
        return { tag: 'number', value: n };
    });
    defBuiltin('number->string', (args, p) => {
        if (args.length !== 1)
            throw posError('number->string: need 1 argument', p);
        return { tag: 'string', value: String(expectNumber(args[0], 'number->string', p)) };
    });
    defBuiltin('symbol->string', (args, p) => {
        if (args.length !== 1 || args[0].tag !== 'symbol')
            throw posError('symbol->string: expected symbol', p);
        return { tag: 'string', value: args[0].value };
    });
    defBuiltin('string->symbol', (args, p) => {
        if (args.length !== 1 || args[0].tag !== 'string')
            throw posError('string->symbol: expected string', p);
        return { tag: 'symbol', value: args[0].value };
    });
    defBuiltin('string-ref', (args, p) => {
        if (args.length !== 2)
            throw posError('string-ref: need 2 arguments', p);
        if (args[0].tag !== 'string')
            throw posError('string-ref: expected string', p);
        const idx = expectNumber(args[1], 'string-ref', p);
        if (idx < 0 || idx >= args[0].value.length)
            throw posError('string-ref: index out of range', p);
        return { tag: 'char', value: args[0].value[idx] };
    });
    defBuiltin('string-set!', (args, p) => {
        if (args.length !== 3)
            throw posError('string-set!: need 3 arguments', p);
        const str = args[0];
        if (str.tag !== 'string')
            throw posError('string-set!: expected string', p);
        const idx = expectNumber(args[1], 'string-set!', p);
        const ch = args[2];
        if (ch.tag !== 'char')
            throw posError('string-set!: expected char', p);
        if (idx < 0 || idx >= str.value.length)
            throw posError('string-set!: index out of range', p);
        str.value = str.value.slice(0, idx) + ch.value + str.value.slice(idx + 1);
        return { tag: 'void' };
    });
    defBuiltin('string-copy', (args, p) => {
        if (args.length !== 1)
            throw posError('string-copy: need 1 argument', p);
        if (args[0].tag !== 'string')
            throw posError('string-copy: expected string', p);
        return { tag: 'string', value: args[0].value };
    });
    return env;
}
// ── Display ────────────────────────────────────────────────────────
function displayVal(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
        case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
        case 'void': return '';
        case 'lambda': return '#<procedure>';
        case 'builtin': return '#<procedure>';
    }
}
function writeVal(val) {
    return displayVal(val);
}
function displayForDisplay(val) {
    switch (val.tag) {
        case 'string': return val.value;
        case 'list': return `(${val.elements.map(displayForDisplay).join(' ')})`;
        case 'char': return val.value;
        default: return displayVal(val);
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
    const exprs = parseAll(input);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const output = [];
    const env = makeGlobalEnv(output);
    let result = { tag: 'void' };
    for (const expr of exprs)
        result = evaluate(expr, env);
    return { result: displayVal(result), output: output.join('') };
}
