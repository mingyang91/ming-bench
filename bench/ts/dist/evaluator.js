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
    set(name, val) {
        if (this.bindings.has(name)) {
            this.bindings.set(name, val);
            return;
        }
        if (this.parent) {
            this.parent.set(name, val);
            return;
        }
        throw new EvalError(`set!: unbound variable: ${name}`);
    }
}
function tokenize(input) {
    const tokens = [];
    let i = 0;
    let line = 1;
    let col = 1;
    function advance() {
        if (input[i] === '\n') {
            line++;
            col = 1;
        }
        else {
            col++;
        }
        i++;
    }
    while (i < input.length) {
        const ch = input[i];
        if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
            advance();
            continue;
        }
        if (ch === ';') {
            while (i < input.length && input[i] !== '\n')
                advance();
            continue;
        }
        const startPos = { line, col };
        if (ch === '(' || ch === ')') {
            tokens.push({ text: ch, pos: startPos });
            advance();
            continue;
        }
        if (ch === "'") {
            tokens.push({ text: "'", pos: startPos });
            advance();
            continue;
        }
        if (ch === '"') {
            let s = '"';
            advance();
            while (i < input.length && input[i] !== '"') {
                if (input[i] === '\\') {
                    s += input[i];
                    advance();
                    if (i < input.length) {
                        s += input[i];
                        advance();
                    }
                    continue;
                }
                s += input[i];
                advance();
            }
            if (i < input.length) {
                s += '"';
                advance();
            }
            tokens.push({ text: s, pos: startPos });
            continue;
        }
        let atom = '';
        while (i < input.length && !("() \t\n\r;'".includes(input[i]))) {
            atom += input[i];
            advance();
        }
        if (atom.length > 0)
            tokens.push({ text: atom, pos: startPos });
    }
    return tokens;
}
function parseTokens(tokens, pos) {
    if (pos >= tokens.length)
        throw new EvalError('unexpected end of input');
    const tok = tokens[pos];
    if (tok.text === "'") {
        const [val, next] = parseTokens(tokens, pos + 1);
        return [{ tag: 'list', value: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, val], pos: tok.pos }, next];
    }
    if (tok.text === '(') {
        const items = [];
        pos++;
        while (pos < tokens.length && tokens[pos].text !== ')') {
            const [val, next] = parseTokens(tokens, pos);
            items.push(val);
            pos = next;
        }
        if (pos >= tokens.length)
            throw new EvalError('missing closing paren');
        return [{ tag: 'list', value: items, pos: tok.pos }, pos + 1];
    }
    if (tok.text === ')')
        throw new EvalError('unexpected )');
    const atom = parseAtom(tok.text);
    atom.pos = tok.pos;
    return [atom, pos + 1];
}
function parseAtom(tok) {
    if (tok === '#t')
        return { tag: 'boolean', value: true };
    if (tok === '#f')
        return { tag: 'boolean', value: false };
    if (tok.startsWith('#\\')) {
        const charName = tok.slice(2);
        if (charName === 'space')
            return { tag: 'char', value: ' ' };
        if (charName === 'newline')
            return { tag: 'char', value: '\n' };
        if (charName === 'tab')
            return { tag: 'char', value: '\t' };
        if (charName.length === 1)
            return { tag: 'char', value: charName };
        throw new EvalError(`unknown character name: ${tok}`);
    }
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
    const toks = tokenize(input);
    const exprs = [];
    let pos = 0;
    while (pos < toks.length) {
        const [val, next] = parseTokens(toks, pos);
        exprs.push(val);
        pos = next;
    }
    return exprs;
}
// ── Evaluator ──────────────────────────────────────────────────────
function posStr(p) {
    return p ? `${p.line}:${p.col}: ` : '';
}
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
    defBuiltin('apply', (args) => {
        const proc = args[0];
        const lastArg = args[args.length - 1];
        const prefixArgs = args.slice(1, args.length - 1);
        const tailArgs = pairsToArray(lastArg);
        const allArgs = [...prefixArgs, ...tailArgs];
        if (proc.tag === 'builtin')
            return proc.fn(allArgs);
        if (proc.tag === 'lambda') {
            const callEnv = new Env(proc.env);
            for (let i = 0; i < proc.params.length; i++) {
                callEnv.define(proc.params[i], allArgs[i]);
            }
            if (proc.rest) {
                callEnv.define(proc.rest, listToPairs(allArgs.slice(proc.params.length)));
            }
            let result = { tag: 'void' };
            for (const bodyExpr of proc.body) {
                result = evaluate(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError('apply: not a procedure');
    });
    defBuiltin('number?', (args) => ({ tag: 'boolean', value: args[0].tag === 'number' }));
    defBuiltin('string?', (args) => ({ tag: 'boolean', value: args[0].tag === 'string' }));
    defBuiltin('boolean?', (args) => ({ tag: 'boolean', value: args[0].tag === 'boolean' }));
    defBuiltin('pair?', (args) => ({ tag: 'boolean', value: args[0].tag === 'pair' }));
    defBuiltin('symbol?', (args) => ({ tag: 'boolean', value: args[0].tag === 'symbol' }));
    defBuiltin('char?', (args) => ({ tag: 'boolean', value: args[0].tag === 'char' }));
    // Display / Write / Newline
    defBuiltin('display', (args) => {
        outputBuffer += displayVal(args[0]);
        return { tag: 'void' };
    });
    defBuiltin('write', (args) => {
        outputBuffer += writeVal(args[0]);
        return { tag: 'void' };
    });
    defBuiltin('newline', (_args) => {
        outputBuffer += '\n';
        return { tag: 'void' };
    });
    // String operations
    defBuiltin('string-append', (args) => {
        let result = '';
        for (const a of args) {
            if (a.tag !== 'string')
                throw new EvalError('string-append: expected string');
            result += a.value;
        }
        return { tag: 'string', value: result };
    });
    defBuiltin('string-length', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string-length: expected string');
        return { tag: 'number', value: args[0].value.length };
    });
    defBuiltin('substring', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('substring: expected string');
        const s = args[0].value;
        const start = expectNumber(args[1], 'substring');
        const end = expectNumber(args[2], 'substring');
        return { tag: 'string', value: s.slice(start, end) };
    });
    defBuiltin('string->number', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string->number: expected string');
        const n = Number(args[0].value);
        if (isNaN(n))
            return { tag: 'boolean', value: false };
        return { tag: 'number', value: n };
    });
    defBuiltin('number->string', (args) => {
        if (args[0].tag !== 'number')
            throw new EvalError('number->string: expected number');
        return { tag: 'string', value: String(args[0].value) };
    });
    defBuiltin('symbol->string', (args) => {
        if (args[0].tag !== 'symbol')
            throw new EvalError('symbol->string: expected symbol');
        return { tag: 'string', value: args[0].value };
    });
    defBuiltin('string->symbol', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string->symbol: expected string');
        return { tag: 'symbol', value: args[0].value };
    });
    defBuiltin('string-ref', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string-ref: expected string');
        const idx = expectNumber(args[1], 'string-ref');
        return { tag: 'char', value: args[0].value[idx] };
    });
    defBuiltin('string-copy', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string-copy: expected string');
        return { tag: 'string', value: args[0].value };
    });
    return env;
}
class TailCall {
    expr;
    env;
    constructor(expr, env) {
        this.expr = expr;
        this.env = env;
    }
}
function parseDotParams(paramExprs) {
    const dotIdx = paramExprs.findIndex(p => p.tag === 'symbol' && p.value === '.');
    if (dotIdx >= 0) {
        const params = paramExprs.slice(0, dotIdx).map(p => p.value);
        const rest = paramExprs[dotIdx + 1].value;
        return { params, rest };
    }
    return { params: paramExprs.map(p => p.value) };
}
function evaluate(startExpr, startEnv) {
    let expr = startExpr;
    let env = startEnv;
    // Trampoline loop
    trampoline: for (;;) {
        if (expr.tag === 'symbol') {
            try {
                return env.get(expr.value);
            }
            catch (e) {
                if (e instanceof EvalError && expr.pos) {
                    throw new EvalError(`${posStr(expr.pos)}${e.message}`);
                }
                throw e;
            }
        }
        if (expr.tag !== 'list') {
            return expr; // self-evaluating
        }
        const items = expr.value;
        if (items.length === 0)
            throw new EvalError(`${posStr(expr.pos)}empty application`);
        const head = items[0];
        if (head.tag === 'symbol') {
            const op = head.value;
            // Special forms
            if (op === 'quote') {
                return astToPairs(items[1]);
            }
            if (op === 'if') {
                if (items.length < 3)
                    throw new EvalError(`${posStr(expr.pos)}if: bad syntax`);
                const cond = evaluate(items[1], env);
                if (isTruthy(cond)) {
                    expr = items[2];
                    continue; // TCO
                }
                else if (items.length > 3) {
                    expr = items[3];
                    continue; // TCO
                }
                return { tag: 'void' };
            }
            if (op === 'define') {
                if (items.length < 3)
                    throw new EvalError(`${posStr(expr.pos)}define: bad syntax`);
                if (items[1].tag === 'list') {
                    // (define (f params...) body...) or (define (f params... . rest) body...)
                    const nameAndParams = items[1].value;
                    const name = nameAndParams[0].value;
                    const { params, rest } = parseDotParams(nameAndParams.slice(1));
                    const body = items.slice(2);
                    env.define(name, { tag: 'lambda', params, rest, body, env });
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
                if (paramList.tag === 'symbol') {
                    // (lambda args body...) — all args as rest
                    const body = items.slice(2);
                    return { tag: 'lambda', params: [], rest: paramList.value, body, env };
                }
                const { params, rest } = parseDotParams(paramList.value);
                const body = items.slice(2);
                return { tag: 'lambda', params, rest, body, env };
            }
            if (op === 'and') {
                if (items.length === 1)
                    return { tag: 'boolean', value: true };
                for (let i = 1; i < items.length - 1; i++) {
                    const result = evaluate(items[i], env);
                    if (!isTruthy(result))
                        return result;
                }
                expr = items[items.length - 1];
                continue; // TCO last
            }
            if (op === 'or') {
                if (items.length === 1)
                    return { tag: 'boolean', value: false };
                for (let i = 1; i < items.length - 1; i++) {
                    const result = evaluate(items[i], env);
                    if (isTruthy(result))
                        return result;
                }
                expr = items[items.length - 1];
                continue; // TCO last
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
                    // TCO: evaluate body with tail call on last expr
                    for (let i = 0; i < body.length - 1; i++) {
                        evaluate(body[i], callEnv);
                    }
                    expr = body[body.length - 1];
                    env = callEnv;
                    continue;
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
                // TCO: tail call on last body expr
                for (let i = 0; i < body.length - 1; i++) {
                    evaluate(body[i], letEnv);
                }
                expr = body[body.length - 1];
                env = letEnv;
                continue;
            }
            if (op === 'begin') {
                // TCO: tail call on last expr
                for (let i = 1; i < items.length - 1; i++) {
                    evaluate(items[i], env);
                }
                if (items.length > 1) {
                    expr = items[items.length - 1];
                    continue;
                }
                return { tag: 'void' };
            }
            if (op === 'set!') {
                const varExpr = items[1];
                if (varExpr.tag !== 'symbol')
                    throw new EvalError(`${posStr(expr.pos)}set!: bad syntax`);
                const val = evaluate(items[2], env);
                env.set(varExpr.value, val);
                return { tag: 'void' };
            }
            if (op === 'string-set!') {
                const varExpr = items[1];
                if (varExpr.tag !== 'symbol')
                    throw new EvalError(`${posStr(expr.pos)}string-set!: first argument must be a variable`);
                const str = env.get(varExpr.value);
                if (str.tag !== 'string')
                    throw new EvalError(`${posStr(expr.pos)}string-set!: expected string`);
                const idx = expectNumber(evaluate(items[2], env), 'string-set!');
                const ch = evaluate(items[3], env);
                if (ch.tag !== 'char')
                    throw new EvalError(`${posStr(expr.pos)}string-set!: expected char`);
                const newStr = str.value.substring(0, idx) + ch.value + str.value.substring(idx + 1);
                env.set(varExpr.value, { tag: 'string', value: newStr });
                return { tag: 'void' };
            }
            if (op === 'cond') {
                for (let i = 1; i < items.length; i++) {
                    const clause = items[i].value;
                    if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
                        for (let j = 1; j < clause.length - 1; j++) {
                            evaluate(clause[j], env);
                        }
                        if (clause.length > 1) {
                            expr = clause[clause.length - 1];
                            continue trampoline;
                        }
                        return { tag: 'void' };
                    }
                    const test = evaluate(clause[0], env);
                    if (isTruthy(test)) {
                        if (clause.length === 1)
                            return test;
                        for (let j = 1; j < clause.length - 1; j++) {
                            evaluate(clause[j], env);
                        }
                        expr = clause[clause.length - 1];
                        continue trampoline;
                    }
                }
                return { tag: 'void' };
            }
        }
        // Function application
        const proc = evaluate(head, env);
        const args = items.slice(1).map(a => evaluate(a, env));
        if (proc.tag === 'builtin') {
            try {
                return proc.fn(args);
            }
            catch (e) {
                if (e instanceof EvalError && expr.pos && !e.message.match(/^\d+:/)) {
                    throw new EvalError(`${posStr(expr.pos)}${e.message}`);
                }
                throw e;
            }
        }
        if (proc.tag === 'lambda') {
            const callEnv = new Env(proc.env);
            for (let i = 0; i < proc.params.length; i++) {
                callEnv.define(proc.params[i], args[i]);
            }
            if (proc.rest) {
                callEnv.define(proc.rest, listToPairs(args.slice(proc.params.length)));
            }
            // TCO: tail call on last body expr
            for (let i = 0; i < proc.body.length - 1; i++) {
                evaluate(proc.body[i], callEnv);
            }
            expr = proc.body[proc.body.length - 1];
            env = callEnv;
            continue;
        }
        throw new EvalError(`${posStr(expr.pos)}not a procedure`);
    } // end trampoline loop
}
// ── Output buffer ──────────────────────────────────────────────────
let outputBuffer = '';
// ── Display / Write formatting ─────────────────────────────────────
/** display format: strings without quotes */
function displayVal(val) {
    switch (val.tag) {
        case 'string': return val.value;
        default: return writeVal(val);
    }
}
/** write format: strings with quotes (also used as default formatter) */
function writeVal(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'char': return `#\\${val.value}`;
        case 'list': return `(${val.value.map(writeVal).join(' ')})`;
        case 'nil': return '()';
        case 'pair': {
            let s = '(' + writeVal(val.car);
            let cur = val.cdr;
            while (cur.tag === 'pair') {
                s += ' ' + writeVal(cur.car);
                cur = cur.cdr;
            }
            if (cur.tag !== 'nil') {
                s += ' . ' + writeVal(cur);
            }
            s += ')';
            return s;
        }
        case 'lambda': return '#<procedure>';
        case 'builtin': return `#<builtin:${val.name}>`;
        case 'void': return '';
    }
}
// Keep old name as alias for writeVal (used in evalStr)
const display = writeVal;
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
    const exprs = parse(input);
    if (exprs.length === 0)
        throw new EvalError('empty input');
    outputBuffer = '';
    const env = makeGlobalEnv();
    let result = { tag: 'void' };
    for (const expr of exprs) {
        result = evaluate(expr, env);
    }
    return { result: display(result), output: outputBuffer };
}
