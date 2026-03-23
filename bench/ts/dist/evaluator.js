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
// ── Macro Support ─────────────────────────────────────────────────
let gensymCounter = 0;
function gensym(base) { return `##${base}_${gensymCounter++}`; }
const SPECIAL_FORMS = new Set([
    'quote', 'if', 'define', 'lambda', 'set!', 'begin', 'let', 'cond', 'and', 'or', 'define-syntax',
]);
function matchPattern(pattern, args, literals) {
    const bindings = new Map();
    let pi = 0, ai = 0;
    while (pi < pattern.length) {
        const nextPat = pi + 1 < pattern.length ? pattern[pi + 1] : undefined;
        if (nextPat && nextPat.tag === 'symbol' && nextPat.value === '...') {
            const varPat = pattern[pi];
            if (varPat.tag !== 'symbol')
                return null;
            const remainingFixed = pattern.length - pi - 2;
            const available = args.length - ai - remainingFixed;
            if (available < 0)
                return null;
            bindings.set(varPat.value, args.slice(ai, ai + available));
            ai += available;
            pi += 2;
        }
        else {
            if (ai >= args.length)
                return null;
            const pat = pattern[pi];
            if (pat.tag === 'symbol') {
                if (literals.includes(pat.value)) {
                    const arg = args[ai];
                    if (arg.tag !== 'symbol' || arg.value !== pat.value)
                        return null;
                }
                else {
                    bindings.set(pat.value, args[ai]);
                }
            }
            else {
                return null;
            }
            pi++;
            ai++;
        }
    }
    if (ai !== args.length)
        return null;
    return bindings;
}
function findEllipsisVar(template, bindings) {
    if (template.tag === 'symbol') {
        const val = bindings.get(template.value);
        if (val !== undefined && Array.isArray(val))
            return template.value;
        return null;
    }
    if (template.tag === 'list') {
        for (const elem of template.elements) {
            const found = findEllipsisVar(elem, bindings);
            if (found)
                return found;
        }
    }
    return null;
}
function instantiateTemplate(template, bindings, renames) {
    if (template.tag === 'symbol') {
        const bound = bindings.get(template.value);
        if (bound !== undefined && !Array.isArray(bound))
            return bound;
        const renamed = renames.get(template.value);
        if (renamed !== undefined)
            return { tag: 'symbol', value: renamed };
        return template;
    }
    if (template.tag === 'list') {
        const result = [];
        for (let i = 0; i < template.elements.length; i++) {
            const nextTpl = i + 1 < template.elements.length ? template.elements[i + 1] : undefined;
            if (nextTpl && nextTpl.tag === 'symbol' && nextTpl.value === '...') {
                const elem = template.elements[i];
                const listVar = findEllipsisVar(elem, bindings);
                if (listVar) {
                    const listVals = bindings.get(listVar);
                    for (const val of listVals) {
                        const singleBindings = new Map(bindings);
                        singleBindings.set(listVar, val);
                        result.push(instantiateTemplate(elem, singleBindings, renames));
                    }
                }
                i++; // skip ellipsis
            }
            else {
                result.push(instantiateTemplate(template.elements[i], bindings, renames));
            }
        }
        return { tag: 'list', elements: result };
    }
    return template;
}
function collectTemplateSymbols(template, patVars, renames) {
    if (template.tag === 'symbol' && !patVars.has(template.value) &&
        !SPECIAL_FORMS.has(template.value) && !renames.has(template.value) &&
        template.value !== '...') {
        renames.set(template.value, gensym(template.value));
    }
    if (template.tag === 'list') {
        for (const elem of template.elements)
            collectTemplateSymbols(elem, patVars, renames);
    }
}
function expandMacro(macro, args, callEnv, pos) {
    for (const rule of macro.rules) {
        const bindings = matchPattern(rule.pattern, args, macro.literals);
        if (bindings !== null) {
            const patVars = new Set(bindings.keys());
            const renames = new Map();
            collectTemplateSymbols(rule.template, patVars, renames);
            const expanded = instantiateTemplate(rule.template, bindings, renames);
            const evalEnv = new Env(callEnv);
            for (const [origName, gensymName] of renames) {
                try {
                    evalEnv.set(gensymName, macro.defEnv.get(origName));
                }
                catch (_) { /* not bound */ }
            }
            return { expanded, evalEnv };
        }
    }
    throw posError('no matching syntax-rules pattern', pos);
}
// ── CPS Evaluator ──────────────────────────────────────────────────
function isTruthy(val) {
    return !(val.tag === 'boolean' && val.value === false);
}
function expectNumber(val, op, p) {
    if (val.tag !== 'number')
        throw posError(`${op}: expected number`, p);
    return val.value;
}
function done(v) { return { done: true, value: v }; }
function bounce(thunk) { return { done: false, thunk }; }
function trampoline(b) {
    while (!b.done)
        b = b.thunk();
    return b.value;
}
let fuel = 0;
function evalK(expr, env, k) {
    if (++fuel > 500) {
        fuel = 0;
        return bounce(() => evalK(expr, env, k));
    }
    switch (expr.tag) {
        case 'number':
        case 'boolean':
        case 'string':
        case 'char':
        case 'void':
            return k(expr);
        case 'symbol':
            return k(env.get(expr.value, expr.pos));
        case 'list':
            break;
        default:
            return k(expr);
    }
    const elems = expr.elements;
    if (elems.length === 0)
        throw posError('empty application', expr.pos);
    const head = elems[0];
    if (head.tag === 'symbol') {
        switch (head.value) {
            case 'quote':
                if (elems.length !== 2)
                    throw posError('quote: need 1 argument', expr.pos);
                return k(elems[1]);
            case 'if':
                if (elems.length < 3 || elems.length > 4)
                    throw posError('if: bad syntax', expr.pos);
                return evalK(elems[1], env, cond => {
                    if (isTruthy(cond))
                        return evalK(elems[2], env, k);
                    if (elems.length === 4)
                        return evalK(elems[3], env, k);
                    return k({ tag: 'void' });
                });
            case 'define': {
                if (elems.length < 3)
                    throw posError('define: bad syntax', expr.pos);
                const target = elems[1];
                if (target.tag === 'symbol') {
                    return evalK(elems[2], env, val => {
                        env.set(target.value, val);
                        return k({ tag: 'void' });
                    });
                }
                if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
                    const fnName = target.elements[0].value;
                    const { params, restParam } = parseParams(target.elements.slice(1), expr.pos);
                    env.set(fnName, { tag: 'lambda', params, restParam, body: elems.slice(2), env });
                    return k({ tag: 'void' });
                }
                throw posError('define: bad syntax', expr.pos);
            }
            case 'lambda': {
                if (elems.length < 3)
                    throw posError('lambda: bad syntax', expr.pos);
                const paramList = elems[1];
                if (paramList.tag === 'symbol') {
                    return k({ tag: 'lambda', params: [], restParam: paramList.value, body: elems.slice(2), env });
                }
                if (paramList.tag !== 'list')
                    throw posError('lambda: params must be a list', expr.pos);
                const { params, restParam } = parseParams(paramList.elements, expr.pos);
                return k({ tag: 'lambda', params, restParam, body: elems.slice(2), env });
            }
            case 'set!': {
                if (elems.length !== 3)
                    throw posError('set!: bad syntax', expr.pos);
                const target = elems[1];
                if (target.tag !== 'symbol')
                    throw posError('set!: target must be a symbol', expr.pos);
                return evalK(elems[2], env, val => {
                    env.mutate(target.value, val, expr.pos);
                    return k({ tag: 'void' });
                });
            }
            case 'begin':
                if (elems.length === 1)
                    return k({ tag: 'void' });
                return evalSeq(elems, 1, env, k);
            case 'let': {
                if (elems.length < 3)
                    throw posError('let: bad syntax', expr.pos);
                // Named let
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
                    const bodyExprs = elems.slice(3);
                    const loopLambda = { tag: 'lambda', params: paramNames, body: bodyExprs, env };
                    const letEnv = new Env(env);
                    letEnv.set(loopName, loopLambda);
                    loopLambda.env = letEnv;
                    return evalList(initExprs, env, args => {
                        const callEnv = new Env(letEnv);
                        for (let i = 0; i < paramNames.length; i++)
                            callEnv.set(paramNames[i], args[i]);
                        return evalSeqArr(bodyExprs, callEnv, k);
                    });
                }
                // Regular let
                const bindings = elems[1];
                if (bindings.tag !== 'list')
                    throw posError('let: bindings must be a list', expr.pos);
                const letEnv = new Env(env);
                const evalBindings = (i) => {
                    if (i >= bindings.elements.length) {
                        return evalSeq(elems, 2, letEnv, k);
                    }
                    const b = bindings.elements[i];
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                        throw posError('let: bad binding', expr.pos);
                    const bindName = b.elements[0].value;
                    return evalK(b.elements[1], env, val => {
                        letEnv.set(bindName, val);
                        return evalBindings(i + 1);
                    });
                };
                return evalBindings(0);
            }
            case 'cond': {
                const evalClauses = (i) => {
                    if (i >= elems.length)
                        return k({ tag: 'void' });
                    const clause = elems[i];
                    if (clause.tag !== 'list' || clause.elements.length < 2)
                        throw posError('cond: bad clause', expr.pos);
                    if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
                        return evalSeqArr(clause.elements.slice(1), env, k);
                    }
                    return evalK(clause.elements[0], env, test => {
                        if (isTruthy(test))
                            return evalSeqArr(clause.elements.slice(1), env, k);
                        return evalClauses(i + 1);
                    });
                };
                return evalClauses(1);
            }
            case 'and': {
                if (elems.length === 1)
                    return k({ tag: 'boolean', value: true });
                const evalAnds = (i) => {
                    if (i === elems.length - 1)
                        return evalK(elems[i], env, k);
                    return evalK(elems[i], env, val => {
                        if (!isTruthy(val))
                            return k(val);
                        return evalAnds(i + 1);
                    });
                };
                return evalAnds(1);
            }
            case 'or': {
                if (elems.length === 1)
                    return k({ tag: 'boolean', value: false });
                const evalOrs = (i) => {
                    if (i === elems.length - 1)
                        return evalK(elems[i], env, k);
                    return evalK(elems[i], env, val => {
                        if (isTruthy(val))
                            return k(val);
                        return evalOrs(i + 1);
                    });
                };
                return evalOrs(1);
            }
            case 'define-syntax': {
                if (elems.length !== 3)
                    throw posError('define-syntax: bad syntax', expr.pos);
                const name = elems[1];
                if (name.tag !== 'symbol')
                    throw posError('define-syntax: name must be symbol', expr.pos);
                const transformer = elems[2];
                if (transformer.tag !== 'list' || transformer.elements.length < 2 ||
                    transformer.elements[0].tag !== 'symbol' || transformer.elements[0].value !== 'syntax-rules')
                    throw posError('define-syntax: expected syntax-rules', expr.pos);
                const litList = transformer.elements[1];
                if (litList.tag !== 'list')
                    throw posError('syntax-rules: literals must be list', expr.pos);
                const literals = litList.elements.map(e => {
                    if (e.tag !== 'symbol')
                        throw posError('syntax-rules: literal must be symbol', expr.pos);
                    return e.value;
                });
                const rules = [];
                for (let i = 2; i < transformer.elements.length; i++) {
                    const clause = transformer.elements[i];
                    if (clause.tag !== 'list' || clause.elements.length !== 2)
                        throw posError('syntax-rules: bad clause', expr.pos);
                    const pat = clause.elements[0];
                    if (pat.tag !== 'list')
                        throw posError('syntax-rules: pattern must be list', expr.pos);
                    rules.push({ pattern: pat.elements.slice(1), template: clause.elements[1] });
                }
                env.set(name.value, { tag: 'macro', literals, rules, defEnv: env });
                return k({ tag: 'void' });
            }
        }
        // Check for macro application
        let macroVal;
        try {
            macroVal = env.get(head.value);
        }
        catch (_) { /* unbound */ }
        if (macroVal && macroVal.tag === 'macro') {
            const { expanded, evalEnv } = expandMacro(macroVal, elems.slice(1), env, expr.pos);
            return evalK(expanded, evalEnv, k);
        }
    }
    // Function application: evaluate head, then args, then apply
    return evalK(head, env, proc => evalList(elems.slice(1), env, args => applyK(proc, args, k, expr.pos)));
}
// Evaluate a sequence of expressions from elems[start..], return last value
function evalSeq(elems, start, env, k) {
    if (start >= elems.length)
        return k({ tag: 'void' });
    const loop = (i) => {
        if (i === elems.length - 1)
            return evalK(elems[i], env, k);
        return evalK(elems[i], env, _ => loop(i + 1));
    };
    return loop(start);
}
// Same but for a standalone array
function evalSeqArr(exprs, env, k) {
    if (exprs.length === 0)
        return k({ tag: 'void' });
    const loop = (i) => {
        if (i === exprs.length - 1)
            return evalK(exprs[i], env, k);
        return evalK(exprs[i], env, _ => loop(i + 1));
    };
    return loop(0);
}
// Evaluate a list of expressions left-to-right, collect results
function evalList(exprs, env, k) {
    const n = exprs.length;
    const results = new Array(n);
    const loop = (i) => {
        if (i < 0)
            return k(results);
        return evalK(exprs[i], env, val => {
            results[i] = val;
            return loop(i - 1);
        });
    };
    return loop(n - 1);
}
// Apply a procedure to arguments in CPS
function applyK(proc, args, k, pos) {
    if (proc.tag === 'callcc') {
        if (args.length !== 1)
            throw posError('call/cc: need 1 argument', pos);
        const kontVal = { tag: 'continuation', fn: k };
        return applyK(args[0], [kontVal], k, pos);
    }
    if (proc.tag === 'continuation') {
        return proc.fn(args.length > 0 ? args[0] : { tag: 'void' });
    }
    if (proc.tag === 'builtin' && proc.name === 'apply') {
        if (args.length < 2)
            throw posError('apply: need at least 2 arguments', pos);
        const fn = args[0];
        const lastArg = args[args.length - 1];
        if (lastArg.tag !== 'list')
            throw posError('apply: last argument must be a list', pos);
        const allArgs = [...args.slice(1, -1), ...lastArg.elements];
        return applyK(fn, allArgs, k, pos);
    }
    if (proc.tag === 'lambda') {
        if (proc.restParam) {
            if (args.length < proc.params.length)
                throw posError('wrong number of arguments', pos);
        }
        else {
            if (args.length !== proc.params.length)
                throw posError('wrong number of arguments', pos);
        }
        const callEnv = new Env(proc.env);
        for (let i = 0; i < proc.params.length; i++)
            callEnv.set(proc.params[i], args[i]);
        if (proc.restParam)
            callEnv.set(proc.restParam, { tag: 'list', elements: args.slice(proc.params.length) });
        return bounce(() => evalSeqArr(proc.body, callEnv, k));
    }
    if (proc.tag === 'builtin') {
        return k(proc.fn(args, pos));
    }
    throw posError('not a procedure', pos);
}
// ── Builtins ──────────────────────────────────────────────────────
function makeGlobalEnv(output = []) {
    const env = new Env();
    function defBuiltin(name, fn) {
        env.set(name, { tag: 'builtin', name, fn });
    }
    // call/cc
    env.set('call/cc', { tag: 'callcc' });
    env.set('call-with-current-continuation', { tag: 'callcc' });
    // apply (handled specially in applyK, but needs a value in the env)
    env.set('apply', { tag: 'builtin', name: 'apply', fn: () => { throw new EvalError('internal: apply handled by applyK'); } });
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
    defBuiltin('char?', (args, p) => { if (args.length !== 1)
        throw posError('char?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'char' }; });
    defBuiltin('procedure?', (args, p) => {
        if (args.length !== 1)
            throw posError('procedure?: need 1 argument', p);
        const t = args[0].tag;
        return { tag: 'boolean', value: t === 'lambda' || t === 'builtin' || t === 'continuation' || t === 'callcc' };
    });
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
        case 'continuation': return '#<procedure>';
        case 'callcc': return '#<procedure>';
        case 'macro': return '#<macro>';
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
    const result = trampoline(evalSeqArr(exprs, env, v => done(v)));
    return displayVal(result);
}
export function evalStrWithOutput(input) {
    const exprs = parseAll(input);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const output = [];
    const env = makeGlobalEnv(output);
    const result = trampoline(evalSeqArr(exprs, env, v => done(v)));
    return { result: displayVal(result), output: output.join('') };
}
