import { EvalError } from './evalError.js';
function posError(msg, pos) {
    if (pos)
        return new EvalError(`${pos.line}:${pos.col}: ${msg}`);
    return new EvalError(msg);
}
// ── Pair / List Helpers ─────────────────────────────────────────────
const EMPTY_LIST = { tag: 'list', elements: [] };
function isNull(v) {
    return v.tag === 'list' && v.elements.length === 0;
}
function makePair(car, cdr) {
    return { tag: 'pair', car, cdr };
}
function arrayToList(arr) {
    let result = EMPTY_LIST;
    for (let i = arr.length - 1; i >= 0; i--) {
        result = makePair(arr[i], result);
    }
    return result;
}
function listToArray(val) {
    const result = [];
    let cur = val;
    while (cur.tag === 'pair') {
        result.push(cur.car);
        cur = cur.cdr;
    }
    return result;
}
function listLength(val) {
    let n = 0;
    let cur = val;
    while (cur.tag === 'pair') {
        n++;
        cur = cur.cdr;
    }
    return n;
}
// Convert AST list nodes to runtime pair chains
function quoteConvert(val) {
    if (val.tag !== 'list')
        return val;
    if (val.elements.length === 0)
        return EMPTY_LIST;
    const elems = val.elements;
    // Dotted pair: (a b . c) stored as [a, b, '.', c]
    const dotIdx = elems.findIndex((e, i) => i > 0 && e.tag === 'symbol' && e.value === '.');
    if (dotIdx !== -1 && dotIdx === elems.length - 2) {
        let result = quoteConvert(elems[elems.length - 1]);
        for (let i = dotIdx - 1; i >= 0; i--) {
            result = makePair(quoteConvert(elems[i]), result);
        }
        return result;
    }
    return arrayToList(elems.map(quoteConvert));
}
// ── Environment ───────────────────────────────────────────────────
class Env {
    parent;
    bindings = new Map();
    syntaxBindings = null;
    syntaxDefEnv = null;
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
    getSyntaxBindings() {
        if (this.syntaxBindings)
            return this.syntaxBindings;
        if (this.parent)
            return this.parent.getSyntaxBindings();
        return null;
    }
    getSyntaxDefEnv() {
        if (this.syntaxDefEnv)
            return this.syntaxDefEnv;
        if (this.parent)
            return this.parent.getSyntaxDefEnv();
        return null;
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
        if (ch === '#' && i + 1 < input.length && input[i + 1] === "'") {
            tokens.push({ text: "#'", pos: startPos });
            i += 2;
            col += 2;
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
    if (tok.text === "#'") {
        cur.i++;
        const tmpl = parse(tokens, cur);
        return { tag: 'list', elements: [{ tag: 'symbol', value: 'syntax', pos: tok.pos }, tmpl], pos: tok.pos };
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
    // Rational literal: e.g. 1/3, -5/2
    const ratMatch = token.match(/^(-?\d+)\/(\d+)$/);
    if (ratMatch) {
        const rn = parseInt(ratMatch[1], 10);
        const rd = parseInt(ratMatch[2], 10);
        if (rd !== 0)
            return makeRational(rn, rd, pos);
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
let recordTypeCounter = 0;
function gensym(base) { return `##${base}_${gensymCounter++}`; }
const SPECIAL_FORMS = new Set([
    'quote', 'if', 'define', 'lambda', 'set!', 'begin', 'let', 'let*', 'letrec', 'letrec*',
    'case', 'do', 'cond', 'and', 'or', 'define-syntax', 'guard',
    'syntax-case', 'syntax', 'with-syntax',
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
        // Skip quoted forms - symbols inside (quote ...) should not be renamed
        if (template.elements.length >= 1 && template.elements[0].tag === 'symbol' &&
            template.elements[0].value === 'quote')
            return;
        for (const elem of template.elements)
            collectTemplateSymbols(elem, patVars, renames);
    }
}
// Side channel for syntax-case expansion hygiene
let lastSyntaxExpansion = null;
// Match a syntax-case pattern (includes the macro name position, _ is wildcard)
function matchSyntaxCasePattern(pattern, elems, literals) {
    const bindings = new Map();
    let pi = 0, ei = 0;
    while (pi < pattern.length) {
        const nextPat = pi + 1 < pattern.length ? pattern[pi + 1] : undefined;
        if (nextPat && nextPat.tag === 'symbol' && nextPat.value === '...') {
            const varPat = pattern[pi];
            if (varPat.tag !== 'symbol')
                return null;
            const remainingFixed = pattern.length - pi - 2;
            const available = elems.length - ei - remainingFixed;
            if (available < 0)
                return null;
            bindings.set(varPat.value, elems.slice(ei, ei + available));
            ei += available;
            pi += 2;
        }
        else {
            if (ei >= elems.length)
                return null;
            const pat = pattern[pi];
            if (pat.tag === 'symbol') {
                if (pat.value === '_') {
                    // wildcard - matches anything, doesn't bind
                }
                else if (literals.includes(pat.value)) {
                    const arg = elems[ei];
                    if (arg.tag !== 'symbol' || arg.value !== pat.value)
                        return null;
                }
                else {
                    bindings.set(pat.value, elems[ei]);
                }
            }
            else if (pat.tag === 'list') {
                // Nested list pattern
                const elem = elems[ei];
                let subElems;
                if (elem.tag === 'list') {
                    subElems = elem.elements;
                }
                else if (elem.tag === 'pair') {
                    subElems = [];
                    let cur = elem;
                    while (cur.tag === 'pair') {
                        subElems.push(cur.car);
                        cur = cur.cdr;
                    }
                }
                else {
                    return null;
                }
                const subBindings = matchSyntaxCasePattern(pat.elements, subElems, literals);
                if (!subBindings)
                    return null;
                for (const [k, v] of subBindings)
                    bindings.set(k, v);
            }
            else {
                return null;
            }
            pi++;
            ei++;
        }
    }
    if (ei !== elems.length)
        return null;
    return bindings;
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
    if (val.tag === 'number')
        return val.value;
    if (val.tag === 'rational')
        return val.num / val.den;
    throw posError(`${op}: expected number`, p);
}
function gcd(a, b) {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b) {
        [a, b] = [b, a % b];
    }
    return a;
}
function makeRational(num, den, pos) {
    if (den === 0)
        throw posError('division by zero', pos);
    if (den < 0) {
        num = -num;
        den = -den;
    }
    const g = gcd(Math.abs(num), den);
    num = num / g;
    den = den / g;
    if (den === 1)
        return { tag: 'number', value: num, pos };
    return { tag: 'rational', num, den, pos };
}
function isExactNum(v) {
    return v.tag === 'rational' || (v.tag === 'number' && Number.isInteger(v.value) && v.exact !== false);
}
function toRatParts(v) {
    if (v.tag === 'rational')
        return [v.num, v.den];
    if (v.tag === 'number')
        return [v.value, 1];
    throw new EvalError('not a number');
}
function numericFloat(v, op, p) {
    if (v.tag === 'number')
        return v.value;
    if (v.tag === 'rational')
        return v.num / v.den;
    throw posError(`${op}: expected number`, p);
}
function isNumericVal(v) {
    return v.tag === 'number' || v.tag === 'rational';
}
function done(v) { return { done: true, value: v }; }
function bounce(thunk) { return { done: false, thunk }; }
function trampoline(b) {
    while (!b.done)
        b = b.thunk();
    return b.value;
}
let windStack = [];
let exHandlers = [];
function raiseValue(val) {
    if (exHandlers.length === 0) {
        throw new EvalError(`unhandled exception: ${displayVal(val)}`);
    }
    const handler = exHandlers.pop();
    return doWind(handler.savedWind, () => handler.fn(val));
}
function doWind(target, after) {
    let common = 0;
    while (common < windStack.length && common < target.length && windStack[common] === target[common]) {
        common++;
    }
    const unwindCount = windStack.length - common;
    const rewindFrames = target.slice(common);
    const doUnwindStep = (remaining) => {
        if (remaining <= 0)
            return doRewindStep(0);
        const frame = windStack[windStack.length - 1];
        windStack.pop();
        return applyK(frame.outThunk, [], _ => doUnwindStep(remaining - 1));
    };
    const doRewindStep = (i) => {
        if (i >= rewindFrames.length)
            return after();
        return applyK(rewindFrames[i].inThunk, [], _ => {
            windStack.push(rewindFrames[i]);
            return doRewindStep(i + 1);
        });
    };
    return doUnwindStep(unwindCount);
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
                return k(quoteConvert(elems[1]));
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
            case 'let*': {
                if (elems.length < 3)
                    throw posError('let*: bad syntax', expr.pos);
                const bindings = elems[1];
                if (bindings.tag !== 'list')
                    throw posError('let*: bindings must be a list', expr.pos);
                const letStarEnv = new Env(env);
                const evalLetStarBindings = (i) => {
                    if (i >= bindings.elements.length)
                        return evalSeq(elems, 2, letStarEnv, k);
                    const b = bindings.elements[i];
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                        throw posError('let*: bad binding', expr.pos);
                    const bName = b.elements[0].value;
                    return evalK(b.elements[1], letStarEnv, val => {
                        letStarEnv.set(bName, val);
                        return evalLetStarBindings(i + 1);
                    });
                };
                return evalLetStarBindings(0);
            }
            case 'letrec': {
                if (elems.length < 3)
                    throw posError('letrec: bad syntax', expr.pos);
                const bindings = elems[1];
                if (bindings.tag !== 'list')
                    throw posError('letrec: bindings must be a list', expr.pos);
                const letrecEnv = new Env(env);
                const names = [];
                const initExprs = [];
                for (const b of bindings.elements) {
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                        throw posError('letrec: bad binding', expr.pos);
                    names.push(b.elements[0].value);
                    initExprs.push(b.elements[1]);
                    letrecEnv.set(b.elements[0].value, { tag: 'void' });
                }
                return evalList(initExprs, letrecEnv, vals => {
                    for (let i = 0; i < names.length; i++)
                        letrecEnv.set(names[i], vals[i]);
                    return evalSeq(elems, 2, letrecEnv, k);
                });
            }
            case 'letrec*': {
                if (elems.length < 3)
                    throw posError('letrec*: bad syntax', expr.pos);
                const bindings = elems[1];
                if (bindings.tag !== 'list')
                    throw posError('letrec*: bindings must be a list', expr.pos);
                const letrecStarEnv = new Env(env);
                for (const b of bindings.elements) {
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                        throw posError('letrec*: bad binding', expr.pos);
                    letrecStarEnv.set(b.elements[0].value, { tag: 'void' });
                }
                const evalLetrecStarBindings = (i) => {
                    if (i >= bindings.elements.length)
                        return evalSeq(elems, 2, letrecStarEnv, k);
                    const b = bindings.elements[i];
                    const name = b.elements[0];
                    const initExpr = b.elements[1];
                    return evalK(initExpr, letrecStarEnv, val => {
                        letrecStarEnv.set(name.value, val);
                        return evalLetrecStarBindings(i + 1);
                    });
                };
                return evalLetrecStarBindings(0);
            }
            case 'case': {
                if (elems.length < 2)
                    throw posError('case: bad syntax', expr.pos);
                return evalK(elems[1], env, keyVal => {
                    const evalCaseClauses = (i) => {
                        if (i >= elems.length)
                            return k({ tag: 'void' });
                        const clause = elems[i];
                        if (clause.tag !== 'list' || clause.elements.length < 2)
                            throw posError('case: bad clause', expr.pos);
                        if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
                            return evalSeqArr(clause.elements.slice(1), env, k);
                        }
                        if (clause.elements[0].tag !== 'list')
                            throw posError('case: datums must be a list', expr.pos);
                        const datums = clause.elements[0].elements;
                        let matched = false;
                        for (const datum of datums) {
                            if (schemeEqv(keyVal, datum)) {
                                matched = true;
                                break;
                            }
                        }
                        if (matched)
                            return evalSeqArr(clause.elements.slice(1), env, k);
                        return evalCaseClauses(i + 1);
                    };
                    return evalCaseClauses(2);
                });
            }
            case 'do': {
                if (elems.length < 3)
                    throw posError('do: bad syntax', expr.pos);
                const varSpecs = elems[1];
                if (varSpecs.tag !== 'list')
                    throw posError('do: var specs must be a list', expr.pos);
                const testClause = elems[2];
                if (testClause.tag !== 'list' || testClause.elements.length < 1)
                    throw posError('do: bad test clause', expr.pos);
                const bodyExprs = elems.slice(3);
                const varNames = [];
                const initExprs = [];
                const stepExprs = [];
                for (const spec of varSpecs.elements) {
                    if (spec.tag !== 'list' || spec.elements.length < 2 || spec.elements[0].tag !== 'symbol')
                        throw posError('do: bad var spec', expr.pos);
                    varNames.push(spec.elements[0].value);
                    initExprs.push(spec.elements[1]);
                    stepExprs.push(spec.elements.length >= 3 ? spec.elements[2] : null);
                }
                return evalList(initExprs, env, initVals => {
                    const doEnv = new Env(env);
                    for (let i = 0; i < varNames.length; i++)
                        doEnv.set(varNames[i], initVals[i]);
                    const doLoop = () => {
                        return evalK(testClause.elements[0], doEnv, testResult => {
                            if (isTruthy(testResult)) {
                                if (testClause.elements.length > 1) {
                                    return evalSeqArr(testClause.elements.slice(1), doEnv, k);
                                }
                                return k({ tag: 'void' });
                            }
                            const afterBody = () => {
                                const stepsToEval = [];
                                const stepIndices = [];
                                for (let i = 0; i < stepExprs.length; i++) {
                                    if (stepExprs[i] !== null) {
                                        stepsToEval.push(stepExprs[i]);
                                        stepIndices.push(i);
                                    }
                                }
                                if (stepsToEval.length === 0)
                                    return bounce(doLoop);
                                return evalList(stepsToEval, doEnv, stepVals => {
                                    for (let j = 0; j < stepIndices.length; j++) {
                                        doEnv.set(varNames[stepIndices[j]], stepVals[j]);
                                    }
                                    return bounce(doLoop);
                                });
                            };
                            if (bodyExprs.length > 0) {
                                return evalSeqArr(bodyExprs, doEnv, _ => afterBody());
                            }
                            return afterBody();
                        });
                    };
                    return doLoop();
                });
            }
            case 'cond': {
                const evalClauses = (i) => {
                    if (i >= elems.length)
                        return k({ tag: 'void' });
                    const clause = elems[i];
                    if (clause.tag !== 'list' || clause.elements.length < 1)
                        throw posError('cond: bad clause', expr.pos);
                    if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
                        return evalSeqArr(clause.elements.slice(1), env, k);
                    }
                    return evalK(clause.elements[0], env, test => {
                        if (isTruthy(test)) {
                            if (clause.elements.length === 1)
                                return k(test);
                            return evalSeqArr(clause.elements.slice(1), env, k);
                        }
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
            case 'guard': {
                if (elems.length < 3)
                    throw posError('guard: bad syntax', expr.pos);
                const guardSpec = elems[1];
                if (guardSpec.tag !== 'list' || guardSpec.elements.length < 1)
                    throw posError('guard: bad syntax', expr.pos);
                const gVarSym = guardSpec.elements[0];
                if (gVarSym.tag !== 'symbol')
                    throw posError('guard: variable must be symbol', expr.pos);
                const gVarName = gVarSym.value;
                const gClauses = guardSpec.elements.slice(1);
                const gBodyExprs = elems.slice(2);
                const guardK = k;
                const guardWind = [...windStack];
                exHandlers.push({
                    fn: (exnVal) => {
                        const clauseEnv = new Env(env);
                        clauseEnv.set(gVarName, exnVal);
                        const testClauses = (ci) => {
                            if (ci >= gClauses.length)
                                return raiseValue(exnVal);
                            const clause = gClauses[ci];
                            if (clause.tag !== 'list' || clause.elements.length < 2)
                                throw posError('guard: bad clause', expr.pos);
                            if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
                                return doWind(guardWind, () => evalSeqArr(clause.elements.slice(1), clauseEnv, guardK));
                            }
                            return evalK(clause.elements[0], clauseEnv, testResult => {
                                if (isTruthy(testResult)) {
                                    return doWind(guardWind, () => evalSeqArr(clause.elements.slice(1), clauseEnv, guardK));
                                }
                                return testClauses(ci + 1);
                            });
                        };
                        return testClauses(0);
                    },
                    savedWind: [...windStack],
                });
                return evalSeqArr(gBodyExprs, env, bodyVal => {
                    exHandlers.pop();
                    return guardK(bodyVal);
                });
            }
            case 'define-record-type': {
                if (elems.length < 4)
                    throw posError('define-record-type: bad syntax', expr.pos);
                const rtName = elems[1];
                if (rtName.tag !== 'symbol')
                    throw posError('define-record-type: name must be symbol', expr.pos);
                const ctorSpec = elems[2];
                if (ctorSpec.tag !== 'list' || ctorSpec.elements.length < 1 || ctorSpec.elements[0].tag !== 'symbol')
                    throw posError('define-record-type: bad constructor spec', expr.pos);
                const ctorName = ctorSpec.elements[0].value;
                const ctorFields = ctorSpec.elements.slice(1).map(e => {
                    if (e.tag !== 'symbol')
                        throw posError('define-record-type: field must be symbol', expr.pos);
                    return e.value;
                });
                const predSym = elems[3];
                if (predSym.tag !== 'symbol')
                    throw posError('define-record-type: predicate must be symbol', expr.pos);
                const predName = predSym.value;
                const typeId = recordTypeCounter++;
                const accessors = [];
                for (let fi = 4; fi < elems.length; fi++) {
                    const fspec = elems[fi];
                    if (fspec.tag !== 'list' || fspec.elements.length < 2 ||
                        fspec.elements[0].tag !== 'symbol' || fspec.elements[1].tag !== 'symbol')
                        throw posError('define-record-type: bad field spec', expr.pos);
                    accessors.push({ field: fspec.elements[0].value, accessor: fspec.elements[1].value });
                }
                env.set(ctorName, { tag: 'builtin', name: ctorName, fn: (args, p) => {
                        if (args.length !== ctorFields.length)
                            throw posError(`${ctorName}: wrong number of arguments`, p);
                        const fields = new Map();
                        for (let i = 0; i < ctorFields.length; i++)
                            fields.set(ctorFields[i], args[i]);
                        return { tag: 'record', typeId, typeName: rtName.value, fields };
                    } });
                env.set(predName, { tag: 'builtin', name: predName, fn: (args, p) => {
                        if (args.length !== 1)
                            throw posError(`${predName}: need 1 argument`, p);
                        return { tag: 'boolean', value: args[0].tag === 'record' && args[0].typeId === typeId };
                    } });
                for (const { field, accessor } of accessors) {
                    env.set(accessor, { tag: 'builtin', name: accessor, fn: (args, p) => {
                            if (args.length !== 1)
                                throw posError(`${accessor}: need 1 argument`, p);
                            if (args[0].tag !== 'record' || args[0].typeId !== typeId)
                                throw posError(`${accessor}: expected ${rtName.value}`, p);
                            return args[0].fields.get(field);
                        } });
                }
                return k({ tag: 'void' });
            }
            case 'define-syntax': {
                if (elems.length !== 3)
                    throw posError('define-syntax: bad syntax', expr.pos);
                const name = elems[1];
                if (name.tag !== 'symbol')
                    throw posError('define-syntax: name must be symbol', expr.pos);
                const transformer = elems[2];
                // Check if it's syntax-rules
                if (transformer.tag === 'list' && transformer.elements.length >= 2 &&
                    transformer.elements[0].tag === 'symbol' && transformer.elements[0].value === 'syntax-rules') {
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
                // Otherwise evaluate as a procedure transformer (syntax-case style)
                const defEnvCapture = env;
                return evalK(transformer, env, proc => {
                    env.set(name.value, { tag: 'syntaxTransformer', proc, defEnv: defEnvCapture });
                    return k({ tag: 'void' });
                });
            }
            case 'syntax-case': {
                // (syntax-case expr (literals) clause ...)
                // clause = (pattern body) or (pattern guard body)
                if (elems.length < 4)
                    throw posError('syntax-case: bad syntax', expr.pos);
                const stxExpr = elems[1];
                const litListSC = elems[2];
                if (litListSC.tag !== 'list')
                    throw posError('syntax-case: literals must be list', expr.pos);
                const scLiterals = litListSC.elements.map(e => {
                    if (e.tag !== 'symbol')
                        throw posError('syntax-case: literal must be symbol', expr.pos);
                    return e.value;
                });
                return evalK(stxExpr, env, stxVal => {
                    // Convert stxVal to elements for pattern matching
                    const stxElems = stxVal.tag === 'list' ? stxVal.elements :
                        stxVal.tag === 'pair' ? listToArray(stxVal).concat(isNull((function (v) { let c = v; while (c.tag === 'pair')
                            c = c.cdr; return c; })(stxVal)) ? [] : []) :
                            [stxVal];
                    // For pair-chain stxVal, convert to array
                    let matchElems;
                    if (stxVal.tag === 'pair') {
                        matchElems = [];
                        let cur = stxVal;
                        while (cur.tag === 'pair') {
                            matchElems.push(cur.car);
                            cur = cur.cdr;
                        }
                        if (!isNull(cur))
                            matchElems.push(cur); // improper list tail
                    }
                    else if (stxVal.tag === 'list') {
                        matchElems = stxVal.elements;
                    }
                    else {
                        matchElems = [stxVal];
                    }
                    for (let ci = 3; ci < elems.length; ci++) {
                        const clause = elems[ci];
                        if (clause.tag !== 'list' || clause.elements.length < 2)
                            throw posError('syntax-case: bad clause', expr.pos);
                        const pat = clause.elements[0];
                        let body;
                        if (clause.elements.length === 2) {
                            body = clause.elements[1];
                        }
                        else {
                            // (pattern guard body) - for now treat as (pattern body), guard ignored if 3 elements
                            body = clause.elements[clause.elements.length - 1];
                        }
                        // Match pattern against stxVal elements
                        let patElems;
                        if (pat.tag === 'list') {
                            patElems = pat.elements;
                        }
                        else {
                            patElems = [pat];
                        }
                        const bindings = matchSyntaxCasePattern(patElems, matchElems, scLiterals);
                        if (bindings !== null) {
                            const scEnv = new Env(env);
                            scEnv.syntaxBindings = bindings;
                            scEnv.syntaxDefEnv = env.getSyntaxDefEnv() || env;
                            return evalK(body, scEnv, k);
                        }
                    }
                    throw posError('syntax-case: no matching pattern', expr.pos);
                });
            }
            case 'syntax': {
                // (syntax template) - aka #'template
                if (elems.length !== 2)
                    throw posError('syntax: need 1 argument', expr.pos);
                const tmpl = elems[1];
                const synBindings = env.getSyntaxBindings();
                if (!synBindings)
                    throw posError('syntax: not in syntax-case context', expr.pos);
                const patVars = new Set(synBindings.keys());
                const renames = new Map();
                collectTemplateSymbols(tmpl, patVars, renames);
                const expanded = instantiateTemplate(tmpl, synBindings, renames);
                // Store renames in side channel for macro application to build eval env
                const defEnv = env.getSyntaxDefEnv() || env;
                lastSyntaxExpansion = { renames, defEnv };
                return k(expanded);
            }
            case 'with-syntax': {
                // (with-syntax ((pat expr) ...) body ...)
                if (elems.length < 3)
                    throw posError('with-syntax: bad syntax', expr.pos);
                const bindingList = elems[1];
                if (bindingList.tag !== 'list')
                    throw posError('with-syntax: bindings must be list', expr.pos);
                const wsBindings = bindingList.elements;
                // Evaluate all RHS expressions, then set up syntax bindings
                const evalWSBindings = (idx, accum) => {
                    if (idx >= wsBindings.length) {
                        // All bindings evaluated, set up env and eval body
                        const wsEnv = new Env(env);
                        // Merge existing syntax bindings
                        const existing = env.getSyntaxBindings();
                        if (existing) {
                            wsEnv.syntaxBindings = new Map(existing);
                            for (const [k2, v] of accum)
                                wsEnv.syntaxBindings.set(k2, v);
                        }
                        else {
                            wsEnv.syntaxBindings = accum;
                        }
                        wsEnv.syntaxDefEnv = env.getSyntaxDefEnv() || env;
                        return evalSeq(elems, 2, wsEnv, k);
                    }
                    const binding = wsBindings[idx];
                    if (binding.tag !== 'list' || binding.elements.length !== 2)
                        throw posError('with-syntax: bad binding', expr.pos);
                    const bPat = binding.elements[0];
                    const bExpr = binding.elements[1];
                    return evalK(bExpr, env, val => {
                        if (bPat.tag === 'symbol') {
                            accum.set(bPat.value, val);
                        }
                        else {
                            throw posError('with-syntax: pattern must be identifier', expr.pos);
                        }
                        return evalWSBindings(idx + 1, accum);
                    });
                };
                return evalWSBindings(0, new Map());
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
        if (macroVal && macroVal.tag === 'syntaxTransformer') {
            // Call the transformer procedure with the whole form as a list
            const form = expr; // the unevaluated form
            // Reset side channel
            lastSyntaxExpansion = null;
            return applyK(macroVal.proc, [form], expandedForm => {
                // Build eval env from renames captured by syntax form
                if (lastSyntaxExpansion) {
                    const { renames, defEnv: synDefEnv } = lastSyntaxExpansion;
                    lastSyntaxExpansion = null;
                    // Add gensym bindings directly to call-site env so defines propagate correctly
                    for (const [origName, gensymName] of renames) {
                        try {
                            env.set(gensymName, synDefEnv.get(origName));
                        }
                        catch (_) { /* not bound */ }
                    }
                    return evalK(expandedForm, env, k);
                }
                return evalK(expandedForm, env, k);
            }, expr.pos);
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
        const savedWind = [...windStack];
        const originalK = k;
        const kontFn = (val) => {
            return doWind(savedWind, () => originalK(val));
        };
        const kontVal = { tag: 'continuation', fn: kontFn };
        return applyK(args[0], [kontVal], k, pos);
    }
    if (proc.tag === 'continuation') {
        return proc.fn(args.length > 0 ? args[0] : { tag: 'void' });
    }
    if (proc.tag === 'builtin' && proc.name === 'dynamic-wind') {
        if (args.length !== 3)
            throw posError('dynamic-wind: need 3 arguments', pos);
        const [inThunk, bodyThunk, outThunk] = args;
        const frame = { inThunk, outThunk };
        return applyK(inThunk, [], _ => {
            windStack.push(frame);
            return applyK(bodyThunk, [], bodyVal => {
                windStack.pop();
                return applyK(outThunk, [], _ => k(bodyVal), pos);
            }, pos);
        }, pos);
    }
    if (proc.tag === 'builtin' && proc.name === 'raise') {
        if (args.length !== 1)
            throw posError('raise: need 1 argument', pos);
        return raiseValue(args[0]);
    }
    if (proc.tag === 'builtin' && proc.name === 'with-exception-handler') {
        if (args.length !== 2)
            throw posError('with-exception-handler: need 2 arguments', pos);
        const [handlerProc, thunk] = args;
        const savedWind = [...windStack];
        exHandlers.push({
            fn: (val) => applyK(handlerProc, [val], _ => {
                throw new EvalError('exception handler returned from raise');
            }, pos),
            savedWind,
        });
        return applyK(thunk, [], bodyVal => {
            exHandlers.pop();
            return k(bodyVal);
        }, pos);
    }
    if (proc.tag === 'builtin' && proc.name === 'call-with-values') {
        if (args.length !== 2)
            throw posError('call-with-values: need 2 arguments', pos);
        const [producer, consumer] = args;
        return applyK(producer, [], producerVal => {
            const consumerArgs = producerVal.tag === 'values' ? producerVal.elements : [producerVal];
            return applyK(consumer, consumerArgs, k, pos);
        }, pos);
    }
    if (proc.tag === 'builtin' && proc.name === 'map') {
        if (args.length < 2)
            throw posError('map: need at least 2 arguments', pos);
        const fn = args[0];
        const lists = args.slice(1);
        // Iterate through pair chains
        let cursors = lists.slice();
        const results = [];
        const mapLoop = () => {
            // Check if any list is exhausted
            if (cursors.some(c => isNull(c)))
                return k(arrayToList(results));
            for (const c of cursors)
                if (c.tag !== 'pair')
                    throw posError('map: expected list', pos);
            const fnArgs = cursors.map(c => c.car);
            cursors = cursors.map(c => c.cdr);
            return applyK(fn, fnArgs, val => { results.push(val); return mapLoop(); }, pos);
        };
        return mapLoop();
    }
    if (proc.tag === 'builtin' && proc.name === 'for-each') {
        if (args.length < 2)
            throw posError('for-each: need at least 2 arguments', pos);
        const fn = args[0];
        const lists = args.slice(1);
        let cursors = lists.slice();
        const forEachLoop = () => {
            if (cursors.some(c => isNull(c)))
                return k({ tag: 'void' });
            for (const c of cursors)
                if (c.tag !== 'pair')
                    throw posError('for-each: expected list', pos);
            const fnArgs = cursors.map(c => c.car);
            cursors = cursors.map(c => c.cdr);
            return applyK(fn, fnArgs, _ => forEachLoop(), pos);
        };
        return forEachLoop();
    }
    if (proc.tag === 'builtin' && proc.name === 'apply') {
        if (args.length < 2)
            throw posError('apply: need at least 2 arguments', pos);
        const fn = args[0];
        const lastArg = args[args.length - 1];
        const allArgs = [...args.slice(1, -1), ...listToArray(lastArg)];
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
            callEnv.set(proc.restParam, arrayToList(args.slice(proc.params.length)));
        return bounce(() => evalSeqArr(proc.body, callEnv, k));
    }
    if (proc.tag === 'builtin') {
        return k(proc.fn(args, pos));
    }
    throw posError('not a procedure', pos);
}
// ── Helpers ────────────────────────────────────────────────────────
function schemeEqv(a, b) {
    if (a === b)
        return true;
    if (a.tag !== b.tag) {
        if ((a.tag === 'number' || a.tag === 'rational') && (b.tag === 'number' || b.tag === 'rational')) {
            return numericFloat(a, 'eqv?') === numericFloat(b, 'eqv?');
        }
        return false;
    }
    switch (a.tag) {
        case 'number': return a.value === b.value;
        case 'rational': return a.num === b.num && a.den === b.den;
        case 'boolean': return a.value === b.value;
        case 'symbol': return a.value === b.value;
        case 'char': return a.value === b.value;
        case 'void': return true;
        default: return false;
    }
}
function schemeEqual(a, b, visited) {
    if (a === b)
        return true;
    if (a.tag === 'pair' && b.tag === 'pair') {
        if (!visited)
            visited = new Set();
        // Track visited pairs by identity to handle cycles
        const key = a;
        if (visited.has(key))
            return true; // assume equal to break cycle
        visited.add(key);
        return schemeEqual(a.car, b.car, visited) && schemeEqual(a.cdr, b.cdr, visited);
    }
    if (a.tag !== b.tag) {
        if ((a.tag === 'number' || a.tag === 'rational') && (b.tag === 'number' || b.tag === 'rational')) {
            return numericFloat(a, 'equal?') === numericFloat(b, 'equal?');
        }
        return false;
    }
    switch (a.tag) {
        case 'number': return a.value === b.value;
        case 'rational': return a.num === b.num && a.den === b.den;
        case 'boolean': return a.value === b.value;
        case 'string': return a.value === b.value;
        case 'symbol': return a.value === b.value;
        case 'char': return a.value === b.value;
        case 'void': return true;
        case 'list': {
            const bList = b;
            if (a.elements.length !== bList.elements.length)
                return false;
            return a.elements.every((e, i) => schemeEqual(e, bList.elements[i], visited));
        }
        case 'vector': {
            const bVec = b;
            if (a.elements.length !== bVec.elements.length)
                return false;
            return a.elements.every((e, i) => schemeEqual(e, bVec.elements[i], visited));
        }
        default: return a === b;
    }
}
function expectChar(val, op, p) {
    if (val.tag !== 'char')
        throw posError(`${op}: expected char`, p);
    return val.value;
}
function expectString(val, op, p) {
    if (val.tag !== 'string')
        throw posError(`${op}: expected string`, p);
    return val.value;
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
    // CPS-aware builtins (handled in applyK, but need env entries)
    env.set('apply', { tag: 'builtin', name: 'apply', fn: () => { throw new EvalError('internal: apply handled by applyK'); } });
    env.set('dynamic-wind', { tag: 'builtin', name: 'dynamic-wind', fn: () => { throw new EvalError('internal: dynamic-wind handled by applyK'); } });
    env.set('raise', { tag: 'builtin', name: 'raise', fn: () => { throw new EvalError('internal: raise handled by applyK'); } });
    env.set('with-exception-handler', { tag: 'builtin', name: 'with-exception-handler', fn: () => { throw new EvalError('internal: with-exception-handler handled by applyK'); } });
    env.set('values', { tag: 'builtin', name: 'values', fn: (args) => {
            if (args.length === 1)
                return args[0];
            return { tag: 'values', elements: args };
        } });
    env.set('call-with-values', { tag: 'builtin', name: 'call-with-values', fn: () => { throw new EvalError('internal: call-with-values handled by applyK'); } });
    env.set('map', { tag: 'builtin', name: 'map', fn: () => { throw new EvalError('internal: map handled by applyK'); } });
    env.set('for-each', { tag: 'builtin', name: 'for-each', fn: () => { throw new EvalError('internal: for-each handled by applyK'); } });
    // Arithmetic
    defBuiltin('+', (args, p) => {
        for (const a of args)
            if (!isNumericVal(a))
                throw posError('+: expected number', p);
        if (args.every(isExactNum)) {
            let n = 0, d = 1;
            for (const a of args) {
                const [an, ad] = toRatParts(a);
                n = n * ad + an * d;
                d = d * ad;
            }
            return makeRational(n, d, p);
        }
        let s = 0;
        for (const a of args)
            s += numericFloat(a, '+', p);
        return { tag: 'number', value: s };
    });
    defBuiltin('-', (args, p) => {
        if (args.length === 0)
            throw posError('-: need at least 1 argument', p);
        for (const a of args)
            if (!isNumericVal(a))
                throw posError('-: expected number', p);
        if (args.every(isExactNum)) {
            if (args.length === 1) {
                const [n, d] = toRatParts(args[0]);
                return makeRational(-n, d, p);
            }
            let [n, d] = toRatParts(args[0]);
            for (let i = 1; i < args.length; i++) {
                const [an, ad] = toRatParts(args[i]);
                n = n * ad - an * d;
                d = d * ad;
            }
            return makeRational(n, d, p);
        }
        if (args.length === 1)
            return { tag: 'number', value: -numericFloat(args[0], '-', p) };
        let r = numericFloat(args[0], '-', p);
        for (let i = 1; i < args.length; i++)
            r -= numericFloat(args[i], '-', p);
        return { tag: 'number', value: r };
    });
    defBuiltin('*', (args, p) => {
        for (const a of args)
            if (!isNumericVal(a))
                throw posError('*: expected number', p);
        if (args.every(isExactNum)) {
            let n = 1, d = 1;
            for (const a of args) {
                const [an, ad] = toRatParts(a);
                n *= an;
                d *= ad;
            }
            return makeRational(n, d, p);
        }
        let s = 1;
        for (const a of args)
            s *= numericFloat(a, '*', p);
        return { tag: 'number', value: s };
    });
    defBuiltin('/', (args, p) => {
        if (args.length < 2)
            throw posError('/: need at least 2 arguments', p);
        for (const a of args)
            if (!isNumericVal(a))
                throw posError('/: expected number', p);
        if (args.every(isExactNum)) {
            let [n, d] = toRatParts(args[0]);
            for (let i = 1; i < args.length; i++) {
                const [an, ad] = toRatParts(args[i]);
                if (an === 0)
                    throw posError('division by zero', p);
                n *= ad;
                d *= an;
            }
            return makeRational(n, d, p);
        }
        let r = numericFloat(args[0], '/', p);
        for (let i = 1; i < args.length; i++) {
            const dv = numericFloat(args[i], '/', p);
            if (dv === 0)
                throw posError('division by zero', p);
            r /= dv;
        }
        return { tag: 'number', value: r };
    });
    defBuiltin('<', (args, p) => { if (args.length !== 2)
        throw posError('<: need 2 arguments', p); return { tag: 'boolean', value: numericFloat(args[0], '<', p) < numericFloat(args[1], '<', p) }; });
    defBuiltin('>', (args, p) => { if (args.length !== 2)
        throw posError('>: need 2 arguments', p); return { tag: 'boolean', value: numericFloat(args[0], '>', p) > numericFloat(args[1], '>', p) }; });
    defBuiltin('=', (args, p) => { if (args.length !== 2)
        throw posError('=: need 2 arguments', p); return { tag: 'boolean', value: numericFloat(args[0], '=', p) === numericFloat(args[1], '=', p) }; });
    defBuiltin('<=', (args, p) => { if (args.length !== 2)
        throw posError('<=: need 2 arguments', p); return { tag: 'boolean', value: numericFloat(args[0], '<=', p) <= numericFloat(args[1], '<=', p) }; });
    defBuiltin('>=', (args, p) => { if (args.length !== 2)
        throw posError('>=: need 2 arguments', p); return { tag: 'boolean', value: numericFloat(args[0], '>=', p) >= numericFloat(args[1], '>=', p) }; });
    defBuiltin('not', (args, p) => { if (args.length !== 1)
        throw posError('not: need 1 argument', p); return { tag: 'boolean', value: !isTruthy(args[0]) }; });
    // Pair / List operations
    defBuiltin('cons', (args, p) => {
        if (args.length !== 2)
            throw posError('cons: need 2 arguments', p);
        return makePair(args[0], args[1]);
    });
    defBuiltin('car', (args, p) => {
        if (args.length !== 1)
            throw posError('car: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('car: not a pair', p);
        return args[0].car;
    });
    defBuiltin('cdr', (args, p) => {
        if (args.length !== 1)
            throw posError('cdr: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('cdr: not a pair', p);
        return args[0].cdr;
    });
    defBuiltin('caar', (args, p) => {
        if (args.length !== 1)
            throw posError('caar: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('caar: not a pair', p);
        const inner = args[0].car;
        if (inner.tag !== 'pair')
            throw posError('caar: not a pair', p);
        return inner.car;
    });
    defBuiltin('cadr', (args, p) => {
        if (args.length !== 1)
            throw posError('cadr: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('cadr: not a pair', p);
        const inner = args[0].cdr;
        if (inner.tag !== 'pair')
            throw posError('cadr: not a pair', p);
        return inner.car;
    });
    defBuiltin('cdar', (args, p) => {
        if (args.length !== 1)
            throw posError('cdar: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('cdar: not a pair', p);
        const inner = args[0].car;
        if (inner.tag !== 'pair')
            throw posError('cdar: not a pair', p);
        return inner.cdr;
    });
    defBuiltin('cddr', (args, p) => {
        if (args.length !== 1)
            throw posError('cddr: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('cddr: not a pair', p);
        const inner = args[0].cdr;
        if (inner.tag !== 'pair')
            throw posError('cddr: not a pair', p);
        return inner.cdr;
    });
    defBuiltin('caddr', (args, p) => {
        if (args.length !== 1)
            throw posError('caddr: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('caddr: not a pair', p);
        const d = args[0].cdr;
        if (d.tag !== 'pair')
            throw posError('caddr: not a pair', p);
        const dd = d.cdr;
        if (dd.tag !== 'pair')
            throw posError('caddr: not a pair', p);
        return dd.car;
    });
    defBuiltin('cadddr', (args, p) => {
        if (args.length !== 1)
            throw posError('cadddr: need 1 argument', p);
        if (args[0].tag !== 'pair')
            throw posError('cadddr: not a pair', p);
        let cur = args[0];
        for (let i = 0; i < 3; i++) {
            cur = cur.cdr;
            if (cur.tag !== 'pair')
                throw posError('cadddr: not a pair', p);
        }
        return cur.car;
    });
    defBuiltin('set-car!', (args, p) => {
        if (args.length !== 2)
            throw posError('set-car!: need 2 arguments', p);
        if (args[0].tag !== 'pair')
            throw posError('set-car!: not a pair', p);
        args[0].car = args[1];
        return { tag: 'void' };
    });
    defBuiltin('set-cdr!', (args, p) => {
        if (args.length !== 2)
            throw posError('set-cdr!: need 2 arguments', p);
        if (args[0].tag !== 'pair')
            throw posError('set-cdr!: not a pair', p);
        args[0].cdr = args[1];
        return { tag: 'void' };
    });
    defBuiltin('null?', (args, p) => {
        if (args.length !== 1)
            throw posError('null?: need 1 argument', p);
        return { tag: 'boolean', value: isNull(args[0]) };
    });
    defBuiltin('list', args => {
        return arrayToList(args);
    });
    defBuiltin('length', (args, p) => {
        if (args.length !== 1)
            throw posError('length: need 1 argument', p);
        return { tag: 'number', value: listLength(args[0]) };
    });
    defBuiltin('append', (args, _p) => {
        if (args.length === 0)
            return EMPTY_LIST;
        if (args.length === 1)
            return args[0];
        // Copy all but last, chain onto last
        let result = args[args.length - 1];
        for (let i = args.length - 2; i >= 0; i--) {
            const elems = listToArray(args[i]);
            for (let j = elems.length - 1; j >= 0; j--) {
                result = makePair(elems[j], result);
            }
        }
        return result;
    });
    defBuiltin('reverse', (args, p) => {
        if (args.length !== 1)
            throw posError('reverse: need 1 argument', p);
        let result = EMPTY_LIST;
        let cur = args[0];
        while (cur.tag === 'pair') {
            result = makePair(cur.car, result);
            cur = cur.cdr;
        }
        return result;
    });
    // Type predicates
    defBuiltin('number?', (args, p) => { if (args.length !== 1)
        throw posError('number?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'number' || args[0].tag === 'rational' }; });
    defBuiltin('string?', (args, p) => { if (args.length !== 1)
        throw posError('string?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'string' }; });
    defBuiltin('boolean?', (args, p) => { if (args.length !== 1)
        throw posError('boolean?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'boolean' }; });
    defBuiltin('pair?', (args, p) => { if (args.length !== 1)
        throw posError('pair?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'pair' }; });
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
    // syntax-case support builtins
    defBuiltin('syntax->datum', (args, p) => {
        if (args.length !== 1)
            throw posError('syntax->datum: need 1 argument', p);
        return args[0]; // In our system, syntax objects are just SchemeVals
    });
    defBuiltin('datum->syntax', (args, p) => {
        if (args.length !== 2)
            throw posError('datum->syntax: need 2 arguments', p);
        // First arg is template id (context), second is datum to wrap
        // In our system, just return the datum as-is
        return args[1];
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
        if (args[0].tag !== 'string')
            throw posError('string-set!: expected string', p);
        if (!args[0].mutable)
            throw posError('string-set!: string is immutable', p);
        const idx = expectNumber(args[1], 'string-set!', p);
        if (args[2].tag !== 'char')
            throw posError('string-set!: expected char', p);
        const s = args[0].value;
        if (idx < 0 || idx >= s.length)
            throw posError('string-set!: index out of range', p);
        args[0].value = s.substring(0, idx) + args[2].value + s.substring(idx + 1);
        return { tag: 'void' };
    });
    defBuiltin('string->list', (args, p) => {
        if (args.length !== 1)
            throw posError('string->list: need 1 argument', p);
        if (args[0].tag !== 'string')
            throw posError('string->list: expected string', p);
        const chars = [];
        for (const ch of args[0].value) {
            chars.push({ tag: 'char', value: ch });
        }
        return arrayToList(chars);
    });
    defBuiltin('list->string', (args, p) => {
        if (args.length !== 1)
            throw posError('list->string: need 1 argument', p);
        let s = '';
        let cur = args[0];
        while (cur.tag === 'pair') {
            if (cur.car.tag !== 'char')
                throw posError('list->string: expected list of chars', p);
            s += cur.car.value;
            cur = cur.cdr;
        }
        return { tag: 'string', value: s };
    });
    defBuiltin('char->integer', (args, p) => {
        if (args.length !== 1)
            throw posError('char->integer: need 1 argument', p);
        if (args[0].tag !== 'char')
            throw posError('char->integer: expected char', p);
        return { tag: 'number', value: args[0].value.codePointAt(0) };
    });
    defBuiltin('integer->char', (args, p) => {
        if (args.length !== 1)
            throw posError('integer->char: need 1 argument', p);
        const n = expectNumber(args[0], 'integer->char', p);
        return { tag: 'char', value: String.fromCodePoint(n) };
    });
    defBuiltin('string-copy', (args, p) => {
        if (args.length !== 1)
            throw posError('string-copy: need 1 argument', p);
        if (args[0].tag !== 'string')
            throw posError('string-copy: expected string', p);
        return { tag: 'string', value: args[0].value, mutable: true };
    });
    defBuiltin('make-string', (args, p) => {
        if (args.length < 1 || args.length > 2)
            throw posError('make-string: need 1-2 arguments', p);
        const n = expectNumber(args[0], 'make-string', p);
        const ch = args.length === 2 ? expectChar(args[1], 'make-string', p) : ' ';
        return { tag: 'string', value: ch.repeat(n), mutable: true };
    });
    defBuiltin('string', (args, p) => {
        let s = '';
        for (const a of args) {
            if (a.tag !== 'char')
                throw posError('string: expected char', p);
            s += a.value;
        }
        return { tag: 'string', value: s };
    });
    // eq? / eqv? / equal?
    defBuiltin('eq?', (args, p) => {
        if (args.length !== 2)
            throw posError('eq?: need 2 arguments', p);
        const [a, b] = args;
        if (a === b)
            return { tag: 'boolean', value: true };
        if (a.tag !== b.tag)
            return { tag: 'boolean', value: false };
        switch (a.tag) {
            case 'number': return { tag: 'boolean', value: a.value === b.value };
            case 'boolean': return { tag: 'boolean', value: a.value === b.value };
            case 'symbol': return { tag: 'boolean', value: a.value === b.value };
            case 'char': return { tag: 'boolean', value: a.value === b.value };
            case 'void': return { tag: 'boolean', value: true };
            default: return { tag: 'boolean', value: false };
        }
    });
    defBuiltin('equal?', (args, p) => {
        if (args.length !== 2)
            throw posError('equal?: need 2 arguments', p);
        return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
    });
    defBuiltin('eqv?', (args, p) => {
        if (args.length !== 2)
            throw posError('eqv?: need 2 arguments', p);
        return { tag: 'boolean', value: schemeEqv(args[0], args[1]) };
    });
    // Vector operations
    defBuiltin('vector', args => ({ tag: 'vector', elements: [...args] }));
    defBuiltin('make-vector', (args, p) => {
        if (args.length < 1 || args.length > 2)
            throw posError('make-vector: need 1-2 arguments', p);
        const n = expectNumber(args[0], 'make-vector', p);
        const fill = args.length === 2 ? args[1] : { tag: 'number', value: 0 };
        const elements = new Array(n);
        for (let i = 0; i < n; i++)
            elements[i] = fill;
        return { tag: 'vector', elements };
    });
    defBuiltin('vector-ref', (args, p) => {
        if (args.length !== 2)
            throw posError('vector-ref: need 2 arguments', p);
        if (args[0].tag !== 'vector')
            throw posError('vector-ref: expected vector', p);
        const idx = expectNumber(args[1], 'vector-ref', p);
        if (idx < 0 || idx >= args[0].elements.length)
            throw posError('vector-ref: index out of range', p);
        return args[0].elements[idx];
    });
    defBuiltin('vector-set!', (args, p) => {
        if (args.length !== 3)
            throw posError('vector-set!: need 3 arguments', p);
        if (args[0].tag !== 'vector')
            throw posError('vector-set!: expected vector', p);
        const idx = expectNumber(args[1], 'vector-set!', p);
        if (idx < 0 || idx >= args[0].elements.length)
            throw posError('vector-set!: index out of range', p);
        args[0].elements[idx] = args[2];
        return { tag: 'void' };
    });
    defBuiltin('vector-length', (args, p) => {
        if (args.length !== 1)
            throw posError('vector-length: need 1 argument', p);
        if (args[0].tag !== 'vector')
            throw posError('vector-length: expected vector', p);
        return { tag: 'number', value: args[0].elements.length };
    });
    defBuiltin('vector?', (args, p) => {
        if (args.length !== 1)
            throw posError('vector?: need 1 argument', p);
        return { tag: 'boolean', value: args[0].tag === 'vector' };
    });
    defBuiltin('vector->list', (args, p) => {
        if (args.length !== 1)
            throw posError('vector->list: need 1 argument', p);
        if (args[0].tag !== 'vector')
            throw posError('vector->list: expected vector', p);
        return arrayToList([...args[0].elements]);
    });
    defBuiltin('list->vector', (args, p) => {
        if (args.length !== 1)
            throw posError('list->vector: need 1 argument', p);
        return { tag: 'vector', elements: listToArray(args[0]) };
    });
    // error
    defBuiltin('error', (args, p) => {
        const msg = args.map(a => a.tag === 'string' ? a.value : displayVal(a)).join(' ');
        throw posError(msg, p);
    });
    // Numeric utilities
    defBuiltin('abs', (args, p) => {
        if (args.length !== 1)
            throw posError('abs: need 1 argument', p);
        return { tag: 'number', value: Math.abs(expectNumber(args[0], 'abs', p)) };
    });
    defBuiltin('modulo', (args, p) => {
        if (args.length !== 2)
            throw posError('modulo: need 2 arguments', p);
        const a = expectNumber(args[0], 'modulo', p);
        const b = expectNumber(args[1], 'modulo', p);
        return { tag: 'number', value: ((a % b) + b) % b };
    });
    defBuiltin('remainder', (args, p) => {
        if (args.length !== 2)
            throw posError('remainder: need 2 arguments', p);
        const a = expectNumber(args[0], 'remainder', p);
        const b = expectNumber(args[1], 'remainder', p);
        return { tag: 'number', value: a % b };
    });
    defBuiltin('quotient', (args, p) => {
        if (args.length !== 2)
            throw posError('quotient: need 2 arguments', p);
        const a = expectNumber(args[0], 'quotient', p);
        const b = expectNumber(args[1], 'quotient', p);
        return { tag: 'number', value: Math.trunc(a / b) };
    });
    defBuiltin('min', (args, p) => {
        if (args.length < 1)
            throw posError('min: need at least 1 argument', p);
        let m = expectNumber(args[0], 'min', p);
        for (let i = 1; i < args.length; i++) {
            const v = expectNumber(args[i], 'min', p);
            if (v < m)
                m = v;
        }
        return { tag: 'number', value: m };
    });
    defBuiltin('max', (args, p) => {
        if (args.length < 1)
            throw posError('max: need at least 1 argument', p);
        let m = expectNumber(args[0], 'max', p);
        for (let i = 1; i < args.length; i++) {
            const v = expectNumber(args[i], 'max', p);
            if (v > m)
                m = v;
        }
        return { tag: 'number', value: m };
    });
    defBuiltin('expt', (args, p) => {
        if (args.length !== 2)
            throw posError('expt: need 2 arguments', p);
        const base = expectNumber(args[0], 'expt', p);
        const exp = expectNumber(args[1], 'expt', p);
        return { tag: 'number', value: Math.pow(base, exp) };
    });
    defBuiltin('gcd', (args, p) => {
        if (args.length === 0)
            return { tag: 'number', value: 0 };
        let result = Math.abs(expectNumber(args[0], 'gcd', p));
        for (let i = 1; i < args.length; i++)
            result = gcd(result, Math.abs(expectNumber(args[i], 'gcd', p)));
        return { tag: 'number', value: result };
    });
    defBuiltin('lcm', (args, p) => {
        if (args.length === 0)
            return { tag: 'number', value: 1 };
        let result = Math.abs(expectNumber(args[0], 'lcm', p));
        for (let i = 1; i < args.length; i++) {
            const b = Math.abs(expectNumber(args[i], 'lcm', p));
            result = result === 0 && b === 0 ? 0 : Math.abs(result * b) / gcd(result, b);
        }
        return { tag: 'number', value: result };
    });
    defBuiltin('truncate', (args, p) => {
        if (args.length !== 1)
            throw posError('truncate: need 1 argument', p);
        return { tag: 'number', value: Math.trunc(expectNumber(args[0], 'truncate', p)) };
    });
    defBuiltin('round', (args, p) => {
        if (args.length !== 1)
            throw posError('round: need 1 argument', p);
        return { tag: 'number', value: Math.round(expectNumber(args[0], 'round', p)) };
    });
    // Numeric predicates
    defBuiltin('zero?', (args, p) => { if (args.length !== 1)
        throw posError('zero?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'zero?', p) === 0 }; });
    defBuiltin('positive?', (args, p) => { if (args.length !== 1)
        throw posError('positive?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'positive?', p) > 0 }; });
    defBuiltin('negative?', (args, p) => { if (args.length !== 1)
        throw posError('negative?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'negative?', p) < 0 }; });
    defBuiltin('odd?', (args, p) => { if (args.length !== 1)
        throw posError('odd?: need 1 argument', p); return { tag: 'boolean', value: Math.abs(expectNumber(args[0], 'odd?', p)) % 2 === 1 }; });
    defBuiltin('even?', (args, p) => { if (args.length !== 1)
        throw posError('even?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'even?', p) % 2 === 0 }; });
    // Exact/inexact predicates and conversions
    defBuiltin('exact?', (args, p) => {
        if (args.length !== 1)
            throw posError('exact?: need 1 argument', p);
        return { tag: 'boolean', value: isExactNum(args[0]) };
    });
    defBuiltin('inexact?', (args, p) => {
        if (args.length !== 1)
            throw posError('inexact?: need 1 argument', p);
        const v = args[0];
        return { tag: 'boolean', value: v.tag === 'number' && !Number.isInteger(v.value) };
    });
    defBuiltin('rational?', (args, p) => {
        if (args.length !== 1)
            throw posError('rational?: need 1 argument', p);
        return { tag: 'boolean', value: isExactNum(args[0]) };
    });
    defBuiltin('integer?', (args, p) => {
        if (args.length !== 1)
            throw posError('integer?: need 1 argument', p);
        const v = args[0];
        if (v.tag === 'number')
            return { tag: 'boolean', value: Number.isInteger(v.value) };
        if (v.tag === 'rational')
            return { tag: 'boolean', value: v.den === 1 };
        return { tag: 'boolean', value: false };
    });
    defBuiltin('exact->inexact', (args, p) => {
        if (args.length !== 1)
            throw posError('exact->inexact: need 1 argument', p);
        const v = args[0];
        const f = numericFloat(v, 'exact->inexact', p);
        return { tag: 'number', value: f, exact: false };
    });
    defBuiltin('inexact->exact', (args, p) => {
        if (args.length !== 1)
            throw posError('inexact->exact: need 1 argument', p);
        const v = args[0];
        const f = numericFloat(v, 'inexact->exact', p);
        if (Number.isInteger(f))
            return { tag: 'number', value: f };
        const eps = 1e-10;
        let bestNum = Math.round(f), bestDen = 1;
        for (let d = 1; d <= 1000000; d++) {
            const n = Math.round(f * d);
            if (Math.abs(n / d - f) < eps) {
                bestNum = n;
                bestDen = d;
                break;
            }
        }
        return makeRational(bestNum, bestDen, p);
    });
    defBuiltin('numerator', (args, p) => {
        if (args.length !== 1)
            throw posError('numerator: need 1 argument', p);
        const v = args[0];
        if (v.tag === 'rational')
            return { tag: 'number', value: v.num };
        if (v.tag === 'number' && Number.isInteger(v.value))
            return { tag: 'number', value: v.value };
        throw posError('numerator: expected exact number', p);
    });
    defBuiltin('denominator', (args, p) => {
        if (args.length !== 1)
            throw posError('denominator: need 1 argument', p);
        const v = args[0];
        if (v.tag === 'rational')
            return { tag: 'number', value: v.den };
        if (v.tag === 'number' && Number.isInteger(v.value))
            return { tag: 'number', value: 1 };
        throw posError('denominator: expected exact number', p);
    });
    // List utilities
    defBuiltin('list-ref', (args, p) => {
        if (args.length !== 2)
            throw posError('list-ref: need 2 arguments', p);
        let idx = expectNumber(args[1], 'list-ref', p);
        let cur = args[0];
        while (idx > 0 && cur.tag === 'pair') {
            cur = cur.cdr;
            idx--;
        }
        if (cur.tag !== 'pair')
            throw posError('list-ref: index out of range', p);
        return cur.car;
    });
    defBuiltin('list-tail', (args, p) => {
        if (args.length !== 2)
            throw posError('list-tail: need 2 arguments', p);
        let idx = expectNumber(args[1], 'list-tail', p);
        let cur = args[0];
        while (idx > 0) {
            if (cur.tag !== 'pair')
                throw posError('list-tail: index out of range', p);
            cur = cur.cdr;
            idx--;
        }
        return cur;
    });
    defBuiltin('list?', (args, p) => {
        if (args.length !== 1)
            throw posError('list?: need 1 argument', p);
        const v = args[0];
        if (isNull(v))
            return { tag: 'boolean', value: true };
        if (v.tag !== 'pair')
            return { tag: 'boolean', value: false };
        // Floyd's tortoise and hare for cycle detection
        let slow = v;
        let fast = v;
        while (true) {
            if (fast.tag !== 'pair')
                return { tag: 'boolean', value: isNull(fast) };
            fast = fast.cdr;
            if (fast.tag !== 'pair')
                return { tag: 'boolean', value: isNull(fast) };
            fast = fast.cdr;
            slow = slow.cdr;
            if (slow === fast)
                return { tag: 'boolean', value: false };
        }
    });
    defBuiltin('assoc', (args, p) => {
        if (args.length !== 2)
            throw posError('assoc: need 2 arguments', p);
        const key = args[0];
        let cur = args[1];
        while (cur.tag === 'pair') {
            const entry = cur.car;
            if (entry.tag === 'pair' && schemeEqual(key, entry.car))
                return entry;
            cur = cur.cdr;
        }
        return { tag: 'boolean', value: false };
    });
    defBuiltin('assv', (args, p) => {
        if (args.length !== 2)
            throw posError('assv: need 2 arguments', p);
        const key = args[0];
        let cur = args[1];
        while (cur.tag === 'pair') {
            const entry = cur.car;
            if (entry.tag === 'pair' && schemeEqv(key, entry.car))
                return entry;
            cur = cur.cdr;
        }
        return { tag: 'boolean', value: false };
    });
    defBuiltin('member', (args, p) => {
        if (args.length !== 2)
            throw posError('member: need 2 arguments', p);
        const obj = args[0];
        let cur = args[1];
        while (cur.tag === 'pair') {
            if (schemeEqual(obj, cur.car))
                return cur;
            cur = cur.cdr;
        }
        return { tag: 'boolean', value: false };
    });
    defBuiltin('memq', (args, p) => {
        if (args.length !== 2)
            throw posError('memq: need 2 arguments', p);
        const obj = args[0];
        let cur = args[1];
        while (cur.tag === 'pair') {
            // eq? semantics
            if (obj === cur.car)
                return cur;
            if (obj.tag === cur.car.tag) {
                switch (obj.tag) {
                    case 'number':
                        if (obj.value === cur.car.value)
                            return cur;
                        break;
                    case 'boolean':
                        if (obj.value === cur.car.value)
                            return cur;
                        break;
                    case 'symbol':
                        if (obj.value === cur.car.value)
                            return cur;
                        break;
                    case 'char':
                        if (obj.value === cur.car.value)
                            return cur;
                        break;
                }
            }
            cur = cur.cdr;
        }
        return { tag: 'boolean', value: false };
    });
    defBuiltin('memv', (args, p) => {
        if (args.length !== 2)
            throw posError('memv: need 2 arguments', p);
        const obj = args[0];
        let cur = args[1];
        while (cur.tag === 'pair') {
            if (schemeEqv(obj, cur.car))
                return cur;
            cur = cur.cdr;
        }
        return { tag: 'boolean', value: false };
    });
    // Character operations
    defBuiltin('char-alphabetic?', (args, p) => { if (args.length !== 1)
        throw posError('char-alphabetic?: need 1 argument', p); const c = expectChar(args[0], 'char-alphabetic?', p); return { tag: 'boolean', value: /^[a-zA-Z]$/.test(c) }; });
    defBuiltin('char-numeric?', (args, p) => { if (args.length !== 1)
        throw posError('char-numeric?: need 1 argument', p); const c = expectChar(args[0], 'char-numeric?', p); return { tag: 'boolean', value: /^[0-9]$/.test(c) }; });
    defBuiltin('char-upcase', (args, p) => { if (args.length !== 1)
        throw posError('char-upcase: need 1 argument', p); return { tag: 'char', value: expectChar(args[0], 'char-upcase', p).toUpperCase() }; });
    defBuiltin('char-downcase', (args, p) => { if (args.length !== 1)
        throw posError('char-downcase: need 1 argument', p); return { tag: 'char', value: expectChar(args[0], 'char-downcase', p).toLowerCase() }; });
    defBuiltin('char=?', (args, p) => { if (args.length !== 2)
        throw posError('char=?: need 2 arguments', p); return { tag: 'boolean', value: expectChar(args[0], 'char=?', p) === expectChar(args[1], 'char=?', p) }; });
    defBuiltin('char<?', (args, p) => { if (args.length !== 2)
        throw posError('char<?: need 2 arguments', p); return { tag: 'boolean', value: expectChar(args[0], 'char<?', p) < expectChar(args[1], 'char<?', p) }; });
    // String comparison operations
    defBuiltin('string=?', (args, p) => { if (args.length !== 2)
        throw posError('string=?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string=?', p) === expectString(args[1], 'string=?', p) }; });
    defBuiltin('string<?', (args, p) => { if (args.length !== 2)
        throw posError('string<?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string<?', p) < expectString(args[1], 'string<?', p) }; });
    defBuiltin('string>?', (args, p) => { if (args.length !== 2)
        throw posError('string>?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string>?', p) > expectString(args[1], 'string>?', p) }; });
    defBuiltin('string<=?', (args, p) => { if (args.length !== 2)
        throw posError('string<=?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string<=?', p) <= expectString(args[1], 'string<=?', p) }; });
    defBuiltin('string>=?', (args, p) => { if (args.length !== 2)
        throw posError('string>=?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string>=?', p) >= expectString(args[1], 'string>=?', p) }; });
    defBuiltin('string-ci=?', (args, p) => { if (args.length !== 2)
        throw posError('string-ci=?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string-ci=?', p).toLowerCase() === expectString(args[1], 'string-ci=?', p).toLowerCase() }; });
    defBuiltin('string-upcase', (args, p) => { if (args.length !== 1)
        throw posError('string-upcase: need 1 argument', p); return { tag: 'string', value: expectString(args[0], 'string-upcase', p).toUpperCase() }; });
    defBuiltin('string-downcase', (args, p) => { if (args.length !== 1)
        throw posError('string-downcase: need 1 argument', p); return { tag: 'string', value: expectString(args[0], 'string-downcase', p).toLowerCase() }; });
    return env;
}
// ── Display ────────────────────────────────────────────────────────
function displayVal(val, seen) {
    switch (val.tag) {
        case 'number': {
            if (val.exact === false && Number.isInteger(val.value))
                return val.value.toFixed(1);
            return String(val.value);
        }
        case 'rational': return `${val.num}/${val.den}`;
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
        case 'list': return `(${val.elements.map(e => displayVal(e, seen)).join(' ')})`;
        case 'pair': {
            if (!seen)
                seen = new Set();
            if (seen.has(val))
                return '(...)';
            seen.add(val);
            let result = '(' + displayVal(val.car, seen);
            let cur = val.cdr;
            while (cur.tag === 'pair') {
                if (seen.has(cur)) {
                    result += ' ...';
                    cur = EMPTY_LIST;
                    break;
                }
                seen.add(cur);
                result += ' ' + displayVal(cur.car, seen);
                cur = cur.cdr;
            }
            if (!isNull(cur)) {
                result += ' . ' + displayVal(cur, seen);
            }
            result += ')';
            return result;
        }
        case 'vector': return `#(${val.elements.map(e => displayVal(e, seen)).join(' ')})`;
        case 'void': return '';
        case 'lambda': return '#<procedure>';
        case 'builtin': return '#<procedure>';
        case 'continuation': return '#<procedure>';
        case 'callcc': return '#<procedure>';
        case 'macro': return '#<macro>';
        case 'syntaxTransformer': return '#<syntax-transformer>';
        case 'values': return val.elements.map(e => displayVal(e, seen)).join('\n');
        case 'record': return `#<${val.typeName}>`;
    }
}
function writeVal(val) {
    return displayVal(val);
}
function displayForDisplay(val, seen) {
    switch (val.tag) {
        case 'string': return val.value;
        case 'pair': {
            if (!seen)
                seen = new Set();
            if (seen.has(val))
                return '(...)';
            seen.add(val);
            let result = '(' + displayForDisplay(val.car, seen);
            let cur = val.cdr;
            while (cur.tag === 'pair') {
                if (seen.has(cur)) {
                    result += ' ...';
                    cur = EMPTY_LIST;
                    break;
                }
                seen.add(cur);
                result += ' ' + displayForDisplay(cur.car, seen);
                cur = cur.cdr;
            }
            if (!isNull(cur)) {
                result += ' . ' + displayForDisplay(cur, seen);
            }
            result += ')';
            return result;
        }
        case 'list': return `(${val.elements.map(e => displayForDisplay(e, seen)).join(' ')})`;
        case 'vector': return `#(${val.elements.map(e => displayForDisplay(e, seen)).join(' ')})`;
        case 'char': return val.value;
        default: return displayVal(val, seen);
    }
}
// ── Public API ─────────────────────────────────────────────────────
export function evalStr(input) {
    const exprs = parseAll(input);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    windStack = [];
    exHandlers = [];
    const env = makeGlobalEnv();
    const result = trampoline(evalSeqArr(exprs, env, v => done(v)));
    return displayVal(result);
}
export function evalStrWithOutput(input) {
    const exprs = parseAll(input);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    windStack = [];
    exHandlers = [];
    const output = [];
    const env = makeGlobalEnv(output);
    const result = trampoline(evalSeqArr(exprs, env, v => done(v)));
    return { result: displayVal(result), output: output.join('') };
}
