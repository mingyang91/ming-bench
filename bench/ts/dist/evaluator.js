import { EvalError } from './evalError.js';
function bounce(thunk) {
    return { done: false, thunk };
}
function done(value) {
    return { done: true, value };
}
// Depth-limited direct calls: avoid bounce allocation in common case
let callDepth = 0;
const MAX_CALL_DEPTH = 200;
function callK(k, val) {
    if (callDepth < MAX_CALL_DEPTH) {
        callDepth++;
        try {
            return k(val);
        }
        finally {
            callDepth--;
        }
    }
    return bounce(() => { callDepth = 0; return k(val); });
}
function runTrampoline(result) {
    while (!result.done)
        result = result.thunk();
    return result.value;
}
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
// ── Helpers ────────────────────────────────────────────────────────
function posStr(p) {
    return p ? `${p.line}:${p.col}: ` : '';
}
const NIL = { tag: 'nil' };
const VOID = { tag: 'void' };
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
function parseDotParams(paramExprs) {
    const dotIdx = paramExprs.findIndex(p => p.tag === 'symbol' && p.value === '.');
    if (dotIdx >= 0) {
        const params = paramExprs.slice(0, dotIdx).map(p => p.value);
        const rest = paramExprs[dotIdx + 1].value;
        return { params, rest };
    }
    return { params: paramExprs.map(p => p.value) };
}
// ── Hygienic Macros (syntax-rules) ─────────────────────────────────
let gensymCounter = 0;
function gensym(base) {
    return `${base}__hyg_${++gensymCounter}`;
}
const SPECIAL_FORMS = new Set([
    'if', 'define', 'lambda', 'and', 'or', 'let', 'begin',
    'set!', 'string-set!', 'cond', 'quote', 'define-syntax',
]);
function collectPatternVars(pattern, literals, vars) {
    if (pattern.tag === 'symbol') {
        if (!literals.includes(pattern.value) && pattern.value !== '_' && pattern.value !== '...') {
            vars.add(pattern.value);
        }
    }
    else if (pattern.tag === 'list') {
        for (const elem of pattern.value) {
            if (elem.tag === 'symbol' && elem.value === '...')
                continue;
            collectPatternVars(elem, literals, vars);
        }
    }
}
function matchPattern(pattern, input, literals, bindings) {
    if (pattern.tag === 'symbol') {
        if (pattern.value === '_')
            return true;
        if (pattern.value === '...')
            return false;
        if (literals.includes(pattern.value)) {
            return input.tag === 'symbol' && input.value === pattern.value;
        }
        bindings.set(pattern.value, input);
        return true;
    }
    if (pattern.tag === 'list' && input.tag === 'list') {
        const pats = pattern.value;
        const inps = input.value;
        const ellipsisIdx = pats.findIndex(p => p.tag === 'symbol' && p.value === '...');
        if (ellipsisIdx >= 0) {
            const beforePats = pats.slice(0, ellipsisIdx - 1);
            const ellipsisPat = pats[ellipsisIdx - 1];
            const afterPats = pats.slice(ellipsisIdx + 1);
            if (inps.length < beforePats.length + afterPats.length)
                return false;
            for (let i = 0; i < beforePats.length; i++) {
                if (!matchPattern(beforePats[i], inps[i], literals, bindings))
                    return false;
            }
            const ellipsisVars = new Set();
            collectPatternVars(ellipsisPat, literals, ellipsisVars);
            for (const v of ellipsisVars)
                bindings.set(v, []);
            const ellipsisCount = inps.length - beforePats.length - afterPats.length;
            for (let i = 0; i < ellipsisCount; i++) {
                const sub = new Map();
                if (!matchPattern(ellipsisPat, inps[beforePats.length + i], literals, sub))
                    return false;
                for (const v of ellipsisVars)
                    bindings.get(v).push(sub.get(v));
            }
            for (let i = 0; i < afterPats.length; i++) {
                if (!matchPattern(afterPats[i], inps[inps.length - afterPats.length + i], literals, bindings))
                    return false;
            }
            return true;
        }
        if (pats.length !== inps.length)
            return false;
        for (let i = 0; i < pats.length; i++) {
            if (!matchPattern(pats[i], inps[i], literals, bindings))
                return false;
        }
        return true;
    }
    if (pattern.tag === input.tag) {
        if (pattern.tag === 'number' && input.tag === 'number')
            return pattern.value === input.value;
        if (pattern.tag === 'boolean' && input.tag === 'boolean')
            return pattern.value === input.value;
    }
    return false;
}
function collectTemplateEllipsisVars(tmpl, bindings, vars) {
    if (tmpl.tag === 'symbol' && bindings.has(tmpl.value) && Array.isArray(bindings.get(tmpl.value))) {
        vars.add(tmpl.value);
    }
    else if (tmpl.tag === 'list') {
        for (const elem of tmpl.value)
            collectTemplateEllipsisVars(elem, bindings, vars);
    }
}
function expandTemplate(tmpl, bindings, renames) {
    if (tmpl.tag === 'symbol') {
        const name = tmpl.value;
        if (bindings.has(name)) {
            const val = bindings.get(name);
            if (Array.isArray(val))
                throw new EvalError('syntax-rules: ellipsis variable used outside ellipsis context');
            return val;
        }
        if (renames.has(name))
            return { tag: 'symbol', value: renames.get(name) };
        return tmpl;
    }
    if (tmpl.tag === 'list') {
        const result = [];
        const elems = tmpl.value;
        for (let i = 0; i < elems.length; i++) {
            const next = i + 1 < elems.length ? elems[i + 1] : null;
            if (next && next.tag === 'symbol' && next.value === '...') {
                const ellVars = new Set();
                collectTemplateEllipsisVars(elems[i], bindings, ellVars);
                if (ellVars.size > 0) {
                    const firstVar = ellVars.values().next().value;
                    const count = bindings.get(firstVar).length;
                    for (let j = 0; j < count; j++) {
                        const sub = new Map(bindings);
                        for (const v of ellVars)
                            sub.set(v, bindings.get(v)[j]);
                        result.push(expandTemplate(elems[i], sub, renames));
                    }
                }
                i++;
                continue;
            }
            result.push(expandTemplate(elems[i], bindings, renames));
        }
        return { tag: 'list', value: result };
    }
    return tmpl;
}
function collectAllSymbols(tmpl, syms) {
    if (tmpl.tag === 'symbol')
        syms.add(tmpl.value);
    else if (tmpl.tag === 'list') {
        for (const e of tmpl.value)
            collectAllSymbols(e, syms);
    }
}
function expandMacro(transformer, inputExpr, useSiteEnv) {
    const inputItems = inputExpr.value;
    for (const rule of transformer.rules) {
        const patItems = rule.pattern.value;
        const bindings = new Map();
        // Match arguments (skip first element = macro name in both pattern and input)
        const patArgs = { tag: 'list', value: patItems.slice(1) };
        const inpArgs = { tag: 'list', value: inputItems.slice(1) };
        if (!matchPattern(patArgs, inpArgs, transformer.literals, bindings))
            continue;
        // Collect pattern variable names
        const patVars = new Set();
        collectPatternVars(patArgs, transformer.literals, patVars);
        // Collect all symbols in template
        const tmplSyms = new Set();
        collectAllSymbols(rule.template, tmplSyms);
        // Build rename map for hygienic expansion
        const renames = new Map();
        for (const sym of tmplSyms) {
            if (patVars.has(sym))
                continue;
            if (sym === '...')
                continue;
            if (SPECIAL_FORMS.has(sym))
                continue;
            const renamed = gensym(sym);
            renames.set(sym, renamed);
            // Inject definition-site binding if available
            try {
                const defVal = transformer.defEnv.get(sym);
                useSiteEnv.define(renamed, defVal);
            }
            catch (_) {
                // Not bound in def env — that's fine, it's a macro-introduced name
            }
        }
        return expandTemplate(rule.template, bindings, renames);
    }
    return null;
}
// ── Global Environment ─────────────────────────────────────────────
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
    // apply is handled specially in applyCPS — this is just a placeholder so it's a first-class value
    defBuiltin('apply', (_args) => { throw new EvalError('apply: internal error — should be handled by applyCPS'); });
    // call/cc is handled specially in applyCPS
    defBuiltin('call/cc', (_args) => { throw new EvalError('call/cc: internal error'); });
    env.define('call-with-current-continuation', env.get('call/cc'));
    defBuiltin('number?', (args) => ({ tag: 'boolean', value: args[0].tag === 'number' }));
    defBuiltin('string?', (args) => ({ tag: 'boolean', value: args[0].tag === 'string' }));
    defBuiltin('boolean?', (args) => ({ tag: 'boolean', value: args[0].tag === 'boolean' }));
    defBuiltin('pair?', (args) => ({ tag: 'boolean', value: args[0].tag === 'pair' }));
    defBuiltin('symbol?', (args) => ({ tag: 'boolean', value: args[0].tag === 'symbol' }));
    defBuiltin('char?', (args) => ({ tag: 'boolean', value: args[0].tag === 'char' }));
    // Display / Write / Newline
    defBuiltin('display', (args) => {
        outputBuffer += displayVal(args[0]);
        return VOID;
    });
    defBuiltin('write', (args) => {
        outputBuffer += writeVal(args[0]);
        return VOID;
    });
    defBuiltin('newline', (_args) => {
        outputBuffer += '\n';
        return VOID;
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
    // eq? / eqv? / equal?
    function schemeEq(a, b) {
        if (a.tag !== b.tag)
            return false;
        if (a.tag === 'nil')
            return true;
        if (a.tag === 'boolean' && b.tag === 'boolean')
            return a.value === b.value;
        if (a.tag === 'number' && b.tag === 'number')
            return a.value === b.value;
        if (a.tag === 'symbol' && b.tag === 'symbol')
            return a.value === b.value;
        if (a.tag === 'char' && b.tag === 'char')
            return a.value === b.value;
        if (a.tag === 'string' && b.tag === 'string')
            return a.value === b.value;
        return a === b;
    }
    function schemeEqual(a, b) {
        if (a.tag === 'pair' && b.tag === 'pair') {
            return schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
        }
        return schemeEq(a, b);
    }
    defBuiltin('eq?', (args) => ({ tag: 'boolean', value: schemeEq(args[0], args[1]) }));
    defBuiltin('eqv?', (args) => ({ tag: 'boolean', value: schemeEq(args[0], args[1]) }));
    defBuiltin('equal?', (args) => ({ tag: 'boolean', value: schemeEqual(args[0], args[1]) }));
    // map (supports multiple lists)
    defBuiltin('map', (args) => {
        const proc = args[0];
        const lists = args.slice(1).map(pairsToArray);
        const len = lists[0].length;
        const result = [];
        for (let i = 0; i < len; i++) {
            const callArgs = lists.map(l => l[i]);
            // Synchronous apply for builtins/lambdas
            const r = runTrampoline(applyCPS(proc, callArgs, (v) => done(v)));
            result.push(r);
        }
        return listToPairs(result);
    });
    // L13: Numeric utilities
    defBuiltin('abs', (args) => ({ tag: 'number', value: Math.abs(expectNumber(args[0], 'abs')) }));
    defBuiltin('modulo', (args) => {
        const a = expectNumber(args[0], 'modulo');
        const b = expectNumber(args[1], 'modulo');
        return { tag: 'number', value: ((a % b) + b) % b };
    });
    defBuiltin('remainder', (args) => {
        const a = expectNumber(args[0], 'remainder');
        const b = expectNumber(args[1], 'remainder');
        return { tag: 'number', value: a % b };
    });
    defBuiltin('quotient', (args) => {
        const a = expectNumber(args[0], 'quotient');
        const b = expectNumber(args[1], 'quotient');
        return { tag: 'number', value: Math.trunc(a / b) };
    });
    defBuiltin('min', (args) => {
        let m = expectNumber(args[0], 'min');
        for (let i = 1; i < args.length; i++)
            m = Math.min(m, expectNumber(args[i], 'min'));
        return { tag: 'number', value: m };
    });
    defBuiltin('max', (args) => {
        let m = expectNumber(args[0], 'max');
        for (let i = 1; i < args.length; i++)
            m = Math.max(m, expectNumber(args[i], 'max'));
        return { tag: 'number', value: m };
    });
    defBuiltin('expt', (args) => {
        const base = expectNumber(args[0], 'expt');
        const exp = expectNumber(args[1], 'expt');
        return { tag: 'number', value: Math.pow(base, exp) };
    });
    defBuiltin('zero?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'zero?') === 0 }));
    defBuiltin('positive?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'positive?') > 0 }));
    defBuiltin('negative?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'negative?') < 0 }));
    defBuiltin('odd?', (args) => ({ tag: 'boolean', value: Math.abs(expectNumber(args[0], 'odd?')) % 2 === 1 }));
    defBuiltin('even?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'even?') % 2 === 0 }));
    // L13: List utilities
    defBuiltin('list-ref', (args) => {
        let cur = args[0];
        let idx = expectNumber(args[1], 'list-ref');
        while (idx > 0 && cur.tag === 'pair') {
            cur = cur.cdr;
            idx--;
        }
        if (cur.tag !== 'pair')
            throw new EvalError('list-ref: index out of range');
        return cur.car;
    });
    defBuiltin('list-tail', (args) => {
        let cur = args[0];
        let idx = expectNumber(args[1], 'list-tail');
        while (idx > 0 && cur.tag === 'pair') {
            cur = cur.cdr;
            idx--;
        }
        if (idx > 0)
            throw new EvalError('list-tail: index out of range');
        return cur;
    });
    defBuiltin('list?', (args) => {
        let cur = args[0];
        while (cur.tag === 'pair')
            cur = cur.cdr;
        return { tag: 'boolean', value: cur.tag === 'nil' };
    });
    defBuiltin('assoc', (args) => {
        const key = args[0];
        let cur = args[1];
        while (cur.tag === 'pair') {
            if (cur.car.tag === 'pair' && schemeEqual(cur.car.car, key))
                return cur.car;
            cur = cur.cdr;
        }
        return { tag: 'boolean', value: false };
    });
    // L13: Character utilities
    defBuiltin('char-alphabetic?', (args) => {
        if (args[0].tag !== 'char')
            throw new EvalError('char-alphabetic?: expected char');
        return { tag: 'boolean', value: /^[a-zA-Z]$/.test(args[0].value) };
    });
    defBuiltin('char-numeric?', (args) => {
        if (args[0].tag !== 'char')
            throw new EvalError('char-numeric?: expected char');
        return { tag: 'boolean', value: /^[0-9]$/.test(args[0].value) };
    });
    defBuiltin('char-upcase', (args) => {
        if (args[0].tag !== 'char')
            throw new EvalError('char-upcase: expected char');
        return { tag: 'char', value: args[0].value.toUpperCase() };
    });
    defBuiltin('char-downcase', (args) => {
        if (args[0].tag !== 'char')
            throw new EvalError('char-downcase: expected char');
        return { tag: 'char', value: args[0].value.toLowerCase() };
    });
    defBuiltin('char=?', (args) => {
        if (args[0].tag !== 'char' || args[1].tag !== 'char')
            throw new EvalError('char=?: expected char');
        return { tag: 'boolean', value: args[0].value === args[1].value };
    });
    defBuiltin('char<?', (args) => {
        if (args[0].tag !== 'char' || args[1].tag !== 'char')
            throw new EvalError('char<?: expected char');
        return { tag: 'boolean', value: args[0].value < args[1].value };
    });
    // L13: String comparison utilities
    defBuiltin('string=?', (args) => {
        if (args[0].tag !== 'string' || args[1].tag !== 'string')
            throw new EvalError('string=?: expected string');
        return { tag: 'boolean', value: args[0].value === args[1].value };
    });
    defBuiltin('string<?', (args) => {
        if (args[0].tag !== 'string' || args[1].tag !== 'string')
            throw new EvalError('string<?: expected string');
        return { tag: 'boolean', value: args[0].value < args[1].value };
    });
    defBuiltin('string-ci=?', (args) => {
        if (args[0].tag !== 'string' || args[1].tag !== 'string')
            throw new EvalError('string-ci=?: expected string');
        return { tag: 'boolean', value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
    });
    defBuiltin('string-upcase', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string-upcase: expected string');
        return { tag: 'string', value: args[0].value.toUpperCase() };
    });
    defBuiltin('string-downcase', (args) => {
        if (args[0].tag !== 'string')
            throw new EvalError('string-downcase: expected string');
        return { tag: 'string', value: args[0].value.toLowerCase() };
    });
    return env;
}
// ── CPS Evaluator ──────────────────────────────────────────────────
// Evaluate arguments right-to-left. R7RS does not specify evaluation order;
// right-to-left ensures that continuations captured by call/cc in later
// argument positions will re-evaluate earlier arguments when re-invoked.
function evaluateArgsCPS(exprs, startIdx, env, _unused, k) {
    if (startIdx >= exprs.length)
        return k([]);
    // First evaluate the rest (right), then evaluate this arg (left)
    return evaluateArgsCPS(exprs, startIdx + 1, env, [], (restVals) => {
        return evaluateCPS(exprs[startIdx], env, (val) => {
            return k([val, ...restVals]);
        });
    });
}
function evaluateSeqCPS(exprs, idx, env, k) {
    if (idx >= exprs.length)
        return callK(k, VOID);
    if (idx === exprs.length - 1)
        return evaluateCPS(exprs[idx], env, k); // tail position
    return evaluateCPS(exprs[idx], env, (_) => evaluateSeqCPS(exprs, idx + 1, env, k));
}
function evaluateAndCPS(items, idx, env, k) {
    if (idx === items.length - 1)
        return evaluateCPS(items[idx], env, k);
    return evaluateCPS(items[idx], env, (val) => {
        if (!isTruthy(val))
            return callK(k, val);
        return evaluateAndCPS(items, idx + 1, env, k);
    });
}
function evaluateOrCPS(items, idx, env, k) {
    if (idx === items.length - 1)
        return evaluateCPS(items[idx], env, k);
    return evaluateCPS(items[idx], env, (val) => {
        if (isTruthy(val))
            return callK(k, val);
        return evaluateOrCPS(items, idx + 1, env, k);
    });
}
function evaluateCondCPS(clauses, idx, env, k) {
    if (idx >= clauses.length)
        return callK(k, VOID);
    const clause = clauses[idx].value;
    if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
        return evaluateSeqCPS(clause, 1, env, k);
    }
    return evaluateCPS(clause[0], env, (test) => {
        if (isTruthy(test)) {
            if (clause.length === 1)
                return callK(k, test);
            return evaluateSeqCPS(clause, 1, env, k);
        }
        return evaluateCondCPS(clauses, idx + 1, env, k);
    });
}
function evaluateLetBindingsCPS(bindings, idx, outerEnv, letEnv, body, k) {
    if (idx >= bindings.length)
        return evaluateSeqCPS(body, 0, letEnv, k);
    const bv = bindings[idx].value;
    const name = bv[0].value;
    return evaluateCPS(bv[1], outerEnv, (val) => {
        letEnv.define(name, val);
        return evaluateLetBindingsCPS(bindings, idx + 1, outerEnv, letEnv, body, k);
    });
}
function applyCPS(proc, args, k, pos) {
    if (proc.tag === 'builtin') {
        // call/cc: capture current continuation
        if (proc.name === 'call/cc') {
            const contVal = { tag: 'continuation', k };
            return applyCPS(args[0], [contVal], k);
        }
        // apply: restructure args and delegate
        if (proc.name === 'apply') {
            const actualProc = args[0];
            const lastArg = args[args.length - 1];
            const prefixArgs = args.slice(1, args.length - 1);
            const tailArgs = pairsToArray(lastArg);
            return applyCPS(actualProc, [...prefixArgs, ...tailArgs], k);
        }
        try {
            const result = proc.fn(args);
            return callK(k, result);
        }
        catch (e) {
            if (e instanceof EvalError && pos && !e.message.match(/^\d+:/)) {
                throw new EvalError(`${posStr(pos)}${e.message}`);
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
        return evaluateSeqCPS(proc.body, 0, callEnv, k);
    }
    if (proc.tag === 'continuation') {
        const val = args[0] || VOID;
        return callK(proc.k, val);
    }
    throw new EvalError(`${posStr(pos)}not a procedure`);
}
function evaluateCPS(expr, env, k) {
    if (expr.tag === 'symbol') {
        try {
            const val = env.get(expr.value);
            return callK(k, val);
        }
        catch (e) {
            if (e instanceof EvalError && expr.pos) {
                throw new EvalError(`${posStr(expr.pos)}${e.message}`);
            }
            throw e;
        }
    }
    if (expr.tag !== 'list') {
        return callK(k, expr); // self-evaluating
    }
    const items = expr.value;
    if (items.length === 0)
        throw new EvalError(`${posStr(expr.pos)}empty application`);
    const head = items[0];
    if (head.tag === 'symbol') {
        const op = head.value;
        if (op === 'quote') {
            const quoted = astToPairs(items[1]);
            return callK(k, quoted);
        }
        if (op === 'if') {
            if (items.length < 3)
                throw new EvalError(`${posStr(expr.pos)}if: bad syntax`);
            return evaluateCPS(items[1], env, (cond) => {
                if (isTruthy(cond)) {
                    return evaluateCPS(items[2], env, k);
                }
                else if (items.length > 3) {
                    return evaluateCPS(items[3], env, k);
                }
                return callK(k, VOID);
            });
        }
        if (op === 'define') {
            if (items.length < 3)
                throw new EvalError(`${posStr(expr.pos)}define: bad syntax`);
            if (items[1].tag === 'list') {
                const nameAndParams = items[1].value;
                const name = nameAndParams[0].value;
                const { params, rest } = parseDotParams(nameAndParams.slice(1));
                const body = items.slice(2);
                env.define(name, { tag: 'lambda', params, rest, body, env });
                return callK(k, VOID);
            }
            const name = items[1].value;
            return evaluateCPS(items[2], env, (val) => {
                env.define(name, val);
                return callK(k, VOID);
            });
        }
        if (op === 'lambda') {
            const paramList = items[1];
            if (paramList.tag === 'symbol') {
                const lam = { tag: 'lambda', params: [], rest: paramList.value, body: items.slice(2), env };
                return callK(k, lam);
            }
            const { params, rest } = parseDotParams(paramList.value);
            const lam = { tag: 'lambda', params, rest, body: items.slice(2), env };
            return callK(k, lam);
        }
        if (op === 'and') {
            if (items.length === 1)
                return callK(k, { tag: 'boolean', value: true });
            return evaluateAndCPS(items, 1, env, k);
        }
        if (op === 'or') {
            if (items.length === 1)
                return callK(k, { tag: 'boolean', value: false });
            return evaluateOrCPS(items, 1, env, k);
        }
        if (op === 'let') {
            if (items[1].tag === 'symbol') {
                // Named let: (let name ((var val) ...) body...)
                const name = items[1].value;
                const bindings = items[2].value;
                const body = items.slice(3);
                const params = [];
                const initExprs = [];
                for (const b of bindings) {
                    const bv = b.value;
                    params.push(bv[0].value);
                    initExprs.push(bv[1]);
                }
                const letEnv = new Env(env);
                const lambda = { tag: 'lambda', params, body, env: letEnv };
                letEnv.define(name, lambda);
                return evaluateArgsCPS(initExprs, 0, env, [], (argVals) => {
                    const callEnv = new Env(letEnv);
                    for (let i = 0; i < params.length; i++) {
                        callEnv.define(params[i], argVals[i]);
                    }
                    return evaluateSeqCPS(body, 0, callEnv, k);
                });
            }
            // Regular let: (let ((var val) ...) body...)
            const bindings = items[1].value;
            const body = items.slice(2);
            const letEnv = new Env(env);
            return evaluateLetBindingsCPS(bindings, 0, env, letEnv, body, k);
        }
        if (op === 'begin') {
            if (items.length <= 1)
                return callK(k, VOID);
            return evaluateSeqCPS(items, 1, env, k);
        }
        if (op === 'set!') {
            const varExpr = items[1];
            if (varExpr.tag !== 'symbol')
                throw new EvalError(`${posStr(expr.pos)}set!: bad syntax`);
            return evaluateCPS(items[2], env, (val) => {
                env.set(varExpr.value, val);
                return callK(k, VOID);
            });
        }
        if (op === 'string-set!') {
            const varExpr = items[1];
            if (varExpr.tag !== 'symbol')
                throw new EvalError(`${posStr(expr.pos)}string-set!: first argument must be a variable`);
            const str = env.get(varExpr.value);
            if (str.tag !== 'string')
                throw new EvalError(`${posStr(expr.pos)}string-set!: expected string`);
            return evaluateCPS(items[2], env, (idxVal) => {
                const idx = expectNumber(idxVal, 'string-set!');
                return evaluateCPS(items[3], env, (chVal) => {
                    if (chVal.tag !== 'char')
                        throw new EvalError(`${posStr(expr.pos)}string-set!: expected char`);
                    const newStr = str.value.substring(0, idx) + chVal.value + str.value.substring(idx + 1);
                    env.set(varExpr.value, { tag: 'string', value: newStr });
                    return callK(k, VOID);
                });
            });
        }
        if (op === 'cond') {
            return evaluateCondCPS(items, 1, env, k);
        }
        if (op === 'define-syntax') {
            const name = items[1].value;
            const srExpr = items[2];
            const srItems = srExpr.value;
            // (syntax-rules (literal ...) (pattern template) ...)
            const literals = srItems[1].value.map(l => l.value);
            const rules = [];
            for (let i = 2; i < srItems.length; i++) {
                const r = srItems[i].value;
                rules.push({ pattern: r[0], template: r[1] });
            }
            env.define(name, { tag: 'syntax', literals, rules, defEnv: env });
            return callK(k, VOID);
        }
        // Check for macro application
        try {
            const headVal = env.get(op);
            if (headVal.tag === 'syntax') {
                const expanded = expandMacro(headVal, expr, env);
                if (expanded)
                    return evaluateCPS(expanded, env, k);
            }
        }
        catch (_) { /* not bound or not a macro */ }
    }
    // Function application: evaluate head, then args, then apply
    return evaluateCPS(head, env, (proc) => {
        return evaluateArgsCPS(items, 1, env, [], (args) => {
            return applyCPS(proc, args, k, expr.pos);
        });
    });
}
// ── Output buffer ──────────────────────────────────────────────────
let outputBuffer = '';
// ── Display / Write formatting ─────────────────────────────────────
function displayVal(val) {
    switch (val.tag) {
        case 'string': return val.value;
        default: return writeVal(val);
    }
}
function writeVal(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'char': {
            if (val.value === ' ')
                return '#\\space';
            if (val.value === '\n')
                return '#\\newline';
            if (val.value === '\t')
                return '#\\tab';
            return `#\\${val.value}`;
        }
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
        case 'continuation': return '#<continuation>';
        case 'void': return '';
        case 'syntax': return '#<syntax>';
    }
}
const display = writeVal;
// ── Public API ─────────────────────────────────────────────────────
function evaluateProgram(exprs, env) {
    // Evaluate all expressions in sequence using CPS, then run the trampoline
    const topK = (val) => done(val);
    const result = evaluateSeqCPS(exprs, 0, env, topK);
    return runTrampoline(result);
}
export function evalStr(input) {
    const exprs = parse(input);
    if (exprs.length === 0)
        throw new EvalError('empty input');
    const env = makeGlobalEnv();
    const result = evaluateProgram(exprs, env);
    return display(result);
}
export function evalStrWithOutput(input) {
    const exprs = parse(input);
    if (exprs.length === 0)
        throw new EvalError('empty input');
    outputBuffer = '';
    const env = makeGlobalEnv();
    const result = evaluateProgram(exprs, env);
    return { result: display(result), output: outputBuffer };
}
