import { EvalError } from './evalError.js';
let windStack = [];
let exceptionHandlers = [];
const NIL = { tag: 'nil' };
function strContent(v) {
    return v.chars ? v.chars.join('') : v.value;
}
function listToConsPairs(lst) {
    if (lst.tag === 'pair') {
        return { tag: 'pair', car: listToConsPairs(lst.car), cdr: listToConsPairs(lst.cdr) };
    }
    if (lst.tag !== 'list')
        return lst;
    let result = NIL;
    for (let i = lst.value.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: listToConsPairs(lst.value[i]), cdr: result };
    }
    return result;
}
// --- Rational helpers ---
function gcd(a, b) {
    a = Math.abs(a);
    b = Math.abs(b);
    while (b) {
        [a, b] = [b, a % b];
    }
    return a;
}
function makeRational(num, den) {
    if (den === 0)
        throw new EvalError('division by zero');
    if (den < 0) {
        num = -num;
        den = -den;
    }
    const g = gcd(Math.abs(num), den);
    num /= g;
    den /= g;
    if (den === 1)
        return { tag: 'number', value: num };
    return { tag: 'rational', num, den };
}
function isExact(v) {
    if (v.tag === 'rational')
        return true;
    if (v.tag === 'number')
        return v.exact !== false && Number.isInteger(v.value);
    return false;
}
function toFloat(v) {
    if (v.tag === 'number')
        return v.value;
    if (v.tag === 'rational')
        return v.num / v.den;
    throw new EvalError('expected number');
}
function toRational(v) {
    if (v.tag === 'rational')
        return { num: v.num, den: v.den };
    if (v.tag === 'number')
        return { num: v.value, den: 1 };
    throw new EvalError('expected number');
}
function isNumeric(v) {
    return v.tag === 'number' || v.tag === 'rational';
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
        // skip whitespace
        if (/\s/.test(ch)) {
            advance();
            continue;
        }
        // skip line comments
        if (ch === ';') {
            while (i < input.length && input[i] !== '\n')
                advance();
            continue;
        }
        const tokLine = line, tokCol = col;
        if (ch === '(') {
            tokens.push({ type: 'lparen', value: '(', line: tokLine, col: tokCol });
            advance();
            continue;
        }
        if (ch === ')') {
            tokens.push({ type: 'rparen', value: ')', line: tokLine, col: tokCol });
            advance();
            continue;
        }
        if (ch === '\'') {
            tokens.push({ type: 'quote', value: '\'', line: tokLine, col: tokCol });
            advance();
            continue;
        }
        if (ch === '`') {
            tokens.push({ type: 'quasiquote', value: '`', line: tokLine, col: tokCol });
            advance();
            continue;
        }
        if (ch === ',') {
            advance();
            if (i < input.length && input[i] === '@') {
                tokens.push({ type: 'unquote-splicing', value: ',@', line: tokLine, col: tokCol });
                advance();
            }
            else {
                tokens.push({ type: 'unquote', value: ',', line: tokLine, col: tokCol });
            }
            continue;
        }
        if (ch === '"') {
            let s = '';
            advance(); // skip opening quote
            while (i < input.length && input[i] !== '"') {
                if (input[i] === '\\') {
                    advance();
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
                advance();
            }
            if (i < input.length)
                advance(); // skip closing quote
            tokens.push({ type: 'string', value: s, line: tokLine, col: tokCol });
            continue;
        }
        // #' (syntax quote)
        if (ch === '#' && i + 1 < input.length && input[i + 1] === '\'') {
            tokens.push({ type: 'syntax-quote', value: "#'", line: tokLine, col: tokCol });
            advance();
            advance();
            continue;
        }
        // atom
        let atom = '';
        while (i < input.length && !/[\s()";]/.test(input[i])) {
            atom += input[i];
            advance();
        }
        if (atom === '.') {
            tokens.push({ type: 'dot', value: '.', line: tokLine, col: tokCol });
        }
        else {
            tokens.push({ type: 'atom', value: atom, line: tokLine, col: tokCol });
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
        const p = { line: tok.line, col: tok.col };
        if (tok.type === 'lparen') {
            pos++; // skip (
            const elements = [];
            while (pos < tokens.length && tokens[pos].type !== 'rparen') {
                if (tokens[pos].type === 'dot') {
                    pos++; // skip dot
                    const cdr = parseExpr();
                    if (pos >= tokens.length || tokens[pos].type !== 'rparen')
                        throw new EvalError('expected ) after dot expression');
                    pos++; // skip )
                    let result = cdr;
                    for (let i = elements.length - 1; i >= 0; i--) {
                        result = { tag: 'pair', car: elements[i], cdr: result };
                    }
                    if (result.tag === 'pair' || result.tag === 'symbol') {
                        result.pos = p;
                    }
                    return result;
                }
                elements.push(parseExpr());
            }
            if (pos >= tokens.length)
                throw new EvalError('missing closing parenthesis');
            pos++; // skip )
            return { tag: 'list', value: elements, pos: p };
        }
        if (tok.type === 'rparen') {
            throw new EvalError('unexpected )');
        }
        if (tok.type === 'quote') {
            pos++;
            const quoted = parseExpr();
            return { tag: 'list', value: [{ tag: 'symbol', value: 'quote', pos: p }, quoted], pos: p };
        }
        if (tok.type === 'syntax-quote') {
            pos++;
            const inner = parseExpr();
            return { tag: 'list', value: [{ tag: 'symbol', value: 'syntax', pos: p }, inner], pos: p };
        }
        if (tok.type === 'quasiquote') {
            pos++;
            const inner = parseExpr();
            return { tag: 'list', value: [{ tag: 'symbol', value: 'quasiquote', pos: p }, inner], pos: p };
        }
        if (tok.type === 'unquote') {
            pos++;
            const inner = parseExpr();
            return { tag: 'list', value: [{ tag: 'symbol', value: 'unquote', pos: p }, inner], pos: p };
        }
        if (tok.type === 'unquote-splicing') {
            pos++;
            const inner = parseExpr();
            return { tag: 'list', value: [{ tag: 'symbol', value: 'unquote-splicing', pos: p }, inner], pos: p };
        }
        if (tok.type === 'string') {
            pos++;
            return { tag: 'string', value: tok.value, pos: p };
        }
        // atom
        pos++;
        const v = tok.value;
        if (v === '#t')
            return { tag: 'boolean', value: true, pos: p };
        if (v === '#f')
            return { tag: 'boolean', value: false, pos: p };
        if (v.startsWith('#\\')) {
            const name = v.slice(2);
            let ch;
            if (name === 'space')
                ch = ' ';
            else if (name === 'newline')
                ch = '\n';
            else if (name === 'tab')
                ch = '\t';
            else if (name.length === 1)
                ch = name;
            else
                throw new EvalError(`unknown character name: ${name}`);
            return { tag: 'char', value: ch, pos: p };
        }
        if (/^-?\d+\/[1-9]\d*$/.test(v)) {
            const idx = v.indexOf('/');
            const r = makeRational(parseInt(v.substring(0, idx), 10), parseInt(v.substring(idx + 1), 10));
            r.pos = p;
            return r;
        }
        if (/^-?\d+$/.test(v))
            return { tag: 'number', value: parseInt(v, 10), pos: p };
        if (/^-?(\d+\.\d*|\d*\.\d+)$/.test(v))
            return { tag: 'number', value: parseFloat(v), exact: false, pos: p };
        return { tag: 'symbol', value: v, pos: p };
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
    update(name, value) {
        if (this.bindings.has(name)) {
            this.bindings.set(name, value);
            return;
        }
        if (this.parent) {
            this.parent.update(name, value);
            return;
        }
        throw new EvalError(`unbound variable: ${name}`);
    }
}
function displayVal(val, seen) {
    switch (val.tag) {
        case 'number': {
            if (val.exact === false && Number.isInteger(val.value))
                return `${val.value}.0`;
            return String(val.value);
        }
        case 'rational': return `${val.num}/${val.den}`;
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return strContent(val); // no quotes for display
        case 'symbol': return val.value;
        case 'char': return val.value;
        case 'nil': return '()';
        case 'pair': {
            if (!seen)
                seen = new Set();
            if (seen.has(val))
                return '(...)';
            seen.add(val);
            let parts = [];
            let cur = val;
            while (cur.tag === 'pair') {
                if (cur !== val && seen.has(cur)) {
                    parts.push('...');
                    break;
                }
                if (cur !== val)
                    seen.add(cur);
                parts.push(displayVal(cur.car, seen));
                cur = cur.cdr;
            }
            if (cur.tag === 'nil' || seen.has(cur))
                return `(${parts.join(' ')})`;
            return `(${parts.join(' ')} . ${displayVal(cur, seen)})`;
        }
        case 'void': return '';
        case 'procedure': return '#<procedure>';
        case 'macro': return '#<macro>';
        case 'syntax-transformer': return '#<syntax-transformer>';
        case 'list': return `(${val.value.map(v => displayVal(v, seen)).join(' ')})`;
        case 'record': return `#<record:${val.typeName}>`;
        case 'vector': return `#(${val.value.map(v => displayVal(v, seen)).join(' ')})`;
        case 'values': return val.values.map(v => displayVal(v, seen)).join('\n');
    }
}
function writeVal(val, seen) {
    switch (val.tag) {
        case 'string': return `"${strContent(val)}"`; // with quotes for write
        case 'char': {
            if (val.value === ' ')
                return '#\\space';
            if (val.value === '\n')
                return '#\\newline';
            if (val.value === '\t')
                return '#\\tab';
            return `#\\${val.value}`;
        }
        case 'vector': return `#(${val.value.map(v => writeVal(v, seen)).join(' ')})`;
        case 'list': return `(${val.value.map(v => writeVal(v, seen)).join(' ')})`;
        case 'pair': {
            if (!seen)
                seen = new Set();
            if (seen.has(val))
                return '(...)';
            seen.add(val);
            const parts = [];
            let cur = val;
            while (cur.tag === 'pair') {
                if (cur !== val && seen.has(cur)) {
                    parts.push('...');
                    break;
                }
                if (cur !== val)
                    seen.add(cur);
                parts.push(writeVal(cur.car, seen));
                cur = cur.cdr;
            }
            if (cur.tag === 'nil' || seen.has(cur))
                return `(${parts.join(' ')})`;
            return `(${parts.join(' ')} . ${writeVal(cur, seen)})`;
        }
        default: return displayVal(val, seen);
    }
}
function schemeEqv(a, b) {
    if (a.tag !== b.tag)
        return false;
    if (a.tag === 'nil')
        return true;
    if (a.tag === 'number' && b.tag === 'number')
        return a.value === b.value;
    if (a.tag === 'rational' && b.tag === 'rational')
        return a.num === b.num && a.den === b.den;
    if (a.tag === 'boolean' && b.tag === 'boolean')
        return a.value === b.value;
    if (a.tag === 'symbol' && b.tag === 'symbol')
        return a.value === b.value;
    if (a.tag === 'char' && b.tag === 'char')
        return a.value === b.value;
    return a === b;
}
function makeGlobalEnv(outputBuf) {
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
            for (const a of args)
                if (!isNumeric(a))
                    throw new EvalError('expected number');
            if (args.length === 0)
                return { tag: 'number', value: 0 };
            if (args.every(isExact)) {
                let rn = 0, rd = 1;
                for (const a of args) {
                    const r = toRational(a);
                    rn = rn * r.den + r.num * rd;
                    rd = rd * r.den;
                    const g = gcd(Math.abs(rn), rd);
                    rn /= g;
                    rd /= g;
                }
                return makeRational(rn, rd);
            }
            return { tag: 'number', value: args.reduce((s, a) => s + toFloat(a), 0), exact: false };
        } });
    env.set('*', { tag: 'procedure', value: (...args) => {
            for (const a of args)
                if (!isNumeric(a))
                    throw new EvalError('expected number');
            if (args.length === 0)
                return { tag: 'number', value: 1 };
            if (args.every(isExact)) {
                let rn = 1, rd = 1;
                for (const a of args) {
                    const r = toRational(a);
                    rn *= r.num;
                    rd *= r.den;
                    const g = gcd(Math.abs(rn), rd);
                    rn /= g;
                    rd /= g;
                }
                return makeRational(rn, rd);
            }
            return { tag: 'number', value: args.reduce((s, a) => s * toFloat(a), 1), exact: false };
        } });
    env.set('-', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                throw new EvalError('- requires at least one argument');
            for (const a of args)
                if (!isNumeric(a))
                    throw new EvalError('expected number');
            if (args.every(isExact)) {
                if (args.length === 1) {
                    const r = toRational(args[0]);
                    return makeRational(-r.num, r.den);
                }
                let { num: rn, den: rd } = toRational(args[0]);
                for (let i = 1; i < args.length; i++) {
                    const r = toRational(args[i]);
                    rn = rn * r.den - r.num * rd;
                    rd = rd * r.den;
                    const g = gcd(Math.abs(rn), rd);
                    rn /= g;
                    rd /= g;
                }
                return makeRational(rn, rd);
            }
            const nums = args.map(toFloat);
            if (nums.length === 1)
                return { tag: 'number', value: -nums[0], exact: false };
            return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]), exact: false };
        } });
    env.set('/', { tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('/ requires at least two arguments');
            for (const a of args)
                if (!isNumeric(a))
                    throw new EvalError('expected number');
            if (args.every(isExact)) {
                let { num: rn, den: rd } = toRational(args[0]);
                for (let i = 1; i < args.length; i++) {
                    const r = toRational(args[i]);
                    if (r.num === 0)
                        throw new EvalError('division by zero');
                    rn *= r.den;
                    rd *= r.num;
                    if (rd < 0) {
                        rn = -rn;
                        rd = -rd;
                    }
                    const g = gcd(Math.abs(rn), rd);
                    rn /= g;
                    rd /= g;
                }
                return makeRational(rn, rd);
            }
            const nums = args.map(toFloat);
            return { tag: 'number', value: nums.slice(1).reduce((a, b) => {
                    if (b === 0)
                        throw new EvalError('division by zero');
                    return a / b;
                }, nums[0]), exact: false };
        } });
    const cmpOp = (op) => ({ tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('comparison requires at least two arguments');
            const nums = args.map(a => { if (!isNumeric(a))
                throw new EvalError('expected number'); return toFloat(a); });
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
    env.set('set-car!', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('set-car! requires exactly 2 arguments');
            if (args[0].tag !== 'pair')
                throw new EvalError('set-car!: not a pair');
            args[0].car = args[1];
            return { tag: 'void' };
        } });
    env.set('set-cdr!', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('set-cdr! requires exactly 2 arguments');
            if (args[0].tag !== 'pair')
                throw new EvalError('set-cdr!: not a pair');
            args[0].cdr = args[1];
            return { tag: 'void' };
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
            let slow = args[0];
            let fast = args[0];
            while (fast.tag === 'pair') {
                fast = fast.cdr;
                count++;
                if (fast.tag !== 'pair')
                    break;
                fast = fast.cdr;
                count++;
                slow = slow.cdr;
                if (slow === fast)
                    throw new EvalError('length: circular list');
            }
            if (fast.tag !== 'nil')
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
            return { tag: 'boolean', value: isNumeric(args[0]) };
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
    env.set('procedure?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('procedure? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'procedure' };
        } });
    // I/O builtins (L05)
    env.set('display', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('display requires exactly 1 argument');
            if (outputBuf)
                outputBuf.push(displayVal(args[0]));
            return { tag: 'void' };
        } });
    env.set('write', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('write requires exactly 1 argument');
            if (outputBuf)
                outputBuf.push(writeVal(args[0]));
            return { tag: 'void' };
        } });
    env.set('newline', { tag: 'procedure', value: (...args) => {
            if (outputBuf)
                outputBuf.push('\n');
            return { tag: 'void' };
        } });
    // String builtins (L05)
    env.set('string-append', { tag: 'procedure', value: (...args) => {
            const strs = args.map(a => {
                if (a.tag !== 'string')
                    throw new EvalError('string-append: expected string');
                return strContent(a);
            });
            return { tag: 'string', value: strs.join('') };
        } });
    env.set('string-length', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string-length: expected string');
            return { tag: 'number', value: strContent(args[0]).length };
        } });
    env.set('substring', { tag: 'procedure', value: (...args) => {
            if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'number')
                throw new EvalError('substring: expected string, number, number');
            return { tag: 'string', value: strContent(args[0]).substring(args[1].value, args[2].value) };
        } });
    env.set('string->number', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string->number: expected string');
            const n = Number(strContent(args[0]));
            if (isNaN(n))
                return { tag: 'boolean', value: false };
            return { tag: 'number', value: n };
        } });
    env.set('number->string', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('number->string: expected number');
            return { tag: 'string', value: String(args[0].value) };
        } });
    env.set('symbol->string', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'symbol')
                throw new EvalError('symbol->string: expected symbol');
            return { tag: 'string', value: args[0].value };
        } });
    env.set('string->symbol', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string->symbol: expected string');
            return { tag: 'symbol', value: args[0].value };
        } });
    // L22: syntax-case support
    env.set('syntax->datum', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('syntax->datum: expected 1 argument');
            return args[0]; // In our representation, syntax objects are just values
        } });
    env.set('datum->syntax', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('datum->syntax: expected 2 arguments');
            // First arg is template-id (for lexical context), second is datum
            return args[1]; // In our representation, just return the datum
        } });
    env.set('string-ref', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'number')
                throw new EvalError('string-ref: expected string and number');
            const s = strContent(args[0]);
            const i = args[1].value;
            if (i < 0 || i >= s.length)
                throw new EvalError('string-ref: index out of range');
            return { tag: 'char', value: s[i] };
        } });
    // L06: Mutable strings
    env.set('string-copy', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string-copy: expected string');
            const s = strContent(args[0]);
            return { tag: 'string', value: '', chars: [...s] };
        } });
    env.set('string-set!', { tag: 'procedure', value: (...args) => {
            if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'char')
                throw new EvalError('string-set!: expected string, index, char');
            const str = args[0];
            if (!str.chars)
                throw new EvalError('string-set!: strings are immutable');
            const idx = args[1].value;
            if (idx < 0 || idx >= str.chars.length)
                throw new EvalError('string-set!: index out of range');
            str.chars[idx] = args[2].value;
            return { tag: 'void' };
        } });
    env.set('string->list', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string->list: expected string');
            const s = strContent(args[0]);
            let result = NIL;
            for (let i = s.length - 1; i >= 0; i--) {
                result = { tag: 'pair', car: { tag: 'char', value: s[i] }, cdr: result };
            }
            return result;
        } });
    env.set('list->string', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('list->string: expected 1 argument');
            let cur = args[0];
            const chars = [];
            while (cur.tag === 'list' ? cur.value.length > 0 : cur.tag === 'pair') {
                if (cur.tag === 'list') {
                    for (const item of cur.value) {
                        if (item.tag !== 'char')
                            throw new EvalError('list->string: expected list of characters');
                        chars.push(item.value);
                    }
                    break;
                }
                const p = cur;
                if (p.car.tag !== 'char')
                    throw new EvalError('list->string: expected list of characters');
                chars.push(p.car.value);
                cur = p.cdr;
            }
            return { tag: 'string', value: chars.join('') };
        } });
    env.set('char->integer', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'char')
                throw new EvalError('char->integer: expected char');
            return { tag: 'number', value: args[0].value.codePointAt(0) };
        } });
    env.set('integer->char', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('integer->char: expected number');
            return { tag: 'char', value: String.fromCodePoint(args[0].value) };
        } });
    env.set('char?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('char? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'char' };
        } });
    // eq? and equal?
    env.set('eq?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('eq? requires exactly 2 arguments');
            const [a, b] = args;
            if (a.tag !== b.tag)
                return { tag: 'boolean', value: false };
            if (a.tag === 'nil')
                return { tag: 'boolean', value: true };
            if (a.tag === 'void')
                return { tag: 'boolean', value: true };
            if (a.tag === 'number' && b.tag === 'number')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'rational' && b.tag === 'rational')
                return { tag: 'boolean', value: a.num === b.num && a.den === b.den };
            if (a.tag === 'boolean' && b.tag === 'boolean')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'symbol' && b.tag === 'symbol')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'char' && b.tag === 'char')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'string' && b.tag === 'string')
                return { tag: 'boolean', value: a === b };
            return { tag: 'boolean', value: a === b };
        } });
    const equalSeen = new Set();
    const schemeEqual = (a, b) => {
        if (isNumeric(a) && isNumeric(b))
            return toFloat(a) === toFloat(b);
        if (a.tag !== b.tag)
            return false;
        if (a.tag === 'nil')
            return true;
        if (a.tag === 'number' && b.tag === 'number')
            return a.value === b.value;
        if (a.tag === 'boolean' && b.tag === 'boolean')
            return a.value === b.value;
        if (a.tag === 'symbol' && b.tag === 'symbol')
            return a.value === b.value;
        if (a.tag === 'char' && b.tag === 'char')
            return a.value === b.value;
        if (a.tag === 'string' && b.tag === 'string')
            return strContent(a) === strContent(b);
        if (a.tag === 'pair' && b.tag === 'pair') {
            // Use object identity to detect cycles
            const key = `${a.__id || (a.__id = ++equalIdCounter)},${b.__id || (b.__id = ++equalIdCounter)}`;
            if (equalSeen.has(key))
                return true; // assume equal if revisited
            equalSeen.add(key);
            const result = schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
            equalSeen.delete(key);
            return result;
        }
        if (a.tag === 'vector' && b.tag === 'vector') {
            if (a.value.length !== b.value.length)
                return false;
            for (let i = 0; i < a.value.length; i++) {
                if (!schemeEqual(a.value[i], b.value[i]))
                    return false;
            }
            return true;
        }
        return a === b;
    };
    env.set('equal?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('equal? requires exactly 2 arguments');
            return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
        } });
    // eqv? (like eq? but compares numbers/chars by value)
    env.set('eqv?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('eqv? requires exactly 2 arguments');
            const [a, b] = args;
            if (a.tag !== b.tag)
                return { tag: 'boolean', value: false };
            if (a.tag === 'nil')
                return { tag: 'boolean', value: true };
            if (a.tag === 'number' && b.tag === 'number')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'rational' && b.tag === 'rational')
                return { tag: 'boolean', value: a.num === b.num && a.den === b.den };
            if (a.tag === 'boolean' && b.tag === 'boolean')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'symbol' && b.tag === 'symbol')
                return { tag: 'boolean', value: a.value === b.value };
            if (a.tag === 'char' && b.tag === 'char')
                return { tag: 'boolean', value: a.value === b.value };
            return { tag: 'boolean', value: a === b };
        } });
    // Vector builtins
    env.set('vector', { tag: 'procedure', value: (...args) => {
            return { tag: 'vector', value: [...args] };
        } });
    env.set('make-vector', { tag: 'procedure', value: (...args) => {
            if (args.length < 1 || args[0].tag !== 'number')
                throw new EvalError('make-vector: expected number');
            const size = args[0].value;
            const fill = args.length > 1 ? args[1] : { tag: 'number', value: 0 };
            return { tag: 'vector', value: Array(size).fill(null).map(() => fill) };
        } });
    env.set('vector-ref', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'vector' || args[1].tag !== 'number')
                throw new EvalError('vector-ref: expected vector and number');
            const idx = args[1].value;
            if (idx < 0 || idx >= args[0].value.length)
                throw new EvalError('vector-ref: index out of range');
            return args[0].value[idx];
        } });
    env.set('vector-set!', { tag: 'procedure', value: (...args) => {
            if (args.length !== 3 || args[0].tag !== 'vector' || args[1].tag !== 'number')
                throw new EvalError('vector-set!: expected vector, number, value');
            const idx = args[1].value;
            if (idx < 0 || idx >= args[0].value.length)
                throw new EvalError('vector-set!: index out of range');
            args[0].value[idx] = args[2];
            return { tag: 'void' };
        } });
    env.set('vector-length', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'vector')
                throw new EvalError('vector-length: expected vector');
            return { tag: 'number', value: args[0].value.length };
        } });
    env.set('vector?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('vector? requires exactly 1 argument');
            return { tag: 'boolean', value: args[0].tag === 'vector' };
        } });
    env.set('vector->list', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'vector')
                throw new EvalError('vector->list: expected vector');
            let result = NIL;
            for (let i = args[0].value.length - 1; i >= 0; i--) {
                result = { tag: 'pair', car: args[0].value[i], cdr: result };
            }
            return result;
        } });
    env.set('list->vector', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('list->vector: expected list');
            const elems = [];
            let cur = args[0];
            while (cur.tag === 'pair') {
                elems.push(cur.car);
                cur = cur.cdr;
            }
            return { tag: 'vector', value: elems };
        } });
    // map (supports multiple lists)
    env.set('map', { tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('map requires at least 2 arguments');
            const func = args[0];
            if (func.tag !== 'procedure')
                throw new EvalError('map: first argument must be a procedure');
            const lists = args.slice(1);
            const results = [];
            // Convert lists to arrays of pairs for iteration
            let cursors = lists;
            while (true) {
                // Check if any list is exhausted
                if (cursors.some(c => c.tag === 'nil'))
                    break;
                if (cursors.some(c => c.tag !== 'pair'))
                    throw new EvalError('map: expected proper list');
                const callArgs = cursors.map(c => c.car);
                results.push(func.value(...callArgs));
                cursors = cursors.map(c => c.cdr);
            }
            let result = NIL;
            for (let i = results.length - 1; i >= 0; i--) {
                result = { tag: 'pair', car: results[i], cdr: result };
            }
            return result;
        } });
    // for-each (like map but discards results)
    env.set('for-each', { tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('for-each requires at least 2 arguments');
            const func = args[0];
            if (func.tag !== 'procedure')
                throw new EvalError('for-each: first argument must be a procedure');
            const lists = args.slice(1);
            let cursors = lists;
            while (true) {
                if (cursors.some(c => c.tag === 'nil'))
                    break;
                if (cursors.some(c => c.tag !== 'pair'))
                    throw new EvalError('for-each: expected proper list');
                const callArgs = cursors.map(c => c.car);
                func.value(...callArgs);
                cursors = cursors.map(c => c.cdr);
            }
            return { tag: 'void' };
        } });
    // L09: Numeric utilities
    env.set('abs', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('abs: expected number');
            return { tag: 'number', value: Math.abs(args[0].value) };
        } });
    env.set('modulo', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
                throw new EvalError('modulo: expected two numbers');
            const [a, b] = [args[0].value, args[1].value];
            if (b === 0)
                throw new EvalError('modulo: division by zero');
            return { tag: 'number', value: ((a % b) + b) % b };
        } });
    env.set('remainder', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
                throw new EvalError('remainder: expected two numbers');
            const [a, b] = [args[0].value, args[1].value];
            if (b === 0)
                throw new EvalError('remainder: division by zero');
            return { tag: 'number', value: a % b };
        } });
    env.set('quotient', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
                throw new EvalError('quotient: expected two numbers');
            const [a, b] = [args[0].value, args[1].value];
            if (b === 0)
                throw new EvalError('quotient: division by zero');
            return { tag: 'number', value: Math.trunc(a / b) };
        } });
    env.set('min', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                throw new EvalError('min: requires at least one argument');
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('min: expected number'); return a.value; });
            return { tag: 'number', value: Math.min(...nums) };
        } });
    env.set('max', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                throw new EvalError('max: requires at least one argument');
            const nums = args.map(a => { if (a.tag !== 'number')
                throw new EvalError('max: expected number'); return a.value; });
            return { tag: 'number', value: Math.max(...nums) };
        } });
    env.set('expt', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
                throw new EvalError('expt: expected two numbers');
            return { tag: 'number', value: Math.pow(args[0].value, args[1].value) };
        } });
    // L09: Numeric predicates
    env.set('zero?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('zero?: expected number');
            return { tag: 'boolean', value: args[0].value === 0 };
        } });
    env.set('positive?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('positive?: expected number');
            return { tag: 'boolean', value: args[0].value > 0 };
        } });
    env.set('negative?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('negative?: expected number');
            return { tag: 'boolean', value: args[0].value < 0 };
        } });
    env.set('odd?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('odd?: expected number');
            return { tag: 'boolean', value: args[0].value % 2 !== 0 };
        } });
    env.set('even?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'number')
                throw new EvalError('even?: expected number');
            return { tag: 'boolean', value: args[0].value % 2 === 0 };
        } });
    // L11: Exact arithmetic & rationals
    env.set('exact?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('exact? requires exactly 1 argument');
            return { tag: 'boolean', value: isExact(args[0]) };
        } });
    env.set('inexact?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('inexact? requires exactly 1 argument');
            return { tag: 'boolean', value: isNumeric(args[0]) && !isExact(args[0]) };
        } });
    env.set('exact->inexact', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || !isNumeric(args[0]))
                throw new EvalError('exact->inexact: expected number');
            return { tag: 'number', value: toFloat(args[0]), exact: false };
        } });
    env.set('inexact->exact', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || !isNumeric(args[0]))
                throw new EvalError('inexact->exact: expected number');
            if (isExact(args[0]))
                return args[0];
            const x = toFloat(args[0]);
            if (Number.isInteger(x))
                return { tag: 'number', value: x };
            const str = x.toString();
            const decIdx = str.indexOf('.');
            if (decIdx === -1)
                return { tag: 'number', value: x };
            const decimals = str.length - decIdx - 1;
            const den = Math.pow(10, decimals);
            const num = Math.round(x * den);
            const g = gcd(Math.abs(num), den);
            return makeRational(num / g, den / g);
        } });
    env.set('numerator', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || !isNumeric(args[0]))
                throw new EvalError('numerator: expected number');
            const r = toRational(args[0]);
            return { tag: 'number', value: r.num };
        } });
    env.set('denominator', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || !isNumeric(args[0]))
                throw new EvalError('denominator: expected number');
            const r = toRational(args[0]);
            return { tag: 'number', value: r.den };
        } });
    env.set('integer?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('integer? requires exactly 1 argument');
            if (args[0].tag === 'number')
                return { tag: 'boolean', value: Number.isInteger(args[0].value) };
            if (args[0].tag === 'rational')
                return { tag: 'boolean', value: false };
            return { tag: 'boolean', value: false };
        } });
    env.set('rational?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('rational? requires exactly 1 argument');
            return { tag: 'boolean', value: isNumeric(args[0]) && isExact(args[0]) };
        } });
    // L09: List utilities
    env.set('list-ref', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[1].tag !== 'number')
                throw new EvalError('list-ref: expected list and number');
            let cur = args[0];
            let idx = args[1].value;
            while (idx > 0 && cur.tag === 'pair') {
                cur = cur.cdr;
                idx--;
            }
            if (cur.tag !== 'pair')
                throw new EvalError('list-ref: index out of range');
            return cur.car;
        } });
    env.set('list-tail', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[1].tag !== 'number')
                throw new EvalError('list-tail: expected list and number');
            let cur = args[0];
            let idx = args[1].value;
            while (idx > 0) {
                if (cur.tag !== 'pair')
                    throw new EvalError('list-tail: index out of range');
                cur = cur.cdr;
                idx--;
            }
            return cur;
        } });
    env.set('list?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('list? requires exactly 1 argument');
            let slow = args[0];
            let fast = args[0];
            while (fast.tag === 'pair') {
                fast = fast.cdr;
                if (fast.tag !== 'pair')
                    break;
                fast = fast.cdr;
                slow = slow.cdr;
                if (slow === fast)
                    return { tag: 'boolean', value: false }; // cycle
            }
            return { tag: 'boolean', value: fast.tag === 'nil' };
        } });
    // L09: assoc
    env.set('assoc', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('assoc requires exactly 2 arguments');
            const key = args[0];
            let cur = args[1];
            while (cur.tag === 'pair') {
                const entry = cur.car;
                if (entry.tag === 'pair' && schemeEqual(entry.car, key))
                    return entry;
                cur = cur.cdr;
            }
            return { tag: 'boolean', value: false };
        } });
    // L09: Character utilities
    env.set('char-alphabetic?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'char')
                throw new EvalError('char-alphabetic?: expected char');
            return { tag: 'boolean', value: /^[a-zA-Z]$/.test(args[0].value) };
        } });
    env.set('char-numeric?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'char')
                throw new EvalError('char-numeric?: expected char');
            return { tag: 'boolean', value: /^[0-9]$/.test(args[0].value) };
        } });
    env.set('char-upcase', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'char')
                throw new EvalError('char-upcase: expected char');
            return { tag: 'char', value: args[0].value.toUpperCase() };
        } });
    env.set('char-downcase', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'char')
                throw new EvalError('char-downcase: expected char');
            return { tag: 'char', value: args[0].value.toLowerCase() };
        } });
    env.set('char=?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char')
                throw new EvalError('char=?: expected two chars');
            return { tag: 'boolean', value: args[0].value === args[1].value };
        } });
    env.set('char<?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char')
                throw new EvalError('char<?: expected two chars');
            return { tag: 'boolean', value: args[0].value < args[1].value };
        } });
    // L09: String comparison/conversion
    env.set('string=?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
                throw new EvalError('string=?: expected two strings');
            return { tag: 'boolean', value: strContent(args[0]) === strContent(args[1]) };
        } });
    env.set('string<?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
                throw new EvalError('string<?: expected two strings');
            return { tag: 'boolean', value: strContent(args[0]) < strContent(args[1]) };
        } });
    env.set('string-ci=?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
                throw new EvalError('string-ci=?: expected two strings');
            return { tag: 'boolean', value: strContent(args[0]).toLowerCase() === strContent(args[1]).toLowerCase() };
        } });
    env.set('string-upcase', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string-upcase: expected string');
            return { tag: 'string', value: strContent(args[0]).toUpperCase() };
        } });
    env.set('string-downcase', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || args[0].tag !== 'string')
                throw new EvalError('string-downcase: expected string');
            return { tag: 'string', value: strContent(args[0]).toLowerCase() };
        } });
    // L08: apply
    env.set('apply', { tag: 'procedure', value: (...args) => {
            if (args.length < 2)
                throw new EvalError('apply requires at least 2 arguments');
            const func = args[0];
            if (func.tag !== 'procedure')
                throw new EvalError('apply: first argument must be a procedure');
            // Last argument must be a list, prefix args come before it
            const lastArg = args[args.length - 1];
            const prefixArgs = args.slice(1, args.length - 1);
            // Convert last arg (pair list) to array
            const listArgs = [];
            let cur = lastArg;
            while (cur.tag === 'pair') {
                listArgs.push(cur.car);
                cur = cur.cdr;
            }
            if (cur.tag !== 'nil')
                throw new EvalError('apply: last argument must be a proper list');
            const allArgs = [...prefixArgs, ...listArgs];
            return func.value(...allArgs);
        } });
    // L18: call/cc
    const callccProc = { tag: 'procedure', value: (..._args) => { throw new EvalError('call/cc must be applied in CPS context'); }, _callcc: true };
    env.set('call/cc', callccProc);
    env.set('call-with-current-continuation', callccProc);
    // L19: dynamic-wind
    const dynamicWindProc = { tag: 'procedure', value: (..._args) => { throw new EvalError('dynamic-wind must be applied in CPS context'); }, _dynamicWind: true };
    env.set('dynamic-wind', dynamicWindProc);
    // L20: raise, with-exception-handler
    const raiseProc = { tag: 'procedure', value: (..._args) => { throw new EvalError('raise must be applied in CPS context'); }, _raise: true };
    env.set('raise', raiseProc);
    const wehProc = { tag: 'procedure', value: (..._args) => { throw new EvalError('with-exception-handler must be applied in CPS context'); }, _withExceptionHandler: true };
    env.set('with-exception-handler', wehProc);
    // L21: values, call-with-values
    const valuesProc = { tag: 'procedure', value: (...args) => {
            if (args.length === 1)
                return args[0];
            return { tag: 'values', values: args };
        }, _values: true };
    env.set('values', valuesProc);
    const cwvProc = { tag: 'procedure', value: (..._args) => { throw new EvalError('call-with-values must be applied in CPS context'); }, _callWithValues: true };
    env.set('call-with-values', cwvProc);
    // cxr helpers
    const cxr = (ops) => ({ tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError(`c${ops}r requires exactly 1 argument`);
            let cur = args[0];
            for (let i = ops.length - 1; i >= 0; i--) {
                if (cur.tag !== 'pair')
                    throw new EvalError(`c${ops}r: not a pair`);
                cur = ops[i] === 'a' ? cur.car : cur.cdr;
            }
            return cur;
        } });
    env.set('caar', cxr('aa'));
    env.set('cadr', cxr('ad'));
    env.set('cdar', cxr('da'));
    env.set('cddr', cxr('dd'));
    env.set('caaar', cxr('aaa'));
    env.set('caadr', cxr('aad'));
    env.set('caddr', cxr('add'));
    env.set('cdddr', cxr('ddd'));
    env.set('caddar', cxr('adda'));
    env.set('cadddr', cxr('addd'));
    // L17: error
    env.set('error', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                throw new EvalError('error');
            const parts = args.map(a => displayVal(a));
            throw new EvalError(parts.join(' '));
        } });
    // reverse
    env.set('reverse', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1)
                throw new EvalError('reverse requires exactly 1 argument');
            let result = NIL;
            let cur = args[0];
            while (cur.tag === 'pair') {
                result = { tag: 'pair', car: cur.car, cdr: result };
                cur = cur.cdr;
            }
            return result;
        } });
    // make-string
    env.set('make-string', { tag: 'procedure', value: (...args) => {
            if (args.length < 1 || args[0].tag !== 'number')
                throw new EvalError('make-string: expected number');
            const n = args[0].value;
            const ch = args.length > 1 && args[1].tag === 'char' ? args[1].value : '\0';
            return { tag: 'string', value: '', chars: Array(n).fill(ch) };
        } });
    // string (from chars)
    env.set('string', { tag: 'procedure', value: (...args) => {
            const chars = [];
            for (const a of args) {
                if (a.tag !== 'char')
                    throw new EvalError('string: expected char');
                chars.push(a.value);
            }
            return { tag: 'string', value: chars.join('') };
        } });
    // string comparisons
    env.set('string>?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
                throw new EvalError('string>?: expected two strings');
            return { tag: 'boolean', value: strContent(args[0]) > strContent(args[1]) };
        } });
    env.set('string<=?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
                throw new EvalError('string<=?: expected two strings');
            return { tag: 'boolean', value: strContent(args[0]) <= strContent(args[1]) };
        } });
    env.set('string>=?', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
                throw new EvalError('string>=?: expected two strings');
            return { tag: 'boolean', value: strContent(args[0]) >= strContent(args[1]) };
        } });
    // member (uses equal?)
    env.set('member', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('member requires exactly 2 arguments');
            let cur = args[1];
            while (cur.tag === 'pair') {
                if (schemeEqual(args[0], cur.car))
                    return cur;
                cur = cur.cdr;
            }
            return { tag: 'boolean', value: false };
        } });
    // assv (uses eqv?)
    env.set('assv', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('assv requires exactly 2 arguments');
            const key = args[0];
            let cur = args[1];
            while (cur.tag === 'pair') {
                const entry = cur.car;
                if (entry.tag === 'pair' && schemeEqv(entry.car, key))
                    return entry;
                cur = cur.cdr;
            }
            return { tag: 'boolean', value: false };
        } });
    // memq (uses eq?)
    env.set('memq', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('memq requires exactly 2 arguments');
            let cur = args[1];
            while (cur.tag === 'pair') {
                if (args[0] === cur.car || schemeEqv(args[0], cur.car) && (args[0].tag === 'symbol' || args[0].tag === 'boolean' || args[0].tag === 'char'))
                    return cur;
                if (args[0].tag === 'symbol' && cur.car.tag === 'symbol' && args[0].value === cur.car.value)
                    return cur;
                if (args[0].tag === 'boolean' && cur.car.tag === 'boolean' && args[0].value === cur.car.value)
                    return cur;
                if (args[0].tag === 'number' && cur.car.tag === 'number' && args[0].value === cur.car.value)
                    return cur;
                if (args[0].tag === 'char' && cur.car.tag === 'char' && args[0].value === cur.car.value)
                    return cur;
                cur = cur.cdr;
            }
            return { tag: 'boolean', value: false };
        } });
    // memv (uses eqv?)
    env.set('memv', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('memv requires exactly 2 arguments');
            let cur = args[1];
            while (cur.tag === 'pair') {
                if (schemeEqv(args[0], cur.car))
                    return cur;
                cur = cur.cdr;
            }
            return { tag: 'boolean', value: false };
        } });
    // assq (uses eq?)
    env.set('assq', { tag: 'procedure', value: (...args) => {
            if (args.length !== 2)
                throw new EvalError('assq requires exactly 2 arguments');
            const key = args[0];
            let cur = args[1];
            while (cur.tag === 'pair') {
                const entry = cur.car;
                if (entry.tag === 'pair') {
                    const k = entry.car;
                    if (key === k)
                        return entry;
                    if (key.tag === 'symbol' && k.tag === 'symbol' && key.value === k.value)
                        return entry;
                    if (key.tag === 'boolean' && k.tag === 'boolean' && key.value === k.value)
                        return entry;
                    if (key.tag === 'number' && k.tag === 'number' && key.value === k.value)
                        return entry;
                    if (key.tag === 'char' && k.tag === 'char' && key.value === k.value)
                        return entry;
                }
                cur = cur.cdr;
            }
            return { tag: 'boolean', value: false };
        } });
    // gcd (as builtin procedure)
    env.set('gcd', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                return { tag: 'number', value: 0 };
            let result = 0;
            for (const a of args) {
                if (a.tag !== 'number')
                    throw new EvalError('gcd: expected number');
                result = gcd(result, Math.abs(a.value));
            }
            return { tag: 'number', value: result };
        } });
    // lcm
    env.set('lcm', { tag: 'procedure', value: (...args) => {
            if (args.length === 0)
                return { tag: 'number', value: 1 };
            let result = 1;
            for (const a of args) {
                if (a.tag !== 'number')
                    throw new EvalError('lcm: expected number');
                const v = Math.abs(a.value);
                if (v === 0)
                    return { tag: 'number', value: 0 };
                result = (result / gcd(result, v)) * v;
            }
            return { tag: 'number', value: result };
        } });
    // truncate
    env.set('truncate', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || !isNumeric(args[0]))
                throw new EvalError('truncate: expected number');
            return { tag: 'number', value: Math.trunc(toFloat(args[0])) };
        } });
    // round
    env.set('round', { tag: 'procedure', value: (...args) => {
            if (args.length !== 1 || !isNumeric(args[0]))
                throw new EvalError('round: expected number');
            return { tag: 'number', value: Math.round(toFloat(args[0])) };
        } });
    return env;
}
// --- Evaluator ---
function isFalsy(val) {
    return val.tag === 'boolean' && val.value === false;
}
function posStr(p) {
    return p ? `${p.line}:${p.col}` : '?:?';
}
function errAt(msg, p) {
    return new EvalError(`${posStr(p)}: ${msg}`);
}
// --- Macro support (L10) ---
let gensymCounter = 0;
let equalIdCounter = 0;
const resolvedSymbols = new Map();
function gensym(base) {
    return `__gs_${base}_${gensymCounter++}`;
}
const SPECIAL_FORMS = new Set([
    'define', 'set!', 'if', 'quote', 'lambda', 'case-lambda', 'and', 'or', 'begin',
    'let', 'let*', 'letrec', 'letrec*', 'cond', 'case', 'do', 'define-syntax', 'syntax-rules', 'define-record-type',
    'syntax-case', 'syntax', 'with-syntax', 'guard',
]);
let syntaxCaseStack = [];
function collectPatternVarNames(pattern, literals) {
    if (pattern.tag === 'symbol') {
        if (pattern.value === '_' || pattern.value === '...' || literals.has(pattern.value))
            return new Set();
        return new Set([pattern.value]);
    }
    if (pattern.tag === 'list') {
        const result = new Set();
        for (const elem of pattern.value) {
            for (const v of collectPatternVarNames(elem, literals))
                result.add(v);
        }
        return result;
    }
    return new Set();
}
function matchPattern(pattern, input, literals, bindings) {
    if (pattern.tag === 'symbol') {
        if (pattern.value === '_')
            return true;
        if (literals.has(pattern.value)) {
            return input.tag === 'symbol' && input.value === pattern.value;
        }
        bindings[pattern.value] = input;
        return true;
    }
    if (pattern.tag === 'list') {
        if (input.tag !== 'list')
            return false;
        const pats = pattern.value;
        const inps = input.value;
        let ellipsisIdx = -1;
        for (let i = 0; i < pats.length; i++) {
            const pi = pats[i];
            if (pi.tag === 'symbol' && pi.value === '...') {
                ellipsisIdx = i;
                break;
            }
        }
        if (ellipsisIdx === -1) {
            if (pats.length !== inps.length)
                return false;
            for (let i = 0; i < pats.length; i++) {
                if (!matchPattern(pats[i], inps[i], literals, bindings))
                    return false;
            }
            return true;
        }
        const repeatPatIdx = ellipsisIdx - 1;
        const beforeCount = repeatPatIdx;
        const afterCount = pats.length - ellipsisIdx - 1;
        const minInputs = beforeCount + afterCount;
        if (inps.length < minInputs)
            return false;
        for (let i = 0; i < beforeCount; i++) {
            if (!matchPattern(pats[i], inps[i], literals, bindings))
                return false;
        }
        const repeatPat = pats[repeatPatIdx];
        const repeatVarNames = [...collectPatternVarNames(repeatPat, literals)];
        const repeatArrays = new Map();
        for (const name of repeatVarNames)
            repeatArrays.set(name, []);
        const repeatCount = inps.length - minInputs;
        for (let i = 0; i < repeatCount; i++) {
            const subBindings = {};
            if (!matchPattern(repeatPat, inps[beforeCount + i], literals, subBindings))
                return false;
            for (const name of repeatVarNames) {
                repeatArrays.get(name).push(subBindings[name]);
            }
        }
        for (const [name, arr] of repeatArrays) {
            bindings[name] = arr;
        }
        for (let i = 0; i < afterCount; i++) {
            if (!matchPattern(pats[ellipsisIdx + 1 + i], inps[inps.length - afterCount + i], literals, bindings))
                return false;
        }
        return true;
    }
    if (pattern.tag === 'boolean' && input.tag === 'boolean')
        return pattern.value === input.value;
    if (pattern.tag === 'number' && input.tag === 'number')
        return pattern.value === input.value;
    return false;
}
function findEllipsisVarsIn(tmpl, ellipsisVars) {
    const found = [];
    if (tmpl.tag === 'symbol' && ellipsisVars.has(tmpl.value)) {
        found.push(tmpl.value);
    }
    else if (tmpl.tag === 'list') {
        for (const elem of tmpl.value) {
            found.push(...findEllipsisVarsIn(elem, ellipsisVars));
        }
    }
    return found;
}
function instantiateTemplate(template, bindings, patternVars, ellipsisVars, defEnv, renameMap) {
    if (template.tag === 'symbol') {
        if (patternVars.has(template.value)) {
            const val = bindings[template.value];
            if (Array.isArray(val)) {
                throw new EvalError('ellipsis variable outside ellipsis context');
            }
            return val;
        }
        if (SPECIAL_FORMS.has(template.value))
            return template;
        // Hygiene: rename introduced identifiers
        if (!renameMap.has(template.value)) {
            const gs = gensym(template.value);
            renameMap.set(template.value, gs);
            resolvedSymbols.set(gs, { name: template.value, env: defEnv });
        }
        return { tag: 'symbol', value: renameMap.get(template.value) };
    }
    if (template.tag === 'list') {
        const elems = template.value;
        // Don't process inside quote
        if (elems.length >= 1 && elems[0].tag === 'symbol' && elems[0].value === 'quote') {
            return template;
        }
        const result = [];
        for (let i = 0; i < elems.length; i++) {
            const next = elems[i + 1];
            if (i + 1 < elems.length && next && next.tag === 'symbol' && next.value === '...') {
                const repeatTmpl = elems[i];
                const usedVars = findEllipsisVarsIn(repeatTmpl, ellipsisVars);
                if (usedVars.length > 0) {
                    const count = bindings[usedVars[0]].length;
                    for (let j = 0; j < count; j++) {
                        const tempBindings = { ...bindings };
                        for (const v of usedVars) {
                            tempBindings[v] = bindings[v][j];
                        }
                        result.push(instantiateTemplate(repeatTmpl, tempBindings, patternVars, new Set([...ellipsisVars].filter(v => !usedVars.includes(v))), defEnv, renameMap));
                    }
                }
                i++; // skip ...
                continue;
            }
            result.push(instantiateTemplate(elems[i], bindings, patternVars, ellipsisVars, defEnv, renameMap));
        }
        return { tag: 'list', value: result };
    }
    return template;
}
function expandMacro(macro, expr) {
    const literals = new Set(macro.literals);
    for (const rule of macro.rules) {
        const bindings = {};
        if (matchPattern(rule.pattern, expr, literals, bindings)) {
            const patternVars = collectPatternVarNames(rule.pattern, literals);
            const ellipsisVars = new Set();
            for (const [name, val] of Object.entries(bindings)) {
                if (Array.isArray(val))
                    ellipsisVars.add(name);
            }
            const renameMap = new Map();
            return instantiateTemplate(rule.template, bindings, patternVars, ellipsisVars, macro.defEnv, renameMap);
        }
    }
    throw new EvalError('no matching syntax-rules pattern');
}
function parseParams(params) {
    if (params.tag === 'symbol') {
        // (lambda args body) — all args captured as rest
        return { names: [], rest: params.value };
    }
    if (params.tag === 'list') {
        const names = params.value.map(p => {
            if (p.tag !== 'symbol')
                throw errAt('parameter must be a symbol', p.pos);
            return p.value;
        });
        return { names, rest: null };
    }
    if (params.tag === 'pair') {
        // Improper list from dotted notation: (a b . rest)
        const names = [];
        let cur = params;
        while (cur.tag === 'pair') {
            if (cur.car.tag !== 'symbol')
                throw errAt('parameter must be a symbol', cur.car.pos);
            names.push(cur.car.value);
            cur = cur.cdr;
        }
        if (cur.tag !== 'symbol')
            throw errAt('rest parameter must be a symbol', cur.pos);
        return { names, rest: cur.value };
    }
    throw errAt('invalid parameter list', params.pos);
}
function bindArgs(childEnv, paramInfo, args) {
    for (let i = 0; i < paramInfo.names.length; i++) {
        childEnv.set(paramInfo.names[i], args[i]);
    }
    if (paramInfo.rest !== null) {
        let restList = NIL;
        for (let i = args.length - 1; i >= paramInfo.names.length; i--) {
            restList = { tag: 'pair', car: args[i], cdr: restList };
        }
        childEnv.set(paramInfo.rest, restList);
    }
}
function makeProcedure(paramInfo, bodyExprs, closureEnv) {
    const proc = {
        tag: 'procedure',
        value: (...args) => {
            const childEnv = new Env(closureEnv);
            bindArgs(childEnv, paramInfo, args);
            return runBounce(evalSeqK(bodyExprs, 0, childEnv, (v) => ({ tag: 'done', value: v })));
        },
        _closure: { params: paramInfo, body: bodyExprs, env: closureEnv }
    };
    return proc;
}
// --- CPS Evaluator (for call/cc support) ---
function runBounce(b, maxSteps) {
    let bounce = b;
    if (maxSteps !== undefined) {
        let steps = 0;
        while (bounce.tag === 'bounce') {
            if (++steps > maxSteps)
                throw new EvalError('step limit exceeded');
            bounce = bounce.fn();
        }
    }
    else {
        while (bounce.tag === 'bounce')
            bounce = bounce.fn();
    }
    return bounce.value;
}
function evalSeqK(exprs, idx, env, k) {
    if (idx >= exprs.length)
        return k({ tag: 'void' });
    if (idx === exprs.length - 1)
        return { tag: 'bounce', fn: () => evalK(exprs[idx], env, k) };
    return evalK(exprs[idx], env, (_) => ({ tag: 'bounce', fn: () => evalSeqK(exprs, idx + 1, env, k) }));
}
// Right-to-left argument evaluation (matches Chez Scheme)
function evalArgsRtoLK(exprs, env, k) {
    const len = exprs.length;
    if (len === 0)
        return k([]);
    // Evaluate from right to left, build result array
    function go(i, acc) {
        if (i < 0)
            return k(acc);
        return evalK(exprs[i], env, (v) => {
            acc[i] = v;
            return { tag: 'bounce', fn: () => go(i - 1, acc) };
        });
    }
    return go(len - 1, new Array(len));
}
function doWindTransition(targetWind, then) {
    // Find common prefix
    let common = 0;
    while (common < windStack.length && common < targetWind.length &&
        windStack[common] === targetWind[common]) {
        common++;
    }
    // Unwind: pop entries and call out-thunks (innermost first)
    function unwind() {
        if (windStack.length <= common)
            return rewind(common);
        const entry = windStack.pop();
        return applyK(entry.outThunk, [], (_) => {
            return { tag: 'bounce', fn: unwind };
        });
    }
    // Rewind: push entries and call in-thunks (outermost first)
    function rewind(i) {
        if (i >= targetWind.length)
            return { tag: 'bounce', fn: then };
        const entry = targetWind[i];
        windStack.push(entry);
        return applyK(entry.inThunk, [], (_) => {
            return { tag: 'bounce', fn: () => rewind(i + 1) };
        });
    }
    return unwind();
}
function applyK(func, args, k, pos) {
    // call/cc: (call/cc proc) — proc receives the continuation
    if (func.tag === 'procedure' && func._callcc) {
        if (args.length !== 1)
            throw errAt('call/cc requires exactly 1 argument', pos);
        const proc = args[0];
        const capturedWind = [...windStack];
        const contVal = {
            tag: 'procedure',
            value: (...cargs) => {
                // For use from non-CPS context (e.g. map callback)
                throw new EvalError('continuation invoked outside CPS context');
            },
            _cont: k,
            _capturedWind: capturedWind
        };
        return applyK(proc, [contVal], k, pos);
    }
    // Continuation invocation (with wind transition)
    if (func.tag === 'procedure' && func._cont) {
        const val = args.length > 1 ? { tag: 'values', values: args } : args.length > 0 ? args[0] : { tag: 'void' };
        const targetWind = func._capturedWind || [];
        return doWindTransition(targetWind, () => func._cont(val));
    }
    // L20: raise
    if (func.tag === 'procedure' && func._raise) {
        if (args.length !== 1)
            throw errAt('raise requires exactly 1 argument', pos);
        const val = args[0];
        if (exceptionHandlers.length === 0) {
            throw new EvalError(`unhandled exception: ${displayVal(val)}`);
        }
        const handler = exceptionHandlers.pop();
        return doWindTransition(handler.wind, () => handler.handle(val));
    }
    // L20: with-exception-handler
    if (func.tag === 'procedure' && func._withExceptionHandler) {
        if (args.length !== 2)
            throw errAt('with-exception-handler requires 2 arguments', pos);
        const [handlerProc, thunk] = args;
        const capturedWind = [...windStack];
        const entry = {
            wind: capturedWind,
            handle: (exn) => {
                return applyK(handlerProc, [exn], (_result) => {
                    // Handler returned normally — re-raise
                    if (exceptionHandlers.length === 0) {
                        throw new EvalError(`handler returned from raise: ${displayVal(exn)}`);
                    }
                    const next = exceptionHandlers.pop();
                    return doWindTransition(next.wind, () => next.handle(exn));
                }, pos);
            }
        };
        exceptionHandlers.push(entry);
        return applyK(thunk, [], (result) => {
            const idx = exceptionHandlers.indexOf(entry);
            if (idx >= 0)
                exceptionHandlers.splice(idx, 1);
            return k(result);
        }, pos);
    }
    // L21: values
    if (func.tag === 'procedure' && func._values) {
        if (args.length === 1)
            return k(args[0]);
        return k({ tag: 'values', values: args });
    }
    // L21: call-with-values
    if (func.tag === 'procedure' && func._callWithValues) {
        if (args.length !== 2)
            throw errAt('call-with-values requires 2 arguments', pos);
        const [producer, consumer] = args;
        return applyK(producer, [], (result) => {
            if (result.tag === 'values') {
                return applyK(consumer, result.values, k, pos);
            }
            return applyK(consumer, [result], k, pos);
        }, pos);
    }
    // dynamic-wind
    if (func.tag === 'procedure' && func._dynamicWind) {
        if (args.length !== 3)
            throw errAt('dynamic-wind requires 3 arguments', pos);
        const [inThunk, bodyThunk, outThunk] = args;
        // Call in-thunk
        return applyK(inThunk, [], (_) => {
            // Push wind entry
            const entry = { inThunk, outThunk };
            windStack.push(entry);
            // Call body-thunk
            return applyK(bodyThunk, [], (bodyResult) => {
                // Pop wind entry
                windStack.pop();
                // Call out-thunk
                return applyK(outThunk, [], (_) => {
                    return k(bodyResult);
                }, pos);
            }, pos);
        }, pos);
    }
    if (func.tag !== 'procedure')
        throw errAt('not a procedure', pos);
    // User-defined closure — evaluate body in CPS
    const cl = func._closure;
    if (cl) {
        const childEnv = new Env(cl.env);
        bindArgs(childEnv, cl.params, args);
        return evalSeqK(cl.body, 0, childEnv, k);
    }
    // Case-lambda
    if (func._caseClauses) {
        const { clauses, closureEnv } = func._caseClauses;
        for (const cl of clauses) {
            if (cl.paramInfo.rest !== null) {
                if (args.length >= cl.paramInfo.names.length) {
                    const childEnv = new Env(closureEnv);
                    bindArgs(childEnv, cl.paramInfo, args);
                    return evalSeqK(cl.bodyExprs, 0, childEnv, k);
                }
            }
            else {
                if (args.length === cl.paramInfo.names.length) {
                    const childEnv = new Env(closureEnv);
                    bindArgs(childEnv, cl.paramInfo, args);
                    return evalSeqK(cl.bodyExprs, 0, childEnv, k);
                }
            }
        }
        throw new EvalError(`no matching clause for ${args.length} arguments`);
    }
    // Native procedure
    try {
        const result = func.value(...args);
        return k(result);
    }
    catch (e) {
        if (e instanceof EvalError && !/^\d+:/.test(e.message)) {
            throw errAt(e.message, pos);
        }
        throw e;
    }
}
function resolveSymbol(name, env, pos) {
    try {
        return env.get(name);
    }
    catch {
        const resolved = resolvedSymbols.get(name);
        if (resolved) {
            try {
                return resolved.env.get(resolved.name);
            }
            catch (e2) {
                if (e2 instanceof EvalError)
                    throw errAt(e2.message, pos);
                throw e2;
            }
        }
        throw errAt(`unbound variable: ${name}`, pos);
    }
}
function expandQuasiquote(tmpl, depth, env, k) {
    if (tmpl.tag === 'list' && tmpl.value.length === 2 && tmpl.value[0].tag === 'symbol' && tmpl.value[0].value === 'unquote') {
        if (depth === 0) {
            return evalK(tmpl.value[1], env, k);
        }
        else {
            return expandQuasiquote(tmpl.value[1], depth - 1, env, inner => k({ tag: 'list', value: [{ tag: 'symbol', value: 'unquote' }, inner] }));
        }
    }
    if (tmpl.tag === 'list' && tmpl.value.length === 2 && tmpl.value[0].tag === 'symbol' && tmpl.value[0].value === 'quasiquote') {
        return expandQuasiquote(tmpl.value[1], depth + 1, env, inner => k({ tag: 'list', value: [{ tag: 'symbol', value: 'quasiquote' }, inner] }));
    }
    if (tmpl.tag === 'list') {
        // Check for unquote-splicing in elements
        const elems = tmpl.value;
        function buildList(i, acc, k2) {
            if (i >= elems.length) {
                let result = NIL;
                for (let j = acc.length - 1; j >= 0; j--) {
                    result = { tag: 'pair', car: acc[j], cdr: result };
                }
                return k2(result);
            }
            const el = elems[i];
            if (el.tag === 'list' && el.value.length === 2 && el.value[0].tag === 'symbol' && el.value[0].value === 'unquote-splicing') {
                if (depth === 0) {
                    return evalK(el.value[1], env, spliced => {
                        const items = [...acc];
                        let cur = spliced;
                        while (cur.tag === 'pair') {
                            items.push(cur.car);
                            cur = cur.cdr;
                        }
                        if (cur.tag === 'list') {
                            for (const x of cur.value)
                                items.push(x);
                        }
                        return buildList(i + 1, items, k2);
                    });
                }
                else {
                    return expandQuasiquote(el.value[1], depth - 1, env, inner => {
                        const newEl = { tag: 'list', value: [{ tag: 'symbol', value: 'unquote-splicing' }, inner] };
                        return buildList(i + 1, [...acc, newEl], k2);
                    });
                }
            }
            return expandQuasiquote(el, depth, env, expanded => {
                return buildList(i + 1, [...acc, expanded], k2);
            });
        }
        return buildList(0, [], k);
    }
    if (tmpl.tag === 'pair') {
        // Check car for unquote-splicing
        if (tmpl.car.tag === 'list' && tmpl.car.value.length === 2 && tmpl.car.value[0].tag === 'symbol' && tmpl.car.value[0].value === 'unquote-splicing' && depth === 0) {
            return evalK(tmpl.car.value[1], env, spliced => {
                return expandQuasiquote(tmpl.cdr, depth, env, expandedCdr => {
                    // Append spliced list to expandedCdr
                    let result = expandedCdr;
                    const items = [];
                    let cur = spliced;
                    while (cur.tag === 'pair') {
                        items.push(cur.car);
                        cur = cur.cdr;
                    }
                    if (cur.tag === 'list') {
                        for (const x of cur.value)
                            items.push(x);
                    }
                    for (let j = items.length - 1; j >= 0; j--) {
                        result = { tag: 'pair', car: items[j], cdr: result };
                    }
                    return k(result);
                });
            });
        }
        return expandQuasiquote(tmpl.car, depth, env, expandedCar => {
            return expandQuasiquote(tmpl.cdr, depth, env, expandedCdr => {
                return k({ tag: 'pair', car: expandedCar, cdr: expandedCdr });
            });
        });
    }
    // Atom / self-evaluating — return as-is (like quote)
    return k(listToConsPairs(tmpl));
}
function evalK(expr, env, k) {
    switch (expr.tag) {
        case 'number':
        case 'rational':
        case 'boolean':
        case 'string':
        case 'char':
        case 'vector':
            return k(expr);
        case 'symbol':
            return k(resolveSymbol(expr.value, env, expr.pos));
        case 'list': {
            const elems = expr.value;
            if (elems.length === 0)
                throw errAt('empty application', expr.pos);
            const first = elems[0];
            // Special forms
            if (first.tag === 'symbol') {
                switch (first.value) {
                    case 'define': {
                        if (elems.length < 3)
                            throw errAt('define requires at least 2 arguments', expr.pos);
                        const target = elems[1];
                        if (target.tag === 'symbol') {
                            return evalK(elems[2], env, (val) => {
                                env.set(target.value, val);
                                return k({ tag: 'void' });
                            });
                        }
                        if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
                            const name = target.value[0].value;
                            const paramsForm = { tag: 'list', value: target.value.slice(1), pos: target.pos };
                            const paramInfo = parseParams(paramsForm);
                            const bodyExprs = elems.slice(2);
                            env.set(name, makeProcedure(paramInfo, bodyExprs, env));
                            return k({ tag: 'void' });
                        }
                        if (target.tag === 'pair' && target.car.tag === 'symbol') {
                            const name = target.car.value;
                            const paramInfo = parseParams(target.cdr);
                            const bodyExprs = elems.slice(2);
                            env.set(name, makeProcedure(paramInfo, bodyExprs, env));
                            return k({ tag: 'void' });
                        }
                        throw errAt('invalid define syntax', expr.pos);
                    }
                    case 'set!': {
                        if (elems.length !== 3)
                            throw errAt('set! requires exactly 2 arguments', expr.pos);
                        const target = elems[1];
                        if (target.tag !== 'symbol')
                            throw errAt('set! target must be a symbol', expr.pos);
                        return evalK(elems[2], env, (val) => {
                            try {
                                env.update(target.value, val);
                            }
                            catch (e) {
                                if (e instanceof EvalError)
                                    throw errAt(e.message, expr.pos);
                                throw e;
                            }
                            return k({ tag: 'void' });
                        });
                    }
                    case 'if': {
                        if (elems.length < 3)
                            throw errAt('if requires at least 2 arguments', expr.pos);
                        return evalK(elems[1], env, (cond) => {
                            if (!isFalsy(cond)) {
                                return { tag: 'bounce', fn: () => evalK(elems[2], env, k) };
                            }
                            else if (elems.length > 3) {
                                return { tag: 'bounce', fn: () => evalK(elems[3], env, k) };
                            }
                            return k({ tag: 'void' });
                        });
                    }
                    case 'quote': {
                        if (elems.length !== 2)
                            throw errAt('quote requires exactly 1 argument', expr.pos);
                        return k(listToConsPairs(elems[1]));
                    }
                    case 'quasiquote': {
                        if (elems.length !== 2)
                            throw errAt('quasiquote requires exactly 1 argument', expr.pos);
                        return expandQuasiquote(elems[1], 0, env, k);
                    }
                    case 'lambda': {
                        if (elems.length < 3)
                            throw errAt('lambda requires params and body', expr.pos);
                        const paramInfo = parseParams(elems[1]);
                        const bodyExprs = elems.slice(2);
                        return k(makeProcedure(paramInfo, bodyExprs, env));
                    }
                    case 'case-lambda': {
                        if (elems.length < 2)
                            throw errAt('case-lambda requires at least one clause', expr.pos);
                        const clauses = [];
                        for (let i = 1; i < elems.length; i++) {
                            const clause = elems[i];
                            if (clause.tag !== 'list' || clause.value.length < 2)
                                throw errAt('case-lambda clause must have params and body', clause.pos);
                            const paramInfo = parseParams(clause.value[0]);
                            const bodyExprs = clause.value.slice(1);
                            clauses.push({ paramInfo, bodyExprs });
                        }
                        const closureEnv = env;
                        const proc = {
                            tag: 'procedure',
                            value: (...args) => {
                                for (const cl of clauses) {
                                    if (cl.paramInfo.rest !== null) {
                                        if (args.length >= cl.paramInfo.names.length) {
                                            const childEnv = new Env(closureEnv);
                                            bindArgs(childEnv, cl.paramInfo, args);
                                            return runBounce(evalSeqK(cl.bodyExprs, 0, childEnv, (v) => ({ tag: 'done', value: v })));
                                        }
                                    }
                                    else {
                                        if (args.length === cl.paramInfo.names.length) {
                                            const childEnv = new Env(closureEnv);
                                            bindArgs(childEnv, cl.paramInfo, args);
                                            return runBounce(evalSeqK(cl.bodyExprs, 0, childEnv, (v) => ({ tag: 'done', value: v })));
                                        }
                                    }
                                }
                                throw new EvalError(`no matching clause for ${args.length} arguments`);
                            },
                            _caseClauses: { clauses, closureEnv }
                        };
                        return k(proc);
                    }
                    case 'and': {
                        if (elems.length === 1)
                            return k({ tag: 'boolean', value: true });
                        const evalAnd = (i) => {
                            if (i === elems.length - 1)
                                return { tag: 'bounce', fn: () => evalK(elems[i], env, k) };
                            return evalK(elems[i], env, (val) => {
                                if (isFalsy(val))
                                    return k(val);
                                return { tag: 'bounce', fn: () => evalAnd(i + 1) };
                            });
                        };
                        return evalAnd(1);
                    }
                    case 'or': {
                        if (elems.length === 1)
                            return k({ tag: 'boolean', value: false });
                        const evalOr = (i) => {
                            if (i === elems.length - 1)
                                return { tag: 'bounce', fn: () => evalK(elems[i], env, k) };
                            return evalK(elems[i], env, (val) => {
                                if (!isFalsy(val))
                                    return k(val);
                                return { tag: 'bounce', fn: () => evalOr(i + 1) };
                            });
                        };
                        return evalOr(1);
                    }
                    case 'begin': {
                        if (elems.length === 1)
                            return k({ tag: 'void' });
                        return evalSeqK(elems.slice(1), 0, env, k);
                    }
                    case 'let': {
                        if (elems.length < 3)
                            throw errAt('let requires bindings and body', expr.pos);
                        // Named let
                        if (elems[1].tag === 'symbol') {
                            if (elems.length < 4)
                                throw errAt('named let requires bindings and body', expr.pos);
                            const loopName = elems[1].value;
                            const bindingsList = elems[2];
                            if (bindingsList.tag !== 'list')
                                throw errAt('let bindings must be a list', expr.pos);
                            const paramNames = [];
                            for (const binding of bindingsList.value) {
                                if (binding.tag !== 'list' || binding.value.length !== 2)
                                    throw errAt('invalid let binding', binding.pos);
                                if (binding.value[0].tag !== 'symbol')
                                    throw errAt('let binding name must be a symbol', binding.pos);
                                paramNames.push(binding.value[0].value);
                            }
                            const bodyExprs = elems.slice(3);
                            const paramInfo = { names: paramNames, rest: null };
                            const evalInits = (i, vals) => {
                                if (i >= bindingsList.value.length) {
                                    const closureEnv = new Env(env);
                                    const loopProc = makeProcedure(paramInfo, bodyExprs, closureEnv);
                                    closureEnv.set(loopName, loopProc);
                                    const childEnv = new Env(closureEnv);
                                    for (let j = 0; j < paramNames.length; j++)
                                        childEnv.set(paramNames[j], vals[j]);
                                    return evalSeqK(bodyExprs, 0, childEnv, k);
                                }
                                return evalK(bindingsList.value[i].value[1], env, (val) => {
                                    vals.push(val);
                                    return { tag: 'bounce', fn: () => evalInits(i + 1, vals) };
                                });
                            };
                            return evalInits(0, []);
                        }
                        // Regular let
                        const bindings = elems[1];
                        if (bindings.tag !== 'list')
                            throw errAt('let bindings must be a list', expr.pos);
                        const childEnv = new Env(env);
                        const evalBindings = (i) => {
                            if (i >= bindings.value.length) {
                                return evalSeqK(elems.slice(2), 0, childEnv, k);
                            }
                            const binding = bindings.value[i];
                            if (binding.tag !== 'list' || binding.value.length !== 2)
                                throw errAt('invalid let binding', binding.pos);
                            const name = binding.value[0];
                            if (name.tag !== 'symbol')
                                throw errAt('let binding name must be a symbol', name.pos);
                            return evalK(binding.value[1], env, (val) => {
                                childEnv.set(name.value, val);
                                return { tag: 'bounce', fn: () => evalBindings(i + 1) };
                            });
                        };
                        return evalBindings(0);
                    }
                    case 'let*': {
                        if (elems.length < 3)
                            throw errAt('let* requires bindings and body', expr.pos);
                        const bindings = elems[1];
                        if (bindings.tag !== 'list')
                            throw errAt('let* bindings must be a list', expr.pos);
                        const childEnv = new Env(env);
                        const evalBindings = (i) => {
                            if (i >= bindings.value.length) {
                                return evalSeqK(elems.slice(2), 0, childEnv, k);
                            }
                            const binding = bindings.value[i];
                            if (binding.tag !== 'list' || binding.value.length !== 2)
                                throw errAt('invalid let* binding', binding.pos);
                            if (binding.value[0].tag !== 'symbol')
                                throw errAt('let* binding name must be a symbol', binding.pos);
                            return evalK(binding.value[1], childEnv, (val) => {
                                childEnv.set(binding.value[0].value, val);
                                return { tag: 'bounce', fn: () => evalBindings(i + 1) };
                            });
                        };
                        return evalBindings(0);
                    }
                    case 'cond': {
                        const tryCond = (i) => {
                            if (i >= elems.length)
                                return k({ tag: 'void' });
                            const clause = elems[i];
                            if (clause.tag !== 'list' || clause.value.length < 1)
                                throw errAt('invalid cond clause', clause.pos);
                            const test = clause.value[0];
                            if (test.tag === 'symbol' && test.value === 'else') {
                                if (clause.value.length === 1)
                                    return k({ tag: 'void' });
                                return evalSeqK(clause.value.slice(1), 0, env, k);
                            }
                            return evalK(test, env, (testVal) => {
                                if (!isFalsy(testVal)) {
                                    if (clause.value.length === 1)
                                        return k(testVal);
                                    // Support (test => proc) syntax
                                    if (clause.value.length === 3 && clause.value[1].tag === 'symbol' && clause.value[1].value === '=>') {
                                        return evalK(clause.value[2], env, (proc) => {
                                            return applyK(proc, [testVal], k, expr.pos);
                                        });
                                    }
                                    return evalSeqK(clause.value.slice(1), 0, env, k);
                                }
                                return { tag: 'bounce', fn: () => tryCond(i + 1) };
                            });
                        };
                        return tryCond(1);
                    }
                    case 'letrec': {
                        if (elems.length < 3)
                            throw errAt('letrec requires bindings and body', expr.pos);
                        const bindings = elems[1];
                        if (bindings.tag !== 'list')
                            throw errAt('letrec bindings must be a list', expr.pos);
                        const childEnv = new Env(env);
                        const names = [];
                        for (const binding of bindings.value) {
                            if (binding.tag !== 'list' || binding.value.length !== 2)
                                throw errAt('invalid letrec binding', binding.pos);
                            if (binding.value[0].tag !== 'symbol')
                                throw errAt('letrec binding name must be a symbol', binding.pos);
                            names.push(binding.value[0].value);
                            childEnv.set(binding.value[0].value, { tag: 'void' });
                        }
                        const evalLetrecBindings = (i) => {
                            if (i >= bindings.value.length) {
                                return evalSeqK(elems.slice(2), 0, childEnv, k);
                            }
                            return evalK(bindings.value[i].value[1], childEnv, (val) => {
                                childEnv.set(names[i], val);
                                return { tag: 'bounce', fn: () => evalLetrecBindings(i + 1) };
                            });
                        };
                        return evalLetrecBindings(0);
                    }
                    case 'letrec*': {
                        if (elems.length < 3)
                            throw errAt('letrec* requires bindings and body', expr.pos);
                        const bindings = elems[1];
                        if (bindings.tag !== 'list')
                            throw errAt('letrec* bindings must be a list', expr.pos);
                        const childEnv = new Env(env);
                        const evalBindings = (i) => {
                            if (i >= bindings.value.length) {
                                return evalSeqK(elems.slice(2), 0, childEnv, k);
                            }
                            const binding = bindings.value[i];
                            if (binding.tag !== 'list' || binding.value.length !== 2)
                                throw errAt('invalid letrec* binding', binding.pos);
                            if (binding.value[0].tag !== 'symbol')
                                throw errAt('letrec* binding name must be a symbol', binding.pos);
                            return evalK(binding.value[1], childEnv, (val) => {
                                childEnv.set(binding.value[0].value, val);
                                return { tag: 'bounce', fn: () => evalBindings(i + 1) };
                            });
                        };
                        return evalBindings(0);
                    }
                    case 'case': {
                        if (elems.length < 2)
                            throw errAt('case requires at least a key', expr.pos);
                        return evalK(elems[1], env, (key) => {
                            const tryClause = (i) => {
                                if (i >= elems.length)
                                    return k({ tag: 'void' });
                                const clause = elems[i];
                                if (clause.tag !== 'list' || clause.value.length < 2)
                                    throw errAt('invalid case clause', clause.pos);
                                const datums = clause.value[0];
                                if (datums.tag === 'symbol' && datums.value === 'else') {
                                    return evalSeqK(clause.value.slice(1), 0, env, k);
                                }
                                if (datums.tag !== 'list')
                                    throw errAt('case clause datums must be a list', clause.pos);
                                let matched = false;
                                for (const datum of datums.value) {
                                    const d = listToConsPairs(datum);
                                    if (schemeEqv(key, d)) {
                                        matched = true;
                                        break;
                                    }
                                }
                                if (matched) {
                                    return evalSeqK(clause.value.slice(1), 0, env, k);
                                }
                                return { tag: 'bounce', fn: () => tryClause(i + 1) };
                            };
                            return tryClause(2);
                        });
                    }
                    case 'do': {
                        if (elems.length < 3)
                            throw errAt('do requires bindings and test', expr.pos);
                        const bindingsForm = elems[1];
                        const testForm = elems[2];
                        if (bindingsForm.tag !== 'list')
                            throw errAt('do bindings must be a list', expr.pos);
                        if (testForm.tag !== 'list' || testForm.value.length < 1)
                            throw errAt('do test must be a list', expr.pos);
                        const vars = [];
                        const childEnv = new Env(env);
                        for (const binding of bindingsForm.value) {
                            if (binding.tag !== 'list' || binding.value.length < 2)
                                throw errAt('invalid do binding', binding.pos);
                            if (binding.value[0].tag !== 'symbol')
                                throw errAt('do variable must be a symbol', binding.pos);
                            const name = binding.value[0].value;
                            const step = binding.value.length > 2 ? binding.value[2] : null;
                            vars.push({ name, step });
                        }
                        // Evaluate init values
                        const evalDoInits = (i) => {
                            if (i >= bindingsForm.value.length)
                                return doLoop();
                            return evalK(bindingsForm.value[i].value[1], env, (val) => {
                                childEnv.set(vars[i].name, val);
                                return { tag: 'bounce', fn: () => evalDoInits(i + 1) };
                            });
                        };
                        const doLoop = () => {
                            return evalK(testForm.value[0], childEnv, (testVal) => {
                                if (!isFalsy(testVal)) {
                                    if (testForm.value.length === 1)
                                        return k({ tag: 'void' });
                                    return evalSeqK(testForm.value.slice(1), 0, childEnv, k);
                                }
                                // Eval body
                                const evalBody = (j) => {
                                    if (j >= elems.length)
                                        return evalSteps(0, []);
                                    return evalK(elems[j], childEnv, (_) => ({ tag: 'bounce', fn: () => evalBody(j + 1) }));
                                };
                                // Eval step values
                                const evalSteps = (j, newVals) => {
                                    if (j >= vars.length) {
                                        for (let m = 0; m < vars.length; m++) {
                                            if (newVals[m] !== null)
                                                childEnv.set(vars[m].name, newVals[m]);
                                        }
                                        return { tag: 'bounce', fn: () => doLoop() };
                                    }
                                    if (vars[j].step) {
                                        return evalK(vars[j].step, childEnv, (val) => {
                                            newVals.push(val);
                                            return { tag: 'bounce', fn: () => evalSteps(j + 1, newVals) };
                                        });
                                    }
                                    newVals.push(null);
                                    return { tag: 'bounce', fn: () => evalSteps(j + 1, newVals) };
                                };
                                return { tag: 'bounce', fn: () => evalBody(3) };
                            });
                        };
                        return evalDoInits(0);
                    }
                    case 'define-syntax': {
                        if (elems.length !== 3)
                            throw errAt('define-syntax requires 2 arguments', expr.pos);
                        if (elems[1].tag !== 'symbol')
                            throw errAt('define-syntax: name must be a symbol', expr.pos);
                        const macroName = elems[1].value;
                        const transformer = elems[2];
                        // Check if it's a syntax-rules form
                        if (transformer.tag === 'list' && transformer.value.length >= 2 &&
                            transformer.value[0].tag === 'symbol' && transformer.value[0].value === 'syntax-rules') {
                            const srElems = transformer.value;
                            if (srElems[1].tag !== 'list')
                                throw errAt('syntax-rules: expected literals list', expr.pos);
                            const macroLiterals = srElems[1].value.map(l => {
                                if (l.tag !== 'symbol')
                                    throw errAt('syntax-rules: literal must be a symbol', l.pos);
                                return l.value;
                            });
                            const macroRules = [];
                            for (let ri = 2; ri < srElems.length; ri++) {
                                const rule = srElems[ri];
                                if (rule.tag !== 'list' || rule.value.length !== 2)
                                    throw errAt('syntax-rules: each rule must be (pattern template)', rule.pos);
                                macroRules.push({ pattern: rule.value[0], template: rule.value[1] });
                            }
                            env.set(macroName, { tag: 'macro', literals: macroLiterals, rules: macroRules, defEnv: env });
                            return k({ tag: 'void' });
                        }
                        // Otherwise evaluate the transformer expression (e.g., a lambda)
                        return evalK(transformer, env, (proc) => {
                            env.set(macroName, { tag: 'syntax-transformer', proc });
                            return k({ tag: 'void' });
                        });
                    }
                    case 'syntax-case': {
                        // (syntax-case expr (literals) clause ...)
                        // clause = (pattern body) or (pattern fender body)
                        if (elems.length < 4)
                            throw errAt('syntax-case requires expr, literals, and clauses', expr.pos);
                        const scLitsList = elems[2];
                        if (scLitsList.tag !== 'list')
                            throw errAt('syntax-case: literals must be a list', expr.pos);
                        const scLiterals = new Set(scLitsList.value.map(l => {
                            if (l.tag !== 'symbol')
                                throw errAt('syntax-case: literal must be a symbol', l.pos);
                            return l.value;
                        }));
                        const scClauses = elems.slice(3);
                        return evalK(elems[1], env, (stxVal) => {
                            const tryClauses = (ci) => {
                                if (ci >= scClauses.length)
                                    throw errAt('no matching syntax-case pattern', expr.pos);
                                const clause = scClauses[ci];
                                if (clause.tag !== 'list' || clause.value.length < 2)
                                    throw errAt('syntax-case: invalid clause', clause.pos);
                                const pat = clause.value[0];
                                const hasFender = clause.value.length === 3;
                                const fenderExpr = hasFender ? clause.value[1] : null;
                                const bodyExpr = hasFender ? clause.value[2] : clause.value[1];
                                const bindings = {};
                                if (matchPattern(pat, stxVal, scLiterals, bindings)) {
                                    const patVars = collectPatternVarNames(pat, scLiterals);
                                    const ellVars = new Set();
                                    for (const [name, val] of Object.entries(bindings)) {
                                        if (Array.isArray(val))
                                            ellVars.add(name);
                                    }
                                    const ctx = {
                                        bindings, patternVars: patVars, ellipsisVars: ellVars, defEnv: env
                                    };
                                    const evalBody = () => {
                                        syntaxCaseStack.push(ctx);
                                        return evalK(bodyExpr, env, (result) => {
                                            syntaxCaseStack.pop();
                                            return k(result);
                                        });
                                    };
                                    if (fenderExpr) {
                                        syntaxCaseStack.push(ctx);
                                        return evalK(fenderExpr, env, (fenderVal) => {
                                            syntaxCaseStack.pop();
                                            if (isFalsy(fenderVal)) {
                                                return { tag: 'bounce', fn: () => tryClauses(ci + 1) };
                                            }
                                            return evalBody();
                                        });
                                    }
                                    return evalBody();
                                }
                                return { tag: 'bounce', fn: () => tryClauses(ci + 1) };
                            };
                            return tryClauses(0);
                        });
                    }
                    case 'syntax': {
                        // (syntax template) — expand template using syntax-case bindings
                        if (elems.length !== 2)
                            throw errAt('syntax requires exactly 1 argument', expr.pos);
                        const tmpl = elems[1];
                        if (syntaxCaseStack.length === 0) {
                            throw errAt('syntax used outside of syntax-case', expr.pos);
                        }
                        const ctx = syntaxCaseStack[syntaxCaseStack.length - 1];
                        const renameMap = new Map();
                        const result = instantiateTemplate(tmpl, ctx.bindings, ctx.patternVars, ctx.ellipsisVars, ctx.defEnv, renameMap);
                        return k(result);
                    }
                    case 'with-syntax': {
                        // (with-syntax ((pattern expr) ...) body ...)
                        if (elems.length < 3)
                            throw errAt('with-syntax requires bindings and body', expr.pos);
                        const wsBindingsList = elems[1];
                        if (wsBindingsList.tag !== 'list')
                            throw errAt('with-syntax: bindings must be a list', expr.pos);
                        const wsBindings = wsBindingsList.value;
                        const wsBodyExprs = elems.slice(2);
                        // Evaluate binding exprs sequentially, collect pattern bindings
                        const evalWSBindings = (i, accumulated, accPatVars, accEllVars) => {
                            if (i >= wsBindings.length) {
                                // Merge with existing syntax-case context
                                let baseCtx = null;
                                if (syntaxCaseStack.length > 0) {
                                    baseCtx = syntaxCaseStack[syntaxCaseStack.length - 1];
                                }
                                const mergedBindings = baseCtx ? { ...baseCtx.bindings, ...accumulated } : { ...accumulated };
                                const mergedPatVars = new Set(baseCtx ? [...baseCtx.patternVars, ...accPatVars] : accPatVars);
                                const mergedEllVars = new Set(baseCtx ? [...baseCtx.ellipsisVars, ...accEllVars] : accEllVars);
                                const newCtx = {
                                    bindings: mergedBindings,
                                    patternVars: mergedPatVars,
                                    ellipsisVars: mergedEllVars,
                                    defEnv: baseCtx ? baseCtx.defEnv : env,
                                };
                                syntaxCaseStack.push(newCtx);
                                return evalSeqK(wsBodyExprs, 0, env, (result) => {
                                    syntaxCaseStack.pop();
                                    return k(result);
                                });
                            }
                            const binding = wsBindings[i];
                            if (binding.tag !== 'list' || binding.value.length !== 2)
                                throw errAt('with-syntax: each binding must be (pattern expr)', binding.pos);
                            const wsPat = binding.value[0];
                            const wsExpr = binding.value[1];
                            return evalK(wsExpr, env, (wsVal) => {
                                const subBindings = {};
                                if (!matchPattern(wsPat, wsVal, new Set(), subBindings)) {
                                    throw errAt('with-syntax: pattern match failed', binding.pos);
                                }
                                const subPatVars = collectPatternVarNames(wsPat, new Set());
                                for (const [n, v] of Object.entries(subBindings)) {
                                    accumulated[n] = v;
                                    if (Array.isArray(v))
                                        accEllVars.add(n);
                                }
                                for (const v of subPatVars)
                                    accPatVars.add(v);
                                return { tag: 'bounce', fn: () => evalWSBindings(i + 1, accumulated, accPatVars, accEllVars) };
                            });
                        };
                        return evalWSBindings(0, {}, new Set(), new Set());
                    }
                    case 'guard': {
                        // (guard (exn clause ...) body ...)
                        if (elems.length < 3)
                            throw errAt('guard requires variable and body', expr.pos);
                        const guardSpec = elems[1];
                        if (guardSpec.tag !== 'list' || guardSpec.value.length < 1)
                            throw errAt('guard requires (var clause ...)', expr.pos);
                        const exnVar = guardSpec.value[0];
                        if (exnVar.tag !== 'symbol')
                            throw errAt('guard variable must be a symbol', expr.pos);
                        const clauses = guardSpec.value.slice(1);
                        const bodyExprs = elems.slice(2);
                        const capturedWind = [...windStack];
                        const guardEnv = new Env(env);
                        const entry = {
                            wind: capturedWind,
                            handle: (exn) => {
                                guardEnv.set(exnVar.value, exn);
                                const tryClauses = (i) => {
                                    if (i >= clauses.length) {
                                        // No matching clause — re-raise
                                        if (exceptionHandlers.length === 0) {
                                            throw new EvalError(`unhandled exception: ${displayVal(exn)}`);
                                        }
                                        const next = exceptionHandlers.pop();
                                        return doWindTransition(next.wind, () => next.handle(exn));
                                    }
                                    const clause = clauses[i];
                                    if (clause.tag !== 'list' || clause.value.length < 1)
                                        throw errAt('invalid guard clause', clause.pos);
                                    const test = clause.value[0];
                                    if (test.tag === 'symbol' && test.value === 'else') {
                                        if (clause.value.length === 1)
                                            return k({ tag: 'void' });
                                        return evalSeqK(clause.value.slice(1), 0, guardEnv, k);
                                    }
                                    return evalK(test, guardEnv, (testVal) => {
                                        if (!isFalsy(testVal)) {
                                            if (clause.value.length === 1)
                                                return k(testVal);
                                            return evalSeqK(clause.value.slice(1), 0, guardEnv, k);
                                        }
                                        return { tag: 'bounce', fn: () => tryClauses(i + 1) };
                                    });
                                };
                                return tryClauses(0);
                            }
                        };
                        exceptionHandlers.push(entry);
                        return evalSeqK(bodyExprs, 0, env, (result) => {
                            const idx = exceptionHandlers.indexOf(entry);
                            if (idx >= 0)
                                exceptionHandlers.splice(idx, 1);
                            return { tag: 'bounce', fn: () => k(result) };
                        });
                    }
                    case 'define-record-type': {
                        if (elems.length < 4)
                            throw errAt('define-record-type requires at least 3 arguments', expr.pos);
                        const nameForm = elems[1];
                        if (nameForm.tag !== 'symbol')
                            throw errAt('define-record-type: expected type name', expr.pos);
                        const typeName = nameForm.value;
                        const typeId = Symbol(typeName);
                        const ctorForm = elems[2];
                        if (ctorForm.tag !== 'list' || ctorForm.value.length < 1)
                            throw errAt('define-record-type: expected constructor', expr.pos);
                        const ctorName = ctorForm.value[0];
                        if (ctorName.tag !== 'symbol')
                            throw errAt('define-record-type: constructor name must be a symbol', expr.pos);
                        const ctorFields = ctorForm.value.slice(1).map(f => {
                            if (f.tag !== 'symbol')
                                throw errAt('define-record-type: field name must be a symbol', f.pos);
                            return f.value;
                        });
                        const predForm = elems[3];
                        if (predForm.tag !== 'symbol')
                            throw errAt('define-record-type: expected predicate name', expr.pos);
                        env.set(ctorName.value, { tag: 'procedure', value: (...args) => {
                                if (args.length !== ctorFields.length)
                                    throw new EvalError(`${ctorName.value}: expected ${ctorFields.length} arguments, got ${args.length}`);
                                const fields = new Map();
                                for (let i = 0; i < ctorFields.length; i++)
                                    fields.set(ctorFields[i], args[i]);
                                return { tag: 'record', typeName, typeId, fields };
                            } });
                        env.set(predForm.value, { tag: 'procedure', value: (...args) => {
                                if (args.length !== 1)
                                    throw new EvalError(`${predForm.value}: expected 1 argument`);
                                return { tag: 'boolean', value: args[0].tag === 'record' && args[0].typeId === typeId };
                            } });
                        for (let i = 4; i < elems.length; i++) {
                            const fieldDef = elems[i];
                            if (fieldDef.tag !== 'list' || fieldDef.value.length < 2)
                                throw errAt('define-record-type: expected (field accessor)', fieldDef.pos);
                            const fieldName = fieldDef.value[0];
                            const accessorName = fieldDef.value[1];
                            if (fieldName.tag !== 'symbol' || accessorName.tag !== 'symbol')
                                throw errAt('define-record-type: field/accessor must be symbols', fieldDef.pos);
                            const fn = fieldName.value;
                            env.set(accessorName.value, { tag: 'procedure', value: (...args) => {
                                    if (args.length !== 1)
                                        throw new EvalError(`${accessorName.value}: expected 1 argument`);
                                    if (args[0].tag !== 'record' || args[0].typeId !== typeId)
                                        throw new EvalError(`${accessorName.value}: expected ${typeName}`);
                                    return args[0].fields.get(fn);
                                } });
                        }
                        return k({ tag: 'void' });
                    }
                }
                // Check for macro
                let macroVal = null;
                const resolvedSym = resolvedSymbols.get(first.value);
                if (resolvedSym) {
                    try {
                        macroVal = resolvedSym.env.get(resolvedSym.name);
                    }
                    catch { }
                }
                else {
                    try {
                        macroVal = env.get(first.value);
                    }
                    catch { }
                }
                if (macroVal && macroVal.tag === 'macro') {
                    const expanded = expandMacro(macroVal, expr);
                    return { tag: 'bounce', fn: () => evalK(expanded, env, k) };
                }
                if (macroVal && macroVal.tag === 'syntax-transformer') {
                    // Call the transformer procedure with the whole form
                    return applyK(macroVal.proc, [expr], (expanded) => {
                        return { tag: 'bounce', fn: () => evalK(expanded, env, k) };
                    }, expr.pos);
                }
            }
            // Function application: evaluate operator, then args R-to-L, then apply
            return evalK(first, env, (func) => {
                return evalArgsRtoLK(elems.slice(1), env, (args) => {
                    return applyK(func, args, k, expr.pos);
                });
            });
        }
        default:
            throw errAt('cannot evaluate', expr.pos);
    }
}
// Synchronous evaluate wrapper (for use by .value callbacks from builtins like map)
function evaluate(expr, env) {
    return runBounce(evalK(expr, env, (v) => ({ tag: 'done', value: v })));
}
function display(val) {
    return writeVal(val);
}
/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result. All expressions are chained
 * in a single CPS chain so continuations can span across them.
 */
export function evalStr(input) {
    gensymCounter = 0;
    resolvedSymbols.clear();
    syntaxCaseStack = [];
    windStack = [];
    exceptionHandlers = [];
    const tokens = tokenize(input);
    const exprs = parse(tokens);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const env = makeGlobalEnv();
    const result = runBounce(evalSeqK(exprs, 0, env, (v) => ({ tag: 'done', value: v })));
    return display(result);
}
/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithLimit(input, maxSteps) {
    gensymCounter = 0;
    resolvedSymbols.clear();
    syntaxCaseStack = [];
    windStack = [];
    exceptionHandlers = [];
    const tokens = tokenize(input);
    const exprs = parse(tokens);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const env = makeGlobalEnv();
    const result = runBounce(evalSeqK(exprs, 0, env, (v) => ({ tag: 'done', value: v })), maxSteps);
    return display(result);
}
export function evalStrWithOutput(input) {
    gensymCounter = 0;
    resolvedSymbols.clear();
    syntaxCaseStack = [];
    windStack = [];
    exceptionHandlers = [];
    const tokens = tokenize(input);
    const exprs = parse(tokens);
    if (exprs.length === 0)
        throw new EvalError('no expressions');
    const outputBuf = [];
    const env = makeGlobalEnv(outputBuf);
    const result = runBounce(evalSeqK(exprs, 0, env, (v) => ({ tag: 'done', value: v })));
    return { result: displayVal(result), output: outputBuf.join('') };
}
