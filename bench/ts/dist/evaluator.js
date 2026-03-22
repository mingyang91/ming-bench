import { EvalError } from './evalError.js';
function posStr(pos) {
    return pos ? `${pos.line}:${pos.col}` : '?:?';
}
const NIL = { tag: 'nil' };
function makeEnv(parent) {
    return { bindings: new Map(), parent };
}
function envLookup(env, name, p) {
    let cur = env;
    while (cur) {
        const val = cur.bindings.get(name);
        if (val !== undefined)
            return val;
        cur = cur.parent;
    }
    throw new EvalError(`${posStr(p)}: unbound variable: ${name}`);
}
function envDefine(env, name, val) {
    env.bindings.set(name, val);
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
        if (ch === "'") {
            tokens.push({ text: "'", pos: startPos });
            advance();
            continue;
        }
        if (ch === '(' || ch === ')') {
            tokens.push({ text: ch, pos: startPos });
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
        while (i < input.length && !' \t\n\r();"\''.includes(input[i])) {
            atom += input[i];
            advance();
        }
        if (atom.length > 0) {
            tokens.push({ text: atom, pos: startPos });
        }
    }
    return tokens;
}
// --- Parser ---
function parse(tokens) {
    let idx = 0;
    function parseExpr() {
        if (idx >= tokens.length) {
            throw new EvalError('unexpected end of input');
        }
        const tok = tokens[idx];
        if (tok.text === "'") {
            idx++;
            const inner = parseExpr();
            return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, inner], pos: tok.pos };
        }
        if (tok.text === '(') {
            idx++;
            const elements = [];
            while (idx < tokens.length && tokens[idx].text !== ')') {
                elements.push(parseExpr());
            }
            if (idx >= tokens.length) {
                throw new EvalError(`${posStr(tok.pos)}: missing closing parenthesis`);
            }
            idx++;
            return { tag: 'list', elements, pos: tok.pos };
        }
        if (tok.text === ')') {
            throw new EvalError(`${posStr(tok.pos)}: unexpected )`);
        }
        idx++;
        return parseAtom(tok.text, tok.pos);
    }
    function parseAtom(token, p) {
        if (token === '#t')
            return { tag: 'boolean', value: true, pos: p };
        if (token === '#f')
            return { tag: 'boolean', value: false, pos: p };
        if (token.startsWith('"') && token.endsWith('"')) {
            const inner = token.slice(1, -1)
                .replace(/\\n/g, '\n')
                .replace(/\\t/g, '\t')
                .replace(/\\"/g, '"')
                .replace(/\\\\/g, '\\');
            return { tag: 'string', value: inner, pos: p };
        }
        const num = Number(token);
        if (!isNaN(num) && token !== '') {
            return { tag: 'number', value: num, pos: p };
        }
        return { tag: 'symbol', value: token, pos: p };
    }
    const exprs = [];
    while (idx < tokens.length) {
        exprs.push(parseExpr());
    }
    return exprs;
}
// --- Quote: convert AST list to runtime pair chain ---
function quoteDatum(val) {
    if (val.tag === 'list') {
        let result = NIL;
        for (let i = val.elements.length - 1; i >= 0; i--) {
            result = { tag: 'pair', car: quoteDatum(val.elements[i]), cdr: result };
        }
        return result;
    }
    return val;
}
// --- Helpers ---
function listToArray(val) {
    const result = [];
    let cur = val;
    while (cur.tag === 'pair') {
        result.push(cur.car);
        cur = cur.cdr;
    }
    if (cur.tag !== 'nil')
        throw new EvalError('not a proper list');
    return result;
}
function arrayToList(arr) {
    let result = NIL;
    for (let i = arr.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: arr[i], cdr: result };
    }
    return result;
}
// --- Evaluator ---
function isTruthy(val) {
    return !(val.tag === 'boolean' && val.value === false);
}
function evalExpr(expr, env) {
    switch (expr.tag) {
        case 'number':
        case 'boolean':
        case 'string':
            return expr;
        case 'symbol':
            return envLookup(env, expr.value, expr.pos);
        case 'list': {
            const elems = expr.elements;
            if (elems.length === 0) {
                throw new EvalError(`${posStr(expr.pos)}: empty application`);
            }
            const head = elems[0];
            if (head.tag === 'symbol') {
                switch (head.value) {
                    case 'quote': {
                        if (elems.length !== 2)
                            throw new EvalError(`${posStr(expr.pos)}: quote: expected 1 argument`);
                        return quoteDatum(elems[1]);
                    }
                    case 'if': {
                        if (elems.length < 3 || elems.length > 4)
                            throw new EvalError(`${posStr(expr.pos)}: if: expected 2 or 3 arguments`);
                        const cond = evalExpr(elems[1], env);
                        if (isTruthy(cond)) {
                            return evalExpr(elems[2], env);
                        }
                        else if (elems.length === 4) {
                            return evalExpr(elems[3], env);
                        }
                        return { tag: 'void' };
                    }
                    case 'define': {
                        if (elems.length < 3)
                            throw new EvalError(`${posStr(expr.pos)}: define: expected at least 2 arguments`);
                        const target = elems[1];
                        if (target.tag === 'symbol') {
                            const val = evalExpr(elems[2], env);
                            envDefine(env, target.value, val);
                            return { tag: 'void' };
                        }
                        if (target.tag === 'list' && target.elements.length >= 1 && target.elements[0].tag === 'symbol') {
                            const name = target.elements[0].value;
                            const params = target.elements.slice(1).map(p => {
                                if (p.tag !== 'symbol')
                                    throw new EvalError(`${posStr(expr.pos)}: define: parameter must be a symbol`);
                                return p.value;
                            });
                            const body = elems.slice(2);
                            const lambda = { tag: 'lambda', params, body, env, pos: expr.pos };
                            envDefine(env, name, lambda);
                            return { tag: 'void' };
                        }
                        throw new EvalError(`${posStr(expr.pos)}: define: invalid syntax`);
                    }
                    case 'lambda': {
                        if (elems.length < 3)
                            throw new EvalError(`${posStr(expr.pos)}: lambda: expected at least 2 arguments`);
                        const paramList = elems[1];
                        if (paramList.tag !== 'list')
                            throw new EvalError(`${posStr(expr.pos)}: lambda: parameters must be a list`);
                        const params = paramList.elements.map(p => {
                            if (p.tag !== 'symbol')
                                throw new EvalError(`${posStr(expr.pos)}: lambda: parameter must be a symbol`);
                            return p.value;
                        });
                        const body = elems.slice(2);
                        return { tag: 'lambda', params, body, env, pos: expr.pos };
                    }
                    case 'and':
                        return evalAnd(elems.slice(1), env);
                    case 'or':
                        return evalOr(elems.slice(1), env);
                    case 'not': {
                        if (elems.length !== 2)
                            throw new EvalError(`${posStr(expr.pos)}: not: expected 1 argument`);
                        const val = evalExpr(elems[1], env);
                        return { tag: 'boolean', value: !isTruthy(val) };
                    }
                    case 'let':
                        return evalLet(elems, env);
                    case 'begin':
                        return evalBegin(elems.slice(1), env);
                    case 'cond':
                        return evalCond(elems.slice(1), env);
                }
            }
            // Procedure application
            const proc = evalExpr(head, env);
            if (proc.tag === 'builtin') {
                const args = elems.slice(1).map(e => evalExpr(e, env));
                try {
                    return proc.fn(args);
                }
                catch (e) {
                    if (e instanceof EvalError && !/^\d/.test(e.message)) {
                        throw new EvalError(`${posStr(expr.pos)}: ${e.message}`);
                    }
                    throw e;
                }
            }
            const args = elems.slice(1).map(e => evalExpr(e, env));
            return applyProc(proc, args, expr.pos);
        }
        default:
            return expr;
    }
}
function applyProc(proc, args, callPos) {
    if (proc.tag === 'lambda') {
        if (args.length !== proc.params.length) {
            throw new EvalError(`${posStr(callPos)}: expected ${proc.params.length} arguments, got ${args.length}`);
        }
        const callEnv = makeEnv(proc.env);
        for (let i = 0; i < proc.params.length; i++) {
            envDefine(callEnv, proc.params[i], args[i]);
        }
        let result = { tag: 'void' };
        for (const bodyExpr of proc.body) {
            result = evalExpr(bodyExpr, callEnv);
        }
        return result;
    }
    if (proc.tag === 'builtin') {
        return proc.fn(args);
    }
    throw new EvalError(`${posStr(callPos)}: not a procedure`);
}
// --- Special forms ---
function evalAnd(exprs, env) {
    let result = { tag: 'boolean', value: true };
    for (const expr of exprs) {
        result = evalExpr(expr, env);
        if (!isTruthy(result))
            return result;
    }
    return result;
}
function evalOr(exprs, env) {
    let result = { tag: 'boolean', value: false };
    for (const expr of exprs) {
        result = evalExpr(expr, env);
        if (isTruthy(result))
            return result;
    }
    return result;
}
function evalLet(elems, env) {
    // (let bindings body...) or (let name bindings body...) for named let
    let idx = 1;
    let loopName = null;
    const first = elems[idx];
    if (first.tag === 'symbol') {
        loopName = first.value;
        idx++;
    }
    const bindingList = elems[idx];
    if (bindingList.tag !== 'list')
        throw new EvalError('let: bindings must be a list');
    idx++;
    const body = elems.slice(idx);
    const paramNames = [];
    const initVals = [];
    for (const b of bindingList.elements) {
        if (b.tag !== 'list' || b.elements.length !== 2)
            throw new EvalError('let: invalid binding');
        if (b.elements[0].tag !== 'symbol')
            throw new EvalError('let: binding name must be a symbol');
        paramNames.push(b.elements[0].value);
        initVals.push(evalExpr(b.elements[1], env));
    }
    const letEnv = makeEnv(env);
    if (loopName) {
        // Named let: create a lambda and bind it
        const lambda = { tag: 'lambda', params: paramNames, body, env: letEnv };
        envDefine(letEnv, loopName, lambda);
    }
    for (let i = 0; i < paramNames.length; i++) {
        envDefine(letEnv, paramNames[i], initVals[i]);
    }
    let result = { tag: 'void' };
    for (const bodyExpr of body) {
        result = evalExpr(bodyExpr, letEnv);
    }
    return result;
}
function evalBegin(exprs, env) {
    let result = { tag: 'void' };
    for (const expr of exprs) {
        result = evalExpr(expr, env);
    }
    return result;
}
function evalCond(clauses, env) {
    for (const clause of clauses) {
        if (clause.tag !== 'list' || clause.elements.length < 2)
            throw new EvalError('cond: invalid clause');
        const test = clause.elements[0];
        if (test.tag === 'symbol' && test.value === 'else') {
            let result = { tag: 'void' };
            for (let i = 1; i < clause.elements.length; i++) {
                result = evalExpr(clause.elements[i], env);
            }
            return result;
        }
        const testVal = evalExpr(test, env);
        if (isTruthy(testVal)) {
            let result = testVal;
            for (let i = 1; i < clause.elements.length; i++) {
                result = evalExpr(clause.elements[i], env);
            }
            return result;
        }
    }
    return { tag: 'void' };
}
// --- Arithmetic & Comparison ---
function requireNumbers(args, name) {
    return args.map(a => {
        if (a.tag !== 'number')
            throw new EvalError(`${name}: expected number`);
        return a.value;
    });
}
function arith(args, op) {
    const nums = requireNumbers(args, op);
    if (nums.length === 0) {
        if (op === '+')
            return { tag: 'number', value: 0 };
        if (op === '*')
            return { tag: 'number', value: 1 };
        throw new EvalError(`${op}: expected at least 1 argument`);
    }
    if (op === '-' && nums.length === 1) {
        return { tag: 'number', value: -nums[0] };
    }
    let result = nums[0];
    for (let i = 1; i < nums.length; i++) {
        switch (op) {
            case '+':
                result += nums[i];
                break;
            case '-':
                result -= nums[i];
                break;
            case '*':
                result *= nums[i];
                break;
            case '/':
                if (nums[i] === 0)
                    throw new EvalError('division by zero');
                result = Math.trunc(result / nums[i]);
                break;
        }
    }
    return { tag: 'number', value: result };
}
function schemeCompare(args, op) {
    if (args.length !== 2)
        throw new EvalError(`${op}: expected 2 arguments`);
    const nums = requireNumbers(args, op);
    let result;
    switch (op) {
        case '<':
            result = nums[0] < nums[1];
            break;
        case '>':
            result = nums[0] > nums[1];
            break;
        case '=':
            result = nums[0] === nums[1];
            break;
        case '<=':
            result = nums[0] <= nums[1];
            break;
        default: result = false;
    }
    return { tag: 'boolean', value: result };
}
// --- Builtins ---
function schemeAppend(args) {
    if (args.length === 0)
        return NIL;
    if (args.length === 1)
        return args[0];
    // append all lists
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
        const elems = listToArray(args[i]);
        for (let j = elems.length - 1; j >= 0; j--) {
            result = { tag: 'pair', car: elems[j], cdr: result };
        }
    }
    return result;
}
function makeGlobalEnv() {
    const env = makeEnv(null);
    function defBuiltin(name, fn) {
        env.bindings.set(name, { tag: 'builtin', fn });
    }
    defBuiltin('+', args => arith(args, '+'));
    defBuiltin('-', args => arith(args, '-'));
    defBuiltin('*', args => arith(args, '*'));
    defBuiltin('/', args => arith(args, '/'));
    defBuiltin('<', args => schemeCompare(args, '<'));
    defBuiltin('>', args => schemeCompare(args, '>'));
    defBuiltin('=', args => schemeCompare(args, '='));
    defBuiltin('<=', args => schemeCompare(args, '<='));
    defBuiltin('cons', args => {
        if (args.length !== 2)
            throw new EvalError('cons: expected 2 arguments');
        return { tag: 'pair', car: args[0], cdr: args[1] };
    });
    defBuiltin('car', args => {
        if (args.length !== 1)
            throw new EvalError('car: expected 1 argument');
        if (args[0].tag !== 'pair')
            throw new EvalError('car: expected pair');
        return args[0].car;
    });
    defBuiltin('cdr', args => {
        if (args.length !== 1)
            throw new EvalError('cdr: expected 1 argument');
        if (args[0].tag !== 'pair')
            throw new EvalError('cdr: expected pair');
        return args[0].cdr;
    });
    defBuiltin('null?', args => {
        if (args.length !== 1)
            throw new EvalError('null?: expected 1 argument');
        return { tag: 'boolean', value: args[0].tag === 'nil' };
    });
    defBuiltin('list', args => arrayToList(args));
    defBuiltin('length', args => {
        if (args.length !== 1)
            throw new EvalError('length: expected 1 argument');
        const elems = listToArray(args[0]);
        return { tag: 'number', value: elems.length };
    });
    defBuiltin('append', args => schemeAppend(args));
    // Type predicates
    defBuiltin('string?', args => {
        if (args.length !== 1)
            throw new EvalError('string?: expected 1 argument');
        return { tag: 'boolean', value: args[0].tag === 'string' };
    });
    defBuiltin('number?', args => {
        if (args.length !== 1)
            throw new EvalError('number?: expected 1 argument');
        return { tag: 'boolean', value: args[0].tag === 'number' };
    });
    defBuiltin('boolean?', args => {
        if (args.length !== 1)
            throw new EvalError('boolean?: expected 1 argument');
        return { tag: 'boolean', value: args[0].tag === 'boolean' };
    });
    defBuiltin('pair?', args => {
        if (args.length !== 1)
            throw new EvalError('pair?: expected 1 argument');
        return { tag: 'boolean', value: args[0].tag === 'pair' };
    });
    defBuiltin('symbol?', args => {
        if (args.length !== 1)
            throw new EvalError('symbol?: expected 1 argument');
        return { tag: 'boolean', value: args[0].tag === 'symbol' };
    });
    return env;
}
// --- Display ---
function displayVal(val) {
    switch (val.tag) {
        case 'number': return String(val.value);
        case 'boolean': return val.value ? '#t' : '#f';
        case 'string': return `"${val.value}"`;
        case 'symbol': return val.value;
        case 'nil': return '()';
        case 'pair': {
            let s = '(';
            let cur = val;
            let first = true;
            while (cur.tag === 'pair') {
                if (!first)
                    s += ' ';
                s += displayVal(cur.car);
                cur = cur.cdr;
                first = false;
            }
            if (cur.tag !== 'nil') {
                s += ' . ' + displayVal(cur);
            }
            s += ')';
            return s;
        }
        case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
        case 'void': return '#<void>';
        case 'lambda': return '#<procedure>';
        default: return '#<builtin>';
    }
}
// --- Public API ---
export function evalStr(input) {
    const tokens = tokenize(input);
    const exprs = parse(tokens);
    if (exprs.length === 0) {
        throw new EvalError('no expressions');
    }
    const env = makeGlobalEnv();
    let result;
    for (const expr of exprs) {
        result = evalExpr(expr, env);
    }
    return displayVal(result);
}
export function evalStrWithOutput(input) {
    return { result: evalStr(input), output: '' };
}
