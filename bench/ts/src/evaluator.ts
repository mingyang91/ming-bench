import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeValBase =
  | { tag: 'number'; val: number }
  | { tag: 'boolean'; val: boolean }
  | { tag: 'string'; val: string }
  | { tag: 'symbol'; val: string }
  | { tag: 'char'; val: string }
  | { tag: 'list'; val: SchemeVal[] }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'nil' }
  | { tag: 'procedure'; val: (args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' };

type SchemeVal = SchemeValBase & { pos?: string };

type Token = { text: string; pos: string };

const NIL: SchemeVal = { tag: 'nil' };

function arrayToList(arr: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: arr[i], cdr: result };
  }
  return result;
}

function quoteSyntax(val: SchemeVal): SchemeVal {
  if (val.tag === 'list') {
    return arrayToList(val.val.map(quoteSyntax));
  }
  return val;
}

// ── Parser ─────────────────────────────────────────────────────────────

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  function advance(): void {
    if (input[i] === '\n') { line++; col = 1; } else { col++; }
    i++;
  }

  while (i < input.length) {
    const ch = input[i];
    // whitespace
    if (/\s/.test(ch)) { advance(); continue; }
    // comment
    if (ch === ';') { while (i < input.length && input[i] !== '\n') advance(); continue; }
    const startPos = `${line}:${col}`;
    // quote shorthand
    if (ch === "'") { tokens.push({ text: "'", pos: startPos }); advance(); continue; }
    // parens
    if (ch === '(' || ch === ')') { tokens.push({ text: ch, pos: startPos }); advance(); continue; }
    // string literal
    if (ch === '"') {
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; advance(); }
        s += input[i]; advance();
      }
      if (i < input.length) { s += '"'; advance(); }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    // #t, #f, #\char
    if (ch === '#' && i + 1 < input.length) {
      if (input[i + 1] === 't') { tokens.push({ text: '#t', pos: startPos }); advance(); advance(); continue; }
      if (input[i + 1] === 'f') { tokens.push({ text: '#f', pos: startPos }); advance(); advance(); continue; }
      if (input[i + 1] === '\\') {
        advance(); advance(); // skip # and backslash
        let charName = '';
        while (i < input.length && !/[\s()";]/.test(input[i])) { charName += input[i]; advance(); }
        tokens.push({ text: '#\\' + charName, pos: startPos });
        continue;
      }
    }
    // atom
    let atom = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      atom += input[i]; advance();
    }
    tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parse(tokens: Token[], pos: { i: number }): SchemeVal {
  if (pos.i >= tokens.length) throw new EvalError('unexpected end of input');
  const tok = tokens[pos.i++];

  if (tok.text === "'") {
    const quoted = parse(tokens, pos);
    return { tag: 'list', val: [{ tag: 'symbol', val: 'quote', pos: tok.pos }, quoted], pos: tok.pos };
  }
  if (tok.text === '(') {
    const elems: SchemeVal[] = [];
    while (pos.i < tokens.length && tokens[pos.i].text !== ')') {
      elems.push(parse(tokens, pos));
    }
    if (pos.i >= tokens.length) throw new EvalError(`${tok.pos}: missing closing paren`);
    pos.i++; // skip ')'
    return { tag: 'list', val: elems, pos: tok.pos };
  }
  if (tok.text === ')') throw new EvalError(`${tok.pos}: unexpected )`);
  return parseAtom(tok);
}

function parseAtom(tok: Token): SchemeVal {
  if (tok.text === '#t') return { tag: 'boolean', val: true, pos: tok.pos };
  if (tok.text === '#f') return { tag: 'boolean', val: false, pos: tok.pos };
  if (tok.text.startsWith('#\\')) {
    const name = tok.text.slice(2);
    if (name === 'space') return { tag: 'char', val: ' ', pos: tok.pos };
    if (name === 'newline') return { tag: 'char', val: '\n', pos: tok.pos };
    if (name === 'tab') return { tag: 'char', val: '\t', pos: tok.pos };
    if (name.length === 1) return { tag: 'char', val: name, pos: tok.pos };
    throw new EvalError(`${tok.pos}: unknown character name: ${name}`);
  }
  if (tok.text.startsWith('"')) return { tag: 'string', val: tok.text.slice(1, -1).replace(/\\"/g, '"').replace(/\\\\/g, '\\'), pos: tok.pos };
  const n = Number(tok.text);
  if (!isNaN(n) && tok.text !== '') return { tag: 'number', val: n, pos: tok.pos };
  return { tag: 'symbol', val: tok.text, pos: tok.pos };
}

function parseAll(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const pos = { i: 0 };
  const exprs: SchemeVal[] = [];
  while (pos.i < tokens.length) {
    exprs.push(parse(tokens, pos));
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────────

type Env = { bindings: Map<string, SchemeVal>; parent: Env | null };

function envLookup(env: Env, name: string, pos?: string): SchemeVal {
  let cur: Env | null = env;
  while (cur) {
    const val = cur.bindings.get(name);
    if (val !== undefined) return val;
    cur = cur.parent;
  }
  throw new EvalError(`${pos ?? '?'}: unbound variable: ${name}`);
}

function envSet(env: Env, name: string, val: SchemeVal): void {
  env.bindings.set(name, val);
}

function makeEnv(parent: Env | null): Env {
  return { bindings: new Map(), parent };
}

function makeGlobalEnv(outputBuf?: string[]): Env {
  const env = makeEnv(null);

  const numBinop = (fn: (a: number, b: number) => number | boolean) =>
    ({ tag: 'procedure' as const, val: (args: SchemeVal[]) => {
      for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
      const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
      const r = nums.reduce((acc, v) => fn(acc, v) as number);
      return typeof r === 'number' ? { tag: 'number' as const, val: r } : { tag: 'boolean' as const, val: r as boolean };
    }});

  // Arithmetic
  envSet(env, '+', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    return { tag: 'number', val: nums.reduce((a, b) => a + b, 0) };
  }});

  envSet(env, '*', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    return { tag: 'number', val: nums.reduce((a, b) => a * b, 1) };
  }});

  envSet(env, '-', { tag: 'procedure', val: (args) => {
    if (args.length === 0) throw new EvalError('- requires at least 1 argument');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    if (nums.length === 1) return { tag: 'number', val: -nums[0] };
    return { tag: 'number', val: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  }});

  envSet(env, '/', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('/ requires at least 2 arguments');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    return { tag: 'number', val: nums.slice(1).reduce((a, b) => {
      if (b === 0) throw new EvalError('division by zero');
      return Math.trunc(a / b);
    }, nums[0]) };
  }});

  // Comparisons
  const numCmp = (cmp: (a: number, b: number) => boolean) =>
    ({ tag: 'procedure' as const, val: (args: SchemeVal[]) => {
      if (args.length < 2) throw new EvalError('comparison requires at least 2 arguments');
      for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
      const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
      for (let i = 0; i < nums.length - 1; i++) {
        if (!cmp(nums[i], nums[i + 1])) return { tag: 'boolean' as const, val: false };
      }
      return { tag: 'boolean' as const, val: true };
    }});

  envSet(env, '<', numCmp((a, b) => a < b));
  envSet(env, '>', numCmp((a, b) => a > b));
  envSet(env, '=', numCmp((a, b) => a === b));
  envSet(env, '<=', numCmp((a, b) => a <= b));
  envSet(env, '>=', numCmp((a, b) => a >= b));

  // not
  envSet(env, 'not', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('not requires 1 argument');
    return { tag: 'boolean', val: isFalsy(args[0]) };
  }});

  // List operations
  envSet(env, 'cons', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('cons requires 2 arguments');
    return { tag: 'pair', car: args[0], cdr: args[1] };
  }});

  envSet(env, 'car', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('car requires 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('car: not a pair');
    return args[0].car;
  }});

  envSet(env, 'cdr', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('cdr requires 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('cdr: not a pair');
    return args[0].cdr;
  }});

  envSet(env, 'null?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('null? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'nil' };
  }});

  envSet(env, 'list', { tag: 'procedure', val: (args) => {
    return arrayToList(args);
  }});

  envSet(env, 'append', { tag: 'procedure', val: (args) => {
    if (args.length === 0) return NIL;
    if (args.length === 1) return args[0];
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
      const items: SchemeVal[] = [];
      let cur = args[i];
      while (cur.tag === 'pair') { items.push(cur.car); cur = cur.cdr; }
      for (let j = items.length - 1; j >= 0; j--) {
        result = { tag: 'pair', car: items[j], cdr: result };
      }
    }
    return result;
  }});

  envSet(env, 'map', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('map requires at least 2 arguments');
    const fn = args[0];
    if (fn.tag !== 'procedure') throw new EvalError('map: first argument must be a procedure');
    const items: SchemeVal[] = [];
    let cur = args[1];
    while (cur.tag === 'pair') { items.push(fn.val([cur.car])); cur = cur.cdr; }
    return arrayToList(items);
  }});

  envSet(env, 'length', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('length requires 1 argument');
    let cur = args[0];
    let len = 0;
    while (cur.tag === 'pair') { len++; cur = cur.cdr; }
    if (cur.tag !== 'nil') throw new EvalError('length: not a proper list');
    return { tag: 'number', val: len };
  }});

  // Type predicates
  envSet(env, 'number?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('number? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'number' };
  }});

  envSet(env, 'string?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('string? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'string' };
  }});

  envSet(env, 'boolean?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('boolean? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'boolean' };
  }});

  envSet(env, 'pair?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('pair? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'pair' };
  }});

  envSet(env, 'symbol?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('symbol? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'symbol' };
  }});

  // I/O
  const displayVal = (v: SchemeVal): string => {
    switch (v.tag) {
      case 'number': return String(v.val);
      case 'boolean': return v.val ? '#t' : '#f';
      case 'string': return v.val;
      case 'symbol': return v.val;
      case 'char': return v.val;
      case 'nil': return '()';
      case 'void': return '';
      case 'procedure': return '#<procedure>';
      case 'pair': {
        let out = '(' + displayVal(v.car);
        let cur: SchemeVal = v.cdr;
        while (cur.tag === 'pair') { out += ' ' + displayVal(cur.car); cur = cur.cdr; }
        if (cur.tag !== 'nil') out += ' . ' + displayVal(cur);
        return out + ')';
      }
      case 'list': return `(${v.val.map(displayVal).join(' ')})`;
    }
  };

  const writeVal = (v: SchemeVal): string => {
    if (v.tag === 'string') return `"${v.val}"`;
    if (v.tag === 'char') return `#\\${v.val}`;
    return displayVal(v);
  };

  envSet(env, 'display', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('display requires 1 argument');
    if (outputBuf) outputBuf.push(displayVal(args[0]));
    return { tag: 'void' };
  }});

  envSet(env, 'write', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('write requires 1 argument');
    if (outputBuf) outputBuf.push(writeVal(args[0]));
    return { tag: 'void' };
  }});

  envSet(env, 'newline', { tag: 'procedure', val: (args) => {
    if (outputBuf) outputBuf.push('\n');
    return { tag: 'void' };
  }});

  // String operations
  envSet(env, 'string-length', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-length: expected string');
    return { tag: 'number', val: args[0].val.length };
  }});

  envSet(env, 'string-append', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'string') throw new EvalError('string-append: expected string');
    return { tag: 'string', val: args.map(a => (a as { tag: 'string'; val: string }).val).join('') };
  }});

  envSet(env, 'substring', { tag: 'procedure', val: (args) => {
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'number')
      throw new EvalError('substring: expected string, start, end');
    return { tag: 'string', val: args[0].val.substring(args[1].val, args[2].val) };
  }});

  envSet(env, 'string->number', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->number: expected string');
    const n = Number(args[0].val);
    if (isNaN(n)) return { tag: 'boolean', val: false };
    return { tag: 'number', val: n };
  }});

  envSet(env, 'number->string', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('number->string: expected number');
    return { tag: 'string', val: String(args[0].val) };
  }});

  envSet(env, 'string-ref', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'number')
      throw new EvalError('string-ref: expected string and index');
    const s = args[0].val;
    const i = args[1].val;
    if (i < 0 || i >= s.length) throw new EvalError('string-ref: index out of range');
    return { tag: 'char', val: s[i] };
  }});

  envSet(env, 'string-copy', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-copy: expected string');
    return { tag: 'string', val: args[0].val };
  }});

  envSet(env, 'string-set!', { tag: 'procedure', val: (args) => {
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'char')
      throw new EvalError('string-set!: expected string, index, char');
    const s = args[0];
    const i = args[1].val;
    if (i < 0 || i >= s.val.length) throw new EvalError('string-set!: index out of range');
    s.val = s.val.substring(0, i) + args[2].val + s.val.substring(i + 1);
    return { tag: 'void' };
  }});

  envSet(env, 'char?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('char? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'char' };
  }});

  // Symbol/string conversions
  envSet(env, 'symbol->string', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'symbol') throw new EvalError('symbol->string: expected symbol');
    return { tag: 'string', val: args[0].val };
  }});

  envSet(env, 'string->symbol', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->symbol: expected string');
    return { tag: 'symbol', val: args[0].val };
  }});

  return env;
}

function isFalsy(v: SchemeVal): boolean {
  return v.tag === 'boolean' && v.val === false;
}

function isTruthy(v: SchemeVal): boolean {
  return !isFalsy(v);
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
    case 'nil':
    case 'pair':
      return expr;

    case 'symbol':
      return envLookup(env, expr.val, expr.pos);

    case 'list': {
      const elems = expr.val;
      const epos = expr.pos ?? '?';
      if (elems.length === 0) throw new EvalError(`${epos}: empty application`);

      // Special forms
      if (elems[0].tag === 'symbol') {
        const name = elems[0].val;

        if (name === 'quote') {
          if (elems.length !== 2) throw new EvalError(`${epos}: quote requires 1 argument`);
          return quoteSyntax(elems[1]);
        }

        if (name === 'begin') {
          let result: SchemeVal = { tag: 'void' };
          for (let i = 1; i < elems.length; i++) {
            result = evaluate(elems[i], env);
          }
          return result;
        }

        if (name === 'let') {
          // Named let: (let name ((var init) ...) body...)
          if (elems.length >= 4 && elems[1].tag === 'symbol') {
            const loopName = elems[1].val;
            const bindings = elems[2];
            if (bindings.tag !== 'list') throw new EvalError(`${epos}: let bindings must be a list`);
            const params: string[] = [];
            const inits: SchemeVal[] = [];
            for (const b of bindings.val) {
              if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
                throw new EvalError(`${epos}: invalid let binding`);
              params.push(b.val[0].val);
              inits.push(evaluate(b.val[1], env));
            }
            const body = elems.slice(3);
            const local = makeEnv(env);
            const loopProc: SchemeVal = { tag: 'procedure', val: (args: SchemeVal[]) => {
              if (args.length !== params.length) throw new EvalError(`${epos}: expected ${params.length} args, got ${args.length}`);
              const inner = makeEnv(local);
              for (let i = 0; i < params.length; i++) envSet(inner, params[i], args[i]);
              let result: SchemeVal = { tag: 'void' };
              for (const expr of body) result = evaluate(expr, inner);
              return result;
            }};
            envSet(local, loopName, loopProc);
            return loopProc.val(inits);
          }
          // Regular let
          if (elems.length < 3) throw new EvalError(`${epos}: let requires bindings and body`);
          const bindings = elems[1];
          if (bindings.tag !== 'list') throw new EvalError(`${epos}: let bindings must be a list`);
          const local = makeEnv(env);
          for (const b of bindings.val) {
            if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
              throw new EvalError(`${epos}: invalid let binding`);
            envSet(local, b.val[0].val, evaluate(b.val[1], env));
          }
          let result: SchemeVal = { tag: 'void' };
          for (let i = 2; i < elems.length; i++) {
            result = evaluate(elems[i], local);
          }
          return result;
        }

        if (name === 'cond') {
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.val.length < 2) throw new EvalError(`${epos}: invalid cond clause`);
            if (clause.val[0].tag === 'symbol' && clause.val[0].val === 'else') {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.val.length; j++) result = evaluate(clause.val[j], env);
              return result;
            }
            const test = evaluate(clause.val[0], env);
            if (isTruthy(test)) {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.val.length; j++) result = evaluate(clause.val[j], env);
              return result;
            }
          }
          return { tag: 'void' };
        }

        if (name === 'if') {
          if (elems.length < 3 || elems.length > 4) throw new EvalError(`${epos}: if requires 2 or 3 arguments`);
          const cond = evaluate(elems[1], env);
          if (isTruthy(cond)) return evaluate(elems[2], env);
          if (elems.length === 4) return evaluate(elems[3], env);
          return { tag: 'void' };
        }

        if (name === 'define') {
          if (elems.length < 3) throw new EvalError(`${epos}: define requires at least 2 arguments`);
          if (elems[1].tag === 'symbol') {
            // (define x expr)
            const val = evaluate(elems[2], env);
            envSet(env, elems[1].val, val);
            return { tag: 'void' };
          }
          if (elems[1].tag === 'list' && elems[1].val.length > 0 && elems[1].val[0].tag === 'symbol') {
            // (define (f params...) body...)
            const fname = elems[1].val[0].val;
            const params = elems[1].val.slice(1).map(p => {
              if (p.tag !== 'symbol') throw new EvalError(`${epos}: parameter must be a symbol`);
              return p.val;
            });
            const body = elems.slice(2);
            const closure: SchemeVal = { tag: 'procedure', val: (args: SchemeVal[]) => {
              if (args.length !== params.length) throw new EvalError(`${epos}: expected ${params.length} args, got ${args.length}`);
              const local = makeEnv(env);
              for (let i = 0; i < params.length; i++) envSet(local, params[i], args[i]);
              let result: SchemeVal = { tag: 'void' };
              for (const expr of body) result = evaluate(expr, local);
              return result;
            }};
            envSet(env, fname, closure);
            return { tag: 'void' };
          }
          throw new EvalError(`${epos}: invalid define`);
        }

        if (name === 'lambda') {
          if (elems.length < 3) throw new EvalError(`${epos}: lambda requires params and body`);
          const paramList = elems[1];
          if (paramList.tag !== 'list') throw new EvalError(`${epos}: lambda params must be a list`);
          const params = paramList.val.map(p => {
            if (p.tag !== 'symbol') throw new EvalError(`${epos}: parameter must be a symbol`);
            return p.val;
          });
          const body = elems.slice(2);
          const closedEnv = env;
          return { tag: 'procedure', val: (args: SchemeVal[]) => {
            if (args.length !== params.length) throw new EvalError(`${epos}: expected ${params.length} args, got ${args.length}`);
            const local = makeEnv(closedEnv);
            for (let i = 0; i < params.length; i++) envSet(local, params[i], args[i]);
            let result: SchemeVal = { tag: 'void' };
            for (const expr of body) result = evaluate(expr, local);
            return result;
          }};
        }

        if (name === 'and') {
          if (elems.length === 1) return { tag: 'boolean', val: true };
          let result: SchemeVal = { tag: 'boolean', val: true };
          for (let i = 1; i < elems.length; i++) {
            result = evaluate(elems[i], env);
            if (isFalsy(result)) return result;
          }
          return result;
        }

        if (name === 'or') {
          if (elems.length === 1) return { tag: 'boolean', val: false };
          let result: SchemeVal = { tag: 'boolean', val: false };
          for (let i = 1; i < elems.length; i++) {
            result = evaluate(elems[i], env);
            if (isTruthy(result)) return result;
          }
          return result;
        }
      }

      // Procedure application
      const proc = evaluate(elems[0], env);
      if (proc.tag !== 'procedure') throw new EvalError(`${epos}: not a procedure`);
      const args = elems.slice(1).map(a => evaluate(a, env));
      try {
        return proc.val(args);
      } catch (e) {
        if (e instanceof EvalError && !/^\d/.test(e.message)) {
          throw new EvalError(`${epos}: ${e.message}`);
        }
        throw e;
      }
    }

    default:
      throw new EvalError(`${expr.pos ?? '?'}: cannot evaluate`);
  }
}

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.val);
    case 'boolean': return val.val ? '#t' : '#f';
    case 'string': return `"${val.val}"`;
    case 'symbol': return val.val;
    case 'char': return `#\\${val.val}`;
    case 'list': return `(${val.val.map(display).join(' ')})`;
    case 'nil': return '()';
    case 'pair': {
      let out = '(' + display(val.car);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        out += ' ' + display(cur.car);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') {
        out += ' . ' + display(cur);
      }
      return out + ')';
    }
    case 'void': return '';
    case 'procedure': return '#<procedure>';
  }
}

// ── Public API ─────────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return { result: display(result), output: outputBuf.join('') };
}
