import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeValBase =
  | { tag: 'number'; val: number; exact?: boolean; num?: number; den?: number }
  | { tag: 'boolean'; val: boolean }
  | { tag: 'string'; val: string }
  | { tag: 'symbol'; val: string }
  | { tag: 'char'; val: string }
  | { tag: 'list'; val: SchemeVal[] }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'nil' }
  | { tag: 'procedure'; val: (args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' }
  | { tag: 'macro'; transformer: MacroTransformer };

type MacroTransformer = {
  literals: string[];
  rules: { pattern: SchemeVal[]; template: SchemeVal }[];
  defEnv: Env;
};

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

// ── Rational helpers ──────────────────────────────────────────────────

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { [a, b] = [b, a % b]; }
  return a;
}

function makeRational(num: number, den: number): SchemeVal {
  if (den === 0) throw new EvalError('division by zero');
  if (den < 0) { num = -num; den = -den; }
  if (num === 0) return { tag: 'number', val: 0, exact: true, num: 0, den: 1 };
  const g = gcd(Math.abs(num), den);
  num = num / g; den = den / g;
  return { tag: 'number', val: num / den, exact: true, num, den };
}

function makeExactInt(n: number): SchemeVal {
  return { tag: 'number', val: n, exact: true, num: n, den: 1 };
}

function makeInexact(n: number): SchemeVal {
  return { tag: 'number', val: n, exact: false };
}

function getNum(v: SchemeVal & { tag: 'number' }): number {
  return v.num ?? v.val;
}

function getDen(v: SchemeVal & { tag: 'number' }): number {
  return v.den ?? 1;
}

function isExact(v: SchemeVal): boolean {
  return v.tag === 'number' && v.exact !== false;
}

function floatToRational(x: number): SchemeVal {
  if (Number.isInteger(x)) return makeExactInt(x);
  const sign = x < 0 ? -1 : 1;
  const ax = Math.abs(x);
  for (let den = 1; den <= 1000000; den++) {
    const num = Math.round(ax * den);
    if (Math.abs(num / den - ax) < 1e-12) {
      return makeRational(sign * num, den);
    }
  }
  const den = 1000000000;
  const num = Math.round(ax * den);
  return makeRational(sign * num, den);
}

function numBinOp(
  a: SchemeVal & { tag: 'number' },
  b: SchemeVal & { tag: 'number' },
  op: 'add' | 'sub' | 'mul' | 'div'
): SchemeVal {
  const aExact = a.exact !== false;
  const bExact = b.exact !== false;
  if (aExact && bExact) {
    const an = getNum(a), ad = getDen(a), bn = getNum(b), bd = getDen(b);
    switch (op) {
      case 'add': return makeRational(an * bd + bn * ad, ad * bd);
      case 'sub': return makeRational(an * bd - bn * ad, ad * bd);
      case 'mul': return makeRational(an * bn, ad * bd);
      case 'div':
        if (bn === 0) throw new EvalError('division by zero');
        return makeRational(an * bd, ad * bn);
    }
  }
  switch (op) {
    case 'add': return makeInexact(a.val + b.val);
    case 'sub': return makeInexact(a.val - b.val);
    case 'mul': return makeInexact(a.val * b.val);
    case 'div':
      if (b.val === 0) throw new EvalError('division by zero');
      return makeInexact(a.val / b.val);
  }
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
  // rational literal: e.g. 1/3, -5/2
  const ratMatch = /^(-?\d+)\/(\d+)$/.exec(tok.text);
  if (ratMatch) {
    const num = parseInt(ratMatch[1], 10);
    const den = parseInt(ratMatch[2], 10);
    const r = makeRational(num, den);
    return { ...r, pos: tok.pos };
  }
  const n = Number(tok.text);
  if (!isNaN(n) && tok.text !== '') {
    if (tok.text.includes('.') || tok.text.includes('e') || tok.text.includes('E')) {
      return { tag: 'number', val: n, exact: false, pos: tok.pos };
    }
    return { tag: 'number', val: n, exact: true, num: n, den: 1, pos: tok.pos };
  }
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

function envMutate(env: Env, name: string, val: SchemeVal, pos?: string): void {
  let cur: Env | null = env;
  while (cur) {
    if (cur.bindings.has(name)) {
      cur.bindings.set(name, val);
      return;
    }
    cur = cur.parent;
  }
  throw new EvalError(`${pos ?? '?'}: unbound variable: ${name}`);
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
    if (args.length === 0) return makeExactInt(0);
    let result = args[0] as SchemeVal & { tag: 'number' };
    for (let i = 1; i < args.length; i++) {
      result = numBinOp(result, args[i] as SchemeVal & { tag: 'number' }, 'add') as SchemeVal & { tag: 'number' };
    }
    return result;
  }});

  envSet(env, '*', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    if (args.length === 0) return makeExactInt(1);
    let result = args[0] as SchemeVal & { tag: 'number' };
    for (let i = 1; i < args.length; i++) {
      result = numBinOp(result, args[i] as SchemeVal & { tag: 'number' }, 'mul') as SchemeVal & { tag: 'number' };
    }
    return result;
  }});

  envSet(env, '-', { tag: 'procedure', val: (args) => {
    if (args.length === 0) throw new EvalError('- requires at least 1 argument');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    if (args.length === 1) {
      const a = args[0] as SchemeVal & { tag: 'number' };
      if (a.exact !== false) return makeRational(-getNum(a), getDen(a));
      return makeInexact(-a.val);
    }
    let result = args[0] as SchemeVal & { tag: 'number' };
    for (let i = 1; i < args.length; i++) {
      result = numBinOp(result, args[i] as SchemeVal & { tag: 'number' }, 'sub') as SchemeVal & { tag: 'number' };
    }
    return result;
  }});

  envSet(env, '/', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('/ requires at least 2 arguments');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    let result = args[0] as SchemeVal & { tag: 'number' };
    for (let i = 1; i < args.length; i++) {
      result = numBinOp(result, args[i] as SchemeVal & { tag: 'number' }, 'div') as SchemeVal & { tag: 'number' };
    }
    return result;
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
    const lists = args.slice(1);
    const results: SchemeVal[] = [];
    const cursors = lists.map(l => l);
    while (true) {
      const fnArgs: SchemeVal[] = [];
      let done = false;
      for (let i = 0; i < cursors.length; i++) {
        if (cursors[i].tag !== 'pair') { done = true; break; }
        fnArgs.push((cursors[i] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).car);
      }
      if (done) break;
      results.push(fn.val(fnArgs));
      for (let i = 0; i < cursors.length; i++) {
        cursors[i] = (cursors[i] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).cdr;
      }
    }
    return arrayToList(results);
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
      case 'number': return displayNumber(v);
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
      case 'macro': return '#<macro>';
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

  // eq? and equal?
  envSet(env, 'eq?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('eq? requires 2 arguments');
    const a = args[0], b = args[1];
    if (a.tag !== b.tag) return { tag: 'boolean', val: false };
    if (a.tag === 'nil') return { tag: 'boolean', val: true };
    if (a.tag === 'boolean') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    if (a.tag === 'number') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    if (a.tag === 'symbol') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    if (a.tag === 'char') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    return { tag: 'boolean', val: a === b };
  }});

  const schemeEqual = (a: SchemeVal, b: SchemeVal): boolean => {
    if (a.tag !== b.tag) return false;
    if (a.tag === 'nil') return true;
    if (a.tag === 'boolean') return a.val === (b as typeof a).val;
    if (a.tag === 'number') return a.val === (b as typeof a).val;
    if (a.tag === 'string') return a.val === (b as typeof a).val;
    if (a.tag === 'symbol') return a.val === (b as typeof a).val;
    if (a.tag === 'char') return a.val === (b as typeof a).val;
    if (a.tag === 'pair' && b.tag === 'pair') return schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
    return false;
  };

  envSet(env, 'equal?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('equal? requires 2 arguments');
    return { tag: 'boolean', val: schemeEqual(args[0], args[1]) };
  }});

  // Numeric utilities
  envSet(env, 'abs', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('abs: expected number');
    return { tag: 'number', val: Math.abs(args[0].val) };
  }});

  envSet(env, 'modulo', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number') throw new EvalError('modulo: expected 2 numbers');
    const a = args[0].val, b = args[1].val;
    if (b === 0) throw new EvalError('modulo: division by zero');
    return { tag: 'number', val: a - b * Math.floor(a / b) };
  }});

  envSet(env, 'remainder', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number') throw new EvalError('remainder: expected 2 numbers');
    const a = args[0].val, b = args[1].val;
    if (b === 0) throw new EvalError('remainder: division by zero');
    return { tag: 'number', val: a % b };
  }});

  envSet(env, 'quotient', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number') throw new EvalError('quotient: expected 2 numbers');
    const b = args[1].val;
    if (b === 0) throw new EvalError('quotient: division by zero');
    return { tag: 'number', val: Math.trunc(args[0].val / b) };
  }});

  envSet(env, 'min', { tag: 'procedure', val: (args) => {
    if (args.length < 1) throw new EvalError('min: expected at least 1 argument');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('min: expected number');
    return { tag: 'number', val: Math.min(...args.map(a => (a as { tag: 'number'; val: number }).val)) };
  }});

  envSet(env, 'max', { tag: 'procedure', val: (args) => {
    if (args.length < 1) throw new EvalError('max: expected at least 1 argument');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('max: expected number');
    return { tag: 'number', val: Math.max(...args.map(a => (a as { tag: 'number'; val: number }).val)) };
  }});

  envSet(env, 'expt', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number') throw new EvalError('expt: expected 2 numbers');
    return { tag: 'number', val: Math.pow(args[0].val, args[1].val) };
  }});

  envSet(env, 'zero?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('zero?: expected number');
    return { tag: 'boolean', val: args[0].val === 0 };
  }});

  envSet(env, 'positive?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('positive?: expected number');
    return { tag: 'boolean', val: args[0].val > 0 };
  }});

  envSet(env, 'negative?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('negative?: expected number');
    return { tag: 'boolean', val: args[0].val < 0 };
  }});

  envSet(env, 'odd?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('odd?: expected number');
    return { tag: 'boolean', val: Math.abs(args[0].val) % 2 === 1 };
  }});

  envSet(env, 'even?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('even?: expected number');
    return { tag: 'boolean', val: args[0].val % 2 === 0 };
  }});

  // Exact/rational predicates and conversions (L11)
  envSet(env, 'exact?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('exact?: expected number');
    return { tag: 'boolean', val: args[0].exact !== false };
  }});

  envSet(env, 'inexact?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('inexact?: expected number');
    return { tag: 'boolean', val: args[0].exact === false };
  }});

  envSet(env, 'integer?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('integer? requires 1 argument');
    if (args[0].tag !== 'number') return { tag: 'boolean', val: false };
    if (args[0].exact !== false) {
      return { tag: 'boolean', val: getDen(args[0] as SchemeVal & { tag: 'number' }) === 1 };
    }
    return { tag: 'boolean', val: Number.isInteger(args[0].val) };
  }});

  envSet(env, 'rational?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('rational? requires 1 argument');
    if (args[0].tag !== 'number') return { tag: 'boolean', val: false };
    return { tag: 'boolean', val: args[0].exact !== false };
  }});

  envSet(env, 'exact->inexact', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('exact->inexact: expected number');
    return makeInexact(args[0].val);
  }});

  envSet(env, 'inexact->exact', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('inexact->exact: expected number');
    if (args[0].exact !== false) return args[0];
    return floatToRational(args[0].val);
  }});

  envSet(env, 'numerator', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('numerator: expected number');
    return makeExactInt(getNum(args[0] as SchemeVal & { tag: 'number' }));
  }});

  envSet(env, 'denominator', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('denominator: expected number');
    return makeExactInt(getDen(args[0] as SchemeVal & { tag: 'number' }));
  }});

  // List utilities
  envSet(env, 'list-ref', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[1].tag !== 'number') throw new EvalError('list-ref: expected list and index');
    let cur = args[0];
    let idx = args[1].val;
    while (idx > 0 && cur.tag === 'pair') { cur = cur.cdr; idx--; }
    if (cur.tag !== 'pair') throw new EvalError('list-ref: index out of range');
    return cur.car;
  }});

  envSet(env, 'list-tail', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[1].tag !== 'number') throw new EvalError('list-tail: expected list and index');
    let cur = args[0];
    let idx = args[1].val;
    while (idx > 0) {
      if (cur.tag !== 'pair') throw new EvalError('list-tail: index out of range');
      cur = cur.cdr; idx--;
    }
    return cur;
  }});

  envSet(env, 'list?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('list? requires 1 argument');
    let cur = args[0];
    while (cur.tag === 'pair') cur = cur.cdr;
    return { tag: 'boolean', val: cur.tag === 'nil' };
  }});

  envSet(env, 'assoc', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('assoc requires 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (cur.car.tag === 'pair' && schemeEqual(cur.car.car, key)) return cur.car;
      cur = cur.cdr;
    }
    return { tag: 'boolean', val: false };
  }});

  // Character operations
  envSet(env, 'char-alphabetic?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-alphabetic?: expected char');
    return { tag: 'boolean', val: /^[a-zA-Z]$/.test(args[0].val) };
  }});

  envSet(env, 'char-numeric?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-numeric?: expected char');
    return { tag: 'boolean', val: /^[0-9]$/.test(args[0].val) };
  }});

  envSet(env, 'char-upcase', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-upcase: expected char');
    return { tag: 'char', val: args[0].val.toUpperCase() };
  }});

  envSet(env, 'char-downcase', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-downcase: expected char');
    return { tag: 'char', val: args[0].val.toLowerCase() };
  }});

  envSet(env, 'char=?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char=?: expected 2 chars');
    return { tag: 'boolean', val: args[0].val === args[1].val };
  }});

  envSet(env, 'char<?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char<?: expected 2 chars');
    return { tag: 'boolean', val: args[0].val < args[1].val };
  }});

  // String comparisons
  envSet(env, 'string=?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string=?: expected 2 strings');
    return { tag: 'boolean', val: args[0].val === args[1].val };
  }});

  envSet(env, 'string<?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string<?: expected 2 strings');
    return { tag: 'boolean', val: args[0].val < args[1].val };
  }});

  envSet(env, 'string-ci=?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string-ci=?: expected 2 strings');
    return { tag: 'boolean', val: args[0].val.toLowerCase() === args[1].val.toLowerCase() };
  }});

  envSet(env, 'string-upcase', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-upcase: expected string');
    return { tag: 'string', val: args[0].val.toUpperCase() };
  }});

  envSet(env, 'string-downcase', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-downcase: expected string');
    return { tag: 'string', val: args[0].val.toLowerCase() };
  }});

  // apply
  envSet(env, 'apply', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('apply requires at least 2 arguments');
    const fn = args[0];
    if (fn.tag !== 'procedure') throw new EvalError('apply: first argument must be a procedure');
    const lastArg = args[args.length - 1];
    const tailArgs = listToArray(lastArg);
    const prefixArgs = args.slice(1, -1);
    return fn.val([...prefixArgs, ...tailArgs]);
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

function listToArray(v: SchemeVal): SchemeVal[] {
  const arr: SchemeVal[] = [];
  let cur = v;
  while (cur.tag === 'pair') { arr.push(cur.car); cur = cur.cdr; }
  return arr;
}

function parseParams(paramList: SchemeVal[], pos: string): { fixed: string[]; rest: string | null } {
  const dotIdx = paramList.findIndex(p => p.tag === 'symbol' && p.val === '.');
  if (dotIdx === -1) {
    return { fixed: paramList.map(p => {
      if (p.tag !== 'symbol') throw new EvalError(`${pos}: parameter must be a symbol`);
      return p.val;
    }), rest: null };
  }
  if (dotIdx !== paramList.length - 2) throw new EvalError(`${pos}: invalid dot notation`);
  const restParam = paramList[paramList.length - 1];
  if (restParam.tag !== 'symbol') throw new EvalError(`${pos}: rest parameter must be a symbol`);
  const fixed = paramList.slice(0, dotIdx).map(p => {
    if (p.tag !== 'symbol') throw new EvalError(`${pos}: parameter must be a symbol`);
    return p.val;
  });
  return { fixed, rest: restParam.val };
}

function makeVariadicClosure(fixed: string[], rest: string, body: SchemeVal[], closedEnv: Env, pos: string): SchemeVal {
  return { tag: 'procedure', val: (args: SchemeVal[]) => {
    if (args.length < fixed.length) throw new EvalError(`${pos}: expected at least ${fixed.length} args, got ${args.length}`);
    const local = makeEnv(closedEnv);
    for (let i = 0; i < fixed.length; i++) envSet(local, fixed[i], args[i]);
    envSet(local, rest, arrayToList(args.slice(fixed.length)));
    let result: SchemeVal = { tag: 'void' };
    for (const expr of body) result = evaluate(expr, local);
    return result;
  }};
}

// ── Macro expansion (syntax-rules) ─────────────────────────────────────

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'set!', 'lambda', 'begin', 'let', 'let*', 'letrec',
  'cond', 'and', 'or', 'define-syntax', 'syntax-rules',
]);

let gensymCounter = 0;
function gensym(base: string): string {
  return `__${base}_${++gensymCounter}`;
}

type Bindings = Map<string, SchemeVal | SchemeVal[]>;

function matchSingle(pattern: SchemeVal, form: SchemeVal, bindings: Bindings, literals: string[]): boolean {
  if (pattern.tag === 'symbol') {
    if (pattern.val === '_') return true;
    if (literals.includes(pattern.val)) {
      return form.tag === 'symbol' && form.val === pattern.val;
    }
    bindings.set(pattern.val, form);
    return true;
  }
  if (pattern.tag === 'list') {
    if (form.tag !== 'list') return false;
    return matchPattern(pattern.val, form.val, bindings, literals);
  }
  if (pattern.tag === 'number' && form.tag === 'number') return pattern.val === form.val;
  if (pattern.tag === 'boolean' && form.tag === 'boolean') return pattern.val === form.val;
  if (pattern.tag === 'string' && form.tag === 'string') return pattern.val === form.val;
  return false;
}

function matchPattern(patternElems: SchemeVal[], formElems: SchemeVal[], bindings: Bindings, literals: string[]): boolean {
  let ellipsisIdx = -1;
  for (let i = 0; i < patternElems.length; i++) {
    const pe = patternElems[i];
    if (pe.tag === 'symbol' && pe.val === '...') {
      ellipsisIdx = i;
      break;
    }
  }

  if (ellipsisIdx === -1) {
    if (formElems.length !== patternElems.length) return false;
    for (let i = 0; i < patternElems.length; i++) {
      if (!matchSingle(patternElems[i], formElems[i], bindings, literals)) return false;
    }
    return true;
  }

  const fixedBefore = ellipsisIdx - 1;
  const afterEllipsis = patternElems.length - ellipsisIdx - 1;
  if (formElems.length < fixedBefore + afterEllipsis) return false;

  for (let i = 0; i < fixedBefore; i++) {
    if (!matchSingle(patternElems[i], formElems[i], bindings, literals)) return false;
  }

  const ellipsisPattern = patternElems[fixedBefore];
  const ellipsisCount = formElems.length - fixedBefore - afterEllipsis;
  if (ellipsisPattern.tag === 'symbol' && !literals.includes(ellipsisPattern.val)) {
    const arr: SchemeVal[] = [];
    for (let i = 0; i < ellipsisCount; i++) arr.push(formElems[fixedBefore + i]);
    bindings.set(ellipsisPattern.val, arr);
  }

  for (let i = 0; i < afterEllipsis; i++) {
    if (!matchSingle(patternElems[ellipsisIdx + 1 + i], formElems[formElems.length - afterEllipsis + i], bindings, literals)) return false;
  }
  return true;
}

function collectFreeSymbols(template: SchemeVal, patternVars: Set<string>, result: Set<string>): void {
  if (template.tag === 'symbol') {
    if (!patternVars.has(template.val) && !SPECIAL_FORMS.has(template.val) && template.val !== '...') {
      result.add(template.val);
    }
  } else if (template.tag === 'list') {
    for (const elem of template.val) collectFreeSymbols(elem, patternVars, result);
  }
}

function findEllipsisVar(template: SchemeVal, bindings: Bindings): string | null {
  if (template.tag === 'symbol') {
    const bound = bindings.get(template.val);
    if (Array.isArray(bound)) return template.val;
    return null;
  }
  if (template.tag === 'list') {
    for (const elem of template.val) {
      const found = findEllipsisVar(elem, bindings);
      if (found) return found;
    }
  }
  return null;
}

function expandTemplate(template: SchemeVal, bindings: Bindings, renames: Map<string, string>): SchemeVal {
  if (template.tag === 'symbol') {
    const name = template.val;
    if (name === '...') return template;
    const bound = bindings.get(name);
    if (bound !== undefined && !Array.isArray(bound)) return bound as SchemeVal;
    const renamed = renames.get(name);
    if (renamed) return { tag: 'symbol', val: renamed };
    return template;
  }
  if (template.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < template.val.length; i++) {
      const elem = template.val[i];
      const next = i + 1 < template.val.length ? template.val[i + 1] : null;
      if (next && next.tag === 'symbol' && next.val === '...') {
        const ellipsisVar = findEllipsisVar(elem, bindings);
        if (ellipsisVar) {
          const values = bindings.get(ellipsisVar) as SchemeVal[];
          for (const val of values) {
            const singleBindings = new Map(bindings);
            singleBindings.set(ellipsisVar, val);
            result.push(expandTemplate(elem, singleBindings, renames));
          }
        }
        i++; // skip ...
        continue;
      }
      result.push(expandTemplate(elem, bindings, renames));
    }
    return { tag: 'list', val: result };
  }
  return template;
}

function expandMacro(transformer: MacroTransformer, form: SchemeVal, env: Env): SchemeVal {
  if (form.tag !== 'list') throw new EvalError('macro application requires a list');
  const formElems = form.val.slice(1);

  for (const rule of transformer.rules) {
    const bindings: Bindings = new Map();
    if (matchPattern(rule.pattern, formElems, bindings, transformer.literals)) {
      const patternVars = new Set(bindings.keys());
      const freeSyms = new Set<string>();
      collectFreeSymbols(rule.template, patternVars, freeSyms);

      const renames = new Map<string, string>();
      for (const sym of freeSyms) renames.set(sym, gensym(sym));

      const expanded = expandTemplate(rule.template, bindings, renames);

      // Inject def-env bindings for renamed symbols
      const wrapperEnv = makeEnv(env);
      for (const [original, gensymName] of renames) {
        try {
          const val = envLookup(transformer.defEnv, original);
          envSet(wrapperEnv, gensymName, val);
        } catch {
          // Not in defEnv — newly introduced identifier, no pre-binding needed
        }
      }

      return evaluate(expanded, wrapperEnv);
    }
  }
  throw new EvalError('no matching pattern for macro');
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
            // (define (f params...) body...) or (define (f x . rest) body...)
            const fname = elems[1].val[0].val;
            const { fixed: params, rest } = parseParams(elems[1].val.slice(1), epos);
            const body = elems.slice(2);
            let closure: SchemeVal;
            if (rest !== null) {
              closure = makeVariadicClosure(params, rest, body, env, epos);
            } else {
              closure = { tag: 'procedure', val: (args: SchemeVal[]) => {
                if (args.length !== params.length) throw new EvalError(`${epos}: expected ${params.length} args, got ${args.length}`);
                const local = makeEnv(env);
                for (let i = 0; i < params.length; i++) envSet(local, params[i], args[i]);
                let result: SchemeVal = { tag: 'void' };
                for (const expr of body) result = evaluate(expr, local);
                return result;
              }};
            }
            envSet(env, fname, closure);
            return { tag: 'void' };
          }
          throw new EvalError(`${epos}: invalid define`);
        }

        if (name === 'set!') {
          if (elems.length !== 3) throw new EvalError(`${epos}: set! requires 2 arguments`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${epos}: set! target must be a symbol`);
          const val = evaluate(elems[2], env);
          envMutate(env, elems[1].val, val, epos);
          return { tag: 'void' };
        }

        if (name === 'lambda') {
          if (elems.length < 3) throw new EvalError(`${epos}: lambda requires params and body`);
          const paramList = elems[1];
          if (paramList.tag === 'symbol') {
            // (lambda args body...) — all args collected into one param
            const restName = paramList.val;
            const body = elems.slice(2);
            const closedEnv = env;
            return makeVariadicClosure([], restName, body, closedEnv, epos);
          }
          if (paramList.tag !== 'list') throw new EvalError(`${epos}: lambda params must be a list or symbol`);
          const { fixed: params, rest } = parseParams(paramList.val, epos);
          const body = elems.slice(2);
          const closedEnv = env;
          if (rest !== null) {
            return makeVariadicClosure(params, rest, body, closedEnv, epos);
          }
          return { tag: 'procedure', val: (args: SchemeVal[]) => {
            if (args.length !== params.length) throw new EvalError(`${epos}: expected ${params.length} args, got ${args.length}`);
            const local = makeEnv(closedEnv);
            for (let i = 0; i < params.length; i++) envSet(local, params[i], args[i]);
            let result: SchemeVal = { tag: 'void' };
            for (const expr of body) result = evaluate(expr, local);
            return result;
          }};
        }

        if (name === 'define-syntax') {
          if (elems.length !== 3) throw new EvalError(`${epos}: define-syntax requires 2 arguments`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${epos}: define-syntax: name must be a symbol`);
          const macroName = elems[1].val;
          const sr = elems[2];
          if (sr.tag !== 'list' || sr.val.length < 2 ||
              sr.val[0].tag !== 'symbol' || sr.val[0].val !== 'syntax-rules')
            throw new EvalError(`${epos}: define-syntax: expected syntax-rules`);
          const literalList = sr.val[1];
          if (literalList.tag !== 'list') throw new EvalError(`${epos}: syntax-rules: literals must be a list`);
          const literals = literalList.val.map(l => {
            if (l.tag !== 'symbol') throw new EvalError(`${epos}: syntax-rules: literal must be a symbol`);
            return l.val;
          });
          const rules: { pattern: SchemeVal[]; template: SchemeVal }[] = [];
          for (let i = 2; i < sr.val.length; i++) {
            const rule = sr.val[i];
            if (rule.tag !== 'list' || rule.val.length !== 2)
              throw new EvalError(`${epos}: syntax-rules: invalid rule`);
            const pat = rule.val[0];
            if (pat.tag !== 'list' || pat.val.length < 1)
              throw new EvalError(`${epos}: syntax-rules: pattern must be a non-empty list`);
            rules.push({ pattern: pat.val.slice(1), template: rule.val[1] });
          }
          envSet(env, macroName, { tag: 'macro', transformer: { literals, rules, defEnv: env } });
          return { tag: 'void' };
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

      // Check for macro application
      {
        let headVal: SchemeVal | undefined;
        try { if (elems[0].tag === 'symbol') headVal = envLookup(env, elems[0].val); } catch { /* not bound */ }
        if (headVal && headVal.tag === 'macro') {
          return expandMacro(headVal.transformer, expr, env);
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

function displayNumber(val: SchemeVal & { tag: 'number' }): string {
  if (val.exact !== false) {
    const den = val.den ?? 1;
    const num = val.num ?? val.val;
    if (den !== 1) return `${num}/${den}`;
    return String(num);
  }
  // Inexact: ensure floats show decimal point
  const s = String(val.val);
  if (Number.isFinite(val.val) && !s.includes('.') && !s.includes('e') && !s.includes('E')) {
    return s + '.0';
  }
  return s;
}

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return displayNumber(val);
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
    case 'macro': return '#<macro>';
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
