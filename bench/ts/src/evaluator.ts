import { EvalError } from './evalError.js';

// --- Types ---

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; exact?: boolean; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; dotted?: boolean; pos?: Pos }
  | { tag: 'builtin'; name: string; func: (args: SchemeVal[], callPos?: Pos) => SchemeVal; pos?: Pos }
  | { tag: 'lambda'; params: string[]; restParam?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'macro'; literals: string[]; rules: { pattern: SchemeVal; template: SchemeVal }[]; defEnv: Env; pos?: Pos }
  | { tag: 'record'; typeName: string; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'case-lambda'; clauses: { params: string[]; restParam?: string; body: SchemeVal[] }[]; env: Env; pos?: Pos }
  | { tag: 'void'; pos?: Pos };

// --- Parser ---

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  const advance = () => {
    if (input[i] === '\n') { line++; col = 1; } else { col++; }
    i++;
  };

  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { advance(); continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') advance(); continue; }
    if (ch === '(' || ch === ')') { tokens.push({ text: ch, pos: { line, col } }); advance(); continue; }
    if (ch === '"') {
      const startPos = { line, col };
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; advance(); if (i < input.length) { s += input[i]; advance(); } }
        else { s += input[i]; advance(); }
      }
      if (i < input.length) { s += '"'; advance(); }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    if (ch === "'") { tokens.push({ text: "'", pos: { line, col } }); advance(); continue; }
    const startPos = { line, col };
    let atom = '';
    while (i < input.length && !/[\s();]/.test(input[i])) { atom += input[i]; advance(); }
    if (atom.length > 0) tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parse(tokens: Token[], idx: number): [SchemeVal, number] {
  if (idx >= tokens.length) throw new EvalError('unexpected end of input');
  const token = tokens[idx];
  const p = token.pos;

  if (token.text === '(') {
    const elements: SchemeVal[] = [];
    idx++;
    while (idx < tokens.length && tokens[idx].text !== ')') {
      const [val, next] = parse(tokens, idx);
      elements.push(val);
      idx = next;
    }
    if (idx >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'list', elements, pos: p }, idx + 1];
  }

  if (token.text === ')') throw new EvalError('unexpected )');

  if (token.text === "'") {
    const [val, next] = parse(tokens, idx + 1);
    return [{ tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: p }, val], pos: p }, next];
  }

  if (token.text === '#t') return [{ tag: 'boolean', value: true, pos: p }, idx + 1];
  if (token.text === '#f') return [{ tag: 'boolean', value: false, pos: p }, idx + 1];

  if (token.text.startsWith('#\\')) {
    const rest = token.text.slice(2);
    let ch: string;
    if (rest === 'space') ch = ' ';
    else if (rest === 'newline') ch = '\n';
    else if (rest === 'tab') ch = '\t';
    else if (rest.length === 1) ch = rest;
    else throw new EvalError(`invalid character literal: ${token.text}`);
    return [{ tag: 'char', value: ch, pos: p }, idx + 1];
  }

  if (token.text.startsWith('"')) {
    const inner = token.text.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\t/g, '\t')
      .replace(/\\"/g, '"')
      .replace(/\\\\/g, '\\');
    return [{ tag: 'string', value: inner, pos: p }, idx + 1];
  }

  const ratMatch = token.text.match(/^(-?\d+)\/(\d+)$/);
  if (ratMatch) {
    const rn = parseInt(ratMatch[1], 10);
    const rd = parseInt(ratMatch[2], 10);
    return [makeRat(rn, rd, p), idx + 1];
  }

  const num = Number(token.text);
  if (!isNaN(num) && token.text !== '') {
    return [{ tag: 'number', value: num, pos: p }, idx + 1];
  }

  return [{ tag: 'symbol', value: token.text, pos: p }, idx + 1];
}

function parseAll(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const exprs: SchemeVal[] = [];
  let pos = 0;
  while (pos < tokens.length) {
    const [val, next] = parse(tokens, pos);
    exprs.push(val);
    pos = next;
  }
  return exprs;
}

// --- Helpers ---

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function schemeToString(val: SchemeVal): string {
  switch (val.tag) {
    case 'number':
      if (val.exact === false && Number.isInteger(val.value)) return val.value.toFixed(1);
      return String(val.value);
    case 'rational': return `${val.num}/${val.den}`;
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
    case 'list': {
      if (val.dotted && val.elements.length >= 2) {
        const init = val.elements.slice(0, -1).map(schemeToString).join(' ');
        return `(${init} . ${schemeToString(val.elements[val.elements.length - 1])})`;
      }
      return `(${val.elements.map(schemeToString).join(' ')})`;
    }
    case 'builtin': return `#<procedure:${val.name}>`;
    case 'lambda': return '#<procedure>';
    case 'case-lambda': return '#<procedure>';
    case 'macro': return '#<macro>';
    case 'record': return `#<record:${val.typeName}>`;
    case 'void': return '';
  }
}

function displayString(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return val.value;
    case 'char': return val.value;
    case 'list': {
      if (val.dotted && val.elements.length >= 2) {
        const init = val.elements.slice(0, -1).map(displayString).join(' ');
        return `(${init} . ${displayString(val.elements[val.elements.length - 1])})`;
      }
      return `(${val.elements.map(displayString).join(' ')})`;
    }
    default: return schemeToString(val);
  }
}

function fmtPos(p?: Pos): string {
  return p ? `${p.line}:${p.col}: ` : '';
}

function parseParams(elements: SchemeVal[], pos?: Pos): { params: string[]; restParam?: string } {
  const params: string[] = [];
  let restParam: string | undefined;
  for (let i = 0; i < elements.length; i++) {
    const el = elements[i];
    if (el.tag === 'symbol' && el.value === '.') {
      if (i + 1 !== elements.length - 1) throw new EvalError(`${fmtPos(pos)}invalid dot in parameter list`);
      const rest = elements[i + 1];
      if (rest.tag !== 'symbol') throw new EvalError(`${fmtPos(pos)}rest parameter must be a symbol`);
      restParam = rest.value;
      break;
    }
    if (el.tag !== 'symbol') throw new EvalError(`${fmtPos(pos)}parameter must be a symbol`);
    params.push(el.value);
  }
  return { params, restParam };
}

function expectNumbers(args: SchemeVal[], name: string, pos?: Pos): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${fmtPos(pos)}${name}: expected number, got ${schemeToString(a)}`);
    return a.value;
  });
}

// --- Rational helpers ---

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { [a, b] = [b, a % b]; }
  return a;
}

function makeRat(num: number, den: number, pos?: Pos): SchemeVal {
  if (den === 0) throw new EvalError('division by zero');
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(Math.abs(num), den);
  num /= g; den /= g;
  if (den === 1) return { tag: 'number', value: num, pos };
  return { tag: 'rational', num, den, pos };
}

function isExactVal(v: SchemeVal): boolean {
  if (v.tag === 'rational') return true;
  if (v.tag === 'number') return v.exact !== false && Number.isInteger(v.value);
  return false;
}

function toFloat(v: SchemeVal, name: string, pos?: Pos): number {
  if (v.tag === 'number') return v.value;
  if (v.tag === 'rational') return v.num / v.den;
  throw new EvalError(`${fmtPos(pos)}${name}: expected number, got ${schemeToString(v)}`);
}

function toRatParts(v: SchemeVal): [number, number] {
  if (v.tag === 'rational') return [v.num, v.den];
  if (v.tag === 'number') return [v.value, 1];
  throw new EvalError('not a number');
}

function expectNumeric(args: SchemeVal[], name: string, pos?: Pos): void {
  for (const a of args) {
    if (a.tag !== 'number' && a.tag !== 'rational')
      throw new EvalError(`${fmtPos(pos)}${name}: expected number, got ${schemeToString(a)}`);
  }
}

// --- Env ---

interface Env {
  bindings: Map<string, SchemeVal>;
  parent: Env | null;
}

function envLookup(env: Env, name: string): SchemeVal | undefined {
  let cur: Env | null = env;
  while (cur) {
    const val = cur.bindings.get(name);
    if (val !== undefined) return val;
    cur = cur.parent;
  }
  return undefined;
}

function envSet(env: Env, name: string, val: SchemeVal): void {
  let cur: Env | null = env;
  while (cur) {
    if (cur.bindings.has(name)) { cur.bindings.set(name, val); return; }
    cur = cur.parent;
  }
  throw new EvalError(`set!: unbound variable: ${name}`);
}

function envDefine(env: Env, name: string, val: SchemeVal): void {
  env.bindings.set(name, val);
}

function childEnv(parent: Env): Env {
  return { bindings: new Map(), parent };
}

function makeGlobalEnv(outputBuf: string[]): Env {
  const env: Env = { bindings: new Map(), parent: null };

  const defBuiltin = (name: string, func: (args: SchemeVal[], callPos?: Pos) => SchemeVal) => {
    envDefine(env, name, { tag: 'builtin', name, func });
  };

  defBuiltin('+', (args, p) => {
    expectNumeric(args, '+', p);
    if (args.every(a => isExactVal(a))) {
      let num = 0, den = 1;
      for (const a of args) {
        const [an, ad] = toRatParts(a);
        num = num * ad + an * den;
        den = den * ad;
      }
      return makeRat(num, den);
    }
    return { tag: 'number', value: args.reduce((acc, a) => acc + toFloat(a, '+', p), 0) };
  });

  defBuiltin('-', (args, p) => {
    if (args.length === 0) throw new EvalError(`${fmtPos(p)}-: need at least 1 arg`);
    expectNumeric(args, '-', p);
    if (args.length === 1) {
      if (isExactVal(args[0])) {
        const [n, d] = toRatParts(args[0]);
        return makeRat(-n, d);
      }
      return { tag: 'number', value: -toFloat(args[0], '-', p) };
    }
    if (args.every(a => isExactVal(a))) {
      let [num, den] = toRatParts(args[0]);
      for (let i = 1; i < args.length; i++) {
        const [an, ad] = toRatParts(args[i]);
        num = num * ad - an * den;
        den = den * ad;
      }
      return makeRat(num, den);
    }
    const floats = args.map(a => toFloat(a, '-', p));
    return { tag: 'number', value: floats.slice(1).reduce((a, b) => a - b, floats[0]) };
  });

  defBuiltin('*', (args, p) => {
    expectNumeric(args, '*', p);
    if (args.every(a => isExactVal(a))) {
      let num = 1, den = 1;
      for (const a of args) {
        const [an, ad] = toRatParts(a);
        num *= an;
        den *= ad;
      }
      return makeRat(num, den);
    }
    return { tag: 'number', value: args.reduce((acc, a) => acc * toFloat(a, '*', p), 1) };
  });

  defBuiltin('/', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}/: expected 2 args`);
    expectNumeric(args, '/', p);
    if (args.every(a => isExactVal(a))) {
      const [an, ad] = toRatParts(args[0]);
      const [bn, bd] = toRatParts(args[1]);
      if (bn === 0) throw new EvalError(`${fmtPos(p)}division by zero`);
      return makeRat(an * bd, ad * bn);
    }
    const d = toFloat(args[1], '/', p);
    if (d === 0) throw new EvalError(`${fmtPos(p)}division by zero`);
    return { tag: 'number', value: toFloat(args[0], '/', p) / d };
  });

  defBuiltin('<', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}<: expected 2 args`);
    expectNumeric(args, '<', p);
    return { tag: 'boolean', value: toFloat(args[0], '<', p) < toFloat(args[1], '<', p) };
  });

  defBuiltin('>', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}>: expected 2 args`);
    expectNumeric(args, '>', p);
    return { tag: 'boolean', value: toFloat(args[0], '>', p) > toFloat(args[1], '>', p) };
  });

  defBuiltin('=', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}=: expected 2 args`);
    expectNumeric(args, '=', p);
    return { tag: 'boolean', value: toFloat(args[0], '=', p) === toFloat(args[1], '=', p) };
  });

  defBuiltin('<=', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}<=: expected 2 args`);
    expectNumeric(args, '<=', p);
    return { tag: 'boolean', value: toFloat(args[0], '<=', p) <= toFloat(args[1], '<=', p) };
  });

  defBuiltin('>=', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}>=: expected 2 args`);
    expectNumeric(args, '>=', p);
    return { tag: 'boolean', value: toFloat(args[0], '>=', p) >= toFloat(args[1], '>=', p) };
  });

  // List operations
  defBuiltin('cons', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}cons: expected 2 args`);
    const [head, tail] = args;
    if (tail.tag === 'list') {
      return { tag: 'list', elements: [head, ...tail.elements], dotted: tail.dotted };
    }
    return { tag: 'list', elements: [head, tail], dotted: true };
  });

  defBuiltin('car', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}car: expected 1 arg`);
    if (args[0].tag !== 'list' || args[0].elements.length === 0)
      throw new EvalError(`${fmtPos(p)}car: expected non-empty list`);
    return args[0].elements[0];
  });

  defBuiltin('cdr', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdr: expected 1 arg`);
    if (args[0].tag !== 'list' || args[0].elements.length === 0)
      throw new EvalError(`${fmtPos(p)}cdr: expected non-empty list`);
    const lst = args[0];
    if (lst.dotted && lst.elements.length === 2) {
      return lst.elements[1];
    }
    const rest = lst.elements.slice(1);
    return { tag: 'list', elements: rest, dotted: lst.dotted };
  });

  defBuiltin('null?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}null?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length === 0 };
  });

  defBuiltin('list', (args) => {
    return { tag: 'list', elements: args };
  });

  defBuiltin('length', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}length: expected 1 arg`);
    if (args[0].tag !== 'list') throw new EvalError(`${fmtPos(p)}length: expected list`);
    return { tag: 'number', value: args[0].elements.length };
  });

  defBuiltin('append', (args, p) => {
    const result: SchemeVal[] = [];
    for (const arg of args) {
      if (arg.tag !== 'list') throw new EvalError(`${fmtPos(p)}append: expected list`);
      result.push(...arg.elements);
    }
    return { tag: 'list', elements: result };
  });

  // Type predicates
  defBuiltin('number?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}number?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'number' || args[0].tag === 'rational' };
  });

  defBuiltin('string?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'string' };
  });

  defBuiltin('boolean?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}boolean?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  });

  defBuiltin('pair?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}pair?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length > 0 };
  });

  defBuiltin('symbol?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}symbol?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  defBuiltin('procedure?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}procedure?: expected 1 arg`);
    const t = args[0].tag;
    return { tag: 'boolean', value: t === 'builtin' || t === 'lambda' || t === 'case-lambda' };
  });

  // L05: Display/Write/Newline
  defBuiltin('display', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}display: expected 1 arg`);
    outputBuf.push(displayString(args[0]));
    return { tag: 'void' };
  });

  defBuiltin('write', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}write: expected 1 arg`);
    outputBuf.push(schemeToString(args[0]));
    return { tag: 'void' };
  });

  defBuiltin('newline', (args, p) => {
    if (args.length !== 0) throw new EvalError(`${fmtPos(p)}newline: expected 0 args`);
    outputBuf.push('\n');
    return { tag: 'void' };
  });

  // L05: String operations
  defBuiltin('string-append', (args, p) => {
    const strs = args.map(a => {
      if (a.tag !== 'string') throw new EvalError(`${fmtPos(p)}string-append: expected string, got ${schemeToString(a)}`);
      return a.value;
    });
    return { tag: 'string', value: strs.join('') };
  });

  defBuiltin('string-length', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string-length: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-length: expected string`);
    return { tag: 'number', value: args[0].value.length };
  });

  defBuiltin('substring', (args, p) => {
    if (args.length !== 3) throw new EvalError(`${fmtPos(p)}substring: expected 3 args`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}substring: expected string`);
    const nums = expectNumbers([args[1], args[2]], 'substring', p);
    return { tag: 'string', value: args[0].value.slice(nums[0], nums[1]) };
  });

  defBuiltin('string->number', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string->number: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string->number: expected string`);
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  });

  defBuiltin('number->string', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}number->string: expected 1 arg`);
    if (args[0].tag !== 'number') throw new EvalError(`${fmtPos(p)}number->string: expected number`);
    return { tag: 'string', value: String(args[0].value) };
  });

  // L05: Symbol/String conversion
  defBuiltin('symbol->string', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}symbol->string: expected 1 arg`);
    if (args[0].tag !== 'symbol') throw new EvalError(`${fmtPos(p)}symbol->string: expected symbol`);
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('string->symbol', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string->symbol: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string->symbol: expected string`);
    return { tag: 'symbol', value: args[0].value };
  });

  // L05: Character operations
  defBuiltin('string-ref', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string-ref: expected 2 args`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-ref: expected string`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}string-ref: expected number`);
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].value.length) throw new EvalError(`${fmtPos(p)}string-ref: index out of range`);
    return { tag: 'char', value: args[0].value[idx] };
  });

  defBuiltin('string-copy', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string-copy: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-copy: expected string`);
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('char?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'char' };
  });

  // eq? and equal?
  defBuiltin('eq?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}eq?: expected 2 args`);
    const [a, b] = args;
    if (a.tag !== b.tag) return { tag: 'boolean', value: false };
    if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'list' && b.tag === 'list' && a.elements.length === 0 && b.elements.length === 0) return { tag: 'boolean', value: true };
    return { tag: 'boolean', value: a === b };
  });

  const schemeEqual = (a: SchemeVal, b: SchemeVal): boolean => {
    if ((a.tag === 'number' || a.tag === 'rational') && (b.tag === 'number' || b.tag === 'rational')) {
      if (a.tag === 'rational' && b.tag === 'rational') return a.num === b.num && a.den === b.den;
      if (a.tag === 'number' && b.tag === 'number') return a.value === b.value;
      return false;
    }
    if (a.tag !== b.tag) return false;
    if (a.tag === 'boolean' && b.tag === 'boolean') return a.value === b.value;
    if (a.tag === 'string' && b.tag === 'string') return a.value === b.value;
    if (a.tag === 'symbol' && b.tag === 'symbol') return a.value === b.value;
    if (a.tag === 'char' && b.tag === 'char') return a.value === b.value;
    if (a.tag === 'list' && b.tag === 'list') {
      if (a.elements.length !== b.elements.length) return false;
      if (!!a.dotted !== !!b.dotted) return false;
      return a.elements.every((el, i) => schemeEqual(el, b.elements[i]));
    }
    return a === b;
  };

  defBuiltin('equal?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}equal?: expected 2 args`);
    return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
  });

  // map (supports multiple lists)
  defBuiltin('map', (args, p) => {
    if (args.length < 2) throw new EvalError(`${fmtPos(p)}map: expected at least 2 args`);
    const func = args[0];
    const lists = args.slice(1);
    for (const l of lists) {
      if (l.tag !== 'list') throw new EvalError(`${fmtPos(p)}map: expected list`);
    }
    const len = (lists[0] as { tag: 'list'; elements: SchemeVal[] }).elements.length;
    const result: SchemeVal[] = [];
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map(l => (l as { tag: 'list'; elements: SchemeVal[] }).elements[i]);
      if (func.tag === 'builtin') {
        result.push(func.func(callArgs, p));
      } else if (func.tag === 'lambda') {
        const callEnv = childEnv(func.env);
        for (let j = 0; j < func.params.length; j++) {
          envDefine(callEnv, func.params[j], callArgs[j]);
        }
        if (func.restParam) {
          envDefine(callEnv, func.restParam, { tag: 'list', elements: callArgs.slice(func.params.length) });
        }
        let res: SchemeVal = { tag: 'void' };
        for (const bodyExpr of func.body) {
          res = evalScheme(bodyExpr, callEnv);
        }
        result.push(res);
      } else if (func.tag === 'case-lambda') {
        result.push(applyCaseLambda(func, callArgs, p));
      } else {
        throw new EvalError(`${fmtPos(p)}map: not a procedure`);
      }
    }
    return { tag: 'list', elements: result };
  });

  // for-each (like map but returns void)
  defBuiltin('for-each', (args, p) => {
    if (args.length < 2) throw new EvalError(`${fmtPos(p)}for-each: expected at least 2 args`);
    const func = args[0];
    const lists = args.slice(1);
    for (const l of lists) {
      if (l.tag !== 'list') throw new EvalError(`${fmtPos(p)}for-each: expected list`);
    }
    const len = (lists[0] as { tag: 'list'; elements: SchemeVal[] }).elements.length;
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map(l => (l as { tag: 'list'; elements: SchemeVal[] }).elements[i]);
      if (func.tag === 'builtin') {
        func.func(callArgs, p);
      } else if (func.tag === 'lambda') {
        const callEnv = childEnv(func.env);
        for (let j = 0; j < func.params.length; j++) {
          envDefine(callEnv, func.params[j], callArgs[j]);
        }
        if (func.restParam) {
          envDefine(callEnv, func.restParam, { tag: 'list', elements: callArgs.slice(func.params.length) });
        }
        for (const bodyExpr of func.body) {
          evalScheme(bodyExpr, callEnv);
        }
      } else if (func.tag === 'case-lambda') {
        applyCaseLambda(func, callArgs, p);
      } else {
        throw new EvalError(`${fmtPos(p)}for-each: not a procedure`);
      }
    }
    return { tag: 'void' };
  });

  // L09: Numeric utilities
  defBuiltin('abs', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}abs: expected 1 arg`);
    const nums = expectNumbers(args, 'abs', p);
    return { tag: 'number', value: Math.abs(nums[0]) };
  });

  defBuiltin('modulo', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}modulo: expected 2 args`);
    const nums = expectNumbers(args, 'modulo', p);
    const [a, b] = nums;
    if (b === 0) throw new EvalError(`${fmtPos(p)}modulo: division by zero`);
    return { tag: 'number', value: a - b * Math.floor(a / b) };
  });

  defBuiltin('remainder', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}remainder: expected 2 args`);
    const nums = expectNumbers(args, 'remainder', p);
    const [a, b] = nums;
    if (b === 0) throw new EvalError(`${fmtPos(p)}remainder: division by zero`);
    return { tag: 'number', value: a - b * Math.trunc(a / b) };
  });

  defBuiltin('quotient', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}quotient: expected 2 args`);
    const nums = expectNumbers(args, 'quotient', p);
    if (nums[1] === 0) throw new EvalError(`${fmtPos(p)}quotient: division by zero`);
    return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
  });

  defBuiltin('expt', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}expt: expected 2 args`);
    const nums = expectNumbers(args, 'expt', p);
    return { tag: 'number', value: Math.pow(nums[0], nums[1]) };
  });

  defBuiltin('min', (args, p) => {
    if (args.length < 1) throw new EvalError(`${fmtPos(p)}min: expected at least 1 arg`);
    const nums = expectNumbers(args, 'min', p);
    return { tag: 'number', value: Math.min(...nums) };
  });

  defBuiltin('max', (args, p) => {
    if (args.length < 1) throw new EvalError(`${fmtPos(p)}max: expected at least 1 arg`);
    const nums = expectNumbers(args, 'max', p);
    return { tag: 'number', value: Math.max(...nums) };
  });

  defBuiltin('zero?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}zero?: expected 1 arg`);
    const nums = expectNumbers(args, 'zero?', p);
    return { tag: 'boolean', value: nums[0] === 0 };
  });

  defBuiltin('positive?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}positive?: expected 1 arg`);
    const nums = expectNumbers(args, 'positive?', p);
    return { tag: 'boolean', value: nums[0] > 0 };
  });

  defBuiltin('negative?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}negative?: expected 1 arg`);
    const nums = expectNumbers(args, 'negative?', p);
    return { tag: 'boolean', value: nums[0] < 0 };
  });

  defBuiltin('odd?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}odd?: expected 1 arg`);
    const nums = expectNumbers(args, 'odd?', p);
    return { tag: 'boolean', value: Math.abs(nums[0]) % 2 === 1 };
  });

  defBuiltin('even?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}even?: expected 1 arg`);
    const nums = expectNumbers(args, 'even?', p);
    return { tag: 'boolean', value: nums[0] % 2 === 0 };
  });

  // L09: List utilities
  defBuiltin('list-ref', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}list-ref: expected 2 args`);
    if (args[0].tag !== 'list') throw new EvalError(`${fmtPos(p)}list-ref: expected list`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}list-ref: expected number`);
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError(`${fmtPos(p)}list-ref: index out of range`);
    return args[0].elements[idx];
  });

  defBuiltin('list-tail', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}list-tail: expected 2 args`);
    if (args[0].tag !== 'list') throw new EvalError(`${fmtPos(p)}list-tail: expected list`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}list-tail: expected number`);
    const idx = args[1].value;
    return { tag: 'list', elements: args[0].elements.slice(idx) } as SchemeVal;
  });

  defBuiltin('list?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}list?: expected 1 arg`);
    const v = args[0];
    if (v.tag !== 'list') return { tag: 'boolean', value: false };
    if (v.dotted) return { tag: 'boolean', value: false };
    return { tag: 'boolean', value: true };
  });

  defBuiltin('assoc', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}assoc: expected 2 args`);
    const key = args[0];
    const alist = args[1];
    if (alist.tag !== 'list') throw new EvalError(`${fmtPos(p)}assoc: expected list`);
    for (const pair of alist.elements) {
      if (pair.tag === 'list' && pair.elements.length >= 2) {
        if (schemeEqual(pair.elements[0], key)) return pair;
      }
    }
    return { tag: 'boolean', value: false };
  });

  // L09: Character utilities
  defBuiltin('char-alphabetic?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char-alphabetic?: expected 1 arg`);
    if (args[0].tag !== 'char') throw new EvalError(`${fmtPos(p)}char-alphabetic?: expected char`);
    return { tag: 'boolean', value: /[a-zA-Z]/.test(args[0].value) };
  });

  defBuiltin('char-numeric?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char-numeric?: expected 1 arg`);
    if (args[0].tag !== 'char') throw new EvalError(`${fmtPos(p)}char-numeric?: expected char`);
    return { tag: 'boolean', value: /[0-9]/.test(args[0].value) };
  });

  defBuiltin('char-upcase', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char-upcase: expected 1 arg`);
    if (args[0].tag !== 'char') throw new EvalError(`${fmtPos(p)}char-upcase: expected char`);
    return { tag: 'char', value: args[0].value.toUpperCase() };
  });

  defBuiltin('char-downcase', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char-downcase: expected 1 arg`);
    if (args[0].tag !== 'char') throw new EvalError(`${fmtPos(p)}char-downcase: expected char`);
    return { tag: 'char', value: args[0].value.toLowerCase() };
  });

  defBuiltin('char=?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}char=?: expected 2 args`);
    if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError(`${fmtPos(p)}char=?: expected chars`);
    return { tag: 'boolean', value: args[0].value === args[1].value };
  });

  defBuiltin('char<?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}char<?: expected 2 args`);
    if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError(`${fmtPos(p)}char<?: expected chars`);
    return { tag: 'boolean', value: args[0].value < args[1].value };
  });

  // L09: String comparison/conversion
  defBuiltin('string=?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string=?: expected 2 args`);
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${fmtPos(p)}string=?: expected strings`);
    return { tag: 'boolean', value: args[0].value === args[1].value };
  });

  defBuiltin('string<?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string<?: expected 2 args`);
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${fmtPos(p)}string<?: expected strings`);
    return { tag: 'boolean', value: args[0].value < args[1].value };
  });

  defBuiltin('string-ci=?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string-ci=?: expected 2 args`);
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-ci=?: expected strings`);
    return { tag: 'boolean', value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
  });

  defBuiltin('string-upcase', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string-upcase: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-upcase: expected string`);
    return { tag: 'string', value: args[0].value.toUpperCase() };
  });

  defBuiltin('string-downcase', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string-downcase: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-downcase: expected string`);
    return { tag: 'string', value: args[0].value.toLowerCase() };
  });

  defBuiltin('integer?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}integer?: expected 1 arg`);
    if (args[0].tag === 'rational') return { tag: 'boolean', value: false };
    return { tag: 'boolean', value: args[0].tag === 'number' && Number.isInteger(args[0].value) };
  });

  // L11: Exact arithmetic & rationals
  defBuiltin('exact?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}exact?: expected 1 arg`);
    return { tag: 'boolean', value: isExactVal(args[0]) };
  });

  defBuiltin('inexact?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}inexact?: expected 1 arg`);
    if (args[0].tag !== 'number' && args[0].tag !== 'rational') return { tag: 'boolean', value: false };
    return { tag: 'boolean', value: !isExactVal(args[0]) };
  });

  defBuiltin('exact->inexact', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}exact->inexact: expected 1 arg`);
    return { tag: 'number', value: toFloat(args[0], 'exact->inexact', p), exact: false };
  });

  defBuiltin('inexact->exact', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}inexact->exact: expected 1 arg`);
    const f = toFloat(args[0], 'inexact->exact', p);
    if (Number.isInteger(f)) return { tag: 'number', value: f };
    const str = f.toString();
    const decIdx = str.indexOf('.');
    if (decIdx >= 0) {
      const decimals = str.length - decIdx - 1;
      const pow = Math.pow(10, decimals);
      const num = Math.round(f * pow);
      return makeRat(num, pow);
    }
    return { tag: 'number', value: f };
  });

  defBuiltin('numerator', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}numerator: expected 1 arg`);
    if (args[0].tag === 'rational') return { tag: 'number', value: args[0].num };
    if (args[0].tag === 'number' && isExactVal(args[0])) return { tag: 'number', value: args[0].value };
    throw new EvalError(`${fmtPos(p)}numerator: expected exact number`);
  });

  defBuiltin('denominator', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}denominator: expected 1 arg`);
    if (args[0].tag === 'rational') return { tag: 'number', value: args[0].den };
    if (args[0].tag === 'number' && isExactVal(args[0])) return { tag: 'number', value: 1 };
    throw new EvalError(`${fmtPos(p)}denominator: expected exact number`);
  });

  defBuiltin('rational?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}rational?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'rational' || (args[0].tag === 'number' && isExactVal(args[0])) };
  });

  // L08: apply
  defBuiltin('apply', (args, p) => {
    if (args.length < 2) throw new EvalError(`${fmtPos(p)}apply: expected at least 2 args`);
    const func = args[0];
    const last = args[args.length - 1];
    if (last.tag !== 'list') throw new EvalError(`${fmtPos(p)}apply: last argument must be a list`);
    const callArgs = [...args.slice(1, -1), ...last.elements];
    if (func.tag === 'builtin') return func.func(callArgs, p);
    if (func.tag === 'lambda') {
      if (func.restParam) {
        if (callArgs.length < func.params.length) throw new EvalError(`${fmtPos(p)}wrong number of arguments`);
      } else {
        if (callArgs.length !== func.params.length) throw new EvalError(`${fmtPos(p)}wrong number of arguments`);
      }
      const callEnv = childEnv(func.env);
      for (let i = 0; i < func.params.length; i++) {
        envDefine(callEnv, func.params[i], callArgs[i]);
      }
      if (func.restParam) {
        envDefine(callEnv, func.restParam, { tag: 'list', elements: callArgs.slice(func.params.length) });
      }
      let result: SchemeVal = { tag: 'void' };
      for (const bodyExpr of func.body) {
        result = evalScheme(bodyExpr, callEnv);
      }
      return result;
    }
    if (func.tag === 'case-lambda') return applyCaseLambda(func, callArgs, p);
    throw new EvalError(`${fmtPos(p)}apply: not a procedure`);
  });

  return env;
}

// --- Macro support ---

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'lambda', 'and', 'or', 'not', 'begin',
  'cond', 'set!', 'string-set!', 'let', 'define-syntax', 'define-record-type', 'case-lambda'
]);

type MacroBindings = Map<string, SchemeVal | SchemeVal[]>;

function getPatternVars(pattern: SchemeVal, literals: string[]): Set<string> {
  const vars = new Set<string>();
  function walk(p: SchemeVal) {
    if (p.tag === 'symbol' && p.value !== '...' && p.value !== '_' && !literals.includes(p.value)) {
      vars.add(p.value);
    }
    if (p.tag === 'list') {
      for (const elem of p.elements) walk(elem);
    }
  }
  walk(pattern);
  return vars;
}

function matchPattern(pattern: SchemeVal, input: SchemeVal, literals: string[], bindings: MacroBindings): boolean {
  if (pattern.tag === 'symbol') {
    if (pattern.value === '_') return true;
    if (literals.includes(pattern.value)) {
      return input.tag === 'symbol' && input.value === pattern.value;
    }
    bindings.set(pattern.value, input);
    return true;
  }
  if (pattern.tag === 'boolean' && input.tag === 'boolean') return pattern.value === input.value;
  if (pattern.tag === 'number' && input.tag === 'number') return pattern.value === input.value;
  if (pattern.tag === 'list' && input.tag === 'list') {
    const patElems = pattern.elements;
    const inpElems = input.elements;
    let ellipsisIdx = -1;
    for (let i = 0; i < patElems.length; i++) {
      const pe = patElems[i];
      if (pe.tag === 'symbol' && pe.value === '...') {
        ellipsisIdx = i;
        break;
      }
    }
    if (ellipsisIdx === -1) {
      if (patElems.length !== inpElems.length) return false;
      for (let i = 0; i < patElems.length; i++) {
        if (!matchPattern(patElems[i], inpElems[i], literals, bindings)) return false;
      }
      return true;
    }
    const beforeCount = ellipsisIdx - 1;
    const afterCount = patElems.length - ellipsisIdx - 1;
    if (inpElems.length < beforeCount + afterCount) return false;
    for (let i = 0; i < beforeCount; i++) {
      if (!matchPattern(patElems[i], inpElems[i], literals, bindings)) return false;
    }
    const repeatedPattern = patElems[ellipsisIdx - 1];
    const repeatCount = inpElems.length - beforeCount - afterCount;
    if (repeatedPattern.tag === 'symbol' && !literals.includes(repeatedPattern.value) && repeatedPattern.value !== '_') {
      const matches: SchemeVal[] = [];
      for (let i = 0; i < repeatCount; i++) {
        matches.push(inpElems[beforeCount + i]);
      }
      bindings.set(repeatedPattern.value, matches);
    }
    for (let i = 0; i < afterCount; i++) {
      if (!matchPattern(patElems[ellipsisIdx + 1 + i], inpElems[inpElems.length - afterCount + i], literals, bindings)) return false;
    }
    return true;
  }
  return false;
}

function findEllipsisVars(template: SchemeVal, bindings: MacroBindings): string[] {
  const result: string[] = [];
  if (template.tag === 'symbol') {
    const val = bindings.get(template.value);
    if (Array.isArray(val)) result.push(template.value);
  }
  if (template.tag === 'list') {
    for (const elem of template.elements) {
      result.push(...findEllipsisVars(elem, bindings));
    }
  }
  return result;
}

function expandTemplate(
  template: SchemeVal,
  bindings: MacroBindings,
  defEnv: Env,
  patternVars: Set<string>
): SchemeVal {
  if (template.tag === 'symbol') {
    const name = template.value;
    if (patternVars.has(name) && bindings.has(name)) {
      const val = bindings.get(name)!;
      if (!Array.isArray(val)) return val;
      throw new EvalError(`macro: ellipsis variable ${name} used without ...`);
    }
    if (!SPECIAL_FORMS.has(name) && !patternVars.has(name)) {
      const defVal = envLookup(defEnv, name);
      if (defVal !== undefined && defVal.tag !== 'macro') {
        return defVal;
      }
    }
    return template;
  }
  if (template.tag === 'list') {
    const result: SchemeVal[] = [];
    const elems = template.elements;
    for (let i = 0; i < elems.length; i++) {
      const next = elems[i + 1];
      if (i + 1 < elems.length && next && next.tag === 'symbol' && next.value === '...') {
        const evars = findEllipsisVars(elems[i], bindings);
        if (evars.length > 0) {
          const firstArr = bindings.get(evars[0]) as SchemeVal[];
          for (let j = 0; j < firstArr.length; j++) {
            const subBindings = new Map(bindings);
            for (const v of evars) {
              subBindings.set(v, (bindings.get(v) as SchemeVal[])[j]);
            }
            result.push(expandTemplate(elems[i], subBindings, defEnv, patternVars));
          }
        }
        i++;
        continue;
      }
      result.push(expandTemplate(elems[i], bindings, defEnv, patternVars));
    }
    return { tag: 'list', elements: result };
  }
  return template;
}

function expandMacroCall(
  macro: { literals: string[]; rules: { pattern: SchemeVal; template: SchemeVal }[]; defEnv: Env },
  input: SchemeVal
): SchemeVal {
  for (const rule of macro.rules) {
    const bindings: MacroBindings = new Map();
    if (matchPattern(rule.pattern, input, macro.literals, bindings)) {
      const patternVars = getPatternVars(rule.pattern, macro.literals);
      return expandTemplate(rule.template, bindings, macro.defEnv, patternVars);
    }
  }
  throw new EvalError('no matching macro pattern');
}

// --- case-lambda helper ---

function applyCaseLambda(func: Extract<SchemeVal, { tag: 'case-lambda' }>, args: SchemeVal[], callPos?: Pos): SchemeVal {
  for (const clause of func.clauses) {
    if (clause.restParam) {
      if (args.length >= clause.params.length) {
        const callEnv = childEnv(func.env);
        for (let i = 0; i < clause.params.length; i++) {
          envDefine(callEnv, clause.params[i], args[i]);
        }
        envDefine(callEnv, clause.restParam, { tag: 'list', elements: args.slice(clause.params.length) });
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of clause.body) {
          result = evalScheme(bodyExpr, callEnv);
        }
        return result;
      }
    } else {
      if (args.length === clause.params.length) {
        const callEnv = childEnv(func.env);
        for (let i = 0; i < clause.params.length; i++) {
          envDefine(callEnv, clause.params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of clause.body) {
          result = evalScheme(bodyExpr, callEnv);
        }
        return result;
      }
    }
  }
  throw new EvalError(`${fmtPos(callPos)}no matching clause for ${args.length} arguments`);
}

// --- Eval ---

function evalScheme(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'rational':
    case 'boolean':
    case 'string':
    case 'char':
      return expr;

    case 'symbol': {
      const val = envLookup(env, expr.value);
      if (val === undefined) throw new EvalError(`${fmtPos(expr.pos)}unbound variable: ${expr.value}`);
      return val;
    }

    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw new EvalError(`${fmtPos(expr.pos)}empty application`);

      if (elems[0].tag === 'symbol') {
        const name = elems[0].value;

        if (name === 'quote') {
          if (elems.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}quote: wrong argument count`);
          return elems[1];
        }

        if (name === 'if') {
          if (elems.length < 3 || elems.length > 4) throw new EvalError(`${fmtPos(expr.pos)}if: wrong argument count`);
          const cond = evalScheme(elems[1], env);
          if (isTruthy(cond)) return evalScheme(elems[2], env);
          if (elems.length === 4) return evalScheme(elems[3], env);
          return { tag: 'void' };
        }

        if (name === 'define') {
          if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}define: wrong argument count`);
          if (elems[1].tag === 'symbol') {
            const val = evalScheme(elems[2], env);
            envDefine(env, elems[1].value, val);
            return { tag: 'void' };
          }
          if (elems[1].tag === 'list' && elems[1].elements.length > 0 && elems[1].elements[0].tag === 'symbol') {
            const fnName = elems[1].elements[0].value;
            const { params, restParam } = parseParams(elems[1].elements.slice(1), expr.pos);
            const lambda: SchemeVal = { tag: 'lambda', params, restParam, body: elems.slice(2), env };
            envDefine(env, fnName, lambda);
            return { tag: 'void' };
          }
          throw new EvalError(`${fmtPos(expr.pos)}define: invalid syntax`);
        }

        if (name === 'lambda') {
          if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}lambda: wrong argument count`);
          if (elems[1].tag === 'symbol') {
            // (lambda args body...) — all args as rest
            return { tag: 'lambda', params: [], restParam: elems[1].value, body: elems.slice(2), env };
          }
          if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}lambda: params must be a list`);
          const { params, restParam } = parseParams(elems[1].elements, expr.pos);
          return { tag: 'lambda', params, restParam, body: elems.slice(2), env };
        }

        if (name === 'case-lambda') {
          const clauses: { params: string[]; restParam?: string; body: SchemeVal[] }[] = [];
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.elements.length < 2)
              throw new EvalError(`${fmtPos(expr.pos)}case-lambda: invalid clause`);
            const paramForm = clause.elements[0];
            if (paramForm.tag !== 'list')
              throw new EvalError(`${fmtPos(expr.pos)}case-lambda: params must be a list`);
            const { params, restParam } = parseParams(paramForm.elements, expr.pos);
            clauses.push({ params, restParam, body: clause.elements.slice(1) });
          }
          return { tag: 'case-lambda', clauses, env, pos: expr.pos };
        }

        if (name === 'and') {
          let result: SchemeVal = { tag: 'boolean', value: true };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
            if (!isTruthy(result)) return result;
          }
          return result;
        }

        if (name === 'or') {
          let result: SchemeVal = { tag: 'boolean', value: false };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
            if (isTruthy(result)) return result;
          }
          return result;
        }

        if (name === 'not') {
          if (elems.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}not: wrong argument count`);
          const val = evalScheme(elems[1], env);
          return { tag: 'boolean', value: !isTruthy(val) };
        }

        if (name === 'begin') {
          let result: SchemeVal = { tag: 'void' };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
          }
          return result;
        }

        if (name === 'cond') {
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.elements.length < 2)
              throw new EvalError(`${fmtPos(expr.pos)}cond: invalid clause`);
            if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.elements.length; j++) {
                result = evalScheme(clause.elements[j], env);
              }
              return result;
            }
            const test = evalScheme(clause.elements[0], env);
            if (isTruthy(test)) {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.elements.length; j++) {
                result = evalScheme(clause.elements[j], env);
              }
              return result;
            }
          }
          return { tag: 'void' };
        }

        if (name === 'set!') {
          if (elems.length !== 3) throw new EvalError(`${fmtPos(expr.pos)}set!: wrong argument count`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}set!: first arg must be a symbol`);
          const val = evalScheme(elems[2], env);
          envSet(env, elems[1].value, val);
          return { tag: 'void' };
        }

        if (name === 'string-set!') {
          if (elems.length !== 4) throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected 3 args`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}string-set!: first arg must be a variable`);
          const strVal = envLookup(env, elems[1].value);
          if (!strVal || strVal.tag !== 'string') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected string variable`);
          const idx = evalScheme(elems[2], env);
          if (idx.tag !== 'number') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected number index`);
          const ch = evalScheme(elems[3], env);
          if (ch.tag !== 'char') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected char`);
          const s = strVal.value;
          const i = idx.value;
          if (i < 0 || i >= s.length) throw new EvalError(`${fmtPos(expr.pos)}string-set!: index out of range`);
          envSet(env, elems[1].value, { tag: 'string', value: s.slice(0, i) + ch.value + s.slice(i + 1) });
          return { tag: 'void' };
        }

        if (name === 'let') {
          if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}let: wrong argument count`);
          // Named let: (let name ((var init) ...) body ...)
          if (elems[1].tag === 'symbol') {
            const loopName = elems[1].value;
            if (elems[2].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}let: bindings must be a list`);
            const bindings = elems[2].elements;
            const params: string[] = [];
            const inits: SchemeVal[] = [];
            for (const b of bindings) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw new EvalError(`${fmtPos(expr.pos)}let: invalid binding`);
              params.push(b.elements[0].value);
              inits.push(evalScheme(b.elements[1], env));
            }
            const body = elems.slice(3);
            const loopEnv = childEnv(env);
            const lambda: SchemeVal = { tag: 'lambda', params, body, env: loopEnv };
            envDefine(loopEnv, loopName, lambda);
            // Call with initial values
            const callEnv = childEnv(loopEnv);
            for (let i = 0; i < params.length; i++) {
              envDefine(callEnv, params[i], inits[i]);
            }
            let result: SchemeVal = { tag: 'void' };
            for (const bodyExpr of body) {
              result = evalScheme(bodyExpr, callEnv);
            }
            return result;
          }
          // Regular let: (let ((var init) ...) body ...)
          if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}let: bindings must be a list`);
          const letEnv = childEnv(env);
          for (const binding of elems[1].elements) {
            if (binding.tag !== 'list' || binding.elements.length !== 2 || binding.elements[0].tag !== 'symbol')
              throw new EvalError(`${fmtPos(expr.pos)}let: invalid binding`);
            const val = evalScheme(binding.elements[1], env);
            envDefine(letEnv, binding.elements[0].value, val);
          }
          let letResult: SchemeVal = { tag: 'void' };
          for (let i = 2; i < elems.length; i++) {
            letResult = evalScheme(elems[i], letEnv);
          }
          return letResult;
        }

        if (name === 'define-record-type') {
          // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
          if (elems.length < 4) throw new EvalError(`${fmtPos(expr.pos)}define-record-type: invalid syntax`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}define-record-type: expected type name`);
          const typeName = elems[1].value;
          const ctorForm = elems[2];
          if (ctorForm.tag !== 'list' || ctorForm.elements.length < 1 || ctorForm.elements[0].tag !== 'symbol')
            throw new EvalError(`${fmtPos(expr.pos)}define-record-type: invalid constructor`);
          const ctorName = ctorForm.elements[0].value;
          const ctorFields = ctorForm.elements.slice(1).map(e => {
            if (e.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}define-record-type: field must be symbol`);
            return e.value;
          });
          if (elems[3].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}define-record-type: expected predicate name`);
          const predName = elems[3].value;
          const fieldAccessors: { field: string; accessor: string }[] = [];
          for (let i = 4; i < elems.length; i++) {
            const fd = elems[i];
            if (fd.tag !== 'list' || fd.elements.length < 2 || fd.elements[0].tag !== 'symbol' || fd.elements[1].tag !== 'symbol')
              throw new EvalError(`${fmtPos(expr.pos)}define-record-type: invalid field spec`);
            fieldAccessors.push({ field: fd.elements[0].value, accessor: fd.elements[1].value });
          }
          // Define constructor
          envDefine(env, ctorName, { tag: 'builtin', name: ctorName, func: (args, callPos) => {
            if (args.length !== ctorFields.length)
              throw new EvalError(`${fmtPos(callPos)}${ctorName}: expected ${ctorFields.length} arguments, got ${args.length}`);
            const fields = new Map<string, SchemeVal>();
            for (let i = 0; i < ctorFields.length; i++) {
              fields.set(ctorFields[i], args[i]);
            }
            return { tag: 'record', typeName, fields };
          }});
          // Define predicate
          envDefine(env, predName, { tag: 'builtin', name: predName, func: (args, callPos) => {
            if (args.length !== 1) throw new EvalError(`${fmtPos(callPos)}${predName}: expected 1 argument`);
            return { tag: 'boolean', value: args[0].tag === 'record' && args[0].typeName === typeName };
          }});
          // Define accessors
          for (const { field, accessor } of fieldAccessors) {
            envDefine(env, accessor, { tag: 'builtin', name: accessor, func: (args, callPos) => {
              if (args.length !== 1) throw new EvalError(`${fmtPos(callPos)}${accessor}: expected 1 argument`);
              if (args[0].tag !== 'record' || args[0].typeName !== typeName)
                throw new EvalError(`${fmtPos(callPos)}${accessor}: not a ${typeName}`);
              const val = args[0].fields.get(field);
              if (val === undefined) throw new EvalError(`${fmtPos(callPos)}${accessor}: field ${field} not found`);
              return val;
            }});
          }
          return { tag: 'void' };
        }

        if (name === 'define-syntax') {
          if (elems.length !== 3) throw new EvalError(`${fmtPos(expr.pos)}define-syntax: expected 2 args`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}define-syntax: expected symbol`);
          const sr = elems[2];
          if (sr.tag !== 'list' || sr.elements.length < 2 ||
              sr.elements[0].tag !== 'symbol' || sr.elements[0].value !== 'syntax-rules')
            throw new EvalError(`${fmtPos(expr.pos)}define-syntax: expected syntax-rules`);
          if (sr.elements[1].tag !== 'list')
            throw new EvalError(`${fmtPos(expr.pos)}syntax-rules: expected literals list`);
          const literals = sr.elements[1].elements.map(e => {
            if (e.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}syntax-rules: literals must be symbols`);
            return e.value;
          });
          const rules: { pattern: SchemeVal; template: SchemeVal }[] = [];
          for (let i = 2; i < sr.elements.length; i++) {
            const rule = sr.elements[i];
            if (rule.tag !== 'list' || rule.elements.length !== 2)
              throw new EvalError(`${fmtPos(expr.pos)}syntax-rules: invalid rule`);
            rules.push({ pattern: rule.elements[0], template: rule.elements[1] });
          }
          envDefine(env, elems[1].value, { tag: 'macro', literals, rules, defEnv: env });
          return { tag: 'void' };
        }

        // Macro expansion
        const maybeMacro = envLookup(env, name);
        if (maybeMacro && maybeMacro.tag === 'macro') {
          const expanded = expandMacroCall(maybeMacro, expr);
          return evalScheme(expanded, env);
        }
      }

      // Function application
      const func = evalScheme(elems[0], env);
      const args = elems.slice(1).map(a => evalScheme(a, env));

      if (func.tag === 'builtin') {
        return func.func(args, expr.pos);
      }

      if (func.tag === 'lambda') {
        if (func.restParam) {
          if (args.length < func.params.length) throw new EvalError(`${fmtPos(expr.pos)}wrong number of arguments`);
        } else {
          if (args.length !== func.params.length) throw new EvalError(`${fmtPos(expr.pos)}wrong number of arguments`);
        }
        const callEnv = childEnv(func.env);
        for (let i = 0; i < func.params.length; i++) {
          envDefine(callEnv, func.params[i], args[i]);
        }
        if (func.restParam) {
          envDefine(callEnv, func.restParam, { tag: 'list', elements: args.slice(func.params.length) });
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of func.body) {
          result = evalScheme(bodyExpr, callEnv);
        }
        return result;
      }

      if (func.tag === 'case-lambda') {
        return applyCaseLambda(func, args, expr.pos);
      }

      throw new EvalError(`${fmtPos(expr.pos)}not a procedure: ${schemeToString(func)}`);
    }

    case 'builtin':
    case 'lambda':
    case 'case-lambda':
    case 'macro':
    case 'record':
    case 'void':
      return expr;
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evalScheme(expr, env);
  }
  return schemeToString(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evalScheme(expr, env);
  }
  return { result: schemeToString(result), output: outputBuf.join('') };
}
