import { EvalError } from './evalError.js';

// --- Types ---

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; exact?: boolean; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; mutable?: boolean; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; dotted?: boolean; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'vector'; elements: SchemeVal[]; pos?: Pos }
  | { tag: 'builtin'; name: string; func: (args: SchemeVal[], callPos?: Pos) => SchemeVal; pos?: Pos }
  | { tag: 'lambda'; params: string[]; restParam?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'macro'; literals: string[]; rules: { pattern: SchemeVal; template: SchemeVal }[]; defEnv: Env; pos?: Pos }
  | { tag: 'record'; typeName: string; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'case-lambda'; clauses: { params: string[]; restParam?: string; body: SchemeVal[] }[]; env: Env; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'continuation'; kont: Kont; windStack?: any; pos?: Pos };

interface WindFrame { inThunk: SchemeVal; outThunk: SchemeVal }

// --- CEK Machine Continuation Types ---

class ContinuationReturn {
  constructor(public kont: Kont, public value: SchemeVal, public windStack: WindFrame[] = []) {}
}

type Kont = KontFrame | DwFrame | null;

type KontFrame = { next: Kont } & KontData;

// Dynamic-wind continuation frame (separate from KontData to avoid TypeScript union size issues)
interface DwFrame {
  tag: 'dw';
  phase: number;
  next: Kont;
  bodyThunk?: SchemeVal; outThunk?: SchemeVal; inThunk?: SchemeVal;
  result?: SchemeVal;
  unwindOuts?: SchemeVal[]; rewindFrames?: WindFrame[];
}

type KontData =
  | { tag: 'seq'; exprs: SchemeVal[]; idx: number; env: Env }
  | { tag: 'define'; name: string; env: Env }
  | { tag: 'set'; name: string; env: Env }
  | { tag: 'if-test'; thenE: SchemeVal; elseE?: SchemeVal; env: Env }
  | { tag: 'ev-args'; done: SchemeVal[]; allElems: SchemeVal[]; idx: number; env: Env; pos?: Pos }
  | { tag: 'and-k'; rest: SchemeVal[]; env: Env }
  | { tag: 'or-k'; rest: SchemeVal[]; env: Env }
  | { tag: 'negate' }
  | { tag: 'let-init'; names: string[]; vals: SchemeVal[]; inits: SchemeVal[]; body: SchemeVal[]; outerEnv: Env; pos?: Pos }
  | { tag: 'letstar-bind'; bindings: SchemeVal[]; idx: number; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'letrec-init'; names: string[]; inits: SchemeVal[]; idx: number; vals: SchemeVal[]; body: SchemeVal[]; env: Env; sequential: boolean; pos?: Pos }
  | { tag: 'named-let-init'; loopName: string; params: string[]; inits: SchemeVal[]; idx: number; vals: SchemeVal[]; body: SchemeVal[]; outerEnv: Env; pos?: Pos }
  | { tag: 'case-key'; clauses: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'cond-test'; clauseElems: SchemeVal[]; restClauses: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'cond-arrow'; testVal: SchemeVal; env: Env; pos?: Pos }
  | { tag: 'str-set-idx'; strVal: SchemeVal; charExpr: SchemeVal; env: Env; pos?: Pos }
  | { tag: 'str-set-char'; strVal: SchemeVal; idx: number; pos?: Pos }
  | { tag: 'do-init'; specs: {name: string; initExpr: SchemeVal; stepExpr?: SchemeVal}[]; idx: number; vals: SchemeVal[]; testExpr: SchemeVal; resultExprs: SchemeVal[]; bodyExprs: SchemeVal[]; outerEnv: Env; pos?: Pos }
  | { tag: 'do-test'; varNames: string[]; stepExprs: (SchemeVal|undefined)[]; testExpr: SchemeVal; resultExprs: SchemeVal[]; bodyExprs: SchemeVal[]; env: Env }
  | { tag: 'do-body'; varNames: string[]; stepExprs: (SchemeVal|undefined)[]; testExpr: SchemeVal; resultExprs: SchemeVal[]; bodyExprs: SchemeVal[]; bodyIdx: number; env: Env }
  | { tag: 'do-step'; varNames: string[]; stepExprs: (SchemeVal|undefined)[]; stepIdx: number; newVals: (SchemeVal|undefined)[]; testExpr: SchemeVal; resultExprs: SchemeVal[]; bodyExprs: SchemeVal[]; env: Env }
;

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
    if (ch === '#' && i + 1 < input.length && input[i + 1] === '(') {
      tokens.push({ text: '#(', pos: { line, col } }); advance(); advance(); continue;
    }
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

  if (token.text === '#(') {
    const elements: SchemeVal[] = [];
    idx++;
    while (idx < tokens.length && tokens[idx].text !== ')') {
      const [val, next] = parse(tokens, idx);
      elements.push(val);
      idx = next;
    }
    if (idx >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'vector', elements, pos: p }, idx + 1];
  }

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

function schemeToString(val: SchemeVal, seen?: Set<SchemeVal>): string {
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
        const init = val.elements.slice(0, -1).map(e => schemeToString(e, seen)).join(' ');
        return `(${init} . ${schemeToString(val.elements[val.elements.length - 1], seen)})`;
      }
      return `(${val.elements.map(e => schemeToString(e, seen)).join(' ')})`;
    }
    case 'pair': {
      if (!seen) seen = new Set();
      let s = '(';
      let cur: SchemeVal = val;
      let first = true;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { s += first ? '...' : ' ...'; break; }
        seen.add(cur);
        if (!first) s += ' ';
        s += schemeToString(cur.car, seen);
        first = false;
        cur = cur.cdr;
      }
      if (cur.tag === 'pair') { /* cycle detected, already handled */ }
      else if (cur.tag === 'list' && !cur.dotted && cur.elements.length > 0) {
        // Inline list elements (e.g., pair consed onto quoted list)
        for (const el of cur.elements) {
          s += ' ' + schemeToString(el, seen);
        }
      } else if (!isNullVal(cur)) {
        s += ' . ' + schemeToString(cur, seen);
      }
      s += ')';
      return s;
    }
    case 'vector': return `#(${val.elements.map(e => schemeToString(e, seen)).join(' ')})`;
    case 'builtin': return `#<procedure:${val.name}>`;
    case 'lambda': return '#<procedure>';
    case 'case-lambda': return '#<procedure>';
    case 'macro': return '#<macro>';
    case 'record': return `#<record:${val.typeName}>`;
    case 'continuation': return '#<continuation>';
    case 'void': return '';
  }
}

function displayString(val: SchemeVal, seen?: Set<SchemeVal>): string {
  switch (val.tag) {
    case 'string': return val.value;
    case 'char': return val.value;
    case 'list': {
      if (val.dotted && val.elements.length >= 2) {
        const init = val.elements.slice(0, -1).map(e => displayString(e, seen)).join(' ');
        return `(${init} . ${displayString(val.elements[val.elements.length - 1], seen)})`;
      }
      return `(${val.elements.map(e => displayString(e, seen)).join(' ')})`;
    }
    case 'pair': {
      if (!seen) seen = new Set();
      let s = '(';
      let cur: SchemeVal = val;
      let first = true;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { s += first ? '...' : ' ...'; break; }
        seen.add(cur);
        if (!first) s += ' ';
        s += displayString(cur.car, seen);
        first = false;
        cur = cur.cdr;
      }
      if (cur.tag === 'pair') { /* cycle */ }
      else if (cur.tag === 'list' && !cur.dotted && cur.elements.length > 0) {
        for (const el of cur.elements) {
          s += ' ' + displayString(el, seen);
        }
      } else if (!isNullVal(cur)) {
        s += ' . ' + displayString(cur, seen);
      }
      s += ')';
      return s;
    }
    case 'vector': return `#(${val.elements.map(e => displayString(e, seen)).join(' ')})`;
    default: return schemeToString(val, seen);
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

// --- Pair/List helpers ---

const NIL: SchemeVal = { tag: 'list', elements: [] };

function isNullVal(v: SchemeVal): boolean {
  return v.tag === 'list' && v.elements.length === 0;
}

function arrayToSchemeList(arr: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: arr[i], cdr: result };
  }
  return result;
}

function toArray(val: SchemeVal): SchemeVal[] {
  if (val.tag === 'list') return val.elements;
  const result: SchemeVal[] = [];
  let cur: SchemeVal = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag === 'list' && !cur.dotted) {
    result.push(...cur.elements);
  }
  return result;
}

function isPairLike(v: SchemeVal): boolean {
  return v.tag === 'pair' || (v.tag === 'list' && v.elements.length > 0);
}

let _nextId = 1;
const _idMap = new WeakMap<object, number>();
function idOf(obj: SchemeVal): number {
  let id = _idMap.get(obj);
  if (id === undefined) { id = _nextId++; _idMap.set(obj, id); }
  return id;
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
    return { tag: 'pair', car: args[0], cdr: args[1] };
  });

  defBuiltin('car', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}car: expected 1 arg`);
    const v = args[0];
    if (v.tag === 'pair') return v.car;
    if (v.tag === 'list' && v.elements.length > 0) return v.elements[0];
    throw new EvalError(`${fmtPos(p)}car: expected pair`);
  });

  defBuiltin('cdr', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdr: expected 1 arg`);
    const v = args[0];
    if (v.tag === 'pair') return v.cdr;
    if (v.tag === 'list' && v.elements.length > 0) {
      if (v.dotted && v.elements.length === 2) return v.elements[1];
      const rest = v.elements.slice(1);
      return { tag: 'list', elements: rest, dotted: v.dotted } as SchemeVal;
    }
    throw new EvalError(`${fmtPos(p)}cdr: expected pair`);
  });

  // cxr helpers
  const getCar = (v: SchemeVal, p?: Pos): SchemeVal => {
    if (v.tag === 'pair') return v.car;
    if (v.tag === 'list' && v.elements.length > 0) return v.elements[0];
    throw new EvalError(`${fmtPos(p)}car: expected pair`);
  };
  const getCdr = (v: SchemeVal, p?: Pos): SchemeVal => {
    if (v.tag === 'pair') return v.cdr;
    if (v.tag === 'list' && v.elements.length > 0) {
      if (v.dotted && v.elements.length === 2) return v.elements[1];
      return { tag: 'list', elements: v.elements.slice(1), dotted: v.dotted } as SchemeVal;
    }
    throw new EvalError(`${fmtPos(p)}cdr: expected pair`);
  };
  defBuiltin('caar', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}caar: expected 1 arg`); return getCar(getCar(args[0], p), p); });
  defBuiltin('cadr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cadr: expected 1 arg`); return getCar(getCdr(args[0], p), p); });
  defBuiltin('cdar', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdar: expected 1 arg`); return getCdr(getCar(args[0], p), p); });
  defBuiltin('cddr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cddr: expected 1 arg`); return getCdr(getCdr(args[0], p), p); });
  defBuiltin('caaar', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}caaar: expected 1 arg`); return getCar(getCar(getCar(args[0], p), p), p); });
  defBuiltin('caadr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}caadr: expected 1 arg`); return getCar(getCar(getCdr(args[0], p), p), p); });
  defBuiltin('cdaar', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdaar: expected 1 arg`); return getCdr(getCar(getCar(args[0], p), p), p); });
  defBuiltin('cdadr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdadr: expected 1 arg`); return getCdr(getCar(getCdr(args[0], p), p), p); });
  defBuiltin('caddr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}caddr: expected 1 arg`); return getCar(getCdr(getCdr(args[0], p), p), p); });
  defBuiltin('cdddr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdddr: expected 1 arg`); return getCdr(getCdr(getCdr(args[0], p), p), p); });
  defBuiltin('cadaar', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cadaar: expected 1 arg`); return getCar(getCdr(getCar(getCar(args[0], p), p), p), p); });
  defBuiltin('caddar', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}caddar: expected 1 arg`); return getCar(getCdr(getCdr(getCar(args[0], p), p), p), p); });
  defBuiltin('cadddr', (args, p) => { if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cadddr: expected 1 arg`); return getCar(getCdr(getCdr(getCdr(args[0], p), p), p), p); });

  defBuiltin('memq', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}memq: expected 2 args`);
    let cur: SchemeVal = args[1];
    while (cur.tag === 'pair') {
      if (cur.car === args[0] || (cur.car.tag === args[0].tag &&
        ((cur.car.tag === 'symbol' && args[0].tag === 'symbol' && cur.car.value === args[0].value) ||
         (cur.car.tag === 'number' && args[0].tag === 'number' && cur.car.value === args[0].value) ||
         (cur.car.tag === 'boolean' && args[0].tag === 'boolean' && cur.car.value === args[0].value) ||
         (cur.car.tag === 'char' && args[0].tag === 'char' && cur.car.value === args[0].value))))
        return cur;
      cur = cur.cdr;
    }
    if (cur.tag === 'list') {
      for (let i = 0; i < cur.elements.length; i++) {
        const el = cur.elements[i];
        if (el === args[0] || (el.tag === args[0].tag &&
          ((el.tag === 'symbol' && args[0].tag === 'symbol' && el.value === args[0].value) ||
           (el.tag === 'number' && args[0].tag === 'number' && el.value === args[0].value) ||
           (el.tag === 'boolean' && args[0].tag === 'boolean' && el.value === args[0].value) ||
           (el.tag === 'char' && args[0].tag === 'char' && el.value === args[0].value))))
          return { tag: 'list', elements: cur.elements.slice(i), dotted: cur.dotted } as SchemeVal;
      }
    }
    return { tag: 'boolean', value: false };
  });

  defBuiltin('memv', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}memv: expected 2 args`);
    // Same as memq for our purposes (eqv? semantics)
    let cur: SchemeVal = args[1];
    while (cur.tag === 'pair') {
      if (cur.car === args[0] || (cur.car.tag === args[0].tag &&
        ((cur.car.tag === 'symbol' && args[0].tag === 'symbol' && cur.car.value === args[0].value) ||
         (cur.car.tag === 'number' && args[0].tag === 'number' && cur.car.value === args[0].value) ||
         (cur.car.tag === 'boolean' && args[0].tag === 'boolean' && cur.car.value === args[0].value) ||
         (cur.car.tag === 'char' && args[0].tag === 'char' && cur.car.value === args[0].value))))
        return cur;
      cur = cur.cdr;
    }
    if (cur.tag === 'list') {
      for (let i = 0; i < cur.elements.length; i++) {
        const el = cur.elements[i];
        if (el === args[0] || (el.tag === args[0].tag &&
          ((el.tag === 'symbol' && args[0].tag === 'symbol' && el.value === args[0].value) ||
           (el.tag === 'number' && args[0].tag === 'number' && el.value === args[0].value) ||
           (el.tag === 'boolean' && args[0].tag === 'boolean' && el.value === args[0].value) ||
           (el.tag === 'char' && args[0].tag === 'char' && el.value === args[0].value))))
          return { tag: 'list', elements: cur.elements.slice(i), dotted: cur.dotted } as SchemeVal;
      }
    }
    return { tag: 'boolean', value: false };
  });

  defBuiltin('member', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}member: expected 2 args`);
    let cur: SchemeVal = args[1];
    while (cur.tag === 'pair') {
      if (schemeEqual(cur.car, args[0])) return cur;
      cur = cur.cdr;
    }
    if (cur.tag === 'list') {
      for (let i = 0; i < cur.elements.length; i++) {
        if (schemeEqual(cur.elements[i], args[0]))
          return { tag: 'list', elements: cur.elements.slice(i), dotted: cur.dotted } as SchemeVal;
      }
    }
    return { tag: 'boolean', value: false };
  });

  defBuiltin('assq', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}assq: expected 2 args`);
    const key = args[0];
    const elems = toArray(args[1]);
    for (const pair of elems) {
      if (pair.tag === 'pair') {
        if (pair.car === key || (pair.car.tag === key.tag &&
          ((pair.car.tag === 'symbol' && key.tag === 'symbol' && pair.car.value === key.value) ||
           (pair.car.tag === 'number' && key.tag === 'number' && pair.car.value === key.value) ||
           (pair.car.tag === 'boolean' && key.tag === 'boolean' && pair.car.value === key.value))))
          return pair;
      } else if (pair.tag === 'list' && pair.elements.length >= 2) {
        const pCar = pair.elements[0];
        if (pCar === key || (pCar.tag === key.tag &&
          ((pCar.tag === 'symbol' && key.tag === 'symbol' && pCar.value === key.value) ||
           (pCar.tag === 'number' && key.tag === 'number' && pCar.value === key.value) ||
           (pCar.tag === 'boolean' && key.tag === 'boolean' && pCar.value === key.value))))
          return pair;
      }
    }
    return { tag: 'boolean', value: false };
  });

  defBuiltin('assv', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}assv: expected 2 args`);
    // Same as assq for our purposes
    const key = args[0];
    const elems = toArray(args[1]);
    for (const pair of elems) {
      if (pair.tag === 'pair') {
        if (pair.car === key || (pair.car.tag === key.tag &&
          ((pair.car.tag === 'symbol' && key.tag === 'symbol' && pair.car.value === key.value) ||
           (pair.car.tag === 'number' && key.tag === 'number' && pair.car.value === key.value) ||
           (pair.car.tag === 'boolean' && key.tag === 'boolean' && pair.car.value === key.value))))
          return pair;
      } else if (pair.tag === 'list' && pair.elements.length >= 2) {
        const pCar = pair.elements[0];
        if (pCar === key || (pCar.tag === key.tag &&
          ((pCar.tag === 'symbol' && key.tag === 'symbol' && pCar.value === key.value) ||
           (pCar.tag === 'number' && key.tag === 'number' && pCar.value === key.value) ||
           (pCar.tag === 'boolean' && key.tag === 'boolean' && pCar.value === key.value))))
          return pair;
      }
    }
    return { tag: 'boolean', value: false };
  });

  // 4-level cxr functions
  defBuiltin('caaaar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}caaaar: expected 1 arg`); return getCar(getCar(getCar(getCar(a[0], p), p), p), p); });
  defBuiltin('caaadr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}caaadr: expected 1 arg`); return getCar(getCar(getCar(getCdr(a[0], p), p), p), p); });
  defBuiltin('caadar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}caadar: expected 1 arg`); return getCar(getCar(getCdr(getCar(a[0], p), p), p), p); });
  defBuiltin('caaddr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}caaddr: expected 1 arg`); return getCar(getCar(getCdr(getCdr(a[0], p), p), p), p); });
  defBuiltin('cadaar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cadaar: expected 1 arg`); return getCar(getCdr(getCar(getCar(a[0], p), p), p), p); });
  defBuiltin('cadadr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cadadr: expected 1 arg`); return getCar(getCdr(getCar(getCdr(a[0], p), p), p), p); });
  defBuiltin('cdaaar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cdaaar: expected 1 arg`); return getCdr(getCar(getCar(getCar(a[0], p), p), p), p); });
  defBuiltin('cdaadr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cdaadr: expected 1 arg`); return getCdr(getCar(getCar(getCdr(a[0], p), p), p), p); });
  defBuiltin('cdadar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cdadar: expected 1 arg`); return getCdr(getCar(getCdr(getCar(a[0], p), p), p), p); });
  defBuiltin('cdaddr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cdaddr: expected 1 arg`); return getCdr(getCar(getCdr(getCdr(a[0], p), p), p), p); });
  defBuiltin('cddaar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cddaar: expected 1 arg`); return getCdr(getCdr(getCar(getCar(a[0], p), p), p), p); });
  defBuiltin('cddadr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cddadr: expected 1 arg`); return getCdr(getCdr(getCar(getCdr(a[0], p), p), p), p); });
  defBuiltin('cdddar', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cdddar: expected 1 arg`); return getCdr(getCdr(getCdr(getCar(a[0], p), p), p), p); });
  defBuiltin('cddddr', (a, p) => { if (a.length !== 1) throw new EvalError(`${fmtPos(p)}cddddr: expected 1 arg`); return getCdr(getCdr(getCdr(getCdr(a[0], p), p), p), p); });

  defBuiltin('gcd', (args, p) => {
    if (args.length === 0) return { tag: 'number', value: 0 };
    const nums = expectNumbers(args, 'gcd', p);
    let result = Math.abs(nums[0]);
    for (let i = 1; i < nums.length; i++) result = gcd(result, Math.abs(nums[i]));
    return { tag: 'number', value: result };
  });

  defBuiltin('lcm', (args, p) => {
    if (args.length === 0) return { tag: 'number', value: 1 };
    const nums = expectNumbers(args, 'lcm', p);
    let result = Math.abs(nums[0]);
    for (let i = 1; i < nums.length; i++) {
      const b = Math.abs(nums[i]);
      result = (result / gcd(result, b)) * b;
    }
    return { tag: 'number', value: result };
  });

  defBuiltin('truncate', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}truncate: expected 1 arg`);
    return { tag: 'number', value: Math.trunc(toFloat(args[0], 'truncate', p)) };
  });

  defBuiltin('round', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}round: expected 1 arg`);
    return { tag: 'number', value: Math.round(toFloat(args[0], 'round', p)) };
  });

  defBuiltin('floor', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}floor: expected 1 arg`);
    return { tag: 'number', value: Math.floor(toFloat(args[0], 'floor', p)) };
  });

  defBuiltin('ceiling', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}ceiling: expected 1 arg`);
    return { tag: 'number', value: Math.ceil(toFloat(args[0], 'ceiling', p)) };
  });

  defBuiltin('sqrt', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}sqrt: expected 1 arg`);
    return { tag: 'number', value: Math.sqrt(toFloat(args[0], 'sqrt', p)), exact: false };
  });

  defBuiltin('make-string', (args, p) => {
    if (args.length < 1 || args.length > 2) throw new EvalError(`${fmtPos(p)}make-string: expected 1-2 args`);
    if (args[0].tag !== 'number') throw new EvalError(`${fmtPos(p)}make-string: expected number`);
    const ch = args.length === 2 && args[1].tag === 'char' ? args[1].value : '\0';
    return { tag: 'string', value: ch.repeat(args[0].value), mutable: true };
  });

  defBuiltin('string', (args) => {
    return { tag: 'string', value: args.map(a => {
      if (a.tag !== 'char') throw new EvalError('string: expected char');
      return a.value;
    }).join(''), mutable: true };
  });

  defBuiltin('string>?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string>?: expected 2 args`);
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${fmtPos(p)}string>?: expected strings`);
    return { tag: 'boolean', value: args[0].value > args[1].value };
  });

  defBuiltin('string<=?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string<=?: expected 2 args`);
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${fmtPos(p)}string<=?: expected strings`);
    return { tag: 'boolean', value: args[0].value <= args[1].value };
  });

  defBuiltin('string>=?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string>=?: expected 2 args`);
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${fmtPos(p)}string>=?: expected strings`);
    return { tag: 'boolean', value: args[0].value >= args[1].value };
  });

  defBuiltin('real?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}real?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'number' || args[0].tag === 'rational' };
  });

  defBuiltin('not', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}not: expected 1 arg`);
    return { tag: 'boolean', value: !isTruthy(args[0]) };
  });

  defBuiltin('reverse', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}reverse: expected 1 arg`);
    if (args[0].tag !== 'list' && args[0].tag !== 'pair') throw new EvalError(`${fmtPos(p)}reverse: expected list`);
    return arrayToSchemeList(toArray(args[0]).reverse());
  });

  defBuiltin('null?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}null?: expected 1 arg`);
    return { tag: 'boolean', value: isNullVal(args[0]) };
  });

  defBuiltin('list', (args) => {
    return arrayToSchemeList(args);
  });

  defBuiltin('length', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}length: expected 1 arg`);
    if (args[0].tag === 'list') return { tag: 'number', value: args[0].elements.length };
    if (args[0].tag === 'pair') return { tag: 'number', value: toArray(args[0]).length };
    throw new EvalError(`${fmtPos(p)}length: expected list`);
  });

  defBuiltin('append', (args, p) => {
    if (args.length === 0) return NIL;
    const result: SchemeVal[] = [];
    for (let i = 0; i < args.length - 1; i++) {
      const arg = args[i];
      if (arg.tag !== 'list' && arg.tag !== 'pair') throw new EvalError(`${fmtPos(p)}append: expected list`);
      result.push(...toArray(arg));
    }
    const last = args[args.length - 1];
    if (last.tag === 'list' || last.tag === 'pair') {
      result.push(...toArray(last));
      return arrayToSchemeList(result);
    }
    // Last arg can be non-list (improper list)
    if (result.length === 0) return last;
    let tail: SchemeVal = last;
    for (let i = result.length - 1; i >= 0; i--) {
      tail = { tag: 'pair', car: result[i], cdr: tail };
    }
    return tail;
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
    return { tag: 'boolean', value: isPairLike(args[0]) };
  });

  defBuiltin('symbol?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}symbol?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  defBuiltin('procedure?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}procedure?: expected 1 arg`);
    const t = args[0].tag;
    return { tag: 'boolean', value: t === 'builtin' || t === 'lambda' || t === 'case-lambda' || t === 'continuation' };
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
    return { tag: 'string', value: args[0].value, mutable: true };
  });

  defBuiltin('string->list', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string->list: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string->list: expected string`);
    const chars: SchemeVal[] = [...args[0].value].map(c => ({ tag: 'char' as const, value: c }));
    return arrayToSchemeList(chars);
  });

  defBuiltin('list->string', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}list->string: expected 1 arg`);
    if (args[0].tag !== 'list' && args[0].tag !== 'pair') throw new EvalError(`${fmtPos(p)}list->string: expected list`);
    const str = toArray(args[0]).map(e => {
      if (e.tag !== 'char') throw new EvalError(`${fmtPos(p)}list->string: expected list of chars`);
      return e.value;
    }).join('');
    return { tag: 'string', value: str, mutable: true };
  });

  defBuiltin('char->integer', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char->integer: expected 1 arg`);
    if (args[0].tag !== 'char') throw new EvalError(`${fmtPos(p)}char->integer: expected char`);
    return { tag: 'number', value: args[0].value.codePointAt(0)! };
  });

  defBuiltin('integer->char', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}integer->char: expected 1 arg`);
    if (args[0].tag !== 'number') throw new EvalError(`${fmtPos(p)}integer->char: expected number`);
    return { tag: 'char', value: String.fromCodePoint(args[0].value) };
  });

  defBuiltin('char?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'char' };
  });

  // eq? and equal?
  defBuiltin('eq?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}eq?: expected 2 args`);
    const [a, b] = args;
    if (a === b) return { tag: 'boolean', value: true };
    if (a.tag !== b.tag) {
      // Both nil representations are eq
      if (isNullVal(a) && isNullVal(b)) return { tag: 'boolean', value: true };
      return { tag: 'boolean', value: false };
    }
    if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', value: a.value === b.value };
    if (isNullVal(a) && isNullVal(b)) return { tag: 'boolean', value: true };
    if (a.tag === 'void' && b.tag === 'void') return { tag: 'boolean', value: true };
    return { tag: 'boolean', value: false };
  });

  defBuiltin('eqv?', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}eqv?: expected 2 args`);
    const [a, b] = args;
    if (a === b) return { tag: 'boolean', value: true };
    if (a.tag !== b.tag) {
      if (isNullVal(a) && isNullVal(b)) return { tag: 'boolean', value: true };
      return { tag: 'boolean', value: false };
    }
    if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', value: a.value === b.value };
    if (isNullVal(a) && isNullVal(b)) return { tag: 'boolean', value: true };
    if (a.tag === 'void' && b.tag === 'void') return { tag: 'boolean', value: true };
    return { tag: 'boolean', value: a === b };
  });

  const schemeEqual = (a: SchemeVal, b: SchemeVal, seen?: Set<string>): boolean => {
    if (a === b) return true;
    if ((a.tag === 'number' || a.tag === 'rational') && (b.tag === 'number' || b.tag === 'rational')) {
      if (a.tag === 'rational' && b.tag === 'rational') return a.num === b.num && a.den === b.den;
      if (a.tag === 'number' && b.tag === 'number') return a.value === b.value;
      return false;
    }
    if (isNullVal(a) && isNullVal(b)) return true;
    // Handle pair/list cross-comparison
    if (isPairLike(a) && isPairLike(b)) {
      if (!seen) seen = new Set();
      // Use object identity pair to detect cycles
      const key = `${idOf(a)},${idOf(b)}`;
      if (seen.has(key)) return true; // assume equal on cycle
      seen.add(key);
      const al = a as Extract<SchemeVal, { tag: 'list' }>;
      const bl = b as Extract<SchemeVal, { tag: 'list' }>;
      const aCar = a.tag === 'pair' ? a.car : al.elements[0];
      const aCdr = a.tag === 'pair' ? a.cdr : (al.dotted && al.elements.length === 2 ? al.elements[1] : { tag: 'list' as const, elements: al.elements.slice(1), dotted: al.dotted });
      const bCar = b.tag === 'pair' ? b.car : bl.elements[0];
      const bCdr = b.tag === 'pair' ? b.cdr : (bl.dotted && bl.elements.length === 2 ? bl.elements[1] : { tag: 'list' as const, elements: bl.elements.slice(1), dotted: bl.dotted });
      return schemeEqual(aCar, bCar, seen) && schemeEqual(aCdr, bCdr, seen);
    }
    if (a.tag !== b.tag) return false;
    if (a.tag === 'boolean' && b.tag === 'boolean') return a.value === b.value;
    if (a.tag === 'string' && b.tag === 'string') return a.value === b.value;
    if (a.tag === 'symbol' && b.tag === 'symbol') return a.value === b.value;
    if (a.tag === 'char' && b.tag === 'char') return a.value === b.value;
    if (a.tag === 'vector' && b.tag === 'vector') {
      if (a.elements.length !== b.elements.length) return false;
      return a.elements.every((el, i) => schemeEqual(el, b.elements[i], seen));
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
    const arrs = lists.map(l => {
      if (l.tag !== 'list' && l.tag !== 'pair') throw new EvalError(`${fmtPos(p)}map: expected list`);
      return toArray(l);
    });
    const len = arrs[0].length;
    const result: SchemeVal[] = [];
    for (let i = 0; i < len; i++) {
      const callArgs = arrs.map(a => a[i]);
      if (func.tag === 'builtin') {
        result.push(func.func(callArgs, p));
      } else if (func.tag === 'lambda') {
        const callEnv = childEnv(func.env);
        for (let j = 0; j < func.params.length; j++) {
          envDefine(callEnv, func.params[j], callArgs[j]);
        }
        if (func.restParam) {
          envDefine(callEnv, func.restParam, arrayToSchemeList(callArgs.slice(func.params.length)));
        }
        let res: SchemeVal = { tag: 'void' };
        for (const bodyExpr of func.body) {
          res = evalScheme(bodyExpr, callEnv);
        }
        result.push(res);
      } else if (func.tag === 'case-lambda') {
        result.push(applyCaseLambda(func, callArgs, p));
      } else if (func.tag === 'continuation') {
        throw new ContinuationReturn(func.kont, callArgs[0] ?? { tag: 'void' }, func.windStack);
      } else {
        throw new EvalError(`${fmtPos(p)}map: not a procedure`);
      }
    }
    return arrayToSchemeList(result);
  });

  // for-each (like map but returns void)
  defBuiltin('for-each', (args, p) => {
    if (args.length < 2) throw new EvalError(`${fmtPos(p)}for-each: expected at least 2 args`);
    const func = args[0];
    const lists = args.slice(1);
    const arrs = lists.map(l => {
      if (l.tag !== 'list' && l.tag !== 'pair') throw new EvalError(`${fmtPos(p)}for-each: expected list`);
      return toArray(l);
    });
    const len = arrs[0].length;
    for (let i = 0; i < len; i++) {
      const callArgs = arrs.map(a => a[i]);
      if (func.tag === 'builtin') {
        func.func(callArgs, p);
      } else if (func.tag === 'lambda') {
        const callEnv = childEnv(func.env);
        for (let j = 0; j < func.params.length; j++) {
          envDefine(callEnv, func.params[j], callArgs[j]);
        }
        if (func.restParam) {
          envDefine(callEnv, func.restParam, arrayToSchemeList(callArgs.slice(func.params.length)));
        }
        for (const bodyExpr of func.body) {
          evalScheme(bodyExpr, callEnv);
        }
      } else if (func.tag === 'case-lambda') {
        applyCaseLambda(func, callArgs, p);
      } else if (func.tag === 'continuation') {
        throw new ContinuationReturn(func.kont, callArgs[0] ?? { tag: 'void' }, func.windStack);
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
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}list-ref: expected number`);
    const idx = args[1].value;
    if (args[0].tag === 'pair') {
      let cur: SchemeVal = args[0];
      for (let i = 0; i < idx; i++) {
        if (cur.tag !== 'pair') throw new EvalError(`${fmtPos(p)}list-ref: index out of range`);
        cur = cur.cdr;
      }
      if (cur.tag === 'pair') return cur.car;
      if (cur.tag === 'list' && cur.elements.length > 0) return cur.elements[0];
      throw new EvalError(`${fmtPos(p)}list-ref: index out of range`);
    }
    if (args[0].tag === 'list') {
      if (idx < 0 || idx >= args[0].elements.length) throw new EvalError(`${fmtPos(p)}list-ref: index out of range`);
      return args[0].elements[idx];
    }
    throw new EvalError(`${fmtPos(p)}list-ref: expected list`);
  });

  defBuiltin('list-tail', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}list-tail: expected 2 args`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}list-tail: expected number`);
    const idx = args[1].value;
    if (args[0].tag === 'pair') {
      let cur: SchemeVal = args[0];
      for (let i = 0; i < idx; i++) {
        if (cur.tag !== 'pair') throw new EvalError(`${fmtPos(p)}list-tail: index out of range`);
        cur = cur.cdr;
      }
      return cur;
    }
    if (args[0].tag === 'list') {
      return { tag: 'list', elements: args[0].elements.slice(idx) } as SchemeVal;
    }
    throw new EvalError(`${fmtPos(p)}list-tail: expected list`);
  });

  defBuiltin('list?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}list?: expected 1 arg`);
    const v = args[0];
    if (v.tag === 'list') return { tag: 'boolean', value: !v.dotted };
    if (v.tag !== 'pair') return { tag: 'boolean', value: false };
    // Tortoise-and-hare cycle detection
    let slow: SchemeVal = v;
    let fast: SchemeVal = v;
    while (true) {
      if (fast.tag !== 'pair') return { tag: 'boolean', value: isNullVal(fast) };
      fast = fast.cdr;
      if (fast.tag !== 'pair') return { tag: 'boolean', value: isNullVal(fast) };
      fast = fast.cdr;
      slow = (slow as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).cdr;
      if (slow === fast) return { tag: 'boolean', value: false }; // cycle
    }
  });

  defBuiltin('set-car!', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}set-car!: expected 2 args`);
    if (args[0].tag !== 'pair') throw new EvalError(`${fmtPos(p)}set-car!: expected pair`);
    args[0].car = args[1];
    return { tag: 'void' };
  });

  defBuiltin('set-cdr!', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}set-cdr!: expected 2 args`);
    if (args[0].tag !== 'pair') throw new EvalError(`${fmtPos(p)}set-cdr!: expected pair`);
    args[0].cdr = args[1];
    return { tag: 'void' };
  });

  defBuiltin('assoc', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}assoc: expected 2 args`);
    const key = args[0];
    const alist = args[1];
    if (alist.tag !== 'list' && alist.tag !== 'pair') throw new EvalError(`${fmtPos(p)}assoc: expected list`);
    const elems = toArray(alist);
    for (const pair of elems) {
      if (isPairLike(pair)) {
        const pairCar = pair.tag === 'pair' ? pair.car : (pair as Extract<SchemeVal, { tag: 'list' }>).elements[0];
        if (schemeEqual(pairCar, key)) return pair;
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
    if (last.tag !== 'list' && last.tag !== 'pair') throw new EvalError(`${fmtPos(p)}apply: last argument must be a list`);
    const callArgs = [...args.slice(1, -1), ...toArray(last)];
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
        envDefine(callEnv, func.restParam, arrayToSchemeList(callArgs.slice(func.params.length)));
      }
      let result: SchemeVal = { tag: 'void' };
      for (const bodyExpr of func.body) {
        result = evalScheme(bodyExpr, callEnv);
      }
      return result;
    }
    if (func.tag === 'case-lambda') return applyCaseLambda(func, callArgs, p);
    if (func.tag === 'continuation') throw new ContinuationReturn(func.kont, callArgs[0] ?? { tag: 'void' }, func.windStack);
    throw new EvalError(`${fmtPos(p)}apply: not a procedure`);
  });

  // L14: Vector operations
  defBuiltin('vector', (args) => {
    return { tag: 'vector', elements: [...args] };
  });

  defBuiltin('make-vector', (args, p) => {
    if (args.length < 1 || args.length > 2) throw new EvalError(`${fmtPos(p)}make-vector: expected 1-2 args`);
    if (args[0].tag !== 'number') throw new EvalError(`${fmtPos(p)}make-vector: expected number`);
    const len = args[0].value;
    const fill: SchemeVal = args.length === 2 ? args[1] : { tag: 'number', value: 0 };
    const elements: SchemeVal[] = [];
    for (let i = 0; i < len; i++) elements.push(fill);
    return { tag: 'vector', elements };
  });

  defBuiltin('vector-ref', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}vector-ref: expected 2 args`);
    if (args[0].tag !== 'vector') throw new EvalError(`${fmtPos(p)}vector-ref: expected vector`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}vector-ref: expected number`);
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError(`${fmtPos(p)}vector-ref: index out of range`);
    return args[0].elements[idx];
  });

  defBuiltin('vector-set!', (args, p) => {
    if (args.length !== 3) throw new EvalError(`${fmtPos(p)}vector-set!: expected 3 args`);
    if (args[0].tag !== 'vector') throw new EvalError(`${fmtPos(p)}vector-set!: expected vector`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}vector-set!: expected number`);
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError(`${fmtPos(p)}vector-set!: index out of range`);
    args[0].elements[idx] = args[2];
    return { tag: 'void' };
  });

  defBuiltin('vector-length', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}vector-length: expected 1 arg`);
    if (args[0].tag !== 'vector') throw new EvalError(`${fmtPos(p)}vector-length: expected vector`);
    return { tag: 'number', value: args[0].elements.length };
  });

  defBuiltin('vector?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}vector?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'vector' };
  });

  defBuiltin('vector->list', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}vector->list: expected 1 arg`);
    if (args[0].tag !== 'vector') throw new EvalError(`${fmtPos(p)}vector->list: expected vector`);
    return arrayToSchemeList([...args[0].elements]);
  });

  defBuiltin('list->vector', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}list->vector: expected 1 arg`);
    if (args[0].tag !== 'list' && args[0].tag !== 'pair') throw new EvalError(`${fmtPos(p)}list->vector: expected list`);
    return { tag: 'vector', elements: [...toArray(args[0])] };
  });

  // call/cc — handled specially by the CEK machine; the func is never called directly
  const callccBuiltin: SchemeVal = { tag: 'builtin', name: 'call/cc', func: () => { throw new Error('call/cc: must be intercepted by CEK machine'); } };
  envDefine(env, 'call/cc', callccBuiltin);
  envDefine(env, 'call-with-current-continuation', callccBuiltin);

  // dynamic-wind — handled specially by the CEK machine
  const dwBuiltin: SchemeVal = { tag: 'builtin', name: 'dynamic-wind', func: () => { throw new Error('dynamic-wind: must be intercepted by CEK machine'); } };
  envDefine(env, 'dynamic-wind', dwBuiltin);

  return env;
}

// --- Macro support ---

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'lambda', 'and', 'or', 'not', 'begin',
  'cond', 'set!', 'string-set!', 'let', 'let*', 'letrec', 'letrec*', 'case', 'do',
  'define-syntax', 'define-record-type', 'case-lambda'
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
        envDefine(callEnv, clause.restParam, arrayToSchemeList(args.slice(clause.params.length)));
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

function evalScheme(initExpr: SchemeVal, initEnv: Env): SchemeVal {
  let ctrl: SchemeVal | null = initExpr;
  let env: Env = initEnv;
  let kont: Kont = null;  // halt
  let val: SchemeVal = { tag: 'void' };
  let windStack: WindFrame[] = [];
  let windTarget: { kont: Kont; val: SchemeVal; windStack: WindFrame[] } | null = null;

  // Deep copy continuation chain (needed for call/cc to snapshot mutable frames)
  function copyKont(k: Kont): Kont {
    if (k === null) return null;
    const rest = copyKont(k.next);
    if (k.tag === 'ev-args') return { ...k, done: [...k.done], next: rest };
    if (k.tag === 'do-step') return { ...k, newVals: [...k.newVals], next: rest };
    if (rest === k.next) return k; // share unchanged frames
    return { ...k, next: rest } as KontFrame;
  }

  // Wind transition: unwind current, rewind target, then resume continuation
  function invokeContinuation(targetKont: Kont, targetWindStack: WindFrame[], value: SchemeVal): void {
    let commonLen = 0;
    const minLen = Math.min(windStack.length, targetWindStack.length);
    while (commonLen < minLen && windStack[commonLen] === targetWindStack[commonLen]) commonLen++;

    const unwindOuts = windStack.slice(commonLen).reverse().map(f => f.outThunk);
    const rewindFrames = targetWindStack.slice(commonLen);

    if (unwindOuts.length === 0 && rewindFrames.length === 0) {
      kont = targetKont;
      val = value;
      ctrl = null;
      return;
    }

    windTarget = { kont: targetKont, val: value, windStack: targetWindStack };

    if (unwindOuts.length > 0) {
      windStack.pop();
      const first = unwindOuts[0];
      const restOuts = unwindOuts.slice(1);
      kont = { tag: 'dw', phase: 3, next: null, unwindOuts: restOuts, rewindFrames };
      applyFunc(first, [], undefined);
    } else {
      windStack.push(rewindFrames[0]);
      kont = { tag: 'dw', phase: 3, next: null, unwindOuts: [], rewindFrames: rewindFrames.slice(1) };
      applyFunc(rewindFrames[0].inThunk, [], undefined);
    }
  }

  // Apply a function to arguments (may set ctrl/env/kont/val)
  function applyFunc(func: SchemeVal, args: SchemeVal[], pos?: Pos): void {
    if (func.tag === 'builtin' && func.name === 'call/cc') {
      if (args.length !== 1) throw new EvalError(`${fmtPos(pos)}call/cc: expected 1 argument`);
      const proc = args[0];
      const kontVal: SchemeVal = { tag: 'continuation', kont: copyKont(kont), windStack: [...windStack] };
      applyFunc(proc, [kontVal], pos);
      return;
    }
    if (func.tag === 'builtin' && func.name === 'dynamic-wind') {
      if (args.length !== 3) throw new EvalError(`${fmtPos(pos)}dynamic-wind: expected 3 arguments`);
      const [inThunk, bodyThunk, outThunk] = args;
      kont = { tag: 'dw', phase: 0, next: kont, bodyThunk, outThunk, inThunk };
      applyFunc(inThunk, [], pos);
      return;
    }
    if (func.tag === 'continuation') {
      invokeContinuation(func.kont, func.windStack, args.length > 0 ? args[0] : { tag: 'void' });
      return;
    }
    if (func.tag === 'builtin') {
      val = func.func(args, pos);
      ctrl = null;
      return;
    }
    if (func.tag === 'lambda') {
      if (func.restParam) {
        if (args.length < func.params.length) throw new EvalError(`${fmtPos(pos)}wrong number of arguments`);
      } else {
        if (args.length !== func.params.length) throw new EvalError(`${fmtPos(pos)}wrong number of arguments`);
      }
      const callEnv = childEnv(func.env);
      for (let i = 0; i < func.params.length; i++) envDefine(callEnv, func.params[i], args[i]);
      if (func.restParam) envDefine(callEnv, func.restParam, arrayToSchemeList(args.slice(func.params.length)));
      if (func.body.length === 0) { val = { tag: 'void' }; ctrl = null; return; }
      if (func.body.length > 1) kont = { tag: 'seq', exprs: func.body, idx: 1, env: callEnv, next: kont };
      ctrl = func.body[0]; env = callEnv;
      return;
    }
    if (func.tag === 'case-lambda') {
      for (const clause of func.clauses) {
        const match = clause.restParam ? args.length >= clause.params.length : args.length === clause.params.length;
        if (match) {
          const callEnv = childEnv(func.env);
          for (let i = 0; i < clause.params.length; i++) envDefine(callEnv, clause.params[i], args[i]);
          if (clause.restParam) envDefine(callEnv, clause.restParam, arrayToSchemeList(args.slice(clause.params.length)));
          if (clause.body.length === 0) { val = { tag: 'void' }; ctrl = null; return; }
          if (clause.body.length > 1) kont = { tag: 'seq', exprs: clause.body, idx: 1, env: callEnv, next: kont };
          ctrl = clause.body[0]; env = callEnv;
          return;
        }
      }
      throw new EvalError(`${fmtPos(pos)}no matching clause for ${args.length} arguments`);
    }
    throw new EvalError(`${fmtPos(pos)}not a procedure: ${schemeToString(func)}`);
  }

  // Process cond clauses starting from clauses[0]
  function processCondClauses(clauses: SchemeVal[], pos?: Pos): void {
    if (clauses.length === 0) { val = { tag: 'void' }; ctrl = null; return; }
    const clause = clauses[0];
    if (clause.tag !== 'list' || clause.elements.length < 1)
      throw new EvalError(`${fmtPos(pos)}cond: invalid clause`);
    if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
      const body = clause.elements.slice(1);
      if (body.length === 0) { val = { tag: 'void' }; ctrl = null; return; }
      if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env, next: kont };
      ctrl = body[0]; return;
    }
    kont = { tag: 'cond-test', clauseElems: clause.elements, restClauses: clauses.slice(1), env, pos, next: kont };
    ctrl = clause.elements[0];
  }

  // Start do-loop stepping
  function startDoStep(varNames: string[], stepExprs: (SchemeVal|undefined)[], testExpr: SchemeVal, resultExprs: SchemeVal[], bodyExprs: SchemeVal[], doEnv: Env, outerNext: Kont): void {
    let firstIdx = 0;
    while (firstIdx < stepExprs.length && stepExprs[firstIdx] === undefined) firstIdx++;
    if (firstIdx >= stepExprs.length) {
      // No step exprs, loop back to test
      kont = { tag: 'do-test', varNames, stepExprs, testExpr, resultExprs, bodyExprs, env: doEnv, next: outerNext };
      ctrl = testExpr; env = doEnv;
      return;
    }
    const newVals: (SchemeVal|undefined)[] = new Array(stepExprs.length).fill(undefined);
    kont = { tag: 'do-step', varNames, stepExprs, stepIdx: firstIdx, newVals, testExpr, resultExprs, bodyExprs, env: doEnv, next: outerNext };
    ctrl = stepExprs[firstIdx]!; env = doEnv;
  }

  // Main CEK loop
  while (true) {
    try {
      for (;;) {
        if (ctrl !== null) {
          // === EVAL PHASE ===
          const expr: SchemeVal = ctrl;
          ctrl = null;

          switch (expr.tag) {
            case 'number': case 'rational': case 'boolean': case 'string': case 'char':
              val = expr; break;

            case 'symbol': {
              const v = envLookup(env, expr.value);
              if (v === undefined) throw new EvalError(`${fmtPos(expr.pos)}unbound variable: ${expr.value}`);
              val = v; break;
            }

            case 'vector': case 'pair': case 'builtin': case 'lambda': case 'case-lambda':
            case 'macro': case 'record': case 'void': case 'continuation':
              val = expr; break;

            case 'list': {
              const elems: SchemeVal[] = expr.elements;
              if (elems.length === 0) throw new EvalError(`${fmtPos(expr.pos)}empty application`);

              if (elems[0].tag === 'symbol') {
                const name = elems[0].value;

                if (name === 'quote') {
                  if (elems.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}quote: wrong argument count`);
                  val = elems[1]; break;
                }

                if (name === 'if') {
                  if (elems.length < 3 || elems.length > 4) throw new EvalError(`${fmtPos(expr.pos)}if: wrong argument count`);
                  kont = { tag: 'if-test', thenE: elems[2], elseE: elems.length === 4 ? elems[3] : undefined, env, next: kont };
                  ctrl = elems[1]; break;
                }

                if (name === 'define') {
                  if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}define: wrong argument count`);
                  if (elems[1].tag === 'symbol') {
                    kont = { tag: 'define', name: elems[1].value, env, next: kont };
                    ctrl = elems[2]; break;
                  }
                  if (elems[1].tag === 'list' && elems[1].elements.length > 0 && elems[1].elements[0].tag === 'symbol') {
                    const fnName = elems[1].elements[0].value;
                    const { params, restParam } = parseParams(elems[1].elements.slice(1), expr.pos);
                    envDefine(env, fnName, { tag: 'lambda', params, restParam, body: elems.slice(2), env });
                    val = { tag: 'void' }; break;
                  }
                  throw new EvalError(`${fmtPos(expr.pos)}define: invalid syntax`);
                }

                if (name === 'lambda') {
                  if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}lambda: wrong argument count`);
                  if (elems[1].tag === 'symbol') {
                    val = { tag: 'lambda', params: [], restParam: elems[1].value, body: elems.slice(2), env }; break;
                  }
                  if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}lambda: params must be a list`);
                  const { params, restParam } = parseParams(elems[1].elements, expr.pos);
                  val = { tag: 'lambda', params, restParam, body: elems.slice(2), env }; break;
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
                  val = { tag: 'case-lambda', clauses, env, pos: expr.pos }; break;
                }

                if (name === 'and') {
                  if (elems.length === 1) { val = { tag: 'boolean', value: true }; break; }
                  if (elems.length === 2) { ctrl = elems[1]; break; }
                  kont = { tag: 'and-k', rest: elems.slice(2), env, next: kont };
                  ctrl = elems[1]; break;
                }

                if (name === 'or') {
                  if (elems.length === 1) { val = { tag: 'boolean', value: false }; break; }
                  if (elems.length === 2) { ctrl = elems[1]; break; }
                  kont = { tag: 'or-k', rest: elems.slice(2), env, next: kont };
                  ctrl = elems[1]; break;
                }

                if (name === 'not') {
                  if (elems.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}not: wrong argument count`);
                  kont = { tag: 'negate', next: kont };
                  ctrl = elems[1]; break;
                }

                if (name === 'begin') {
                  if (elems.length === 1) { val = { tag: 'void' }; break; }
                  if (elems.length === 2) { ctrl = elems[1]; break; }
                  kont = { tag: 'seq', exprs: elems, idx: 2, env, next: kont };
                  ctrl = elems[1]; break;
                }

                if (name === 'cond') {
                  processCondClauses(elems.slice(1), expr.pos);
                  break;
                }

                if (name === 'set!') {
                  if (elems.length !== 3) throw new EvalError(`${fmtPos(expr.pos)}set!: wrong argument count`);
                  if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}set!: first arg must be a symbol`);
                  kont = { tag: 'set', name: elems[1].value, env, next: kont };
                  ctrl = elems[2]; break;
                }

                if (name === 'string-set!') {
                  if (elems.length !== 4) throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected 3 args`);
                  if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}string-set!: strings are immutable`);
                  const strVal = envLookup(env, elems[1].value);
                  if (!strVal || strVal.tag !== 'string') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected string variable`);
                  if (!strVal.mutable) throw new EvalError(`${fmtPos(expr.pos)}string-set!: strings are immutable`);
                  kont = { tag: 'str-set-idx', strVal, charExpr: elems[3], env, pos: expr.pos, next: kont };
                  ctrl = elems[2]; break;
                }

                if (name === 'let') {
                  if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}let: wrong argument count`);
                  // Named let
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
                      inits.push(b.elements[1]);
                    }
                    const body = elems.slice(3);
                    if (inits.length === 0) {
                      const loopEnv = childEnv(env);
                      const lambda: SchemeVal = { tag: 'lambda', params, body, env: loopEnv };
                      envDefine(loopEnv, loopName, lambda);
                      const callEnv = childEnv(loopEnv);
                      if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: callEnv, next: kont };
                      ctrl = body[0]; env = callEnv; break;
                    }
                    kont = { tag: 'named-let-init', loopName, params, inits, idx: 0, vals: [], body, outerEnv: env, pos: expr.pos, next: kont };
                    ctrl = inits[0]; break;
                  }
                  // Regular let
                  if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}let: bindings must be a list`);
                  const bindingsList = elems[1].elements;
                  const body = elems.slice(2);
                  if (bindingsList.length === 0) {
                    const letEnv = childEnv(env);
                    if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: letEnv, next: kont };
                    ctrl = body[0]; env = letEnv; break;
                  }
                  const names: string[] = [];
                  const letInits: SchemeVal[] = [];
                  for (const b of bindingsList) {
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                      throw new EvalError(`${fmtPos(expr.pos)}let: invalid binding`);
                    names.push(b.elements[0].value);
                    letInits.push(b.elements[1]);
                  }
                  kont = { tag: 'let-init', names, vals: [], inits: letInits, body, outerEnv: env, pos: expr.pos, next: kont };
                  ctrl = letInits[0]; break;
                }

                if (name === 'let*') {
                  if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}let*: wrong argument count`);
                  if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}let*: bindings must be a list`);
                  const body = elems.slice(2);
                  const bindingsList = elems[1].elements;
                  const letStarEnv = childEnv(env);
                  if (bindingsList.length === 0) {
                    if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: letStarEnv, next: kont };
                    ctrl = body[0]; env = letStarEnv; break;
                  }
                  const b0 = bindingsList[0];
                  if (b0.tag !== 'list' || b0.elements.length !== 2 || b0.elements[0].tag !== 'symbol')
                    throw new EvalError(`${fmtPos(expr.pos)}let*: invalid binding`);
                  kont = { tag: 'letstar-bind', bindings: bindingsList, idx: 0, body, env: letStarEnv, pos: expr.pos, next: kont };
                  ctrl = b0.elements[1]; env = letStarEnv; break;
                }

                if (name === 'letrec' || name === 'letrec*') {
                  const sequential = name === 'letrec*';
                  if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}${name}: wrong argument count`);
                  if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}${name}: bindings must be a list`);
                  const body = elems.slice(2);
                  const letrecEnv = childEnv(env);
                  const lrNames: string[] = [];
                  const lrInits: SchemeVal[] = [];
                  for (const b of elems[1].elements) {
                    if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                      throw new EvalError(`${fmtPos(expr.pos)}${name}: invalid binding`);
                    lrNames.push(b.elements[0].value);
                    lrInits.push(b.elements[1]);
                    envDefine(letrecEnv, b.elements[0].value, { tag: 'void' });
                  }
                  if (lrNames.length === 0) {
                    if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: letrecEnv, next: kont };
                    ctrl = body[0]; env = letrecEnv; break;
                  }
                  kont = { tag: 'letrec-init', names: lrNames, inits: lrInits, idx: 0, vals: [], body, env: letrecEnv, sequential, pos: expr.pos, next: kont };
                  ctrl = lrInits[0]; env = letrecEnv; break;
                }

                if (name === 'case') {
                  if (elems.length < 2) throw new EvalError(`${fmtPos(expr.pos)}case: wrong argument count`);
                  kont = { tag: 'case-key', clauses: elems.slice(2), env, pos: expr.pos, next: kont };
                  ctrl = elems[1]; break;
                }

                if (name === 'do') {
                  if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}do: wrong argument count`);
                  if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}do: bindings must be a list`);
                  if (elems[2].tag !== 'list' || elems[2].elements.length < 1)
                    throw new EvalError(`${fmtPos(expr.pos)}do: test clause required`);
                  const specs: {name: string; initExpr: SchemeVal; stepExpr?: SchemeVal}[] = [];
                  for (const binding of elems[1].elements) {
                    if (binding.tag !== 'list' || binding.elements.length < 2 || binding.elements[0].tag !== 'symbol')
                      throw new EvalError(`${fmtPos(expr.pos)}do: invalid variable spec`);
                    specs.push({
                      name: binding.elements[0].value,
                      initExpr: binding.elements[1],
                      stepExpr: binding.elements.length >= 3 ? binding.elements[2] : undefined,
                    });
                  }
                  const testClause = elems[2];
                  const testExpr = testClause.elements[0];
                  const resultExprs = testClause.elements.slice(1);
                  const bodyExprs = elems.slice(3);
                  if (specs.length === 0) {
                    const doEnv = childEnv(env);
                    kont = { tag: 'do-test', varNames: [], stepExprs: [], testExpr, resultExprs, bodyExprs, env: doEnv, next: kont };
                    ctrl = testExpr; env = doEnv; break;
                  }
                  kont = { tag: 'do-init', specs, idx: 0, vals: [], testExpr, resultExprs, bodyExprs, outerEnv: env, pos: expr.pos, next: kont };
                  ctrl = specs[0].initExpr; break;
                }

                if (name === 'define-record-type') {
                  if (elems.length < 4) throw new EvalError(`${fmtPos(expr.pos)}define-record-type: invalid syntax`);
                  if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}define-record-type: expected type name`);
                  const typeName = elems[1].value;
                  const ctorForm = elems[2];
                  if (ctorForm.tag !== 'list' || ctorForm.elements.length < 1 || ctorForm.elements[0].tag !== 'symbol')
                    throw new EvalError(`${fmtPos(expr.pos)}define-record-type: invalid constructor`);
                  const ctorName = ctorForm.elements[0].value;
                  const ctorFields = ctorForm.elements.slice(1).map((e: SchemeVal) => {
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
                  envDefine(env, ctorName, { tag: 'builtin', name: ctorName, func: (args, callPos) => {
                    if (args.length !== ctorFields.length)
                      throw new EvalError(`${fmtPos(callPos)}${ctorName}: expected ${ctorFields.length} arguments, got ${args.length}`);
                    const fields = new Map<string, SchemeVal>();
                    for (let i = 0; i < ctorFields.length; i++) fields.set(ctorFields[i], args[i]);
                    return { tag: 'record', typeName, fields };
                  }});
                  envDefine(env, predName, { tag: 'builtin', name: predName, func: (args, callPos) => {
                    if (args.length !== 1) throw new EvalError(`${fmtPos(callPos)}${predName}: expected 1 argument`);
                    return { tag: 'boolean', value: args[0].tag === 'record' && args[0].typeName === typeName };
                  }});
                  for (const { field, accessor } of fieldAccessors) {
                    envDefine(env, accessor, { tag: 'builtin', name: accessor, func: (args, callPos) => {
                      if (args.length !== 1) throw new EvalError(`${fmtPos(callPos)}${accessor}: expected 1 argument`);
                      if (args[0].tag !== 'record' || args[0].typeName !== typeName)
                        throw new EvalError(`${fmtPos(callPos)}${accessor}: not a ${typeName}`);
                      const v = args[0].fields.get(field);
                      if (v === undefined) throw new EvalError(`${fmtPos(callPos)}${accessor}: field ${field} not found`);
                      return v;
                    }});
                  }
                  val = { tag: 'void' }; break;
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
                  const literals = sr.elements[1].elements.map((e: SchemeVal) => {
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
                  val = { tag: 'void' }; break;
                }

                // Macro expansion
                const maybeMacro = envLookup(env, name);
                if (maybeMacro && maybeMacro.tag === 'macro') {
                  ctrl = expandMacroCall(maybeMacro, expr); break;
                }
              }

              // Function application: eval all elements left-to-right, then apply
              kont = { tag: 'ev-args', done: [], allElems: elems, idx: 1, env, pos: expr.pos, next: kont };
              ctrl = elems[0]; break;
            }
          }
        } else {
          // === APPLY-KONT PHASE ===
          if (kont === null) return val;

          // Handle dynamic-wind frames separately (not in KontData union)
          if (kont.tag === 'dw') {
            const dw = kont as DwFrame;
            if (dw.phase === 0) {
              windStack.push({ inThunk: dw.inThunk!, outThunk: dw.outThunk! });
              kont = { tag: 'dw', phase: 1, next: dw.next, outThunk: dw.outThunk, inThunk: dw.inThunk };
              applyFunc(dw.bodyThunk!, []);
            } else if (dw.phase === 1) {
              const bodyResult = val;
              windStack.pop();
              kont = { tag: 'dw', phase: 2, next: dw.next, result: bodyResult };
              applyFunc(dw.outThunk!, []);
            } else if (dw.phase === 2) {
              val = dw.result!;
              kont = dw.next;
            } else {
              // phase 3: wind transition
              const uo = dw.unwindOuts!;
              const rf = dw.rewindFrames!;
              if (uo.length > 0) {
                windStack.pop();
                kont = { tag: 'dw', phase: 3, next: null, unwindOuts: uo.slice(1), rewindFrames: rf };
                applyFunc(uo[0], []);
              } else if (rf.length > 0) {
                windStack.push(rf[0]);
                kont = { tag: 'dw', phase: 3, next: null, unwindOuts: [], rewindFrames: rf.slice(1) };
                applyFunc(rf[0].inThunk, []);
              } else {
                const t = windTarget!;
                windTarget = null;
                windStack = [...t.windStack];
                kont = t.kont;
                val = t.val;
              }
            }
            break;
          }

          const frame = kont as KontFrame;
          switch (frame.tag) {
            case 'seq': {
              // Discard val, eval next. Last is tail position.
              if (frame.idx >= frame.exprs.length - 1) {
                ctrl = frame.exprs[frame.idx]; env = frame.env; kont = frame.next; break;
              }
              ctrl = frame.exprs[frame.idx++]; env = frame.env; break;
            }

            case 'define': {
              envDefine(frame.env, frame.name, val);
              val = { tag: 'void' }; kont = frame.next; break;
            }

            case 'set': {
              envSet(frame.env, frame.name, val);
              val = { tag: 'void' }; kont = frame.next; break;
            }

            case 'if-test': {
              kont = frame.next;
              if (isTruthy(val)) { ctrl = frame.thenE; env = frame.env; break; }
              if (frame.elseE) { ctrl = frame.elseE; env = frame.env; break; }
              val = { tag: 'void' }; break;
            }

            case 'ev-args': {
              frame.done.push(val);
              if (frame.idx >= frame.allElems.length) {
                // All elements evaluated. done[0] = func, done[1:] = args
                kont = frame.next;
                applyFunc(frame.done[0], frame.done.slice(1), frame.pos);
                break;
              }
              ctrl = frame.allElems[frame.idx++]; env = frame.env; break;
            }

            case 'and-k': {
              if (!isTruthy(val)) { kont = frame.next; break; }
              if (frame.rest.length === 1) { ctrl = frame.rest[0]; env = frame.env; kont = frame.next; break; }
              kont = { tag: 'and-k', rest: frame.rest.slice(1), env: frame.env, next: frame.next };
              ctrl = frame.rest[0]; env = frame.env; break;
            }

            case 'or-k': {
              if (isTruthy(val)) { kont = frame.next; break; }
              if (frame.rest.length === 1) { ctrl = frame.rest[0]; env = frame.env; kont = frame.next; break; }
              kont = { tag: 'or-k', rest: frame.rest.slice(1), env: frame.env, next: frame.next };
              ctrl = frame.rest[0]; env = frame.env; break;
            }

            case 'negate': {
              val = { tag: 'boolean', value: !isTruthy(val) }; kont = frame.next; break;
            }

            case 'let-init': {
              const vals: SchemeVal[] = [...frame.vals, val];
              const nextIdx: number = vals.length;
              if (nextIdx < frame.inits.length) {
                kont = { ...frame, vals }; ctrl = frame.inits[nextIdx]; env = frame.outerEnv; break;
              }
              // All inits done — create env, bind, eval body
              const letEnv = childEnv(frame.outerEnv);
              for (let i = 0; i < frame.names.length; i++) envDefine(letEnv, frame.names[i], vals[i]);
              kont = frame.next;
              if (frame.body.length > 1) kont = { tag: 'seq', exprs: frame.body, idx: 1, env: letEnv, next: frame.next };
              ctrl = frame.body[0]; env = letEnv; break;
            }

            case 'letstar-bind': {
              const b = frame.bindings[frame.idx];
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw new EvalError(`${fmtPos(frame.pos)}let*: invalid binding`);
              envDefine(frame.env, b.elements[0].value, val);
              if (frame.idx + 1 < frame.bindings.length) {
                const nb = frame.bindings[frame.idx + 1];
                if (nb.tag !== 'list' || nb.elements.length !== 2 || nb.elements[0].tag !== 'symbol')
                  throw new EvalError(`${fmtPos(frame.pos)}let*: invalid binding`);
                kont = { ...frame, idx: frame.idx + 1 }; ctrl = nb.elements[1]; env = frame.env; break;
              }
              kont = frame.next;
              if (frame.body.length > 1) kont = { tag: 'seq', exprs: frame.body, idx: 1, env: frame.env, next: frame.next };
              ctrl = frame.body[0]; env = frame.env; break;
            }

            case 'letrec-init': {
              const vals = [...frame.vals, val];
              if (frame.sequential) envSet(frame.env, frame.names[frame.idx], val);
              if (frame.idx + 1 < frame.inits.length) {
                kont = { ...frame, idx: frame.idx + 1, vals }; ctrl = frame.inits[frame.idx + 1]; env = frame.env; break;
              }
              if (!frame.sequential) {
                for (let i = 0; i < frame.names.length; i++) envSet(frame.env, frame.names[i], vals[i]);
              }
              kont = frame.next;
              if (frame.body.length > 1) kont = { tag: 'seq', exprs: frame.body, idx: 1, env: frame.env, next: frame.next };
              ctrl = frame.body[0]; env = frame.env; break;
            }

            case 'named-let-init': {
              const vals = [...frame.vals, val];
              if (frame.idx + 1 < frame.inits.length) {
                kont = { ...frame, idx: frame.idx + 1, vals }; ctrl = frame.inits[frame.idx + 1]; env = frame.outerEnv; break;
              }
              const loopEnv = childEnv(frame.outerEnv);
              const lambda: SchemeVal = { tag: 'lambda', params: frame.params, body: frame.body, env: loopEnv };
              envDefine(loopEnv, frame.loopName, lambda);
              const callEnv = childEnv(loopEnv);
              for (let i = 0; i < frame.params.length; i++) envDefine(callEnv, frame.params[i], vals[i]);
              kont = frame.next;
              if (frame.body.length > 1) kont = { tag: 'seq', exprs: frame.body, idx: 1, env: callEnv, next: frame.next };
              ctrl = frame.body[0]; env = callEnv; break;
            }

            case 'case-key': {
              const key = val;
              let matched = false;
              for (const clause of frame.clauses) {
                if (clause.tag !== 'list' || clause.elements.length < 2)
                  throw new EvalError(`${fmtPos(frame.pos)}case: invalid clause`);
                const datums = clause.elements[0];
                if (datums.tag === 'symbol' && datums.value === 'else') {
                  const body = clause.elements.slice(1);
                  kont = frame.next;
                  if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: frame.env, next: frame.next };
                  ctrl = body[0]; env = frame.env; matched = true; break;
                }
                if (datums.tag !== 'list') throw new EvalError(`${fmtPos(frame.pos)}case: datums must be a list`);
                let found = false;
                for (const datum of datums.elements) {
                  if (key.tag === datum.tag) {
                    if ((key.tag === 'number' && datum.tag === 'number' && key.value === datum.value) ||
                        (key.tag === 'symbol' && datum.tag === 'symbol' && key.value === datum.value) ||
                        (key.tag === 'boolean' && datum.tag === 'boolean' && key.value === datum.value) ||
                        (key.tag === 'char' && datum.tag === 'char' && key.value === datum.value) ||
                        (key.tag === 'string' && datum.tag === 'string' && key.value === datum.value)) {
                      found = true; break;
                    }
                  }
                }
                if (found) {
                  const body = clause.elements.slice(1);
                  kont = frame.next;
                  if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: frame.env, next: frame.next };
                  ctrl = body[0]; env = frame.env; matched = true; break;
                }
              }
              if (!matched) { val = { tag: 'void' }; kont = frame.next; }
              break;
            }

            case 'cond-test': {
              if (isTruthy(val)) {
                const ce = frame.clauseElems;
                if (ce.length === 1) { /* (cond (test)) */ kont = frame.next; break; }
                if (ce.length === 3 && ce[1].tag === 'symbol' && ce[1].value === '=>') {
                  kont = { tag: 'cond-arrow', testVal: val, env: frame.env, pos: frame.pos, next: frame.next };
                  ctrl = ce[2]; env = frame.env; break;
                }
                const body = ce.slice(1);
                kont = frame.next;
                if (body.length > 1) kont = { tag: 'seq', exprs: body, idx: 1, env: frame.env, next: frame.next };
                ctrl = body[0]; env = frame.env; break;
              }
              kont = frame.next; env = frame.env;
              processCondClauses(frame.restClauses, frame.pos);
              break;
            }

            case 'cond-arrow': {
              kont = frame.next;
              applyFunc(val, [frame.testVal], frame.pos);
              break;
            }

            case 'str-set-idx': {
              if (val.tag !== 'number') throw new EvalError(`${fmtPos(frame.pos)}string-set!: expected number index`);
              kont = { tag: 'str-set-char', strVal: frame.strVal, idx: val.value, pos: frame.pos, next: frame.next };
              ctrl = frame.charExpr; env = frame.env; break;
            }

            case 'str-set-char': {
              if (val.tag !== 'char') throw new EvalError(`${fmtPos(frame.pos)}string-set!: expected char`);
              const sv = frame.strVal as SchemeVal & { tag: 'string'; value: string };
              const i = frame.idx;
              if (i < 0 || i >= sv.value.length) throw new EvalError(`${fmtPos(frame.pos)}string-set!: index out of range`);
              sv.value = sv.value.slice(0, i) + val.value + sv.value.slice(i + 1);
              val = { tag: 'void' }; kont = frame.next; break;
            }

            case 'do-init': {
              const vals = [...frame.vals, val];
              if (frame.idx + 1 < frame.specs.length) {
                kont = { ...frame, idx: frame.idx + 1, vals };
                ctrl = frame.specs[frame.idx + 1].initExpr; env = frame.outerEnv; break;
              }
              const doEnv = childEnv(frame.outerEnv);
              const varNames: string[] = [];
              const stepExprs: (SchemeVal|undefined)[] = [];
              for (let i = 0; i < frame.specs.length; i++) {
                envDefine(doEnv, frame.specs[i].name, vals[i]);
                varNames.push(frame.specs[i].name);
                stepExprs.push(frame.specs[i].stepExpr);
              }
              kont = { tag: 'do-test', varNames, stepExprs, testExpr: frame.testExpr, resultExprs: frame.resultExprs, bodyExprs: frame.bodyExprs, env: doEnv, next: frame.next };
              ctrl = frame.testExpr; env = doEnv; break;
            }

            case 'do-test': {
              if (isTruthy(val)) {
                if (frame.resultExprs.length === 0) { val = { tag: 'void' }; kont = frame.next; break; }
                kont = frame.next;
                if (frame.resultExprs.length > 1) kont = { tag: 'seq', exprs: frame.resultExprs, idx: 1, env: frame.env, next: frame.next };
                ctrl = frame.resultExprs[0]; env = frame.env; break;
              }
              if (frame.bodyExprs.length === 0) {
                startDoStep(frame.varNames, frame.stepExprs, frame.testExpr, frame.resultExprs, frame.bodyExprs, frame.env, frame.next);
                break;
              }
              kont = { tag: 'do-body', varNames: frame.varNames, stepExprs: frame.stepExprs, testExpr: frame.testExpr, resultExprs: frame.resultExprs, bodyExprs: frame.bodyExprs, bodyIdx: 0, env: frame.env, next: frame.next };
              ctrl = frame.bodyExprs[0]; env = frame.env; break;
            }

            case 'do-body': {
              if (frame.bodyIdx + 1 < frame.bodyExprs.length) {
                kont = { ...frame, bodyIdx: frame.bodyIdx + 1 };
                ctrl = frame.bodyExprs[frame.bodyIdx + 1]; env = frame.env; break;
              }
              startDoStep(frame.varNames, frame.stepExprs, frame.testExpr, frame.resultExprs, frame.bodyExprs, frame.env, frame.next);
              break;
            }

            case 'do-step': {
              frame.newVals[frame.stepIdx] = val;
              let nextIdx = frame.stepIdx + 1;
              while (nextIdx < frame.stepExprs.length && frame.stepExprs[nextIdx] === undefined) nextIdx++;
              if (nextIdx < frame.stepExprs.length) {
                frame.stepIdx = nextIdx;
                ctrl = frame.stepExprs[nextIdx]!; env = frame.env; break;
              }
              const newVals = frame.newVals;
              for (let i = 0; i < frame.varNames.length; i++) {
                if (newVals[i] !== undefined) envSet(frame.env, frame.varNames[i], newVals[i]!);
              }
              kont = { tag: 'do-test', varNames: frame.varNames, stepExprs: frame.stepExprs, testExpr: frame.testExpr, resultExprs: frame.resultExprs, bodyExprs: frame.bodyExprs, env: frame.env, next: frame.next };
              ctrl = frame.testExpr; env = frame.env; break;
            }

          }
        }
      }
    } catch (e) {
      if (e instanceof ContinuationReturn) {
        invokeContinuation(e.kont, e.windStack, e.value);
        continue;
      }
      throw e;
    }
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
  // Evaluate all expressions in a single CEK session so call/cc continuations
  // can span across top-level expressions
  const beginExpr: SchemeVal = exprs.length === 1 ? exprs[0]
    : { tag: 'list', elements: [{ tag: 'symbol', value: 'begin' }, ...exprs] };
  const result = evalScheme(beginExpr, env);
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
  const beginExpr: SchemeVal = exprs.length === 1 ? exprs[0]
    : { tag: 'list', elements: [{ tag: 'symbol', value: 'begin' }, ...exprs] };
  const result = evalScheme(beginExpr, env);
  return { result: schemeToString(result), output: outputBuf.join('') };
}
