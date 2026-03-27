import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeValBase =
  | { tag: 'number'; val: number; exact?: boolean; num?: number; den?: number }
  | { tag: 'boolean'; val: boolean }
  | { tag: 'string'; val: string; mutable?: boolean; chars?: string[] }
  | { tag: 'symbol'; val: string }
  | { tag: 'char'; val: string }
  | { tag: 'list'; val: SchemeVal[] }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'nil' }
  | { tag: 'procedure'; val: (args: SchemeVal[]) => SchemeVal }
  | { tag: 'closure'; params: string[]; rest: string | null; body: SchemeVal[]; closedEnv: Env }
  | { tag: 'void' }
  | { tag: 'macro'; transformer: MacroTransformer }
  | { tag: 'record'; type: symbol; fields: Map<string, SchemeVal> }
  | { tag: 'vector'; val: SchemeVal[] }
  | { tag: 'continuation'; k: Cont; winders: Winder[] }
  | { tag: 'callcc' }
  | { tag: 'dynamicWind' }
  | { tag: 'case-closure'; clauses: { params: string[]; rest: string | null; body: SchemeVal[] }[]; closedEnv: Env };

type Winder = { inThunk: SchemeVal; outThunk: SchemeVal };

type MacroTransformer = {
  literals: string[];
  rules: { pattern: SchemeVal[]; template: SchemeVal }[];
  defEnv: Env;
};

type SchemeVal = SchemeValBase & { pos?: string };

type Cont =
  | { tag: 'halt' }
  | { tag: 'seq'; rest: SchemeVal[]; env: Env; next: Cont }
  | { tag: 'ifK'; thenE: SchemeVal; elseE: SchemeVal | undefined; env: Env; next: Cont }
  | { tag: 'defK'; name: string; env: Env; next: Cont }
  | { tag: 'setK'; name: string; env: Env; epos: string; next: Cont }
  | { tag: 'evalOpK'; argExprs: SchemeVal[]; env: Env; epos: string; next: Cont }
  | { tag: 'evalArgsK'; op: SchemeVal; done: SchemeVal[]; rest: SchemeVal[]; env: Env; epos: string; next: Cont }
  | { tag: 'callccK'; next: Cont }
  | { tag: 'letK'; cur: string; done: [string, SchemeVal][]; rest: [string, SchemeVal][]; outerEnv: Env; body: SchemeVal[]; next: Cont }
  | { tag: 'letStarK'; curName: string; rest: [string, SchemeVal][]; local: Env; body: SchemeVal[]; next: Cont }
  | { tag: 'letrecK'; curName: string; done: [string, SchemeVal][]; rest: [string, SchemeVal][]; local: Env; body: SchemeVal[]; next: Cont }
  | { tag: 'namedLetK'; loopName: string; params: string[]; restInits: SchemeVal[]; doneVals: SchemeVal[]; outerEnv: Env; body: SchemeVal[]; next: Cont }
  | { tag: 'condK'; clause: SchemeVal; restClauses: SchemeVal[]; env: Env; epos: string; next: Cont }
  | { tag: 'condArrowK'; testVal: SchemeVal; env: Env; epos: string; next: Cont }
  | { tag: 'caseK'; clauses: SchemeVal[]; env: Env; epos: string; next: Cont }
  | { tag: 'andK'; rest: SchemeVal[]; env: Env; next: Cont }
  | { tag: 'orK'; rest: SchemeVal[]; env: Env; next: Cont }
  | { tag: 'dwAfterInK'; bodyThunk: SchemeVal; outThunk: SchemeVal; inThunk: SchemeVal; epos: string; next: Cont }
  | { tag: 'dwAfterBodyK'; outThunk: SchemeVal; winder: Winder; next: Cont }
  | { tag: 'dwAfterOutK'; bodyVal: SchemeVal; next: Cont }
  | { tag: 'dwWindK'; setWindersTo: Winder[]; remaining: { thunk: SchemeVal; setWindersTo: Winder[] }[]; val: SchemeVal; targetK: Cont }
  ;

class ContinuationEscape { constructor(public k: Cont, public val: SchemeVal, public winders: Winder[]) {} }

type Token = { text: string; pos: string };

const NIL: SchemeVal = { tag: 'nil' };
const VOID: SchemeVal = { tag: 'void' };

// ── dynamic-wind state ──────────────────────────────────────────────────
let currentWinders: Winder[] = [];

type WindAction = { thunk: SchemeVal; setWindersTo: Winder[] };

function computeWindActions(from: Winder[], to: Winder[]): WindAction[] {
  let common = 0;
  while (common < from.length && common < to.length && from[common] === to[common]) common++;
  const actions: WindAction[] = [];
  for (let i = from.length - 1; i >= common; i--)
    actions.push({ thunk: from[i].outThunk, setWindersTo: from.slice(0, i) });
  for (let i = common; i < to.length; i++)
    actions.push({ thunk: to[i].inThunk, setWindersTo: to.slice(0, i + 1) });
  return actions;
}

function initiateWinding(actions: WindAction[], val: SchemeVal, targetK: Cont): CEKState {
  const first = actions[0];
  const rest = actions.slice(1);
  const k: Cont = { tag: 'dwWindK', setWindersTo: first.setWindersTo, remaining: rest, val, targetK };
  return cekApply(first.thunk, [], '?', k);
}

// Unique identity for cycle detection
let _nextId = 0;
const _idMap = new WeakMap<object, number>();
function idOf(obj: SchemeVal): number {
  let id = _idMap.get(obj);
  if (id === undefined) { id = _nextId++; _idMap.set(obj, id); }
  return id;
}

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

function strChars(v: SchemeVal & { tag: 'string' }): string[] {
  return v.chars ?? [...v.val];
}

function strVal(v: SchemeVal & { tag: 'string' }): string {
  return v.chars ? v.chars.join('') : v.val;
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

function schemeEqv(a: SchemeVal, b: SchemeVal): boolean {
  if (a.tag !== b.tag) return false;
  if (a.tag === 'nil') return true;
  if (a.tag === 'void') return true;
  if (a.tag === 'boolean') return a.val === (b as typeof a).val;
  if (a.tag === 'number') return a.val === (b as typeof a).val;
  if (a.tag === 'symbol') return a.val === (b as typeof a).val;
  if (a.tag === 'char') return a.val === (b as typeof a).val;
  return a === b;
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

  // cxr compositions
  const pairCar = (v: SchemeVal, name: string): SchemeVal => {
    if (v.tag !== 'pair') throw new EvalError(`${name}: not a pair`);
    return v.car;
  };
  const pairCdr = (v: SchemeVal, name: string): SchemeVal => {
    if (v.tag !== 'pair') throw new EvalError(`${name}: not a pair`);
    return v.cdr;
  };
  envSet(env, 'caar', { tag: 'procedure', val: (a) => pairCar(pairCar(a[0], 'caar'), 'caar') });
  envSet(env, 'cadr', { tag: 'procedure', val: (a) => pairCar(pairCdr(a[0], 'cadr'), 'cadr') });
  envSet(env, 'cdar', { tag: 'procedure', val: (a) => pairCdr(pairCar(a[0], 'cdar'), 'cdar') });
  envSet(env, 'cddr', { tag: 'procedure', val: (a) => pairCdr(pairCdr(a[0], 'cddr'), 'cddr') });
  envSet(env, 'caaar', { tag: 'procedure', val: (a) => pairCar(pairCar(pairCar(a[0], 'caaar'), 'caaar'), 'caaar') });
  envSet(env, 'caadr', { tag: 'procedure', val: (a) => pairCar(pairCar(pairCdr(a[0], 'caadr'), 'caadr'), 'caadr') });
  envSet(env, 'caddr', { tag: 'procedure', val: (a) => pairCar(pairCdr(pairCdr(a[0], 'caddr'), 'caddr'), 'caddr') });
  envSet(env, 'cdddr', { tag: 'procedure', val: (a) => pairCdr(pairCdr(pairCdr(a[0], 'cdddr'), 'cdddr'), 'cdddr') });
  envSet(env, 'cdaar', { tag: 'procedure', val: (a) => pairCdr(pairCar(pairCar(a[0], 'cdaar'), 'cdaar'), 'cdaar') });
  envSet(env, 'cdadr', { tag: 'procedure', val: (a) => pairCdr(pairCar(pairCdr(a[0], 'cdadr'), 'cdadr'), 'cdadr') });
  envSet(env, 'cddar', { tag: 'procedure', val: (a) => pairCdr(pairCdr(pairCar(a[0], 'cddar'), 'cddar'), 'cddar') });
  envSet(env, 'cadar', { tag: 'procedure', val: (a) => pairCar(pairCdr(pairCar(a[0], 'cadar'), 'cadar'), 'cadar') });
  envSet(env, 'caddar', { tag: 'procedure', val: (a) => pairCar(pairCdr(pairCdr(pairCar(a[0], 'caddar'), 'caddar'), 'caddar'), 'caddar') });
  envSet(env, 'cadddr', { tag: 'procedure', val: (a) => pairCar(pairCdr(pairCdr(pairCdr(a[0], 'cadddr'), 'cadddr'), 'cadddr'), 'cadddr') });

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
    if (fn.tag !== 'procedure' && fn.tag !== 'closure' && fn.tag !== 'case-closure' && fn.tag !== 'continuation' && fn.tag !== 'callcc') throw new EvalError('map: first argument must be a procedure');
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
      results.push(callAny(fn, fnArgs));
      for (let i = 0; i < cursors.length; i++) {
        cursors[i] = (cursors[i] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).cdr;
      }
    }
    return arrayToList(results);
  }});

  envSet(env, 'for-each', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('for-each requires at least 2 arguments');
    const fn = args[0];
    if (fn.tag !== 'procedure' && fn.tag !== 'closure' && fn.tag !== 'case-closure' && fn.tag !== 'continuation' && fn.tag !== 'callcc') throw new EvalError('for-each: first argument must be a procedure');
    const lists = args.slice(1);
    const cursors = lists.map(l => l);
    while (true) {
      const fnArgs: SchemeVal[] = [];
      let done = false;
      for (let i = 0; i < cursors.length; i++) {
        if (cursors[i].tag !== 'pair') { done = true; break; }
        fnArgs.push((cursors[i] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).car);
      }
      if (done) break;
      callAny(fn, fnArgs);
      for (let i = 0; i < cursors.length; i++) {
        cursors[i] = (cursors[i] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).cdr;
      }
    }
    return { tag: 'void' };
  }});

  envSet(env, 'reverse', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('reverse requires 1 argument');
    let cur = args[0];
    let result: SchemeVal = NIL;
    while (cur.tag === 'pair') {
      result = { tag: 'pair', car: cur.car, cdr: result };
      cur = cur.cdr;
    }
    return result;
  }});

  envSet(env, 'member', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('member requires 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (schemeEqual(cur.car, key)) return cur;
      cur = cur.cdr;
    }
    return { tag: 'boolean', val: false };
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

  envSet(env, 'procedure?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('procedure? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'procedure' || args[0].tag === 'closure' || args[0].tag === 'continuation' || args[0].tag === 'callcc' || args[0].tag === 'dynamicWind' || args[0].tag === 'case-closure' };
  }});

  // I/O
  const displayVal = (v: SchemeVal, seen?: Set<SchemeVal>): string => {
    switch (v.tag) {
      case 'number': return displayNumber(v);
      case 'boolean': return v.val ? '#t' : '#f';
      case 'string': return v.val;
      case 'symbol': return v.val;
      case 'char': return v.val;
      case 'nil': return '()';
      case 'void': return '';
      case 'procedure': return '#<procedure>';
      case 'closure': return '#<procedure>';
      case 'continuation': return '#<procedure>';
      case 'callcc': return '#<procedure>';
      case 'dynamicWind': return '#<procedure>';
      case 'case-closure': return '#<procedure>';
      case 'pair': {
        if (!seen) seen = new Set();
        if (seen.has(v)) return '(...)';
        seen.add(v);
        let out = '(' + displayVal(v.car, seen);
        let cur: SchemeVal = v.cdr;
        while (cur.tag === 'pair') {
          if (seen.has(cur)) { out += ' ...'; break; }
          seen.add(cur);
          out += ' ' + displayVal(cur.car, seen);
          cur = cur.cdr;
        }
        if (cur.tag !== 'nil' && cur.tag !== 'pair') out += ' . ' + displayVal(cur, seen);
        return out + ')';
      }
      case 'list': return `(${v.val.map(x => displayVal(x, seen)).join(' ')})`;
      case 'macro': return '#<macro>';
      case 'record': return '#<record>';
      case 'vector': return '#(' + v.val.map(x => displayVal(x, seen)).join(' ') + ')';
    }
  };

  const writeVal = (v: SchemeVal, seen?: Set<SchemeVal>): string => {
    if (v.tag === 'string') return `"${v.val}"`;
    if (v.tag === 'char') return `#\\${v.val}`;
    if (v.tag === 'vector') return '#(' + v.val.map(x => writeVal(x, seen)).join(' ') + ')';
    if (v.tag === 'pair') {
      if (!seen) seen = new Set();
      if (seen.has(v)) return '(...)';
      seen.add(v);
      let out = '(' + writeVal(v.car, seen);
      let cur: SchemeVal = v.cdr;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { out += ' ...'; break; }
        seen.add(cur);
        out += ' ' + writeVal(cur.car, seen);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil' && cur.tag !== 'pair') out += ' . ' + writeVal(cur, seen);
      return out + ')';
    }
    return displayVal(v, seen);
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
    const chars = strChars(args[0] as SchemeVal & { tag: 'string' });
    const i = args[1].val;
    if (i < 0 || i >= chars.length) throw new EvalError('string-ref: index out of range');
    return { tag: 'char', val: chars[i] };
  }});

  envSet(env, 'string-copy', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-copy: expected string');
    const chars = strChars(args[0] as SchemeVal & { tag: 'string' });
    return { tag: 'string', val: chars.join(''), mutable: true, chars: [...chars] };
  }});

  envSet(env, 'string-set!', { tag: 'procedure', val: (args) => {
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'char')
      throw new EvalError('string-set!: expected mutable string, index, and char');
    const s = args[0] as SchemeVal & { tag: 'string' };
    if (!s.mutable) throw new EvalError('string-set!: strings are immutable');
    const i = args[1].val;
    if (!s.chars) s.chars = [...s.val];
    if (i < 0 || i >= s.chars.length) throw new EvalError('string-set!: index out of range');
    s.chars[i] = args[2].val;
    s.val = s.chars.join('');
    return { tag: 'void' };
  }});

  envSet(env, 'string->list', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->list: expected string');
    const chars = [...args[0].val].map(c => ({ tag: 'char' as const, val: c }));
    let result: SchemeVal = { tag: 'nil' };
    for (let i = chars.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: chars[i], cdr: result };
    }
    return result;
  }});

  envSet(env, 'list->string', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('list->string: expected list');
    let node = args[0];
    let s = '';
    while (node.tag === 'pair') {
      if (node.car.tag !== 'char') throw new EvalError('list->string: expected list of characters');
      s += node.car.val;
      node = node.cdr;
    }
    return { tag: 'string', val: s };
  }});

  envSet(env, 'char->integer', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char->integer: expected char');
    return { tag: 'number', val: args[0].val.codePointAt(0)! };
  }});

  envSet(env, 'integer->char', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('integer->char: expected integer');
    return { tag: 'char', val: String.fromCodePoint(args[0].val) };
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
    if (a.tag === 'void') return { tag: 'boolean', val: true };
    if (a.tag === 'boolean') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    if (a.tag === 'number') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    if (a.tag === 'symbol') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    if (a.tag === 'char') return { tag: 'boolean', val: a.val === (b as typeof a).val };
    return { tag: 'boolean', val: a === b };
  }});

  const equalSeen = new Set<string>();
  const schemeEqual = (a: SchemeVal, b: SchemeVal): boolean => {
    if (a === b) return true;
    if (a.tag !== b.tag) return false;
    if (a.tag === 'nil') return true;
    if (a.tag === 'boolean') return a.val === (b as typeof a).val;
    if (a.tag === 'number') return a.val === (b as typeof a).val;
    if (a.tag === 'string') return a.val === (b as typeof a).val;
    if (a.tag === 'symbol') return a.val === (b as typeof a).val;
    if (a.tag === 'char') return a.val === (b as typeof a).val;
    if (a.tag === 'pair' && b.tag === 'pair') {
      // Use object identity to detect cycles
      const key = `${idOf(a)},${idOf(b)}`;
      if (equalSeen.has(key)) return true; // assume equal if we've seen this pair before
      equalSeen.add(key);
      const result = schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
      equalSeen.delete(key);
      return result;
    }
    if (a.tag === 'vector' && b.tag === 'vector') {
      if (a.val.length !== b.val.length) return false;
      for (let i = 0; i < a.val.length; i++) {
        if (!schemeEqual(a.val[i], b.val[i])) return false;
      }
      return true;
    }
    return false;
  };

  envSet(env, 'eqv?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('eqv? requires 2 arguments');
    return { tag: 'boolean', val: schemeEqv(args[0], args[1]) };
  }});

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

  envSet(env, 'gcd', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('gcd: expected number');
    const nums = args as (SchemeVal & { tag: 'number' })[];
    if (nums.length === 0) return makeExactInt(0);
    let result = Math.abs(nums[0].val);
    for (let i = 1; i < nums.length; i++) result = gcd(result, Math.abs(nums[i].val));
    return makeExactInt(result);
  }});

  envSet(env, 'lcm', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('lcm: expected number');
    const nums = args as (SchemeVal & { tag: 'number' })[];
    if (nums.length === 0) return makeExactInt(1);
    let result = Math.abs(nums[0].val);
    for (let i = 1; i < nums.length; i++) {
      const b = Math.abs(nums[i].val);
      result = result === 0 && b === 0 ? 0 : (result / gcd(result, b)) * b;
    }
    return makeExactInt(result);
  }});

  envSet(env, 'truncate', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('truncate: expected number');
    return makeExactInt(Math.trunc(args[0].val));
  }});

  envSet(env, 'round', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('round: expected number');
    return makeExactInt(Math.round(args[0].val));
  }});

  envSet(env, 'floor', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('floor: expected number');
    return makeExactInt(Math.floor(args[0].val));
  }});

  envSet(env, 'ceiling', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('ceiling: expected number');
    return makeExactInt(Math.ceil(args[0].val));
  }});

  envSet(env, 'make-string', { tag: 'procedure', val: (args) => {
    if (args.length < 1 || args[0].tag !== 'number') throw new EvalError('make-string: expected number');
    const ch = args.length >= 2 && args[1].tag === 'char' ? args[1].val : '\0';
    const chars = Array(args[0].val).fill(ch);
    return { tag: 'string', val: chars.join(''), mutable: true, chars };
  }});

  envSet(env, 'string', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'char') throw new EvalError('string: expected chars');
    const chars = args.map(a => (a as { tag: 'char'; val: string }).val);
    return { tag: 'string', val: chars.join('') };
  }});

  envSet(env, 'memq', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('memq requires 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (schemeEqv(cur.car, key)) return cur;
      cur = cur.cdr;
    }
    return { tag: 'boolean', val: false };
  }});

  envSet(env, 'assq', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('assq requires 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (cur.car.tag === 'pair' && schemeEqv(cur.car.car, key)) return cur.car;
      cur = cur.cdr;
    }
    return { tag: 'boolean', val: false };
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
    // Floyd's cycle detection
    let slow = args[0];
    let fast = args[0];
    while (fast.tag === 'pair') {
      slow = (slow as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).cdr;
      fast = fast.cdr;
      if (fast.tag !== 'pair') break;
      fast = fast.cdr;
      if (slow === fast) return { tag: 'boolean', val: false }; // cycle detected
    }
    return { tag: 'boolean', val: fast.tag === 'nil' };
  }});

  envSet(env, 'set-car!', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('set-car! requires 2 arguments');
    if (args[0].tag !== 'pair') throw new EvalError('set-car!: not a pair');
    (args[0] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).car = args[1];
    return { tag: 'void' };
  }});

  envSet(env, 'set-cdr!', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('set-cdr! requires 2 arguments');
    if (args[0].tag !== 'pair') throw new EvalError('set-cdr!: not a pair');
    (args[0] as { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }).cdr = args[1];
    return { tag: 'void' };
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

  envSet(env, 'assv', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('assv requires 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (cur.car.tag === 'pair' && schemeEqv(cur.car.car, key)) return cur.car;
      cur = cur.cdr;
    }
    return { tag: 'boolean', val: false };
  }});

  envSet(env, 'memv', { tag: 'procedure', val: (args) => {
    if (args.length !== 2) throw new EvalError('memv requires 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (schemeEqv(cur.car, key)) return cur;
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

  envSet(env, 'string>?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string>?: expected 2 strings');
    return { tag: 'boolean', val: args[0].val > args[1].val };
  }});

  envSet(env, 'string<=?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string<=?: expected 2 strings');
    return { tag: 'boolean', val: args[0].val <= args[1].val };
  }});

  envSet(env, 'string>=?', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string>=?: expected 2 strings');
    return { tag: 'boolean', val: args[0].val >= args[1].val };
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

  // call/cc as first-class value
  envSet(env, 'call/cc', { tag: 'callcc' } as SchemeVal);
  envSet(env, 'call-with-current-continuation', { tag: 'callcc' } as SchemeVal);

  // dynamic-wind
  envSet(env, 'dynamic-wind', { tag: 'dynamicWind' } as SchemeVal);

  // apply
  envSet(env, 'apply', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('apply requires at least 2 arguments');
    const fn = args[0];
    if (fn.tag !== 'procedure' && fn.tag !== 'closure' && fn.tag !== 'case-closure' && fn.tag !== 'continuation' && fn.tag !== 'callcc') throw new EvalError('apply: first argument must be a procedure');
    const lastArg = args[args.length - 1];
    const tailArgs = listToArray(lastArg);
    const prefixArgs = args.slice(1, -1);
    return callAny(fn, [...prefixArgs, ...tailArgs]);
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

  // Vector operations
  envSet(env, 'vector', { tag: 'procedure', val: (args) => {
    return { tag: 'vector', val: [...args] };
  }});

  envSet(env, 'make-vector', { tag: 'procedure', val: (args) => {
    if (args.length < 1 || args[0].tag !== 'number') throw new EvalError('make-vector: expected length');
    const len = args[0].val;
    const fill: SchemeVal = args.length >= 2 ? args[1] : makeExactInt(0);
    const arr: SchemeVal[] = [];
    for (let i = 0; i < len; i++) arr.push(fill);
    return { tag: 'vector', val: arr };
  }});

  envSet(env, 'vector-ref', { tag: 'procedure', val: (args) => {
    if (args.length !== 2 || args[0].tag !== 'vector' || args[1].tag !== 'number')
      throw new EvalError('vector-ref: expected vector and index');
    const idx = args[1].val;
    if (idx < 0 || idx >= args[0].val.length) throw new EvalError('vector-ref: index out of range');
    return args[0].val[idx];
  }});

  envSet(env, 'vector-set!', { tag: 'procedure', val: (args) => {
    if (args.length !== 3 || args[0].tag !== 'vector' || args[1].tag !== 'number')
      throw new EvalError('vector-set!: expected vector, index, value');
    const idx = args[1].val;
    if (idx < 0 || idx >= args[0].val.length) throw new EvalError('vector-set!: index out of range');
    args[0].val[idx] = args[2];
    return { tag: 'void' };
  }});

  envSet(env, 'vector-length', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'vector') throw new EvalError('vector-length: expected vector');
    return makeExactInt(args[0].val.length);
  }});

  envSet(env, 'vector?', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('vector? requires 1 argument');
    return { tag: 'boolean', val: args[0].tag === 'vector' };
  }});

  envSet(env, 'vector->list', { tag: 'procedure', val: (args) => {
    if (args.length !== 1 || args[0].tag !== 'vector') throw new EvalError('vector->list: expected vector');
    return arrayToList(args[0].val);
  }});

  envSet(env, 'list->vector', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('list->vector: expected list');
    return { tag: 'vector', val: listToArray(args[0]) };
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

function makeVariadicClosure(fixed: string[], rest: string, body: SchemeVal[], closedEnv: Env, _pos: string): SchemeVal {
  return { tag: 'closure', params: fixed, rest, body, closedEnv };
}

function callClosure(c: SchemeVal & { tag: 'closure' }, args: SchemeVal[]): SchemeVal {
  if (c.rest === null) {
    if (args.length !== c.params.length) throw new EvalError(`expected ${c.params.length} args, got ${args.length}`);
  } else {
    if (args.length < c.params.length) throw new EvalError(`expected at least ${c.params.length} args, got ${args.length}`);
  }
  const local = makeEnv(c.closedEnv);
  for (let i = 0; i < c.params.length; i++) envSet(local, c.params[i], args[i]);
  if (c.rest !== null) envSet(local, c.rest, arrayToList(args.slice(c.params.length)));
  let result: SchemeVal = { tag: 'void' };
  for (const expr of c.body) result = evaluate(expr, local);
  return result;
}

function callCaseClosure(cc: SchemeVal & { tag: 'case-closure' }, args: SchemeVal[]): SchemeVal {
  for (const cl of cc.clauses) {
    if (cl.rest !== null) {
      if (args.length >= cl.params.length) {
        const local = makeEnv(cc.closedEnv);
        for (let i = 0; i < cl.params.length; i++) envSet(local, cl.params[i], args[i]);
        envSet(local, cl.rest, arrayToList(args.slice(cl.params.length)));
        let result: SchemeVal = VOID;
        for (const bexpr of cl.body) result = evaluate(bexpr, local);
        return result;
      }
    } else {
      if (args.length === cl.params.length) {
        const local = makeEnv(cc.closedEnv);
        for (let i = 0; i < cl.params.length; i++) envSet(local, cl.params[i], args[i]);
        let result: SchemeVal = VOID;
        for (const bexpr of cl.body) result = evaluate(bexpr, local);
        return result;
      }
    }
  }
  throw new EvalError(`case-lambda: no matching clause for ${args.length} args`);
}

function callAny(proc: SchemeVal, args: SchemeVal[]): SchemeVal {
  if (proc.tag === 'closure') return callClosure(proc, args);
  if (proc.tag === 'case-closure') return callCaseClosure(proc, args);
  if (proc.tag === 'continuation') throw new ContinuationEscape(proc.k, args[0] ?? VOID, proc.winders);
  if (proc.tag === 'callcc') {
    if (args.length !== 1) throw new EvalError('call/cc requires 1 argument');
    // When call/cc is used as a first-class value via callAny (e.g. from apply/map),
    // we need to start a nested CEK evaluation
    return evaluate(
      { tag: 'list', val: [{ tag: 'symbol', val: 'call/cc' }, { tag: 'list', val: [{ tag: 'symbol', val: 'quote' }, args[0]] }] },
      makeEnv(null) // dummy env; won't be used since args[0] is already a value
    );
  }
  if (proc.tag === 'procedure') return proc.val(args);
  throw new EvalError('not a procedure');
}

// ── Macro expansion (syntax-rules) ─────────────────────────────────────

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'set!', 'lambda', 'begin', 'let', 'let*', 'letrec',
  'cond', 'and', 'or', 'define-syntax', 'syntax-rules', 'define-record-type',
  'case-lambda', 'let*', 'letrec', 'letrec*', 'case', 'do',
  'call/cc', 'call-with-current-continuation',
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

function expandMacroToForm(transformer: MacroTransformer, form: SchemeVal, env: Env): { form: SchemeVal; env: Env } {
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

      return { form: expanded, env: wrapperEnv };
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

type CEKState = { c: SchemeVal; e: Env; k: Cont; m: 0 | 1 };

function cekSetBody(body: SchemeVal[], benv: Env, k: Cont): CEKState {
  if (body.length === 0) return { c: VOID, e: benv, k, m: 1 };
  if (body.length === 1) return { c: body[0], e: benv, k, m: 0 };
  return { c: body[0], e: benv, k: { tag: 'seq', rest: body.slice(1), env: benv, next: k }, m: 0 };
}

function cekApply(proc: SchemeVal, args: SchemeVal[], epos: string, k: Cont): CEKState {
  if (proc.tag === 'closure') {
    if (proc.rest === null) {
      if (args.length !== proc.params.length) throw new EvalError(`${epos}: expected ${proc.params.length} args, got ${args.length}`);
    } else {
      if (args.length < proc.params.length) throw new EvalError(`${epos}: expected at least ${proc.params.length} args, got ${args.length}`);
    }
    const local = makeEnv(proc.closedEnv);
    for (let i = 0; i < proc.params.length; i++) envSet(local, proc.params[i], args[i]);
    if (proc.rest !== null) envSet(local, proc.rest, arrayToList(args.slice(proc.params.length)));
    return cekSetBody(proc.body, local, k);
  }
  if (proc.tag === 'case-closure') {
    for (const cl of proc.clauses) {
      if (cl.rest !== null ? args.length >= cl.params.length : args.length === cl.params.length) {
        const local = makeEnv(proc.closedEnv);
        for (let i = 0; i < cl.params.length; i++) envSet(local, cl.params[i], args[i]);
        if (cl.rest !== null) envSet(local, cl.rest, arrayToList(args.slice(cl.params.length)));
        return cekSetBody(cl.body, local, k);
      }
    }
    throw new EvalError(`${epos}: case-lambda: no matching clause for ${args.length} args`);
  }
  if (proc.tag === 'continuation') {
    const val = args.length > 0 ? args[0] : VOID;
    const targetWinders = proc.winders;
    const actions = computeWindActions(currentWinders, targetWinders);
    if (actions.length === 0) {
      currentWinders = targetWinders;
      return { c: val, e: null as any, k: proc.k, m: 1 };
    }
    return initiateWinding(actions, val, proc.k);
  }
  if (proc.tag === 'callcc') {
    if (args.length !== 1) throw new EvalError(`${epos}: call/cc requires 1 argument`);
    const cont: SchemeVal = { tag: 'continuation', k, winders: [...currentWinders] };
    return cekApply(args[0], [cont], epos, k);
  }
  if (proc.tag === 'dynamicWind') {
    if (args.length !== 3) throw new EvalError(`${epos}: dynamic-wind requires 3 arguments`);
    const [inThunk, bodyThunk, outThunk] = args;
    const nextK: Cont = { tag: 'dwAfterInK', bodyThunk, outThunk, inThunk, epos, next: k };
    return cekApply(inThunk, [], epos, nextK);
  }
  if (proc.tag === 'procedure') {
    try {
      return { c: proc.val(args), e: null as any, k, m: 1 };
    } catch (err) {
      if (err instanceof ContinuationEscape) {
        const actions = computeWindActions(currentWinders, err.winders);
        if (actions.length === 0) { currentWinders = err.winders; return { c: err.val, e: null as any, k: err.k, m: 1 }; }
        return initiateWinding(actions, err.val, err.k);
      }
      if (err instanceof EvalError && !/^\d/.test(err.message)) throw new EvalError(`${epos}: ${err.message}`);
      throw err;
    }
  }
  throw new EvalError(`${epos}: not a procedure`);
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  let m: 0 | 1 = 0;
  let c: SchemeVal = expr;
  let e: Env = env;
  let k: Cont = { tag: 'halt' };

  mainLoop: while (true) {
    if (m === 0) {
      // ── EVAL ──
      switch (c.tag) {
        case 'number': case 'boolean': case 'string': case 'char':
        case 'nil': case 'pair': case 'vector': case 'void':
          m = 1; continue;
        case 'symbol':
          c = envLookup(e, c.val, c.pos); m = 1; continue;
        case 'list': {
          const elems = c.val;
          const epos = c.pos ?? '?';
          if (elems.length === 0) throw new EvalError(`${epos}: empty application`);

          if (elems[0].tag === 'symbol') {
            const name = elems[0].val;

            if (name === 'quote') {
              if (elems.length !== 2) throw new EvalError(`${epos}: quote requires 1 argument`);
              c = quoteSyntax(elems[1]); m = 1; continue;
            }

            if (name === 'begin') {
              if (elems.length === 1) { c = VOID; m = 1; continue; }
              if (elems.length === 2) { c = elems[1]; continue; }
              k = { tag: 'seq', rest: elems.slice(2), env: e, next: k };
              c = elems[1]; continue;
            }

            if (name === 'if') {
              if (elems.length < 3 || elems.length > 4) throw new EvalError(`${epos}: if requires 2 or 3 arguments`);
              k = { tag: 'ifK', thenE: elems[2], elseE: elems[3], env: e, next: k };
              c = elems[1]; continue;
            }

            if (name === 'define') {
              if (elems.length < 3) throw new EvalError(`${epos}: define requires at least 2 arguments`);
              if (elems[1].tag === 'symbol') {
                k = { tag: 'defK', name: elems[1].val, env: e, next: k };
                c = elems[2]; continue;
              }
              if (elems[1].tag === 'list' && elems[1].val.length > 0 && elems[1].val[0].tag === 'symbol') {
                const fname = elems[1].val[0].val;
                const { fixed: params, rest } = parseParams(elems[1].val.slice(1), epos);
                envSet(e, fname, { tag: 'closure', params, rest, body: elems.slice(2), closedEnv: e });
                c = VOID; m = 1; continue;
              }
              throw new EvalError(`${epos}: invalid define`);
            }

            if (name === 'set!') {
              if (elems.length !== 3) throw new EvalError(`${epos}: set! requires 2 arguments`);
              if (elems[1].tag !== 'symbol') throw new EvalError(`${epos}: set! target must be a symbol`);
              k = { tag: 'setK', name: elems[1].val, env: e, epos, next: k };
              c = elems[2]; continue;
            }

            if (name === 'lambda') {
              if (elems.length < 3) throw new EvalError(`${epos}: lambda requires params and body`);
              const pl = elems[1];
              if (pl.tag === 'symbol') {
                c = { tag: 'closure', params: [], rest: pl.val, body: elems.slice(2), closedEnv: e };
                m = 1; continue;
              }
              if (pl.tag !== 'list') throw new EvalError(`${epos}: lambda params must be a list or symbol`);
              const { fixed: params, rest } = parseParams(pl.val, epos);
              c = { tag: 'closure', params, rest, body: elems.slice(2), closedEnv: e };
              m = 1; continue;
            }

            if (name === 'case-lambda') {
              if (elems.length < 2) throw new EvalError(`${epos}: case-lambda requires at least one clause`);
              const clauses: { params: string[]; rest: string | null; body: SchemeVal[] }[] = [];
              for (let i = 1; i < elems.length; i++) {
                const cl = elems[i];
                if (cl.tag !== 'list' || cl.val.length < 2) throw new EvalError(`${epos}: case-lambda: invalid clause`);
                const pl = cl.val[0];
                if (pl.tag === 'symbol') { clauses.push({ params: [], rest: pl.val, body: cl.val.slice(1) }); }
                else if (pl.tag !== 'list') { throw new EvalError(`${epos}: case-lambda: params must be a list or symbol`); }
                else { const { fixed, rest } = parseParams(pl.val, epos); clauses.push({ params: fixed, rest, body: cl.val.slice(1) }); }
              }
              c = { tag: 'case-closure', clauses, closedEnv: e } as SchemeVal;
              m = 1; continue;
            }

            if (name === 'let') {
              // Named let
              if (elems.length >= 4 && elems[1].tag === 'symbol') {
                const loopName = elems[1].val;
                const bs = elems[2];
                if (bs.tag !== 'list') throw new EvalError(`${epos}: let bindings must be a list`);
                const pairs: [string, SchemeVal][] = [];
                for (const b of bs.val) {
                  if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
                    throw new EvalError(`${epos}: invalid let binding`);
                  pairs.push([b.val[0].val, b.val[1]]);
                }
                const body = elems.slice(3);
                const params = pairs.map(p => p[0]);
                if (pairs.length === 0) {
                  const local = makeEnv(e);
                  envSet(local, loopName, { tag: 'closure', params: [], rest: null, body, closedEnv: local });
                  ({ c, e, k, m } = cekSetBody(body, local, k)); continue;
                }
                k = { tag: 'namedLetK', loopName, params, restInits: pairs.slice(1).map(p => p[1]), doneVals: [], outerEnv: e, body, next: k };
                c = pairs[0][1]; continue;
              }
              // Regular let
              if (elems.length < 3) throw new EvalError(`${epos}: let requires bindings and body`);
              const bs = elems[1];
              if (bs.tag !== 'list') throw new EvalError(`${epos}: let bindings must be a list`);
              const pairs: [string, SchemeVal][] = [];
              for (const b of bs.val) {
                if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
                  throw new EvalError(`${epos}: invalid let binding`);
                pairs.push([b.val[0].val, b.val[1]]);
              }
              const body = elems.slice(2);
              if (pairs.length === 0) { ({ c, e, k, m } = cekSetBody(body, makeEnv(e), k)); continue; }
              k = { tag: 'letK', cur: pairs[0][0], done: [], rest: pairs.slice(1), outerEnv: e, body, next: k };
              c = pairs[0][1]; continue;
            }

            if (name === 'let*') {
              if (elems.length < 3) throw new EvalError(`${epos}: let* requires bindings and body`);
              const bs = elems[1];
              if (bs.tag !== 'list') throw new EvalError(`${epos}: let* bindings must be a list`);
              const pairs: [string, SchemeVal][] = [];
              for (const b of bs.val) {
                if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
                  throw new EvalError(`${epos}: invalid let* binding`);
                pairs.push([b.val[0].val, b.val[1]]);
              }
              const body = elems.slice(2);
              const local = makeEnv(e);
              if (pairs.length === 0) { ({ c, e, k, m } = cekSetBody(body, local, k)); continue; }
              k = { tag: 'letStarK', curName: pairs[0][0], rest: pairs.slice(1), local, body, next: k };
              c = pairs[0][1]; e = local; continue;
            }

            if (name === 'letrec') {
              if (elems.length < 3) throw new EvalError(`${epos}: letrec requires bindings and body`);
              const bs = elems[1];
              if (bs.tag !== 'list') throw new EvalError(`${epos}: letrec bindings must be a list`);
              const pairs: [string, SchemeVal][] = [];
              for (const b of bs.val) {
                if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
                  throw new EvalError(`${epos}: invalid letrec binding`);
                pairs.push([b.val[0].val, b.val[1]]);
              }
              const body = elems.slice(2);
              const local = makeEnv(e);
              for (const [n] of pairs) envSet(local, n, VOID);
              if (pairs.length === 0) { ({ c, e, k, m } = cekSetBody(body, local, k)); continue; }
              k = { tag: 'letrecK', curName: pairs[0][0], done: [], rest: pairs.slice(1), local, body, next: k };
              c = pairs[0][1]; e = local; continue;
            }

            if (name === 'letrec*') {
              if (elems.length < 3) throw new EvalError(`${epos}: letrec* requires bindings and body`);
              const bs = elems[1];
              if (bs.tag !== 'list') throw new EvalError(`${epos}: letrec* bindings must be a list`);
              const pairs: [string, SchemeVal][] = [];
              for (const b of bs.val) {
                if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
                  throw new EvalError(`${epos}: invalid letrec* binding`);
                pairs.push([b.val[0].val, b.val[1]]);
              }
              const body = elems.slice(2);
              const local = makeEnv(e);
              if (pairs.length === 0) { ({ c, e, k, m } = cekSetBody(body, local, k)); continue; }
              // letrec* is like let* but in local env (identical to let* with local parent)
              k = { tag: 'letStarK', curName: pairs[0][0], rest: pairs.slice(1), local, body, next: k };
              c = pairs[0][1]; e = local; continue;
            }

            if (name === 'cond') {
              const clauses = elems.slice(1);
              if (clauses.length === 0) { c = VOID; m = 1; continue; }
              const first = clauses[0];
              if (first.tag !== 'list' || first.val.length < 1) throw new EvalError(`${epos}: invalid cond clause`);
              if (first.val[0].tag === 'symbol' && first.val[0].val === 'else') {
                ({ c, e, k, m } = cekSetBody(first.val.slice(1), e, k)); continue;
              }
              k = { tag: 'condK', clause: first, restClauses: clauses.slice(1), env: e, epos, next: k };
              c = first.val[0]; continue;
            }

            if (name === 'case') {
              if (elems.length < 2) throw new EvalError(`${epos}: case requires at least a key`);
              k = { tag: 'caseK', clauses: elems.slice(2), env: e, epos, next: k };
              c = elems[1]; continue;
            }

            if (name === 'and') {
              if (elems.length === 1) { c = { tag: 'boolean', val: true }; m = 1; continue; }
              if (elems.length === 2) { c = elems[1]; continue; }
              k = { tag: 'andK', rest: elems.slice(2), env: e, next: k };
              c = elems[1]; continue;
            }

            if (name === 'or') {
              if (elems.length === 1) { c = { tag: 'boolean', val: false }; m = 1; continue; }
              if (elems.length === 2) { c = elems[1]; continue; }
              k = { tag: 'orK', rest: elems.slice(2), env: e, next: k };
              c = elems[1]; continue;
            }

            if (name === 'do') {
              // Desugar to named let
              if (elems.length < 3) throw new EvalError(`${epos}: do requires variable bindings and test`);
              const varSpecs = elems[1];
              if (varSpecs.tag !== 'list') throw new EvalError(`${epos}: do variable specs must be a list`);
              const testClause = elems[2];
              if (testClause.tag !== 'list' || testClause.val.length < 1) throw new EvalError(`${epos}: do test clause must be a list`);
              const bodyExprs = elems.slice(3);
              const loopName = gensym('do');
              const letBindings: SchemeVal[] = [];
              const stepArgs: SchemeVal[] = [];
              for (const spec of varSpecs.val) {
                if (spec.tag !== 'list' || spec.val.length < 2 || spec.val[0].tag !== 'symbol')
                  throw new EvalError(`${epos}: invalid do variable spec`);
                letBindings.push({ tag: 'list', val: [spec.val[0], spec.val[1]] });
                stepArgs.push(spec.val.length >= 3 ? spec.val[2] : spec.val[0]);
              }
              const testExpr = testClause.val[0];
              const resultExprs = testClause.val.slice(1);
              const thenBranch = resultExprs.length === 0
                ? { tag: 'list' as const, val: [{ tag: 'symbol' as const, val: 'begin' }] }
                : resultExprs.length === 1 ? resultExprs[0]
                : { tag: 'list' as const, val: [{ tag: 'symbol' as const, val: 'begin' }, ...resultExprs] };
              const loopCall: SchemeVal = { tag: 'list', val: [{ tag: 'symbol', val: loopName }, ...stepArgs] };
              const elseBranch = bodyExprs.length === 0 ? loopCall
                : { tag: 'list' as const, val: [{ tag: 'symbol' as const, val: 'begin' }, ...bodyExprs, loopCall] };
              c = { tag: 'list', val: [
                { tag: 'symbol', val: 'let' },
                { tag: 'symbol', val: loopName },
                { tag: 'list', val: letBindings },
                { tag: 'list', val: [{ tag: 'symbol', val: 'if' }, testExpr, thenBranch, elseBranch] }
              ] };
              continue;
            }

            if (name === 'call/cc' || name === 'call-with-current-continuation') {
              if (elems.length !== 2) throw new EvalError(`${epos}: call/cc requires 1 argument`);
              k = { tag: 'callccK', next: k };
              c = elems[1]; continue;
            }

            if (name === 'define-syntax') {
              if (elems.length !== 3) throw new EvalError(`${epos}: define-syntax requires 2 arguments`);
              if (elems[1].tag !== 'symbol') throw new EvalError(`${epos}: define-syntax: name must be a symbol`);
              const macroName = elems[1].val;
              const sr = elems[2];
              if (sr.tag !== 'list' || sr.val.length < 2 || sr.val[0].tag !== 'symbol' || sr.val[0].val !== 'syntax-rules')
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
                if (rule.tag !== 'list' || rule.val.length !== 2) throw new EvalError(`${epos}: syntax-rules: invalid rule`);
                const pat = rule.val[0];
                if (pat.tag !== 'list' || pat.val.length < 1) throw new EvalError(`${epos}: syntax-rules: pattern must be a non-empty list`);
                rules.push({ pattern: pat.val.slice(1), template: rule.val[1] });
              }
              envSet(e, macroName, { tag: 'macro', transformer: { literals, rules, defEnv: e } });
              c = VOID; m = 1; continue;
            }

            if (name === 'define-record-type') {
              if (elems.length < 4) throw new EvalError(`${epos}: define-record-type requires at least 3 arguments`);
              const typeName = elems[1];
              if (typeName.tag !== 'symbol') throw new EvalError(`${epos}: define-record-type: type name must be a symbol`);
              const typeTag = Symbol(typeName.val);
              const ctorSpec = elems[2];
              if (ctorSpec.tag !== 'list' || ctorSpec.val.length < 1 || ctorSpec.val[0].tag !== 'symbol')
                throw new EvalError(`${epos}: define-record-type: invalid constructor`);
              const ctorName = ctorSpec.val[0].val;
              const ctorFields = ctorSpec.val.slice(1).map(f => {
                if (f.tag !== 'symbol') throw new EvalError(`${epos}: define-record-type: field must be a symbol`);
                return f.val;
              });
              const predSpec = elems[3];
              if (predSpec.tag !== 'symbol') throw new EvalError(`${epos}: define-record-type: predicate must be a symbol`);
              const predName = predSpec.val;
              const fieldAccessors: { field: string; accessor: string }[] = [];
              for (let i = 4; i < elems.length; i++) {
                const fd = elems[i];
                if (fd.tag !== 'list' || fd.val.length < 2 || fd.val[0].tag !== 'symbol' || fd.val[1].tag !== 'symbol')
                  throw new EvalError(`${epos}: define-record-type: invalid field spec`);
                fieldAccessors.push({ field: fd.val[0].val, accessor: fd.val[1].val });
              }
              envSet(e, ctorName, { tag: 'procedure', val: (args: SchemeVal[]) => {
                if (args.length !== ctorFields.length) throw new EvalError(`${ctorName}: expected ${ctorFields.length} args, got ${args.length}`);
                const fields = new Map<string, SchemeVal>();
                for (let i = 0; i < ctorFields.length; i++) fields.set(ctorFields[i], args[i]);
                return { tag: 'record', type: typeTag, fields } as SchemeVal;
              }});
              envSet(e, predName, { tag: 'procedure', val: (args: SchemeVal[]) => {
                if (args.length !== 1) throw new EvalError(`${predName}: expected 1 arg`);
                return { tag: 'boolean', val: args[0].tag === 'record' && (args[0] as any).type === typeTag };
              }});
              for (const { field, accessor } of fieldAccessors) {
                const fName = field; const aName = accessor;
                envSet(e, aName, { tag: 'procedure', val: (args: SchemeVal[]) => {
                  if (args.length !== 1) throw new EvalError(`${aName}: expected 1 arg`);
                  const rec = args[0];
                  if (rec.tag !== 'record' || rec.type !== typeTag) throw new EvalError(`${aName}: not a ${typeName.val}`);
                  return rec.fields.get(fName)!;
                }});
              }
              c = VOID; m = 1; continue;
            }
          }

          // Check for macro application
          {
            let headVal: SchemeVal | undefined;
            try { if (elems[0].tag === 'symbol') headVal = envLookup(e, elems[0].val); } catch { /* not bound */ }
            if (headVal && headVal.tag === 'macro') {
              const expanded = expandMacroToForm(headVal.transformer, c, e);
              c = expanded.form; e = expanded.env; continue;
            }
          }

          // Procedure application: eval operator, then args
          k = { tag: 'evalOpK', argExprs: elems.slice(1), env: e, epos, next: k };
          c = elems[0]; continue;
        }
        default:
          throw new EvalError(`${c.pos ?? '?'}: cannot evaluate`);
      }
    } else {
      // ── RETURN: value is in c, process continuation k ──
      switch (k.tag) {
        case 'halt': return c;

        case 'seq': {
          const f = k;
          if (f.rest.length === 1) { k = f.next; c = f.rest[0]; e = f.env; m = 0; continue; }
          k = { tag: 'seq', rest: f.rest.slice(1), env: f.env, next: f.next };
          c = f.rest[0]; e = f.env; m = 0; continue;
        }

        case 'ifK': {
          const f = k; k = f.next;
          if (isTruthy(c)) { c = f.thenE; e = f.env; m = 0; continue; }
          if (f.elseE) { c = f.elseE; e = f.env; m = 0; continue; }
          c = VOID; continue;
        }

        case 'defK': {
          const f = k; envSet(f.env, f.name, c); k = f.next; c = VOID; continue;
        }

        case 'setK': {
          const f = k; envMutate(f.env, f.name, c, f.epos); k = f.next; c = VOID; continue;
        }

        case 'evalOpK': {
          const f = k;
          const op = c;
          const a = f.argExprs;
          if (a.length === 0) { ({ c, e, k, m } = cekApply(op, [], f.epos, f.next)); continue; }
          // Right-to-left argument evaluation (matches Chez Scheme)
          k = { tag: 'evalArgsK', op, done: [], rest: a.slice(0, -1), env: f.env, epos: f.epos, next: f.next };
          c = a[a.length - 1]; e = f.env; m = 0; continue;
        }

        case 'evalArgsK': {
          const f = k;
          const done = [c, ...f.done]; // prepend (right-to-left accumulation)
          if (f.rest.length === 0) { ({ c, e, k, m } = cekApply(f.op, done, f.epos, f.next)); continue; }
          k = { tag: 'evalArgsK', op: f.op, done, rest: f.rest.slice(0, -1), env: f.env, epos: f.epos, next: f.next };
          c = f.rest[f.rest.length - 1]; e = f.env; m = 0; continue;
        }

        case 'callccK': {
          const f = k;
          const func = c;
          const cont: SchemeVal = { tag: 'continuation', k: f.next, winders: [...currentWinders] };
          ({ c, e, k, m } = cekApply(func, [cont], '?', f.next));
          continue;
        }

        case 'letK': {
          const f = k;
          const done: [string, SchemeVal][] = [...f.done, [f.cur, c]];
          if (f.rest.length === 0) {
            const local = makeEnv(f.outerEnv);
            for (const [n, v] of done) envSet(local, n, v);
            ({ c, e, k, m } = cekSetBody(f.body, local, f.next)); continue;
          }
          k = { tag: 'letK', cur: f.rest[0][0], done, rest: f.rest.slice(1), outerEnv: f.outerEnv, body: f.body, next: f.next };
          c = f.rest[0][1]; e = f.outerEnv; m = 0; continue;
        }

        case 'letStarK': {
          const f = k;
          envSet(f.local, f.curName, c);
          if (f.rest.length === 0) { ({ c, e, k, m } = cekSetBody(f.body, f.local, f.next)); continue; }
          k = { tag: 'letStarK', curName: f.rest[0][0], rest: f.rest.slice(1), local: f.local, body: f.body, next: f.next };
          c = f.rest[0][1]; e = f.local; m = 0; continue;
        }

        case 'letrecK': {
          const f = k;
          const done: [string, SchemeVal][] = [...f.done, [f.curName, c]];
          if (f.rest.length === 0) {
            for (const [n, v] of done) envSet(f.local, n, v);
            ({ c, e, k, m } = cekSetBody(f.body, f.local, f.next)); continue;
          }
          k = { tag: 'letrecK', curName: f.rest[0][0], done, rest: f.rest.slice(1), local: f.local, body: f.body, next: f.next };
          c = f.rest[0][1]; e = f.local; m = 0; continue;
        }

        case 'namedLetK': {
          const f = k;
          const doneVals = [...f.doneVals, c];
          if (f.restInits.length === 0) {
            const local = makeEnv(f.outerEnv);
            const loopClosure: SchemeVal = { tag: 'closure', params: f.params, rest: null, body: f.body, closedEnv: local };
            envSet(local, f.loopName, loopClosure);
            const inner = makeEnv(local);
            for (let i = 0; i < f.params.length; i++) envSet(inner, f.params[i], doneVals[i]);
            ({ c, e, k, m } = cekSetBody(f.body, inner, f.next)); continue;
          }
          k = { tag: 'namedLetK', loopName: f.loopName, params: f.params, restInits: f.restInits.slice(1), doneVals, outerEnv: f.outerEnv, body: f.body, next: f.next };
          c = f.restInits[0]; e = f.outerEnv; m = 0; continue;
        }

        case 'condK': {
          const f = k;
          if (isTruthy(c)) {
            const cl = f.clause;
            k = f.next;
            if (cl.tag === 'list' && cl.val.length === 1) { /* return test value */ continue; }
            if (cl.tag === 'list' && cl.val.length >= 3 && cl.val[1].tag === 'symbol' && cl.val[1].val === '=>') {
              k = { tag: 'condArrowK', testVal: c, env: f.env, epos: f.epos, next: k };
              c = cl.val[2]; e = f.env; m = 0; continue;
            }
            if (cl.tag === 'list') { ({ c, e, k, m } = cekSetBody(cl.val.slice(1), f.env, k)); continue; }
          }
          // Test was falsy
          if (f.restClauses.length === 0) { k = f.next; c = VOID; continue; }
          const next = f.restClauses[0];
          if (next.tag !== 'list' || next.val.length < 1) throw new EvalError(`${f.epos}: invalid cond clause`);
          if (next.val[0].tag === 'symbol' && next.val[0].val === 'else') {
            ({ c, e, k, m } = cekSetBody(next.val.slice(1), f.env, f.next)); continue;
          }
          k = { tag: 'condK', clause: next, restClauses: f.restClauses.slice(1), env: f.env, epos: f.epos, next: f.next };
          c = next.val[0]; e = f.env; m = 0; continue;
        }

        case 'condArrowK': {
          const f = k;
          ({ c, e, k, m } = cekApply(c, [f.testVal], f.epos, f.next)); continue;
        }

        case 'caseK': {
          const f = k;
          const kn = f.next;
          const key = c;
          let matched = false;
          for (const clause of f.clauses) {
            if (clause.tag !== 'list' || clause.val.length < 2) throw new EvalError(`${f.epos}: invalid case clause`);
            if (clause.val[0].tag === 'symbol' && clause.val[0].val === 'else') {
              ({ c, e, k, m } = cekSetBody(clause.val.slice(1), f.env, kn)); matched = true; break;
            }
            if (clause.val[0].tag !== 'list') throw new EvalError(`${f.epos}: case clause datums must be a list`);
            let found = false;
            for (const datum of clause.val[0].val) {
              if (schemeEqv(key, quoteSyntax(datum))) { found = true; break; }
            }
            if (found) {
              ({ c, e, k, m } = cekSetBody(clause.val.slice(1), f.env, kn)); matched = true; break;
            }
          }
          if (!matched) { k = kn; c = VOID; }
          continue;
        }

        case 'andK': {
          const f = k;
          if (isFalsy(c)) { k = f.next; continue; }
          if (f.rest.length === 1) { k = f.next; c = f.rest[0]; e = f.env; m = 0; continue; }
          k = { tag: 'andK', rest: f.rest.slice(1), env: f.env, next: f.next };
          c = f.rest[0]; e = f.env; m = 0; continue;
        }

        case 'orK': {
          const f = k;
          if (isTruthy(c)) { k = f.next; continue; }
          if (f.rest.length === 1) { k = f.next; c = f.rest[0]; e = f.env; m = 0; continue; }
          k = { tag: 'orK', rest: f.rest.slice(1), env: f.env, next: f.next };
          c = f.rest[0]; e = f.env; m = 0; continue;
        }

        case 'dwAfterInK': {
          const f = k;
          const winder: Winder = { inThunk: f.inThunk, outThunk: f.outThunk };
          currentWinders = [...currentWinders, winder];
          k = { tag: 'dwAfterBodyK', outThunk: f.outThunk, winder, next: f.next };
          ({ c, e, k, m } = cekApply(f.bodyThunk, [], f.epos, k));
          continue;
        }

        case 'dwAfterBodyK': {
          const f = k;
          const bodyVal = c;
          currentWinders = currentWinders.filter(w => w !== f.winder);
          k = { tag: 'dwAfterOutK', bodyVal, next: f.next };
          ({ c, e, k, m } = cekApply(f.outThunk, [], '?', k));
          continue;
        }

        case 'dwAfterOutK': {
          const f = k;
          c = f.bodyVal;
          k = f.next;
          continue;
        }

        case 'dwWindK': {
          const f = k;
          currentWinders = f.setWindersTo;
          if (f.remaining.length === 0) {
            c = f.val; k = f.targetK; m = 1; continue;
          }
          const next = f.remaining[0];
          k = { tag: 'dwWindK', setWindersTo: next.setWindersTo, remaining: f.remaining.slice(1), val: f.val, targetK: f.targetK };
          ({ c, e, k, m } = cekApply(next.thunk, [], '?', k));
          continue;
        }

        default:
          throw new EvalError('unknown continuation frame');
      }
    }
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

function display(val: SchemeVal, seen?: Set<SchemeVal>): string {
  switch (val.tag) {
    case 'number': return displayNumber(val);
    case 'boolean': return val.val ? '#t' : '#f';
    case 'string': return `"${val.val}"`;
    case 'symbol': return val.val;
    case 'char': return `#\\${val.val}`;
    case 'list': return `(${val.val.map(v => display(v, seen)).join(' ')})`;
    case 'nil': return '()';
    case 'pair': {
      if (!seen) seen = new Set();
      if (seen.has(val)) return '(...)';
      seen.add(val);
      let out = '(' + display(val.car, seen);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { out += ' ...'; break; }
        seen.add(cur);
        out += ' ' + display(cur.car, seen);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil' && cur.tag !== 'pair') {
        out += ' . ' + display(cur, seen);
      }
      return out + ')';
    }
    case 'void': return '';
    case 'procedure': return '#<procedure>';
    case 'closure': return '#<procedure>';
    case 'continuation': return '#<procedure>';
    case 'callcc': return '#<procedure>';
    case 'dynamicWind': return '#<procedure>';
    case 'case-closure': return '#<procedure>';
    case 'macro': return '#<macro>';
    case 'record': return '#<record>';
    case 'vector': return '#(' + val.val.map(v => display(v, seen)).join(' ') + ')';
  }
}

// ── Public API ─────────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  currentWinders = [];
  const env = makeGlobalEnv();
  // Evaluate all expressions in a single CEK run so continuations span forms
  const beginExpr: SchemeVal = exprs.length === 1 ? exprs[0]
    : { tag: 'list', val: [{ tag: 'symbol', val: 'begin' } as SchemeVal, ...exprs] };
  const result = evaluate(beginExpr, env);
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  currentWinders = [];
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  const beginExpr: SchemeVal = exprs.length === 1 ? exprs[0]
    : { tag: 'list', val: [{ tag: 'symbol', val: 'begin' } as SchemeVal, ...exprs] };
  const result = evaluate(beginExpr, env);
  return { result: display(result), output: outputBuf.join('') };
}
