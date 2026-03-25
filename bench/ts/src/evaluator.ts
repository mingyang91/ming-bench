import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type BuiltinFn = (args: SchemeVal[]) => SchemeVal;

interface MacroClause { pattern: SchemeVal[]; template: SchemeVal }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; mutable?: boolean; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }  // syntax only (parsed S-expr)
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; rest?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; fn: BuiltinFn; pos?: Pos }
  | { tag: 'macro'; literals: string[]; clauses: MacroClause[]; defEnv: Env; pos?: Pos }
  | { tag: 'record'; typeName: string; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'case-lambda'; clauses: { params: string[]; rest?: string; body: SchemeVal[] }[]; env: Env; pos?: Pos }
  | { tag: 'vector'; elements: SchemeVal[]; pos?: Pos };

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}` : '?:?';
}

function errAt(msg: string, pos?: Pos): EvalError {
  return new EvalError(`${posStr(pos)}: ${msg}`);
}

const NIL: SchemeVal = { tag: 'nil' };

// ── Rational helpers ────────────────────────────────────────────
function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { [a, b] = [b, a % b]; }
  return a;
}

function makeRat(num: number, den: number): SchemeVal {
  if (den === 0) throw new EvalError('division by zero');
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(Math.abs(num), den);
  return { tag: 'rational', num: num / g, den: den / g };
}

function toFloat(v: SchemeVal): number {
  if (v.tag === 'number') return v.value;
  if (v.tag === 'rational') return v.num / v.den;
  throw new EvalError('expected number');
}

function isNumeric(v: SchemeVal): boolean {
  return v.tag === 'number' || v.tag === 'rational';
}

function assertNumeric(v: SchemeVal, op: string): void {
  if (!isNumeric(v)) throw new EvalError(`${op}: expected number`);
}

function anyInexact(args: SchemeVal[]): boolean {
  return args.some(a => a.tag === 'number');
}

function ratAdd(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (a.tag === 'rational' && b.tag === 'rational') {
    return makeRat(a.num * b.den + b.num * a.den, a.den * b.den);
  }
  return { tag: 'number', value: toFloat(a) + toFloat(b) };
}

function ratSub(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (a.tag === 'rational' && b.tag === 'rational') {
    return makeRat(a.num * b.den - b.num * a.den, a.den * b.den);
  }
  return { tag: 'number', value: toFloat(a) - toFloat(b) };
}

function ratMul(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (a.tag === 'rational' && b.tag === 'rational') {
    return makeRat(a.num * b.num, a.den * b.den);
  }
  return { tag: 'number', value: toFloat(a) * toFloat(b) };
}

function ratDiv(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (a.tag === 'rational' && b.tag === 'rational') {
    if (b.num === 0) throw new EvalError('division by zero');
    return makeRat(a.num * b.den, a.den * b.num);
  }
  const bv = toFloat(b);
  if (bv === 0) throw new EvalError('division by zero');
  return { tag: 'number', value: toFloat(a) / bv };
}

// ── Output Buffer ────────────────────────────────────────────────
let outputBuffer = '';

function displayValUnquoted(val: SchemeVal): string {
  if (val.tag === 'string') return val.value;
  if (val.tag === 'char') return val.value;
  return displayVal(val);
}

function writeVal(val: SchemeVal): string {
  return displayVal(val);
}

function makeList(items: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: items[i], cdr: result };
  }
  return result;
}

function pairToArray(val: SchemeVal): SchemeVal[] {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag !== 'nil') throw new EvalError('not a proper list');
  return result;
}

// ── Environment ───────────────────────────────────────────────────

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent: Env | null = null) {}

  get(name: string, pos?: Pos): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name, pos);
    throw errAt(`unbound variable: ${name}`, pos);
  }

  set(name: string, val: SchemeVal, pos?: Pos): void {
    if (this.bindings.has(name)) { this.bindings.set(name, val); return; }
    if (this.parent) { this.parent.set(name, val, pos); return; }
    throw errAt(`set!: unbound variable: ${name}`, pos);
  }

  define(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }
}

// ── Builtins ──────────────────────────────────────────────────────

function expectNum(v: SchemeVal, op: string): number {
  if (v.tag === 'number') return v.value;
  if (v.tag === 'rational') return v.num / v.den;
  throw new EvalError(`${op}: expected number`);
}

function schemeEqv(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a.tag !== b.tag) return false;
  if (a.tag === 'boolean' && b.tag === 'boolean') return a.value === b.value;
  if (a.tag === 'symbol' && b.tag === 'symbol') return a.value === b.value;
  if (a.tag === 'char' && b.tag === 'char') return a.value === b.value;
  if (a.tag === 'nil' && b.tag === 'nil') return true;
  if (a.tag === 'void' && b.tag === 'void') return true;
  return a === b;
}

function schemeEqual(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a.tag !== b.tag) return false;
  if (a.tag === 'boolean' && b.tag === 'boolean') return a.value === b.value;
  if (a.tag === 'string' && b.tag === 'string') return a.value === b.value;
  if (a.tag === 'symbol' && b.tag === 'symbol') return a.value === b.value;
  if (a.tag === 'char' && b.tag === 'char') return a.value === b.value;
  if (a.tag === 'nil' && b.tag === 'nil') return true;
  if (a.tag === 'pair' && b.tag === 'pair') return schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
  if (a.tag === 'vector' && b.tag === 'vector') {
    if (a.elements.length !== b.elements.length) return false;
    for (let i = 0; i < a.elements.length; i++) {
      if (!schemeEqual(a.elements[i], b.elements[i])) return false;
    }
    return true;
  }
  return false;
}

function makeGlobalEnv(): Env {
  const env = new Env();

  function defBuiltin(name: string, fn: BuiltinFn) {
    env.define(name, { tag: 'builtin', name, fn });
  }

  defBuiltin('+', (args) => {
    for (const a of args) assertNumeric(a, '+');
    if (args.length === 0) return makeRat(0, 1);
    let result: SchemeVal = args[0];
    for (let i = 1; i < args.length; i++) result = ratAdd(result, args[i]);
    return result;
  });

  defBuiltin('-', (args) => {
    if (args.length === 0) throw new EvalError('-: expected at least 1 argument');
    for (const a of args) assertNumeric(a, '-');
    if (args.length === 1) {
      if (args[0].tag === 'rational') return makeRat(-args[0].num, args[0].den);
      return { tag: 'number', value: -args[0].value };
    }
    let result: SchemeVal = args[0];
    for (let i = 1; i < args.length; i++) result = ratSub(result, args[i]);
    return result;
  });

  defBuiltin('*', (args) => {
    for (const a of args) assertNumeric(a, '*');
    if (args.length === 0) return makeRat(1, 1);
    let result: SchemeVal = args[0];
    for (let i = 1; i < args.length; i++) result = ratMul(result, args[i]);
    return result;
  });

  defBuiltin('/', (args) => {
    if (args.length < 1) throw new EvalError('/: expected at least 1 argument');
    for (const a of args) assertNumeric(a, '/');
    if (args.length === 1) {
      // (/ x) => 1/x
      return ratDiv(makeRat(1, 1), args[0]);
    }
    let result: SchemeVal = args[0];
    for (let i = 1; i < args.length; i++) result = ratDiv(result, args[i]);
    return result;
  });

  for (const op of ['<', '>', '=', '>=', '<='] as const) {
    defBuiltin(op, (args) => {
      if (args.length !== 2) throw new EvalError(`${op}: expected 2 arguments`);
      assertNumeric(args[0], op); assertNumeric(args[1], op);
      const a = toFloat(args[0]), b = toFloat(args[1]);
      let r: boolean;
      switch (op) {
        case '<': r = a < b; break;
        case '>': r = a > b; break;
        case '=': r = a === b; break;
        case '>=': r = a >= b; break;
        case '<=': r = a <= b; break;
      }
      return { tag: 'boolean', value: r };
    });
  }

  // List operations
  defBuiltin('cons', (args) => {
    if (args.length !== 2) throw new EvalError('cons: expected 2 arguments');
    return { tag: 'pair', car: args[0], cdr: args[1] };
  });

  defBuiltin('car', (args) => {
    if (args.length !== 1) throw new EvalError('car: expected 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('car: expected pair');
    return args[0].car;
  });

  defBuiltin('cdr', (args) => {
    if (args.length !== 1) throw new EvalError('cdr: expected 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('cdr: expected pair');
    return args[0].cdr;
  });

  defBuiltin('null?', (args) => {
    if (args.length !== 1) throw new EvalError('null?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'nil' };
  });

  defBuiltin('list', (args) => {
    return makeList(args);
  });

  defBuiltin('length', (args) => {
    if (args.length !== 1) throw new EvalError('length: expected 1 argument');
    let count = 0;
    let cur = args[0];
    while (cur.tag === 'pair') { count++; cur = cur.cdr; }
    if (cur.tag !== 'nil') throw new EvalError('length: expected proper list');
    return makeRat(count, 1);
  });

  defBuiltin('append', (args) => {
    if (args.length === 0) return NIL;
    if (args.length === 1) return args[0];
    // Append all lists
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
      const items = pairToArray(args[i]);
      for (let j = items.length - 1; j >= 0; j--) {
        result = { tag: 'pair', car: items[j], cdr: result };
      }
    }
    return result;
  });

  // Type predicates
  defBuiltin('number?', (args) => {
    if (args.length !== 1) throw new EvalError('number?: expected 1 argument');
    return { tag: 'boolean', value: isNumeric(args[0]) };
  });

  defBuiltin('exact?', (args) => {
    if (args.length !== 1) throw new EvalError('exact?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'rational' };
  });

  defBuiltin('inexact?', (args) => {
    if (args.length !== 1) throw new EvalError('inexact?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'number' };
  });

  defBuiltin('integer?', (args) => {
    if (args.length !== 1) throw new EvalError('integer?: expected 1 argument');
    if (args[0].tag === 'rational') return { tag: 'boolean', value: args[0].den === 1 };
    if (args[0].tag === 'number') return { tag: 'boolean', value: Number.isInteger(args[0].value) };
    return { tag: 'boolean', value: false };
  });

  defBuiltin('rational?', (args) => {
    if (args.length !== 1) throw new EvalError('rational?: expected 1 argument');
    return { tag: 'boolean', value: isNumeric(args[0]) };
  });

  defBuiltin('exact->inexact', (args) => {
    if (args.length !== 1) throw new EvalError('exact->inexact: expected 1 argument');
    assertNumeric(args[0], 'exact->inexact');
    return { tag: 'number', value: toFloat(args[0]) };
  });

  defBuiltin('inexact->exact', (args) => {
    if (args.length !== 1) throw new EvalError('inexact->exact: expected 1 argument');
    assertNumeric(args[0], 'inexact->exact');
    if (args[0].tag === 'rational') return args[0];
    // Convert float to rational using continued fraction approach
    const v = args[0].value;
    if (Number.isInteger(v)) return makeRat(v, 1);
    // Use a simple approach: multiply by power of 2 to get exact fraction
    let num = v, den = 1;
    while (num !== Math.floor(num) && den < 1e15) { num *= 2; den *= 2; }
    return makeRat(Math.round(num), den);
  });

  defBuiltin('numerator', (args) => {
    if (args.length !== 1) throw new EvalError('numerator: expected 1 argument');
    assertNumeric(args[0], 'numerator');
    if (args[0].tag === 'rational') return makeRat(args[0].num, 1);
    return { tag: 'number', value: args[0].value };
  });

  defBuiltin('denominator', (args) => {
    if (args.length !== 1) throw new EvalError('denominator: expected 1 argument');
    assertNumeric(args[0], 'denominator');
    if (args[0].tag === 'rational') return makeRat(args[0].den, 1);
    return { tag: 'number', value: 1 };
  });

  defBuiltin('boolean?', (args) => {
    if (args.length !== 1) throw new EvalError('boolean?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  });

  defBuiltin('string?', (args) => {
    if (args.length !== 1) throw new EvalError('string?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'string' };
  });

  defBuiltin('symbol?', (args) => {
    if (args.length !== 1) throw new EvalError('symbol?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  defBuiltin('pair?', (args) => {
    if (args.length !== 1) throw new EvalError('pair?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'pair' };
  });

  defBuiltin('char?', (args) => {
    if (args.length !== 1) throw new EvalError('char?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'char' };
  });

  // I/O
  defBuiltin('display', (args) => {
    if (args.length !== 1) throw new EvalError('display: expected 1 argument');
    outputBuffer += displayValUnquoted(args[0]);
    return { tag: 'void' };
  });

  defBuiltin('write', (args) => {
    if (args.length !== 1) throw new EvalError('write: expected 1 argument');
    outputBuffer += writeVal(args[0]);
    return { tag: 'void' };
  });

  defBuiltin('newline', (args) => {
    if (args.length !== 0) throw new EvalError('newline: expected 0 arguments');
    outputBuffer += '\n';
    return { tag: 'void' };
  });

  // String operations
  defBuiltin('string-append', (args) => {
    let result = '';
    for (const a of args) {
      if (a.tag !== 'string') throw new EvalError('string-append: expected string');
      result += a.value;
    }
    return { tag: 'string', value: result };
  });

  defBuiltin('string-length', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string-length: expected 1 string argument');
    return makeRat(args[0].value.length, 1);
  });

  defBuiltin('substring', (args) => {
    if (args.length !== 3) throw new EvalError('substring: expected 3 arguments');
    if (args[0].tag !== 'string') throw new EvalError('substring: expected string');
    const start = expectNum(args[1], 'substring');
    const end = expectNum(args[2], 'substring');
    return { tag: 'string', value: args[0].value.substring(start, end) };
  });

  defBuiltin('string->number', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string->number: expected 1 string argument');
    const s = args[0].value;
    // Check for rational notation
    const ratMatch = s.match(/^(-?\d+)\/(\d+)$/);
    if (ratMatch) return makeRat(parseInt(ratMatch[1], 10), parseInt(ratMatch[2], 10));
    const n = Number(s);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    if (Number.isInteger(n) && !s.includes('.')) return makeRat(n, 1);
    return { tag: 'number', value: n };
  });

  defBuiltin('number->string', (args) => {
    if (args.length !== 1 || !isNumeric(args[0]))
      throw new EvalError('number->string: expected 1 number argument');
    return { tag: 'string', value: displayVal(args[0]) };
  });

  defBuiltin('string-ref', (args) => {
    if (args.length !== 2) throw new EvalError('string-ref: expected 2 arguments');
    if (args[0].tag !== 'string') throw new EvalError('string-ref: expected string');
    const idx = expectNum(args[1], 'string-ref');
    return { tag: 'char', value: args[0].value[idx] };
  });

  defBuiltin('string-copy', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string-copy: expected 1 string argument');
    return { tag: 'string', value: args[0].value, mutable: true };
  });

  defBuiltin('string-set!', (args) => {
    if (args.length !== 3 || args[0].tag !== 'string' || args[2].tag !== 'char')
      throw new EvalError('string-set!: expected string, index, char');
    const s = args[0];
    if (!s.mutable) throw new EvalError('string-set!: strings are immutable');
    const idx = expectNum(args[1], 'string-set!');
    if (idx < 0 || idx >= s.value.length) throw new EvalError('string-set!: index out of range');
    s.value = s.value.substring(0, idx) + args[2].value + s.value.substring(idx + 1);
    return { tag: 'void' };
  });

  defBuiltin('string->list', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string->list: expected 1 string argument');
    let result: SchemeVal = { tag: 'nil' };
    const s = args[0].value;
    for (let i = s.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: { tag: 'char', value: s[i] }, cdr: result };
    }
    return result;
  });

  defBuiltin('list->string', (args) => {
    if (args.length !== 1) throw new EvalError('list->string: expected 1 argument');
    let result = '';
    let cur = args[0];
    while (cur.tag === 'pair') {
      if (cur.car.tag !== 'char') throw new EvalError('list->string: expected list of characters');
      result += cur.car.value;
      cur = cur.cdr;
    }
    if (cur.tag !== 'nil') throw new EvalError('list->string: expected proper list');
    return { tag: 'string', value: result };
  });

  defBuiltin('char->integer', (args) => {
    if (args.length !== 1 || args[0].tag !== 'char')
      throw new EvalError('char->integer: expected 1 char argument');
    return { tag: 'number', value: args[0].value.charCodeAt(0) };
  });

  defBuiltin('integer->char', (args) => {
    if (args.length !== 1 || args[0].tag !== 'number')
      throw new EvalError('integer->char: expected 1 integer argument');
    return { tag: 'char', value: String.fromCharCode(args[0].value) };
  });

  defBuiltin('symbol->string', (args) => {
    if (args.length !== 1 || args[0].tag !== 'symbol')
      throw new EvalError('symbol->string: expected 1 symbol argument');
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('string->symbol', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string->symbol: expected 1 string argument');
    return { tag: 'symbol', value: args[0].value };
  });

  // Equality
  defBuiltin('eq?', (args) => {
    if (args.length !== 2) throw new EvalError('eq?: expected 2 arguments');
    const a = args[0], b = args[1];
    if (a.tag !== b.tag) return { tag: 'boolean', value: false };
    if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'rational' && b.tag === 'rational') return { tag: 'boolean', value: a.num === b.num && a.den === b.den };
    if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'string' && b.tag === 'string') return { tag: 'boolean', value: a === b };
    if (a.tag === 'nil' && b.tag === 'nil') return { tag: 'boolean', value: true };
    if (a.tag === 'void' && b.tag === 'void') return { tag: 'boolean', value: true };
    return { tag: 'boolean', value: a === b };
  });

  defBuiltin('eqv?', (args) => {
    if (args.length !== 2) throw new EvalError('eqv?: expected 2 arguments');
    return { tag: 'boolean', value: schemeEqv(args[0], args[1]) };
  });

  defBuiltin('equal?', (args) => {
    if (args.length !== 2) throw new EvalError('equal?: expected 2 arguments');
    return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
  });

  // Numeric utilities
  defBuiltin('abs', (args) => {
    if (args.length !== 1) throw new EvalError('abs: expected 1 argument');
    assertNumeric(args[0], 'abs');
    if (args[0].tag === 'rational') return makeRat(Math.abs(args[0].num), args[0].den);
    return { tag: 'number', value: Math.abs(args[0].value) };
  });

  defBuiltin('modulo', (args) => {
    if (args.length !== 2) throw new EvalError('modulo: expected 2 arguments');
    const a = expectNum(args[0], 'modulo'), b = expectNum(args[1], 'modulo');
    if (b === 0) throw new EvalError('modulo: division by zero');
    const r = ((a % b) + b) % b;
    if (args[0].tag === 'rational' && args[1].tag === 'rational') return makeRat(r, 1);
    return { tag: 'number', value: r };
  });

  defBuiltin('remainder', (args) => {
    if (args.length !== 2) throw new EvalError('remainder: expected 2 arguments');
    const a = expectNum(args[0], 'remainder'), b = expectNum(args[1], 'remainder');
    if (b === 0) throw new EvalError('remainder: division by zero');
    const r = a % b;
    if (args[0].tag === 'rational' && args[1].tag === 'rational') return makeRat(r, 1);
    return { tag: 'number', value: r };
  });

  defBuiltin('quotient', (args) => {
    if (args.length !== 2) throw new EvalError('quotient: expected 2 arguments');
    const a = expectNum(args[0], 'quotient'), b = expectNum(args[1], 'quotient');
    if (b === 0) throw new EvalError('quotient: division by zero');
    const r = Math.trunc(a / b);
    if (args[0].tag === 'rational' && args[1].tag === 'rational') return makeRat(r, 1);
    return { tag: 'number', value: r };
  });

  defBuiltin('min', (args) => {
    if (args.length < 1) throw new EvalError('min: expected at least 1 argument');
    for (const a of args) assertNumeric(a, 'min');
    let best = args[0];
    for (let i = 1; i < args.length; i++) {
      if (toFloat(args[i]) < toFloat(best)) best = args[i];
    }
    return best;
  });

  defBuiltin('max', (args) => {
    if (args.length < 1) throw new EvalError('max: expected at least 1 argument');
    for (const a of args) assertNumeric(a, 'max');
    let best = args[0];
    for (let i = 1; i < args.length; i++) {
      if (toFloat(args[i]) > toFloat(best)) best = args[i];
    }
    return best;
  });

  defBuiltin('expt', (args) => {
    if (args.length !== 2) throw new EvalError('expt: expected 2 arguments');
    assertNumeric(args[0], 'expt'); assertNumeric(args[1], 'expt');
    if (args[0].tag === 'rational' && args[1].tag === 'rational' && args[1].den === 1) {
      const exp = args[1].num;
      if (exp >= 0) {
        return makeRat(Math.pow(args[0].num, exp), Math.pow(args[0].den, exp));
      }
      return makeRat(Math.pow(args[0].den, -exp), Math.pow(args[0].num, -exp));
    }
    return { tag: 'number', value: Math.pow(toFloat(args[0]), toFloat(args[1])) };
  });

  defBuiltin('zero?', (args) => {
    if (args.length !== 1) throw new EvalError('zero?: expected 1 argument');
    assertNumeric(args[0], 'zero?');
    return { tag: 'boolean', value: toFloat(args[0]) === 0 };
  });

  defBuiltin('positive?', (args) => {
    if (args.length !== 1) throw new EvalError('positive?: expected 1 argument');
    assertNumeric(args[0], 'positive?');
    return { tag: 'boolean', value: toFloat(args[0]) > 0 };
  });

  defBuiltin('negative?', (args) => {
    if (args.length !== 1) throw new EvalError('negative?: expected 1 argument');
    assertNumeric(args[0], 'negative?');
    return { tag: 'boolean', value: toFloat(args[0]) < 0 };
  });

  defBuiltin('odd?', (args) => {
    if (args.length !== 1) throw new EvalError('odd?: expected 1 argument');
    const n = expectNum(args[0], 'odd?');
    return { tag: 'boolean', value: Math.abs(n) % 2 === 1 };
  });

  defBuiltin('even?', (args) => {
    if (args.length !== 1) throw new EvalError('even?: expected 1 argument');
    const n = expectNum(args[0], 'even?');
    return { tag: 'boolean', value: n % 2 === 0 };
  });

  // List utilities
  defBuiltin('list?', (args) => {
    if (args.length !== 1) throw new EvalError('list?: expected 1 argument');
    let cur = args[0];
    while (cur.tag === 'pair') cur = cur.cdr;
    return { tag: 'boolean', value: cur.tag === 'nil' };
  });

  defBuiltin('list-ref', (args) => {
    if (args.length !== 2) throw new EvalError('list-ref: expected 2 arguments');
    let cur = args[0];
    let idx = expectNum(args[1], 'list-ref');
    while (idx > 0 && cur.tag === 'pair') { cur = cur.cdr; idx--; }
    if (cur.tag !== 'pair') throw new EvalError('list-ref: index out of range');
    return cur.car;
  });

  defBuiltin('list-tail', (args) => {
    if (args.length !== 2) throw new EvalError('list-tail: expected 2 arguments');
    let cur = args[0];
    let idx = expectNum(args[1], 'list-tail');
    while (idx > 0) {
      if (cur.tag !== 'pair') throw new EvalError('list-tail: index out of range');
      cur = cur.cdr;
      idx--;
    }
    return cur;
  });

  defBuiltin('assoc', (args) => {
    if (args.length !== 2) throw new EvalError('assoc: expected 2 arguments');
    const key = args[0];
    let alist = args[1];
    while (alist.tag === 'pair') {
      const entry = alist.car;
      if (entry.tag === 'pair' && schemeEqual(entry.car, key)) return entry;
      alist = alist.cdr;
    }
    return { tag: 'boolean', value: false };
  });

  defBuiltin('procedure?', (args) => {
    if (args.length !== 1) throw new EvalError('procedure?: expected 1 argument');
    const v = args[0];
    return { tag: 'boolean', value: v.tag === 'lambda' || v.tag === 'builtin' || v.tag === 'case-lambda' };
  });

  // Map (supports multiple lists)
  defBuiltin('map', (args) => {
    if (args.length < 2) throw new EvalError('map: expected at least 2 arguments');
    const func = args[0];
    const lists = args.slice(1).map(a => pairToArray(a));
    const len = lists[0].length;
    const result: SchemeVal[] = [];
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map(l => l[i]);
      if (func.tag === 'lambda') {
        const callEnv = new Env(func.env);
        for (let j = 0; j < func.params.length; j++) {
          callEnv.define(func.params[j], callArgs[j]);
        }
        if (func.rest) {
          callEnv.define(func.rest, makeList(callArgs.slice(func.params.length)));
        }
        let res: SchemeVal = { tag: 'void' };
        for (const bodyExpr of func.body) res = evalExpr(bodyExpr, callEnv);
        result.push(res);
      } else if (func.tag === 'builtin') {
        result.push(func.fn(callArgs));
      } else if (func.tag === 'case-lambda') {
        result.push(applyCaseLambda(func, callArgs));
      } else {
        throw new EvalError('map: not a procedure');
      }
    }
    return makeList(result);
  });

  // Character utilities
  defBuiltin('char=?', (args) => {
    if (args.length !== 2) throw new EvalError('char=?: expected 2 arguments');
    if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char=?: expected chars');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  });

  defBuiltin('char<?', (args) => {
    if (args.length !== 2) throw new EvalError('char<?: expected 2 arguments');
    if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char<?: expected chars');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  });

  defBuiltin('char-alphabetic?', (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-alphabetic?: expected 1 char');
    return { tag: 'boolean', value: /^[a-zA-Z]$/.test(args[0].value) };
  });

  defBuiltin('char-numeric?', (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-numeric?: expected 1 char');
    return { tag: 'boolean', value: /^[0-9]$/.test(args[0].value) };
  });

  defBuiltin('char-upcase', (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-upcase: expected 1 char');
    return { tag: 'char', value: args[0].value.toUpperCase() };
  });

  defBuiltin('char-downcase', (args) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-downcase: expected 1 char');
    return { tag: 'char', value: args[0].value.toLowerCase() };
  });

  // String comparison/case utilities
  defBuiltin('string=?', (args) => {
    if (args.length !== 2) throw new EvalError('string=?: expected 2 arguments');
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string=?: expected strings');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  });

  defBuiltin('string<?', (args) => {
    if (args.length !== 2) throw new EvalError('string<?: expected 2 arguments');
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string<?: expected strings');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  });

  defBuiltin('string-ci=?', (args) => {
    if (args.length !== 2) throw new EvalError('string-ci=?: expected 2 arguments');
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string-ci=?: expected strings');
    return { tag: 'boolean', value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
  });

  defBuiltin('string-upcase', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-upcase: expected 1 string');
    return { tag: 'string', value: args[0].value.toUpperCase() };
  });

  defBuiltin('string-downcase', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-downcase: expected 1 string');
    return { tag: 'string', value: args[0].value.toLowerCase() };
  });

  // Vector operations
  defBuiltin('vector', (args) => {
    return { tag: 'vector', elements: [...args] };
  });

  defBuiltin('make-vector', (args) => {
    if (args.length < 1 || args.length > 2) throw new EvalError('make-vector: expected 1 or 2 arguments');
    const len = expectNum(args[0], 'make-vector');
    const fill: SchemeVal = args.length === 2 ? args[1] : makeRat(0, 1);
    const elements: SchemeVal[] = [];
    for (let i = 0; i < len; i++) elements.push(fill);
    return { tag: 'vector', elements };
  });

  defBuiltin('vector-ref', (args) => {
    if (args.length !== 2) throw new EvalError('vector-ref: expected 2 arguments');
    if (args[0].tag !== 'vector') throw new EvalError('vector-ref: expected vector');
    const idx = expectNum(args[1], 'vector-ref');
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError('vector-ref: index out of range');
    return args[0].elements[idx];
  });

  defBuiltin('vector-set!', (args) => {
    if (args.length !== 3) throw new EvalError('vector-set!: expected 3 arguments');
    if (args[0].tag !== 'vector') throw new EvalError('vector-set!: expected vector');
    const idx = expectNum(args[1], 'vector-set!');
    if (idx < 0 || idx >= args[0].elements.length) throw new EvalError('vector-set!: index out of range');
    args[0].elements[idx] = args[2];
    return { tag: 'void' };
  });

  defBuiltin('vector-length', (args) => {
    if (args.length !== 1) throw new EvalError('vector-length: expected 1 argument');
    if (args[0].tag !== 'vector') throw new EvalError('vector-length: expected vector');
    return makeRat(args[0].elements.length, 1);
  });

  defBuiltin('vector?', (args) => {
    if (args.length !== 1) throw new EvalError('vector?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'vector' };
  });

  defBuiltin('vector->list', (args) => {
    if (args.length !== 1) throw new EvalError('vector->list: expected 1 argument');
    if (args[0].tag !== 'vector') throw new EvalError('vector->list: expected vector');
    return makeList(args[0].elements);
  });

  defBuiltin('list->vector', (args) => {
    if (args.length !== 1) throw new EvalError('list->vector: expected 1 argument');
    return { tag: 'vector', elements: pairToArray(args[0]) };
  });

  defBuiltin('apply', (args) => {
    if (args.length < 2) throw new EvalError('apply: expected at least 2 arguments');
    const func = args[0];
    const lastArg = args[args.length - 1];
    const prefixArgs = args.slice(1, args.length - 1);
    const tailArgs = pairToArray(lastArg);
    const allArgs = [...prefixArgs, ...tailArgs];
    if (func.tag === 'lambda') {
      if (func.rest) {
        if (allArgs.length < func.params.length) {
          throw new EvalError(`lambda: expected at least ${func.params.length} arguments, got ${allArgs.length}`);
        }
      } else {
        if (allArgs.length !== func.params.length) {
          throw new EvalError(`lambda: expected ${func.params.length} arguments, got ${allArgs.length}`);
        }
      }
      const callEnv = new Env(func.env);
      for (let i = 0; i < func.params.length; i++) {
        callEnv.define(func.params[i], allArgs[i]);
      }
      if (func.rest) {
        callEnv.define(func.rest, makeList(allArgs.slice(func.params.length)));
      }
      let result: SchemeVal = { tag: 'void' };
      for (const bodyExpr of func.body) {
        result = evalExpr(bodyExpr, callEnv);
      }
      return result;
    }
    if (func.tag === 'builtin') {
      return func.fn(allArgs);
    }
    if (func.tag === 'case-lambda') {
      return applyCaseLambda(func, allArgs);
    }
    throw new EvalError(`apply: not a procedure: ${displayVal(func)}`);
  });

  return env;
}

// ── Parser ─────────────────────────────────────────────────────────

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1, col = 1;

  function advance(): string {
    const ch = input[i++];
    if (ch === '\n') { line++; col = 1; } else { col++; }
    return ch;
  }

  while (i < input.length) {
    const ch = input[i];
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') advance();
      continue;
    }
    if (/\s/.test(ch)) { advance(); continue; }
    const startPos: Pos = { line, col };
    if (ch === '(' || ch === ')') { advance(); tokens.push({ text: ch, pos: startPos }); continue; }
    if (ch === '\'') { advance(); tokens.push({ text: "'", pos: startPos }); continue; }
    if (ch === '"') {
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; advance(); }
        s += input[i]; advance();
      }
      s += '"';
      advance(); // closing quote
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    if (ch === '#') {
      if (input[i + 1] === 't' && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        advance(); advance();
        tokens.push({ text: '#t', pos: startPos }); continue;
      }
      if (input[i + 1] === 'f' && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        advance(); advance();
        tokens.push({ text: '#f', pos: startPos }); continue;
      }
    }
    let tok = '';
    while (i < input.length && !/[\s()]/.test(input[i])) {
      tok += input[i]; advance();
    }
    tokens.push({ text: tok, pos: startPos });
  }
  return tokens;
}

function parse(tokens: Token[]): SchemeVal[] {
  let idx = 0;

  function parseExpr(): SchemeVal {
    if (idx >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[idx++];
    if (tok.text === '(') {
      const elems: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elems.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError('missing closing paren');
      idx++;
      return { tag: 'list', elements: elems, pos: tok.pos };
    }
    if (tok.text === ')') throw new EvalError('unexpected )');
    if (tok.text === "'") {
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, inner], pos: tok.pos };
    }
    return parseAtom(tok);
  }

  function parseAtom(tok: Token): SchemeVal {
    if (tok.text === '#t') return { tag: 'boolean', value: true, pos: tok.pos };
    if (tok.text === '#f') return { tag: 'boolean', value: false, pos: tok.pos };
    if (tok.text.startsWith('"')) return { tag: 'string', value: tok.text.slice(1, -1), pos: tok.pos };
    if (tok.text.startsWith('#\\')) {
      const charPart = tok.text.slice(2);
      if (charPart === 'space') return { tag: 'char', value: ' ', pos: tok.pos };
      if (charPart === 'newline') return { tag: 'char', value: '\n', pos: tok.pos };
      if (charPart === 'tab') return { tag: 'char', value: '\t', pos: tok.pos };
      if (charPart.length === 1) return { tag: 'char', value: charPart, pos: tok.pos };
      throw new EvalError(`unknown character literal: ${tok.text}`);
    }
    // Rational literal: e.g. 1/3, -5/2
    if (/^-?\d+\/\d+$/.test(tok.text)) {
      const parts = tok.text.split('/');
      const r = makeRat(parseInt(parts[0], 10), parseInt(parts[1], 10));
      (r as any).pos = tok.pos;
      return r;
    }
    // Float literal
    if (/^-?\d+\.\d+$/.test(tok.text)) return { tag: 'number', value: parseFloat(tok.text), pos: tok.pos };
    // Integer literal → exact rational
    if (/^-?\d+$/.test(tok.text)) { const r = makeRat(parseInt(tok.text, 10), 1); (r as any).pos = tok.pos; return r; }
    return { tag: 'symbol', value: tok.text, pos: tok.pos };
  }

  const exprs: SchemeVal[] = [];
  while (idx < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Quote conversion ──────────────────────────────────────────────
// Convert parsed syntax (list tag) to runtime values (pair/nil)

function quoteSyntaxToValue(expr: SchemeVal): SchemeVal {
  if (expr.tag === 'list') {
    const items = expr.elements.map(quoteSyntaxToValue);
    return makeList(items);
  }
  return expr;
}

// ── Parameter parsing ─────────────────────────────────────────────

function parseParams(paramList: SchemeVal[], pos?: Pos): { params: string[]; rest?: string } {
  const params: string[] = [];
  for (let i = 0; i < paramList.length; i++) {
    const p = paramList[i];
    if (p.tag === 'symbol' && p.value === '.') {
      if (i !== paramList.length - 2) throw errAt('bad dot syntax in parameter list', pos);
      const restParam = paramList[i + 1];
      if (restParam.tag !== 'symbol') throw errAt('rest parameter must be a symbol', pos);
      return { params, rest: restParam.value };
    }
    if (p.tag !== 'symbol') throw errAt('parameter must be a symbol', pos);
    params.push(p.value);
  }
  return { params };
}

// ── Macro Expansion ────────────────────────────────────────────────

let gensymCounter = 0;
function gensym(prefix: string): string {
  return `__${prefix}_${++gensymCounter}`;
}

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'lambda', 'case-lambda', 'set!', 'begin', 'let', 'let*', 'letrec', 'letrec*',
  'cond', 'and', 'or', 'not', 'define-syntax', 'syntax-rules', 'case', 'do',
]);

type MatchBinding =
  | { ellipsis: false; value: SchemeVal }
  | { ellipsis: true; values: SchemeVal[] };

function matchPattern(
  pattern: SchemeVal[], input: SchemeVal[], literals: Set<string>, bindings: Map<string, MatchBinding>
): boolean {
  let pi = 0, ii = 0;
  while (pi < pattern.length) {
    if (pi + 1 < pattern.length &&
        pattern[pi + 1].tag === 'symbol' && pattern[pi + 1].value === '...') {
      const subPat = pattern[pi];
      const remaining = pattern.length - pi - 2;
      const matchCount = input.length - ii - remaining;
      if (matchCount < 0) return false;
      if (subPat.tag === 'symbol' && !literals.has(subPat.value)) {
        const values: SchemeVal[] = [];
        for (let k = 0; k < matchCount; k++) values.push(input[ii + k]);
        bindings.set(subPat.value, { ellipsis: true, values });
      }
      ii += matchCount;
      pi += 2;
      continue;
    }
    if (ii >= input.length) return false;
    const pat = pattern[pi], inp = input[ii];
    if (pat.tag === 'symbol') {
      if (literals.has(pat.value)) {
        if (inp.tag !== 'symbol' || inp.value !== pat.value) return false;
      } else if (pat.value !== '_') {
        bindings.set(pat.value, { ellipsis: false, value: inp });
      }
    } else if (pat.tag === 'list') {
      if (inp.tag !== 'list') return false;
      if (!matchPattern(pat.elements, inp.elements, literals, bindings)) return false;
    }
    pi++; ii++;
  }
  return ii === input.length;
}

function collectIntroduced(template: SchemeVal, patVars: Set<string>, result: Set<string>): void {
  if (template.tag === 'symbol') {
    if (!patVars.has(template.value) && !SPECIAL_FORMS.has(template.value) && template.value !== '...') {
      result.add(template.value);
    }
  } else if (template.tag === 'list') {
    for (const elem of template.elements) collectIntroduced(elem, patVars, result);
  }
}

function collectEllipsisVars(template: SchemeVal, bindings: Map<string, MatchBinding>): string[] {
  const vars: string[] = [];
  if (template.tag === 'symbol') {
    const b = bindings.get(template.value);
    if (b && b.ellipsis) vars.push(template.value);
  } else if (template.tag === 'list') {
    for (const elem of template.elements) vars.push(...collectEllipsisVars(elem, bindings));
  }
  return vars;
}

function expandTemplate(
  template: SchemeVal, bindings: Map<string, MatchBinding>, renames: Map<string, string>
): SchemeVal {
  if (template.tag === 'symbol') {
    const b = bindings.get(template.value);
    if (b) {
      if (!b.ellipsis) return b.value;
      throw new EvalError('syntax: ellipsis variable outside ellipsis context');
    }
    const r = renames.get(template.value);
    if (r) return { tag: 'symbol', value: r };
    return template;
  }
  if (template.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < template.elements.length; i++) {
      if (i + 1 < template.elements.length &&
          template.elements[i + 1].tag === 'symbol' &&
          template.elements[i + 1].value === '...') {
        const sub = template.elements[i];
        const evars = collectEllipsisVars(sub, bindings);
        if (evars.length > 0) {
          const count = (bindings.get(evars[0])! as { ellipsis: true; values: SchemeVal[] }).values.length;
          for (let k = 0; k < count; k++) {
            const iter = new Map(bindings);
            for (const v of evars) {
              const vb = bindings.get(v)! as { ellipsis: true; values: SchemeVal[] };
              iter.set(v, { ellipsis: false, value: vb.values[k] });
            }
            result.push(expandTemplate(sub, iter, renames));
          }
        }
        i++; // skip ellipsis
        continue;
      }
      result.push(expandTemplate(template.elements[i], bindings, renames));
    }
    return { tag: 'list', elements: result };
  }
  return template;
}

function expandMacro(
  macro: { literals: string[]; clauses: MacroClause[]; defEnv: Env },
  form: SchemeVal[],
  useEnv: Env
): SchemeVal {
  const literalSet = new Set(macro.literals);
  for (const clause of macro.clauses) {
    const bindings = new Map<string, MatchBinding>();
    if (matchPattern(clause.pattern.slice(1), form.slice(1), literalSet, bindings)) {
      const patVars = new Set(bindings.keys());
      const introduced = new Set<string>();
      collectIntroduced(clause.template, patVars, introduced);
      const renames = new Map<string, string>();
      for (const sym of introduced) renames.set(sym, gensym(sym));
      const expanded = expandTemplate(clause.template, bindings, renames);
      for (const [original, renamed] of renames) {
        try { useEnv.define(renamed, macro.defEnv.get(original)); }
        catch { /* fresh introduced variable */ }
      }
      return expanded;
    }
  }
  throw new EvalError('syntax-rules: no matching pattern');
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function evalExpr(initExpr: SchemeVal, initEnv: Env): SchemeVal {
  let expr = initExpr;
  let env = initEnv;

  trampoline: for (;;) {
  if (expr.tag === 'number' || expr.tag === 'rational' || expr.tag === 'boolean' || expr.tag === 'string' || expr.tag === 'char') {
    return expr;
  }

  if (expr.tag === 'symbol') {
    return env.get(expr.value, expr.pos);
  }

  if (expr.tag === 'list') {
    const elems = expr.elements;
    const epos = expr.pos;
    if (elems.length === 0) throw errAt('empty application', epos);

    // Special forms
    if (elems[0].tag === 'symbol') {
      const op = elems[0].value;

      if (op === 'quote') {
        if (elems.length !== 2) throw errAt('quote: expected 1 argument', epos);
        return quoteSyntaxToValue(elems[1]);
      }

      if (op === 'if') {
        if (elems.length < 3 || elems.length > 4) throw errAt('if: expected 2 or 3 arguments', epos);
        const cond = evalExpr(elems[1], env);
        if (isTruthy(cond)) {
          expr = elems[2]; continue trampoline; // TCO
        } else {
          if (elems.length === 4) { expr = elems[3]; continue; } // TCO
          return { tag: 'void' };
        }
      }

      if (op === 'define') {
        if (elems.length < 3) throw errAt('define: bad syntax', epos);
        const target = elems[1];
        if (target.tag === 'symbol') {
          const val = evalExpr(elems[2], env);
          env.define(target.value, val);
          return { tag: 'void' };
        }
        if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
          const name = target.elements[0].value;
          const { params, rest } = parseParams(target.elements.slice(1), epos);
          const body = elems.slice(2);
          const lambda: SchemeVal = { tag: 'lambda', params, rest, body, env };
          env.define(name, lambda);
          return { tag: 'void' };
        }
        throw errAt('define: bad syntax', epos);
      }

      if (op === 'lambda') {
        if (elems.length < 3) throw errAt('lambda: bad syntax', epos);
        const paramList = elems[1];
        // (lambda args body) — single symbol catches all args
        if (paramList.tag === 'symbol') {
          const body = elems.slice(2);
          return { tag: 'lambda', params: [], rest: paramList.value, body, env };
        }
        if (paramList.tag !== 'list') throw errAt('lambda: parameters must be a list', epos);
        const { params, rest } = parseParams(paramList.elements, epos);
        const body = elems.slice(2);
        return { tag: 'lambda', params, rest, body, env };
      }

      if (op === 'case-lambda') {
        if (elems.length < 2) throw errAt('case-lambda: bad syntax', epos);
        const clauses: { params: string[]; rest?: string; body: SchemeVal[] }[] = [];
        for (let i = 1; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.elements.length < 2) throw errAt('case-lambda: bad clause', epos);
          const paramList = clause.elements[0];
          if (paramList.tag !== 'list') throw errAt('case-lambda: parameters must be a list', epos);
          const { params, rest } = parseParams(paramList.elements, epos);
          const body = clause.elements.slice(1);
          clauses.push({ params, rest, body });
        }
        return { tag: 'case-lambda', clauses, env };
      }

      if (op === 'set!') {
        if (elems.length !== 3) throw errAt('set!: bad syntax', epos);
        if (elems[1].tag !== 'symbol') throw errAt('set!: expected symbol', epos);
        const val = evalExpr(elems[2], env);
        env.set(elems[1].value, val, epos);
        return { tag: 'void' };
      }

      if (op === 'begin') {
        if (elems.length === 1) return { tag: 'void' };
        for (let i = 1; i < elems.length - 1; i++) {
          evalExpr(elems[i], env);
        }
        expr = elems[elems.length - 1]; continue trampoline; // TCO
      }

      if (op === 'let') {
        // Named let: (let name ((var init) ...) body ...)
        if (elems.length >= 3 && elems[1].tag === 'symbol') {
          const name = elems[1].value;
          const bindingsList = elems[2];
          if (bindingsList.tag !== 'list') throw errAt('let: bad syntax', epos);
          const paramNames: string[] = [];
          const initVals: SchemeVal[] = [];
          for (const b of bindingsList.elements) {
            if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
              throw errAt('let: bad binding', epos);
            paramNames.push(b.elements[0].value);
            initVals.push(evalExpr(b.elements[1], env));
          }
          const body = elems.slice(3);
          const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
          // Create env where name is bound to the lambda (for recursion)
          const letEnv = new Env(env);
          letEnv.define(name, lambda);
          // Update lambda's env to include itself
          (lambda as any).env = letEnv;
          // Call with initial values
          const callEnv = new Env(letEnv);
          for (let i = 0; i < paramNames.length; i++) {
            callEnv.define(paramNames[i], initVals[i]);
          }
          for (let i = 0; i < body.length - 1; i++) {
            evalExpr(body[i], callEnv);
          }
          expr = body[body.length - 1]; env = callEnv; continue trampoline; // TCO
        }
        // Regular let: (let ((var init) ...) body ...)
        if (elems.length < 3) throw errAt('let: bad syntax', epos);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw errAt('let: bad syntax', epos);
        const letEnv2 = new Env(env);
        for (const b of bindings.elements) {
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw new EvalError('let: bad binding');
          const val = evalExpr(b.elements[1], env); // eval in outer env
          letEnv2.define(b.elements[0].value, val);
        }
        for (let i = 2; i < elems.length - 1; i++) {
          evalExpr(elems[i], letEnv2);
        }
        expr = elems[elems.length - 1]; env = letEnv2; continue trampoline; // TCO
      }

      if (op === 'letrec') {
        if (elems.length < 3) throw errAt('letrec: bad syntax', epos);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw errAt('letrec: bad syntax', epos);
        const letrecEnv = new Env(env);
        // First, define all variables as undefined
        const names: string[] = [];
        for (const b of bindings.elements) {
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw errAt('letrec: bad binding', epos);
          names.push(b.elements[0].value);
          letrecEnv.define(b.elements[0].value, { tag: 'void' });
        }
        // Then evaluate all inits in the letrec env
        for (let i = 0; i < bindings.elements.length; i++) {
          const val = evalExpr(bindings.elements[i].elements[1], letrecEnv);
          letrecEnv.define(names[i], val);
        }
        for (let i = 2; i < elems.length - 1; i++) {
          evalExpr(elems[i], letrecEnv);
        }
        expr = elems[elems.length - 1]; env = letrecEnv; continue trampoline; // TCO
      }

      if (op === 'letrec*') {
        if (elems.length < 3) throw errAt('letrec*: bad syntax', epos);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw errAt('letrec*: bad syntax', epos);
        const letrecEnv = new Env(env);
        for (const b of bindings.elements) {
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw errAt('letrec*: bad binding', epos);
          const val = evalExpr(b.elements[1], letrecEnv);
          letrecEnv.define(b.elements[0].value, val);
        }
        for (let i = 2; i < elems.length - 1; i++) {
          evalExpr(elems[i], letrecEnv);
        }
        expr = elems[elems.length - 1]; env = letrecEnv; continue trampoline; // TCO
      }

      if (op === 'case') {
        if (elems.length < 2) throw errAt('case: bad syntax', epos);
        const key = evalExpr(elems[1], env);
        for (let i = 2; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.elements.length < 2) throw errAt('case: bad clause', epos);
          // else clause
          if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
            for (let j = 1; j < clause.elements.length - 1; j++) {
              evalExpr(clause.elements[j], env);
            }
            expr = clause.elements[clause.elements.length - 1]; continue trampoline; // TCO
          }
          // ((datum ...) expr ...)
          if (clause.elements[0].tag !== 'list') throw errAt('case: expected datum list', epos);
          const datums = clause.elements[0].elements;
          for (const datum of datums) {
            const dval = quoteSyntaxToValue(datum);
            if (schemeEqv(key, dval)) {
              for (let j = 1; j < clause.elements.length - 1; j++) {
                evalExpr(clause.elements[j], env);
              }
              expr = clause.elements[clause.elements.length - 1]; continue trampoline; // TCO
            }
          }
        }
        return { tag: 'void' };
      }

      if (op === 'do') {
        // (do ((var init step) ...) (test expr ...) body ...)
        if (elems.length < 3) throw errAt('do: bad syntax', epos);
        const varSpecs = elems[1];
        if (varSpecs.tag !== 'list') throw errAt('do: bad syntax', epos);
        const testClause = elems[2];
        if (testClause.tag !== 'list' || testClause.elements.length < 1) throw errAt('do: bad test clause', epos);
        const bodyExprs = elems.slice(3);

        // Parse variable specs
        const vars: { name: string; stepExpr?: SchemeVal }[] = [];
        const doEnv = new Env(env);
        for (const spec of varSpecs.elements) {
          if (spec.tag !== 'list' || spec.elements.length < 2 || spec.elements[0].tag !== 'symbol')
            throw errAt('do: bad variable spec', epos);
          const name = spec.elements[0].value;
          const init = evalExpr(spec.elements[1], env);
          doEnv.define(name, init);
          vars.push({ name, stepExpr: spec.elements.length >= 3 ? spec.elements[2] : undefined });
        }

        // Iteration loop
        while (true) {
          // Test
          const testResult = evalExpr(testClause.elements[0], doEnv);
          if (isTruthy(testResult)) {
            // Evaluate result expressions
            if (testClause.elements.length === 1) return { tag: 'void' };
            for (let j = 1; j < testClause.elements.length - 1; j++) {
              evalExpr(testClause.elements[j], doEnv);
            }
            expr = testClause.elements[testClause.elements.length - 1]; env = doEnv; continue trampoline; // TCO
          }
          // Execute body
          for (const bodyExpr of bodyExprs) {
            evalExpr(bodyExpr, doEnv);
          }
          // Step: evaluate all step expressions with current values, then update (parallel)
          const newVals: (SchemeVal | undefined)[] = [];
          for (const v of vars) {
            newVals.push(v.stepExpr ? evalExpr(v.stepExpr, doEnv) : undefined);
          }
          for (let i = 0; i < vars.length; i++) {
            if (newVals[i] !== undefined) {
              doEnv.define(vars[i].name, newVals[i]!);
            }
          }
        }
      }

      if (op === 'cond') {
        for (let i = 1; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.elements.length < 1) throw errAt('cond: bad clause', epos);
          // else clause
          if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
            for (let j = 1; j < clause.elements.length - 1; j++) {
              evalExpr(clause.elements[j], env);
            }
            expr = clause.elements[clause.elements.length - 1]; continue trampoline; // TCO
          }
          const test = evalExpr(clause.elements[0], env);
          if (isTruthy(test)) {
            if (clause.elements.length === 1) return test;
            for (let j = 1; j < clause.elements.length - 1; j++) {
              evalExpr(clause.elements[j], env);
            }
            expr = clause.elements[clause.elements.length - 1]; continue trampoline; // TCO
          }
        }
        return { tag: 'void' };
      }

      if (op === 'and') {
        if (elems.length === 1) return { tag: 'boolean', value: true };
        for (let i = 1; i < elems.length - 1; i++) {
          const result = evalExpr(elems[i], env);
          if (!isTruthy(result)) return result;
        }
        expr = elems[elems.length - 1]; continue trampoline; // TCO
      }

      if (op === 'or') {
        if (elems.length === 1) return { tag: 'boolean', value: false };
        for (let i = 1; i < elems.length - 1; i++) {
          const result = evalExpr(elems[i], env);
          if (isTruthy(result)) return result;
        }
        expr = elems[elems.length - 1]; continue trampoline; // TCO
      }

      if (op === 'not') {
        if (elems.length !== 2) throw errAt('not: expected 1 argument', epos);
        const val = evalExpr(elems[1], env);
        return { tag: 'boolean', value: !isTruthy(val) };
      }

      if (op === 'define-syntax') {
        if (elems.length !== 3) throw errAt('define-syntax: bad syntax', epos);
        if (elems[1].tag !== 'symbol') throw errAt('define-syntax: expected symbol', epos);
        const name = elems[1].value;
        const transformer = elems[2];
        if (transformer.tag !== 'list' || transformer.elements.length < 2 ||
            transformer.elements[0].tag !== 'symbol' || transformer.elements[0].value !== 'syntax-rules') {
          throw errAt('define-syntax: expected syntax-rules', epos);
        }
        const srElems = transformer.elements;
        if (srElems[1].tag !== 'list') throw errAt('syntax-rules: expected literals list', epos);
        const literals = srElems[1].elements.map(e => {
          if (e.tag !== 'symbol') throw errAt('syntax-rules: literal must be symbol', epos);
          return e.value;
        });
        const clauses: MacroClause[] = [];
        for (let ci = 2; ci < srElems.length; ci++) {
          const c = srElems[ci];
          if (c.tag !== 'list' || c.elements.length !== 2)
            throw errAt('syntax-rules: bad clause', epos);
          if (c.elements[0].tag !== 'list')
            throw errAt('syntax-rules: pattern must be list', epos);
          clauses.push({ pattern: c.elements[0].elements, template: c.elements[1] });
        }
        env.define(name, { tag: 'macro', literals, clauses, defEnv: env });
        return { tag: 'void' };
      }

      if (op === 'define-record-type') {
        // (define-record-type <name> (<constructor> <field> ...) <predicate> (<field> <accessor>) ...)
        if (elems.length < 4) throw errAt('define-record-type: bad syntax', epos);
        if (elems[1].tag !== 'symbol') throw errAt('define-record-type: expected type name', epos);
        const typeName = elems[1].value;
        const ctorSpec = elems[2];
        if (ctorSpec.tag !== 'list' || ctorSpec.elements.length < 1 || ctorSpec.elements[0].tag !== 'symbol')
          throw errAt('define-record-type: bad constructor spec', epos);
        const ctorName = ctorSpec.elements[0].value;
        const ctorFields: string[] = [];
        for (let i = 1; i < ctorSpec.elements.length; i++) {
          if (ctorSpec.elements[i].tag !== 'symbol') throw errAt('define-record-type: field must be symbol', epos);
          ctorFields.push(ctorSpec.elements[i].value);
        }
        if (elems[3].tag !== 'symbol') throw errAt('define-record-type: expected predicate name', epos);
        const predName = elems[3].value;
        // Parse field accessors
        const accessors: { field: string; accessor: string }[] = [];
        for (let i = 4; i < elems.length; i++) {
          const spec = elems[i];
          if (spec.tag !== 'list' || spec.elements.length < 2 || spec.elements[0].tag !== 'symbol' || spec.elements[1].tag !== 'symbol')
            throw errAt('define-record-type: bad field spec', epos);
          accessors.push({ field: spec.elements[0].value, accessor: spec.elements[1].value });
        }
        // Define constructor
        env.define(ctorName, { tag: 'builtin', name: ctorName, fn: (args) => {
          if (args.length !== ctorFields.length)
            throw new EvalError(`${ctorName}: expected ${ctorFields.length} arguments, got ${args.length}`);
          const fields = new Map<string, SchemeVal>();
          for (let i = 0; i < ctorFields.length; i++) fields.set(ctorFields[i], args[i]);
          return { tag: 'record', typeName, fields };
        }});
        // Define predicate
        env.define(predName, { tag: 'builtin', name: predName, fn: (args) => {
          if (args.length !== 1) throw new EvalError(`${predName}: expected 1 argument`);
          return { tag: 'boolean', value: args[0].tag === 'record' && args[0].typeName === typeName };
        }});
        // Define accessors
        for (const { field, accessor } of accessors) {
          env.define(accessor, { tag: 'builtin', name: accessor, fn: (args) => {
            if (args.length !== 1) throw new EvalError(`${accessor}: expected 1 argument`);
            if (args[0].tag !== 'record' || args[0].typeName !== typeName)
              throw new EvalError(`${accessor}: expected ${typeName}`);
            return args[0].fields.get(field)!;
          }});
        }
        return { tag: 'void' };
      }

      // Check for macro application
      try {
        const macroVal = env.get(op);
        if (macroVal.tag === 'macro') {
          const expanded = expandMacro(macroVal, elems, env);
          expr = expanded; continue trampoline; // TCO
        }
      } catch { /* not bound — fall through */ }
    }

    // Function application
    const func = evalExpr(elems[0], env);
    const args = elems.slice(1).map(e => evalExpr(e, env));

    if (func.tag === 'lambda') {
      if (func.rest) {
        if (args.length < func.params.length) {
          throw errAt(`lambda: expected at least ${func.params.length} arguments, got ${args.length}`, epos);
        }
      } else {
        if (args.length !== func.params.length) {
          throw errAt(`lambda: expected ${func.params.length} arguments, got ${args.length}`, epos);
        }
      }
      const callEnv = new Env(func.env);
      for (let i = 0; i < func.params.length; i++) {
        callEnv.define(func.params[i], args[i]);
      }
      if (func.rest) {
        callEnv.define(func.rest, makeList(args.slice(func.params.length)));
      }
      // TCO: eval all but last body expr, then loop for last
      for (let i = 0; i < func.body.length - 1; i++) {
        evalExpr(func.body[i], callEnv);
      }
      expr = func.body[func.body.length - 1]; env = callEnv; continue trampoline; // TCO
    }

    if (func.tag === 'builtin') {
      try {
        return func.fn(args);
      } catch (e) {
        if (e instanceof EvalError && !/^\d+:/.test(e.message)) {
          throw errAt(e.message, epos);
        }
        throw e;
      }
    }

    if (func.tag === 'case-lambda') {
      // Inline TCO for case-lambda
      let matched = false;
      for (const clause of func.clauses) {
        if (clause.rest ? args.length >= clause.params.length : args.length === clause.params.length) {
          const callEnv = new Env(func.env);
          for (let i = 0; i < clause.params.length; i++) {
            callEnv.define(clause.params[i], args[i]);
          }
          if (clause.rest) {
            callEnv.define(clause.rest, makeList(args.slice(clause.params.length)));
          }
          for (let i = 0; i < clause.body.length - 1; i++) {
            evalExpr(clause.body[i], callEnv);
          }
          expr = clause.body[clause.body.length - 1]; env = callEnv; matched = true; break;
        }
      }
      if (matched) continue trampoline; // TCO
      throw new EvalError(`case-lambda: no matching clause for ${args.length} arguments`);
    }

    throw errAt(`not a procedure: ${displayVal(func)}`, epos);
  }

  throw errAt('cannot evaluate', expr.pos);
  } // end for(;;)
}

function applyCaseLambda(func: Extract<SchemeVal, { tag: 'case-lambda' }>, args: SchemeVal[]): SchemeVal {
  for (const clause of func.clauses) {
    if (clause.rest) {
      if (args.length >= clause.params.length) {
        const callEnv = new Env(func.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.define(clause.params[i], args[i]);
        }
        callEnv.define(clause.rest, makeList(args.slice(clause.params.length)));
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of clause.body) result = evalExpr(bodyExpr, callEnv);
        return result;
      }
    } else {
      if (args.length === clause.params.length) {
        const callEnv = new Env(func.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.define(clause.params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of clause.body) result = evalExpr(bodyExpr, callEnv);
        return result;
      }
    }
  }
  throw new EvalError(`case-lambda: no matching clause for ${args.length} arguments`);
}

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': {
      const s = String(val.value);
      // Ensure inexact floats display with decimal point
      if (Number.isInteger(val.value) && !s.includes('.')) return s + '.0';
      return s;
    }
    case 'rational': return val.den === 1 ? String(val.num) : `${val.num}/${val.den}`;
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'char': return `#\\${val.value}`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
    case 'nil': return '()';
    case 'pair': {
      let parts: string[] = [];
      let cur: SchemeVal = val;
      while (cur.tag === 'pair') {
        parts.push(displayVal(cur.car));
        cur = cur.cdr;
      }
      if (cur.tag === 'nil') {
        return `(${parts.join(' ')})`;
      }
      return `(${parts.join(' ')} . ${displayVal(cur)})`;
    }
    case 'void': return '';
    case 'lambda': return '#<procedure>';
    case 'case-lambda': return '#<procedure>';
    case 'builtin': return `#<builtin:${val.name}>`;
    case 'macro': return '#<macro>';
    case 'record': return `#<record:${val.typeName}>`;
    case 'vector': return `#(${val.elements.map(displayVal).join(' ')})`;
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  outputBuffer = '';
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  outputBuffer = '';
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return { result: displayVal(result!), output: outputBuffer };
}
