import { EvalError } from './evalError.js';

// ── Source Position ──────────────────────────────────────────────────

interface Pos {
  line: number;
  col: number;
}

function fmtPos(pos: Pos): string {
  return `${pos.line}:${pos.col}`;
}

// ── AST ──────────────────────────────────────────────────────────────

type Expr =
  | { tag: 'number'; value: number; pos: Pos }
  | { tag: 'boolean'; value: boolean; pos: Pos }
  | { tag: 'string'; value: string; pos: Pos }
  | { tag: 'char'; value: string; pos: Pos }
  | { tag: 'symbol'; name: string; pos: Pos }
  | { tag: 'list'; items: Expr[]; pos: Pos };

// ── Parser ───────────────────────────────────────────────────────────

interface Token {
  text: string;
  pos: Pos;
}

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
    if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
      advance();
      continue;
    }
    // comment
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') advance();
      continue;
    }
    const startPos: Pos = { line, col };
    // parens
    if (ch === '(' || ch === ')') {
      tokens.push({ text: ch, pos: startPos });
      advance();
      continue;
    }
    // quote shorthand
    if (ch === "'") {
      tokens.push({ text: "'", pos: startPos });
      advance();
      continue;
    }
    // string literal
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
        advance(); // closing quote
      }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    // atom
    let atom = '';
    while (
      i < input.length &&
      input[i] !== ' ' &&
      input[i] !== '\t' &&
      input[i] !== '\n' &&
      input[i] !== '\r' &&
      input[i] !== '(' &&
      input[i] !== ')' &&
      input[i] !== ';' &&
      input[i] !== "'"
    ) {
      atom += input[i];
      advance();
    }
    tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parseTokens(tokens: Token[], idx: number): [Expr, number] {
  if (idx >= tokens.length) {
    throw new EvalError('unexpected end of input');
  }
  const tok = tokens[idx];
  const p = tok.pos;

  if (tok.text === "'") {
    const [inner, next] = parseTokens(tokens, idx + 1);
    return [{ tag: 'list', items: [{ tag: 'symbol', name: 'quote', pos: p }, inner], pos: p }, next];
  }

  if (tok.text === '(') {
    const items: Expr[] = [];
    idx++;
    while (idx < tokens.length && tokens[idx].text !== ')') {
      const [expr, next] = parseTokens(tokens, idx);
      items.push(expr);
      idx = next;
    }
    if (idx >= tokens.length) {
      throw new EvalError(`${fmtPos(p)}: missing closing parenthesis`);
    }
    idx++; // skip ')'
    return [{ tag: 'list', items, pos: p }, idx];
  }

  if (tok.text === ')') {
    throw new EvalError(`${fmtPos(p)}: unexpected )`);
  }

  // boolean
  if (tok.text === '#t') return [{ tag: 'boolean', value: true, pos: p }, idx + 1];
  if (tok.text === '#f') return [{ tag: 'boolean', value: false, pos: p }, idx + 1];

  // character literal
  if (tok.text.startsWith('#\\')) {
    const charName = tok.text.slice(2);
    let ch: string;
    if (charName === 'space') ch = ' ';
    else if (charName === 'newline') ch = '\n';
    else if (charName === 'tab') ch = '\t';
    else ch = charName;
    return [{ tag: 'char', value: ch, pos: p }, idx + 1];
  }

  // number
  if (/^-?\d+$/.test(tok.text)) {
    return [{ tag: 'number', value: parseInt(tok.text, 10), pos: p }, idx + 1];
  }

  // string
  if (tok.text.startsWith('"') && tok.text.endsWith('"')) {
    const raw = tok.text.slice(1, -1);
    const value = raw.replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return [{ tag: 'string', value, pos: p }, idx + 1];
  }

  // symbol
  return [{ tag: 'symbol', name: tok.text, pos: p }, idx + 1];
}

function parse(input: string): Expr[] {
  const tokens = tokenize(input);
  const exprs: Expr[] = [];
  let pos = 0;
  while (pos < tokens.length) {
    const [expr, next] = parseTokens(tokens, pos);
    exprs.push(expr);
    pos = next;
  }
  return exprs;
}

// ── Environment ─────────────────────────────────────────────────────

class Env {
  bindings: Map<string, Value> = new Map();
  constructor(public parent: Env | null = null) {}

  get(name: string, pos?: Pos): Value {
    const v = this.bindings.get(name);
    if (v !== undefined) return v;
    if (this.parent) return this.parent.get(name, pos);
    const prefix = pos ? `${fmtPos(pos)}: ` : '';
    throw new EvalError(`${prefix}unbound variable: ${name}`);
  }

  define(name: string, value: Value): void {
    this.bindings.set(name, value);
  }

  set(name: string, value: Value, pos?: Pos): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, value);
      return;
    }
    if (this.parent) return this.parent.set(name, value, pos);
    const prefix = pos ? `${fmtPos(pos)}: ` : '';
    throw new EvalError(`${prefix}unbound variable: ${name}`);
  }
}

// ── Values ───────────────────────────────────────────────────────────

type Value =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'char'; value: string }
  | { tag: 'symbol'; name: string }
  | { tag: 'nil' }
  | { tag: 'pair'; car: Value; cdr: Value }
  | { tag: 'vector'; items: Value[] }
  | { tag: 'builtin'; name: string; fn: (args: Value[]) => Value }
  | { tag: 'lambda'; params: string[]; rest: string | null; body: Expr[]; env: Env }
  | { tag: 'continuation'; id: number; exprPos: string; topIdx: number }
  | { tag: 'macro'; literals: string[]; rules: { pattern: Expr; template: Expr }[]; defEnv: Env };

function isTruthy(v: Value): boolean {
  return !(v.tag === 'boolean' && v.value === false);
}

function displayValue(v: Value): string {
  switch (v.tag) {
    case 'number': return String(v.value);
    case 'boolean': return v.value ? '#t' : '#f';
    case 'string': return `"${v.value}"`;
    case 'char': return `#\\${v.value}`;
    case 'symbol': return v.name;
    case 'nil': return '()';
    case 'pair': return displayPair(v);
    case 'builtin': return `#<procedure:${v.name}>`;
    case 'lambda': return '#<procedure>';
    case 'continuation': return '#<continuation>';
    case 'macro': return '#<macro>';
    case 'vector': return `#(${v.items.map(displayValue).join(' ')})`;
  }
}

function displayValueRaw(v: Value): string {
  switch (v.tag) {
    case 'string': return v.value;
    case 'char': return String(v.value);
    case 'pair': {
      let parts: string[] = [];
      let cur: Value = v;
      while (cur.tag === 'pair') {
        parts.push(displayValueRaw(cur.car));
        cur = cur.cdr;
      }
      if (cur.tag === 'nil') return `(${parts.join(' ')})`;
      return `(${parts.join(' ')} . ${displayValueRaw(cur)})`;
    }
    default: return displayValue(v);
  }
}

function displayPair(p: { tag: 'pair'; car: Value; cdr: Value }): string {
  let parts: string[] = [];
  let cur: Value = p;
  while (cur.tag === 'pair') {
    parts.push(displayValue(cur.car));
    cur = cur.cdr;
  }
  if (cur.tag === 'nil') {
    return `(${parts.join(' ')})`;
  }
  return `(${parts.join(' ')} . ${displayValue(cur)})`;
}

// ── Continuations ───────────────────────────────────────────────────

let nextContId = 0;
let currentTopIdx = 0;
let pendingContReturn: { exprPos: string; value: Value } | null = null;

class ContinuationJump {
  constructor(
    public id: number,
    public value: Value,
    public exprPos: string,
    public topIdx: number,
  ) {}
}

// ── eqv? comparison (used by case) ───────────────────────────────────

function eqvCompare(a: Value, b: Value): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'number': return a.value === (b as typeof a).value;
    case 'boolean': return a.value === (b as typeof a).value;
    case 'symbol': return a.name === (b as typeof a).name;
    case 'char': return a.value === (b as typeof a).value;
    case 'string': return a.value === (b as typeof a).value;
    case 'nil': return true;
    default: return a === b;
  }
}

// ── Builtins ─────────────────────────────────────────────────────────

function requireNumbers(args: Value[], name: string): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number`);
    return a.value;
  });
}

function makeGlobalEnv(output: string[] = []): Env {
  const env = new Env();

  const numBuiltins: Record<string, (args: Value[]) => Value> = {
    '+'(args) {
      const nums = requireNumbers(args, '+');
      return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
    },
    '-'(args) {
      if (args.length === 0) throw new EvalError('-: need at least 1 argument');
      const nums = requireNumbers(args, '-');
      if (nums.length === 1) return { tag: 'number', value: -nums[0] };
      return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
    },
    '*'(args) {
      const nums = requireNumbers(args, '*');
      return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
    },
    '/'(args) {
      if (args.length < 2) throw new EvalError('/: need at least 2 arguments');
      const nums = requireNumbers(args, '/');
      return { tag: 'number', value: nums.slice(1).reduce((a, b) => {
        if (b === 0) throw new EvalError('division by zero');
        return Math.trunc(a / b);
      }, nums[0]) };
    },
    '<'(args) {
      const nums = requireNumbers(args, '<');
      for (let i = 0; i < nums.length - 1; i++) {
        if (!(nums[i] < nums[i + 1])) return { tag: 'boolean', value: false };
      }
      return { tag: 'boolean', value: true };
    },
    '>'(args) {
      const nums = requireNumbers(args, '>');
      for (let i = 0; i < nums.length - 1; i++) {
        if (!(nums[i] > nums[i + 1])) return { tag: 'boolean', value: false };
      }
      return { tag: 'boolean', value: true };
    },
    '='(args) {
      const nums = requireNumbers(args, '=');
      for (let i = 0; i < nums.length - 1; i++) {
        if (nums[i] !== nums[i + 1]) return { tag: 'boolean', value: false };
      }
      return { tag: 'boolean', value: true };
    },
    '<='(args) {
      const nums = requireNumbers(args, '<=');
      for (let i = 0; i < nums.length - 1; i++) {
        if (!(nums[i] <= nums[i + 1])) return { tag: 'boolean', value: false };
      }
      return { tag: 'boolean', value: true };
    },
    '>='(args) {
      const nums = requireNumbers(args, '>=');
      for (let i = 0; i < nums.length - 1; i++) {
        if (!(nums[i] >= nums[i + 1])) return { tag: 'boolean', value: false };
      }
      return { tag: 'boolean', value: true };
    },
    'not'(args) {
      if (args.length !== 1) throw new EvalError('not: expected 1 argument');
      return { tag: 'boolean', value: !isTruthy(args[0]) };
    },
  };

  for (const [name, fn] of Object.entries(numBuiltins)) {
    env.define(name, { tag: 'builtin', name, fn });
  }

  // List builtins
  env.define('cons', { tag: 'builtin', name: 'cons', fn(args) {
    if (args.length !== 2) throw new EvalError('cons: expected 2 arguments');
    return { tag: 'pair', car: args[0], cdr: args[1] };
  }});
  env.define('car', { tag: 'builtin', name: 'car', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'pair') throw new EvalError('car: expected pair');
    return args[0].car;
  }});
  env.define('cdr', { tag: 'builtin', name: 'cdr', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'pair') throw new EvalError('cdr: expected pair');
    return args[0].cdr;
  }});
  env.define('null?', { tag: 'builtin', name: 'null?', fn(args) {
    if (args.length !== 1) throw new EvalError('null?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'nil' };
  }});
  env.define('list', { tag: 'builtin', name: 'list', fn(args) {
    let result: Value = { tag: 'nil' };
    for (let i = args.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: args[i], cdr: result };
    }
    return result;
  }});
  env.define('length', { tag: 'builtin', name: 'length', fn(args) {
    if (args.length !== 1) throw new EvalError('length: expected 1 argument');
    let count = 0;
    let cur = args[0];
    while (cur.tag === 'pair') { count++; cur = cur.cdr; }
    if (cur.tag !== 'nil') throw new EvalError('length: expected proper list');
    return { tag: 'number', value: count };
  }});

  // Type predicates
  env.define('string?', { tag: 'builtin', name: 'string?', fn(args) {
    if (args.length !== 1) throw new EvalError('string?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'string' };
  }});
  env.define('number?', { tag: 'builtin', name: 'number?', fn(args) {
    if (args.length !== 1) throw new EvalError('number?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'number' };
  }});
  env.define('boolean?', { tag: 'builtin', name: 'boolean?', fn(args) {
    if (args.length !== 1) throw new EvalError('boolean?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  }});
  env.define('pair?', { tag: 'builtin', name: 'pair?', fn(args) {
    if (args.length !== 1) throw new EvalError('pair?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'pair' };
  }});
  env.define('symbol?', { tag: 'builtin', name: 'symbol?', fn(args) {
    if (args.length !== 1) throw new EvalError('symbol?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  }});
  env.define('char?', { tag: 'builtin', name: 'char?', fn(args) {
    if (args.length !== 1) throw new EvalError('char?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'char' };
  }});

  // Output builtins
  env.define('display', { tag: 'builtin', name: 'display', fn(args) {
    if (args.length !== 1) throw new EvalError('display: expected 1 argument');
    output.push(displayValueRaw(args[0]));
    return { tag: 'nil' };
  }});
  env.define('write', { tag: 'builtin', name: 'write', fn(args) {
    if (args.length !== 1) throw new EvalError('write: expected 1 argument');
    output.push(displayValue(args[0]));
    return { tag: 'nil' };
  }});
  env.define('newline', { tag: 'builtin', name: 'newline', fn(args) {
    output.push('\n');
    return { tag: 'nil' };
  }});

  // String operations
  env.define('string-append', { tag: 'builtin', name: 'string-append', fn(args) {
    const strs = args.map(a => {
      if (a.tag !== 'string') throw new EvalError('string-append: expected string');
      return a.value;
    });
    return { tag: 'string', value: strs.join('') };
  }});
  env.define('string-length', { tag: 'builtin', name: 'string-length', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-length: expected string');
    return { tag: 'number', value: args[0].value.length };
  }});
  env.define('substring', { tag: 'builtin', name: 'substring', fn(args) {
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'number')
      throw new EvalError('substring: expected string, number, number');
    return { tag: 'string', value: args[0].value.slice(args[1].value, args[2].value) };
  }});
  env.define('string->number', { tag: 'builtin', name: 'string->number', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->number: expected string');
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  }});
  env.define('number->string', { tag: 'builtin', name: 'number->string', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('number->string: expected number');
    return { tag: 'string', value: String(args[0].value) };
  }});
  env.define('symbol->string', { tag: 'builtin', name: 'symbol->string', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'symbol') throw new EvalError('symbol->string: expected symbol');
    return { tag: 'string', value: args[0].name };
  }});
  env.define('string->symbol', { tag: 'builtin', name: 'string->symbol', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->symbol: expected string');
    return { tag: 'symbol', name: args[0].value };
  }});
  env.define('string-ref', { tag: 'builtin', name: 'string-ref', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'number')
      throw new EvalError('string-ref: expected string and number');
    return { tag: 'char', value: args[0].value[args[1].value] };
  }});
  env.define('string-copy', { tag: 'builtin', name: 'string-copy', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-copy: expected string');
    return { tag: 'string', value: args[0].value };
  }});
  env.define('string-set!', { tag: 'builtin', name: 'string-set!', fn(args) {
    throw new EvalError('string-set!: strings are immutable');
  }});

  env.define('string->list', { tag: 'builtin', name: 'string->list', fn(args) {
    if (args.length < 1 || args[0].tag !== 'string')
      throw new EvalError('string->list: expected string');
    const s = args[0].value;
    let result: Value = { tag: 'nil' };
    for (let i = s.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: { tag: 'char', value: s[i] }, cdr: result };
    }
    return result;
  }});

  env.define('list->string', { tag: 'builtin', name: 'list->string', fn(args) {
    if (args.length !== 1) throw new EvalError('list->string: expected one argument');
    let result = '';
    let cur = args[0];
    while (cur.tag === 'pair') {
      if (cur.car.tag !== 'char') throw new EvalError('list->string: expected list of chars');
      result += cur.car.value;
      cur = cur.cdr;
    }
    return { tag: 'string', value: result };
  }});

  env.define('char->integer', { tag: 'builtin', name: 'char->integer', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'char')
      throw new EvalError('char->integer: expected char');
    return { tag: 'number', value: args[0].value.charCodeAt(0) };
  }});

  env.define('integer->char', { tag: 'builtin', name: 'integer->char', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number')
      throw new EvalError('integer->char: expected number');
    return { tag: 'char', value: String.fromCharCode(args[0].value) };
  }});

  // Equality
  function valuesEqual(a: Value, b: Value): boolean {
    if (a.tag !== b.tag) return false;
    switch (a.tag) {
      case 'number': return a.value === (b as typeof a).value;
      case 'boolean': return a.value === (b as typeof a).value;
      case 'string': return a.value === (b as typeof a).value;
      case 'char': return a.value === (b as typeof a).value;
      case 'symbol': return a.name === (b as typeof a).name;
      case 'nil': return true;
      case 'pair': return valuesEqual(a.car, (b as typeof a).car) && valuesEqual(a.cdr, (b as typeof a).cdr);
      case 'vector': {
        const bv = b as typeof a;
        if (a.items.length !== bv.items.length) return false;
        return a.items.every((item, i) => valuesEqual(item, bv.items[i]));
      }
      default: return a === b;
    }
  }

  env.define('eq?', { tag: 'builtin', name: 'eq?', fn(args) {
    if (args.length !== 2) throw new EvalError('eq?: expected 2 arguments');
    const a = args[0], b = args[1];
    if (a.tag !== b.tag) return { tag: 'boolean', value: false };
    switch (a.tag) {
      case 'number': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'boolean': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'symbol': return { tag: 'boolean', value: a.name === (b as typeof a).name };
      case 'char': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'nil': return { tag: 'boolean', value: true };
      default: return { tag: 'boolean', value: a === b };
    }
  }});
  env.define('equal?', { tag: 'builtin', name: 'equal?', fn(args) {
    if (args.length !== 2) throw new EvalError('equal?: expected 2 arguments');
    return { tag: 'boolean', value: valuesEqual(args[0], args[1]) };
  }});
  env.define('eqv?', { tag: 'builtin', name: 'eqv?', fn(args) {
    if (args.length !== 2) throw new EvalError('eqv?: expected 2 arguments');
    const a = args[0], b = args[1];
    if (a.tag !== b.tag) return { tag: 'boolean', value: false };
    switch (a.tag) {
      case 'number': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'boolean': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'symbol': return { tag: 'boolean', value: a.name === (b as typeof a).name };
      case 'char': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'nil': return { tag: 'boolean', value: true };
      default: return { tag: 'boolean', value: a === b };
    }
  }});

  // L13 Numeric builtins
  env.define('abs', { tag: 'builtin', name: 'abs', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('abs: expected number');
    return { tag: 'number', value: Math.abs(args[0].value) };
  }});
  env.define('modulo', { tag: 'builtin', name: 'modulo', fn(args) {
    const nums = requireNumbers(args, 'modulo');
    if (nums.length !== 2) throw new EvalError('modulo: expected 2 arguments');
    const [a, b] = nums;
    return { tag: 'number', value: ((a % b) + b) % b };
  }});
  env.define('remainder', { tag: 'builtin', name: 'remainder', fn(args) {
    const nums = requireNumbers(args, 'remainder');
    if (nums.length !== 2) throw new EvalError('remainder: expected 2 arguments');
    return { tag: 'number', value: nums[0] % nums[1] };
  }});
  env.define('quotient', { tag: 'builtin', name: 'quotient', fn(args) {
    const nums = requireNumbers(args, 'quotient');
    if (nums.length !== 2) throw new EvalError('quotient: expected 2 arguments');
    return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
  }});
  env.define('min', { tag: 'builtin', name: 'min', fn(args) {
    if (args.length === 0) throw new EvalError('min: need at least 1 argument');
    const nums = requireNumbers(args, 'min');
    return { tag: 'number', value: Math.min(...nums) };
  }});
  env.define('max', { tag: 'builtin', name: 'max', fn(args) {
    if (args.length === 0) throw new EvalError('max: need at least 1 argument');
    const nums = requireNumbers(args, 'max');
    return { tag: 'number', value: Math.max(...nums) };
  }});
  env.define('expt', { tag: 'builtin', name: 'expt', fn(args) {
    const nums = requireNumbers(args, 'expt');
    if (nums.length !== 2) throw new EvalError('expt: expected 2 arguments');
    return { tag: 'number', value: Math.pow(nums[0], nums[1]) };
  }});
  env.define('zero?', { tag: 'builtin', name: 'zero?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('zero?: expected number');
    return { tag: 'boolean', value: args[0].value === 0 };
  }});
  env.define('positive?', { tag: 'builtin', name: 'positive?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('positive?: expected number');
    return { tag: 'boolean', value: args[0].value > 0 };
  }});
  env.define('negative?', { tag: 'builtin', name: 'negative?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('negative?: expected number');
    return { tag: 'boolean', value: args[0].value < 0 };
  }});
  env.define('odd?', { tag: 'builtin', name: 'odd?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('odd?: expected number');
    return { tag: 'boolean', value: Math.abs(args[0].value) % 2 === 1 };
  }});
  env.define('even?', { tag: 'builtin', name: 'even?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('even?: expected number');
    return { tag: 'boolean', value: args[0].value % 2 === 0 };
  }});

  // L13 List builtins
  env.define('list-ref', { tag: 'builtin', name: 'list-ref', fn(args) {
    if (args.length !== 2 || args[1].tag !== 'number') throw new EvalError('list-ref: expected list and number');
    let cur = args[0];
    let idx = args[1].value;
    while (idx > 0) {
      if (cur.tag !== 'pair') throw new EvalError('list-ref: index out of range');
      cur = cur.cdr;
      idx--;
    }
    if (cur.tag !== 'pair') throw new EvalError('list-ref: index out of range');
    return cur.car;
  }});
  env.define('list-tail', { tag: 'builtin', name: 'list-tail', fn(args) {
    if (args.length !== 2 || args[1].tag !== 'number') throw new EvalError('list-tail: expected list and number');
    let cur = args[0];
    let idx = args[1].value;
    while (idx > 0) {
      if (cur.tag !== 'pair') throw new EvalError('list-tail: index out of range');
      cur = cur.cdr;
      idx--;
    }
    return cur;
  }});
  env.define('list?', { tag: 'builtin', name: 'list?', fn(args) {
    if (args.length !== 1) throw new EvalError('list?: expected 1 argument');
    let cur = args[0];
    while (cur.tag === 'pair') cur = cur.cdr;
    return { tag: 'boolean', value: cur.tag === 'nil' };
  }});
  env.define('assoc', { tag: 'builtin', name: 'assoc', fn(args) {
    if (args.length !== 2) throw new EvalError('assoc: expected 2 arguments');
    const key = args[0];
    let alist = args[1];
    while (alist.tag === 'pair') {
      const entry = alist.car;
      if (entry.tag === 'pair' && valuesEqual(entry.car, key)) return entry;
      alist = alist.cdr;
    }
    return { tag: 'boolean', value: false };
  }});

  // map (supports multiple list arguments)
  env.define('map', { tag: 'builtin', name: 'map', fn(args) {
    if (args.length < 2) throw new EvalError('map: need at least 2 arguments');
    const proc = args[0];
    // Collect all lists into arrays
    const lists: Value[][] = [];
    for (let i = 1; i < args.length; i++) {
      const arr: Value[] = [];
      let cur = args[i];
      while (cur.tag === 'pair') { arr.push(cur.car); cur = cur.cdr; }
      lists.push(arr);
    }
    const len = lists[0].length;
    let result: Value = { tag: 'nil' };
    const results: Value[] = [];
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map(l => l[i]);
      if (proc.tag === 'builtin') {
        results.push(proc.fn(callArgs));
      } else if (proc.tag === 'lambda') {
        results.push(applyFn(proc, callArgs, { line: 0, col: 0 }));
      } else {
        throw new EvalError('map: first argument must be a procedure');
      }
    }
    for (let i = results.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: results[i], cdr: result };
    }
    return result;
  }});

  // L13 Character builtins
  env.define('char-alphabetic?', { tag: 'builtin', name: 'char-alphabetic?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-alphabetic?: expected char');
    return { tag: 'boolean', value: /^[a-zA-Z]$/.test(args[0].value) };
  }});
  env.define('char-numeric?', { tag: 'builtin', name: 'char-numeric?', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-numeric?: expected char');
    return { tag: 'boolean', value: /^[0-9]$/.test(args[0].value) };
  }});
  env.define('char-upcase', { tag: 'builtin', name: 'char-upcase', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-upcase: expected char');
    return { tag: 'char', value: args[0].value.toUpperCase() };
  }});
  env.define('char-downcase', { tag: 'builtin', name: 'char-downcase', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-downcase: expected char');
    return { tag: 'char', value: args[0].value.toLowerCase() };
  }});
  env.define('char=?', { tag: 'builtin', name: 'char=?', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char=?: expected chars');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  }});
  env.define('char<?', { tag: 'builtin', name: 'char<?', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char<?: expected chars');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  }});

  // L13 String builtins
  env.define('string=?', { tag: 'builtin', name: 'string=?', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string=?: expected strings');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  }});
  env.define('string<?', { tag: 'builtin', name: 'string<?', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string<?: expected strings');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  }});
  env.define('string-ci=?', { tag: 'builtin', name: 'string-ci=?', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string-ci=?: expected strings');
    return { tag: 'boolean', value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
  }});
  env.define('string-upcase', { tag: 'builtin', name: 'string-upcase', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-upcase: expected string');
    return { tag: 'string', value: args[0].value.toUpperCase() };
  }});
  env.define('string-downcase', { tag: 'builtin', name: 'string-downcase', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-downcase: expected string');
    return { tag: 'string', value: args[0].value.toLowerCase() };
  }});

  // apply
  env.define('apply', { tag: 'builtin', name: 'apply', fn(args) {
    if (args.length < 2) throw new EvalError('apply: need at least 2 arguments');
    const proc = args[0];
    // Last arg must be a list; prefix args are prepended
    let lastArg = args[args.length - 1];
    const collected: Value[] = [];
    for (let i = 1; i < args.length - 1; i++) {
      collected.push(args[i]);
    }
    // Flatten the last argument (a list) into collected
    while (lastArg.tag === 'pair') {
      collected.push(lastArg.car);
      lastArg = lastArg.cdr;
    }
    if (lastArg.tag !== 'nil') throw new EvalError('apply: last argument must be a proper list');
    if (proc.tag === 'builtin') return proc.fn(collected);
    if (proc.tag === 'lambda') {
      const callEnv = new Env(proc.env);
      for (let i = 0; i < proc.params.length; i++) {
        callEnv.define(proc.params[i], collected[i]);
      }
      if (proc.rest !== null) {
        let restList: Value = { tag: 'nil' };
        for (let i = collected.length - 1; i >= proc.params.length; i--) {
          restList = { tag: 'pair', car: collected[i], cdr: restList };
        }
        callEnv.define(proc.rest, restList);
      }
      let result: Value = { tag: 'nil' };
      for (const bodyExpr of proc.body) {
        result = evaluate(bodyExpr, callEnv);
      }
      return result;
    }
    if (proc.tag === 'continuation') {
      if (collected.length !== 1) throw new EvalError('continuation: expected 1 argument');
      throw new ContinuationJump(proc.id, collected[0], proc.exprPos, proc.topIdx);
    }
    throw new EvalError('apply: first argument must be a procedure');
  }});

  // Vectors
  env.define('vector', { tag: 'builtin', name: 'vector', fn(args) {
    return { tag: 'vector', items: [...args] };
  }});
  env.define('make-vector', { tag: 'builtin', name: 'make-vector', fn(args) {
    if (args.length < 1 || args[0].tag !== 'number') throw new EvalError('make-vector: expected number');
    const len = args[0].value;
    const fill: Value = args.length > 1 ? args[1] : { tag: 'number', value: 0 };
    const items: Value[] = [];
    for (let i = 0; i < len; i++) items.push(fill);
    return { tag: 'vector', items };
  }});
  env.define('vector-ref', { tag: 'builtin', name: 'vector-ref', fn(args) {
    if (args.length !== 2 || args[0].tag !== 'vector' || args[1].tag !== 'number')
      throw new EvalError('vector-ref: expected vector and number');
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].items.length) throw new EvalError('vector-ref: index out of range');
    return args[0].items[idx];
  }});
  env.define('vector-set!', { tag: 'builtin', name: 'vector-set!', fn(args) {
    if (args.length !== 3 || args[0].tag !== 'vector' || args[1].tag !== 'number')
      throw new EvalError('vector-set!: expected vector, number, and value');
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].items.length) throw new EvalError('vector-set!: index out of range');
    args[0].items[idx] = args[2];
    return { tag: 'nil' };
  }});
  env.define('vector?', { tag: 'builtin', name: 'vector?', fn(args) {
    if (args.length !== 1) throw new EvalError('vector?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'vector' };
  }});
  env.define('vector-length', { tag: 'builtin', name: 'vector-length', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'vector') throw new EvalError('vector-length: expected vector');
    return { tag: 'number', value: args[0].items.length };
  }});
  env.define('vector->list', { tag: 'builtin', name: 'vector->list', fn(args) {
    if (args.length !== 1 || args[0].tag !== 'vector') throw new EvalError('vector->list: expected vector');
    let result: Value = { tag: 'nil' };
    for (let i = args[0].items.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: args[0].items[i], cdr: result };
    }
    return result;
  }});
  env.define('list->vector', { tag: 'builtin', name: 'list->vector', fn(args) {
    if (args.length !== 1) throw new EvalError('list->vector: expected 1 argument');
    const items: Value[] = [];
    let cur = args[0];
    while (cur.tag === 'pair') {
      items.push(cur.car);
      cur = cur.cdr;
    }
    return { tag: 'vector', items };
  }});

  // call/cc — handled specially by the evaluator
  env.define('call/cc', { tag: 'builtin', name: 'call/cc', fn() { throw new EvalError('call/cc: internal'); } });
  env.define('call-with-current-continuation', { tag: 'builtin', name: 'call/cc', fn() { throw new EvalError('call/cc: internal'); } });

  return env;
}

// ── Quote helper ────────────────────────────────────────────────────

function exprToValue(expr: Expr): Value {
  switch (expr.tag) {
    case 'number': return { tag: 'number', value: expr.value };
    case 'boolean': return { tag: 'boolean', value: expr.value };
    case 'string': return { tag: 'string', value: expr.value };
    case 'char': return { tag: 'char', value: expr.value };
    case 'symbol': return { tag: 'symbol', name: expr.name };
    case 'list': {
      let result: Value = { tag: 'nil' };
      for (let i = expr.items.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: exprToValue(expr.items[i]), cdr: result };
      }
      return result;
    }
  }
}

// ── Parameter parsing helper ─────────────────────────────────────────

function parseParams(exprs: Expr[]): { params: string[]; rest: string | null } {
  const dotIdx = exprs.findIndex(e => e.tag === 'symbol' && e.name === '.');
  if (dotIdx === -1) {
    return {
      params: exprs.map(p => {
        if (p.tag !== 'symbol') throw new EvalError('expected symbol in parameter list');
        return p.name;
      }),
      rest: null,
    };
  }
  if (dotIdx !== exprs.length - 2) throw new EvalError('bad dot in parameter list');
  const restExpr = exprs[exprs.length - 1];
  if (restExpr.tag !== 'symbol') throw new EvalError('expected symbol after dot');
  return {
    params: exprs.slice(0, dotIdx).map(p => {
      if (p.tag !== 'symbol') throw new EvalError('expected symbol in parameter list');
      return p.name;
    }),
    rest: restExpr.name,
  };
}

// ── Apply helper (no TCO, used by call/cc) ──────────────────────────

function applyFn(fn: Value, args: Value[], pos: Pos): Value {
  if (fn.tag === 'builtin') {
    if (fn.name === 'call/cc') {
      throw new EvalError(`${fmtPos(pos)}: call/cc: cannot be used inside apply`);
    }
    try {
      return fn.fn(args);
    } catch (e) {
      if (e instanceof EvalError && !e.message.match(/^\d+:/)) {
        throw new EvalError(`${fmtPos(pos)}: ${e.message}`);
      }
      throw e;
    }
  }
  if (fn.tag === 'lambda') {
    const callEnv = new Env(fn.env);
    for (let i = 0; i < fn.params.length; i++) {
      callEnv.define(fn.params[i], args[i]);
    }
    if (fn.rest !== null) {
      let restList: Value = { tag: 'nil' };
      for (let i = args.length - 1; i >= fn.params.length; i--) {
        restList = { tag: 'pair', car: args[i], cdr: restList };
      }
      callEnv.define(fn.rest, restList);
    }
    let result: Value = { tag: 'nil' };
    for (const bodyExpr of fn.body) {
      result = evaluate(bodyExpr, callEnv);
    }
    return result;
  }
  if (fn.tag === 'continuation') {
    if (args.length !== 1) throw new EvalError('continuation: expected 1 argument');
    throw new ContinuationJump(fn.id, args[0], fn.exprPos, fn.topIdx);
  }
  throw new EvalError(`${fmtPos(pos)}: not a procedure`);
}

// ── Macros (syntax-rules) ────────────────────────────────────────────

let gensymCounter = 0;
function gensym(name: string): string {
  return `${name}$$${++gensymCounter}`;
}

const SPECIAL_FORMS = new Set([
  'if', 'define', 'set!', 'quote', 'lambda', 'and', 'or',
  'let', 'begin', 'cond', 'define-syntax', 'syntax-rules',
]);

function collectPatternVars(patternItems: Expr[], literals: Set<string>): Set<string> {
  const vars = new Set<string>();
  function walk(e: Expr): void {
    if (e.tag === 'symbol') {
      if (e.name !== '...' && e.name !== '_' && !literals.has(e.name)) {
        vars.add(e.name);
      }
    } else if (e.tag === 'list') {
      for (const item of e.items) walk(item);
    }
  }
  for (const item of patternItems) walk(item);
  return vars;
}

type PatternBindings = Map<string, Expr | Expr[]>;

function matchPattern(pattern: Expr[], input: Expr[], literals: Set<string>): PatternBindings | null {
  const bindings: PatternBindings = new Map();
  let pi = 0, ii = 0;

  while (pi < pattern.length) {
    // Check for ellipsis following this pattern element
    const nextPat = pi + 1 < pattern.length ? pattern[pi + 1] : null;
    if (nextPat && nextPat.tag === 'symbol' && nextPat.name === '...') {
      const subPat = pattern[pi];
      const remaining = pattern.length - pi - 2;
      const available = input.length - ii - remaining;
      if (available < 0) return null;
      if (subPat.tag === 'symbol' && !literals.has(subPat.name) && subPat.name !== '_') {
        bindings.set(subPat.name, input.slice(ii, ii + available));
      }
      ii += available;
      pi += 2;
      continue;
    }

    if (ii >= input.length) return null;

    const pat = pattern[pi];
    const inp = input[ii];

    if (pat.tag === 'symbol') {
      if (literals.has(pat.name)) {
        if (inp.tag !== 'symbol' || inp.name !== pat.name) return null;
      } else if (pat.name === '_') {
        // wildcard
      } else {
        bindings.set(pat.name, inp);
      }
    } else if (pat.tag === 'list' && inp.tag === 'list') {
      const sub = matchPattern(pat.items, inp.items, literals);
      if (sub === null) return null;
      for (const [k, v] of sub) bindings.set(k, v);
    } else {
      return null;
    }

    pi++;
    ii++;
  }

  if (ii !== input.length) return null;
  return bindings;
}

function findEllipsisVars(template: Expr, patternVars: Set<string>): string[] {
  if (template.tag === 'symbol' && patternVars.has(template.name)) return [template.name];
  if (template.tag === 'list') {
    const vars: string[] = [];
    for (const item of template.items) vars.push(...findEllipsisVars(item, patternVars));
    return vars;
  }
  return [];
}

function expandTemplate(
  template: Expr,
  bindings: PatternBindings,
  patternVars: Set<string>,
  renameMap: Map<string, string>,
  literals: Set<string>,
  pos: Pos,
): Expr {
  if (template.tag === 'symbol') {
    const name = template.name;
    if (patternVars.has(name)) {
      const val = bindings.get(name);
      if (val !== undefined && !Array.isArray(val)) return val;
      if (val === undefined) return template;
      throw new EvalError('macro: ellipsis variable used outside ellipsis context');
    }
    if (SPECIAL_FORMS.has(name) || literals.has(name)) return template;
    if (!renameMap.has(name)) renameMap.set(name, gensym(name));
    return { tag: 'symbol', name: renameMap.get(name)!, pos: template.pos };
  }

  if (template.tag === 'list') {
    const result: Expr[] = [];
    for (let i = 0; i < template.items.length; i++) {
      const nextTpl = i + 1 < template.items.length ? template.items[i + 1] : null;
      if (nextTpl && nextTpl.tag === 'symbol' && nextTpl.name === '...') {
        const subTemplate = template.items[i];
        const ellipsisVars = findEllipsisVars(subTemplate, patternVars);
        if (ellipsisVars.length > 0) {
          const firstArr = bindings.get(ellipsisVars[0]);
          const count = Array.isArray(firstArr) ? firstArr.length : 0;
          for (let k = 0; k < count; k++) {
            const iterBindings = new Map(bindings);
            for (const ev of ellipsisVars) {
              const arr = bindings.get(ev);
              if (Array.isArray(arr)) iterBindings.set(ev, arr[k]);
            }
            result.push(expandTemplate(subTemplate, iterBindings, patternVars, renameMap, literals, pos));
          }
        }
        i++; // skip ...
        continue;
      }
      result.push(expandTemplate(template.items[i], bindings, patternVars, renameMap, literals, pos));
    }
    return { tag: 'list', items: result, pos: template.pos };
  }

  return template;
}

function expandMacro(macro: Value & { tag: 'macro' }, form: Expr & { tag: 'list' }, env: Env): Expr {
  const literals = new Set(macro.literals);
  for (const rule of macro.rules) {
    if (rule.pattern.tag !== 'list') continue;
    const patternItems = rule.pattern.items.slice(1); // skip macro name placeholder
    const inputItems = form.items.slice(1);
    const bindings = matchPattern(patternItems, inputItems, literals);
    if (bindings !== null) {
      const patternVars = collectPatternVars(patternItems, literals);
      const renameMap = new Map<string, string>();
      const expanded = expandTemplate(rule.template, bindings, patternVars, renameMap, literals, form.pos);

      // Set up definition-site bindings for renamed symbols
      for (const [orig, renamed] of renameMap) {
        try {
          const val = macro.defEnv.get(orig);
          env.define(renamed, val);
        } catch {
          // Not bound in definition env (e.g., introduced binding like tmp)
        }
      }

      return expanded;
    }
  }
  throw new EvalError(`${fmtPos(form.pos)}: no matching pattern for macro`);
}

// ── Eval ─────────────────────────────────────────────────────────────

function evaluate(expr: Expr, env: Env): Value {
  // Trampoline loop for TCO — tail positions reassign expr/env and continue
  trampoline: while (true) {
  switch (expr.tag) {
    case 'number': return { tag: 'number', value: expr.value };
    case 'boolean': return { tag: 'boolean', value: expr.value };
    case 'string': return { tag: 'string', value: expr.value };
    case 'char': return { tag: 'char', value: expr.value };
    case 'symbol': return env.get(expr.name, expr.pos);
    case 'list': {
      const items = expr.items;
      if (items.length === 0) throw new EvalError(`${fmtPos(expr.pos)}: empty application`);

      const head = items[0];
      if (head.tag === 'symbol') {
        // Special forms
        switch (head.name) {
          case 'if': {
            if (items.length < 3) throw new EvalError(`${fmtPos(expr.pos)}: if: too few arguments`);
            const cond = evaluate(items[1], env);
            if (isTruthy(cond)) {
              expr = items[2]; continue; // TCO
            }
            if (items.length > 3) {
              expr = items[3]; continue; // TCO
            }
            return { tag: 'nil' };
          }

          case 'define': {
            if (items.length < 2) throw new EvalError(`${fmtPos(expr.pos)}: define: bad syntax`);
            const target = items[1];
            if (target.tag === 'symbol') {
              const val = evaluate(items[2], env);
              env.define(target.name, val);
              return { tag: 'nil' };
            }
            if (target.tag === 'list') {
              const nameExpr = target.items[0];
              if (nameExpr.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}: define: expected symbol`);
              const { params, rest } = parseParams(target.items.slice(1));
              const body = items.slice(2);
              const lambda: Value = { tag: 'lambda', params, rest, body, env };
              env.define(nameExpr.name, lambda);
              return { tag: 'nil' };
            }
            throw new EvalError(`${fmtPos(expr.pos)}: define: bad syntax`);
          }

          case 'set!': {
            if (items.length !== 3) throw new EvalError(`${fmtPos(expr.pos)}: set!: bad syntax`);
            const target = items[1];
            if (target.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}: set!: expected symbol`);
            const val = evaluate(items[2], env);
            env.set(target.name, val, expr.pos);
            return { tag: 'nil' };
          }

          case 'quote':
            return exprToValue(items[1]);

          case 'lambda': {
            const paramsExpr = items[1];
            if (paramsExpr.tag === 'symbol') {
              // (lambda args body...) — all args as rest
              return { tag: 'lambda', params: [], rest: paramsExpr.name, body: items.slice(2), env };
            }
            if (paramsExpr.tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}: lambda: expected parameter list`);
            const { params, rest } = parseParams(paramsExpr.items);
            const body = items.slice(2);
            return { tag: 'lambda', params, rest, body, env };
          }

          case 'and': {
            if (items.length === 1) return { tag: 'boolean', value: true };
            for (let i = 1; i < items.length - 1; i++) {
              const v = evaluate(items[i], env);
              if (!isTruthy(v)) return v;
            }
            expr = items[items.length - 1]; continue; // TCO last
          }

          case 'or': {
            if (items.length === 1) return { tag: 'boolean', value: false };
            for (let i = 1; i < items.length - 1; i++) {
              const v = evaluate(items[i], env);
              if (isTruthy(v)) return v;
            }
            expr = items[items.length - 1]; continue; // TCO last
          }

          case 'let': {
            // Named let: (let name ((var init) ...) body...)
            if (items[1].tag === 'symbol') {
              const loopName = items[1].name;
              const bindingsExpr = items[2];
              if (bindingsExpr.tag !== 'list') throw new EvalError('let: expected bindings list');
              const params: string[] = [];
              const inits: Value[] = [];
              for (const b of bindingsExpr.items) {
                if (b.tag !== 'list' || b.items.length !== 2) throw new EvalError('let: bad binding');
                if (b.items[0].tag !== 'symbol') throw new EvalError('let: expected symbol');
                params.push(b.items[0].name);
                inits.push(evaluate(b.items[1], env));
              }
              const body = items.slice(3);
              const loopLambda: Value = { tag: 'lambda', params, rest: null, body, env };
              // The lambda's env needs to include itself for recursion
              const loopEnv = new Env(env);
              loopEnv.define(loopName, loopLambda);
              (loopLambda as any).env = loopEnv;
              // Now call it with initial values
              const callEnv = new Env(loopEnv);
              for (let i = 0; i < params.length; i++) {
                callEnv.define(params[i], inits[i]);
              }
              for (let i = 0; i < body.length - 1; i++) {
                evaluate(body[i], callEnv);
              }
              expr = body[body.length - 1]; env = callEnv; continue; // TCO
            }
            // Regular let
            const bindingsExpr = items[1];
            if (bindingsExpr.tag !== 'list') throw new EvalError('let: expected bindings list');
            const letEnv = new Env(env);
            for (const b of bindingsExpr.items) {
              if (b.tag !== 'list' || b.items.length !== 2) throw new EvalError('let: bad binding');
              if (b.items[0].tag !== 'symbol') throw new EvalError('let: expected symbol');
              const val = evaluate(b.items[1], env);
              letEnv.define(b.items[0].name, val);
            }
            for (let i = 2; i < items.length - 1; i++) {
              evaluate(items[i], letEnv);
            }
            expr = items[items.length - 1]; env = letEnv; continue; // TCO
          }

          case 'begin': {
            if (items.length === 1) return { tag: 'nil' };
            for (let i = 1; i < items.length - 1; i++) {
              evaluate(items[i], env);
            }
            expr = items[items.length - 1]; continue; // TCO
          }

          case 'define-syntax': {
            if (items.length !== 3) throw new EvalError(`${fmtPos(expr.pos)}: define-syntax: bad syntax`);
            const dsName = items[1];
            if (dsName.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}: define-syntax: expected symbol`);
            const transformer = items[2];
            if (transformer.tag !== 'list' || transformer.items.length < 2 ||
                transformer.items[0].tag !== 'symbol' || transformer.items[0].name !== 'syntax-rules') {
              throw new EvalError(`${fmtPos(expr.pos)}: define-syntax: expected syntax-rules`);
            }
            const litExpr = transformer.items[1];
            if (litExpr.tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}: syntax-rules: expected literals list`);
            const lits = litExpr.items.map(l => {
              if (l.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}: syntax-rules: expected symbol in literals`);
              return l.name;
            });
            const macroRules: { pattern: Expr; template: Expr }[] = [];
            for (let ri = 2; ri < transformer.items.length; ri++) {
              const rule = transformer.items[ri];
              if (rule.tag !== 'list' || rule.items.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}: syntax-rules: bad rule`);
              macroRules.push({ pattern: rule.items[0], template: rule.items[1] });
            }
            env.define(dsName.name, { tag: 'macro', literals: lits, rules: macroRules, defEnv: env });
            return { tag: 'nil' };
          }

          case 'cond': {
            for (let i = 1; i < items.length; i++) {
              const clause = items[i];
              if (clause.tag !== 'list' || clause.items.length < 1) throw new EvalError('cond: bad clause');
              if (clause.items[0].tag === 'symbol' && clause.items[0].name === 'else') {
                for (let j = 1; j < clause.items.length - 1; j++) {
                  evaluate(clause.items[j], env);
                }
                expr = clause.items[clause.items.length - 1]; continue trampoline; // TCO
              }
              const test = evaluate(clause.items[0], env);
              if (isTruthy(test)) {
                if (clause.items.length === 1) return test;
                for (let j = 1; j < clause.items.length - 1; j++) {
                  evaluate(clause.items[j], env);
                }
                expr = clause.items[clause.items.length - 1]; continue trampoline; // TCO
              }
            }
            return { tag: 'nil' };
          }

          case 'letrec': {
            const bindingsExpr = items[1];
            if (bindingsExpr.tag !== 'list') throw new EvalError('letrec: expected bindings list');
            const letrecEnv = new Env(env);
            // First define all names as undefined
            const names: string[] = [];
            for (const b of bindingsExpr.items) {
              if (b.tag !== 'list' || b.items.length !== 2) throw new EvalError('letrec: bad binding');
              if (b.items[0].tag !== 'symbol') throw new EvalError('letrec: expected symbol');
              names.push(b.items[0].name);
              letrecEnv.define(b.items[0].name, { tag: 'nil' });
            }
            // Then evaluate all init expressions in the letrec env
            for (let i = 0; i < bindingsExpr.items.length; i++) {
              const val = evaluate(bindingsExpr.items[i].tag === 'list' ? (bindingsExpr.items[i] as any).items[1] : bindingsExpr.items[i], letrecEnv);
              letrecEnv.define(names[i], val);
            }
            for (let i = 2; i < items.length - 1; i++) {
              evaluate(items[i], letrecEnv);
            }
            expr = items[items.length - 1]; env = letrecEnv; continue trampoline;
          }

          case 'letrec*': {
            const bindingsExpr = items[1];
            if (bindingsExpr.tag !== 'list') throw new EvalError('letrec*: expected bindings list');
            const letrecStarEnv = new Env(env);
            // Define all names first, then evaluate sequentially
            for (const b of bindingsExpr.items) {
              if (b.tag !== 'list' || b.items.length !== 2) throw new EvalError('letrec*: bad binding');
              if (b.items[0].tag !== 'symbol') throw new EvalError('letrec*: expected symbol');
              letrecStarEnv.define(b.items[0].name, { tag: 'nil' });
            }
            for (const b of bindingsExpr.items) {
              const val = evaluate((b as any).items[1], letrecStarEnv);
              letrecStarEnv.define((b as any).items[0].name, val);
            }
            for (let i = 2; i < items.length - 1; i++) {
              evaluate(items[i], letrecStarEnv);
            }
            expr = items[items.length - 1]; env = letrecStarEnv; continue trampoline;
          }

          case 'case': {
            const key = evaluate(items[1], env);
            for (let i = 2; i < items.length; i++) {
              const clause = items[i];
              if (clause.tag !== 'list' || clause.items.length < 2) throw new EvalError('case: bad clause');
              // else clause
              if (clause.items[0].tag === 'symbol' && clause.items[0].name === 'else') {
                for (let j = 1; j < clause.items.length - 1; j++) {
                  evaluate(clause.items[j], env);
                }
                expr = clause.items[clause.items.length - 1]; continue trampoline;
              }
              // datum list
              const datums = clause.items[0];
              if (datums.tag !== 'list') throw new EvalError('case: expected datum list');
              let matched = false;
              for (const d of datums.items) {
                const dv = exprToValue(d);
                if (eqvCompare(key, dv)) { matched = true; break; }
              }
              if (matched) {
                for (let j = 1; j < clause.items.length - 1; j++) {
                  evaluate(clause.items[j], env);
                }
                expr = clause.items[clause.items.length - 1]; continue trampoline;
              }
            }
            return { tag: 'nil' };
          }
        }
      }

      // Macro expansion check
      if (head.tag === 'symbol') {
        try {
          const maybeM = env.get(head.name);
          if (maybeM.tag === 'macro') {
            expr = expandMacro(maybeM, expr as Expr & { tag: 'list' }, env);
            continue trampoline;
          }
        } catch { /* not bound — proceed to function application */ }
      }

      // Function application
      const fn = evaluate(head, env);
      const args = items.slice(1).map(a => evaluate(a, env));

      // call/cc handling
      if (fn.tag === 'builtin' && fn.name === 'call/cc') {
        const posKey = `${expr.pos.line}:${expr.pos.col}`;
        if (pendingContReturn && pendingContReturn.exprPos === posKey) {
          const val = pendingContReturn.value;
          pendingContReturn = null;
          return val;
        }
        if (args.length !== 1) throw new EvalError(`${fmtPos(expr.pos)}: call/cc: expected 1 argument`);
        const proc = args[0];
        const id = nextContId++;
        const cont: Value = { tag: 'continuation', id, exprPos: posKey, topIdx: currentTopIdx };
        try {
          return applyFn(proc, [cont], expr.pos);
        } catch (e) {
          if (e instanceof ContinuationJump && e.id === id) {
            return e.value;
          }
          throw e;
        }
      }

      // Continuation invocation
      if (fn.tag === 'continuation') {
        if (args.length !== 1) throw new EvalError(`${fmtPos(expr.pos)}: continuation: expected 1 argument`);
        throw new ContinuationJump(fn.id, args[0], fn.exprPos, fn.topIdx);
      }

      if (fn.tag === 'builtin') {
        try {
          return fn.fn(args);
        } catch (e) {
          if (e instanceof EvalError && !e.message.match(/^\d+:/)) {
            throw new EvalError(`${fmtPos(expr.pos)}: ${e.message}`);
          }
          throw e;
        }
      }
      if (fn.tag === 'lambda') {
        const callEnv = new Env(fn.env);
        for (let i = 0; i < fn.params.length; i++) {
          callEnv.define(fn.params[i], args[i]);
        }
        if (fn.rest !== null) {
          let restList: Value = { tag: 'nil' };
          for (let i = args.length - 1; i >= fn.params.length; i--) {
            restList = { tag: 'pair', car: args[i], cdr: restList };
          }
          callEnv.define(fn.rest, restList);
        }
        for (let i = 0; i < fn.body.length - 1; i++) {
          evaluate(fn.body[i], callEnv);
        }
        expr = fn.body[fn.body.length - 1]; env = callEnv; continue; // TCO
      }
      throw new EvalError(`${fmtPos(expr.pos)}: not a procedure`);
    }
  }
  } // end while
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
function evalProgram(exprs: Expr[], env: Env): Value {
  let result: Value = { tag: 'nil' };
  nextContId = 0;
  gensymCounter = 0;
  pendingContReturn = null;
  for (let i = 0; i < exprs.length; i++) {
    currentTopIdx = i;
    try {
      result = evaluate(exprs[i], env);
    } catch (e) {
      if (e instanceof ContinuationJump) {
        pendingContReturn = { exprPos: e.exprPos, value: e.value };
        i = e.topIdx - 1; // -1 because for loop increments
        continue;
      }
      throw e;
    }
  }
  return result;
}

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  return displayValue(evalProgram(exprs, env));
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const output: string[] = [];
  const env = makeGlobalEnv(output);
  return { result: displayValue(evalProgram(exprs, env)), output: output.join('') };
}
