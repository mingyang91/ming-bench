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
  | { tag: 'builtin'; name: string; fn: (args: Value[]) => Value }
  | { tag: 'lambda'; params: string[]; rest: string | null; body: Expr[]; env: Env }
  | { tag: 'continuation'; id: number; exprPos: string; topIdx: number };

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
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'char')
      throw new EvalError('string-set!: expected string, number, char');
    const s = args[0];
    const idx = args[1].value;
    const ch = args[2].value;
    s.value = s.value.substring(0, idx) + ch + s.value.substring(idx + 1);
    return { tag: 'nil' };
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
        }
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
