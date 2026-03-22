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
  | { tag: 'lambda'; params: string[]; body: Expr[]; env: Env };

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

  return env;
}

// ── Quote helper ────────────────────────────────────────────────────

function exprToValue(expr: Expr): Value {
  switch (expr.tag) {
    case 'number': return { tag: 'number', value: expr.value };
    case 'boolean': return { tag: 'boolean', value: expr.value };
    case 'string': return { tag: 'string', value: expr.value };
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

// ── Eval ─────────────────────────────────────────────────────────────

function evaluate(expr: Expr, env: Env): Value {
  switch (expr.tag) {
    case 'number': return { tag: 'number', value: expr.value };
    case 'boolean': return { tag: 'boolean', value: expr.value };
    case 'string': return { tag: 'string', value: expr.value };
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
              return evaluate(items[2], env);
            }
            if (items.length > 3) {
              return evaluate(items[3], env);
            }
            return { tag: 'nil' };
          }

          case 'define': {
            if (items.length < 2) throw new EvalError(`${fmtPos(expr.pos)}: define: bad syntax`);
            const target = items[1];
            if (target.tag === 'symbol') {
              // (define x expr)
              const val = evaluate(items[2], env);
              env.define(target.name, val);
              return { tag: 'nil' };
            }
            if (target.tag === 'list') {
              // (define (f params...) body...)
              const nameExpr = target.items[0];
              if (nameExpr.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}: define: expected symbol`);
              const params = target.items.slice(1).map(p => {
                if (p.tag !== 'symbol') throw new EvalError('define: expected symbol');
                return p.name;
              });
              const body = items.slice(2);
              const lambda: Value = { tag: 'lambda', params, body, env };
              env.define(nameExpr.name, lambda);
              return { tag: 'nil' };
            }
            throw new EvalError(`${fmtPos(expr.pos)}: define: bad syntax`);
          }

          case 'quote':
            return exprToValue(items[1]);

          case 'lambda': {
            const paramsExpr = items[1];
            if (paramsExpr.tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}: lambda: expected parameter list`);
            const params = paramsExpr.items.map(p => {
              if (p.tag !== 'symbol') throw new EvalError('lambda: expected symbol');
              return p.name;
            });
            const body = items.slice(2);
            return { tag: 'lambda', params, body, env };
          }

          case 'and': {
            if (items.length === 1) return { tag: 'boolean', value: true };
            let result: Value = { tag: 'boolean', value: true };
            for (let i = 1; i < items.length; i++) {
              result = evaluate(items[i], env);
              if (!isTruthy(result)) return result;
            }
            return result;
          }

          case 'or': {
            if (items.length === 1) return { tag: 'boolean', value: false };
            let result: Value = { tag: 'boolean', value: false };
            for (let i = 1; i < items.length; i++) {
              result = evaluate(items[i], env);
              if (isTruthy(result)) return result;
            }
            return result;
          }

          case 'let': {
            const bindingsExpr = items[1];
            if (bindingsExpr.tag !== 'list') throw new EvalError('let: expected bindings list');
            const letEnv = new Env(env);
            for (const b of bindingsExpr.items) {
              if (b.tag !== 'list' || b.items.length !== 2) throw new EvalError('let: bad binding');
              if (b.items[0].tag !== 'symbol') throw new EvalError('let: expected symbol');
              const val = evaluate(b.items[1], env);
              letEnv.define(b.items[0].name, val);
            }
            let result: Value = { tag: 'nil' };
            for (let i = 2; i < items.length; i++) {
              result = evaluate(items[i], letEnv);
            }
            return result;
          }

          case 'begin': {
            let result: Value = { tag: 'nil' };
            for (let i = 1; i < items.length; i++) {
              result = evaluate(items[i], env);
            }
            return result;
          }

          case 'cond': {
            for (let i = 1; i < items.length; i++) {
              const clause = items[i];
              if (clause.tag !== 'list' || clause.items.length < 1) throw new EvalError('cond: bad clause');
              if (clause.items[0].tag === 'symbol' && clause.items[0].name === 'else') {
                let result: Value = { tag: 'nil' };
                for (let j = 1; j < clause.items.length; j++) {
                  result = evaluate(clause.items[j], env);
                }
                return result;
              }
              const test = evaluate(clause.items[0], env);
              if (isTruthy(test)) {
                if (clause.items.length === 1) return test;
                let result: Value = { tag: 'nil' };
                for (let j = 1; j < clause.items.length; j++) {
                  result = evaluate(clause.items[j], env);
                }
                return result;
              }
            }
            return { tag: 'nil' };
          }
        }
      }

      // Function application
      const fn = evaluate(head, env);
      const args = items.slice(1).map(a => evaluate(a, env));
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
        let result: Value = { tag: 'nil' };
        for (const bodyExpr of fn.body) {
          result = evaluate(bodyExpr, callEnv);
        }
        return result;
      }
      throw new EvalError(`${fmtPos(expr.pos)}: not a procedure`);
    }
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: Value | undefined;
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return displayValue(result!);
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
  let result: Value | undefined;
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return { result: displayValue(result!), output: output.join('') };
}
