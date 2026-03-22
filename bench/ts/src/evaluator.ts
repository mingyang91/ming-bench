import { EvalError } from './evalError.js';

// ── AST ──────────────────────────────────────────────────────────────

type Expr =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; name: string }
  | { tag: 'list'; items: Expr[] };

// ── Parser ───────────────────────────────────────────────────────────

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    // whitespace
    if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
      i++;
      continue;
    }
    // comment
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }
    // parens
    if (ch === '(' || ch === ')') {
      tokens.push(ch);
      i++;
      continue;
    }
    // quote shorthand
    if (ch === "'") {
      tokens.push("'");
      i++;
      continue;
    }
    // string literal
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') {
          s += input[i];
          i++;
          if (i < input.length) {
            s += input[i];
            i++;
          }
          continue;
        }
        s += input[i];
        i++;
      }
      if (i < input.length) {
        s += '"';
        i++; // closing quote
      }
      tokens.push(s);
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
      i++;
    }
    tokens.push(atom);
  }
  return tokens;
}

function parseTokens(tokens: string[], pos: number): [Expr, number] {
  if (pos >= tokens.length) {
    throw new EvalError('unexpected end of input');
  }
  const token = tokens[pos];

  if (token === "'") {
    const [inner, next] = parseTokens(tokens, pos + 1);
    return [{ tag: 'list', items: [{ tag: 'symbol', name: 'quote' }, inner] }, next];
  }

  if (token === '(') {
    const items: Expr[] = [];
    pos++;
    while (pos < tokens.length && tokens[pos] !== ')') {
      const [expr, next] = parseTokens(tokens, pos);
      items.push(expr);
      pos = next;
    }
    if (pos >= tokens.length) {
      throw new EvalError('missing closing parenthesis');
    }
    pos++; // skip ')'
    return [{ tag: 'list', items }, pos];
  }

  if (token === ')') {
    throw new EvalError('unexpected )');
  }

  // boolean
  if (token === '#t') return [{ tag: 'boolean', value: true }, pos + 1];
  if (token === '#f') return [{ tag: 'boolean', value: false }, pos + 1];

  // number
  if (/^-?\d+$/.test(token)) {
    return [{ tag: 'number', value: parseInt(token, 10) }, pos + 1];
  }

  // string
  if (token.startsWith('"') && token.endsWith('"')) {
    const raw = token.slice(1, -1);
    const value = raw.replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return [{ tag: 'string', value }, pos + 1];
  }

  // symbol
  return [{ tag: 'symbol', name: token }, pos + 1];
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

  get(name: string): Value {
    const v = this.bindings.get(name);
    if (v !== undefined) return v;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
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
    case 'symbol': return v.name;
    case 'nil': return '()';
    case 'pair': return displayPair(v);
    case 'builtin': return `#<procedure:${v.name}>`;
    case 'lambda': return '#<procedure>';
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

function makeGlobalEnv(): Env {
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
    case 'symbol': return env.get(expr.name);
    case 'list': {
      const items = expr.items;
      if (items.length === 0) throw new EvalError('empty application');

      const head = items[0];
      if (head.tag === 'symbol') {
        // Special forms
        switch (head.name) {
          case 'if': {
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
              if (nameExpr.tag !== 'symbol') throw new EvalError('define: expected symbol');
              const params = target.items.slice(1).map(p => {
                if (p.tag !== 'symbol') throw new EvalError('define: expected symbol');
                return p.name;
              });
              const body = items.slice(2);
              const lambda: Value = { tag: 'lambda', params, body, env };
              env.define(nameExpr.name, lambda);
              return { tag: 'nil' };
            }
            throw new EvalError('define: bad syntax');
          }

          case 'quote':
            return exprToValue(items[1]);

          case 'lambda': {
            const paramsExpr = items[1];
            if (paramsExpr.tag !== 'list') throw new EvalError('lambda: expected parameter list');
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
        }
      }

      // Function application
      const fn = evaluate(head, env);
      const args = items.slice(1).map(a => evaluate(a, env));
      if (fn.tag === 'builtin') {
        return fn.fn(args);
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
      throw new EvalError('not a procedure');
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
  const result = evalStr(input);
  return { result, output: '' };
}
