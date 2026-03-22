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
      input[i] !== ';'
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

// ── Values ───────────────────────────────────────────────────────────

type Value =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; name: string }
  | { tag: 'builtin'; name: string; fn: (args: Value[]) => Value };

function isTruthy(v: Value): boolean {
  return !(v.tag === 'boolean' && v.value === false);
}

function displayValue(v: Value): string {
  switch (v.tag) {
    case 'number': return String(v.value);
    case 'boolean': return v.value ? '#t' : '#f';
    case 'string': return `"${v.value}"`;
    case 'symbol': return v.name;
    case 'builtin': return `#<procedure:${v.name}>`;
  }
}

// ── Builtins ─────────────────────────────────────────────────────────

function requireNumbers(args: Value[], name: string): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number`);
    return a.value;
  });
}

const builtins: Record<string, (args: Value[]) => Value> = {
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

// ── Eval ─────────────────────────────────────────────────────────────

function evaluate(expr: Expr): Value {
  switch (expr.tag) {
    case 'number': return { tag: 'number', value: expr.value };
    case 'boolean': return { tag: 'boolean', value: expr.value };
    case 'string': return { tag: 'string', value: expr.value };
    case 'symbol': {
      const name = expr.name;
      if (name in builtins) {
        return { tag: 'builtin', name, fn: builtins[name] };
      }
      throw new EvalError(`unbound variable: ${name}`);
    }
    case 'list': {
      const items = expr.items;
      if (items.length === 0) throw new EvalError('empty application');

      // Special forms: and, or
      const head = items[0];
      if (head.tag === 'symbol') {
        if (head.name === 'and') {
          if (items.length === 1) return { tag: 'boolean', value: true };
          let result: Value = { tag: 'boolean', value: true };
          for (let i = 1; i < items.length; i++) {
            result = evaluate(items[i]);
            if (!isTruthy(result)) return result;
          }
          return result;
        }
        if (head.name === 'or') {
          if (items.length === 1) return { tag: 'boolean', value: false };
          let result: Value = { tag: 'boolean', value: false };
          for (let i = 1; i < items.length; i++) {
            result = evaluate(items[i]);
            if (isTruthy(result)) return result;
          }
          return result;
        }
      }

      // Function application
      const fn = evaluate(head);
      const args = items.slice(1).map(a => evaluate(a));
      if (fn.tag === 'builtin') {
        return fn.fn(args);
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
  let result: Value | undefined;
  for (const expr of exprs) {
    result = evaluate(expr);
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
