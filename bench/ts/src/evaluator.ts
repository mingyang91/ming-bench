import { EvalError } from './evalError.js';

// --- Types ---

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] }
  | { tag: 'builtin'; name: string; func: (args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' };

// --- Parser ---

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { i++; continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') i++; continue; }
    if (ch === '(' || ch === ')') { tokens.push(ch); i++; continue; }
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; i++; if (i < input.length) { s += input[i]; i++; } }
        else { s += input[i]; i++; }
      }
      if (i < input.length) { s += '"'; i++; }
      tokens.push(s);
      continue;
    }
    if (ch === "'") { tokens.push("'"); i++; continue; }
    let atom = '';
    while (i < input.length && !/[\s();]/.test(input[i])) { atom += input[i]; i++; }
    if (atom.length > 0) tokens.push(atom);
  }
  return tokens;
}

function parse(tokens: string[], pos: number): [SchemeVal, number] {
  if (pos >= tokens.length) throw new EvalError('unexpected end of input');
  const token = tokens[pos];

  if (token === '(') {
    const elements: SchemeVal[] = [];
    pos++;
    while (pos < tokens.length && tokens[pos] !== ')') {
      const [val, next] = parse(tokens, pos);
      elements.push(val);
      pos = next;
    }
    if (pos >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'list', elements }, pos + 1];
  }

  if (token === ')') throw new EvalError('unexpected )');

  if (token === "'") {
    const [val, next] = parse(tokens, pos + 1);
    return [{ tag: 'list', elements: [{ tag: 'symbol', value: 'quote' }, val] }, next];
  }

  if (token === '#t') return [{ tag: 'boolean', value: true }, pos + 1];
  if (token === '#f') return [{ tag: 'boolean', value: false }, pos + 1];

  if (token.startsWith('"')) {
    const inner = token.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\t/g, '\t')
      .replace(/\\"/g, '"')
      .replace(/\\\\/g, '\\');
    return [{ tag: 'string', value: inner }, pos + 1];
  }

  const num = Number(token);
  if (!isNaN(num) && token !== '') {
    return [{ tag: 'number', value: num }, pos + 1];
  }

  return [{ tag: 'symbol', value: token }, pos + 1];
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
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(schemeToString).join(' ')})`;
    case 'builtin': return `#<procedure:${val.name}>`;
    case 'void': return '';
  }
}

function expectNumbers(args: SchemeVal[], name: string): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number, got ${schemeToString(a)}`);
    return a.value;
  });
}

// --- Env ---

type Env = Map<string, SchemeVal>;

function makeGlobalEnv(): Env {
  const env: Env = new Map();

  const defBuiltin = (name: string, func: (args: SchemeVal[]) => SchemeVal) => {
    env.set(name, { tag: 'builtin', name, func });
  };

  defBuiltin('+', (args) => {
    const nums = expectNumbers(args, '+');
    return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
  });

  defBuiltin('-', (args) => {
    if (args.length === 0) throw new EvalError('-: need at least 1 arg');
    const nums = expectNumbers(args, '-');
    if (nums.length === 1) return { tag: 'number', value: -nums[0] };
    return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  });

  defBuiltin('*', (args) => {
    const nums = expectNumbers(args, '*');
    return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
  });

  defBuiltin('/', (args) => {
    if (args.length !== 2) throw new EvalError('/: expected 2 args');
    const nums = expectNumbers(args, '/');
    if (nums[1] === 0) throw new EvalError('division by zero');
    return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
  });

  defBuiltin('<', (args) => {
    if (args.length !== 2) throw new EvalError('<: expected 2 args');
    const nums = expectNumbers(args, '<');
    return { tag: 'boolean', value: nums[0] < nums[1] };
  });

  defBuiltin('>', (args) => {
    if (args.length !== 2) throw new EvalError('>: expected 2 args');
    const nums = expectNumbers(args, '>');
    return { tag: 'boolean', value: nums[0] > nums[1] };
  });

  defBuiltin('=', (args) => {
    if (args.length !== 2) throw new EvalError('=: expected 2 args');
    const nums = expectNumbers(args, '=');
    return { tag: 'boolean', value: nums[0] === nums[1] };
  });

  defBuiltin('<=', (args) => {
    if (args.length !== 2) throw new EvalError('<=: expected 2 args');
    const nums = expectNumbers(args, '<=');
    return { tag: 'boolean', value: nums[0] <= nums[1] };
  });

  return env;
}

// --- Eval ---

function evalScheme(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;

    case 'symbol': {
      const val = env.get(expr.value);
      if (val === undefined) throw new EvalError(`unbound variable: ${expr.value}`);
      return val;
    }

    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw new EvalError('empty application');

      if (elems[0].tag === 'symbol') {
        const name = elems[0].value;

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
          if (elems.length !== 2) throw new EvalError('not: wrong argument count');
          const val = evalScheme(elems[1], env);
          return { tag: 'boolean', value: !isTruthy(val) };
        }
      }

      // Function application
      const func = evalScheme(elems[0], env);
      const args = elems.slice(1).map(a => evalScheme(a, env));

      if (func.tag === 'builtin') {
        return func.func(args);
      }

      throw new EvalError(`not a procedure: ${schemeToString(func)}`);
    }

    case 'builtin':
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
  const env = makeGlobalEnv();
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
  throw new EvalError('not implemented');
}
