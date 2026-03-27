import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; val: number }
  | { tag: 'boolean'; val: boolean }
  | { tag: 'string'; val: string }
  | { tag: 'symbol'; val: string }
  | { tag: 'list'; val: SchemeVal[] }
  | { tag: 'procedure'; val: (args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' };

// ── Parser ─────────────────────────────────────────────────────────────

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    // whitespace
    if (/\s/.test(ch)) { i++; continue; }
    // comment
    if (ch === ';') { while (i < input.length && input[i] !== '\n') i++; continue; }
    // parens
    if (ch === '(' || ch === ')') { tokens.push(ch); i++; continue; }
    // string literal
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i++]; }
        s += input[i++];
      }
      if (i < input.length) { s += '"'; i++; }
      tokens.push(s);
      continue;
    }
    // #t, #f
    if (ch === '#' && i + 1 < input.length) {
      if (input[i + 1] === 't') { tokens.push('#t'); i += 2; continue; }
      if (input[i + 1] === 'f') { tokens.push('#f'); i += 2; continue; }
    }
    // atom
    let atom = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      atom += input[i++];
    }
    tokens.push(atom);
  }
  return tokens;
}

function parse(tokens: string[], pos: { i: number }): SchemeVal {
  if (pos.i >= tokens.length) throw new EvalError('unexpected end of input');
  const tok = tokens[pos.i++];

  if (tok === '(') {
    const elems: SchemeVal[] = [];
    while (pos.i < tokens.length && tokens[pos.i] !== ')') {
      elems.push(parse(tokens, pos));
    }
    if (pos.i >= tokens.length) throw new EvalError('missing closing paren');
    pos.i++; // skip ')'
    return { tag: 'list', val: elems };
  }
  if (tok === ')') throw new EvalError('unexpected )');
  return parseAtom(tok);
}

function parseAtom(tok: string): SchemeVal {
  if (tok === '#t') return { tag: 'boolean', val: true };
  if (tok === '#f') return { tag: 'boolean', val: false };
  if (tok.startsWith('"')) return { tag: 'string', val: tok.slice(1, -1).replace(/\\"/g, '"').replace(/\\\\/g, '\\') };
  const n = Number(tok);
  if (!isNaN(n) && tok !== '') return { tag: 'number', val: n };
  return { tag: 'symbol', val: tok };
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

type Env = Map<string, SchemeVal>;

function makeGlobalEnv(): Env {
  const env: Env = new Map();

  const numBinop = (fn: (a: number, b: number) => number | boolean) =>
    ({ tag: 'procedure' as const, val: (args: SchemeVal[]) => {
      for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
      const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
      const r = nums.reduce((acc, v) => fn(acc, v) as number);
      return typeof r === 'number' ? { tag: 'number' as const, val: r } : { tag: 'boolean' as const, val: r as boolean };
    }});

  // Arithmetic
  env.set('+', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    return { tag: 'number', val: nums.reduce((a, b) => a + b, 0) };
  }});

  env.set('*', { tag: 'procedure', val: (args) => {
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    return { tag: 'number', val: nums.reduce((a, b) => a * b, 1) };
  }});

  env.set('-', { tag: 'procedure', val: (args) => {
    if (args.length === 0) throw new EvalError('- requires at least 1 argument');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    if (nums.length === 1) return { tag: 'number', val: -nums[0] };
    return { tag: 'number', val: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  }});

  env.set('/', { tag: 'procedure', val: (args) => {
    if (args.length < 2) throw new EvalError('/ requires at least 2 arguments');
    for (const a of args) if (a.tag !== 'number') throw new EvalError('expected number');
    const nums = args.map(a => (a as { tag: 'number'; val: number }).val);
    return { tag: 'number', val: nums.slice(1).reduce((a, b) => {
      if (b === 0) throw new EvalError('division by zero');
      return Math.trunc(a / b);
    }, nums[0]) };
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

  env.set('<', numCmp((a, b) => a < b));
  env.set('>', numCmp((a, b) => a > b));
  env.set('=', numCmp((a, b) => a === b));
  env.set('<=', numCmp((a, b) => a <= b));
  env.set('>=', numCmp((a, b) => a >= b));

  // not
  env.set('not', { tag: 'procedure', val: (args) => {
    if (args.length !== 1) throw new EvalError('not requires 1 argument');
    return { tag: 'boolean', val: isFalsy(args[0]) };
  }});

  return env;
}

function isFalsy(v: SchemeVal): boolean {
  return v.tag === 'boolean' && v.val === false;
}

function isTruthy(v: SchemeVal): boolean {
  return !isFalsy(v);
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;

    case 'symbol': {
      const val = env.get(expr.val);
      if (val === undefined) throw new EvalError(`unbound variable: ${expr.val}`);
      return val;
    }

    case 'list': {
      const elems = expr.val;
      if (elems.length === 0) throw new EvalError('empty application');

      // Special forms
      if (elems[0].tag === 'symbol') {
        const name = elems[0].val;

        if (name === 'and') {
          if (elems.length === 1) return { tag: 'boolean', val: true };
          let result: SchemeVal = { tag: 'boolean', val: true };
          for (let i = 1; i < elems.length; i++) {
            result = evaluate(elems[i], env);
            if (isFalsy(result)) return result;
          }
          return result;
        }

        if (name === 'or') {
          if (elems.length === 1) return { tag: 'boolean', val: false };
          let result: SchemeVal = { tag: 'boolean', val: false };
          for (let i = 1; i < elems.length; i++) {
            result = evaluate(elems[i], env);
            if (isTruthy(result)) return result;
          }
          return result;
        }
      }

      // Procedure application
      const proc = evaluate(elems[0], env);
      if (proc.tag !== 'procedure') throw new EvalError('not a procedure');
      const args = elems.slice(1).map(a => evaluate(a, env));
      return proc.val(args);
    }

    default:
      throw new EvalError('cannot evaluate');
  }
}

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.val);
    case 'boolean': return val.val ? '#t' : '#f';
    case 'string': return `"${val.val}"`;
    case 'symbol': return val.val;
    case 'list': return `(${val.val.map(display).join(' ')})`;
    case 'void': return '';
    case 'procedure': return '#<procedure>';
  }
}

// ── Public API ─────────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return { result: display(result), output: '' };
}
