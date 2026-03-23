import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; value: SchemeVal[] }
  | { tag: 'void' };

// ── Parser ─────────────────────────────────────────────────────────

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
    // string
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
    while (i < input.length && !('() \t\n\r;'.includes(input[i]))) {
      atom += input[i];
      i++;
    }
    if (atom.length > 0) tokens.push(atom);
  }
  return tokens;
}

function parseTokens(tokens: string[], pos: number): [SchemeVal, number] {
  if (pos >= tokens.length) throw new EvalError('unexpected end of input');
  const tok = tokens[pos];
  if (tok === '(') {
    const items: SchemeVal[] = [];
    pos++;
    while (pos < tokens.length && tokens[pos] !== ')') {
      const [val, next] = parseTokens(tokens, pos);
      items.push(val);
      pos = next;
    }
    if (pos >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'list', value: items }, pos + 1];
  }
  if (tok === ')') throw new EvalError('unexpected )');
  return [parseAtom(tok), pos + 1];
}

function parseAtom(tok: string): SchemeVal {
  if (tok === '#t') return { tag: 'boolean', value: true };
  if (tok === '#f') return { tag: 'boolean', value: false };
  if (tok.startsWith('"') && tok.endsWith('"')) {
    // unescape
    const inner = tok.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return { tag: 'string', value: inner };
  }
  const num = Number(tok);
  if (!isNaN(num) && tok !== '') {
    return { tag: 'number', value: num };
  }
  return { tag: 'symbol', value: tok };
}

function parse(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const exprs: SchemeVal[] = [];
  let pos = 0;
  while (pos < tokens.length) {
    const [val, next] = parseTokens(tokens, pos);
    exprs.push(val);
    pos = next;
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function expectNumber(val: SchemeVal, op: string): number {
  if (val.tag !== 'number') throw new EvalError(`${op}: expected number`);
  return val.value;
}

function evaluate(expr: SchemeVal): SchemeVal {
  if (expr.tag !== 'list') {
    if (expr.tag === 'symbol') {
      throw new EvalError(`unbound variable: ${expr.value}`);
    }
    return expr; // self-evaluating: number, boolean, string
  }

  const items = expr.value;
  if (items.length === 0) throw new EvalError('empty application');

  const head = items[0];
  if (head.tag === 'symbol') {
    const op = head.value;

    // Arithmetic
    if (op === '+') {
      let sum = 0;
      for (let i = 1; i < items.length; i++) {
        sum += expectNumber(evaluate(items[i]), '+');
      }
      return { tag: 'number', value: sum };
    }
    if (op === '-') {
      if (items.length < 2) throw new EvalError('-: need at least 1 argument');
      if (items.length === 2) {
        return { tag: 'number', value: -expectNumber(evaluate(items[1]), '-') };
      }
      let result = expectNumber(evaluate(items[1]), '-');
      for (let i = 2; i < items.length; i++) {
        result -= expectNumber(evaluate(items[i]), '-');
      }
      return { tag: 'number', value: result };
    }
    if (op === '*') {
      let prod = 1;
      for (let i = 1; i < items.length; i++) {
        prod *= expectNumber(evaluate(items[i]), '*');
      }
      return { tag: 'number', value: prod };
    }
    if (op === '/') {
      if (items.length < 3) throw new EvalError('/: need at least 2 arguments');
      let result = expectNumber(evaluate(items[1]), '/');
      for (let i = 2; i < items.length; i++) {
        const d = expectNumber(evaluate(items[i]), '/');
        if (d === 0) throw new EvalError('division by zero');
        result = Math.trunc(result / d);
      }
      return { tag: 'number', value: result };
    }

    // Comparisons
    if (op === '<') {
      const a = expectNumber(evaluate(items[1]), '<');
      const b = expectNumber(evaluate(items[2]), '<');
      return { tag: 'boolean', value: a < b };
    }
    if (op === '>') {
      const a = expectNumber(evaluate(items[1]), '>');
      const b = expectNumber(evaluate(items[2]), '>');
      return { tag: 'boolean', value: a > b };
    }
    if (op === '=') {
      const a = expectNumber(evaluate(items[1]), '=');
      const b = expectNumber(evaluate(items[2]), '=');
      return { tag: 'boolean', value: a === b };
    }
    if (op === '<=') {
      const a = expectNumber(evaluate(items[1]), '<=');
      const b = expectNumber(evaluate(items[2]), '<=');
      return { tag: 'boolean', value: a <= b };
    }
    if (op === '>=') {
      const a = expectNumber(evaluate(items[1]), '>=');
      const b = expectNumber(evaluate(items[2]), '>=');
      return { tag: 'boolean', value: a >= b };
    }

    // Logic
    if (op === 'not') {
      const val = evaluate(items[1]);
      return { tag: 'boolean', value: !isTruthy(val) };
    }
    if (op === 'and') {
      let result: SchemeVal = { tag: 'boolean', value: true };
      for (let i = 1; i < items.length; i++) {
        result = evaluate(items[i]);
        if (!isTruthy(result)) return result;
      }
      return result;
    }
    if (op === 'or') {
      let result: SchemeVal = { tag: 'boolean', value: false };
      for (let i = 1; i < items.length; i++) {
        result = evaluate(items[i]);
        if (isTruthy(result)) return result;
      }
      return result;
    }

    throw new EvalError(`unknown procedure: ${op}`);
  }

  throw new EvalError('not a procedure');
}

// ── Display ────────────────────────────────────────────────────────

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.value.map(display).join(' ')})`;
    case 'void': return '';
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('empty input');
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr);
  }
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
