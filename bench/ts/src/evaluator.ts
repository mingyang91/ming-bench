import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] }
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
    while (i < input.length && !' \t\n\r();"'.includes(input[i])) {
      atom += input[i];
      i++;
    }
    if (atom.length > 0) tokens.push(atom);
  }
  return tokens;
}

function parse(tokens: string[], pos: { i: number }): SchemeVal {
  if (pos.i >= tokens.length) {
    throw new EvalError('unexpected end of input');
  }
  const token = tokens[pos.i];
  if (token === '(') {
    pos.i++;
    const elements: SchemeVal[] = [];
    while (pos.i < tokens.length && tokens[pos.i] !== ')') {
      elements.push(parse(tokens, pos));
    }
    if (pos.i >= tokens.length) throw new EvalError('missing closing paren');
    pos.i++; // skip ')'
    return { tag: 'list', elements };
  }
  if (token === ')') {
    throw new EvalError('unexpected )');
  }
  pos.i++;
  return parseAtom(token);
}

function parseAtom(token: string): SchemeVal {
  if (token === '#t') return { tag: 'boolean', value: true };
  if (token === '#f') return { tag: 'boolean', value: false };
  if (token.startsWith('"') && token.endsWith('"')) {
    // unescape
    const inner = token.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return { tag: 'string', value: inner };
  }
  const num = Number(token);
  if (!isNaN(num) && token !== '') {
    return { tag: 'number', value: num };
  }
  return { tag: 'symbol', value: token };
}

function parseAll(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const exprs: SchemeVal[] = [];
  const pos = { i: 0 };
  while (pos.i < tokens.length) {
    exprs.push(parse(tokens, pos));
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

  const elems = expr.elements;
  if (elems.length === 0) throw new EvalError('empty application');

  const head = elems[0];

  if (head.tag === 'symbol') {
    const name = head.value;

    // Short-circuit forms (don't evaluate all args upfront)
    switch (name) {
      case 'and': {
        if (elems.length === 1) return { tag: 'boolean', value: true };
        let result: SchemeVal = { tag: 'boolean', value: true };
        for (let i = 1; i < elems.length; i++) {
          result = evaluate(elems[i]);
          if (!isTruthy(result)) return result;
        }
        return result;
      }
      case 'or': {
        if (elems.length === 1) return { tag: 'boolean', value: false };
        let result: SchemeVal = { tag: 'boolean', value: false };
        for (let i = 1; i < elems.length; i++) {
          result = evaluate(elems[i]);
          if (isTruthy(result)) return result;
        }
        return result;
      }
    }

    // Eager-evaluated builtins
    const args = elems.slice(1).map(e => evaluate(e));
    switch (name) {
      case '+': {
        let sum = 0;
        for (const a of args) sum += expectNumber(a, '+');
        return { tag: 'number', value: sum };
      }
      case '-': {
        if (args.length === 0) throw new EvalError('-: need at least 1 argument');
        if (args.length === 1) return { tag: 'number', value: -expectNumber(args[0], '-') };
        let result = expectNumber(args[0], '-');
        for (let i = 1; i < args.length; i++) result -= expectNumber(args[i], '-');
        return { tag: 'number', value: result };
      }
      case '*': {
        let product = 1;
        for (const a of args) product *= expectNumber(a, '*');
        return { tag: 'number', value: product };
      }
      case '/': {
        if (args.length < 2) throw new EvalError('/: need at least 2 arguments');
        let result = expectNumber(args[0], '/');
        for (let i = 1; i < args.length; i++) {
          const d = expectNumber(args[i], '/');
          if (d === 0) throw new EvalError('division by zero');
          result = Math.trunc(result / d);
        }
        return { tag: 'number', value: result };
      }
      case '<': {
        if (args.length !== 2) throw new EvalError('<: need 2 arguments');
        return { tag: 'boolean', value: expectNumber(args[0], '<') < expectNumber(args[1], '<') };
      }
      case '>': {
        if (args.length !== 2) throw new EvalError('>: need 2 arguments');
        return { tag: 'boolean', value: expectNumber(args[0], '>') > expectNumber(args[1], '>') };
      }
      case '=': {
        if (args.length !== 2) throw new EvalError('=: need 2 arguments');
        return { tag: 'boolean', value: expectNumber(args[0], '=') === expectNumber(args[1], '=') };
      }
      case '<=': {
        if (args.length !== 2) throw new EvalError('<=: need 2 arguments');
        return { tag: 'boolean', value: expectNumber(args[0], '<=') <= expectNumber(args[1], '<=') };
      }
      case '>=': {
        if (args.length !== 2) throw new EvalError('>=: need 2 arguments');
        return { tag: 'boolean', value: expectNumber(args[0], '>=') >= expectNumber(args[1], '>=') };
      }
      case 'not': {
        if (args.length !== 1) throw new EvalError('not: need 1 argument');
        return { tag: 'boolean', value: !isTruthy(args[0]) };
      }
    }
  }

  throw new EvalError(`not a procedure`);
}

// ── Display ────────────────────────────────────────────────────────

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
    case 'void': return '';
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr);
  }
  return displayVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  return { result: evalStr(input), output: '' };
}
