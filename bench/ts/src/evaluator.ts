import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; val: number }
  | { tag: 'boolean'; val: boolean }
  | { tag: 'string'; val: string }
  | { tag: 'symbol'; val: string }
  | { tag: 'list'; val: SchemeVal[] };

// ── Parser ─────────────────────────────────────────────────────────

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    // skip whitespace
    if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
      i++;
      continue;
    }
    // skip comments
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }
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

function parseTokens(tokens: string[], pos: number): [SchemeVal, number] {
  if (pos >= tokens.length) {
    throw new EvalError('unexpected end of input');
  }
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
    return [{ tag: 'list', val: items }, pos + 1];
  }
  if (tok === ')') {
    throw new EvalError('unexpected )');
  }
  return [parseAtom(tok), pos + 1];
}

function parseAtom(tok: string): SchemeVal {
  if (tok === '#t') return { tag: 'boolean', val: true };
  if (tok === '#f') return { tag: 'boolean', val: false };
  if (tok.startsWith('"') && tok.endsWith('"')) {
    // unescape
    const inner = tok.slice(1, -1).replace(/\\(.)/g, (_, c) => {
      if (c === 'n') return '\n';
      if (c === 't') return '\t';
      if (c === '\\') return '\\';
      if (c === '"') return '"';
      return c;
    });
    return { tag: 'string', val: inner };
  }
  // number (including negatives like -7)
  if (/^-?\d+$/.test(tok)) {
    return { tag: 'number', val: parseInt(tok, 10) };
  }
  return { tag: 'symbol', val: tok };
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

function isTruthy(v: SchemeVal): boolean {
  return !(v.tag === 'boolean' && v.val === false);
}

function toNumber(v: SchemeVal, op: string): number {
  if (v.tag !== 'number') throw new EvalError(`${op}: expected number`);
  return v.val;
}

function display(v: SchemeVal): string {
  switch (v.tag) {
    case 'number':
      return String(v.val);
    case 'boolean':
      return v.val ? '#t' : '#f';
    case 'string':
      return `"${v.val}"`;
    case 'symbol':
      return v.val;
    case 'list':
      return '(' + v.val.map(display).join(' ') + ')';
  }
}

function evalExpr(expr: SchemeVal): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') {
    return expr;
  }
  if (expr.tag === 'symbol') {
    throw new EvalError(`unbound variable: ${expr.val}`);
  }
  if (expr.tag === 'list') {
    const items = expr.val;
    if (items.length === 0) throw new EvalError('empty application');
    const head = items[0];
    if (head.tag !== 'symbol') throw new EvalError('expected operator');
    const op = head.val;
    const args = items.slice(1);

    // special forms
    if (op === 'and') {
      if (args.length === 0) return { tag: 'boolean', val: true };
      let result: SchemeVal = { tag: 'boolean', val: true };
      for (const a of args) {
        result = evalExpr(a);
        if (!isTruthy(result)) return result;
      }
      return result;
    }
    if (op === 'or') {
      if (args.length === 0) return { tag: 'boolean', val: false };
      let result: SchemeVal = { tag: 'boolean', val: false };
      for (const a of args) {
        result = evalExpr(a);
        if (isTruthy(result)) return result;
      }
      return result;
    }

    // evaluate arguments
    const evalArgs = args.map(a => evalExpr(a));

    switch (op) {
      case '+': {
        let sum = 0;
        for (const a of evalArgs) sum += toNumber(a, '+');
        return { tag: 'number', val: sum };
      }
      case '-': {
        if (evalArgs.length === 0) throw new EvalError('-: need at least one arg');
        if (evalArgs.length === 1) return { tag: 'number', val: -toNumber(evalArgs[0], '-') };
        let result = toNumber(evalArgs[0], '-');
        for (let i = 1; i < evalArgs.length; i++) result -= toNumber(evalArgs[i], '-');
        return { tag: 'number', val: result };
      }
      case '*': {
        let prod = 1;
        for (const a of evalArgs) prod *= toNumber(a, '*');
        return { tag: 'number', val: prod };
      }
      case '/': {
        if (evalArgs.length < 2) throw new EvalError('/: need at least two args');
        let result = toNumber(evalArgs[0], '/');
        for (let i = 1; i < evalArgs.length; i++) {
          const d = toNumber(evalArgs[i], '/');
          if (d === 0) throw new EvalError('division by zero');
          result = Math.trunc(result / d);
        }
        return { tag: 'number', val: result };
      }
      case '<': {
        if (evalArgs.length !== 2) throw new EvalError('<: need exactly two args');
        return { tag: 'boolean', val: toNumber(evalArgs[0], '<') < toNumber(evalArgs[1], '<') };
      }
      case '>': {
        if (evalArgs.length !== 2) throw new EvalError('>: need exactly two args');
        return { tag: 'boolean', val: toNumber(evalArgs[0], '>') > toNumber(evalArgs[1], '>') };
      }
      case '=': {
        if (evalArgs.length !== 2) throw new EvalError('=: need exactly two args');
        return { tag: 'boolean', val: toNumber(evalArgs[0], '=') === toNumber(evalArgs[1], '=') };
      }
      case '<=': {
        if (evalArgs.length !== 2) throw new EvalError('<=: need exactly two args');
        return { tag: 'boolean', val: toNumber(evalArgs[0], '<=') <= toNumber(evalArgs[1], '<=') };
      }
      case '>=': {
        if (evalArgs.length !== 2) throw new EvalError('>=: need exactly two args');
        return { tag: 'boolean', val: toNumber(evalArgs[0], '>=') >= toNumber(evalArgs[1], '>=') };
      }
      case 'not': {
        if (evalArgs.length !== 1) throw new EvalError('not: need exactly one arg');
        return { tag: 'boolean', val: !isTruthy(evalArgs[0]) };
      }
      default:
        throw new EvalError(`unknown procedure: ${op}`);
    }
  }
  throw new EvalError('cannot evaluate');
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr);
  }
  return display(result!);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const result = evalStr(input);
  return { result, output: '' };
}
