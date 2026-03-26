import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; value: SchemeVal[] }
  | { tag: 'void' };

// ── Tokenizer ──────────────────────────────────────────────────────

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
    if (ch === '#') {
      if (i + 1 < input.length && (input[i + 1] === 't' || input[i + 1] === 'f')) {
        tokens.push(input.substring(i, i + 2));
        i += 2;
        continue;
      }
    }
    // atom (symbol / number)
    let atom = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      atom += input[i++];
    }
    if (atom) tokens.push(atom);
  }
  return tokens;
}

// ── Parser ─────────────────────────────────────────────────────────

function parse(tokens: string[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[pos++];
    if (tok === '(') {
      const elems: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos] !== ')') {
        elems.push(parseExpr());
      }
      if (pos >= tokens.length) throw new EvalError('missing closing paren');
      pos++; // skip ')'
      return { tag: 'list', value: elems };
    }
    if (tok === ')') throw new EvalError('unexpected )');
    // boolean
    if (tok === '#t') return { tag: 'boolean', value: true };
    if (tok === '#f') return { tag: 'boolean', value: false };
    // string
    if (tok.startsWith('"')) return { tag: 'string', value: tok.slice(1, -1) };
    // number
    const num = Number(tok);
    if (!isNaN(num) && tok !== '') return { tag: 'number', value: num };
    // symbol
    return { tag: 'symbol', value: tok };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(v: SchemeVal): boolean {
  return !(v.tag === 'boolean' && v.value === false);
}

function toNumber(v: SchemeVal, op: string): number {
  if (v.tag !== 'number') throw new EvalError(`${op}: expected number`);
  return v.value;
}

function evalExpr(expr: SchemeVal): SchemeVal {
  if (expr.tag !== 'list') {
    // self-evaluating
    if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') return expr;
    if (expr.tag === 'symbol') throw new EvalError(`unbound variable: ${expr.value}`);
    return expr;
  }

  const elems = expr.value;
  if (elems.length === 0) throw new EvalError('empty application');

  const head = elems[0];

  // Special forms: and, or, not
  if (head.tag === 'symbol') {
    if (head.value === 'and') {
      if (elems.length === 1) return { tag: 'boolean', value: true };
      let result: SchemeVal = { tag: 'boolean', value: true };
      for (let i = 1; i < elems.length; i++) {
        result = evalExpr(elems[i]);
        if (!isTruthy(result)) return result;
      }
      return result;
    }
    if (head.value === 'or') {
      if (elems.length === 1) return { tag: 'boolean', value: false };
      let result: SchemeVal = { tag: 'boolean', value: false };
      for (let i = 1; i < elems.length; i++) {
        result = evalExpr(elems[i]);
        if (isTruthy(result)) return result;
      }
      return result;
    }
    if (head.value === 'not') {
      if (elems.length !== 2) throw new EvalError('not: wrong number of arguments');
      const val = evalExpr(elems[1]);
      return { tag: 'boolean', value: !isTruthy(val) };
    }
  }

  // Function application
  const op = evalExpr(head);
  if (op.tag !== 'symbol') throw new EvalError('not a procedure');
  // This shouldn't happen at L1 since symbols would error above.
  // For builtins, head is a symbol - handle directly:
  throw new EvalError(`not a procedure: ${display(op)}`);
}

function evalBuiltin(name: string, args: SchemeVal[]): SchemeVal {
  switch (name) {
    case '+': {
      let sum = 0;
      for (const a of args) sum += toNumber(a, '+');
      return { tag: 'number', value: sum };
    }
    case '-': {
      if (args.length === 0) throw new EvalError('-: need at least 1 argument');
      if (args.length === 1) return { tag: 'number', value: -toNumber(args[0], '-') };
      let result = toNumber(args[0], '-');
      for (let i = 1; i < args.length; i++) result -= toNumber(args[i], '-');
      return { tag: 'number', value: result };
    }
    case '*': {
      let prod = 1;
      for (const a of args) prod *= toNumber(a, '*');
      return { tag: 'number', value: prod };
    }
    case '/': {
      if (args.length < 2) throw new EvalError('/: need at least 2 arguments');
      let result = toNumber(args[0], '/');
      for (let i = 1; i < args.length; i++) {
        const d = toNumber(args[i], '/');
        if (d === 0) throw new EvalError('division by zero');
        result = Math.trunc(result / d);
      }
      return { tag: 'number', value: result };
    }
    case '<': {
      if (args.length !== 2) throw new EvalError('<: need 2 arguments');
      return { tag: 'boolean', value: toNumber(args[0], '<') < toNumber(args[1], '<') };
    }
    case '>': {
      if (args.length !== 2) throw new EvalError('>: need 2 arguments');
      return { tag: 'boolean', value: toNumber(args[0], '>') > toNumber(args[1], '>') };
    }
    case '=': {
      if (args.length !== 2) throw new EvalError('=: need 2 arguments');
      return { tag: 'boolean', value: toNumber(args[0], '=') === toNumber(args[1], '=') };
    }
    case '<=': {
      if (args.length !== 2) throw new EvalError('<=: need 2 arguments');
      return { tag: 'boolean', value: toNumber(args[0], '<=') <= toNumber(args[1], '<=') };
    }
    default:
      throw new EvalError(`unbound variable: ${name}`);
  }
}

// Revise evalExpr to handle builtins properly
function evaluate(expr: SchemeVal): SchemeVal {
  if (expr.tag !== 'list') {
    if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') return expr;
    if (expr.tag === 'symbol') throw new EvalError(`unbound variable: ${expr.value}`);
    return expr;
  }

  const elems = expr.value;
  if (elems.length === 0) throw new EvalError('empty application');

  const head = elems[0];

  // Special forms
  if (head.tag === 'symbol') {
    if (head.value === 'and') {
      if (elems.length === 1) return { tag: 'boolean', value: true };
      let result: SchemeVal = { tag: 'boolean', value: true };
      for (let i = 1; i < elems.length; i++) {
        result = evaluate(elems[i]);
        if (!isTruthy(result)) return result;
      }
      return result;
    }
    if (head.value === 'or') {
      if (elems.length === 1) return { tag: 'boolean', value: false };
      let result: SchemeVal = { tag: 'boolean', value: false };
      for (let i = 1; i < elems.length; i++) {
        result = evaluate(elems[i]);
        if (isTruthy(result)) return result;
      }
      return result;
    }
    if (head.value === 'not') {
      if (elems.length !== 2) throw new EvalError('not: wrong number of arguments');
      const val = evaluate(elems[1]);
      return { tag: 'boolean', value: !isTruthy(val) };
    }

    // Builtin function call
    const builtins = new Set(['+', '-', '*', '/', '<', '>', '=', '<=']);
    if (builtins.has(head.value)) {
      const args = elems.slice(1).map(e => evaluate(e));
      return evalBuiltin(head.value, args);
    }
  }

  throw new EvalError(`not a procedure`);
}

// ── Display ────────────────────────────────────────────────────────

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'void': return '';
    case 'list': return `(${val.value.map(display).join(' ')})`;
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr);
  }
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const result = evalStr(input);
  return { result, output: '' };
}
