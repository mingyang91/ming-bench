import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] };

// ── Parser ─────────────────────────────────────────────────────────

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }
    if (/\s/.test(ch)) { i++; continue; }
    if (ch === '(' || ch === ')') { tokens.push(ch); i++; continue; }
    if (ch === '\'') { tokens.push("'"); i++; continue; }
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i++]; }
        s += input[i++];
      }
      s += '"';
      i++; // closing quote
      tokens.push(s);
      continue;
    }
    if (ch === '#') {
      if (input[i + 1] === 't' && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        tokens.push('#t'); i += 2; continue;
      }
      if (input[i + 1] === 'f' && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        tokens.push('#f'); i += 2; continue;
      }
    }
    // symbol or number
    let tok = '';
    while (i < input.length && !/[\s()]/.test(input[i])) {
      tok += input[i++];
    }
    tokens.push(tok);
  }
  return tokens;
}

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
      return { tag: 'list', elements: elems };
    }
    if (tok === ')') throw new EvalError('unexpected )');
    if (tok === "'") {
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote' }, inner] };
    }
    return parseAtom(tok);
  }

  function parseAtom(tok: string): SchemeVal {
    if (tok === '#t') return { tag: 'boolean', value: true };
    if (tok === '#f') return { tag: 'boolean', value: false };
    if (tok.startsWith('"')) return { tag: 'string', value: tok.slice(1, -1) };
    if (/^-?\d+$/.test(tok)) return { tag: 'number', value: parseInt(tok, 10) };
    return { tag: 'symbol', value: tok };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function evalExpr(expr: SchemeVal): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') {
    return expr;
  }

  if (expr.tag === 'symbol') {
    throw new EvalError(`unbound variable: ${expr.value}`);
  }

  if (expr.tag === 'list') {
    const elems = expr.elements;
    if (elems.length === 0) throw new EvalError('empty application');

    // Special forms
    if (elems[0].tag === 'symbol') {
      const op = elems[0].value;

      if (op === 'and') {
        let result: SchemeVal = { tag: 'boolean', value: true };
        for (let i = 1; i < elems.length; i++) {
          result = evalExpr(elems[i]);
          if (!isTruthy(result)) return result;
        }
        return result;
      }

      if (op === 'or') {
        let result: SchemeVal = { tag: 'boolean', value: false };
        for (let i = 1; i < elems.length; i++) {
          result = evalExpr(elems[i]);
          if (isTruthy(result)) return result;
        }
        return result;
      }

      if (op === 'not') {
        if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
        const val = evalExpr(elems[1]);
        return { tag: 'boolean', value: !isTruthy(val) };
      }
    }

    // Function application (builtins)
    const func = elems[0].tag === 'symbol' ? elems[0].value : null;
    const args = elems.slice(1).map(e => evalExpr(e));

    if (func === '+') {
      let sum = 0;
      for (const a of args) {
        if (a.tag !== 'number') throw new EvalError('+: expected number');
        sum += a.value;
      }
      return { tag: 'number', value: sum };
    }

    if (func === '-') {
      if (args.length === 0) throw new EvalError('-: expected at least 1 argument');
      if (args[0].tag !== 'number') throw new EvalError('-: expected number');
      if (args.length === 1) return { tag: 'number', value: -args[0].value };
      let result = args[0].value;
      for (let i = 1; i < args.length; i++) {
        const arg = args[i];
        if (arg.tag !== 'number') throw new EvalError('-: expected number');
        result -= arg.value;
      }
      return { tag: 'number', value: result };
    }

    if (func === '*') {
      let prod = 1;
      for (const a of args) {
        if (a.tag !== 'number') throw new EvalError('*: expected number');
        prod *= a.value;
      }
      return { tag: 'number', value: prod };
    }

    if (func === '/') {
      if (args.length !== 2) throw new EvalError('/: expected 2 arguments');
      if (args[0].tag !== 'number' || args[1].tag !== 'number') throw new EvalError('/: expected number');
      if (args[1].value === 0) throw new EvalError('division by zero');
      return { tag: 'number', value: Math.trunc(args[0].value / args[1].value) };
    }

    if (func === '<' || func === '>' || func === '=' || func === '<=') {
      if (args.length !== 2) throw new EvalError(`${func}: expected 2 arguments`);
      if (args[0].tag !== 'number' || args[1].tag !== 'number') throw new EvalError(`${func}: expected number`);
      const a = args[0].value, b = args[1].value;
      let result: boolean;
      switch (func) {
        case '<': result = a < b; break;
        case '>': result = a > b; break;
        case '=': result = a === b; break;
        case '<=': result = a <= b; break;
        default: result = false;
      }
      return { tag: 'boolean', value: result };
    }

    throw new EvalError(`unknown procedure: ${func ?? 'non-symbol'}`);
  }

  throw new EvalError('cannot evaluate');
}

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr);
  }
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
