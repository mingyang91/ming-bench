import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type BuiltinFn = (args: SchemeVal[]) => SchemeVal;

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] }
  | { tag: 'void' }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env }
  | { tag: 'builtin'; name: string; fn: BuiltinFn };

// ── Environment ───────────────────────────────────────────────────

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent: Env | null = null) {}

  get(name: string): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
  }

  define(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }
}

// ── Builtins ──────────────────────────────────────────────────────

function expectNum(v: SchemeVal, op: string): number {
  if (v.tag !== 'number') throw new EvalError(`${op}: expected number`);
  return v.value;
}

function makeGlobalEnv(): Env {
  const env = new Env();

  function defBuiltin(name: string, fn: BuiltinFn) {
    env.define(name, { tag: 'builtin', name, fn });
  }

  defBuiltin('+', (args) => {
    let sum = 0;
    for (const a of args) sum += expectNum(a, '+');
    return { tag: 'number', value: sum };
  });

  defBuiltin('-', (args) => {
    if (args.length === 0) throw new EvalError('-: expected at least 1 argument');
    if (args.length === 1) return { tag: 'number', value: -expectNum(args[0], '-') };
    let result = expectNum(args[0], '-');
    for (let i = 1; i < args.length; i++) result -= expectNum(args[i], '-');
    return { tag: 'number', value: result };
  });

  defBuiltin('*', (args) => {
    let prod = 1;
    for (const a of args) prod *= expectNum(a, '*');
    return { tag: 'number', value: prod };
  });

  defBuiltin('/', (args) => {
    if (args.length !== 2) throw new EvalError('/: expected 2 arguments');
    const a = expectNum(args[0], '/'), b = expectNum(args[1], '/');
    if (b === 0) throw new EvalError('division by zero');
    return { tag: 'number', value: Math.trunc(a / b) };
  });

  for (const op of ['<', '>', '=', '>=', '<='] as const) {
    defBuiltin(op, (args) => {
      if (args.length !== 2) throw new EvalError(`${op}: expected 2 arguments`);
      const a = expectNum(args[0], op), b = expectNum(args[1], op);
      let r: boolean;
      switch (op) {
        case '<': r = a < b; break;
        case '>': r = a > b; break;
        case '=': r = a === b; break;
        case '>=': r = a >= b; break;
        case '<=': r = a <= b; break;
      }
      return { tag: 'boolean', value: r };
    });
  }

  return env;
}

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
      pos++;
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

function evalExpr(expr: SchemeVal, env: Env): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') {
    return expr;
  }

  if (expr.tag === 'symbol') {
    return env.get(expr.value);
  }

  if (expr.tag === 'list') {
    const elems = expr.elements;
    if (elems.length === 0) throw new EvalError('empty application');

    // Special forms
    if (elems[0].tag === 'symbol') {
      const op = elems[0].value;

      if (op === 'quote') {
        if (elems.length !== 2) throw new EvalError('quote: expected 1 argument');
        return elems[1];
      }

      if (op === 'if') {
        if (elems.length < 3 || elems.length > 4) throw new EvalError('if: expected 2 or 3 arguments');
        const cond = evalExpr(elems[1], env);
        if (isTruthy(cond)) {
          return evalExpr(elems[2], env);
        } else {
          if (elems.length === 4) return evalExpr(elems[3], env);
          return { tag: 'void' };
        }
      }

      if (op === 'define') {
        if (elems.length < 3) throw new EvalError('define: bad syntax');
        const target = elems[1];
        if (target.tag === 'symbol') {
          const val = evalExpr(elems[2], env);
          env.define(target.value, val);
          return { tag: 'void' };
        }
        if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
          const name = target.elements[0].value;
          const params = target.elements.slice(1).map(p => {
            if (p.tag !== 'symbol') throw new EvalError('define: parameter must be a symbol');
            return p.value;
          });
          const body = elems.slice(2);
          const lambda: SchemeVal = { tag: 'lambda', params, body, env };
          env.define(name, lambda);
          return { tag: 'void' };
        }
        throw new EvalError('define: bad syntax');
      }

      if (op === 'lambda') {
        if (elems.length < 3) throw new EvalError('lambda: bad syntax');
        const paramList = elems[1];
        if (paramList.tag !== 'list') throw new EvalError('lambda: parameters must be a list');
        const params = paramList.elements.map(p => {
          if (p.tag !== 'symbol') throw new EvalError('lambda: parameter must be a symbol');
          return p.value;
        });
        const body = elems.slice(2);
        return { tag: 'lambda', params, body, env };
      }

      if (op === 'and') {
        let result: SchemeVal = { tag: 'boolean', value: true };
        for (let i = 1; i < elems.length; i++) {
          result = evalExpr(elems[i], env);
          if (!isTruthy(result)) return result;
        }
        return result;
      }

      if (op === 'or') {
        let result: SchemeVal = { tag: 'boolean', value: false };
        for (let i = 1; i < elems.length; i++) {
          result = evalExpr(elems[i], env);
          if (isTruthy(result)) return result;
        }
        return result;
      }

      if (op === 'not') {
        if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
        const val = evalExpr(elems[1], env);
        return { tag: 'boolean', value: !isTruthy(val) };
      }
    }

    // Function application
    const func = evalExpr(elems[0], env);
    const args = elems.slice(1).map(e => evalExpr(e, env));

    if (func.tag === 'lambda') {
      if (args.length !== func.params.length) {
        throw new EvalError(`lambda: expected ${func.params.length} arguments, got ${args.length}`);
      }
      const callEnv = new Env(func.env);
      for (let i = 0; i < func.params.length; i++) {
        callEnv.define(func.params[i], args[i]);
      }
      let result: SchemeVal = { tag: 'void' };
      for (const bodyExpr of func.body) {
        result = evalExpr(bodyExpr, callEnv);
      }
      return result;
    }

    if (func.tag === 'builtin') {
      return func.fn(args);
    }

    throw new EvalError(`not a procedure: ${displayVal(func)}`);
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
    case 'void': return '';
    case 'lambda': return '#<procedure>';
    case 'builtin': return `#<builtin:${val.name}>`;
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
