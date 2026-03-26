import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env };

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent?: Env) {}

  get(name: string): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
  }

  set(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }
}

// ── Tokenizer ──────────────────────────────────────────────────────────

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { i++; continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') i++; continue; }
    if (ch === '(' || ch === ')') { tokens.push(ch); i++; continue; }
    if (ch === '\'') { tokens.push("'"); i++; continue; }
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
    let tok = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      tok += input[i++];
    }
    tokens.push(tok);
  }
  return tokens;
}

// ── Parser ─────────────────────────────────────────────────────────────

function parse(tokens: string[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[pos++];
    if (tok === "'") {
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote' }, inner] };
    }
    if (tok === '(') {
      const elements: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos] !== ')') {
        elements.push(parseExpr());
      }
      if (pos >= tokens.length) throw new EvalError('missing closing parenthesis');
      pos++;
      return { tag: 'list', elements };
    }
    if (tok === ')') throw new EvalError('unexpected )');
    return parseAtom(tok);
  }

  function parseAtom(tok: string): SchemeVal {
    if (tok === '#t') return { tag: 'boolean', value: true };
    if (tok === '#f') return { tag: 'boolean', value: false };
    if (tok.startsWith('"') && tok.endsWith('"')) {
      return { tag: 'string', value: tok.slice(1, -1) };
    }
    const num = Number(tok);
    if (!isNaN(num) && tok !== '') {
      return { tag: 'number', value: num };
    }
    return { tag: 'symbol', value: tok };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────────

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function evalExpr(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      return env.get(expr.value);
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw new EvalError('empty application');

      const head = elems[0];

      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'quote': {
            if (elems.length !== 2) throw new EvalError('quote: expected 1 argument');
            return elems[1];
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4)
              throw new EvalError('if: expected 2-3 arguments');
            const cond = evalExpr(elems[1], env);
            if (isTruthy(cond)) return evalExpr(elems[2], env);
            if (elems.length === 4) return evalExpr(elems[3], env);
            return { tag: 'boolean', value: false }; // unspecified
          }
          case 'define': {
            if (elems.length < 3) throw new EvalError('define: bad syntax');
            const target = elems[1];
            if (target.tag === 'symbol') {
              // (define x expr)
              const val = evalExpr(elems[2], env);
              env.set(target.value, val);
              return val;
            }
            if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
              // (define (f params...) body...)
              const name = target.elements[0].value;
              const params = target.elements.slice(1).map(p => {
                if (p.tag !== 'symbol') throw new EvalError('define: param must be symbol');
                return p.value;
              });
              const body = elems.slice(2);
              const lambda: SchemeVal = { tag: 'lambda', params, body, env };
              env.set(name, lambda);
              return lambda;
            }
            throw new EvalError('define: bad syntax');
          }
          case 'lambda': {
            if (elems.length < 3) throw new EvalError('lambda: bad syntax');
            const paramList = elems[1];
            if (paramList.tag !== 'list') throw new EvalError('lambda: params must be a list');
            const params = paramList.elements.map(p => {
              if (p.tag !== 'symbol') throw new EvalError('lambda: param must be symbol');
              return p.value;
            });
            const body = elems.slice(2);
            return { tag: 'lambda', params, body, env };
          }
          case 'and': {
            let result: SchemeVal = { tag: 'boolean', value: true };
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
              if (!isTruthy(result)) return result;
            }
            return result;
          }
          case 'or': {
            let result: SchemeVal = { tag: 'boolean', value: false };
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
              if (isTruthy(result)) return result;
            }
            return result;
          }
          case 'not': {
            if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
            const val = evalExpr(elems[1], env);
            return { tag: 'boolean', value: !isTruthy(val) };
          }
        }
      }

      // Function application
      const args = elems.slice(1).map(a => evalExpr(a, env));

      // Try builtin first for bare symbols
      if (head.tag === 'symbol' && isBuiltin(head.value)) {
        return applyBuiltin(head.value, args);
      }

      const proc = evalExpr(head, env);

      if (proc.tag === 'lambda') {
        if (args.length !== proc.params.length)
          throw new EvalError(`expected ${proc.params.length} arguments, got ${args.length}`);
        const callEnv = new Env(proc.env);
        for (let i = 0; i < proc.params.length; i++) {
          callEnv.set(proc.params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'boolean', value: false };
        for (const bodyExpr of proc.body) {
          result = evalExpr(bodyExpr, callEnv);
        }
        return result;
      }

      throw new EvalError(`not a procedure: ${display(proc)}`);
    }
    default:
      throw new EvalError(`cannot evaluate: ${display(expr)}`);
  }
}

function requireNumbers(name: string, args: SchemeVal[]): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number`);
    return a.value;
  });
}

const BUILTINS = new Set(['+', '-', '*', '/', '<', '>', '=', '<=', '>=']);

function isBuiltin(name: string): boolean {
  return BUILTINS.has(name);
}

function applyBuiltin(name: string, args: SchemeVal[]): SchemeVal {
  switch (name) {
    case '+': {
      const nums = requireNumbers('+', args);
      return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
    }
    case '-': {
      if (args.length === 0) throw new EvalError('-: expected at least 1 argument');
      const nums = requireNumbers('-', args);
      if (nums.length === 1) return { tag: 'number', value: -nums[0] };
      return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
    }
    case '*': {
      const nums = requireNumbers('*', args);
      return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
    }
    case '/': {
      if (args.length < 2) throw new EvalError('/: expected at least 2 arguments');
      const nums = requireNumbers('/', args);
      if (nums[1] === 0) throw new EvalError('division by zero');
      return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
    }
    case '<': {
      const nums = requireNumbers('<', args);
      return { tag: 'boolean', value: nums[0] < nums[1] };
    }
    case '>': {
      const nums = requireNumbers('>', args);
      return { tag: 'boolean', value: nums[0] > nums[1] };
    }
    case '=': {
      const nums = requireNumbers('=', args);
      return { tag: 'boolean', value: nums[0] === nums[1] };
    }
    case '<=': {
      const nums = requireNumbers('<=', args);
      return { tag: 'boolean', value: nums[0] <= nums[1] };
    }
    case '>=': {
      const nums = requireNumbers('>=', args);
      return { tag: 'boolean', value: nums[0] >= nums[1] };
    }
    default:
      throw new EvalError(`unbound variable: ${name}`);
  }
}

// ── Display ────────────────────────────────────────────────────────────

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(display).join(' ')})`;
    case 'lambda': return '#<procedure>';
  }
}

// ── Public API ─────────────────────────────────────────────────────────

function makeGlobalEnv(): Env {
  return new Env();
}

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return display(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
