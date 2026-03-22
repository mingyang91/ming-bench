import { EvalError } from './evalError.js';

// --- Types ---

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] }
  | { tag: 'void' }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env };

// --- Environment ---

interface Env {
  bindings: Map<string, SchemeVal>;
  parent: Env | null;
}

function makeEnv(parent: Env | null): Env {
  return { bindings: new Map(), parent };
}

function envLookup(env: Env, name: string): SchemeVal {
  let cur: Env | null = env;
  while (cur) {
    const val = cur.bindings.get(name);
    if (val !== undefined) return val;
    cur = cur.parent;
  }
  throw new EvalError(`unbound variable: ${name}`);
}

function envDefine(env: Env, name: string, val: SchemeVal): void {
  env.bindings.set(name, val);
}

// --- Tokenizer ---

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
      i++;
      continue;
    }
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }
    // Quote shorthand
    if (ch === "'") {
      tokens.push("'");
      i++;
      continue;
    }
    if (ch === '(' || ch === ')') {
      tokens.push(ch);
      i++;
      continue;
    }
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
        i++;
      }
      tokens.push(s);
      continue;
    }
    let atom = '';
    while (i < input.length && !' \t\n\r();"\''.includes(input[i])) {
      atom += input[i];
      i++;
    }
    if (atom.length > 0) {
      tokens.push(atom);
    }
  }
  return tokens;
}

// --- Parser ---

function parse(tokens: string[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) {
      throw new EvalError('unexpected end of input');
    }
    const token = tokens[pos];
    if (token === "'") {
      pos++;
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote' }, inner] };
    }
    if (token === '(') {
      pos++;
      const elements: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos] !== ')') {
        elements.push(parseExpr());
      }
      if (pos >= tokens.length) {
        throw new EvalError('missing closing parenthesis');
      }
      pos++;
      return { tag: 'list', elements };
    }
    if (token === ')') {
      throw new EvalError('unexpected )');
    }
    pos++;
    return parseAtom(token);
  }

  function parseAtom(token: string): SchemeVal {
    if (token === '#t') return { tag: 'boolean', value: true };
    if (token === '#f') return { tag: 'boolean', value: false };
    if (token.startsWith('"') && token.endsWith('"')) {
      const inner = token.slice(1, -1)
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
      return { tag: 'string', value: inner };
    }
    const num = Number(token);
    if (!isNaN(num) && token !== '') {
      return { tag: 'number', value: num };
    }
    return { tag: 'symbol', value: token };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// --- Evaluator ---

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
      return envLookup(env, expr.value);
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) {
        throw new EvalError('empty application');
      }
      const head = elems[0];
      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'quote': {
            if (elems.length !== 2) throw new EvalError('quote: expected 1 argument');
            return elems[1];
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4) throw new EvalError('if: expected 2 or 3 arguments');
            const cond = evalExpr(elems[1], env);
            if (isTruthy(cond)) {
              return evalExpr(elems[2], env);
            } else if (elems.length === 4) {
              return evalExpr(elems[3], env);
            }
            return { tag: 'void' };
          }
          case 'define': {
            if (elems.length < 3) throw new EvalError('define: expected at least 2 arguments');
            const target = elems[1];
            if (target.tag === 'symbol') {
              const val = evalExpr(elems[2], env);
              envDefine(env, target.value, val);
              return { tag: 'void' };
            }
            // (define (f params...) body...)
            if (target.tag === 'list' && target.elements.length >= 1 && target.elements[0].tag === 'symbol') {
              const name = target.elements[0].value;
              const params = target.elements.slice(1).map(p => {
                if (p.tag !== 'symbol') throw new EvalError('define: parameter must be a symbol');
                return p.value;
              });
              const body = elems.slice(2);
              const lambda: SchemeVal = { tag: 'lambda', params, body, env };
              envDefine(env, name, lambda);
              return { tag: 'void' };
            }
            throw new EvalError('define: invalid syntax');
          }
          case 'lambda': {
            if (elems.length < 3) throw new EvalError('lambda: expected at least 2 arguments');
            const paramList = elems[1];
            if (paramList.tag !== 'list') throw new EvalError('lambda: parameters must be a list');
            const params = paramList.elements.map(p => {
              if (p.tag !== 'symbol') throw new EvalError('lambda: parameter must be a symbol');
              return p.value;
            });
            const body = elems.slice(2);
            return { tag: 'lambda', params, body, env };
          }
          case 'and':
            return evalAnd(elems.slice(1), env);
          case 'or':
            return evalOr(elems.slice(1), env);
          case 'not': {
            if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
            const val = evalExpr(elems[1], env);
            return { tag: 'boolean', value: !isTruthy(val) };
          }
        }
      }
      // Check for built-in procedure by name before general eval
      if (head.tag === 'symbol') {
        const builtin = lookupBuiltin(head.value);
        if (builtin) {
          const args = elems.slice(1).map(e => evalExpr(e, env));
          return builtin(args);
        }
      }
      // Procedure application
      const proc = evalExpr(head, env);
      const args = elems.slice(1).map(e => evalExpr(e, env));
      return applyProc(proc, args);
    }
    default:
      return expr;
  }
}

function applyProc(proc: SchemeVal, args: SchemeVal[]): SchemeVal {
  if (proc.tag === 'lambda') {
    if (args.length !== proc.params.length) {
      throw new EvalError(`lambda: expected ${proc.params.length} arguments, got ${args.length}`);
    }
    const callEnv = makeEnv(proc.env);
    for (let i = 0; i < proc.params.length; i++) {
      envDefine(callEnv, proc.params[i], args[i]);
    }
    let result: SchemeVal = { tag: 'void' };
    for (const bodyExpr of proc.body) {
      result = evalExpr(bodyExpr, callEnv);
    }
    return result;
  }
  // Built-in procedures by name won't reach here; handle them via symbol dispatch
  throw new EvalError('not a procedure');
}

function evalAnd(exprs: SchemeVal[], env: Env): SchemeVal {
  let result: SchemeVal = { tag: 'boolean', value: true };
  for (const expr of exprs) {
    result = evalExpr(expr, env);
    if (!isTruthy(result)) return result;
  }
  return result;
}

function evalOr(exprs: SchemeVal[], env: Env): SchemeVal {
  let result: SchemeVal = { tag: 'boolean', value: false };
  for (const expr of exprs) {
    result = evalExpr(expr, env);
    if (isTruthy(result)) return result;
  }
  return result;
}

function requireNumbers(args: SchemeVal[], name: string): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number`);
    return a.value;
  });
}

function arith(args: SchemeVal[], op: string): SchemeVal {
  const nums = requireNumbers(args, op);
  if (nums.length === 0) {
    if (op === '+') return { tag: 'number', value: 0 };
    if (op === '*') return { tag: 'number', value: 1 };
    throw new EvalError(`${op}: expected at least 1 argument`);
  }
  if (op === '-' && nums.length === 1) {
    return { tag: 'number', value: -nums[0] };
  }
  let result = nums[0];
  for (let i = 1; i < nums.length; i++) {
    switch (op) {
      case '+': result += nums[i]; break;
      case '-': result -= nums[i]; break;
      case '*': result *= nums[i]; break;
      case '/':
        if (nums[i] === 0) throw new EvalError('division by zero');
        result = Math.trunc(result / nums[i]);
        break;
    }
  }
  return { tag: 'number', value: result };
}

function compare(args: SchemeVal[], op: string): SchemeVal {
  if (args.length !== 2) throw new EvalError(`${op}: expected 2 arguments`);
  const nums = requireNumbers(args, op);
  let result: boolean;
  switch (op) {
    case '<': result = nums[0] < nums[1]; break;
    case '>': result = nums[0] > nums[1]; break;
    case '=': result = nums[0] === nums[1]; break;
    case '<=': result = nums[0] <= nums[1]; break;
    default: result = false;
  }
  return { tag: 'boolean', value: result };
}

// Built-in procedure lookup
function lookupBuiltin(name: string): ((args: SchemeVal[]) => SchemeVal) | null {
  switch (name) {
    case '+': return args => arith(args, '+');
    case '-': return args => arith(args, '-');
    case '*': return args => arith(args, '*');
    case '/': return args => arith(args, '/');
    case '<': return args => compare(args, '<');
    case '>': return args => compare(args, '>');
    case '=': return args => compare(args, '=');
    case '<=': return args => compare(args, '<=');
    default: return null;
  }
}

// --- Display ---

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
    case 'void': return '#<void>';
    case 'lambda': return '#<procedure>';
  }
}

// --- Public API ---

function makeGlobalEnv(): Env {
  return makeEnv(null);
}

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) {
    throw new EvalError('no expressions');
  }
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  // If last result is void (from define), still display it
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  return { result: evalStr(input), output: '' };
}
