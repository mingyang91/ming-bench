import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'nil' }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'list'; elements: SchemeVal[] }  // parse-time only
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env };

const SCM_NIL: SchemeVal = { tag: 'nil' };
const SCM_TRUE: SchemeVal = { tag: 'boolean', value: true };
const SCM_FALSE: SchemeVal = { tag: 'boolean', value: false };

function makePair(car: SchemeVal, cdr: SchemeVal): SchemeVal {
  return { tag: 'pair', car, cdr };
}

// Convert a JS array to a proper Scheme list (pair chain ending in nil)
function arrayToList(arr: SchemeVal[]): SchemeVal {
  let result: SchemeVal = SCM_NIL;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = makePair(arr[i], result);
  }
  return result;
}

// Convert a parse-time list to a runtime pair chain
function listToPairs(val: SchemeVal): SchemeVal {
  if (val.tag === 'list') {
    return arrayToList(val.elements.map(listToPairs));
  }
  return val;
}

// Convert a runtime pair chain to a JS array (returns null if improper)
function pairToArray(val: SchemeVal): SchemeVal[] | null {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag === 'nil') return result;
  return null; // improper list
}

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
    case 'nil':
    case 'pair':
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
            return listToPairs(elems[1]);
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4)
              throw new EvalError('if: expected 2-3 arguments');
            const cond = evalExpr(elems[1], env);
            if (isTruthy(cond)) return evalExpr(elems[2], env);
            if (elems.length === 4) return evalExpr(elems[3], env);
            return SCM_FALSE;
          }
          case 'define': {
            if (elems.length < 3) throw new EvalError('define: bad syntax');
            const target = elems[1];
            if (target.tag === 'symbol') {
              const val = evalExpr(elems[2], env);
              env.set(target.value, val);
              return val;
            }
            if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
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
            let result: SchemeVal = SCM_TRUE;
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
              if (!isTruthy(result)) return result;
            }
            return result;
          }
          case 'or': {
            let result: SchemeVal = SCM_FALSE;
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
              if (isTruthy(result)) return result;
            }
            return result;
          }
          case 'not': {
            if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
            const val = evalExpr(elems[1], env);
            return isTruthy(val) ? SCM_FALSE : SCM_TRUE;
          }
          case 'begin': {
            let result: SchemeVal = SCM_FALSE;
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
            }
            return result;
          }
          case 'cond': {
            for (let i = 1; i < elems.length; i++) {
              const clause = elems[i];
              if (clause.tag !== 'list' || clause.elements.length < 2)
                throw new EvalError('cond: bad clause');
              const test = clause.elements[0];
              if (test.tag === 'symbol' && test.value === 'else') {
                let result: SchemeVal = SCM_FALSE;
                for (let j = 1; j < clause.elements.length; j++) {
                  result = evalExpr(clause.elements[j], env);
                }
                return result;
              }
              const testVal = evalExpr(test, env);
              if (isTruthy(testVal)) {
                let result: SchemeVal = testVal;
                for (let j = 1; j < clause.elements.length; j++) {
                  result = evalExpr(clause.elements[j], env);
                }
                return result;
              }
            }
            return SCM_FALSE;
          }
          case 'let': {
            // Named let: (let name ((var init) ...) body...)
            if (elems.length >= 3 && elems[1].tag === 'symbol') {
              const name = elems[1].value;
              const bindingList = elems[2];
              if (bindingList.tag !== 'list') throw new EvalError('let: bad syntax');
              const paramNames: string[] = [];
              const initVals: SchemeVal[] = [];
              for (const b of bindingList.elements) {
                if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                  throw new EvalError('let: bad binding');
                paramNames.push(b.elements[0].value);
                initVals.push(evalExpr(b.elements[1], env));
              }
              const body = elems.slice(3);
              const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
              // Create env where name is bound to the lambda (for recursion)
              const callEnv = new Env(env);
              callEnv.set(name, lambda);
              // Also update the lambda's env to include itself
              (lambda as any).env = callEnv;
              for (let i = 0; i < paramNames.length; i++) {
                callEnv.set(paramNames[i], initVals[i]);
              }
              let result: SchemeVal = SCM_FALSE;
              for (const bodyExpr of body) {
                result = evalExpr(bodyExpr, callEnv);
              }
              return result;
            }
            // Regular let: (let ((var init) ...) body...)
            if (elems.length < 3) throw new EvalError('let: bad syntax');
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw new EvalError('let: bad syntax');
            const letEnv = new Env(env);
            for (const b of bindings.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw new EvalError('let: bad binding');
              const val = evalExpr(b.elements[1], env);
              letEnv.set(b.elements[0].value, val);
            }
            let result: SchemeVal = SCM_FALSE;
            for (let i = 2; i < elems.length; i++) {
              result = evalExpr(elems[i], letEnv);
            }
            return result;
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
        let result: SchemeVal = SCM_FALSE;
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

const BUILTINS = new Set([
  '+', '-', '*', '/', '<', '>', '=', '<=', '>=',
  'cons', 'car', 'cdr', 'null?', 'list', 'length', 'append',
  'number?', 'string?', 'boolean?', 'pair?', 'symbol?',
]);

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
    case 'cons': {
      if (args.length !== 2) throw new EvalError('cons: expected 2 arguments');
      return makePair(args[0], args[1]);
    }
    case 'car': {
      if (args.length !== 1) throw new EvalError('car: expected 1 argument');
      if (args[0].tag !== 'pair') throw new EvalError('car: expected pair');
      return args[0].car;
    }
    case 'cdr': {
      if (args.length !== 1) throw new EvalError('cdr: expected 1 argument');
      if (args[0].tag !== 'pair') throw new EvalError('cdr: expected pair');
      return args[0].cdr;
    }
    case 'null?': {
      if (args.length !== 1) throw new EvalError('null?: expected 1 argument');
      return args[0].tag === 'nil' ? SCM_TRUE : SCM_FALSE;
    }
    case 'list': {
      return arrayToList(args);
    }
    case 'length': {
      if (args.length !== 1) throw new EvalError('length: expected 1 argument');
      let count = 0;
      let cur = args[0];
      while (cur.tag === 'pair') {
        count++;
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') throw new EvalError('length: expected proper list');
      return { tag: 'number', value: count };
    }
    case 'append': {
      if (args.length === 0) return SCM_NIL;
      if (args.length === 1) return args[0];
      // Append all lists
      let result = args[args.length - 1];
      for (let i = args.length - 2; i >= 0; i--) {
        const items = pairToArray(args[i]);
        if (items === null) throw new EvalError('append: expected proper list');
        for (let j = items.length - 1; j >= 0; j--) {
          result = makePair(items[j], result);
        }
      }
      return result;
    }
    case 'number?':
      if (args.length !== 1) throw new EvalError('number?: expected 1 argument');
      return args[0].tag === 'number' ? SCM_TRUE : SCM_FALSE;
    case 'string?':
      if (args.length !== 1) throw new EvalError('string?: expected 1 argument');
      return args[0].tag === 'string' ? SCM_TRUE : SCM_FALSE;
    case 'boolean?':
      if (args.length !== 1) throw new EvalError('boolean?: expected 1 argument');
      return args[0].tag === 'boolean' ? SCM_TRUE : SCM_FALSE;
    case 'pair?':
      if (args.length !== 1) throw new EvalError('pair?: expected 1 argument');
      return args[0].tag === 'pair' ? SCM_TRUE : SCM_FALSE;
    case 'symbol?':
      if (args.length !== 1) throw new EvalError('symbol?: expected 1 argument');
      return args[0].tag === 'symbol' ? SCM_TRUE : SCM_FALSE;
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
    case 'nil': return '()';
    case 'pair': {
      let result = '(' + display(val.car);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        result += ' ' + display(cur.car);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') {
        result += ' . ' + display(cur);
      }
      result += ')';
      return result;
    }
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
