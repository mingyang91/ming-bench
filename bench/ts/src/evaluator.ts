import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; value: SchemeVal[] }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'nil' }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env }
  | { tag: 'builtin'; name: string }
  | { tag: 'void' };

const NIL: SchemeVal = { tag: 'nil' };

function makePair(car: SchemeVal, cdr: SchemeVal): SchemeVal {
  return { tag: 'pair', car, cdr };
}

function schemeListToArray(val: SchemeVal): SchemeVal[] {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag !== 'nil') throw new EvalError('not a proper list');
  return result;
}

function arrayToSchemeList(arr: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = makePair(arr[i], result);
  }
  return result;
}

// Convert parsed list AST to runtime pair representation for quote
function quoteDatum(val: SchemeVal): SchemeVal {
  if (val.tag === 'list') {
    if (val.value.length === 0) return NIL;
    return arrayToSchemeList(val.value.map(quoteDatum));
  }
  return val;
}

// ── Environment ───────────────────────────────────────────────────

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent?: Env) {}

  get(name: string): SchemeVal {
    const v = this.bindings.get(name);
    if (v !== undefined) return v;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
  }

  set(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }
}

// ── Tokenizer ──────────────────────────────────────────────────────

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
    if (ch === '#') {
      if (i + 1 < input.length && (input[i + 1] === 't' || input[i + 1] === 'f')) {
        tokens.push(input.substring(i, i + 2));
        i += 2;
        continue;
      }
    }
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
    if (tok === "'") {
      const datum = parseExpr();
      return { tag: 'list', value: [{ tag: 'symbol', value: 'quote' }, datum] };
    }
    if (tok === '(') {
      const elems: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos] !== ')') {
        elems.push(parseExpr());
      }
      if (pos >= tokens.length) throw new EvalError('missing closing paren');
      pos++;
      return { tag: 'list', value: elems };
    }
    if (tok === ')') throw new EvalError('unexpected )');
    if (tok === '#t') return { tag: 'boolean', value: true };
    if (tok === '#f') return { tag: 'boolean', value: false };
    if (tok.startsWith('"')) return { tag: 'string', value: tok.slice(1, -1) };
    const num = Number(tok);
    if (!isNaN(num) && tok !== '') return { tag: 'number', value: num };
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
    case '>=': {
      if (args.length !== 2) throw new EvalError('>=: need 2 arguments');
      return { tag: 'boolean', value: toNumber(args[0], '>=') >= toNumber(args[1], '>=') };
    }
    case 'cons': {
      if (args.length !== 2) throw new EvalError('cons: need 2 arguments');
      return makePair(args[0], args[1]);
    }
    case 'car': {
      if (args.length !== 1) throw new EvalError('car: need 1 argument');
      if (args[0].tag !== 'pair') throw new EvalError('car: not a pair');
      return args[0].car;
    }
    case 'cdr': {
      if (args.length !== 1) throw new EvalError('cdr: need 1 argument');
      if (args[0].tag !== 'pair') throw new EvalError('cdr: not a pair');
      return args[0].cdr;
    }
    case 'null?': {
      if (args.length !== 1) throw new EvalError('null?: need 1 argument');
      return { tag: 'boolean', value: args[0].tag === 'nil' };
    }
    case 'pair?': {
      if (args.length !== 1) throw new EvalError('pair?: need 1 argument');
      return { tag: 'boolean', value: args[0].tag === 'pair' };
    }
    case 'list': {
      return arrayToSchemeList(args);
    }
    case 'length': {
      if (args.length !== 1) throw new EvalError('length: need 1 argument');
      const arr = schemeListToArray(args[0]);
      return { tag: 'number', value: arr.length };
    }
    case 'append': {
      if (args.length === 0) return NIL;
      if (args.length === 1) return args[0];
      // append all lists
      let result = args[args.length - 1];
      for (let i = args.length - 2; i >= 0; i--) {
        const items = schemeListToArray(args[i]);
        for (let j = items.length - 1; j >= 0; j--) {
          result = makePair(items[j], result);
        }
      }
      return result;
    }
    case 'number?': {
      if (args.length !== 1) throw new EvalError('number?: need 1 argument');
      return { tag: 'boolean', value: args[0].tag === 'number' };
    }
    case 'boolean?': {
      if (args.length !== 1) throw new EvalError('boolean?: need 1 argument');
      return { tag: 'boolean', value: args[0].tag === 'boolean' };
    }
    case 'string?': {
      if (args.length !== 1) throw new EvalError('string?: need 1 argument');
      return { tag: 'boolean', value: args[0].tag === 'string' };
    }
    case 'symbol?': {
      if (args.length !== 1) throw new EvalError('symbol?: need 1 argument');
      return { tag: 'boolean', value: args[0].tag === 'symbol' };
    }
    case 'not': {
      if (args.length !== 1) throw new EvalError('not: need 1 argument');
      return { tag: 'boolean', value: !isTruthy(args[0]) };
    }
    default:
      throw new EvalError(`unknown builtin: ${name}`);
  }
}

const BUILTIN_NAMES = new Set([
  '+', '-', '*', '/', '<', '>', '=', '<=', '>=',
  'cons', 'car', 'cdr', 'null?', 'pair?', 'list', 'length', 'append',
  'number?', 'boolean?', 'string?', 'symbol?', 'not',
]);

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') return expr;
  if (expr.tag === 'nil' || expr.tag === 'pair') return expr;

  if (expr.tag === 'symbol') {
    if (BUILTIN_NAMES.has(expr.value)) return { tag: 'builtin', name: expr.value };
    return env.get(expr.value);
  }

  if (expr.tag !== 'list') return expr;

  const elems = expr.value;
  if (elems.length === 0) throw new EvalError('empty application');

  const head = elems[0];

  if (head.tag === 'symbol') {
    switch (head.value) {
      case 'quote': {
        if (elems.length !== 2) throw new EvalError('quote: wrong number of arguments');
        return quoteDatum(elems[1]);
      }
      case 'if': {
        if (elems.length < 3 || elems.length > 4) throw new EvalError('if: wrong number of arguments');
        const cond = evaluate(elems[1], env);
        if (isTruthy(cond)) return evaluate(elems[2], env);
        if (elems.length === 4) return evaluate(elems[3], env);
        return { tag: 'void' };
      }
      case 'define': {
        if (elems.length < 3) throw new EvalError('define: wrong number of arguments');
        const target = elems[1];
        if (target.tag === 'symbol') {
          const val = evaluate(elems[2], env);
          env.set(target.value, val);
          return { tag: 'void' };
        }
        if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
          const name = target.value[0].value;
          const params = target.value.slice(1).map(p => {
            if (p.tag !== 'symbol') throw new EvalError('define: parameter must be a symbol');
            return p.value;
          });
          const body = elems.slice(2);
          const lam: SchemeVal = { tag: 'lambda', params, body, env };
          env.set(name, lam);
          return { tag: 'void' };
        }
        throw new EvalError('define: invalid syntax');
      }
      case 'lambda': {
        if (elems.length < 3) throw new EvalError('lambda: wrong number of arguments');
        const paramList = elems[1];
        if (paramList.tag !== 'list') throw new EvalError('lambda: parameters must be a list');
        const params = paramList.value.map(p => {
          if (p.tag !== 'symbol') throw new EvalError('lambda: parameter must be a symbol');
          return p.value;
        });
        const body = elems.slice(2);
        return { tag: 'lambda', params, body, env };
      }
      case 'begin': {
        if (elems.length < 2) throw new EvalError('begin: need at least 1 expression');
        let result: SchemeVal = { tag: 'void' };
        for (let i = 1; i < elems.length; i++) {
          result = evaluate(elems[i], env);
        }
        return result;
      }
      case 'let': {
        // Named let: (let name ((var init) ...) body...)
        if (elems.length >= 3 && elems[1].tag === 'symbol') {
          const loopName = elems[1].value;
          const bindingsList = elems[2];
          if (bindingsList.tag !== 'list') throw new EvalError('let: invalid bindings');
          const paramNames: string[] = [];
          const initVals: SchemeVal[] = [];
          for (const b of bindingsList.value) {
            if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
              throw new EvalError('let: invalid binding');
            paramNames.push(b.value[0].value);
            initVals.push(evaluate(b.value[1], env));
          }
          const body = elems.slice(3);
          const loopLam: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
          const loopEnv = new Env(env);
          loopEnv.set(loopName, loopLam);
          // Update the lambda's env to include itself
          (loopLam as any).env = loopEnv;
          const callEnv = new Env(loopEnv);
          for (let i = 0; i < paramNames.length; i++) {
            callEnv.set(paramNames[i], initVals[i]);
          }
          let result: SchemeVal = { tag: 'void' };
          for (const bodyExpr of body) {
            result = evaluate(bodyExpr, callEnv);
          }
          return result;
        }
        // Regular let: (let ((var init) ...) body...)
        if (elems.length < 3) throw new EvalError('let: wrong number of arguments');
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw new EvalError('let: invalid bindings');
        const letEnv = new Env(env);
        for (const b of bindings.value) {
          if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
            throw new EvalError('let: invalid binding');
          const val = evaluate(b.value[1], env);
          letEnv.set(b.value[0].value, val);
        }
        let result: SchemeVal = { tag: 'void' };
        for (let i = 2; i < elems.length; i++) {
          result = evaluate(elems[i], letEnv);
        }
        return result;
      }
      case 'cond': {
        for (let i = 1; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.value.length < 2)
            throw new EvalError('cond: invalid clause');
          const test = clause.value[0];
          if (test.tag === 'symbol' && test.value === 'else') {
            let result: SchemeVal = { tag: 'void' };
            for (let j = 1; j < clause.value.length; j++) {
              result = evaluate(clause.value[j], env);
            }
            return result;
          }
          const testVal = evaluate(test, env);
          if (isTruthy(testVal)) {
            let result: SchemeVal = testVal;
            for (let j = 1; j < clause.value.length; j++) {
              result = evaluate(clause.value[j], env);
            }
            return result;
          }
        }
        return { tag: 'void' };
      }
      case 'and': {
        if (elems.length === 1) return { tag: 'boolean', value: true };
        let result: SchemeVal = { tag: 'boolean', value: true };
        for (let i = 1; i < elems.length; i++) {
          result = evaluate(elems[i], env);
          if (!isTruthy(result)) return result;
        }
        return result;
      }
      case 'or': {
        if (elems.length === 1) return { tag: 'boolean', value: false };
        let result: SchemeVal = { tag: 'boolean', value: false };
        for (let i = 1; i < elems.length; i++) {
          result = evaluate(elems[i], env);
          if (isTruthy(result)) return result;
        }
        return result;
      }
    }
  }

  // Function application
  const proc = evaluate(head, env);
  const args = elems.slice(1).map(e => evaluate(e, env));

  if (proc.tag === 'builtin') {
    return evalBuiltin(proc.name, args);
  }

  if (proc.tag === 'lambda') {
    if (args.length !== proc.params.length) {
      throw new EvalError(`lambda: expected ${proc.params.length} arguments, got ${args.length}`);
    }
    const callEnv = new Env(proc.env);
    for (let i = 0; i < proc.params.length; i++) {
      callEnv.set(proc.params[i], args[i]);
    }
    let result: SchemeVal = { tag: 'void' };
    for (const bodyExpr of proc.body) {
      result = evaluate(bodyExpr, callEnv);
    }
    return result;
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
    case 'list': return `(${val.value.map(display).join(' ')})`;
    case 'lambda': return '#<procedure>';
    case 'builtin': return '#<procedure>';
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const globalEnv = new Env();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, globalEnv);
  }
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const result = evalStr(input);
  return { result, output: '' };
}
