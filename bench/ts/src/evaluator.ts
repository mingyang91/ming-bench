import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }  // parse-time only
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env; pos?: Pos };

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}: ` : '';
}

function errAt(msg: string, pos?: Pos): EvalError {
  return new EvalError(`${posStr(pos)}${msg}`);
}

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

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  function advance(): string {
    const ch = input[i++];
    if (ch === '\n') { line++; col = 1; } else { col++; }
    return ch;
  }

  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { advance(); continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') advance(); continue; }
    const startPos: Pos = { line, col };
    if (ch === '(' || ch === ')') { advance(); tokens.push({ text: ch, pos: startPos }); continue; }
    if (ch === '\'') { advance(); tokens.push({ text: "'", pos: startPos }); continue; }
    if (ch === '"') {
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += advance(); }
        s += advance();
      }
      if (i < input.length) { s += '"'; advance(); }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    let tok = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      tok += advance();
    }
    tokens.push({ text: tok, pos: startPos });
  }
  return tokens;
}

// ── Parser ─────────────────────────────────────────────────────────────

function parse(tokens: Token[]): SchemeVal[] {
  let idx = 0;

  function parseExpr(): SchemeVal {
    if (idx >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[idx++];
    if (tok.text === "'") {
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === '(') {
      const elements: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elements.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError(`${tok.pos.line}:${tok.pos.col}: missing closing parenthesis`);
      idx++;
      return { tag: 'list', elements, pos: tok.pos };
    }
    if (tok.text === ')') throw new EvalError(`${tok.pos.line}:${tok.pos.col}: unexpected )`);
    return parseAtom(tok);
  }

  function parseAtom(tok: Token): SchemeVal {
    if (tok.text === '#t') return { tag: 'boolean', value: true, pos: tok.pos };
    if (tok.text === '#f') return { tag: 'boolean', value: false, pos: tok.pos };
    if (tok.text.startsWith('"') && tok.text.endsWith('"')) {
      return { tag: 'string', value: tok.text.slice(1, -1), pos: tok.pos };
    }
    const num = Number(tok.text);
    if (!isNaN(num) && tok.text !== '') {
      return { tag: 'number', value: num, pos: tok.pos };
    }
    return { tag: 'symbol', value: tok.text, pos: tok.pos };
  }

  const exprs: SchemeVal[] = [];
  while (idx < tokens.length) {
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
      try { return env.get(expr.value); }
      catch (e) { throw errAt(`unbound variable: ${expr.value}`, expr.pos); }
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw errAt('empty application', expr.pos);

      const head = elems[0];

      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'quote': {
            if (elems.length !== 2) throw errAt('quote: expected 1 argument', expr.pos);
            return listToPairs(elems[1]);
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4)
              throw errAt('if: expected 2-3 arguments', expr.pos);
            const cond = evalExpr(elems[1], env);
            if (isTruthy(cond)) return evalExpr(elems[2], env);
            if (elems.length === 4) return evalExpr(elems[3], env);
            return SCM_FALSE;
          }
          case 'define': {
            if (elems.length < 3) throw errAt('define: bad syntax', expr.pos);
            const target = elems[1];
            if (target.tag === 'symbol') {
              const val = evalExpr(elems[2], env);
              env.set(target.value, val);
              return val;
            }
            if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
              const name = target.elements[0].value;
              const params = target.elements.slice(1).map(p => {
                if (p.tag !== 'symbol') throw errAt('define: param must be symbol', expr.pos);
                return p.value;
              });
              const body = elems.slice(2);
              const lambda: SchemeVal = { tag: 'lambda', params, body, env };
              env.set(name, lambda);
              return lambda;
            }
            throw errAt('define: bad syntax', expr.pos);
          }
          case 'lambda': {
            if (elems.length < 3) throw errAt('lambda: bad syntax', expr.pos);
            const paramList = elems[1];
            if (paramList.tag !== 'list') throw errAt('lambda: params must be a list', expr.pos);
            const params = paramList.elements.map(p => {
              if (p.tag !== 'symbol') throw errAt('lambda: param must be symbol', expr.pos);
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
            if (elems.length !== 2) throw errAt('not: expected 1 argument', expr.pos);
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
                throw errAt('cond: bad clause', expr.pos);
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
              if (bindingList.tag !== 'list') throw errAt('let: bad syntax', expr.pos);
              const paramNames: string[] = [];
              const initVals: SchemeVal[] = [];
              for (const b of bindingList.elements) {
                if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                  throw errAt('let: bad binding', expr.pos);
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
            if (elems.length < 3) throw errAt('let: bad syntax', expr.pos);
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('let: bad syntax', expr.pos);
            const letEnv = new Env(env);
            for (const b of bindings.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('let: bad binding', expr.pos);
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
        return applyBuiltin(head.value, args, expr.pos);
      }

      const proc = evalExpr(head, env);

      if (proc.tag === 'lambda') {
        if (args.length !== proc.params.length)
          throw errAt(`expected ${proc.params.length} arguments, got ${args.length}`, expr.pos);
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

      throw errAt(`not a procedure: ${display(proc)}`, expr.pos);
    }
    default:
      throw errAt(`cannot evaluate: ${display(expr)}`, expr.pos);
  }
}

function requireNumbers(name: string, args: SchemeVal[], pos?: Pos): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw errAt(`${name}: expected number`, pos);
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

function applyBuiltin(name: string, args: SchemeVal[], pos?: Pos): SchemeVal {
  switch (name) {
    case '+': {
      const nums = requireNumbers('+', args, pos);
      return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
    }
    case '-': {
      if (args.length === 0) throw errAt('-: expected at least 1 argument', pos);
      const nums = requireNumbers('-', args, pos);
      if (nums.length === 1) return { tag: 'number', value: -nums[0] };
      return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
    }
    case '*': {
      const nums = requireNumbers('*', args, pos);
      return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
    }
    case '/': {
      if (args.length < 2) throw errAt('/: expected at least 2 arguments', pos);
      const nums = requireNumbers('/', args, pos);
      if (nums[1] === 0) throw errAt('division by zero', pos);
      return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
    }
    case '<': {
      const nums = requireNumbers('<', args, pos);
      return { tag: 'boolean', value: nums[0] < nums[1] };
    }
    case '>': {
      const nums = requireNumbers('>', args, pos);
      return { tag: 'boolean', value: nums[0] > nums[1] };
    }
    case '=': {
      const nums = requireNumbers('=', args, pos);
      return { tag: 'boolean', value: nums[0] === nums[1] };
    }
    case '<=': {
      const nums = requireNumbers('<=', args, pos);
      return { tag: 'boolean', value: nums[0] <= nums[1] };
    }
    case '>=': {
      const nums = requireNumbers('>=', args, pos);
      return { tag: 'boolean', value: nums[0] >= nums[1] };
    }
    case 'cons': {
      if (args.length !== 2) throw errAt('cons: expected 2 arguments', pos);
      return makePair(args[0], args[1]);
    }
    case 'car': {
      if (args.length !== 1) throw errAt('car: expected 1 argument', pos);
      if (args[0].tag !== 'pair') throw errAt('car: expected pair', pos);
      return args[0].car;
    }
    case 'cdr': {
      if (args.length !== 1) throw errAt('cdr: expected 1 argument', pos);
      if (args[0].tag !== 'pair') throw errAt('cdr: expected pair', pos);
      return args[0].cdr;
    }
    case 'null?': {
      if (args.length !== 1) throw errAt('null?: expected 1 argument', pos);
      return args[0].tag === 'nil' ? SCM_TRUE : SCM_FALSE;
    }
    case 'list': {
      return arrayToList(args);
    }
    case 'length': {
      if (args.length !== 1) throw errAt('length: expected 1 argument', pos);
      let count = 0;
      let cur = args[0];
      while (cur.tag === 'pair') {
        count++;
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') throw errAt('length: expected proper list', pos);
      return { tag: 'number', value: count };
    }
    case 'append': {
      if (args.length === 0) return SCM_NIL;
      if (args.length === 1) return args[0];
      let result = args[args.length - 1];
      for (let i = args.length - 2; i >= 0; i--) {
        const items = pairToArray(args[i]);
        if (items === null) throw errAt('append: expected proper list', pos);
        for (let j = items.length - 1; j >= 0; j--) {
          result = makePair(items[j], result);
        }
      }
      return result;
    }
    case 'number?':
      if (args.length !== 1) throw errAt('number?: expected 1 argument', pos);
      return args[0].tag === 'number' ? SCM_TRUE : SCM_FALSE;
    case 'string?':
      if (args.length !== 1) throw errAt('string?: expected 1 argument', pos);
      return args[0].tag === 'string' ? SCM_TRUE : SCM_FALSE;
    case 'boolean?':
      if (args.length !== 1) throw errAt('boolean?: expected 1 argument', pos);
      return args[0].tag === 'boolean' ? SCM_TRUE : SCM_FALSE;
    case 'pair?':
      if (args.length !== 1) throw errAt('pair?: expected 1 argument', pos);
      return args[0].tag === 'pair' ? SCM_TRUE : SCM_FALSE;
    case 'symbol?':
      if (args.length !== 1) throw errAt('symbol?: expected 1 argument', pos);
      return args[0].tag === 'symbol' ? SCM_TRUE : SCM_FALSE;
    default:
      throw errAt(`unbound variable: ${name}`, pos);
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
