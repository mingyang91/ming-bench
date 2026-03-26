import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; value: SchemeVal[]; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'void'; pos?: Pos };

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}: ` : '';
}

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

  get(name: string, pos?: Pos): SchemeVal {
    const v = this.bindings.get(name);
    if (v !== undefined) return v;
    if (this.parent) return this.parent.get(name, pos);
    throw new EvalError(`${posStr(pos)}unbound variable: ${name}`);
  }

  set(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }
}

// ── Tokenizer ──────────────────────────────────────────────────────

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  function curPos(): Pos { return { line, col }; }
  function advance() {
    if (input[i] === '\n') { line++; col = 1; } else { col++; }
    i++;
  }

  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { advance(); continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') advance(); continue; }
    if (ch === '(' || ch === ')') { tokens.push({ text: ch, pos: curPos() }); advance(); continue; }
    if (ch === '\'') { tokens.push({ text: "'", pos: curPos() }); advance(); continue; }
    if (ch === '"') {
      const p = curPos();
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; advance(); }
        s += input[i]; advance();
      }
      if (i < input.length) { s += '"'; advance(); }
      tokens.push({ text: s, pos: p });
      continue;
    }
    if (ch === '#') {
      if (i + 1 < input.length && (input[i + 1] === 't' || input[i + 1] === 'f')) {
        const p = curPos();
        tokens.push({ text: input.substring(i, i + 2), pos: p });
        advance(); advance();
        continue;
      }
    }
    const p = curPos();
    let atom = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      atom += input[i]; advance();
    }
    if (atom) tokens.push({ text: atom, pos: p });
  }
  return tokens;
}

// ── Parser ─────────────────────────────────────────────────────────

function parse(tokens: Token[]): SchemeVal[] {
  let idx = 0;

  function parseExpr(): SchemeVal {
    if (idx >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[idx++];
    if (tok.text === "'") {
      const datum = parseExpr();
      return { tag: 'list', value: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, datum], pos: tok.pos };
    }
    if (tok.text === '(') {
      const elems: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elems.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError(`${tok.pos.line}:${tok.pos.col}: missing closing paren`);
      idx++;
      return { tag: 'list', value: elems, pos: tok.pos };
    }
    if (tok.text === ')') throw new EvalError(`${tok.pos.line}:${tok.pos.col}: unexpected )`);
    if (tok.text === '#t') return { tag: 'boolean', value: true, pos: tok.pos };
    if (tok.text === '#f') return { tag: 'boolean', value: false, pos: tok.pos };
    if (tok.text.startsWith('"')) return { tag: 'string', value: tok.text.slice(1, -1), pos: tok.pos };
    const num = Number(tok.text);
    if (!isNaN(num) && tok.text !== '') return { tag: 'number', value: num, pos: tok.pos };
    return { tag: 'symbol', value: tok.text, pos: tok.pos };
  }

  const exprs: SchemeVal[] = [];
  while (idx < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(v: SchemeVal): boolean {
  return !(v.tag === 'boolean' && v.value === false);
}

function toNumber(v: SchemeVal, op: string, callPos?: Pos): number {
  if (v.tag !== 'number') throw new EvalError(`${posStr(callPos)}${op}: expected number`);
  return v.value;
}

let _outputBuf: string[] = [];

function evalBuiltin(name: string, args: SchemeVal[], callPos?: Pos): SchemeVal {
  switch (name) {
    case '+': {
      let sum = 0;
      for (const a of args) sum += toNumber(a, '+', callPos);
      return { tag: 'number', value: sum };
    }
    case '-': {
      if (args.length === 0) throw new EvalError(`${posStr(callPos)}-: need at least 1 argument`);
      if (args.length === 1) return { tag: 'number', value: -toNumber(args[0], '-', callPos) };
      let result = toNumber(args[0], '-', callPos);
      for (let i = 1; i < args.length; i++) result -= toNumber(args[i], '-', callPos);
      return { tag: 'number', value: result };
    }
    case '*': {
      let prod = 1;
      for (const a of args) prod *= toNumber(a, '*', callPos);
      return { tag: 'number', value: prod };
    }
    case '/': {
      if (args.length < 2) throw new EvalError(`${posStr(callPos)}/: need at least 2 arguments`);
      let result = toNumber(args[0], '/', callPos);
      for (let i = 1; i < args.length; i++) {
        const d = toNumber(args[i], '/', callPos);
        if (d === 0) throw new EvalError(`${posStr(callPos)}division by zero`);
        result = Math.trunc(result / d);
      }
      return { tag: 'number', value: result };
    }
    case '<': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}<: need 2 arguments`);
      return { tag: 'boolean', value: toNumber(args[0], '<', callPos) < toNumber(args[1], '<', callPos) };
    }
    case '>': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}>: need 2 arguments`);
      return { tag: 'boolean', value: toNumber(args[0], '>', callPos) > toNumber(args[1], '>', callPos) };
    }
    case '=': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}=: need 2 arguments`);
      return { tag: 'boolean', value: toNumber(args[0], '=', callPos) === toNumber(args[1], '=', callPos) };
    }
    case '<=': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}<=: need 2 arguments`);
      return { tag: 'boolean', value: toNumber(args[0], '<=', callPos) <= toNumber(args[1], '<=', callPos) };
    }
    case '>=': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}>=: need 2 arguments`);
      return { tag: 'boolean', value: toNumber(args[0], '>=', callPos) >= toNumber(args[1], '>=', callPos) };
    }
    case 'cons': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}cons: need 2 arguments`);
      return makePair(args[0], args[1]);
    }
    case 'car': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}car: need 1 argument`);
      if (args[0].tag !== 'pair') throw new EvalError(`${posStr(callPos)}car: not a pair`);
      return args[0].car;
    }
    case 'cdr': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}cdr: need 1 argument`);
      if (args[0].tag !== 'pair') throw new EvalError(`${posStr(callPos)}cdr: not a pair`);
      return args[0].cdr;
    }
    case 'null?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}null?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'nil' };
    }
    case 'pair?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}pair?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'pair' };
    }
    case 'list': {
      return arrayToSchemeList(args);
    }
    case 'length': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}length: need 1 argument`);
      const arr = schemeListToArray(args[0]);
      return { tag: 'number', value: arr.length };
    }
    case 'append': {
      if (args.length === 0) return NIL;
      if (args.length === 1) return args[0];
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
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}number?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'number' };
    }
    case 'boolean?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}boolean?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'boolean' };
    }
    case 'string?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'string' };
    }
    case 'symbol?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}symbol?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'symbol' };
    }
    case 'not': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}not: need 1 argument`);
      return { tag: 'boolean', value: !isTruthy(args[0]) };
    }
    case 'display': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}display: need 1 argument`);
      _outputBuf.push(displayVal(args[0]));
      return { tag: 'void' };
    }
    case 'write': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}write: need 1 argument`);
      _outputBuf.push(writeVal(args[0]));
      return { tag: 'void' };
    }
    case 'newline': {
      if (args.length !== 0) throw new EvalError(`${posStr(callPos)}newline: need 0 arguments`);
      _outputBuf.push('\n');
      return { tag: 'void' };
    }
    case 'string-append': {
      let result = '';
      for (const a of args) {
        if (a.tag !== 'string') throw new EvalError(`${posStr(callPos)}string-append: expected string`);
        result += a.value;
      }
      return { tag: 'string', value: result };
    }
    case 'string-length': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string-length: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-length: expected string`);
      return { tag: 'number', value: args[0].value.length };
    }
    case 'substring': {
      if (args.length !== 3) throw new EvalError(`${posStr(callPos)}substring: need 3 arguments`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}substring: expected string`);
      const start = toNumber(args[1], 'substring', callPos);
      const end = toNumber(args[2], 'substring', callPos);
      return { tag: 'string', value: args[0].value.substring(start, end) };
    }
    case 'string->number': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string->number: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string->number: expected string`);
      const n = Number(args[0].value);
      if (isNaN(n)) return { tag: 'boolean', value: false };
      return { tag: 'number', value: n };
    }
    case 'number->string': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}number->string: need 1 argument`);
      if (args[0].tag !== 'number') throw new EvalError(`${posStr(callPos)}number->string: expected number`);
      return { tag: 'string', value: String(args[0].value) };
    }
    case 'symbol->string': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}symbol->string: need 1 argument`);
      if (args[0].tag !== 'symbol') throw new EvalError(`${posStr(callPos)}symbol->string: expected symbol`);
      return { tag: 'string', value: args[0].value };
    }
    case 'string->symbol': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string->symbol: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string->symbol: expected string`);
      return { tag: 'symbol', value: args[0].value };
    }
    case 'string-ref': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string-ref: need 2 arguments`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-ref: expected string`);
      const idx = toNumber(args[1], 'string-ref', callPos);
      if (idx < 0 || idx >= args[0].value.length) throw new EvalError(`${posStr(callPos)}string-ref: index out of range`);
      return { tag: 'char', value: args[0].value[idx] };
    }
    case 'char?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}char?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'char' };
    }
    default:
      throw new EvalError(`${posStr(callPos)}unknown builtin: ${name}`);
  }
}

const BUILTIN_NAMES = new Set([
  '+', '-', '*', '/', '<', '>', '=', '<=', '>=',
  'cons', 'car', 'cdr', 'null?', 'pair?', 'list', 'length', 'append',
  'number?', 'boolean?', 'string?', 'symbol?', 'not',
  'display', 'write', 'newline',
  'string-append', 'string-length', 'substring',
  'string->number', 'number->string',
  'symbol->string', 'string->symbol',
  'string-ref', 'char?',
]);

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string') return expr;
  if (expr.tag === 'nil' || expr.tag === 'pair') return expr;

  if (expr.tag === 'symbol') {
    if (BUILTIN_NAMES.has(expr.value)) return { tag: 'builtin', name: expr.value, pos: expr.pos };
    return env.get(expr.value, expr.pos);
  }

  if (expr.tag !== 'list') return expr;

  const elems = expr.value;
  if (elems.length === 0) throw new EvalError(`${posStr(expr.pos)}empty application`);

  const head = elems[0];

  if (head.tag === 'symbol') {
    switch (head.value) {
      case 'quote': {
        if (elems.length !== 2) throw new EvalError(`${posStr(expr.pos)}quote: wrong number of arguments`);
        return quoteDatum(elems[1]);
      }
      case 'if': {
        if (elems.length < 3 || elems.length > 4) throw new EvalError(`${posStr(expr.pos)}if: wrong number of arguments`);
        const cond = evaluate(elems[1], env);
        if (isTruthy(cond)) return evaluate(elems[2], env);
        if (elems.length === 4) return evaluate(elems[3], env);
        return { tag: 'void' };
      }
      case 'define': {
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}define: wrong number of arguments`);
        const target = elems[1];
        if (target.tag === 'symbol') {
          const val = evaluate(elems[2], env);
          env.set(target.value, val);
          return { tag: 'void' };
        }
        if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
          const name = target.value[0].value;
          const params = target.value.slice(1).map(p => {
            if (p.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}define: parameter must be a symbol`);
            return p.value;
          });
          const body = elems.slice(2);
          const lam: SchemeVal = { tag: 'lambda', params, body, env };
          env.set(name, lam);
          return { tag: 'void' };
        }
        throw new EvalError(`${posStr(expr.pos)}define: invalid syntax`);
      }
      case 'lambda': {
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}lambda: wrong number of arguments`);
        const paramList = elems[1];
        if (paramList.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}lambda: parameters must be a list`);
        const params = paramList.value.map(p => {
          if (p.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}lambda: parameter must be a symbol`);
          return p.value;
        });
        const body = elems.slice(2);
        return { tag: 'lambda', params, body, env };
      }
      case 'begin': {
        if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}begin: need at least 1 expression`);
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
          if (bindingsList.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}let: invalid bindings`);
          const paramNames: string[] = [];
          const initVals: SchemeVal[] = [];
          for (const b of bindingsList.value) {
            if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
              throw new EvalError(`${posStr(expr.pos)}let: invalid binding`);
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
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}let: wrong number of arguments`);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}let: invalid bindings`);
        const letEnv = new Env(env);
        for (const b of bindings.value) {
          if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
            throw new EvalError(`${posStr(expr.pos)}let: invalid binding`);
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
            throw new EvalError(`${posStr(expr.pos)}cond: invalid clause`);
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
    return evalBuiltin(proc.name, args, expr.pos);
  }

  if (proc.tag === 'lambda') {
    if (args.length !== proc.params.length) {
      throw new EvalError(`${posStr(expr.pos)}lambda: expected ${proc.params.length} arguments, got ${args.length}`);
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

  throw new EvalError(`${posStr(expr.pos)}not a procedure`);
}

// ── Display ────────────────────────────────────────────────────────

// writeVal: like Scheme's `write` — strings get quotes
function writeVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'char': return `#\\${val.value}`;
    case 'symbol': return val.value;
    case 'void': return '';
    case 'nil': return '()';
    case 'pair': {
      let result = '(' + writeVal(val.car);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        result += ' ' + writeVal(cur.car);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') {
        result += ' . ' + writeVal(cur);
      }
      result += ')';
      return result;
    }
    case 'list': return `(${val.value.map(writeVal).join(' ')})`;
    case 'lambda': return '#<procedure>';
    case 'builtin': return '#<procedure>';
  }
}

// displayVal: like Scheme's `display` — strings without quotes
function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return val.value;
    case 'char': return val.value;
    default: return writeVal(val);
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const globalEnv = new Env();
  _outputBuf = [];
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, globalEnv);
  }
  return writeVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const globalEnv = new Env();
  _outputBuf = [];
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, globalEnv);
  }
  return { result: writeVal(result), output: _outputBuf.join('') };
}
