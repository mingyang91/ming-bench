import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type Cont = (val: SchemeVal) => Bounce;
type Bounce = SchemeVal | (() => Bounce);

type SchemeVal =
  | { tag: 'number'; val: number; pos?: Pos }
  | { tag: 'boolean'; val: boolean; pos?: Pos }
  | { tag: 'string'; val: string; pos?: Pos }
  | { tag: 'char'; val: string; pos?: Pos }
  | { tag: 'symbol'; val: string; pos?: Pos }
  | { tag: 'list'; val: SchemeVal[]; pos?: Pos }
  | { tag: 'lambda'; params: string[]; rest?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'continuation'; cont: Cont; pos?: Pos };

interface Token { text: string; pos: Pos }

// ── Environment ────────────────────────────────────────────────────

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent: Env | null = null) {}

  get(name: string, pos?: Pos): SchemeVal {
    const v = this.bindings.get(name);
    if (v !== undefined) return v;
    if (this.parent) return this.parent.get(name, pos);
    throw posError(`unbound variable: ${name}`, pos);
  }

  define(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }

  set(name: string, val: SchemeVal, pos?: Pos): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, val);
      return;
    }
    if (this.parent) { this.parent.set(name, val, pos); return; }
    throw posError(`unbound variable: ${name}`, pos);
  }
}

// ── Parser ─────────────────────────────────────────────────────────

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;
  const advance = () => {
    if (input[i] === '\n') { line++; col = 1; } else { col++; }
    i++;
  };
  while (i < input.length) {
    const ch = input[i];
    if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
      advance();
      continue;
    }
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') advance();
      continue;
    }
    if (ch === '\'') {
      tokens.push({ text: "'", pos: { line, col } });
      advance();
      continue;
    }
    if (ch === '(' || ch === ')') {
      tokens.push({ text: ch, pos: { line, col } });
      advance();
      continue;
    }
    if (ch === '"') {
      const startPos = { line, col };
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') {
          s += input[i];
          advance();
          if (i < input.length) {
            s += input[i];
            advance();
          }
          continue;
        }
        s += input[i];
        advance();
      }
      if (i < input.length) {
        s += '"';
        advance();
      }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    const startPos = { line, col };
    let atom = '';
    while (
      i < input.length &&
      input[i] !== ' ' &&
      input[i] !== '\t' &&
      input[i] !== '\n' &&
      input[i] !== '\r' &&
      input[i] !== '(' &&
      input[i] !== ')' &&
      input[i] !== ';' &&
      input[i] !== '\''
    ) {
      atom += input[i];
      advance();
    }
    if (atom) tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parseTokens(tokens: Token[], pos: number): [SchemeVal, number] {
  if (pos >= tokens.length) {
    throw new EvalError('unexpected end of input');
  }
  const tok = tokens[pos];
  if (tok.text === "'") {
    const [val, next] = parseTokens(tokens, pos + 1);
    return [{ tag: 'list', val: [{ tag: 'symbol', val: 'quote', pos: tok.pos }, val], pos: tok.pos }, next];
  }
  if (tok.text === '(') {
    const items: SchemeVal[] = [];
    pos++;
    while (pos < tokens.length && tokens[pos].text !== ')') {
      const [val, next] = parseTokens(tokens, pos);
      items.push(val);
      pos = next;
    }
    if (pos >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'list', val: items, pos: tok.pos }, pos + 1];
  }
  if (tok.text === ')') {
    throw new EvalError('unexpected )');
  }
  return [parseAtom(tok), pos + 1];
}

function parseAtom(tok: Token): SchemeVal {
  const p = tok.pos;
  if (tok.text === '#t') return { tag: 'boolean', val: true, pos: p };
  if (tok.text === '#f') return { tag: 'boolean', val: false, pos: p };
  if (tok.text.startsWith('"') && tok.text.endsWith('"')) {
    const inner = tok.text.slice(1, -1).replace(/\\(.)/g, (_, c) => {
      if (c === 'n') return '\n';
      if (c === 't') return '\t';
      if (c === '\\') return '\\';
      if (c === '"') return '"';
      return c;
    });
    return { tag: 'string', val: inner, pos: p };
  }
  if (tok.text.startsWith('#\\')) {
    const rest = tok.text.slice(2);
    if (rest === 'space') return { tag: 'char', val: ' ', pos: p };
    if (rest === 'newline') return { tag: 'char', val: '\n', pos: p };
    if (rest === 'tab') return { tag: 'char', val: '\t', pos: p };
    if (rest.length === 1) return { tag: 'char', val: rest, pos: p };
    throw new EvalError(`unknown character literal: ${tok.text}`);
  }
  if (/^-?\d+$/.test(tok.text)) {
    return { tag: 'number', val: parseInt(tok.text, 10), pos: p };
  }
  return { tag: 'symbol', val: tok.text, pos: p };
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

// ── Helpers ─────────────────────────────────────────────────────────

function fmtPos(p?: Pos): string {
  return p ? `${p.line}:${p.col}: ` : '';
}

function posError(msg: string, p?: Pos): EvalError {
  return new EvalError(`${fmtPos(p)}${msg}`);
}

function isTruthy(v: SchemeVal): boolean {
  return !(v.tag === 'boolean' && v.val === false);
}

function toNumber(v: SchemeVal, op: string, p?: Pos): number {
  if (v.tag !== 'number') throw posError(`${op}: expected number`, p);
  return v.val;
}

function displayVal(v: SchemeVal): string {
  switch (v.tag) {
    case 'number':
      return String(v.val);
    case 'boolean':
      return v.val ? '#t' : '#f';
    case 'string':
      return `"${v.val}"`;
    case 'char':
      return `#\\${v.val}`;
    case 'symbol':
      return v.val;
    case 'list':
      return '(' + v.val.map(displayVal).join(' ') + ')';
    case 'lambda':
    case 'builtin':
    case 'continuation':
      return '#<procedure>';
  }
}

function displayValUnquoted(v: SchemeVal): string {
  switch (v.tag) {
    case 'string':
      return v.val;
    case 'char':
      return v.val;
    default:
      return displayVal(v);
  }
}

function quoteToScheme(v: SchemeVal): SchemeVal {
  return v;
}

// ── Builtins ────────────────────────────────────────────────────────

function applyBuiltin(op: string, evalArgs: SchemeVal[], p?: Pos, out?: string[]): SchemeVal {
  switch (op) {
    case '+': {
      let sum = 0;
      for (const a of evalArgs) sum += toNumber(a, '+', p);
      return { tag: 'number', val: sum };
    }
    case '-': {
      if (evalArgs.length === 0) throw posError('-: need at least one arg', p);
      if (evalArgs.length === 1) return { tag: 'number', val: -toNumber(evalArgs[0], '-', p) };
      let result = toNumber(evalArgs[0], '-', p);
      for (let i = 1; i < evalArgs.length; i++) result -= toNumber(evalArgs[i], '-', p);
      return { tag: 'number', val: result };
    }
    case '*': {
      let prod = 1;
      for (const a of evalArgs) prod *= toNumber(a, '*', p);
      return { tag: 'number', val: prod };
    }
    case '/': {
      if (evalArgs.length < 2) throw posError('/: need at least two args', p);
      let result = toNumber(evalArgs[0], '/', p);
      for (let i = 1; i < evalArgs.length; i++) {
        const d = toNumber(evalArgs[i], '/', p);
        if (d === 0) throw posError('division by zero', p);
        result = Math.trunc(result / d);
      }
      return { tag: 'number', val: result };
    }
    case '<': {
      if (evalArgs.length !== 2) throw posError('<: need exactly two args', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], '<', p) < toNumber(evalArgs[1], '<', p) };
    }
    case '>': {
      if (evalArgs.length !== 2) throw posError('>: need exactly two args', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], '>', p) > toNumber(evalArgs[1], '>', p) };
    }
    case '=': {
      if (evalArgs.length !== 2) throw posError('=: need exactly two args', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], '=', p) === toNumber(evalArgs[1], '=', p) };
    }
    case '<=': {
      if (evalArgs.length !== 2) throw posError('<=: need exactly two args', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], '<=', p) <= toNumber(evalArgs[1], '<=', p) };
    }
    case '>=': {
      if (evalArgs.length !== 2) throw posError('>=: need exactly two args', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], '>=', p) >= toNumber(evalArgs[1], '>=', p) };
    }
    case 'not': {
      if (evalArgs.length !== 1) throw posError('not: need exactly one arg', p);
      return { tag: 'boolean', val: !isTruthy(evalArgs[0]) };
    }
    case 'cons': {
      if (evalArgs.length !== 2) throw posError('cons: need exactly two args', p);
      const [h, t] = evalArgs;
      if (t.tag === 'list') return { tag: 'list', val: [h, ...t.val] };
      return { tag: 'list', val: [h, { tag: 'symbol', val: '.' }, t] };
    }
    case 'car': {
      if (evalArgs.length !== 1) throw posError('car: need exactly one arg', p);
      const a = evalArgs[0];
      if (a.tag !== 'list' || a.val.length === 0) throw posError('car: not a pair', p);
      return a.val[0];
    }
    case 'cdr': {
      if (evalArgs.length !== 1) throw posError('cdr: need exactly one arg', p);
      const a = evalArgs[0];
      if (a.tag !== 'list' || a.val.length === 0) throw posError('cdr: not a pair', p);
      return { tag: 'list', val: a.val.slice(1) };
    }
    case 'null?': {
      if (evalArgs.length !== 1) throw posError('null?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'list' && evalArgs[0].val.length === 0 };
    }
    case 'list': {
      return { tag: 'list', val: evalArgs };
    }
    case 'length': {
      if (evalArgs.length !== 1) throw posError('length: need exactly one arg', p);
      if (evalArgs[0].tag !== 'list') throw posError('length: not a list', p);
      return { tag: 'number', val: evalArgs[0].val.length };
    }
    case 'append': {
      if (evalArgs.length === 0) return { tag: 'list', val: [] };
      let result: SchemeVal[] = [];
      for (let i = 0; i < evalArgs.length; i++) {
        const a = evalArgs[i];
        if (i < evalArgs.length - 1) {
          if (a.tag !== 'list') throw posError('append: not a list', p);
          result = result.concat(a.val);
        } else {
          if (a.tag === 'list') result = result.concat(a.val);
          else result.push(a);
        }
      }
      return { tag: 'list', val: result };
    }
    case 'pair?': {
      if (evalArgs.length !== 1) throw posError('pair?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'list' && evalArgs[0].val.length > 0 };
    }
    case 'number?': {
      if (evalArgs.length !== 1) throw posError('number?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'number' };
    }
    case 'string?': {
      if (evalArgs.length !== 1) throw posError('string?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'string' };
    }
    case 'boolean?': {
      if (evalArgs.length !== 1) throw posError('boolean?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'boolean' };
    }
    case 'symbol?': {
      if (evalArgs.length !== 1) throw posError('symbol?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'symbol' };
    }
    case 'procedure?': {
      if (evalArgs.length !== 1) throw posError('procedure?: need exactly one arg', p);
      const t = evalArgs[0].tag;
      return { tag: 'boolean', val: t === 'lambda' || t === 'builtin' || t === 'continuation' };
    }
    case 'display': {
      if (evalArgs.length !== 1) throw posError('display: need exactly one arg', p);
      if (out) out.push(displayValUnquoted(evalArgs[0]));
      return { tag: 'boolean', val: false };
    }
    case 'write': {
      if (evalArgs.length !== 1) throw posError('write: need exactly one arg', p);
      if (out) out.push(displayVal(evalArgs[0]));
      return { tag: 'boolean', val: false };
    }
    case 'newline': {
      if (evalArgs.length !== 0) throw posError('newline: no arguments expected', p);
      if (out) out.push('\n');
      return { tag: 'boolean', val: false };
    }
    case 'string-append': {
      let result = '';
      for (const a of evalArgs) {
        if (a.tag !== 'string') throw posError('string-append: expected string', p);
        result += a.val;
      }
      return { tag: 'string', val: result };
    }
    case 'string-length': {
      if (evalArgs.length !== 1) throw posError('string-length: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string-length: expected string', p);
      return { tag: 'number', val: evalArgs[0].val.length };
    }
    case 'substring': {
      if (evalArgs.length !== 3) throw posError('substring: need exactly three args', p);
      if (evalArgs[0].tag !== 'string') throw posError('substring: expected string', p);
      const s = evalArgs[0].val;
      const start = toNumber(evalArgs[1], 'substring', p);
      const end = toNumber(evalArgs[2], 'substring', p);
      return { tag: 'string', val: s.slice(start, end) };
    }
    case 'string->number': {
      if (evalArgs.length !== 1) throw posError('string->number: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string->number: expected string', p);
      const n = Number(evalArgs[0].val);
      if (isNaN(n)) return { tag: 'boolean', val: false };
      return { tag: 'number', val: n };
    }
    case 'number->string': {
      if (evalArgs.length !== 1) throw posError('number->string: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') throw posError('number->string: expected number', p);
      return { tag: 'string', val: String(evalArgs[0].val) };
    }
    case 'symbol->string': {
      if (evalArgs.length !== 1) throw posError('symbol->string: need exactly one arg', p);
      if (evalArgs[0].tag !== 'symbol') throw posError('symbol->string: expected symbol', p);
      return { tag: 'string', val: evalArgs[0].val };
    }
    case 'string->symbol': {
      if (evalArgs.length !== 1) throw posError('string->symbol: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string->symbol: expected string', p);
      return { tag: 'symbol', val: evalArgs[0].val };
    }
    case 'string-ref': {
      if (evalArgs.length !== 2) throw posError('string-ref: need exactly two args', p);
      if (evalArgs[0].tag !== 'string') throw posError('string-ref: expected string', p);
      const idx = toNumber(evalArgs[1], 'string-ref', p);
      const str = evalArgs[0].val;
      if (idx < 0 || idx >= str.length) throw posError('string-ref: index out of range', p);
      return { tag: 'char', val: str[idx] };
    }
    case 'string-copy': {
      if (evalArgs.length !== 1) throw posError('string-copy: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string-copy: expected string', p);
      return { tag: 'string', val: evalArgs[0].val };
    }
    case 'string-set!': {
      if (evalArgs.length !== 3) throw posError('string-set!: need exactly three args', p);
      const target = evalArgs[0];
      if (target.tag !== 'string') throw posError('string-set!: expected string', p);
      const idx = toNumber(evalArgs[1], 'string-set!', p);
      if (evalArgs[2].tag !== 'char') throw posError('string-set!: expected char', p);
      const s = target.val;
      if (idx < 0 || idx >= s.length) throw posError('string-set!: index out of range', p);
      target.val = s.substring(0, idx) + evalArgs[2].val + s.substring(idx + 1);
      return { tag: 'boolean', val: false };
    }
    case 'char?': {
      if (evalArgs.length !== 1) throw posError('char?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'char' };
    }
    default:
      throw posError(`unknown procedure: ${op}`, p);
  }
}

const BUILTINS = new Set(['+', '-', '*', '/', '<', '>', '=', '<=', '>=', 'not',
  'cons', 'car', 'cdr', 'null?', 'list', 'length', 'append',
  'pair?', 'number?', 'string?', 'boolean?', 'symbol?', 'procedure?', 'char?',
  'display', 'write', 'newline',
  'string-append', 'string-length', 'substring', 'string->number', 'number->string',
  'symbol->string', 'string->symbol', 'string-ref', 'string-copy', 'string-set!']);

function parseParams(paramList: SchemeVal, p?: Pos): { params: string[]; rest?: string } {
  if (paramList.tag !== 'list') throw posError('params must be a list', p);
  const items = paramList.val;
  const dotIdx = items.findIndex(x => x.tag === 'symbol' && x.val === '.');
  if (dotIdx >= 0) {
    if (dotIdx !== items.length - 2) throw posError('bad dot syntax in params', p);
    const rest = items[items.length - 1];
    if (rest.tag !== 'symbol') throw posError('rest param must be a symbol', p);
    const params = items.slice(0, dotIdx).map(pm => {
      if (pm.tag !== 'symbol') throw posError('bad parameter', p);
      return pm.val;
    });
    return { params, rest: rest.val };
  }
  const params = items.map(pm => {
    if (pm.tag !== 'symbol') throw posError('bad parameter', p);
    return pm.val;
  });
  return { params };
}

// ── CPS Evaluator with Trampoline ───────────────────────────────────

function trampoline(bounce: Bounce): SchemeVal {
  while (typeof bounce === 'function') {
    bounce = (bounce as () => Bounce)();
  }
  return bounce as SchemeVal;
}

function evalListCPS(exprs: SchemeVal[], env: Env, k: (vals: SchemeVal[]) => Bounce, out?: string[]): Bounce {
  // Evaluate right-to-left (valid per R7RS: argument evaluation order is unspecified).
  // This ensures call/cc captures pending left-sibling evaluations in the continuation,
  // so variables like `count` are re-read fresh when the continuation is re-invoked.
  const loop = (i: number, acc: SchemeVal[]): Bounce => {
    if (i < 0) return k(acc);
    return evalCPS(exprs[i], env, v => {
      const next = acc.length === 0 ? [v] : [v, ...acc];
      return () => loop(i - 1, next);
    }, out);
  };
  return loop(exprs.length - 1, []);
}

function evalBodyCPS(exprs: SchemeVal[], idx: number, env: Env, k: Cont, out?: string[]): Bounce {
  if (idx >= exprs.length) return k({ tag: 'boolean', val: false });
  if (idx === exprs.length - 1) return evalCPS(exprs[idx], env, k, out);
  return evalCPS(exprs[idx], env, _ => () => evalBodyCPS(exprs, idx + 1, env, k, out), out);
}

function applyCPS(proc: SchemeVal, args: SchemeVal[], k: Cont, p?: Pos, out?: string[]): Bounce {
  if (proc.tag === 'continuation') {
    if (args.length !== 1) throw posError('continuation: need exactly one arg', p);
    return () => proc.cont(args[0]);
  }
  if (proc.tag === 'lambda') {
    if (proc.rest) {
      if (args.length < proc.params.length)
        throw posError(`wrong number of arguments: expected at least ${proc.params.length}, got ${args.length}`, p);
    } else {
      if (args.length !== proc.params.length)
        throw posError(`wrong number of arguments: expected ${proc.params.length}, got ${args.length}`, p);
    }
    const callEnv = new Env(proc.env);
    for (let i = 0; i < proc.params.length; i++) callEnv.define(proc.params[i], args[i]);
    if (proc.rest) callEnv.define(proc.rest, { tag: 'list', val: args.slice(proc.params.length) });
    return evalBodyCPS(proc.body, 0, callEnv, k, out);
  }
  if (proc.tag === 'builtin') {
    if (proc.name === 'call/cc' || proc.name === 'call-with-current-continuation') {
      if (args.length !== 1) throw posError(`${proc.name}: need exactly one arg`, p);
      const contVal: SchemeVal = { tag: 'continuation', cont: k };
      return () => applyCPS(args[0], [contVal], k, p, out);
    }
    if (proc.name === 'apply') {
      if (args.length < 2) throw posError('apply: need at least two args', p);
      const applyProc = args[0];
      const lastArg = args[args.length - 1];
      if (lastArg.tag !== 'list') throw posError('apply: last argument must be a list', p);
      const allArgs = [...args.slice(1, -1), ...lastArg.val];
      return () => applyCPS(applyProc, allArgs, k, p, out);
    }
    return k(applyBuiltin(proc.name, args, p, out));
  }
  throw posError('not a procedure', p);
}

function evalCPS(expr: SchemeVal, env: Env, k: Cont, out?: string[]): Bounce {
  const p = expr.pos;

  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string' || expr.tag === 'char') {
    return k(expr);
  }

  if (expr.tag === 'symbol') {
    return k(env.get(expr.val, p));
  }

  if (expr.tag !== 'list') throw posError('cannot evaluate', p);

  const items = expr.val;
  if (items.length === 0) throw posError('empty application', p);
  const head = items[0];
  const args = items.slice(1);

  if (head.tag === 'symbol') {
    const op = head.val;

    if (op === 'quote') {
      if (args.length !== 1) throw posError('quote: need exactly one arg', p);
      return k(quoteToScheme(args[0]));
    }

    if (op === 'if') {
      if (args.length < 2 || args.length > 3) throw posError('if: bad syntax', p);
      return evalCPS(args[0], env, condVal => {
        if (isTruthy(condVal)) return () => evalCPS(args[1], env, k, out);
        if (args.length === 3) return () => evalCPS(args[2], env, k, out);
        return k({ tag: 'boolean', val: false });
      }, out);
    }

    if (op === 'define') {
      if (args.length < 2) throw posError('define: bad syntax', p);
      const target = args[0];
      if (target.tag === 'symbol') {
        return evalCPS(args[1], env, val => {
          env.define(target.val, val);
          return k(val);
        }, out);
      }
      if (target.tag === 'list' && target.val.length > 0 && target.val[0].tag === 'symbol') {
        const name = target.val[0].val;
        const paramListVal: SchemeVal = { tag: 'list', val: target.val.slice(1) };
        const { params, rest } = parseParams(paramListVal, p);
        const body = args.slice(1);
        const lambda: SchemeVal = { tag: 'lambda', params, rest, body, env };
        env.define(name, lambda);
        return k(lambda);
      }
      throw posError('define: bad syntax', p);
    }

    if (op === 'set!') {
      if (args.length !== 2) throw posError('set!: bad syntax', p);
      if (args[0].tag !== 'symbol') throw posError('set!: first arg must be a symbol', p);
      const setName = args[0].val;
      return evalCPS(args[1], env, val => {
        env.set(setName, val, p);
        return k(val);
      }, out);
    }

    if (op === 'lambda') {
      if (args.length < 2) throw posError('lambda: bad syntax', p);
      const { params, rest } = parseParams(args[0], p);
      const body = args.slice(1);
      return k({ tag: 'lambda', params, rest, body, env });
    }

    if (op === 'and') {
      if (args.length === 0) return k({ tag: 'boolean', val: true });
      const evalAnd = (i: number): Bounce => {
        if (i === args.length - 1) return evalCPS(args[i], env, k, out);
        return evalCPS(args[i], env, val => {
          if (!isTruthy(val)) return k(val);
          return () => evalAnd(i + 1);
        }, out);
      };
      return evalAnd(0);
    }

    if (op === 'or') {
      if (args.length === 0) return k({ tag: 'boolean', val: false });
      const evalOr = (i: number): Bounce => {
        if (i === args.length - 1) return evalCPS(args[i], env, k, out);
        return evalCPS(args[i], env, val => {
          if (isTruthy(val)) return k(val);
          return () => evalOr(i + 1);
        }, out);
      };
      return evalOr(0);
    }

    if (op === 'begin') {
      if (args.length === 0) return k({ tag: 'boolean', val: false });
      return evalBodyCPS(args, 0, env, k, out);
    }

    if (op === 'cond') {
      const evalCond = (i: number): Bounce => {
        if (i >= args.length) return k({ tag: 'boolean', val: false });
        const clause = args[i];
        if (clause.tag !== 'list') throw posError('cond: bad clause', p);
        if (clause.val.length < 2 && !(clause.val[0]?.tag === 'symbol' && clause.val[0]?.val === 'else')) {
          throw posError('cond: bad clause', p);
        }
        const test = clause.val[0];
        if (test.tag === 'symbol' && test.val === 'else') {
          return evalBodyCPS(clause.val, 1, env, k, out);
        }
        return evalCPS(test, env, condVal => {
          if (isTruthy(condVal)) {
            if (clause.val.length === 1) return k(condVal);
            return evalBodyCPS(clause.val, 1, env, k, out);
          }
          return () => evalCond(i + 1);
        }, out);
      };
      return evalCond(0);
    }

    if (op === 'let') {
      if (args.length < 2) throw posError('let: bad syntax', p);
      let name: string | null = null;
      let bindingsExpr: SchemeVal;
      let body: SchemeVal[];
      if (args[0].tag === 'symbol') {
        name = args[0].val;
        if (args.length < 3) throw posError('let: bad syntax', p);
        bindingsExpr = args[1];
        body = args.slice(2);
      } else {
        bindingsExpr = args[0];
        body = args.slice(1);
      }
      if (bindingsExpr.tag !== 'list') throw posError('let: bad bindings', p);

      const paramNames: string[] = [];
      const initExprs: SchemeVal[] = [];
      for (const b of bindingsExpr.val) {
        if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
          throw posError('let: bad binding', p);
        paramNames.push(b.val[0].val);
        initExprs.push(b.val[1]);
      }

      return evalListCPS(initExprs, env, initVals => {
        if (name !== null) {
          const letEnv = new Env(env);
          const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env: letEnv };
          letEnv.define(name, lambda);
          const callEnv = new Env(letEnv);
          for (let i = 0; i < paramNames.length; i++) callEnv.define(paramNames[i], initVals[i]);
          return evalBodyCPS(body, 0, callEnv, k, out);
        } else {
          const letEnv = new Env(env);
          for (let i = 0; i < paramNames.length; i++) letEnv.define(paramNames[i], initVals[i]);
          return evalBodyCPS(body, 0, letEnv, k, out);
        }
      }, out);
    }

    // Builtin shortcut (not call/cc — those go through general application)
    if (BUILTINS.has(op)) {
      return evalListCPS(args, env, evalArgs => {
        return k(applyBuiltin(op, evalArgs, p, out));
      }, out);
    }
  }

  // General application: evaluate head and args, then apply
  return evalCPS(head, env, proc => {
    return evalListCPS(items.slice(1), env, evalArgs => {
      return () => applyCPS(proc, evalArgs, k, p, out);
    }, out);
  }, out);
}

// ── Global Environment ──────────────────────────────────────────────

function makeGlobalEnv(): Env {
  const env = new Env();
  env.define('apply', { tag: 'builtin', name: 'apply' });
  env.define('call/cc', { tag: 'builtin', name: 'call/cc' });
  env.define('call-with-current-continuation', { tag: 'builtin', name: 'call-with-current-continuation' });
  for (const name of BUILTINS) {
    env.define(name, { tag: 'builtin', name });
  }
  return env;
}

// ── Public API ──────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = trampoline(evalCPS(expr, env, v => v));
  }
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  const out: string[] = [];
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = trampoline(evalCPS(expr, env, v => v, out));
  }
  return { result: displayVal(result!), output: out.join('') };
}
