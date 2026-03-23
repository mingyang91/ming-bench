import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'list'; value: SchemeVal[]; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; fn: (args: SchemeVal[]) => SchemeVal; pos?: Pos }
  | { tag: 'void'; pos?: Pos };

// ── Environment ────────────────────────────────────────────────────

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

// ── Parser ─────────────────────────────────────────────────────────

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  function advance() {
    if (input[i] === '\n') { line++; col = 1; } else { col++; }
    i++;
  }

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
    const startPos: Pos = { line, col };
    if (ch === '(' || ch === ')') {
      tokens.push({ text: ch, pos: startPos });
      advance();
      continue;
    }
    if (ch === "'") {
      tokens.push({ text: "'", pos: startPos });
      advance();
      continue;
    }
    if (ch === '"') {
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
    let atom = '';
    while (i < input.length && !("() \t\n\r;'".includes(input[i]))) {
      atom += input[i];
      advance();
    }
    if (atom.length > 0) tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parseTokens(tokens: Token[], pos: number): [SchemeVal, number] {
  if (pos >= tokens.length) throw new EvalError('unexpected end of input');
  const tok = tokens[pos];
  if (tok.text === "'") {
    const [val, next] = parseTokens(tokens, pos + 1);
    return [{ tag: 'list', value: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, val], pos: tok.pos }, next];
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
    return [{ tag: 'list', value: items, pos: tok.pos }, pos + 1];
  }
  if (tok.text === ')') throw new EvalError('unexpected )');
  const atom = parseAtom(tok.text);
  atom.pos = tok.pos;
  return [atom, pos + 1];
}

function parseAtom(tok: string): SchemeVal {
  if (tok === '#t') return { tag: 'boolean', value: true };
  if (tok === '#f') return { tag: 'boolean', value: false };
  if (tok.startsWith('"') && tok.endsWith('"')) {
    const inner = tok.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return { tag: 'string', value: inner };
  }
  const num = Number(tok);
  if (!isNaN(num) && tok !== '') {
    return { tag: 'number', value: num };
  }
  return { tag: 'symbol', value: tok };
}

function parse(input: string): SchemeVal[] {
  const toks = tokenize(input);
  const exprs: SchemeVal[] = [];
  let pos = 0;
  while (pos < toks.length) {
    const [val, next] = parseTokens(toks, pos);
    exprs.push(val);
    pos = next;
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────

function posStr(p?: Pos): string {
  return p ? `${p.line}:${p.col}: ` : '';
}

const NIL: SchemeVal = { tag: 'nil' };

function listToPairs(items: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: items[i], cdr: result };
  }
  return result;
}

function astToPairs(val: SchemeVal): SchemeVal {
  if (val.tag === 'list') {
    return listToPairs(val.value.map(astToPairs));
  }
  return val;
}

function pairsToArray(val: SchemeVal): SchemeVal[] {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  return result;
}

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function expectNumber(val: SchemeVal, op: string): number {
  if (val.tag !== 'number') throw new EvalError(`${op}: expected number`);
  return val.value;
}

function makeGlobalEnv(): Env {
  const env = new Env();

  function defBuiltin(name: string, fn: (args: SchemeVal[]) => SchemeVal) {
    env.define(name, { tag: 'builtin', name, fn });
  }

  defBuiltin('+', (args) => {
    let sum = 0;
    for (const a of args) sum += expectNumber(a, '+');
    return { tag: 'number', value: sum };
  });

  defBuiltin('-', (args) => {
    if (args.length < 1) throw new EvalError('-: need at least 1 argument');
    if (args.length === 1) return { tag: 'number', value: -expectNumber(args[0], '-') };
    let result = expectNumber(args[0], '-');
    for (let i = 1; i < args.length; i++) result -= expectNumber(args[i], '-');
    return { tag: 'number', value: result };
  });

  defBuiltin('*', (args) => {
    let prod = 1;
    for (const a of args) prod *= expectNumber(a, '*');
    return { tag: 'number', value: prod };
  });

  defBuiltin('/', (args) => {
    if (args.length < 2) throw new EvalError('/: need at least 2 arguments');
    let result = expectNumber(args[0], '/');
    for (let i = 1; i < args.length; i++) {
      const d = expectNumber(args[i], '/');
      if (d === 0) throw new EvalError('division by zero');
      result = Math.trunc(result / d);
    }
    return { tag: 'number', value: result };
  });

  defBuiltin('<', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '<') < expectNumber(args[1], '<') }));
  defBuiltin('>', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '>') > expectNumber(args[1], '>') }));
  defBuiltin('=', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '=') === expectNumber(args[1], '=') }));
  defBuiltin('<=', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '<=') <= expectNumber(args[1], '<=') }));
  defBuiltin('>=', (args) => ({ tag: 'boolean', value: expectNumber(args[0], '>=') >= expectNumber(args[1], '>=') }));

  defBuiltin('not', (args) => ({ tag: 'boolean', value: !isTruthy(args[0]) }));

  defBuiltin('cons', (args) => ({ tag: 'pair', car: args[0], cdr: args[1] }));
  defBuiltin('car', (args) => {
    if (args[0].tag !== 'pair') throw new EvalError('car: not a pair');
    return args[0].car;
  });
  defBuiltin('cdr', (args) => {
    if (args[0].tag !== 'pair') throw new EvalError('cdr: not a pair');
    return args[0].cdr;
  });
  defBuiltin('null?', (args) => ({ tag: 'boolean', value: args[0].tag === 'nil' }));
  defBuiltin('list', (args) => listToPairs(args));
  defBuiltin('length', (args) => {
    let len = 0;
    let cur = args[0];
    while (cur.tag === 'pair') { len++; cur = cur.cdr; }
    return { tag: 'number', value: len };
  });
  defBuiltin('append', (args) => {
    if (args.length === 0) return NIL;
    if (args.length === 1) return args[0];
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
      const elems = pairsToArray(args[i]);
      for (let j = elems.length - 1; j >= 0; j--) {
        result = { tag: 'pair', car: elems[j], cdr: result };
      }
    }
    return result;
  });

  defBuiltin('number?', (args) => ({ tag: 'boolean', value: args[0].tag === 'number' }));
  defBuiltin('string?', (args) => ({ tag: 'boolean', value: args[0].tag === 'string' }));
  defBuiltin('boolean?', (args) => ({ tag: 'boolean', value: args[0].tag === 'boolean' }));
  defBuiltin('pair?', (args) => ({ tag: 'boolean', value: args[0].tag === 'pair' }));
  defBuiltin('symbol?', (args) => ({ tag: 'boolean', value: args[0].tag === 'symbol' }));

  return env;
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  if (expr.tag === 'symbol') {
    try {
      return env.get(expr.value);
    } catch (e) {
      if (e instanceof EvalError && expr.pos) {
        throw new EvalError(`${posStr(expr.pos)}${e.message}`);
      }
      throw e;
    }
  }
  if (expr.tag !== 'list') {
    return expr; // self-evaluating
  }

  const items = expr.value;
  if (items.length === 0) throw new EvalError(`${posStr(expr.pos)}empty application`);

  const head = items[0];
  if (head.tag === 'symbol') {
    const op = head.value;

    // Special forms
    if (op === 'quote') {
      return astToPairs(items[1]);
    }

    if (op === 'if') {
      if (items.length < 3) throw new EvalError(`${posStr(expr.pos)}if: bad syntax`);
      const cond = evaluate(items[1], env);
      if (isTruthy(cond)) {
        return evaluate(items[2], env);
      } else if (items.length > 3) {
        return evaluate(items[3], env);
      }
      return { tag: 'void' };
    }

    if (op === 'define') {
      if (items.length < 3) throw new EvalError(`${posStr(expr.pos)}define: bad syntax`);
      if (items[1].tag === 'list') {
        // (define (f params...) body...)
        const nameAndParams = items[1].value;
        const name = (nameAndParams[0] as { tag: 'symbol'; value: string }).value;
        const params = nameAndParams.slice(1).map(p => (p as { tag: 'symbol'; value: string }).value);
        const body = items.slice(2);
        env.define(name, { tag: 'lambda', params, body, env });
        return { tag: 'void' };
      }
      // (define x expr)
      const name = (items[1] as { tag: 'symbol'; value: string }).value;
      const val = evaluate(items[2], env);
      env.define(name, val);
      return { tag: 'void' };
    }

    if (op === 'lambda') {
      const paramList = items[1];
      const params = (paramList as { tag: 'list'; value: SchemeVal[] }).value.map(
        p => (p as { tag: 'symbol'; value: string }).value
      );
      const body = items.slice(2);
      return { tag: 'lambda', params, body, env };
    }

    if (op === 'and') {
      let result: SchemeVal = { tag: 'boolean', value: true };
      for (let i = 1; i < items.length; i++) {
        result = evaluate(items[i], env);
        if (!isTruthy(result)) return result;
      }
      return result;
    }

    if (op === 'or') {
      let result: SchemeVal = { tag: 'boolean', value: false };
      for (let i = 1; i < items.length; i++) {
        result = evaluate(items[i], env);
        if (isTruthy(result)) return result;
      }
      return result;
    }

    if (op === 'let') {
      // Named let: (let name ((var val) ...) body...)
      if (items[1].tag === 'symbol') {
        const name = items[1].value;
        const bindings = (items[2] as { tag: 'list'; value: SchemeVal[] }).value;
        const body = items.slice(3);
        const params: string[] = [];
        const inits: SchemeVal[] = [];
        for (const b of bindings) {
          const bv = (b as { tag: 'list'; value: SchemeVal[] }).value;
          params.push((bv[0] as { tag: 'symbol'; value: string }).value);
          inits.push(bv[1]);
        }
        const letEnv = new Env(env);
        const lambda: SchemeVal = { tag: 'lambda', params, body, env: letEnv };
        letEnv.define(name, lambda);
        const args = inits.map(i => evaluate(i, env));
        const callEnv = new Env(letEnv);
        for (let i = 0; i < params.length; i++) {
          callEnv.define(params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of body) {
          result = evaluate(bodyExpr, callEnv);
        }
        return result;
      }
      // Regular let: (let ((var val) ...) body...)
      const bindings = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const body = items.slice(2);
      const letEnv = new Env(env);
      for (const b of bindings) {
        const bv = (b as { tag: 'list'; value: SchemeVal[] }).value;
        const name = (bv[0] as { tag: 'symbol'; value: string }).value;
        const val = evaluate(bv[1], env);
        letEnv.define(name, val);
      }
      let result: SchemeVal = { tag: 'void' };
      for (const bodyExpr of body) {
        result = evaluate(bodyExpr, letEnv);
      }
      return result;
    }

    if (op === 'begin') {
      let result: SchemeVal = { tag: 'void' };
      for (let i = 1; i < items.length; i++) {
        result = evaluate(items[i], env);
      }
      return result;
    }

    if (op === 'cond') {
      for (let i = 1; i < items.length; i++) {
        const clause = (items[i] as { tag: 'list'; value: SchemeVal[] }).value;
        if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
          let result: SchemeVal = { tag: 'void' };
          for (let j = 1; j < clause.length; j++) {
            result = evaluate(clause[j], env);
          }
          return result;
        }
        const test = evaluate(clause[0], env);
        if (isTruthy(test)) {
          let result: SchemeVal = test;
          for (let j = 1; j < clause.length; j++) {
            result = evaluate(clause[j], env);
          }
          return result;
        }
      }
      return { tag: 'void' };
    }
  }

  // Function application
  const proc = evaluate(head, env);
  const args = items.slice(1).map(a => evaluate(a, env));

  if (proc.tag === 'builtin') {
    try {
      return proc.fn(args);
    } catch (e) {
      if (e instanceof EvalError && expr.pos && !e.message.match(/^\d+:/)) {
        throw new EvalError(`${posStr(expr.pos)}${e.message}`);
      }
      throw e;
    }
  }

  if (proc.tag === 'lambda') {
    const callEnv = new Env(proc.env);
    for (let i = 0; i < proc.params.length; i++) {
      callEnv.define(proc.params[i], args[i]);
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

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.value.map(display).join(' ')})`;
    case 'nil': return '()';
    case 'pair': {
      let s = '(' + display(val.car);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        s += ' ' + display(cur.car);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') {
        s += ' . ' + display(cur);
      }
      s += ')';
      return s;
    }
    case 'lambda': return '#<procedure>';
    case 'builtin': return `#<builtin:${val.name}>`;
    case 'void': return '';
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('empty input');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
