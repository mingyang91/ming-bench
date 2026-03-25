import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type BuiltinFn = (args: SchemeVal[]) => SchemeVal;

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }  // syntax only (parsed S-expr)
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; fn: BuiltinFn; pos?: Pos };

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}` : '?:?';
}

function errAt(msg: string, pos?: Pos): EvalError {
  return new EvalError(`${posStr(pos)}: ${msg}`);
}

const NIL: SchemeVal = { tag: 'nil' };

// ── Output Buffer ────────────────────────────────────────────────
let outputBuffer = '';

function displayValUnquoted(val: SchemeVal): string {
  if (val.tag === 'string') return val.value;
  if (val.tag === 'char') return val.value;
  return displayVal(val);
}

function writeVal(val: SchemeVal): string {
  return displayVal(val);
}

function makeList(items: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = items.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: items[i], cdr: result };
  }
  return result;
}

function pairToArray(val: SchemeVal): SchemeVal[] {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag !== 'nil') throw new EvalError('not a proper list');
  return result;
}

// ── Environment ───────────────────────────────────────────────────

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent: Env | null = null) {}

  get(name: string, pos?: Pos): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name, pos);
    throw errAt(`unbound variable: ${name}`, pos);
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

  // List operations
  defBuiltin('cons', (args) => {
    if (args.length !== 2) throw new EvalError('cons: expected 2 arguments');
    return { tag: 'pair', car: args[0], cdr: args[1] };
  });

  defBuiltin('car', (args) => {
    if (args.length !== 1) throw new EvalError('car: expected 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('car: expected pair');
    return args[0].car;
  });

  defBuiltin('cdr', (args) => {
    if (args.length !== 1) throw new EvalError('cdr: expected 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('cdr: expected pair');
    return args[0].cdr;
  });

  defBuiltin('null?', (args) => {
    if (args.length !== 1) throw new EvalError('null?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'nil' };
  });

  defBuiltin('list', (args) => {
    return makeList(args);
  });

  defBuiltin('length', (args) => {
    if (args.length !== 1) throw new EvalError('length: expected 1 argument');
    let count = 0;
    let cur = args[0];
    while (cur.tag === 'pair') { count++; cur = cur.cdr; }
    if (cur.tag !== 'nil') throw new EvalError('length: expected proper list');
    return { tag: 'number', value: count };
  });

  defBuiltin('append', (args) => {
    if (args.length === 0) return NIL;
    if (args.length === 1) return args[0];
    // Append all lists
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
      const items = pairToArray(args[i]);
      for (let j = items.length - 1; j >= 0; j--) {
        result = { tag: 'pair', car: items[j], cdr: result };
      }
    }
    return result;
  });

  // Type predicates
  defBuiltin('number?', (args) => {
    if (args.length !== 1) throw new EvalError('number?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'number' };
  });

  defBuiltin('boolean?', (args) => {
    if (args.length !== 1) throw new EvalError('boolean?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  });

  defBuiltin('string?', (args) => {
    if (args.length !== 1) throw new EvalError('string?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'string' };
  });

  defBuiltin('symbol?', (args) => {
    if (args.length !== 1) throw new EvalError('symbol?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  defBuiltin('pair?', (args) => {
    if (args.length !== 1) throw new EvalError('pair?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'pair' };
  });

  defBuiltin('char?', (args) => {
    if (args.length !== 1) throw new EvalError('char?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'char' };
  });

  // I/O
  defBuiltin('display', (args) => {
    if (args.length !== 1) throw new EvalError('display: expected 1 argument');
    outputBuffer += displayValUnquoted(args[0]);
    return { tag: 'void' };
  });

  defBuiltin('write', (args) => {
    if (args.length !== 1) throw new EvalError('write: expected 1 argument');
    outputBuffer += writeVal(args[0]);
    return { tag: 'void' };
  });

  defBuiltin('newline', (args) => {
    if (args.length !== 0) throw new EvalError('newline: expected 0 arguments');
    outputBuffer += '\n';
    return { tag: 'void' };
  });

  // String operations
  defBuiltin('string-append', (args) => {
    let result = '';
    for (const a of args) {
      if (a.tag !== 'string') throw new EvalError('string-append: expected string');
      result += a.value;
    }
    return { tag: 'string', value: result };
  });

  defBuiltin('string-length', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string-length: expected 1 string argument');
    return { tag: 'number', value: args[0].value.length };
  });

  defBuiltin('substring', (args) => {
    if (args.length !== 3) throw new EvalError('substring: expected 3 arguments');
    if (args[0].tag !== 'string') throw new EvalError('substring: expected string');
    const start = expectNum(args[1], 'substring');
    const end = expectNum(args[2], 'substring');
    return { tag: 'string', value: args[0].value.substring(start, end) };
  });

  defBuiltin('string->number', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string->number: expected 1 string argument');
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  });

  defBuiltin('number->string', (args) => {
    if (args.length !== 1 || args[0].tag !== 'number')
      throw new EvalError('number->string: expected 1 number argument');
    return { tag: 'string', value: String(args[0].value) };
  });

  defBuiltin('string-ref', (args) => {
    if (args.length !== 2) throw new EvalError('string-ref: expected 2 arguments');
    if (args[0].tag !== 'string') throw new EvalError('string-ref: expected string');
    const idx = expectNum(args[1], 'string-ref');
    return { tag: 'char', value: args[0].value[idx] };
  });

  defBuiltin('string-copy', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string-copy: expected 1 string argument');
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('string-set!', (args) => {
    if (args.length !== 3) throw new EvalError('string-set!: expected 3 arguments');
    if (args[0].tag !== 'string') throw new EvalError('string-set!: expected string');
    const idx = expectNum(args[1], 'string-set!');
    if (args[2].tag !== 'char') throw new EvalError('string-set!: expected char');
    const s = args[0].value;
    (args[0] as any).value = s.substring(0, idx) + args[2].value + s.substring(idx + 1);
    return { tag: 'void' };
  });

  defBuiltin('symbol->string', (args) => {
    if (args.length !== 1 || args[0].tag !== 'symbol')
      throw new EvalError('symbol->string: expected 1 symbol argument');
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('string->symbol', (args) => {
    if (args.length !== 1 || args[0].tag !== 'string')
      throw new EvalError('string->symbol: expected 1 string argument');
    return { tag: 'symbol', value: args[0].value };
  });

  return env;
}

// ── Parser ─────────────────────────────────────────────────────────

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1, col = 1;

  function advance(): string {
    const ch = input[i++];
    if (ch === '\n') { line++; col = 1; } else { col++; }
    return ch;
  }

  while (i < input.length) {
    const ch = input[i];
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') advance();
      continue;
    }
    if (/\s/.test(ch)) { advance(); continue; }
    const startPos: Pos = { line, col };
    if (ch === '(' || ch === ')') { advance(); tokens.push({ text: ch, pos: startPos }); continue; }
    if (ch === '\'') { advance(); tokens.push({ text: "'", pos: startPos }); continue; }
    if (ch === '"') {
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; advance(); }
        s += input[i]; advance();
      }
      s += '"';
      advance(); // closing quote
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    if (ch === '#') {
      if (input[i + 1] === 't' && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        advance(); advance();
        tokens.push({ text: '#t', pos: startPos }); continue;
      }
      if (input[i + 1] === 'f' && (i + 2 >= input.length || /[\s()]/.test(input[i + 2]))) {
        advance(); advance();
        tokens.push({ text: '#f', pos: startPos }); continue;
      }
    }
    let tok = '';
    while (i < input.length && !/[\s()]/.test(input[i])) {
      tok += input[i]; advance();
    }
    tokens.push({ text: tok, pos: startPos });
  }
  return tokens;
}

function parse(tokens: Token[]): SchemeVal[] {
  let idx = 0;

  function parseExpr(): SchemeVal {
    if (idx >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[idx++];
    if (tok.text === '(') {
      const elems: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elems.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError('missing closing paren');
      idx++;
      return { tag: 'list', elements: elems, pos: tok.pos };
    }
    if (tok.text === ')') throw new EvalError('unexpected )');
    if (tok.text === "'") {
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, inner], pos: tok.pos };
    }
    return parseAtom(tok);
  }

  function parseAtom(tok: Token): SchemeVal {
    if (tok.text === '#t') return { tag: 'boolean', value: true, pos: tok.pos };
    if (tok.text === '#f') return { tag: 'boolean', value: false, pos: tok.pos };
    if (tok.text.startsWith('"')) return { tag: 'string', value: tok.text.slice(1, -1), pos: tok.pos };
    if (tok.text.startsWith('#\\')) {
      const charPart = tok.text.slice(2);
      if (charPart === 'space') return { tag: 'char', value: ' ', pos: tok.pos };
      if (charPart === 'newline') return { tag: 'char', value: '\n', pos: tok.pos };
      if (charPart === 'tab') return { tag: 'char', value: '\t', pos: tok.pos };
      if (charPart.length === 1) return { tag: 'char', value: charPart, pos: tok.pos };
      throw new EvalError(`unknown character literal: ${tok.text}`);
    }
    if (/^-?\d+$/.test(tok.text)) return { tag: 'number', value: parseInt(tok.text, 10), pos: tok.pos };
    return { tag: 'symbol', value: tok.text, pos: tok.pos };
  }

  const exprs: SchemeVal[] = [];
  while (idx < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Quote conversion ──────────────────────────────────────────────
// Convert parsed syntax (list tag) to runtime values (pair/nil)

function quoteSyntaxToValue(expr: SchemeVal): SchemeVal {
  if (expr.tag === 'list') {
    const items = expr.elements.map(quoteSyntaxToValue);
    return makeList(items);
  }
  return expr;
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function evalExpr(expr: SchemeVal, env: Env): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'boolean' || expr.tag === 'string' || expr.tag === 'char') {
    return expr;
  }

  if (expr.tag === 'symbol') {
    return env.get(expr.value, expr.pos);
  }

  if (expr.tag === 'list') {
    const elems = expr.elements;
    const epos = expr.pos;
    if (elems.length === 0) throw errAt('empty application', epos);

    // Special forms
    if (elems[0].tag === 'symbol') {
      const op = elems[0].value;

      if (op === 'quote') {
        if (elems.length !== 2) throw errAt('quote: expected 1 argument', epos);
        return quoteSyntaxToValue(elems[1]);
      }

      if (op === 'if') {
        if (elems.length < 3 || elems.length > 4) throw errAt('if: expected 2 or 3 arguments', epos);
        const cond = evalExpr(elems[1], env);
        if (isTruthy(cond)) {
          return evalExpr(elems[2], env);
        } else {
          if (elems.length === 4) return evalExpr(elems[3], env);
          return { tag: 'void' };
        }
      }

      if (op === 'define') {
        if (elems.length < 3) throw errAt('define: bad syntax', epos);
        const target = elems[1];
        if (target.tag === 'symbol') {
          const val = evalExpr(elems[2], env);
          env.define(target.value, val);
          return { tag: 'void' };
        }
        if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
          const name = target.elements[0].value;
          const params = target.elements.slice(1).map(p => {
            if (p.tag !== 'symbol') throw errAt('define: parameter must be a symbol', epos);
            return p.value;
          });
          const body = elems.slice(2);
          const lambda: SchemeVal = { tag: 'lambda', params, body, env };
          env.define(name, lambda);
          return { tag: 'void' };
        }
        throw errAt('define: bad syntax', epos);
      }

      if (op === 'lambda') {
        if (elems.length < 3) throw errAt('lambda: bad syntax', epos);
        const paramList = elems[1];
        if (paramList.tag !== 'list') throw errAt('lambda: parameters must be a list', epos);
        const params = paramList.elements.map(p => {
          if (p.tag !== 'symbol') throw errAt('lambda: parameter must be a symbol', epos);
          return p.value;
        });
        const body = elems.slice(2);
        return { tag: 'lambda', params, body, env };
      }

      if (op === 'begin') {
        let result: SchemeVal = { tag: 'void' };
        for (let i = 1; i < elems.length; i++) {
          result = evalExpr(elems[i], env);
        }
        return result;
      }

      if (op === 'let') {
        // Named let: (let name ((var init) ...) body ...)
        if (elems.length >= 3 && elems[1].tag === 'symbol') {
          const name = elems[1].value;
          const bindingsList = elems[2];
          if (bindingsList.tag !== 'list') throw errAt('let: bad syntax', epos);
          const paramNames: string[] = [];
          const initVals: SchemeVal[] = [];
          for (const b of bindingsList.elements) {
            if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
              throw errAt('let: bad binding', epos);
            paramNames.push(b.elements[0].value);
            initVals.push(evalExpr(b.elements[1], env));
          }
          const body = elems.slice(3);
          const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
          // Create env where name is bound to the lambda (for recursion)
          const letEnv = new Env(env);
          letEnv.define(name, lambda);
          // Update lambda's env to include itself
          (lambda as any).env = letEnv;
          // Call with initial values
          const callEnv = new Env(letEnv);
          for (let i = 0; i < paramNames.length; i++) {
            callEnv.define(paramNames[i], initVals[i]);
          }
          let result: SchemeVal = { tag: 'void' };
          for (const bodyExpr of body) {
            result = evalExpr(bodyExpr, callEnv);
          }
          return result;
        }
        // Regular let: (let ((var init) ...) body ...)
        if (elems.length < 3) throw errAt('let: bad syntax', epos);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw errAt('let: bad syntax', epos);
        const letEnv = new Env(env);
        for (const b of bindings.elements) {
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw new EvalError('let: bad binding');
          const val = evalExpr(b.elements[1], env); // eval in outer env
          letEnv.define(b.elements[0].value, val);
        }
        let result: SchemeVal = { tag: 'void' };
        for (let i = 2; i < elems.length; i++) {
          result = evalExpr(elems[i], letEnv);
        }
        return result;
      }

      if (op === 'cond') {
        for (let i = 1; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.elements.length < 1) throw errAt('cond: bad clause', epos);
          // else clause
          if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
            let result: SchemeVal = { tag: 'void' };
            for (let j = 1; j < clause.elements.length; j++) {
              result = evalExpr(clause.elements[j], env);
            }
            return result;
          }
          const test = evalExpr(clause.elements[0], env);
          if (isTruthy(test)) {
            if (clause.elements.length === 1) return test;
            let result: SchemeVal = { tag: 'void' };
            for (let j = 1; j < clause.elements.length; j++) {
              result = evalExpr(clause.elements[j], env);
            }
            return result;
          }
        }
        return { tag: 'void' };
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
        if (elems.length !== 2) throw errAt('not: expected 1 argument', epos);
        const val = evalExpr(elems[1], env);
        return { tag: 'boolean', value: !isTruthy(val) };
      }
    }

    // Function application
    const func = evalExpr(elems[0], env);
    const args = elems.slice(1).map(e => evalExpr(e, env));

    if (func.tag === 'lambda') {
      if (args.length !== func.params.length) {
        throw errAt(`lambda: expected ${func.params.length} arguments, got ${args.length}`, epos);
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
      try {
        return func.fn(args);
      } catch (e) {
        if (e instanceof EvalError && !/^\d+:/.test(e.message)) {
          throw errAt(e.message, epos);
        }
        throw e;
      }
    }

    throw errAt(`not a procedure: ${displayVal(func)}`, epos);
  }

  throw errAt('cannot evaluate', expr.pos);
}

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'char': return `#\\${val.value}`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
    case 'nil': return '()';
    case 'pair': {
      let parts: string[] = [];
      let cur: SchemeVal = val;
      while (cur.tag === 'pair') {
        parts.push(displayVal(cur.car));
        cur = cur.cdr;
      }
      if (cur.tag === 'nil') {
        return `(${parts.join(' ')})`;
      }
      return `(${parts.join(' ')} . ${displayVal(cur)})`;
    }
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
  outputBuffer = '';
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  outputBuffer = '';
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return { result: displayVal(result!), output: outputBuffer };
}
