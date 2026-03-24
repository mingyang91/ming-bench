import { EvalError } from './evalError.js';

// --- Types ---

interface Pos { line: number; col: number }

type SchemeValBase =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string; chars?: string[] }
  | { tag: 'symbol'; value: string }
  | { tag: 'char'; value: string }
  | { tag: 'list'; value: SchemeVal[] }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'nil' }
  | { tag: 'procedure'; value: (...args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' };

type SchemeVal = SchemeValBase & { pos?: Pos };

const NIL: SchemeVal = { tag: 'nil' };

function strContent(v: SchemeVal & { tag: 'string' }): string {
  return v.chars ? v.chars.join('') : v.value;
}

function listToConsPairs(lst: SchemeVal): SchemeVal {
  if (lst.tag !== 'list') return lst;
  let result: SchemeVal = NIL;
  for (let i = lst.value.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: listToConsPairs(lst.value[i]), cdr: result };
  }
  return result;
}

// --- Parser ---

interface Token {
  type: 'lparen' | 'rparen' | 'quote' | 'atom' | 'string' | 'dot';
  value: string;
  line: number;
  col: number;
}

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  function advance(): void {
    if (input[i] === '\n') { line++; col = 1; } else { col++; }
    i++;
  }

  while (i < input.length) {
    const ch = input[i];
    // skip whitespace
    if (/\s/.test(ch)) { advance(); continue; }
    // skip line comments
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') advance();
      continue;
    }
    const tokLine = line, tokCol = col;
    if (ch === '(') { tokens.push({ type: 'lparen', value: '(', line: tokLine, col: tokCol }); advance(); continue; }
    if (ch === ')') { tokens.push({ type: 'rparen', value: ')', line: tokLine, col: tokCol }); advance(); continue; }
    if (ch === '\'') { tokens.push({ type: 'quote', value: '\'', line: tokLine, col: tokCol }); advance(); continue; }
    if (ch === '"') {
      let s = '';
      advance(); // skip opening quote
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') {
          advance();
          if (i < input.length) {
            if (input[i] === 'n') s += '\n';
            else if (input[i] === 't') s += '\t';
            else if (input[i] === '"') s += '"';
            else if (input[i] === '\\') s += '\\';
            else s += input[i];
          }
        } else {
          s += input[i];
        }
        advance();
      }
      if (i < input.length) advance(); // skip closing quote
      tokens.push({ type: 'string', value: s, line: tokLine, col: tokCol });
      continue;
    }
    // atom
    let atom = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      atom += input[i];
      advance();
    }
    if (atom === '.') {
      tokens.push({ type: 'dot', value: '.', line: tokLine, col: tokCol });
    } else {
      tokens.push({ type: 'atom', value: atom, line: tokLine, col: tokCol });
    }
  }
  return tokens;
}

function parse(tokens: Token[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[pos];
    const p: Pos = { line: tok.line, col: tok.col };

    if (tok.type === 'lparen') {
      pos++; // skip (
      const elements: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos].type !== 'rparen') {
        elements.push(parseExpr());
      }
      if (pos >= tokens.length) throw new EvalError('missing closing parenthesis');
      pos++; // skip )
      return { tag: 'list', value: elements, pos: p };
    }

    if (tok.type === 'rparen') {
      throw new EvalError('unexpected )');
    }

    if (tok.type === 'quote') {
      pos++;
      const quoted = parseExpr();
      return { tag: 'list', value: [{ tag: 'symbol', value: 'quote', pos: p }, quoted], pos: p };
    }

    if (tok.type === 'string') {
      pos++;
      return { tag: 'string', value: tok.value, pos: p };
    }

    // atom
    pos++;
    const v = tok.value;
    if (v === '#t') return { tag: 'boolean', value: true, pos: p };
    if (v === '#f') return { tag: 'boolean', value: false, pos: p };
    if (v.startsWith('#\\')) {
      const name = v.slice(2);
      let ch: string;
      if (name === 'space') ch = ' ';
      else if (name === 'newline') ch = '\n';
      else if (name === 'tab') ch = '\t';
      else if (name.length === 1) ch = name;
      else throw new EvalError(`unknown character name: ${name}`);
      return { tag: 'char', value: ch, pos: p };
    }
    if (/^-?\d+$/.test(v)) return { tag: 'number', value: parseInt(v, 10), pos: p };
    return { tag: 'symbol', value: v, pos: p };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// --- Environment ---

class Env {
  private bindings: Map<string, SchemeVal>;
  private parent: Env | null;

  constructor(parent: Env | null = null) {
    this.bindings = new Map();
    this.parent = parent;
  }

  get(name: string): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
  }

  set(name: string, value: SchemeVal): void {
    this.bindings.set(name, value);
  }
}

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return strContent(val); // no quotes for display
    case 'symbol': return val.value;
    case 'char': return val.value;
    case 'nil': return '()';
    case 'pair': {
      let parts: string[] = [];
      let cur: SchemeVal = val;
      while (cur.tag === 'pair') {
        parts.push(displayVal(cur.car));
        cur = cur.cdr;
      }
      if (cur.tag === 'nil') return `(${parts.join(' ')})`;
      return `(${parts.join(' ')} . ${displayVal(cur)})`;
    }
    case 'void': return '';
    case 'procedure': return '#<procedure>';
    case 'list': return `(${val.value.map(displayVal).join(' ')})`;
  }
}

function writeVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return `"${strContent(val)}"`; // with quotes for write
    case 'char': return `#\\${val.value}`;
    default: return displayVal(val);
  }
}

function makeGlobalEnv(outputBuf?: string[]): Env {
  const env = new Env();

  const numOp = (op: (a: number, b: number) => number, identity: number) =>
    ({ tag: 'procedure' as const, value: (...args: SchemeVal[]) => {
      const nums = args.map(a => {
        if (a.tag !== 'number') throw new EvalError('expected number');
        return a.value;
      });
      return { tag: 'number' as const, value: nums.reduce(op, identity) };
    }});

  env.set('+', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
  }});

  env.set('*', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
  }});

  env.set('-', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length === 0) throw new EvalError('- requires at least one argument');
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    if (nums.length === 1) return { tag: 'number' as const, value: -nums[0] };
    return { tag: 'number' as const, value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  }});

  env.set('/', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length < 2) throw new EvalError('/ requires at least two arguments');
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    return { tag: 'number' as const, value: nums.slice(1).reduce((a, b) => {
      if (b === 0) throw new EvalError('division by zero');
      return Math.trunc(a / b);
    }, nums[0]) };
  }});

  const cmpOp = (op: (a: number, b: number) => boolean) =>
    ({ tag: 'procedure' as const, value: (...args: SchemeVal[]) => {
      if (args.length < 2) throw new EvalError('comparison requires at least two arguments');
      const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
      for (let i = 0; i < nums.length - 1; i++) {
        if (!op(nums[i], nums[i + 1])) return { tag: 'boolean' as const, value: false };
      }
      return { tag: 'boolean' as const, value: true };
    }});

  env.set('<', cmpOp((a, b) => a < b));
  env.set('>', cmpOp((a, b) => a > b));
  env.set('=', cmpOp((a, b) => a === b));
  env.set('<=', cmpOp((a, b) => a <= b));
  env.set('>=', cmpOp((a, b) => a >= b));

  env.set('not', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('not requires exactly one argument');
    return { tag: 'boolean', value: isFalsy(args[0]) };
  }});

  // List primitives
  env.set('cons', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2) throw new EvalError('cons requires exactly 2 arguments');
    return { tag: 'pair', car: args[0], cdr: args[1] };
  }});

  env.set('car', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('car requires exactly 1 argument');
    if (args[0].tag === 'pair') return args[0].car;
    throw new EvalError('car: not a pair');
  }});

  env.set('cdr', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('cdr requires exactly 1 argument');
    if (args[0].tag === 'pair') return args[0].cdr;
    throw new EvalError('cdr: not a pair');
  }});

  env.set('null?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('null? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'nil' };
  }});

  env.set('list', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    let result: SchemeVal = NIL;
    for (let i = args.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: args[i], cdr: result };
    }
    return result;
  }});

  env.set('length', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('length requires exactly 1 argument');
    let count = 0;
    let cur = args[0];
    while (cur.tag === 'pair') { count++; cur = cur.cdr; }
    if (cur.tag !== 'nil') throw new EvalError('length: not a proper list');
    return { tag: 'number', value: count };
  }});

  env.set('append', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length === 0) return NIL;
    if (args.length === 1) return args[0];
    // Build result by appending all lists
    let result = args[args.length - 1];
    for (let i = args.length - 2; i >= 0; i--) {
      const elems: SchemeVal[] = [];
      let cur = args[i];
      while (cur.tag === 'pair') { elems.push(cur.car); cur = cur.cdr; }
      for (let j = elems.length - 1; j >= 0; j--) {
        result = { tag: 'pair', car: elems[j], cdr: result };
      }
    }
    return result;
  }});

  // Type predicates
  env.set('number?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('number? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'number' };
  }});

  env.set('string?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('string? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'string' };
  }});

  env.set('boolean?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('boolean? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  }});

  env.set('pair?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('pair? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'pair' };
  }});

  env.set('symbol?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('symbol? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  }});

  // I/O builtins (L05)
  env.set('display', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('display requires exactly 1 argument');
    if (outputBuf) outputBuf.push(displayVal(args[0]));
    return { tag: 'void' };
  }});

  env.set('write', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('write requires exactly 1 argument');
    if (outputBuf) outputBuf.push(writeVal(args[0]));
    return { tag: 'void' };
  }});

  env.set('newline', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (outputBuf) outputBuf.push('\n');
    return { tag: 'void' };
  }});

  // String builtins (L05)
  env.set('string-append', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    const strs = args.map(a => {
      if (a.tag !== 'string') throw new EvalError('string-append: expected string');
      return strContent(a);
    });
    return { tag: 'string', value: strs.join('') };
  }});

  env.set('string-length', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-length: expected string');
    return { tag: 'number', value: strContent(args[0]).length };
  }});

  env.set('substring', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'number')
      throw new EvalError('substring: expected string, number, number');
    return { tag: 'string', value: strContent(args[0]).substring(args[1].value, args[2].value) };
  }});

  env.set('string->number', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->number: expected string');
    const n = Number(strContent(args[0]));
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  }});

  env.set('number->string', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('number->string: expected number');
    return { tag: 'string', value: String(args[0].value) };
  }});

  env.set('symbol->string', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'symbol') throw new EvalError('symbol->string: expected symbol');
    return { tag: 'string', value: args[0].value };
  }});

  env.set('string->symbol', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->symbol: expected string');
    return { tag: 'symbol', value: args[0].value };
  }});

  env.set('string-ref', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'number')
      throw new EvalError('string-ref: expected string and number');
    const s = strContent(args[0]);
    const i = args[1].value;
    if (i < 0 || i >= s.length) throw new EvalError('string-ref: index out of range');
    return { tag: 'char', value: s[i] };
  }});

  // L06: Mutable strings
  env.set('string-copy', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-copy: expected string');
    const s = strContent(args[0]);
    return { tag: 'string', value: '', chars: [...s] };
  }});

  env.set('string-set!', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'char')
      throw new EvalError('string-set!: expected mutable string, number, char');
    const str = args[0];
    if (!str.chars) throw new EvalError('string-set!: string is immutable');
    const i = args[1].value;
    if (i < 0 || i >= str.chars.length) throw new EvalError('string-set!: index out of range');
    str.chars[i] = args[2].value;
    return { tag: 'void' };
  }});

  env.set('char?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('char? requires exactly 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'char' };
  }});

  return env;
}

// --- Evaluator ---

function isFalsy(val: SchemeVal): boolean {
  return val.tag === 'boolean' && val.value === false;
}

function posStr(p?: Pos): string {
  return p ? `${p.line}:${p.col}` : '?:?';
}

function errAt(msg: string, p?: Pos): EvalError {
  return new EvalError(`${posStr(p)}: ${msg}`);
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return expr;

    case 'symbol': {
      try {
        return env.get(expr.value);
      } catch (e) {
        if (e instanceof EvalError) throw errAt(e.message, expr.pos);
        throw e;
      }
    }

    case 'list': {
      const elems = expr.value;
      if (elems.length === 0) throw errAt('empty application', expr.pos);

      const first = elems[0];

      // Special forms
      if (first.tag === 'symbol') {
        switch (first.value) {
          case 'define': {
            if (elems.length < 3) throw errAt('define requires at least 2 arguments', expr.pos);
            const target = elems[1];
            if (target.tag === 'symbol') {
              // (define x expr)
              const val = evaluate(elems[2], env);
              env.set(target.value, val);
              return { tag: 'void' };
            }
            if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
              // (define (f params...) body...)
              const name = target.value[0].value;
              const paramNames = target.value.slice(1).map(p => {
                if (p.tag !== 'symbol') throw errAt('parameter must be a symbol', p.pos);
                return p.value;
              });
              const bodyExprs = elems.slice(2);
              const proc: SchemeVal = { tag: 'procedure', value: (...args: SchemeVal[]) => {
                const childEnv = new Env(env);
                for (let i = 0; i < paramNames.length; i++) {
                  childEnv.set(paramNames[i], args[i]);
                }
                let result: SchemeVal = { tag: 'void' };
                for (const b of bodyExprs) {
                  result = evaluate(b, childEnv);
                }
                return result;
              }};
              env.set(name, proc);
              return { tag: 'void' };
            }
            throw errAt('invalid define syntax', expr.pos);
          }
          case 'if': {
            if (elems.length < 3) throw errAt('if requires at least 2 arguments', expr.pos);
            const cond = evaluate(elems[1], env);
            if (!isFalsy(cond)) {
              return evaluate(elems[2], env);
            } else if (elems.length > 3) {
              return evaluate(elems[3], env);
            }
            return { tag: 'void' };
          }
          case 'quote': {
            if (elems.length !== 2) throw errAt('quote requires exactly 1 argument', expr.pos);
            return listToConsPairs(elems[1]);
          }
          case 'lambda': {
            if (elems.length < 3) throw errAt('lambda requires params and body', expr.pos);
            const params = elems[1];
            if (params.tag !== 'list') throw errAt('lambda params must be a list', expr.pos);
            const paramNames = params.value.map(p => {
              if (p.tag !== 'symbol') throw errAt('parameter must be a symbol', p.pos);
              return p.value;
            });
            const bodyExprs = elems.slice(2);
            return { tag: 'procedure', value: (...args: SchemeVal[]) => {
              const childEnv = new Env(env);
              for (let i = 0; i < paramNames.length; i++) {
                childEnv.set(paramNames[i], args[i]);
              }
              let result: SchemeVal = { tag: 'void' };
              for (const b of bodyExprs) {
                result = evaluate(b, childEnv);
              }
              return result;
            }};
          }
          case 'and': {
            if (elems.length === 1) return { tag: 'boolean', value: true };
            let result: SchemeVal = { tag: 'boolean', value: true };
            for (let i = 1; i < elems.length; i++) {
              result = evaluate(elems[i], env);
              if (isFalsy(result)) return result;
            }
            return result;
          }
          case 'or': {
            if (elems.length === 1) return { tag: 'boolean', value: false };
            let result: SchemeVal = { tag: 'boolean', value: false };
            for (let i = 1; i < elems.length; i++) {
              result = evaluate(elems[i], env);
              if (!isFalsy(result)) return result;
            }
            return result;
          }
          case 'begin': {
            let result: SchemeVal = { tag: 'void' };
            for (let i = 1; i < elems.length; i++) {
              result = evaluate(elems[i], env);
            }
            return result;
          }
          case 'let': {
            if (elems.length < 3) throw errAt('let requires bindings and body', expr.pos);
            // Named let: (let name ((var init) ...) body ...)
            if (elems[1].tag === 'symbol') {
              if (elems.length < 4) throw errAt('named let requires bindings and body', expr.pos);
              const loopName = elems[1].value;
              const bindingsList = elems[2];
              if (bindingsList.tag !== 'list') throw errAt('let bindings must be a list', expr.pos);
              const paramNames: string[] = [];
              const initVals: SchemeVal[] = [];
              for (const binding of bindingsList.value) {
                if (binding.tag !== 'list' || binding.value.length !== 2)
                  throw errAt('invalid let binding', binding.pos);
                if (binding.value[0].tag !== 'symbol') throw errAt('let binding name must be a symbol', binding.pos);
                paramNames.push(binding.value[0].value);
                initVals.push(evaluate(binding.value[1], env));
              }
              const bodyExprs = elems.slice(3);
              const childEnv = new Env(env);
              const loopProc: SchemeVal = { tag: 'procedure', value: (...args: SchemeVal[]) => {
                const innerEnv = new Env(env);
                for (let i = 0; i < paramNames.length; i++) {
                  innerEnv.set(paramNames[i], args[i]);
                }
                innerEnv.set(loopName, loopProc);
                let result: SchemeVal = { tag: 'void' };
                for (const b of bodyExprs) result = evaluate(b, innerEnv);
                return result;
              }};
              childEnv.set(loopName, loopProc);
              for (let i = 0; i < paramNames.length; i++) {
                childEnv.set(paramNames[i], initVals[i]);
              }
              let result: SchemeVal = { tag: 'void' };
              for (const b of bodyExprs) result = evaluate(b, childEnv);
              return result;
            }
            // Regular let: (let ((var init) ...) body ...)
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('let bindings must be a list', expr.pos);
            const childEnv = new Env(env);
            for (const binding of bindings.value) {
              if (binding.tag !== 'list' || binding.value.length !== 2)
                throw errAt('invalid let binding', binding.pos);
              const name = binding.value[0];
              if (name.tag !== 'symbol') throw errAt('let binding name must be a symbol', name.pos);
              const val = evaluate(binding.value[1], env);
              childEnv.set(name.value, val);
            }
            let result: SchemeVal = { tag: 'void' };
            for (let i = 2; i < elems.length; i++) {
              result = evaluate(elems[i], childEnv);
            }
            return result;
          }
          case 'cond': {
            for (let i = 1; i < elems.length; i++) {
              const clause = elems[i];
              if (clause.tag !== 'list' || clause.value.length < 1)
                throw errAt('invalid cond clause', clause.pos);
              const test = clause.value[0];
              if (test.tag === 'symbol' && test.value === 'else') {
                let result: SchemeVal = { tag: 'void' };
                for (let j = 1; j < clause.value.length; j++) {
                  result = evaluate(clause.value[j], env);
                }
                return result;
              }
              const testVal = evaluate(test, env);
              if (!isFalsy(testVal)) {
                if (clause.value.length === 1) return testVal;
                let result: SchemeVal = { tag: 'void' };
                for (let j = 1; j < clause.value.length; j++) {
                  result = evaluate(clause.value[j], env);
                }
                return result;
              }
            }
            return { tag: 'void' };
          }
        }
      }

      // Function application
      const func = evaluate(first, env);
      if (func.tag !== 'procedure') throw errAt('not a procedure', expr.pos);
      const args = elems.slice(1).map(a => evaluate(a, env));
      try {
        return func.value(...args);
      } catch (e) {
        if (e instanceof EvalError && !/^\d+:/.test(e.message)) {
          throw errAt(e.message, expr.pos);
        }
        throw e;
      }
    }

    default:
      throw errAt('cannot evaluate', expr.pos);
  }
}

function display(val: SchemeVal): string {
  return writeVal(val);
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return display(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return { result: display(result), output: outputBuf.join('') };
}
