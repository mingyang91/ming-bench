import { EvalError } from './evalError.js';

// --- Types ---

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }
  | { tag: 'builtin'; name: string; func: (args: SchemeVal[], callPos?: Pos) => SchemeVal; pos?: Pos }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'void'; pos?: Pos };

// --- Parser ---

interface Token { text: string; pos: Pos }

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
    if (/\s/.test(ch)) { advance(); continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') advance(); continue; }
    if (ch === '(' || ch === ')') { tokens.push({ text: ch, pos: { line, col } }); advance(); continue; }
    if (ch === '"') {
      const startPos = { line, col };
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; advance(); if (i < input.length) { s += input[i]; advance(); } }
        else { s += input[i]; advance(); }
      }
      if (i < input.length) { s += '"'; advance(); }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    if (ch === "'") { tokens.push({ text: "'", pos: { line, col } }); advance(); continue; }
    const startPos = { line, col };
    let atom = '';
    while (i < input.length && !/[\s();]/.test(input[i])) { atom += input[i]; advance(); }
    if (atom.length > 0) tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parse(tokens: Token[], idx: number): [SchemeVal, number] {
  if (idx >= tokens.length) throw new EvalError('unexpected end of input');
  const token = tokens[idx];
  const p = token.pos;

  if (token.text === '(') {
    const elements: SchemeVal[] = [];
    idx++;
    while (idx < tokens.length && tokens[idx].text !== ')') {
      const [val, next] = parse(tokens, idx);
      elements.push(val);
      idx = next;
    }
    if (idx >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'list', elements, pos: p }, idx + 1];
  }

  if (token.text === ')') throw new EvalError('unexpected )');

  if (token.text === "'") {
    const [val, next] = parse(tokens, idx + 1);
    return [{ tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: p }, val], pos: p }, next];
  }

  if (token.text === '#t') return [{ tag: 'boolean', value: true, pos: p }, idx + 1];
  if (token.text === '#f') return [{ tag: 'boolean', value: false, pos: p }, idx + 1];

  if (token.text.startsWith('#\\')) {
    const rest = token.text.slice(2);
    let ch: string;
    if (rest === 'space') ch = ' ';
    else if (rest === 'newline') ch = '\n';
    else if (rest === 'tab') ch = '\t';
    else if (rest.length === 1) ch = rest;
    else throw new EvalError(`invalid character literal: ${token.text}`);
    return [{ tag: 'char', value: ch, pos: p }, idx + 1];
  }

  if (token.text.startsWith('"')) {
    const inner = token.text.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\t/g, '\t')
      .replace(/\\"/g, '"')
      .replace(/\\\\/g, '\\');
    return [{ tag: 'string', value: inner, pos: p }, idx + 1];
  }

  const num = Number(token.text);
  if (!isNaN(num) && token.text !== '') {
    return [{ tag: 'number', value: num, pos: p }, idx + 1];
  }

  return [{ tag: 'symbol', value: token.text, pos: p }, idx + 1];
}

function parseAll(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const exprs: SchemeVal[] = [];
  let pos = 0;
  while (pos < tokens.length) {
    const [val, next] = parse(tokens, pos);
    exprs.push(val);
    pos = next;
  }
  return exprs;
}

// --- Helpers ---

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function schemeToString(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
    case 'list': return `(${val.elements.map(schemeToString).join(' ')})`;
    case 'builtin': return `#<procedure:${val.name}>`;
    case 'lambda': return '#<procedure>';
    case 'void': return '';
  }
}

function displayString(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return val.value;
    case 'char': return val.value;
    case 'list': return `(${val.elements.map(displayString).join(' ')})`;
    default: return schemeToString(val);
  }
}

function fmtPos(p?: Pos): string {
  return p ? `${p.line}:${p.col}: ` : '';
}

function expectNumbers(args: SchemeVal[], name: string, pos?: Pos): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${fmtPos(pos)}${name}: expected number, got ${schemeToString(a)}`);
    return a.value;
  });
}

// --- Env ---

interface Env {
  bindings: Map<string, SchemeVal>;
  parent: Env | null;
}

function envLookup(env: Env, name: string): SchemeVal | undefined {
  let cur: Env | null = env;
  while (cur) {
    const val = cur.bindings.get(name);
    if (val !== undefined) return val;
    cur = cur.parent;
  }
  return undefined;
}

function envSet(env: Env, name: string, val: SchemeVal): void {
  let cur: Env | null = env;
  while (cur) {
    if (cur.bindings.has(name)) { cur.bindings.set(name, val); return; }
    cur = cur.parent;
  }
  throw new EvalError(`set!: unbound variable: ${name}`);
}

function envDefine(env: Env, name: string, val: SchemeVal): void {
  env.bindings.set(name, val);
}

function childEnv(parent: Env): Env {
  return { bindings: new Map(), parent };
}

function makeGlobalEnv(outputBuf: string[]): Env {
  const env: Env = { bindings: new Map(), parent: null };

  const defBuiltin = (name: string, func: (args: SchemeVal[], callPos?: Pos) => SchemeVal) => {
    envDefine(env, name, { tag: 'builtin', name, func });
  };

  defBuiltin('+', (args, p) => {
    const nums = expectNumbers(args, '+', p);
    return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
  });

  defBuiltin('-', (args, p) => {
    if (args.length === 0) throw new EvalError(`${fmtPos(p)}-: need at least 1 arg`);
    const nums = expectNumbers(args, '-', p);
    if (nums.length === 1) return { tag: 'number', value: -nums[0] };
    return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  });

  defBuiltin('*', (args, p) => {
    const nums = expectNumbers(args, '*', p);
    return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
  });

  defBuiltin('/', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}/: expected 2 args`);
    const nums = expectNumbers(args, '/', p);
    if (nums[1] === 0) throw new EvalError(`${fmtPos(p)}division by zero`);
    return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
  });

  defBuiltin('<', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}<: expected 2 args`);
    const nums = expectNumbers(args, '<', p);
    return { tag: 'boolean', value: nums[0] < nums[1] };
  });

  defBuiltin('>', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}>: expected 2 args`);
    const nums = expectNumbers(args, '>', p);
    return { tag: 'boolean', value: nums[0] > nums[1] };
  });

  defBuiltin('=', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}=: expected 2 args`);
    const nums = expectNumbers(args, '=', p);
    return { tag: 'boolean', value: nums[0] === nums[1] };
  });

  defBuiltin('<=', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}<=: expected 2 args`);
    const nums = expectNumbers(args, '<=', p);
    return { tag: 'boolean', value: nums[0] <= nums[1] };
  });

  defBuiltin('>=', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}>=: expected 2 args`);
    const nums = expectNumbers(args, '>=', p);
    return { tag: 'boolean', value: nums[0] >= nums[1] };
  });

  // List operations
  defBuiltin('cons', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}cons: expected 2 args`);
    const [head, tail] = args;
    if (tail.tag === 'list') {
      return { tag: 'list', elements: [head, ...tail.elements] };
    }
    return { tag: 'list', elements: [head, tail] };
  });

  defBuiltin('car', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}car: expected 1 arg`);
    if (args[0].tag !== 'list' || args[0].elements.length === 0)
      throw new EvalError(`${fmtPos(p)}car: expected non-empty list`);
    return args[0].elements[0];
  });

  defBuiltin('cdr', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}cdr: expected 1 arg`);
    if (args[0].tag !== 'list' || args[0].elements.length === 0)
      throw new EvalError(`${fmtPos(p)}cdr: expected non-empty list`);
    return { tag: 'list', elements: args[0].elements.slice(1) };
  });

  defBuiltin('null?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}null?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length === 0 };
  });

  defBuiltin('list', (args) => {
    return { tag: 'list', elements: args };
  });

  defBuiltin('length', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}length: expected 1 arg`);
    if (args[0].tag !== 'list') throw new EvalError(`${fmtPos(p)}length: expected list`);
    return { tag: 'number', value: args[0].elements.length };
  });

  defBuiltin('append', (args, p) => {
    const result: SchemeVal[] = [];
    for (const arg of args) {
      if (arg.tag !== 'list') throw new EvalError(`${fmtPos(p)}append: expected list`);
      result.push(...arg.elements);
    }
    return { tag: 'list', elements: result };
  });

  // Type predicates
  defBuiltin('number?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}number?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'number' };
  });

  defBuiltin('string?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'string' };
  });

  defBuiltin('boolean?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}boolean?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  });

  defBuiltin('pair?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}pair?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length > 0 };
  });

  defBuiltin('symbol?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}symbol?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  // L05: Display/Write/Newline
  defBuiltin('display', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}display: expected 1 arg`);
    outputBuf.push(displayString(args[0]));
    return { tag: 'void' };
  });

  defBuiltin('write', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}write: expected 1 arg`);
    outputBuf.push(schemeToString(args[0]));
    return { tag: 'void' };
  });

  defBuiltin('newline', (args, p) => {
    if (args.length !== 0) throw new EvalError(`${fmtPos(p)}newline: expected 0 args`);
    outputBuf.push('\n');
    return { tag: 'void' };
  });

  // L05: String operations
  defBuiltin('string-append', (args, p) => {
    const strs = args.map(a => {
      if (a.tag !== 'string') throw new EvalError(`${fmtPos(p)}string-append: expected string, got ${schemeToString(a)}`);
      return a.value;
    });
    return { tag: 'string', value: strs.join('') };
  });

  defBuiltin('string-length', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string-length: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-length: expected string`);
    return { tag: 'number', value: args[0].value.length };
  });

  defBuiltin('substring', (args, p) => {
    if (args.length !== 3) throw new EvalError(`${fmtPos(p)}substring: expected 3 args`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}substring: expected string`);
    const nums = expectNumbers([args[1], args[2]], 'substring', p);
    return { tag: 'string', value: args[0].value.slice(nums[0], nums[1]) };
  });

  defBuiltin('string->number', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string->number: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string->number: expected string`);
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  });

  defBuiltin('number->string', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}number->string: expected 1 arg`);
    if (args[0].tag !== 'number') throw new EvalError(`${fmtPos(p)}number->string: expected number`);
    return { tag: 'string', value: String(args[0].value) };
  });

  // L05: Symbol/String conversion
  defBuiltin('symbol->string', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}symbol->string: expected 1 arg`);
    if (args[0].tag !== 'symbol') throw new EvalError(`${fmtPos(p)}symbol->string: expected symbol`);
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('string->symbol', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string->symbol: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string->symbol: expected string`);
    return { tag: 'symbol', value: args[0].value };
  });

  // L05: Character operations
  defBuiltin('string-ref', (args, p) => {
    if (args.length !== 2) throw new EvalError(`${fmtPos(p)}string-ref: expected 2 args`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-ref: expected string`);
    if (args[1].tag !== 'number') throw new EvalError(`${fmtPos(p)}string-ref: expected number`);
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].value.length) throw new EvalError(`${fmtPos(p)}string-ref: index out of range`);
    return { tag: 'char', value: args[0].value[idx] };
  });

  defBuiltin('string-copy', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}string-copy: expected 1 arg`);
    if (args[0].tag !== 'string') throw new EvalError(`${fmtPos(p)}string-copy: expected string`);
    return { tag: 'string', value: args[0].value };
  });

  defBuiltin('char?', (args, p) => {
    if (args.length !== 1) throw new EvalError(`${fmtPos(p)}char?: expected 1 arg`);
    return { tag: 'boolean', value: args[0].tag === 'char' };
  });

  return env;
}

// --- Eval ---

function evalScheme(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return expr;

    case 'symbol': {
      const val = envLookup(env, expr.value);
      if (val === undefined) throw new EvalError(`${fmtPos(expr.pos)}unbound variable: ${expr.value}`);
      return val;
    }

    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw new EvalError(`${fmtPos(expr.pos)}empty application`);

      if (elems[0].tag === 'symbol') {
        const name = elems[0].value;

        if (name === 'quote') {
          if (elems.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}quote: wrong argument count`);
          return elems[1];
        }

        if (name === 'if') {
          if (elems.length < 3 || elems.length > 4) throw new EvalError(`${fmtPos(expr.pos)}if: wrong argument count`);
          const cond = evalScheme(elems[1], env);
          if (isTruthy(cond)) return evalScheme(elems[2], env);
          if (elems.length === 4) return evalScheme(elems[3], env);
          return { tag: 'void' };
        }

        if (name === 'define') {
          if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}define: wrong argument count`);
          if (elems[1].tag === 'symbol') {
            const val = evalScheme(elems[2], env);
            envDefine(env, elems[1].value, val);
            return { tag: 'void' };
          }
          if (elems[1].tag === 'list' && elems[1].elements.length > 0 && elems[1].elements[0].tag === 'symbol') {
            const fnName = elems[1].elements[0].value;
            const params = elems[1].elements.slice(1).map(p => {
              if (p.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}define: parameter must be a symbol`);
              return p.value;
            });
            const lambda: SchemeVal = { tag: 'lambda', params, body: elems.slice(2), env };
            envDefine(env, fnName, lambda);
            return { tag: 'void' };
          }
          throw new EvalError(`${fmtPos(expr.pos)}define: invalid syntax`);
        }

        if (name === 'lambda') {
          if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}lambda: wrong argument count`);
          if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}lambda: params must be a list`);
          const params = elems[1].elements.map(p => {
            if (p.tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}lambda: parameter must be a symbol`);
            return p.value;
          });
          return { tag: 'lambda', params, body: elems.slice(2), env };
        }

        if (name === 'and') {
          let result: SchemeVal = { tag: 'boolean', value: true };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
            if (!isTruthy(result)) return result;
          }
          return result;
        }

        if (name === 'or') {
          let result: SchemeVal = { tag: 'boolean', value: false };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
            if (isTruthy(result)) return result;
          }
          return result;
        }

        if (name === 'not') {
          if (elems.length !== 2) throw new EvalError(`${fmtPos(expr.pos)}not: wrong argument count`);
          const val = evalScheme(elems[1], env);
          return { tag: 'boolean', value: !isTruthy(val) };
        }

        if (name === 'begin') {
          let result: SchemeVal = { tag: 'void' };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
          }
          return result;
        }

        if (name === 'cond') {
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.elements.length < 2)
              throw new EvalError(`${fmtPos(expr.pos)}cond: invalid clause`);
            if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.elements.length; j++) {
                result = evalScheme(clause.elements[j], env);
              }
              return result;
            }
            const test = evalScheme(clause.elements[0], env);
            if (isTruthy(test)) {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.elements.length; j++) {
                result = evalScheme(clause.elements[j], env);
              }
              return result;
            }
          }
          return { tag: 'void' };
        }

        if (name === 'set!') {
          if (elems.length !== 3) throw new EvalError(`${fmtPos(expr.pos)}set!: wrong argument count`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}set!: first arg must be a symbol`);
          const val = evalScheme(elems[2], env);
          envSet(env, elems[1].value, val);
          return { tag: 'void' };
        }

        if (name === 'string-set!') {
          if (elems.length !== 4) throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected 3 args`);
          if (elems[1].tag !== 'symbol') throw new EvalError(`${fmtPos(expr.pos)}string-set!: first arg must be a variable`);
          const strVal = envLookup(env, elems[1].value);
          if (!strVal || strVal.tag !== 'string') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected string variable`);
          const idx = evalScheme(elems[2], env);
          if (idx.tag !== 'number') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected number index`);
          const ch = evalScheme(elems[3], env);
          if (ch.tag !== 'char') throw new EvalError(`${fmtPos(expr.pos)}string-set!: expected char`);
          const s = strVal.value;
          const i = idx.value;
          if (i < 0 || i >= s.length) throw new EvalError(`${fmtPos(expr.pos)}string-set!: index out of range`);
          envSet(env, elems[1].value, { tag: 'string', value: s.slice(0, i) + ch.value + s.slice(i + 1) });
          return { tag: 'void' };
        }

        if (name === 'let') {
          if (elems.length < 3) throw new EvalError(`${fmtPos(expr.pos)}let: wrong argument count`);
          // Named let: (let name ((var init) ...) body ...)
          if (elems[1].tag === 'symbol') {
            const loopName = elems[1].value;
            if (elems[2].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}let: bindings must be a list`);
            const bindings = elems[2].elements;
            const params: string[] = [];
            const inits: SchemeVal[] = [];
            for (const b of bindings) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw new EvalError(`${fmtPos(expr.pos)}let: invalid binding`);
              params.push(b.elements[0].value);
              inits.push(evalScheme(b.elements[1], env));
            }
            const body = elems.slice(3);
            const loopEnv = childEnv(env);
            const lambda: SchemeVal = { tag: 'lambda', params, body, env: loopEnv };
            envDefine(loopEnv, loopName, lambda);
            // Call with initial values
            const callEnv = childEnv(loopEnv);
            for (let i = 0; i < params.length; i++) {
              envDefine(callEnv, params[i], inits[i]);
            }
            let result: SchemeVal = { tag: 'void' };
            for (const bodyExpr of body) {
              result = evalScheme(bodyExpr, callEnv);
            }
            return result;
          }
          // Regular let: (let ((var init) ...) body ...)
          if (elems[1].tag !== 'list') throw new EvalError(`${fmtPos(expr.pos)}let: bindings must be a list`);
          const letEnv = childEnv(env);
          for (const binding of elems[1].elements) {
            if (binding.tag !== 'list' || binding.elements.length !== 2 || binding.elements[0].tag !== 'symbol')
              throw new EvalError(`${fmtPos(expr.pos)}let: invalid binding`);
            const val = evalScheme(binding.elements[1], env);
            envDefine(letEnv, binding.elements[0].value, val);
          }
          let letResult: SchemeVal = { tag: 'void' };
          for (let i = 2; i < elems.length; i++) {
            letResult = evalScheme(elems[i], letEnv);
          }
          return letResult;
        }
      }

      // Function application
      const func = evalScheme(elems[0], env);
      const args = elems.slice(1).map(a => evalScheme(a, env));

      if (func.tag === 'builtin') {
        return func.func(args, expr.pos);
      }

      if (func.tag === 'lambda') {
        if (args.length !== func.params.length) throw new EvalError(`${fmtPos(expr.pos)}wrong number of arguments`);
        const callEnv = childEnv(func.env);
        for (let i = 0; i < func.params.length; i++) {
          envDefine(callEnv, func.params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of func.body) {
          result = evalScheme(bodyExpr, callEnv);
        }
        return result;
      }

      throw new EvalError(`${fmtPos(expr.pos)}not a procedure: ${schemeToString(func)}`);
    }

    case 'builtin':
    case 'lambda':
    case 'void':
      return expr;
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evalScheme(expr, env);
  }
  return schemeToString(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const outputBuf: string[] = [];
  const env = makeGlobalEnv(outputBuf);
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evalScheme(expr, env);
  }
  return { result: schemeToString(result), output: outputBuf.join('') };
}
