import { EvalError } from './evalError.js';

// --- Types ---

interface Pos { line: number; col: number }

type SchemeValBase =
  | { tag: 'number'; value: number; exact?: boolean }
  | { tag: 'rational'; num: number; den: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string; chars?: string[] }
  | { tag: 'symbol'; value: string }
  | { tag: 'char'; value: string }
  | { tag: 'list'; value: SchemeVal[] }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal }
  | { tag: 'nil' }
  | { tag: 'procedure'; value: (...args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' }
  | { tag: 'macro'; literals: string[]; rules: { pattern: SchemeVal; template: SchemeVal }[]; defEnv: Env };

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

// --- Rational helpers ---

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { [a, b] = [b, a % b]; }
  return a;
}

function makeRational(num: number, den: number): SchemeVal {
  if (den === 0) throw new EvalError('division by zero');
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(Math.abs(num), den);
  num /= g; den /= g;
  if (den === 1) return { tag: 'number', value: num };
  return { tag: 'rational', num, den };
}

function isExact(v: SchemeVal): boolean {
  if (v.tag === 'rational') return true;
  if (v.tag === 'number') return v.exact !== false && Number.isInteger(v.value);
  return false;
}

function toFloat(v: SchemeVal): number {
  if (v.tag === 'number') return v.value;
  if (v.tag === 'rational') return v.num / v.den;
  throw new EvalError('expected number');
}

function toRational(v: SchemeVal): { num: number; den: number } {
  if (v.tag === 'rational') return { num: v.num, den: v.den };
  if (v.tag === 'number') return { num: v.value, den: 1 };
  throw new EvalError('expected number');
}

function isNumeric(v: SchemeVal): boolean {
  return v.tag === 'number' || v.tag === 'rational';
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
        if (tokens[pos].type === 'dot') {
          pos++; // skip dot
          const cdr = parseExpr();
          if (pos >= tokens.length || tokens[pos].type !== 'rparen')
            throw new EvalError('expected ) after dot expression');
          pos++; // skip )
          let result: SchemeVal = cdr;
          for (let i = elements.length - 1; i >= 0; i--) {
            result = { tag: 'pair', car: elements[i], cdr: result };
          }
          if (result.tag === 'pair' || result.tag === 'symbol') {
            (result as any).pos = p;
          }
          return result;
        }
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
    if (/^-?\d+\/[1-9]\d*$/.test(v)) {
      const idx = v.indexOf('/');
      const r = makeRational(parseInt(v.substring(0, idx), 10), parseInt(v.substring(idx + 1), 10));
      r.pos = p;
      return r;
    }
    if (/^-?\d+$/.test(v)) return { tag: 'number', value: parseInt(v, 10), pos: p };
    if (/^-?(\d+\.\d*|\d*\.\d+)$/.test(v)) return { tag: 'number', value: parseFloat(v), exact: false, pos: p };
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

  update(name: string, value: SchemeVal): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, value);
      return;
    }
    if (this.parent) {
      this.parent.update(name, value);
      return;
    }
    throw new EvalError(`unbound variable: ${name}`);
  }
}

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': {
      if (val.exact === false && Number.isInteger(val.value)) return `${val.value}.0`;
      return String(val.value);
    }
    case 'rational': return `${val.num}/${val.den}`;
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
    case 'macro': return '#<macro>';
    case 'list': return `(${val.value.map(displayVal).join(' ')})`;
  }
}

function writeVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return `"${strContent(val)}"`; // with quotes for write
    case 'char': {
      if (val.value === ' ') return '#\\space';
      if (val.value === '\n') return '#\\newline';
      if (val.value === '\t') return '#\\tab';
      return `#\\${val.value}`;
    }
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
    for (const a of args) if (!isNumeric(a)) throw new EvalError('expected number');
    if (args.length === 0) return { tag: 'number', value: 0 };
    if (args.every(isExact)) {
      let rn = 0, rd = 1;
      for (const a of args) {
        const r = toRational(a);
        rn = rn * r.den + r.num * rd;
        rd = rd * r.den;
        const g = gcd(Math.abs(rn), rd);
        rn /= g; rd /= g;
      }
      return makeRational(rn, rd);
    }
    return { tag: 'number', value: args.reduce((s, a) => s + toFloat(a), 0), exact: false };
  }});

  env.set('*', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    for (const a of args) if (!isNumeric(a)) throw new EvalError('expected number');
    if (args.length === 0) return { tag: 'number', value: 1 };
    if (args.every(isExact)) {
      let rn = 1, rd = 1;
      for (const a of args) {
        const r = toRational(a);
        rn *= r.num; rd *= r.den;
        const g = gcd(Math.abs(rn), rd);
        rn /= g; rd /= g;
      }
      return makeRational(rn, rd);
    }
    return { tag: 'number', value: args.reduce((s, a) => s * toFloat(a), 1), exact: false };
  }});

  env.set('-', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length === 0) throw new EvalError('- requires at least one argument');
    for (const a of args) if (!isNumeric(a)) throw new EvalError('expected number');
    if (args.every(isExact)) {
      if (args.length === 1) { const r = toRational(args[0]); return makeRational(-r.num, r.den); }
      let { num: rn, den: rd } = toRational(args[0]);
      for (let i = 1; i < args.length; i++) {
        const r = toRational(args[i]);
        rn = rn * r.den - r.num * rd;
        rd = rd * r.den;
        const g = gcd(Math.abs(rn), rd); rn /= g; rd /= g;
      }
      return makeRational(rn, rd);
    }
    const nums = args.map(toFloat);
    if (nums.length === 1) return { tag: 'number' as const, value: -nums[0], exact: false };
    return { tag: 'number' as const, value: nums.slice(1).reduce((a, b) => a - b, nums[0]), exact: false };
  }});

  env.set('/', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length < 2) throw new EvalError('/ requires at least two arguments');
    for (const a of args) if (!isNumeric(a)) throw new EvalError('expected number');
    if (args.every(isExact)) {
      let { num: rn, den: rd } = toRational(args[0]);
      for (let i = 1; i < args.length; i++) {
        const r = toRational(args[i]);
        if (r.num === 0) throw new EvalError('division by zero');
        rn *= r.den; rd *= r.num;
        if (rd < 0) { rn = -rn; rd = -rd; }
        const g = gcd(Math.abs(rn), rd); rn /= g; rd /= g;
      }
      return makeRational(rn, rd);
    }
    const nums = args.map(toFloat);
    return { tag: 'number' as const, value: nums.slice(1).reduce((a, b) => {
      if (b === 0) throw new EvalError('division by zero');
      return a / b;
    }, nums[0]), exact: false };
  }});

  const cmpOp = (op: (a: number, b: number) => boolean) =>
    ({ tag: 'procedure' as const, value: (...args: SchemeVal[]) => {
      if (args.length < 2) throw new EvalError('comparison requires at least two arguments');
      const nums = args.map(a => { if (!isNumeric(a)) throw new EvalError('expected number'); return toFloat(a); });
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
    return { tag: 'boolean', value: isNumeric(args[0]) };
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

  // eq? and equal?
  env.set('eq?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2) throw new EvalError('eq? requires exactly 2 arguments');
    const [a, b] = args;
    if (a.tag !== b.tag) return { tag: 'boolean', value: false };
    if (a.tag === 'nil') return { tag: 'boolean', value: true };
    if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'rational' && b.tag === 'rational') return { tag: 'boolean', value: a.num === b.num && a.den === b.den };
    if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', value: a.value === b.value };
    if (a.tag === 'string' && b.tag === 'string') return { tag: 'boolean', value: a === b };
    return { tag: 'boolean', value: a === b };
  }});

  const schemeEqual = (a: SchemeVal, b: SchemeVal): boolean => {
    if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
    if (a.tag !== b.tag) return false;
    if (a.tag === 'nil') return true;
    if (a.tag === 'number' && b.tag === 'number') return a.value === b.value;
    if (a.tag === 'boolean' && b.tag === 'boolean') return a.value === b.value;
    if (a.tag === 'symbol' && b.tag === 'symbol') return a.value === b.value;
    if (a.tag === 'char' && b.tag === 'char') return a.value === b.value;
    if (a.tag === 'string' && b.tag === 'string') return strContent(a) === strContent(b as SchemeVal & { tag: 'string' });
    if (a.tag === 'pair' && b.tag === 'pair') return schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
    return a === b;
  };

  env.set('equal?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2) throw new EvalError('equal? requires exactly 2 arguments');
    return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
  }});

  // map (supports multiple lists)
  env.set('map', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length < 2) throw new EvalError('map requires at least 2 arguments');
    const func = args[0];
    if (func.tag !== 'procedure') throw new EvalError('map: first argument must be a procedure');
    const lists = args.slice(1);
    const results: SchemeVal[] = [];
    // Convert lists to arrays of pairs for iteration
    let cursors: SchemeVal[] = lists;
    while (true) {
      // Check if any list is exhausted
      if (cursors.some(c => c.tag === 'nil')) break;
      if (cursors.some(c => c.tag !== 'pair')) throw new EvalError('map: expected proper list');
      const callArgs = cursors.map(c => (c as any).car as SchemeVal);
      results.push(func.value(...callArgs));
      cursors = cursors.map(c => (c as any).cdr as SchemeVal);
    }
    let result: SchemeVal = NIL;
    for (let i = results.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: results[i], cdr: result };
    }
    return result;
  }});

  // for-each (like map but discards results)
  env.set('for-each', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length < 2) throw new EvalError('for-each requires at least 2 arguments');
    const func = args[0];
    if (func.tag !== 'procedure') throw new EvalError('for-each: first argument must be a procedure');
    const lists = args.slice(1);
    let cursors: SchemeVal[] = lists;
    while (true) {
      if (cursors.some(c => c.tag === 'nil')) break;
      if (cursors.some(c => c.tag !== 'pair')) throw new EvalError('for-each: expected proper list');
      const callArgs = cursors.map(c => (c as any).car as SchemeVal);
      func.value(...callArgs);
      cursors = cursors.map(c => (c as any).cdr as SchemeVal);
    }
    return { tag: 'void' };
  }});

  // L09: Numeric utilities
  env.set('abs', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('abs: expected number');
    return { tag: 'number', value: Math.abs(args[0].value) };
  }});

  env.set('modulo', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
      throw new EvalError('modulo: expected two numbers');
    const [a, b] = [args[0].value, args[1].value];
    if (b === 0) throw new EvalError('modulo: division by zero');
    return { tag: 'number', value: ((a % b) + b) % b };
  }});

  env.set('remainder', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
      throw new EvalError('remainder: expected two numbers');
    const [a, b] = [args[0].value, args[1].value];
    if (b === 0) throw new EvalError('remainder: division by zero');
    return { tag: 'number', value: a % b };
  }});

  env.set('quotient', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
      throw new EvalError('quotient: expected two numbers');
    const [a, b] = [args[0].value, args[1].value];
    if (b === 0) throw new EvalError('quotient: division by zero');
    return { tag: 'number', value: Math.trunc(a / b) };
  }});

  env.set('min', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length === 0) throw new EvalError('min: requires at least one argument');
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('min: expected number'); return a.value; });
    return { tag: 'number', value: Math.min(...nums) };
  }});

  env.set('max', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length === 0) throw new EvalError('max: requires at least one argument');
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('max: expected number'); return a.value; });
    return { tag: 'number', value: Math.max(...nums) };
  }});

  env.set('expt', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'number' || args[1].tag !== 'number')
      throw new EvalError('expt: expected two numbers');
    return { tag: 'number', value: Math.pow(args[0].value, args[1].value) };
  }});

  // L09: Numeric predicates
  env.set('zero?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('zero?: expected number');
    return { tag: 'boolean', value: args[0].value === 0 };
  }});

  env.set('positive?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('positive?: expected number');
    return { tag: 'boolean', value: args[0].value > 0 };
  }});

  env.set('negative?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('negative?: expected number');
    return { tag: 'boolean', value: args[0].value < 0 };
  }});

  env.set('odd?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('odd?: expected number');
    return { tag: 'boolean', value: args[0].value % 2 !== 0 };
  }});

  env.set('even?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('even?: expected number');
    return { tag: 'boolean', value: args[0].value % 2 === 0 };
  }});

  // L11: Exact arithmetic & rationals
  env.set('exact?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('exact? requires exactly 1 argument');
    return { tag: 'boolean', value: isExact(args[0]) };
  }});

  env.set('inexact?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('inexact? requires exactly 1 argument');
    return { tag: 'boolean', value: isNumeric(args[0]) && !isExact(args[0]) };
  }});

  env.set('exact->inexact', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || !isNumeric(args[0])) throw new EvalError('exact->inexact: expected number');
    return { tag: 'number', value: toFloat(args[0]), exact: false };
  }});

  env.set('inexact->exact', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || !isNumeric(args[0])) throw new EvalError('inexact->exact: expected number');
    if (isExact(args[0])) return args[0];
    const x = toFloat(args[0]);
    if (Number.isInteger(x)) return { tag: 'number', value: x };
    const str = x.toString();
    const decIdx = str.indexOf('.');
    if (decIdx === -1) return { tag: 'number', value: x };
    const decimals = str.length - decIdx - 1;
    const den = Math.pow(10, decimals);
    const num = Math.round(x * den);
    const g = gcd(Math.abs(num), den);
    return makeRational(num / g, den / g);
  }});

  env.set('numerator', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || !isNumeric(args[0])) throw new EvalError('numerator: expected number');
    const r = toRational(args[0]);
    return { tag: 'number', value: r.num };
  }});

  env.set('denominator', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || !isNumeric(args[0])) throw new EvalError('denominator: expected number');
    const r = toRational(args[0]);
    return { tag: 'number', value: r.den };
  }});

  env.set('integer?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('integer? requires exactly 1 argument');
    if (args[0].tag === 'number') return { tag: 'boolean', value: Number.isInteger(args[0].value) };
    if (args[0].tag === 'rational') return { tag: 'boolean', value: false };
    return { tag: 'boolean', value: false };
  }});

  env.set('rational?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('rational? requires exactly 1 argument');
    return { tag: 'boolean', value: isNumeric(args[0]) && isExact(args[0]) };
  }});

  // L09: List utilities
  env.set('list-ref', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[1].tag !== 'number') throw new EvalError('list-ref: expected list and number');
    let cur = args[0];
    let idx = args[1].value;
    while (idx > 0 && cur.tag === 'pair') { cur = cur.cdr; idx--; }
    if (cur.tag !== 'pair') throw new EvalError('list-ref: index out of range');
    return cur.car;
  }});

  env.set('list-tail', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[1].tag !== 'number') throw new EvalError('list-tail: expected list and number');
    let cur = args[0];
    let idx = args[1].value;
    while (idx > 0) {
      if (cur.tag !== 'pair') throw new EvalError('list-tail: index out of range');
      cur = cur.cdr;
      idx--;
    }
    return cur;
  }});

  env.set('list?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('list? requires exactly 1 argument');
    let cur = args[0];
    while (cur.tag === 'pair') cur = cur.cdr;
    return { tag: 'boolean', value: cur.tag === 'nil' };
  }});

  // L09: assoc
  env.set('assoc', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2) throw new EvalError('assoc requires exactly 2 arguments');
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      const entry = cur.car;
      if (entry.tag === 'pair' && schemeEqual(entry.car, key)) return entry;
      cur = cur.cdr;
    }
    return { tag: 'boolean', value: false };
  }});

  // L09: Character utilities
  env.set('char-alphabetic?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-alphabetic?: expected char');
    return { tag: 'boolean', value: /^[a-zA-Z]$/.test(args[0].value) };
  }});

  env.set('char-numeric?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-numeric?: expected char');
    return { tag: 'boolean', value: /^[0-9]$/.test(args[0].value) };
  }});

  env.set('char-upcase', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-upcase: expected char');
    return { tag: 'char', value: args[0].value.toUpperCase() };
  }});

  env.set('char-downcase', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'char') throw new EvalError('char-downcase: expected char');
    return { tag: 'char', value: args[0].value.toLowerCase() };
  }});

  env.set('char=?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char')
      throw new EvalError('char=?: expected two chars');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  }});

  env.set('char<?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char')
      throw new EvalError('char<?: expected two chars');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  }});

  // L09: String comparison/conversion
  env.set('string=?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
      throw new EvalError('string=?: expected two strings');
    return { tag: 'boolean', value: strContent(args[0]) === strContent(args[1]) };
  }});

  env.set('string<?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
      throw new EvalError('string<?: expected two strings');
    return { tag: 'boolean', value: strContent(args[0]) < strContent(args[1]) };
  }});

  env.set('string-ci=?', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
      throw new EvalError('string-ci=?: expected two strings');
    return { tag: 'boolean', value: strContent(args[0]).toLowerCase() === strContent(args[1]).toLowerCase() };
  }});

  env.set('string-upcase', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-upcase: expected string');
    return { tag: 'string', value: strContent(args[0]).toUpperCase() };
  }});

  env.set('string-downcase', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-downcase: expected string');
    return { tag: 'string', value: strContent(args[0]).toLowerCase() };
  }});

  // L08: apply
  env.set('apply', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length < 2) throw new EvalError('apply requires at least 2 arguments');
    const func = args[0];
    if (func.tag !== 'procedure') throw new EvalError('apply: first argument must be a procedure');
    // Last argument must be a list, prefix args come before it
    const lastArg = args[args.length - 1];
    const prefixArgs = args.slice(1, args.length - 1);
    // Convert last arg (pair list) to array
    const listArgs: SchemeVal[] = [];
    let cur: SchemeVal = lastArg;
    while (cur.tag === 'pair') {
      listArgs.push(cur.car);
      cur = cur.cdr;
    }
    if (cur.tag !== 'nil') throw new EvalError('apply: last argument must be a proper list');
    const allArgs = [...prefixArgs, ...listArgs];
    return func.value(...allArgs);
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

// --- Macro support (L10) ---

let gensymCounter = 0;
const resolvedSymbols = new Map<string, { name: string; env: Env }>();

function gensym(base: string): string {
  return `__gs_${base}_${gensymCounter++}`;
}

const SPECIAL_FORMS = new Set([
  'define', 'set!', 'if', 'quote', 'lambda', 'and', 'or', 'begin',
  'let', 'cond', 'define-syntax', 'syntax-rules',
]);

interface PatternBindings {
  [key: string]: SchemeVal | SchemeVal[];
}

function collectPatternVarNames(pattern: SchemeVal, literals: Set<string>): Set<string> {
  if (pattern.tag === 'symbol') {
    if (pattern.value === '_' || pattern.value === '...' || literals.has(pattern.value)) return new Set();
    return new Set([pattern.value]);
  }
  if (pattern.tag === 'list') {
    const result = new Set<string>();
    for (const elem of pattern.value) {
      for (const v of collectPatternVarNames(elem, literals)) result.add(v);
    }
    return result;
  }
  return new Set();
}

function matchPattern(
  pattern: SchemeVal, input: SchemeVal, literals: Set<string>, bindings: PatternBindings
): boolean {
  if (pattern.tag === 'symbol') {
    if (pattern.value === '_') return true;
    if (literals.has(pattern.value)) {
      return input.tag === 'symbol' && input.value === pattern.value;
    }
    bindings[pattern.value] = input;
    return true;
  }
  if (pattern.tag === 'list') {
    if (input.tag !== 'list') return false;
    const pats = pattern.value;
    const inps = input.value;

    let ellipsisIdx = -1;
    for (let i = 0; i < pats.length; i++) {
      const pi = pats[i];
      if (pi.tag === 'symbol' && pi.value === '...') {
        ellipsisIdx = i;
        break;
      }
    }

    if (ellipsisIdx === -1) {
      if (pats.length !== inps.length) return false;
      for (let i = 0; i < pats.length; i++) {
        if (!matchPattern(pats[i], inps[i], literals, bindings)) return false;
      }
      return true;
    }

    const repeatPatIdx = ellipsisIdx - 1;
    const beforeCount = repeatPatIdx;
    const afterCount = pats.length - ellipsisIdx - 1;
    const minInputs = beforeCount + afterCount;
    if (inps.length < minInputs) return false;

    for (let i = 0; i < beforeCount; i++) {
      if (!matchPattern(pats[i], inps[i], literals, bindings)) return false;
    }

    const repeatPat = pats[repeatPatIdx];
    const repeatVarNames = [...collectPatternVarNames(repeatPat, literals)];
    const repeatArrays = new Map<string, SchemeVal[]>();
    for (const name of repeatVarNames) repeatArrays.set(name, []);

    const repeatCount = inps.length - minInputs;
    for (let i = 0; i < repeatCount; i++) {
      const subBindings: PatternBindings = {};
      if (!matchPattern(repeatPat, inps[beforeCount + i], literals, subBindings)) return false;
      for (const name of repeatVarNames) {
        repeatArrays.get(name)!.push(subBindings[name] as SchemeVal);
      }
    }

    for (const [name, arr] of repeatArrays) {
      bindings[name] = arr;
    }

    for (let i = 0; i < afterCount; i++) {
      if (!matchPattern(pats[ellipsisIdx + 1 + i], inps[inps.length - afterCount + i], literals, bindings)) return false;
    }

    return true;
  }

  if (pattern.tag === 'boolean' && input.tag === 'boolean') return pattern.value === input.value;
  if (pattern.tag === 'number' && input.tag === 'number') return pattern.value === input.value;
  return false;
}

function findEllipsisVarsIn(tmpl: SchemeVal, ellipsisVars: Set<string>): string[] {
  const found: string[] = [];
  if (tmpl.tag === 'symbol' && ellipsisVars.has(tmpl.value)) {
    found.push(tmpl.value);
  } else if (tmpl.tag === 'list') {
    for (const elem of tmpl.value) {
      found.push(...findEllipsisVarsIn(elem, ellipsisVars));
    }
  }
  return found;
}

function instantiateTemplate(
  template: SchemeVal,
  bindings: PatternBindings,
  patternVars: Set<string>,
  ellipsisVars: Set<string>,
  defEnv: Env,
  renameMap: Map<string, string>
): SchemeVal {
  if (template.tag === 'symbol') {
    if (patternVars.has(template.value)) {
      const val = bindings[template.value];
      if (Array.isArray(val)) {
        throw new EvalError('ellipsis variable outside ellipsis context');
      }
      return val;
    }
    if (SPECIAL_FORMS.has(template.value)) return template;
    // Hygiene: rename introduced identifiers
    if (!renameMap.has(template.value)) {
      const gs = gensym(template.value);
      renameMap.set(template.value, gs);
      resolvedSymbols.set(gs, { name: template.value, env: defEnv });
    }
    return { tag: 'symbol', value: renameMap.get(template.value)! };
  }

  if (template.tag === 'list') {
    const elems = template.value;
    const result: SchemeVal[] = [];

    for (let i = 0; i < elems.length; i++) {
      const next = elems[i + 1];
      if (i + 1 < elems.length && next && next.tag === 'symbol' && next.value === '...') {
        const repeatTmpl = elems[i];
        const usedVars = findEllipsisVarsIn(repeatTmpl, ellipsisVars);

        if (usedVars.length > 0) {
          const count = (bindings[usedVars[0]] as SchemeVal[]).length;
          for (let j = 0; j < count; j++) {
            const tempBindings = { ...bindings };
            for (const v of usedVars) {
              tempBindings[v] = (bindings[v] as SchemeVal[])[j];
            }
            result.push(instantiateTemplate(
              repeatTmpl, tempBindings, patternVars,
              new Set([...ellipsisVars].filter(v => !usedVars.includes(v))),
              defEnv, renameMap
            ));
          }
        }
        i++; // skip ...
        continue;
      }
      result.push(instantiateTemplate(elems[i], bindings, patternVars, ellipsisVars, defEnv, renameMap));
    }

    return { tag: 'list', value: result };
  }

  return template;
}

function expandMacro(macro: SchemeVal & { tag: 'macro' }, expr: SchemeVal): SchemeVal {
  const literals = new Set(macro.literals);

  for (const rule of macro.rules) {
    const bindings: PatternBindings = {};
    if (matchPattern(rule.pattern, expr, literals, bindings)) {
      const patternVars = collectPatternVarNames(rule.pattern, literals);
      const ellipsisVars = new Set<string>();
      for (const [name, val] of Object.entries(bindings)) {
        if (Array.isArray(val)) ellipsisVars.add(name);
      }
      const renameMap = new Map<string, string>();
      return instantiateTemplate(rule.template, bindings, patternVars, ellipsisVars, macro.defEnv, renameMap);
    }
  }

  throw new EvalError('no matching syntax-rules pattern');
}

function parseParams(params: SchemeVal): { names: string[]; rest: string | null } {
  if (params.tag === 'symbol') {
    // (lambda args body) — all args captured as rest
    return { names: [], rest: params.value };
  }
  if (params.tag === 'list') {
    const names = params.value.map(p => {
      if (p.tag !== 'symbol') throw errAt('parameter must be a symbol', p.pos);
      return p.value;
    });
    return { names, rest: null };
  }
  if (params.tag === 'pair') {
    // Improper list from dotted notation: (a b . rest)
    const names: string[] = [];
    let cur: SchemeVal = params;
    while (cur.tag === 'pair') {
      if (cur.car.tag !== 'symbol') throw errAt('parameter must be a symbol', cur.car.pos);
      names.push(cur.car.value);
      cur = cur.cdr;
    }
    if (cur.tag !== 'symbol') throw errAt('rest parameter must be a symbol', cur.pos);
    return { names, rest: cur.value };
  }
  throw errAt('invalid parameter list', params.pos);
}

function makeProcedure(paramInfo: { names: string[]; rest: string | null }, bodyExprs: SchemeVal[], closureEnv: Env): SchemeVal {
  return { tag: 'procedure', value: (...args: SchemeVal[]) => {
    const childEnv = new Env(closureEnv);
    for (let i = 0; i < paramInfo.names.length; i++) {
      childEnv.set(paramInfo.names[i], args[i]);
    }
    if (paramInfo.rest !== null) {
      // Collect remaining args into a list
      let restList: SchemeVal = NIL;
      for (let i = args.length - 1; i >= paramInfo.names.length; i--) {
        restList = { tag: 'pair', car: args[i], cdr: restList };
      }
      childEnv.set(paramInfo.rest, restList);
    }
    let result: SchemeVal = { tag: 'void' };
    for (const b of bodyExprs) {
      result = evaluate(b, childEnv);
    }
    return result;
  }};
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'rational':
    case 'boolean':
    case 'string':
    case 'char':
      return expr;

    case 'symbol': {
      try {
        return env.get(expr.value);
      } catch {
        const resolved = resolvedSymbols.get(expr.value);
        if (resolved) {
          try {
            return resolved.env.get(resolved.name);
          } catch (e2) {
            if (e2 instanceof EvalError) throw errAt(e2.message, expr.pos);
            throw e2;
          }
        }
        throw errAt(`unbound variable: ${expr.value}`, expr.pos);
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
              const paramsForm: SchemeVal = { tag: 'list', value: target.value.slice(1), pos: target.pos };
              const paramInfo = parseParams(paramsForm);
              const bodyExprs = elems.slice(2);
              env.set(name, makeProcedure(paramInfo, bodyExprs, env));
              return { tag: 'void' };
            }
            if (target.tag === 'pair' && target.car.tag === 'symbol') {
              // (define (f x . rest) body...) — dotted param list parsed as pair
              const name = target.car.value;
              const paramInfo = parseParams(target.cdr);
              // The first pair element is the function name, rest is params
              // Actually target is (f x . rest) parsed as pair chain
              // target.car = f, target.cdr = pair(x, symbol:rest)
              const bodyExprs = elems.slice(2);
              env.set(name, makeProcedure(paramInfo, bodyExprs, env));
              return { tag: 'void' };
            }
            throw errAt('invalid define syntax', expr.pos);
          }
          case 'set!': {
            if (elems.length !== 3) throw errAt('set! requires exactly 2 arguments', expr.pos);
            const target = elems[1];
            if (target.tag !== 'symbol') throw errAt('set! target must be a symbol', expr.pos);
            const val = evaluate(elems[2], env);
            try {
              env.update(target.value, val);
            } catch (e) {
              if (e instanceof EvalError) throw errAt(e.message, expr.pos);
              throw e;
            }
            return { tag: 'void' };
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
            const paramInfo = parseParams(elems[1]);
            const bodyExprs = elems.slice(2);
            return makeProcedure(paramInfo, bodyExprs, env);
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
          case 'define-syntax': {
            if (elems.length !== 3) throw errAt('define-syntax requires 2 arguments', expr.pos);
            if (elems[1].tag !== 'symbol') throw errAt('define-syntax: name must be a symbol', expr.pos);
            const macroName = elems[1].value;
            const transformer = elems[2];
            if (transformer.tag !== 'list' || transformer.value.length < 2 ||
                transformer.value[0].tag !== 'symbol' || transformer.value[0].value !== 'syntax-rules') {
              throw errAt('define-syntax: expected syntax-rules', expr.pos);
            }
            const srElems = transformer.value;
            if (srElems[1].tag !== 'list') throw errAt('syntax-rules: expected literals list', expr.pos);
            const macroLiterals = srElems[1].value.map(l => {
              if (l.tag !== 'symbol') throw errAt('syntax-rules: literal must be a symbol', l.pos);
              return l.value;
            });
            const macroRules: { pattern: SchemeVal; template: SchemeVal }[] = [];
            for (let ri = 2; ri < srElems.length; ri++) {
              const rule = srElems[ri];
              if (rule.tag !== 'list' || rule.value.length !== 2)
                throw errAt('syntax-rules: each rule must be (pattern template)', rule.pos);
              macroRules.push({ pattern: rule.value[0], template: rule.value[1] });
            }
            env.set(macroName, { tag: 'macro', literals: macroLiterals, rules: macroRules, defEnv: env });
            return { tag: 'void' };
          }
        }

        // Check for macro
        let macroVal: SchemeVal | null = null;
        const resolvedSym = resolvedSymbols.get(first.value);
        if (resolvedSym) {
          try { macroVal = resolvedSym.env.get(resolvedSym.name); } catch {}
        } else {
          try { macroVal = env.get(first.value); } catch {}
        }
        if (macroVal && macroVal.tag === 'macro') {
          const expanded = expandMacro(macroVal, expr);
          return evaluate(expanded, env);
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
  gensymCounter = 0;
  resolvedSymbols.clear();
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
  gensymCounter = 0;
  resolvedSymbols.clear();
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
