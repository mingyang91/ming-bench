import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; value: SchemeVal[]; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; rest?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'syntax'; rules: { pattern: SchemeVal; template: SchemeVal }[]; literals: string[]; defEnv: Env; pos?: Pos }
  | { tag: 'record'; type: symbol; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'case-lambda'; clauses: { params: string[]; rest?: string; body: SchemeVal[] }[]; env: Env; pos?: Pos }
  | { tag: 'vector'; value: SchemeVal[]; pos?: Pos };

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}: ` : '';
}

const NIL: SchemeVal = { tag: 'nil' };

// Unique record type identity
let _recordTypeCounter = 0;
function newRecordTypeId(): symbol {
  return Symbol(`record-type-${_recordTypeCounter++}`);
}

// Native functions (for record constructors, predicates, accessors)
const _nativeFns = new Map<string, (args: SchemeVal[]) => SchemeVal>();

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

// ── Rational helpers ──────────────────────────────────────────────

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b !== 0) { [a, b] = [b, a % b]; }
  return a;
}

function makeRational(num: number, den: number, pos?: Pos): SchemeVal {
  if (den === 0) throw new EvalError(`${posStr(pos)}division by zero`);
  if (num === 0) return { tag: 'rational', num: 0, den: 1, pos };
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(Math.abs(num), den);
  return { tag: 'rational', num: num / g, den: den / g, pos };
}

function isNumeric(v: SchemeVal): boolean {
  return v.tag === 'number' || v.tag === 'rational';
}

function toFloat(v: SchemeVal): number {
  if (v.tag === 'number') return v.value;
  if (v.tag === 'rational') return v.num / v.den;
  throw new Error('not numeric');
}

function makeExactInt(n: number, pos?: Pos): SchemeVal {
  return { tag: 'rational', num: n, den: 1, pos };
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

  update(name: string, val: SchemeVal, pos?: Pos): void {
    if (this.bindings.has(name)) { this.bindings.set(name, val); return; }
    if (this.parent) { this.parent.update(name, val, pos); return; }
    throw new EvalError(`${posStr(pos)}unbound variable: ${name}`);
  }

  lookup(name: string): SchemeVal | undefined {
    const v = this.bindings.get(name);
    if (v !== undefined) return v;
    if (this.parent) return this.parent.lookup(name);
    return undefined;
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
      if (i + 1 < input.length && input[i + 1] === '(') {
        const p = curPos();
        tokens.push({ text: '#(', pos: p });
        advance(); advance();
        continue;
      }
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
    if (tok.text === '#(') {
      const elems: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elems.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError(`${tok.pos.line}:${tok.pos.col}: missing closing paren`);
      idx++;
      return { tag: 'list', value: [{ tag: 'symbol', value: 'vector', pos: tok.pos }, ...elems], pos: tok.pos };
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
    if (tok.text.startsWith('#\\')) {
      const charName = tok.text.substring(2);
      let ch: string;
      if (charName === 'space') ch = ' ';
      else if (charName === 'newline') ch = '\n';
      else if (charName === 'tab') ch = '\t';
      else if (charName.length === 1) ch = charName;
      else throw new EvalError(`${tok.pos.line}:${tok.pos.col}: unknown character name: ${charName}`);
      return { tag: 'char', value: ch, pos: tok.pos };
    }
    // Rational literal: N/M
    const ratMatch = /^(-?\d+)\/(\d+)$/.exec(tok.text);
    if (ratMatch) {
      return makeRational(parseInt(ratMatch[1], 10), parseInt(ratMatch[2], 10), tok.pos);
    }
    const num = Number(tok.text);
    if (!isNaN(num) && tok.text !== '') {
      // Integer literals are exact (rational with den=1), floats are inexact
      if (/^-?\d+$/.test(tok.text)) return { tag: 'rational', num: num, den: 1, pos: tok.pos };
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

// ── Parameter parsing ───────────────────────────────────────────────

function parseParams(paramList: SchemeVal, pos?: Pos): { params: string[]; rest?: string } {
  if (paramList.tag === 'symbol') {
    // (lambda args body) — single rest param
    return { params: [], rest: paramList.value };
  }
  if (paramList.tag !== 'list') throw new EvalError(`${posStr(pos)}parameters must be a list`);
  const items = paramList.value;
  const params: string[] = [];
  let rest: string | undefined;
  for (let i = 0; i < items.length; i++) {
    if (items[i].tag === 'symbol' && items[i].value === '.') {
      if (i + 1 >= items.length || items[i + 1].tag !== 'symbol')
        throw new EvalError(`${posStr(pos)}invalid dot syntax in parameters`);
      rest = (items[i + 1] as any).value;
      break;
    }
    if (items[i].tag !== 'symbol') throw new EvalError(`${posStr(pos)}parameter must be a symbol`);
    params.push((items[i] as any).value);
  }
  return { params, rest };
}

// ── Macros (syntax-rules) ────────────────────────────────────────

let _gensymCounter = 0;
function gensym(base: string): string {
  return `__${base}_${++_gensymCounter}`;
}

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'lambda', 'set!', 'begin', 'let', 'cond', 'and', 'or',
  'define-syntax', 'syntax-rules', 'case-lambda',
  'letrec', 'letrec*', 'case', 'do',
]);

function matchSyntaxPattern(
  pattern: SchemeVal,
  input: SchemeVal,
  literals: string[],
  bindings: Map<string, SchemeVal | SchemeVal[]>,
): boolean {
  if (pattern.tag === 'symbol') {
    if (pattern.value === '_') return true;
    if (literals.includes(pattern.value)) {
      return input.tag === 'symbol' && input.value === pattern.value;
    }
    bindings.set(pattern.value, input);
    return true;
  }
  if (pattern.tag === 'list' && input.tag === 'list') {
    const pElems = pattern.value;
    const iElems = input.value;

    // Find ellipsis
    let ellipsisIdx = -1;
    for (let i = 0; i < pElems.length; i++) {
      if (pElems[i].tag === 'symbol' && pElems[i].value === '...') { ellipsisIdx = i; break; }
    }

    if (ellipsisIdx === -1) {
      if (pElems.length !== iElems.length) return false;
      for (let i = 0; i < pElems.length; i++) {
        if (!matchSyntaxPattern(pElems[i], iElems[i], literals, bindings)) return false;
      }
      return true;
    }

    // Ellipsis at ellipsisIdx — the element before it is the repeated pattern
    const repeatedPatIdx = ellipsisIdx - 1;
    if (repeatedPatIdx < 0) return false;
    const beforeRepeat = repeatedPatIdx;
    const afterEllipsis = pElems.length - ellipsisIdx - 1;
    if (iElems.length < beforeRepeat + afterEllipsis) return false;

    for (let i = 0; i < beforeRepeat; i++) {
      if (!matchSyntaxPattern(pElems[i], iElems[i], literals, bindings)) return false;
    }

    const repeatCount = iElems.length - beforeRepeat - afterEllipsis;
    const repeatedPat = pElems[repeatedPatIdx];
    if (repeatedPat.tag === 'symbol' && !literals.includes(repeatedPat.value) && repeatedPat.value !== '_') {
      const matches: SchemeVal[] = [];
      for (let i = 0; i < repeatCount; i++) matches.push(iElems[beforeRepeat + i]);
      bindings.set(repeatedPat.value, matches);
    } else {
      return false;
    }

    for (let i = 0; i < afterEllipsis; i++) {
      if (!matchSyntaxPattern(pElems[ellipsisIdx + 1 + i], iElems[iElems.length - afterEllipsis + i], literals, bindings)) return false;
    }
    return true;
  }
  if (pattern.tag === 'number' && input.tag === 'number') return pattern.value === input.value;
  if (pattern.tag === 'rational' && input.tag === 'rational') return pattern.num === input.num && pattern.den === input.den;
  if (pattern.tag === 'boolean' && input.tag === 'boolean') return pattern.value === input.value;
  return false;
}

function findEllipsisVars(template: SchemeVal, bindings: Map<string, SchemeVal | SchemeVal[]>): string[] {
  if (template.tag === 'symbol') {
    return Array.isArray(bindings.get(template.value)) ? [template.value] : [];
  }
  if (template.tag === 'list') {
    const result: string[] = [];
    for (const elem of template.value) result.push(...findEllipsisVars(elem, bindings));
    return result;
  }
  return [];
}

function collectPatternVars(pattern: SchemeVal, literals: string[], vars: Set<string>): void {
  if (pattern.tag === 'symbol') {
    if (pattern.value !== '_' && pattern.value !== '...' && !literals.includes(pattern.value)) vars.add(pattern.value);
  } else if (pattern.tag === 'list') {
    for (const elem of pattern.value) collectPatternVars(elem, literals, vars);
  }
}

function collectFreeSymbols(template: SchemeVal, patternVars: Set<string>, result: Set<string>): void {
  if (template.tag === 'symbol') {
    if (template.value !== '...' && !patternVars.has(template.value) &&
        !SPECIAL_FORMS.has(template.value) && !BUILTIN_NAMES.has(template.value)) {
      result.add(template.value);
    }
  } else if (template.tag === 'list') {
    for (const elem of template.value) collectFreeSymbols(elem, patternVars, result);
  }
}

function expandTemplate(
  template: SchemeVal,
  bindings: Map<string, SchemeVal | SchemeVal[]>,
  renameMap: Map<string, string>,
): SchemeVal {
  if (template.tag === 'symbol') {
    if (template.value === '...') return template;
    const binding = bindings.get(template.value);
    if (binding !== undefined && !Array.isArray(binding)) return binding;
    const renamed = renameMap.get(template.value);
    if (renamed !== undefined) return { tag: 'symbol', value: renamed, pos: template.pos };
    return template;
  }
  if (template.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < template.value.length; i++) {
      const elem = template.value[i];
      if (i + 1 < template.value.length &&
          template.value[i + 1].tag === 'symbol' && template.value[i + 1].value === '...') {
        const ellipsisVars = findEllipsisVars(elem, bindings);
        if (ellipsisVars.length > 0) {
          const varName = ellipsisVars[0];
          const matches = bindings.get(varName) as SchemeVal[];
          for (const match of matches) {
            const subBindings = new Map(bindings);
            subBindings.set(varName, match);
            result.push(expandTemplate(elem, subBindings, renameMap));
          }
        }
        i++;
        continue;
      }
      result.push(expandTemplate(elem, bindings, renameMap));
    }
    return { tag: 'list', value: result, pos: template.pos };
  }
  return template;
}

function expandMacro(
  syntax: SchemeVal & { tag: 'syntax' },
  form: SchemeVal & { tag: 'list' },
  env: Env,
): SchemeVal {
  for (const rule of syntax.rules) {
    const bindings = new Map<string, SchemeVal | SchemeVal[]>();
    // Skip first element (macro keyword) in both pattern and input
    const patArgs: SchemeVal = { tag: 'list', value: rule.pattern.tag === 'list' ? rule.pattern.value.slice(1) : [] };
    const inputArgs: SchemeVal = { tag: 'list', value: form.value.slice(1) };
    if (matchSyntaxPattern(patArgs, inputArgs, syntax.literals, bindings)) {
      const patternVars = new Set<string>();
      collectPatternVars(patArgs, syntax.literals, patternVars);

      const freeSyms = new Set<string>();
      collectFreeSymbols(rule.template, patternVars, freeSyms);

      const renameMap = new Map<string, string>();
      for (const sym of freeSyms) {
        const defVal = syntax.defEnv.lookup(sym);
        if (defVal !== undefined) {
          const renamed = gensym(sym);
          renameMap.set(sym, renamed);
          env.set(renamed, defVal);
        }
      }

      return expandTemplate(rule.template, bindings, renameMap);
    }
  }
  throw new EvalError(`no matching pattern for macro`);
}

// ── Evaluator ──────────────────────────────────────────────────────

function isTruthy(v: SchemeVal): boolean {
  return !(v.tag === 'boolean' && v.value === false);
}

function toNumber(v: SchemeVal, op: string, callPos?: Pos): number {
  if (v.tag === 'number') return v.value;
  if (v.tag === 'rational') return v.num / v.den;
  throw new EvalError(`${posStr(callPos)}${op}: expected number`);
}

function schemeEqual(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'boolean': return a.value === (b as typeof a).value;
    case 'string': return a.value === (b as typeof a).value;
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'nil': return true;
    case 'pair': return b.tag === 'pair' && schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
    case 'vector': {
      if (b.tag !== 'vector') return false;
      if (a.value.length !== b.value.length) return false;
      for (let i = 0; i < a.value.length; i++) {
        if (!schemeEqual(a.value[i], b.value[i])) return false;
      }
      return true;
    }
    default: return a === b;
  }
}

function schemeEq(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'boolean': return a.value === (b as typeof a).value;
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'nil': return true;
    case 'void': return true;
    default: return a === b;
  }
}

let _outputBuf: string[] = [];

function evalBuiltin(name: string, args: SchemeVal[], callPos?: Pos): SchemeVal {
  // Check native functions (record constructors, predicates, accessors)
  const nativeFn = _nativeFns.get(name);
  if (nativeFn) return nativeFn(args);

  switch (name) {
    case '+': {
      const allExact = args.every(a => a.tag === 'rational');
      if (allExact) {
        let rn = 0, rd = 1;
        for (const a of args) {
          if (a.tag !== 'rational') throw new EvalError(`${posStr(callPos)}+: expected number`);
          rn = rn * a.den + a.num * rd;
          rd = rd * a.den;
          const g = gcd(Math.abs(rn), rd);
          rn /= g; rd /= g;
        }
        return makeRational(rn, rd, callPos);
      }
      let sum = 0;
      for (const a of args) sum += toNumber(a, '+', callPos);
      return { tag: 'number', value: sum };
    }
    case '-': {
      if (args.length === 0) throw new EvalError(`${posStr(callPos)}-: need at least 1 argument`);
      const allExact = args.every(a => a.tag === 'rational');
      if (allExact) {
        const first = args[0] as SchemeVal & { tag: 'rational' };
        if (args.length === 1) return makeRational(-first.num, first.den, callPos);
        let rn = first.num, rd = first.den;
        for (let i = 1; i < args.length; i++) {
          const a = args[i] as SchemeVal & { tag: 'rational' };
          rn = rn * a.den - a.num * rd;
          rd = rd * a.den;
          const g = gcd(Math.abs(rn), rd);
          rn /= g; rd /= g;
        }
        return makeRational(rn, rd, callPos);
      }
      if (args.length === 1) return { tag: 'number', value: -toNumber(args[0], '-', callPos) };
      let result = toNumber(args[0], '-', callPos);
      for (let i = 1; i < args.length; i++) result -= toNumber(args[i], '-', callPos);
      return { tag: 'number', value: result };
    }
    case '*': {
      const allExact = args.every(a => a.tag === 'rational');
      if (allExact) {
        let rn = 1, rd = 1;
        for (const a of args) {
          if (a.tag !== 'rational') throw new EvalError(`${posStr(callPos)}*: expected number`);
          rn *= a.num;
          rd *= a.den;
          const g = gcd(Math.abs(rn), rd);
          rn /= g; rd /= g;
        }
        return makeRational(rn, rd, callPos);
      }
      let prod = 1;
      for (const a of args) prod *= toNumber(a, '*', callPos);
      return { tag: 'number', value: prod };
    }
    case '/': {
      if (args.length < 2) throw new EvalError(`${posStr(callPos)}/: need at least 2 arguments`);
      const allExact = args.every(a => a.tag === 'rational');
      if (allExact) {
        const first = args[0] as SchemeVal & { tag: 'rational' };
        let rn = first.num, rd = first.den;
        for (let i = 1; i < args.length; i++) {
          const a = args[i] as SchemeVal & { tag: 'rational' };
          if (a.num === 0) throw new EvalError(`${posStr(callPos)}division by zero`);
          rn *= a.den;
          rd *= a.num;
        }
        return makeRational(rn, rd, callPos);
      }
      let result = toNumber(args[0], '/', callPos);
      for (let i = 1; i < args.length; i++) {
        const d = toNumber(args[i], '/', callPos);
        if (d === 0) throw new EvalError(`${posStr(callPos)}division by zero`);
        result /= d;
      }
      return { tag: 'number', value: result };
    }
    case '<': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}<: need 2 arguments`);
      return { tag: 'boolean', value: toFloat(args[0]) < toFloat(args[1]) };
    }
    case '>': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}>: need 2 arguments`);
      return { tag: 'boolean', value: toFloat(args[0]) > toFloat(args[1]) };
    }
    case '=': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}=: need 2 arguments`);
      return { tag: 'boolean', value: toFloat(args[0]) === toFloat(args[1]) };
    }
    case '<=': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}<=: need 2 arguments`);
      return { tag: 'boolean', value: toFloat(args[0]) <= toFloat(args[1]) };
    }
    case '>=': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}>=: need 2 arguments`);
      return { tag: 'boolean', value: toFloat(args[0]) >= toFloat(args[1]) };
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
      return makeExactInt(arr.length);
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
      return { tag: 'boolean', value: isNumeric(args[0]) };
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
      return makeExactInt(args[0].value.length);
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
      if (Number.isInteger(n)) return makeExactInt(n);
      return { tag: 'number', value: n };
    }
    case 'number->string': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}number->string: need 1 argument`);
      if (args[0].tag === 'rational') return { tag: 'string', value: writeVal(args[0]) };
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
    case 'string-copy': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string-copy: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-copy: expected string`);
      return { tag: 'string', value: args[0].value };
    }
    case 'string-set!': {
      if (args.length !== 3) throw new EvalError(`${posStr(callPos)}string-set!: need 3 arguments`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-set!: expected string`);
      const idx = toNumber(args[1], 'string-set!', callPos);
      if (args[2].tag !== 'char') throw new EvalError(`${posStr(callPos)}string-set!: expected char`);
      if (idx < 0 || idx >= args[0].value.length) throw new EvalError(`${posStr(callPos)}string-set!: index out of range`);
      (args[0] as any).value = args[0].value.substring(0, idx) + args[2].value + args[0].value.substring(idx + 1);
      return { tag: 'void' };
    }
    // ── L09: Numeric utilities ──
    case 'abs': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}abs: need 1 argument`);
      if (args[0].tag === 'rational') return makeRational(Math.abs(args[0].num), args[0].den, callPos);
      return { tag: 'number', value: Math.abs(toNumber(args[0], 'abs', callPos)) };
    }
    case 'modulo': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}modulo: need 2 arguments`);
      const a = toNumber(args[0], 'modulo', callPos);
      const b = toNumber(args[1], 'modulo', callPos);
      if (b === 0) throw new EvalError(`${posStr(callPos)}modulo: division by zero`);
      const r = a - b * Math.floor(a / b);
      if (args[0].tag === 'rational' && args[1].tag === 'rational') return makeExactInt(r);
      return { tag: 'number', value: r };
    }
    case 'remainder': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}remainder: need 2 arguments`);
      const a = toNumber(args[0], 'remainder', callPos);
      const b = toNumber(args[1], 'remainder', callPos);
      if (b === 0) throw new EvalError(`${posStr(callPos)}remainder: division by zero`);
      const r = a - b * Math.trunc(a / b);
      if (args[0].tag === 'rational' && args[1].tag === 'rational') return makeExactInt(r);
      return { tag: 'number', value: r };
    }
    case 'quotient': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}quotient: need 2 arguments`);
      const a = toNumber(args[0], 'quotient', callPos);
      const b = toNumber(args[1], 'quotient', callPos);
      if (b === 0) throw new EvalError(`${posStr(callPos)}quotient: division by zero`);
      const r = Math.trunc(a / b);
      if (args[0].tag === 'rational' && args[1].tag === 'rational') return makeExactInt(r);
      return { tag: 'number', value: r };
    }
    case 'min': {
      if (args.length === 0) throw new EvalError(`${posStr(callPos)}min: need at least 1 argument`);
      let bestIdx = 0;
      let bestVal = toNumber(args[0], 'min', callPos);
      for (let i = 1; i < args.length; i++) {
        const v = toNumber(args[i], 'min', callPos);
        if (v < bestVal) { bestVal = v; bestIdx = i; }
      }
      return args[bestIdx];
    }
    case 'max': {
      if (args.length === 0) throw new EvalError(`${posStr(callPos)}max: need at least 1 argument`);
      let bestIdx = 0;
      let bestVal = toNumber(args[0], 'max', callPos);
      for (let i = 1; i < args.length; i++) {
        const v = toNumber(args[i], 'max', callPos);
        if (v > bestVal) { bestVal = v; bestIdx = i; }
      }
      return args[bestIdx];
    }
    case 'expt': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}expt: need 2 arguments`);
      const r = Math.pow(toNumber(args[0], 'expt', callPos), toNumber(args[1], 'expt', callPos));
      if (args[0].tag === 'rational' && args[1].tag === 'rational' && Number.isInteger(r)) return makeExactInt(r);
      return { tag: 'number', value: r };
    }
    case 'zero?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}zero?: need 1 argument`);
      return { tag: 'boolean', value: toNumber(args[0], 'zero?', callPos) === 0 };
    }
    case 'positive?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}positive?: need 1 argument`);
      return { tag: 'boolean', value: toNumber(args[0], 'positive?', callPos) > 0 };
    }
    case 'negative?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}negative?: need 1 argument`);
      return { tag: 'boolean', value: toNumber(args[0], 'negative?', callPos) < 0 };
    }
    case 'odd?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}odd?: need 1 argument`);
      return { tag: 'boolean', value: Math.abs(toNumber(args[0], 'odd?', callPos)) % 2 === 1 };
    }
    case 'even?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}even?: need 1 argument`);
      return { tag: 'boolean', value: toNumber(args[0], 'even?', callPos) % 2 === 0 };
    }
    // ── L09: List utilities ──
    case 'list-ref': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}list-ref: need 2 arguments`);
      const idx = toNumber(args[1], 'list-ref', callPos);
      const items = schemeListToArray(args[0]);
      if (idx < 0 || idx >= items.length) throw new EvalError(`${posStr(callPos)}list-ref: index out of range`);
      return items[idx];
    }
    case 'list-tail': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}list-tail: need 2 arguments`);
      let k = toNumber(args[1], 'list-tail', callPos);
      let cur = args[0];
      while (k > 0) {
        if (cur.tag !== 'pair') throw new EvalError(`${posStr(callPos)}list-tail: index out of range`);
        cur = cur.cdr;
        k--;
      }
      return cur;
    }
    case 'list?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}list?: need 1 argument`);
      let cur = args[0];
      while (cur.tag === 'pair') cur = cur.cdr;
      return { tag: 'boolean', value: cur.tag === 'nil' };
    }
    case 'assoc': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}assoc: need 2 arguments`);
      const key = args[0];
      const items = schemeListToArray(args[1]);
      for (const item of items) {
        if (item.tag !== 'pair') throw new EvalError(`${posStr(callPos)}assoc: not an alist`);
        if (schemeEqual(key, item.car)) return item;
      }
      return { tag: 'boolean', value: false };
    }
    case 'map': {
      if (args.length < 2) throw new EvalError(`${posStr(callPos)}map: need at least 2 arguments`);
      const fn = args[0];
      const lists = args.slice(1).map(a => schemeListToArray(a));
      const len = lists[0].length;
      const result: SchemeVal[] = [];
      for (let i = 0; i < len; i++) {
        const fnArgs = lists.map(l => l[i]);
        if (fn.tag === 'builtin') result.push(evalBuiltin(fn.name, fnArgs, callPos));
        else if (fn.tag === 'lambda') result.push(applyLambda(fn, fnArgs, callPos));
        else if (fn.tag === 'case-lambda') result.push(applyCaseLambda(fn, fnArgs, callPos));
        else throw new EvalError(`${posStr(callPos)}map: not a procedure`);
      }
      return arrayToSchemeList(result);
    }
    case 'equal?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}equal?: need 2 arguments`);
      return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
    }
    case 'eq?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}eq?: need 2 arguments`);
      return { tag: 'boolean', value: schemeEq(args[0], args[1]) };
    }
    case 'eqv?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}eqv?: need 2 arguments`);
      return { tag: 'boolean', value: schemeEq(args[0], args[1]) };
    }
    // ── L14: Vectors ──
    case 'vector': {
      return { tag: 'vector', value: [...args] };
    }
    case 'make-vector': {
      if (args.length < 1 || args.length > 2) throw new EvalError(`${posStr(callPos)}make-vector: need 1-2 arguments`);
      const len = toNumber(args[0], 'make-vector', callPos);
      const fill: SchemeVal = args.length === 2 ? args[1] : makeExactInt(0);
      return { tag: 'vector', value: Array.from({ length: len }, () => fill) };
    }
    case 'vector-ref': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}vector-ref: need 2 arguments`);
      if (args[0].tag !== 'vector') throw new EvalError(`${posStr(callPos)}vector-ref: not a vector`);
      const idx = toNumber(args[1], 'vector-ref', callPos);
      if (idx < 0 || idx >= args[0].value.length) throw new EvalError(`${posStr(callPos)}vector-ref: index out of range`);
      return args[0].value[idx];
    }
    case 'vector-set!': {
      if (args.length !== 3) throw new EvalError(`${posStr(callPos)}vector-set!: need 3 arguments`);
      if (args[0].tag !== 'vector') throw new EvalError(`${posStr(callPos)}vector-set!: not a vector`);
      const idx = toNumber(args[1], 'vector-set!', callPos);
      if (idx < 0 || idx >= args[0].value.length) throw new EvalError(`${posStr(callPos)}vector-set!: index out of range`);
      args[0].value[idx] = args[2];
      return { tag: 'void' };
    }
    case 'vector-length': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}vector-length: need 1 argument`);
      if (args[0].tag !== 'vector') throw new EvalError(`${posStr(callPos)}vector-length: not a vector`);
      return makeExactInt(args[0].value.length);
    }
    case 'vector?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}vector?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'vector' };
    }
    case 'vector->list': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}vector->list: need 1 argument`);
      if (args[0].tag !== 'vector') throw new EvalError(`${posStr(callPos)}vector->list: not a vector`);
      return arrayToSchemeList(args[0].value);
    }
    case 'list->vector': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}list->vector: need 1 argument`);
      return { tag: 'vector', value: schemeListToArray(args[0]) };
    }
    // ── L09: Character utilities ──
    case 'char-alphabetic?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}char-alphabetic?: need 1 argument`);
      if (args[0].tag !== 'char') throw new EvalError(`${posStr(callPos)}char-alphabetic?: expected char`);
      return { tag: 'boolean', value: /[a-zA-Z]/.test(args[0].value) };
    }
    case 'char-numeric?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}char-numeric?: need 1 argument`);
      if (args[0].tag !== 'char') throw new EvalError(`${posStr(callPos)}char-numeric?: expected char`);
      return { tag: 'boolean', value: /[0-9]/.test(args[0].value) };
    }
    case 'char-upcase': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}char-upcase: need 1 argument`);
      if (args[0].tag !== 'char') throw new EvalError(`${posStr(callPos)}char-upcase: expected char`);
      return { tag: 'char', value: args[0].value.toUpperCase() };
    }
    case 'char-downcase': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}char-downcase: need 1 argument`);
      if (args[0].tag !== 'char') throw new EvalError(`${posStr(callPos)}char-downcase: expected char`);
      return { tag: 'char', value: args[0].value.toLowerCase() };
    }
    case 'char=?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}char=?: need 2 arguments`);
      if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError(`${posStr(callPos)}char=?: expected chars`);
      return { tag: 'boolean', value: args[0].value === args[1].value };
    }
    case 'char<?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}char<?: need 2 arguments`);
      if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError(`${posStr(callPos)}char<?: expected chars`);
      return { tag: 'boolean', value: args[0].value < args[1].value };
    }
    // ── L09: String utilities ──
    case 'string=?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string=?: need 2 arguments`);
      if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${posStr(callPos)}string=?: expected strings`);
      return { tag: 'boolean', value: args[0].value === args[1].value };
    }
    case 'string<?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string<?: need 2 arguments`);
      if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${posStr(callPos)}string<?: expected strings`);
      return { tag: 'boolean', value: args[0].value < args[1].value };
    }
    case 'string-ci=?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string-ci=?: need 2 arguments`);
      if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-ci=?: expected strings`);
      return { tag: 'boolean', value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
    }
    case 'string-upcase': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string-upcase: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-upcase: expected string`);
      return { tag: 'string', value: args[0].value.toUpperCase() };
    }
    case 'string-downcase': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string-downcase: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-downcase: expected string`);
      return { tag: 'string', value: args[0].value.toLowerCase() };
    }
    // ── L11: Exact arithmetic & rationals ──
    case 'exact?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}exact?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'rational' };
    }
    case 'inexact?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}inexact?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'number' };
    }
    case 'integer?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}integer?: need 1 argument`);
      if (args[0].tag === 'rational') return { tag: 'boolean', value: args[0].den === 1 };
      if (args[0].tag === 'number') return { tag: 'boolean', value: Number.isInteger(args[0].value) };
      return { tag: 'boolean', value: false };
    }
    case 'rational?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}rational?: need 1 argument`);
      return { tag: 'boolean', value: args[0].tag === 'rational' };
    }
    case 'exact->inexact': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}exact->inexact: need 1 argument`);
      return { tag: 'number', value: toNumber(args[0], 'exact->inexact', callPos) };
    }
    case 'inexact->exact': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}inexact->exact: need 1 argument`);
      if (args[0].tag === 'rational') return args[0];
      const v = toNumber(args[0], 'inexact->exact', callPos);
      // Convert float to rational via fraction approximation
      if (Number.isInteger(v)) return makeRational(v, 1, callPos);
      // Use continued fraction to find best rational approximation
      const sign = v < 0 ? -1 : 1;
      const absV = Math.abs(v);
      let num0 = 0, den0 = 1, num1 = 1, den1 = 0;
      let x = absV;
      for (let i = 0; i < 64; i++) {
        const a = Math.floor(x);
        const num2 = a * num1 + num0;
        const den2 = a * den1 + den0;
        if (Math.abs(num2 / den2 - absV) < 1e-15) {
          return makeRational(sign * num2, den2, callPos);
        }
        num0 = num1; den0 = den1;
        num1 = num2; den1 = den2;
        const rem = x - a;
        if (rem < 1e-15) break;
        x = 1 / rem;
      }
      return makeRational(sign * num1, den1, callPos);
    }
    case 'numerator': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}numerator: need 1 argument`);
      if (args[0].tag === 'rational') return makeRational(args[0].num, 1, callPos);
      throw new EvalError(`${posStr(callPos)}numerator: expected rational`);
    }
    case 'denominator': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}denominator: need 1 argument`);
      if (args[0].tag === 'rational') return makeRational(args[0].den, 1, callPos);
      throw new EvalError(`${posStr(callPos)}denominator: expected rational`);
    }
    case 'procedure?': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}procedure?: need 1 argument`);
      const t = args[0].tag;
      return { tag: 'boolean', value: t === 'lambda' || t === 'builtin' || t === 'case-lambda' };
    }
    case 'apply': {
      if (args.length < 2) throw new EvalError(`${posStr(callPos)}apply: need at least 2 arguments`);
      const fn = args[0];
      const lastArg = args[args.length - 1];
      const prefixArgs = args.slice(1, -1);
      const tailArgs = schemeListToArray(lastArg);
      const allArgs = [...prefixArgs, ...tailArgs];
      if (fn.tag === 'builtin') {
        return evalBuiltin(fn.name, allArgs, callPos);
      }
      if (fn.tag === 'lambda') {
        return applyLambda(fn, allArgs, callPos);
      }
      if (fn.tag === 'case-lambda') {
        return applyCaseLambda(fn, allArgs, callPos);
      }
      throw new EvalError(`${posStr(callPos)}apply: not a procedure`);
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
  'string-copy', 'string-set!',
  'apply',
  // L09
  'abs', 'modulo', 'remainder', 'quotient', 'min', 'max', 'expt',
  'zero?', 'positive?', 'negative?', 'odd?', 'even?',
  'list-ref', 'list-tail', 'list?', 'assoc', 'map', 'equal?', 'eq?',
  'char-alphabetic?', 'char-numeric?', 'char-upcase', 'char-downcase', 'char=?', 'char<?',
  'string=?', 'string<?', 'string-ci=?', 'string-upcase', 'string-downcase',
  // L11
  'exact?', 'inexact?', 'integer?', 'rational?', 'procedure?',
  'exact->inexact', 'inexact->exact',
  'numerator', 'denominator',
  // L14
  'eqv?',
  'vector', 'make-vector', 'vector-ref', 'vector-set!', 'vector-length', 'vector?',
  'vector->list', 'list->vector',
]);

function applyLambda(proc: SchemeVal & { tag: 'lambda' }, args: SchemeVal[], callPos?: Pos): SchemeVal {
  if (proc.rest) {
    if (args.length < proc.params.length) {
      throw new EvalError(`${posStr(callPos)}lambda: expected at least ${proc.params.length} arguments, got ${args.length}`);
    }
  } else {
    if (args.length !== proc.params.length) {
      throw new EvalError(`${posStr(callPos)}lambda: expected ${proc.params.length} arguments, got ${args.length}`);
    }
  }
  const callEnv = new Env(proc.env);
  for (let i = 0; i < proc.params.length; i++) {
    callEnv.set(proc.params[i], args[i]);
  }
  if (proc.rest) {
    callEnv.set(proc.rest, arrayToSchemeList(args.slice(proc.params.length)));
  }
  let result: SchemeVal = { tag: 'void' };
  for (const bodyExpr of proc.body) {
    result = evaluate(bodyExpr, callEnv);
  }
  return result;
}

function applyCaseLambda(proc: SchemeVal & { tag: 'case-lambda' }, args: SchemeVal[], callPos?: Pos): SchemeVal {
  for (const clause of proc.clauses) {
    if (clause.rest) {
      if (args.length >= clause.params.length) {
        const callEnv = new Env(proc.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.set(clause.params[i], args[i]);
        }
        callEnv.set(clause.rest, arrayToSchemeList(args.slice(clause.params.length)));
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of clause.body) {
          result = evaluate(bodyExpr, callEnv);
        }
        return result;
      }
    } else {
      if (args.length === clause.params.length) {
        const callEnv = new Env(proc.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.set(clause.params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of clause.body) {
          result = evaluate(bodyExpr, callEnv);
        }
        return result;
      }
    }
  }
  throw new EvalError(`${posStr(callPos)}case-lambda: no matching clause for ${args.length} arguments`);
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  if (expr.tag === 'number' || expr.tag === 'rational' || expr.tag === 'boolean' || expr.tag === 'string' || expr.tag === 'char') return expr;
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
          const paramList: SchemeVal = { tag: 'list', value: target.value.slice(1) };
          const { params, rest } = parseParams(paramList, expr.pos);
          const body = elems.slice(2);
          const lam: SchemeVal = { tag: 'lambda', params, rest, body, env };
          env.set(name, lam);
          return { tag: 'void' };
        }
        throw new EvalError(`${posStr(expr.pos)}define: invalid syntax`);
      }
      case 'lambda': {
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}lambda: wrong number of arguments`);
        const { params, rest } = parseParams(elems[1], expr.pos);
        const body = elems.slice(2);
        return { tag: 'lambda', params, rest, body, env };
      }
      case 'case-lambda': {
        if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}case-lambda: need at least 1 clause`);
        const clauses: { params: string[]; rest?: string; body: SchemeVal[] }[] = [];
        for (let i = 1; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.value.length < 2)
            throw new EvalError(`${posStr(expr.pos)}case-lambda: invalid clause`);
          const { params, rest } = parseParams(clause.value[0], expr.pos);
          const body = clause.value.slice(1);
          clauses.push({ params, rest, body });
        }
        return { tag: 'case-lambda', clauses, env, pos: expr.pos };
      }
      case 'set!': {
        if (elems.length !== 3) throw new EvalError(`${posStr(expr.pos)}set!: wrong number of arguments`);
        const target = elems[1];
        if (target.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}set!: target must be a symbol`);
        const val = evaluate(elems[2], env);
        env.update(target.value, val, expr.pos);
        return { tag: 'void' };
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
      case 'letrec': {
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}letrec: wrong number of arguments`);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}letrec: invalid bindings`);
        const letrecEnv = new Env(env);
        // First, bind all vars to undefined placeholder
        const names: string[] = [];
        for (const b of bindings.value) {
          if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
            throw new EvalError(`${posStr(expr.pos)}letrec: invalid binding`);
          names.push(b.value[0].value);
          letrecEnv.set(b.value[0].value, { tag: 'void' });
        }
        // Then evaluate inits in the letrec env (all bindings visible)
        for (let i = 0; i < bindings.value.length; i++) {
          const b = bindings.value[i];
          const val = evaluate(b.value[1], letrecEnv);
          letrecEnv.set(names[i], val);
        }
        let result: SchemeVal = { tag: 'void' };
        for (let i = 2; i < elems.length; i++) {
          result = evaluate(elems[i], letrecEnv);
        }
        return result;
      }
      case 'letrec*': {
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}letrec*: wrong number of arguments`);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}letrec*: invalid bindings`);
        const letrecEnv = new Env(env);
        for (const b of bindings.value) {
          if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
            throw new EvalError(`${posStr(expr.pos)}letrec*: invalid binding`);
          const val = evaluate(b.value[1], letrecEnv);
          letrecEnv.set(b.value[0].value, val);
        }
        let result: SchemeVal = { tag: 'void' };
        for (let i = 2; i < elems.length; i++) {
          result = evaluate(elems[i], letrecEnv);
        }
        return result;
      }
      case 'case': {
        if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}case: wrong number of arguments`);
        const key = evaluate(elems[1], env);
        for (let i = 2; i < elems.length; i++) {
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.value.length < 2)
            throw new EvalError(`${posStr(expr.pos)}case: invalid clause`);
          const datums = clause.value[0];
          if (datums.tag === 'symbol' && datums.value === 'else') {
            let result: SchemeVal = { tag: 'void' };
            for (let j = 1; j < clause.value.length; j++) {
              result = evaluate(clause.value[j], env);
            }
            return result;
          }
          if (datums.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}case: datums must be a list`);
          for (const datum of datums.value) {
            const d = quoteDatum(datum);
            if (schemeEq(key, d)) {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.value.length; j++) {
                result = evaluate(clause.value[j], env);
              }
              return result;
            }
          }
        }
        return { tag: 'void' };
      }
      case 'do': {
        // (do ((var init step) ...) (test expr ...) body ...)
        if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}do: wrong number of arguments`);
        const varSpecs = elems[1];
        const testClause = elems[2];
        if (varSpecs.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}do: invalid variable specs`);
        if (testClause.tag !== 'list' || testClause.value.length < 1)
          throw new EvalError(`${posStr(expr.pos)}do: invalid test clause`);

        const doEnv = new Env(env);
        const vars: { name: string; step?: SchemeVal }[] = [];
        for (const spec of varSpecs.value) {
          if (spec.tag !== 'list' || spec.value.length < 2 || spec.value[0].tag !== 'symbol')
            throw new EvalError(`${posStr(expr.pos)}do: invalid variable spec`);
          const name = spec.value[0].value;
          const init = evaluate(spec.value[1], env);
          doEnv.set(name, init);
          vars.push({ name, step: spec.value.length >= 3 ? spec.value[2] : undefined });
        }

        // Iteration loop
        for (;;) {
          // Test
          const testVal = evaluate(testClause.value[0], doEnv);
          if (isTruthy(testVal)) {
            // Test is true — evaluate result expressions
            if (testClause.value.length === 1) return { tag: 'void' };
            let result: SchemeVal = { tag: 'void' };
            for (let j = 1; j < testClause.value.length; j++) {
              result = evaluate(testClause.value[j], doEnv);
            }
            return result;
          }
          // Execute body
          for (let i = 3; i < elems.length; i++) {
            evaluate(elems[i], doEnv);
          }
          // Parallel step: evaluate all steps using current values, then update
          const newVals: (SchemeVal | undefined)[] = [];
          for (const v of vars) {
            if (v.step !== undefined) {
              newVals.push(evaluate(v.step, doEnv));
            } else {
              newVals.push(undefined);
            }
          }
          for (let i = 0; i < vars.length; i++) {
            if (newVals[i] !== undefined) {
              doEnv.set(vars[i].name, newVals[i]!);
            }
          }
        }
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
      case 'define-record-type': {
        // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
        if (elems.length < 4) throw new EvalError(`${posStr(expr.pos)}define-record-type: invalid syntax`);
        const rtId = newRecordTypeId();
        // Parse constructor: (make-name field1 field2 ...)
        const ctorForm = elems[2];
        if (ctorForm.tag !== 'list' || ctorForm.value.length < 1 || ctorForm.value[0].tag !== 'symbol')
          throw new EvalError(`${posStr(expr.pos)}define-record-type: invalid constructor`);
        const ctorName = ctorForm.value[0].value;
        const ctorFields = ctorForm.value.slice(1).map(f => {
          if (f.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}define-record-type: field must be symbol`);
          return f.value;
        });
        // Register constructor as native function
        const ctorKey = `__native_${ctorName}_${_recordTypeCounter}`;
        _nativeFns.set(ctorKey, (args: SchemeVal[]) => {
          if (args.length !== ctorFields.length)
            throw new EvalError(`${ctorName}: expected ${ctorFields.length} arguments, got ${args.length}`);
          const fields = new Map<string, SchemeVal>();
          for (let i = 0; i < ctorFields.length; i++) {
            fields.set(ctorFields[i], args[i]);
          }
          return { tag: 'record', type: rtId, fields };
        });
        env.set(ctorName, { tag: 'builtin', name: ctorKey });

        // Predicate
        const predName = elems[3];
        if (predName.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}define-record-type: predicate must be symbol`);
        const predKey = `__native_${predName.value}_${_recordTypeCounter}`;
        _nativeFns.set(predKey, (args: SchemeVal[]) => {
          if (args.length !== 1) throw new EvalError(`${predName.value}: expected 1 argument`);
          return { tag: 'boolean', value: args[0].tag === 'record' && args[0].type === rtId };
        });
        env.set(predName.value, { tag: 'builtin', name: predKey });

        // Field accessors
        for (let i = 4; i < elems.length; i++) {
          const fieldSpec = elems[i];
          if (fieldSpec.tag !== 'list' || fieldSpec.value.length < 2)
            throw new EvalError(`${posStr(expr.pos)}define-record-type: invalid field spec`);
          const fieldName = fieldSpec.value[0];
          const accessorName = fieldSpec.value[1];
          if (fieldName.tag !== 'symbol' || accessorName.tag !== 'symbol')
            throw new EvalError(`${posStr(expr.pos)}define-record-type: field spec must contain symbols`);
          const fn = fieldName.value;
          const accKey = `__native_${accessorName.value}_${_recordTypeCounter}`;
          _nativeFns.set(accKey, (args: SchemeVal[]) => {
            if (args.length !== 1) throw new EvalError(`${accessorName.value}: expected 1 argument`);
            if (args[0].tag !== 'record' || args[0].type !== rtId)
              throw new EvalError(`${accessorName.value}: not a valid record`);
            return args[0].fields.get(fn)!;
          });
          env.set(accessorName.value, { tag: 'builtin', name: accKey });
        }

        return { tag: 'void' };
      }
      case 'define-syntax': {
        if (elems.length !== 3) throw new EvalError(`${posStr(expr.pos)}define-syntax: wrong number of arguments`);
        const name = elems[1];
        if (name.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}define-syntax: expected symbol`);
        const transformer = elems[2];
        if (transformer.tag !== 'list' || transformer.value.length < 2 ||
            transformer.value[0].tag !== 'symbol' || transformer.value[0].value !== 'syntax-rules') {
          throw new EvalError(`${posStr(expr.pos)}define-syntax: expected syntax-rules`);
        }
        const literals: string[] = [];
        const litList = transformer.value[1];
        if (litList.tag === 'list') {
          for (const l of litList.value) {
            if (l.tag === 'symbol') literals.push(l.value);
          }
        }
        const rules: { pattern: SchemeVal; template: SchemeVal }[] = [];
        for (let i = 2; i < transformer.value.length; i++) {
          const rule = transformer.value[i];
          if (rule.tag !== 'list' || rule.value.length !== 2) {
            throw new EvalError(`${posStr(expr.pos)}define-syntax: invalid rule`);
          }
          rules.push({ pattern: rule.value[0], template: rule.value[1] });
        }
        env.set(name.value, { tag: 'syntax', rules, literals, defEnv: env, pos: expr.pos });
        return { tag: 'void' };
      }
    }

    // Check for macro invocation
    if (!BUILTIN_NAMES.has(head.value)) {
      const val = env.lookup(head.value);
      if (val && val.tag === 'syntax') {
        const expanded = expandMacro(val, expr as SchemeVal & { tag: 'list' }, env);
        return evaluate(expanded, env);
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
    return applyLambda(proc, args, expr.pos);
  }

  if (proc.tag === 'case-lambda') {
    return applyCaseLambda(proc, args, expr.pos);
  }

  throw new EvalError(`${posStr(expr.pos)}not a procedure`);
}

// ── Display ────────────────────────────────────────────────────────

// writeVal: like Scheme's `write` — strings get quotes
function writeVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': {
      const s = String(val.value);
      // Inexact numbers should always show decimal point
      if (Number.isFinite(val.value) && !s.includes('.') && !s.includes('e')) return s + '.0';
      return s;
    }
    case 'rational': return val.den === 1 ? String(val.num) : `${val.num}/${val.den}`;
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
    case 'case-lambda': return '#<procedure>';
    case 'syntax': return '#<syntax>';
    case 'record': return '#<record>';
    case 'vector': return `#(${val.value.map(writeVal).join(' ')})`;
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
