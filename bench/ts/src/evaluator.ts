import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; mutable?: boolean; pos?: Pos }
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
  | { tag: 'vector'; value: SchemeVal[]; pos?: Pos }
  | { tag: 'continuation'; k: Kont; winders: Winder[]; pos?: Pos }
  | { tag: 'values'; vals: SchemeVal[]; pos?: Pos };

type DoVar = { name: string; step?: SchemeVal };

type Winder = { inThunk: SchemeVal; outThunk: SchemeVal };

type ExnHandler =
  | { tag: 'proc'; handler: SchemeVal }
  | { tag: 'guard'; gVar: string; clauses: SchemeVal[]; gEnv: Env; guardK: Kont; ws: Winder[] };

type Kont =
  | { tag: 'halt' }
  | { tag: 'seq'; rest: SchemeVal[]; env: Env; k: Kont }
  | { tag: 'if'; t: SchemeVal; f?: SchemeVal; env: Env; k: Kont }
  | { tag: 'def'; name: string; env: Env; k: Kont }
  | { tag: 'set'; name: string; env: Env; pos?: Pos; k: Kont }
  | { tag: 'head'; argExprs: SchemeVal[]; env: Env; pos?: Pos; k: Kont }
  | { tag: 'args'; proc: SchemeVal; unevaled: SchemeVal[]; evaled: SchemeVal[]; env: Env; pos?: Pos; k: Kont }
  | { tag: 'and'; rest: SchemeVal[]; env: Env; k: Kont }
  | { tag: 'or'; rest: SchemeVal[]; env: Env; k: Kont }
  | { tag: 'let'; nm: string; done: [string, SchemeVal][]; todo: { n: string; e: SchemeVal }[]; oEnv: Env; body: SchemeVal[]; k: Kont }
  | { tag: 'lets'; nm: string; todo: { n: string; e: SchemeVal }[]; env: Env; body: SchemeVal[]; k: Kont }
  | { tag: 'letrec'; names: string[]; idx: number; inits: SchemeVal[]; env: Env; body: SchemeVal[]; k: Kont }
  | { tag: 'nlet'; loop: string; params: string[]; done: SchemeVal[]; todo: SchemeVal[]; oEnv: Env; body: SchemeVal[]; k: Kont }
  | { tag: 'case'; clauses: SchemeVal[]; env: Env; pos?: Pos; k: Kont }
  | { tag: 'cond'; cl: SchemeVal[]; ci: number; env: Env; k: Kont }
  | { tag: 'do-init'; vars: DoVar[]; done: SchemeVal[]; todo: SchemeVal[]; oEnv: Env; test: SchemeVal; body: SchemeVal[]; k: Kont }
  | { tag: 'do-test'; vars: DoVar[]; dEnv: Env; test: SchemeVal; body: SchemeVal[]; k: Kont }
  | { tag: 'do-step'; vars: DoVar[]; nv: (SchemeVal | undefined)[]; si: number; dEnv: Env; test: SchemeVal; body: SchemeVal[]; k: Kont }
  | { tag: 'dw-pre'; bodyThunk: SchemeVal; outThunk: SchemeVal; winder: Winder; k: Kont }
  | { tag: 'dw-body'; outThunk: SchemeVal; k: Kont }
  | { tag: 'dw-post'; bodyVal: SchemeVal; k: Kont }
  | { tag: 'dw-wind'; ops: { thunk: SchemeVal; ws: Winder[] }[]; targetK: Kont; targetVal: SchemeVal }
  | { tag: 'weh'; k: Kont }
  | { tag: 'guard-body'; k: Kont }
  | { tag: 'guard-start'; gVar: string; clauses: SchemeVal[]; exnVal: SchemeVal; gEnv: Env; k: Kont }
  | { tag: 'guard-test'; gVar: string; clauses: SchemeVal[]; ci: number; exnVal: SchemeVal; gEnv: Env; k: Kont }
  | { tag: 'raise-err'; k: Kont }
  | { tag: 'cwv'; consumer: SchemeVal; pos?: Pos; k: Kont };

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
  let slow = val;
  let step = 0;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
    step++;
    if (step % 2 === 0) {
      slow = (slow as any).cdr;
      if (slow === cur) throw new EvalError('not a proper list (circular)');
    }
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

function schemeEqual(a: SchemeVal, b: SchemeVal, seen?: Set<string>): boolean {
  if (isNumeric(a) && isNumeric(b)) return toFloat(a) === toFloat(b);
  if (a === b) return true;
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'boolean': return a.value === (b as typeof a).value;
    case 'string': return a.value === (b as typeof a).value;
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'nil': return true;
    case 'pair': {
      if (b.tag !== 'pair') return false;
      if (!seen) seen = new Set();
      // Use object identity pair as key for cycle detection
      const key = _pairId(a) + ',' + _pairId(b);
      if (seen.has(key)) return true; // assume equal for cycles
      seen.add(key);
      return schemeEqual(a.car, b.car, seen) && schemeEqual(a.cdr, b.cdr, seen);
    }
    case 'vector': {
      if (b.tag !== 'vector') return false;
      if (a.value.length !== b.value.length) return false;
      for (let i = 0; i < a.value.length; i++) {
        if (!schemeEqual(a.value[i], b.value[i], seen)) return false;
      }
      return true;
    }
    default: return a === b;
  }
}

// Unique identity for pair objects (for cycle detection in equal?)
let _pairIdCounter = 0;
const _pairIdMap = new WeakMap<object, number>();
function _pairId(v: SchemeVal): number {
  let id = _pairIdMap.get(v as object);
  if (id === undefined) { id = ++_pairIdCounter; _pairIdMap.set(v as object, id); }
  return id;
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

// Forward-declared; set after evaluate is defined
let _callProc: (proc: SchemeVal, args: SchemeVal[], callPos?: Pos) => SchemeVal;

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
    case 'caar': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}caar: need 1 argument`);
      if (args[0].tag !== 'pair' || args[0].car.tag !== 'pair') throw new EvalError(`${posStr(callPos)}caar: not a pair`);
      return args[0].car.car;
    }
    case 'cadr': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}cadr: need 1 argument`);
      if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw new EvalError(`${posStr(callPos)}cadr: not a pair`);
      return args[0].cdr.car;
    }
    case 'cdar': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}cdar: need 1 argument`);
      if (args[0].tag !== 'pair' || args[0].car.tag !== 'pair') throw new EvalError(`${posStr(callPos)}cdar: not a pair`);
      return args[0].car.cdr;
    }
    case 'cddr': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}cddr: need 1 argument`);
      if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw new EvalError(`${posStr(callPos)}cddr: not a pair`);
      return args[0].cdr.cdr;
    }
    case 'set-car!': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}set-car!: need 2 arguments`);
      if (args[0].tag !== 'pair') throw new EvalError(`${posStr(callPos)}set-car!: not a pair`);
      (args[0] as any).car = args[1];
      return { tag: 'void' };
    }
    case 'set-cdr!': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}set-cdr!: need 2 arguments`);
      if (args[0].tag !== 'pair') throw new EvalError(`${posStr(callPos)}set-cdr!: not a pair`);
      (args[0] as any).cdr = args[1];
      return { tag: 'void' };
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
      return { tag: 'string', value: args[0].value, mutable: true };
    }
    case 'string-set!': {
      if (args.length !== 3) throw new EvalError(`${posStr(callPos)}string-set!: need 3 arguments`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string-set!: expected string`);
      if (!args[0].mutable) throw new EvalError(`${posStr(callPos)}string-set!: strings are immutable`);
      if (args[2].tag !== 'char') throw new EvalError(`${posStr(callPos)}string-set!: expected char`);
      const si = toNumber(args[1], 'string-set!', callPos);
      if (si < 0 || si >= args[0].value.length) throw new EvalError(`${posStr(callPos)}string-set!: index out of range`);
      args[0].value = args[0].value.substring(0, si) + args[2].value + args[0].value.substring(si + 1);
      return { tag: 'void' };
    }
    // ── L15: String immutability, list/string conversion, char/integer conversion ──
    case 'string->list': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}string->list: need 1 argument`);
      if (args[0].tag !== 'string') throw new EvalError(`${posStr(callPos)}string->list: expected string`);
      let result: SchemeVal = { tag: 'nil' };
      const s = args[0].value;
      for (let i = s.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: { tag: 'char', value: s[i] }, cdr: result };
      }
      return result;
    }
    case 'list->string': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}list->string: need 1 argument`);
      let chars = '';
      let lst = args[0];
      while (lst.tag === 'pair') {
        if (lst.car.tag !== 'char') throw new EvalError(`${posStr(callPos)}list->string: expected list of chars`);
        chars += lst.car.value;
        lst = lst.cdr;
      }
      return { tag: 'string', value: chars };
    }
    case 'char->integer': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}char->integer: need 1 argument`);
      if (args[0].tag !== 'char') throw new EvalError(`${posStr(callPos)}char->integer: expected char`);
      return { tag: 'number', value: args[0].value.charCodeAt(0) };
    }
    case 'integer->char': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}integer->char: need 1 argument`);
      const code = toNumber(args[0], 'integer->char', callPos);
      return { tag: 'char', value: String.fromCharCode(code) };
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
      // Tortoise-and-hare cycle detection
      let slow = args[0];
      let fast = args[0];
      while (fast.tag === 'pair') {
        slow = (slow as any).cdr;
        fast = fast.cdr;
        if (fast.tag !== 'pair') break;
        fast = fast.cdr;
        if (slow === fast) return { tag: 'boolean', value: false }; // cycle detected
      }
      let cur = fast;
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
        result.push(_callProc(fn, fnArgs, callPos));
      }
      return arrayToSchemeList(result);
    }
    case 'member': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}member: need 2 arguments`);
      let cur = args[1];
      while (cur.tag === 'pair') {
        if (schemeEqual(args[0], cur.car)) return cur;
        cur = cur.cdr;
      }
      return { tag: 'boolean', value: false };
    }
    case 'reverse': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}reverse: need 1 argument`);
      const items = schemeListToArray(args[0]);
      return arrayToSchemeList(items.reverse());
    }
    case 'for-each': {
      if (args.length < 2) throw new EvalError(`${posStr(callPos)}for-each: need at least 2 arguments`);
      const fn = args[0];
      const lists = args.slice(1).map(a => schemeListToArray(a));
      const len = lists[0].length;
      for (let i = 0; i < len; i++) {
        const fnArgs = lists.map(l => l[i]);
        _callProc(fn, fnArgs, callPos);
      }
      return { tag: 'void' };
    }
    case 'assv': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}assv: need 2 arguments`);
      const key = args[0];
      let cur = args[1];
      while (cur.tag === 'pair') {
        if (cur.car.tag === 'pair' && schemeEq(key, cur.car.car)) return cur.car;
        cur = cur.cdr;
      }
      return { tag: 'boolean', value: false };
    }
    case 'gcd': {
      if (args.length === 0) return makeExactInt(0);
      let result = Math.abs(toNumber(args[0], 'gcd', callPos));
      for (let i = 1; i < args.length; i++) result = gcd(result, Math.abs(toNumber(args[i], 'gcd', callPos)));
      return makeExactInt(result);
    }
    case 'lcm': {
      if (args.length === 0) return makeExactInt(1);
      let result = Math.abs(toNumber(args[0], 'lcm', callPos));
      for (let i = 1; i < args.length; i++) {
        const b = Math.abs(toNumber(args[i], 'lcm', callPos));
        result = (result / gcd(result, b)) * b;
      }
      return makeExactInt(result);
    }
    case 'truncate': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}truncate: need 1 argument`);
      return makeExactInt(Math.trunc(toFloat(args[0])));
    }
    case 'round': {
      if (args.length !== 1) throw new EvalError(`${posStr(callPos)}round: need 1 argument`);
      const v = toFloat(args[0]);
      // Banker's rounding
      const rounded = Math.round(v);
      if (Math.abs(v - Math.floor(v) - 0.5) < 1e-15) {
        const floor = Math.floor(v);
        return makeExactInt(floor % 2 === 0 ? floor : floor + 1);
      }
      return makeExactInt(rounded);
    }
    case 'make-string': {
      if (args.length < 1 || args.length > 2) throw new EvalError(`${posStr(callPos)}make-string: need 1-2 arguments`);
      const k = toNumber(args[0], 'make-string', callPos);
      const ch = args.length === 2 ? (args[1].tag === 'char' ? args[1].value : '\0') : '\0';
      return { tag: 'string', value: ch.repeat(k), mutable: true };
    }
    case 'string': {
      // (string char1 char2 ...) -> string from chars
      let s = '';
      for (const a of args) {
        if (a.tag !== 'char') throw new EvalError(`${posStr(callPos)}string: expected char`);
        s += a.value;
      }
      return { tag: 'string', value: s };
    }
    case 'error': {
      const msg = args.map(a => displayVal(a)).join(' ');
      throw new EvalError(msg);
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
    case 'string>?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string>?: need 2 arguments`);
      if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${posStr(callPos)}string>?: expected strings`);
      return { tag: 'boolean', value: args[0].value > args[1].value };
    }
    case 'string<=?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string<=?: need 2 arguments`);
      if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${posStr(callPos)}string<=?: expected strings`);
      return { tag: 'boolean', value: args[0].value <= args[1].value };
    }
    case 'string>=?': {
      if (args.length !== 2) throw new EvalError(`${posStr(callPos)}string>=?: need 2 arguments`);
      if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError(`${posStr(callPos)}string>=?: expected strings`);
      return { tag: 'boolean', value: args[0].value >= args[1].value };
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
      return { tag: 'boolean', value: t === 'lambda' || t === 'builtin' || t === 'case-lambda' || t === 'continuation' };
    }
    case 'apply': {
      if (args.length < 2) throw new EvalError(`${posStr(callPos)}apply: need at least 2 arguments`);
      const fn = args[0];
      const lastArg = args[args.length - 1];
      const prefixArgs = args.slice(1, -1);
      const tailArgs = schemeListToArray(lastArg);
      const allArgs = [...prefixArgs, ...tailArgs];
      return _callProc(fn, allArgs, callPos);
    }
    default:
      throw new EvalError(`${posStr(callPos)}unknown builtin: ${name}`);
  }
}

const BUILTIN_NAMES = new Set([
  '+', '-', '*', '/', '<', '>', '=', '<=', '>=',
  'cons', 'car', 'cdr', 'caar', 'cadr', 'cdar', 'cddr', 'null?', 'pair?', 'list', 'length', 'append',
  'set-car!', 'set-cdr!',
  'number?', 'boolean?', 'string?', 'symbol?', 'not',
  'display', 'write', 'newline',
  'string-append', 'string-length', 'substring',
  'string->number', 'number->string',
  'symbol->string', 'string->symbol',
  'string-ref', 'char?',
  'string-copy', 'string-set!',
  // L15
  'string->list', 'list->string', 'char->integer', 'integer->char',
  'apply',
  // L09
  'abs', 'modulo', 'remainder', 'quotient', 'min', 'max', 'expt',
  'zero?', 'positive?', 'negative?', 'odd?', 'even?',
  'list-ref', 'list-tail', 'list?', 'assoc', 'assv', 'map', 'for-each', 'reverse', 'member', 'equal?', 'eq?',
  'error', 'gcd', 'lcm', 'truncate', 'round', 'make-string', 'string',
  'string>?', 'string<=?', 'string>=?',
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
  // L18
  'call/cc', 'call-with-current-continuation',
  'dynamic-wind',
  // L20
  'raise', 'with-exception-handler',
  // L21
  'values', 'call-with-values',
]);

function bindLambdaArgs(proc: { params: string[]; rest?: string; body: SchemeVal[] }, args: SchemeVal[], procEnv: Env, callPos?: Pos): Env {
  if (proc.rest) {
    if (args.length < proc.params.length) {
      throw new EvalError(`${posStr(callPos)}lambda: expected at least ${proc.params.length} arguments, got ${args.length}`);
    }
  } else {
    if (args.length !== proc.params.length) {
      throw new EvalError(`${posStr(callPos)}lambda: expected ${proc.params.length} arguments, got ${args.length}`);
    }
  }
  const callEnv = new Env(procEnv);
  for (let i = 0; i < proc.params.length; i++) {
    callEnv.set(proc.params[i], args[i]);
  }
  if (proc.rest) {
    callEnv.set(proc.rest, arrayToSchemeList(args.slice(proc.params.length)));
  }
  return callEnv;
}

function evaluate(startExpr: SchemeVal, startEnv: Env, startK: Kont = { tag: 'halt' }): SchemeVal {
  let expr = startExpr;
  let env = startEnv;
  let k: Kont = startK;
  let val: SchemeVal = NIL;
  let isEval = true;
  const winders: Winder[] = [];
  const exnHandlers: ExnHandler[] = [];

  function doApply(proc: SchemeVal, args: SchemeVal[], pos?: Pos, kk?: Kont): void {
    const kCont = kk!;
    if (proc.tag === 'builtin') {
      if (proc.name === 'call/cc' || proc.name === 'call-with-current-continuation') {
        if (args.length !== 1) throw new EvalError(`${posStr(pos)}call/cc: expected 1 argument, got ${args.length}`);
        const contVal: SchemeVal = { tag: 'continuation', k: kCont, winders: [...winders] };
        doApply(args[0], [contVal], pos, kCont);
        return;
      }
      if (proc.name === 'dynamic-wind') {
        if (args.length !== 3) throw new EvalError(`${posStr(pos)}dynamic-wind: expected 3 arguments, got ${args.length}`);
        const [inThunk, bodyThunk, outThunk] = args;
        const winder: Winder = { inThunk, outThunk };
        const dwK: Kont = { tag: 'dw-pre', bodyThunk, outThunk, winder, k: kCont };
        doApply(inThunk, [], pos, dwK);
        return;
      }
      if (proc.name === 'raise') {
        if (args.length !== 1) throw new EvalError(`${posStr(pos)}raise: expected 1 argument, got ${args.length}`);
        const exnVal = args[0];
        if (exnHandlers.length === 0) throw new EvalError(`unhandled exception: ${writeVal(exnVal)}`);
        const handler = exnHandlers.pop()!;
        if (handler.tag === 'proc') {
          const errK: Kont = { tag: 'raise-err', k: kCont };
          doApply(handler.handler, [exnVal], pos, errK);
          return;
        }
        // guard handler: unwind winders to guard's state, then evaluate clauses
        const targetWinders = handler.ws;
        const clauseStartK: Kont = { tag: 'guard-start', gVar: handler.gVar, clauses: handler.clauses, exnVal, gEnv: handler.gEnv, k: handler.guardK };
        let cp = 0;
        while (cp < winders.length && cp < targetWinders.length && winders[cp] === targetWinders[cp]) cp++;
        const ops: { thunk: SchemeVal; ws: Winder[] }[] = [];
        for (let i = winders.length - 1; i >= cp; i--) {
          ops.push({ thunk: winders[i].outThunk, ws: winders.slice(0, i) });
        }
        for (let i = cp; i < targetWinders.length; i++) {
          ops.push({ thunk: targetWinders[i].inThunk, ws: targetWinders.slice(0, i + 1) });
        }
        if (ops.length === 0) {
          val = exnVal; k = clauseStartK; isEval = false;
        } else {
          const [first, ...rest] = ops;
          winders.length = 0; winders.push(...first.ws);
          const windK: Kont = { tag: 'dw-wind', ops: rest, targetK: clauseStartK, targetVal: exnVal };
          doApply(first.thunk, [], pos, windK);
        }
        return;
      }
      if (proc.name === 'with-exception-handler') {
        if (args.length !== 2) throw new EvalError(`${posStr(pos)}with-exception-handler: expected 2 arguments, got ${args.length}`);
        const [handler, thunk] = args;
        exnHandlers.push({ tag: 'proc', handler });
        const wehK: Kont = { tag: 'weh', k: kCont };
        doApply(thunk, [], pos, wehK);
        return;
      }
      if (proc.name === 'values') {
        if (args.length === 1) { val = args[0]; k = kCont; isEval = false; return; }
        val = { tag: 'values', vals: args }; k = kCont; isEval = false; return;
      }
      if (proc.name === 'call-with-values') {
        if (args.length !== 2) throw new EvalError(`${posStr(pos)}call-with-values: expected 2 arguments, got ${args.length}`);
        const [producer, consumer] = args;
        const cwvK: Kont = { tag: 'cwv', consumer, pos, k: kCont };
        doApply(producer, [], pos, cwvK);
        return;
      }
      if (proc.name === 'apply') {
        if (args.length < 2) throw new EvalError(`${posStr(pos)}apply: need at least 2 arguments`);
        const fn = args[0];
        const lastArg = args[args.length - 1];
        const prefixArgs = args.slice(1, -1);
        const tailArgs = schemeListToArray(lastArg);
        doApply(fn, [...prefixArgs, ...tailArgs], pos, kCont);
        return;
      }
      val = evalBuiltin(proc.name, args, pos);
      k = kCont; isEval = false;
      return;
    }
    if (proc.tag === 'lambda') {
      const callEnv = bindLambdaArgs(proc, args, proc.env, pos);
      if (proc.body.length === 0) { val = { tag: 'void' }; k = kCont; isEval = false; return; }
      if (proc.body.length === 1) { expr = proc.body[0]; env = callEnv; k = kCont; isEval = true; return; }
      expr = proc.body[0]; env = callEnv;
      k = { tag: 'seq', rest: proc.body.slice(1), env: callEnv, k: kCont };
      isEval = true; return;
    }
    if (proc.tag === 'case-lambda') {
      for (const clause of proc.clauses) {
        const ok = clause.rest ? args.length >= clause.params.length : args.length === clause.params.length;
        if (ok) {
          const callEnv = bindLambdaArgs(clause, args, proc.env, pos);
          if (clause.body.length === 0) { val = { tag: 'void' }; k = kCont; isEval = false; return; }
          if (clause.body.length === 1) { expr = clause.body[0]; env = callEnv; k = kCont; isEval = true; return; }
          expr = clause.body[0]; env = callEnv;
          k = { tag: 'seq', rest: clause.body.slice(1), env: callEnv, k: kCont };
          isEval = true; return;
        }
      }
      throw new EvalError(`${posStr(pos)}case-lambda: no matching clause for ${args.length} arguments`);
    }
    if (proc.tag === 'continuation') {
      const targetVal: SchemeVal = args.length > 0 ? args[0] : { tag: 'void' };
      const targetWinders = proc.winders;
      const targetK = proc.k;

      // Find common prefix
      let cp = 0;
      while (cp < winders.length && cp < targetWinders.length && winders[cp] === targetWinders[cp]) cp++;

      // Build wind operations: unwind then rewind
      const ops: { thunk: SchemeVal; ws: Winder[] }[] = [];
      for (let i = winders.length - 1; i >= cp; i--) {
        ops.push({ thunk: winders[i].outThunk, ws: winders.slice(0, i) });
      }
      for (let i = cp; i < targetWinders.length; i++) {
        ops.push({ thunk: targetWinders[i].inThunk, ws: targetWinders.slice(0, i + 1) });
      }

      if (ops.length === 0) {
        val = targetVal; k = targetK; isEval = false;
      } else {
        const [first, ...rest] = ops;
        winders.length = 0; winders.push(...first.ws);
        const windK: Kont = { tag: 'dw-wind', ops: rest, targetK, targetVal };
        doApply(first.thunk, [], pos, windK);
      }
      return;
    }
    throw new EvalError(`${posStr(pos)}not a procedure`);
  }

  function evalBody(body: SchemeVal[], bodyEnv: Env, kk: Kont): void {
    if (body.length === 0) { val = { tag: 'void' }; k = kk; isEval = false; return; }
    if (body.length === 1) { expr = body[0]; env = bodyEnv; k = kk; isEval = true; return; }
    expr = body[0]; env = bodyEnv;
    k = { tag: 'seq', rest: body.slice(1), env: bodyEnv, k: kk };
    isEval = true;
  }

  function startArgs(proc: SchemeVal, argExprs: SchemeVal[], argEnv: Env, pos: Pos | undefined, kk: Kont): void {
    if (argExprs.length === 0) {
      doApply(proc, [], pos, kk);
      return;
    }
    const n = argExprs.length;
    k = { tag: 'args', proc, unevaled: argExprs.slice(0, -1), evaled: [], env: argEnv, pos, k: kk };
    expr = argExprs[n - 1]; env = argEnv;
    isEval = true;
  }

  for (;;) {
    if (!isEval) {
      // ── APPLY CONTINUATION ──
      switch (k.tag) {
        case 'halt': return val;

        case 'seq': {
          if (k.rest.length === 1) { expr = k.rest[0]; env = k.env; k = k.k; isEval = true; break; }
          expr = k.rest[0]; env = k.env;
          k = { tag: 'seq', rest: k.rest.slice(1), env: k.env, k: k.k };
          isEval = true; break;
        }

        case 'if': {
          if (isTruthy(val)) { expr = k.t; env = k.env; k = k.k; isEval = true; break; }
          if (k.f !== undefined) { expr = k.f; env = k.env; k = k.k; isEval = true; break; }
          val = { tag: 'void' }; k = k.k; break;
        }

        case 'def': { k.env.set(k.name, val); val = { tag: 'void' }; k = k.k; break; }

        case 'set': { k.env.update(k.name, val, k.pos); val = { tag: 'void' }; k = k.k; break; }

        case 'head': {
          startArgs(val, k.argExprs, k.env, k.pos, k.k);
          break;
        }

        case 'args': {
          const newEvaled = [...k.evaled, val];
          if (k.unevaled.length === 0) {
            doApply(k.proc, newEvaled.reverse(), k.pos, k.k);
            break;
          }
          const last = k.unevaled.length - 1;
          expr = k.unevaled[last]; env = k.env;
          k = { tag: 'args', proc: k.proc, unevaled: k.unevaled.slice(0, last), evaled: newEvaled, env: k.env, pos: k.pos, k: k.k };
          isEval = true; break;
        }

        case 'and': {
          if (!isTruthy(val)) { k = k.k; break; }
          if (k.rest.length === 1) { expr = k.rest[0]; env = k.env; k = k.k; isEval = true; break; }
          expr = k.rest[0]; env = k.env;
          k = { tag: 'and', rest: k.rest.slice(1), env: k.env, k: k.k };
          isEval = true; break;
        }

        case 'or': {
          if (isTruthy(val)) { k = k.k; break; }
          if (k.rest.length === 1) { expr = k.rest[0]; env = k.env; k = k.k; isEval = true; break; }
          expr = k.rest[0]; env = k.env;
          k = { tag: 'or', rest: k.rest.slice(1), env: k.env, k: k.k };
          isEval = true; break;
        }

        case 'let': {
          const done: [string, SchemeVal][] = [...k.done, [k.nm, val]];
          if (k.todo.length === 0) {
            const letEnv = new Env(k.oEnv);
            for (const [n, v] of done) letEnv.set(n, v);
            evalBody(k.body, letEnv, k.k);
            break;
          }
          const next = k.todo[0];
          expr = next.e; env = k.oEnv;
          k = { tag: 'let', nm: next.n, done, todo: k.todo.slice(1), oEnv: k.oEnv, body: k.body, k: k.k };
          isEval = true; break;
        }

        case 'lets': {
          const nextEnv = new Env(k.env);
          nextEnv.set(k.nm, val);
          if (k.todo.length === 0) {
            evalBody(k.body, nextEnv, k.k);
            break;
          }
          const next = k.todo[0];
          expr = next.e; env = nextEnv;
          k = { tag: 'lets', nm: next.n, todo: k.todo.slice(1), env: nextEnv, body: k.body, k: k.k };
          isEval = true; break;
        }

        case 'letrec': {
          k.env.set(k.names[k.idx], val);
          const nextIdx = k.idx + 1;
          if (nextIdx >= k.inits.length) {
            evalBody(k.body, k.env, k.k);
            break;
          }
          expr = k.inits[nextIdx]; env = k.env;
          k = { tag: 'letrec', names: k.names, idx: nextIdx, inits: k.inits, env: k.env, body: k.body, k: k.k };
          isEval = true; break;
        }

        case 'nlet': {
          const done = [...k.done, val];
          if (k.todo.length === 0) {
            const loopLam: SchemeVal = { tag: 'lambda', params: k.params, body: k.body, env: k.oEnv };
            const loopEnv = new Env(k.oEnv);
            loopEnv.set(k.loop, loopLam);
            (loopLam as any).env = loopEnv;
            const callEnv = new Env(loopEnv);
            for (let i = 0; i < k.params.length; i++) callEnv.set(k.params[i], done[i]);
            evalBody(k.body, callEnv, k.k);
            break;
          }
          expr = k.todo[0]; env = k.oEnv;
          k = { tag: 'nlet', loop: k.loop, params: k.params, done, todo: k.todo.slice(1), oEnv: k.oEnv, body: k.body, k: k.k };
          isEval = true; break;
        }

        case 'case': {
          const key = val;
          let matched = false;
          for (let ci = 0; ci < k.clauses.length; ci++) {
            const clause = k.clauses[ci];
            if (clause.tag !== 'list' || clause.value.length < 2) throw new EvalError(`${posStr(k.pos)}case: invalid clause`);
            const datums = clause.value[0];
            let isMatch = false;
            if (datums.tag === 'symbol' && datums.value === 'else') isMatch = true;
            else if (datums.tag === 'list') {
              for (const d of datums.value) {
                if (schemeEq(key, quoteDatum(d))) { isMatch = true; break; }
              }
            } else {
              throw new EvalError(`${posStr(k.pos)}case: datums must be a list`);
            }
            if (isMatch) {
              evalBody(clause.value.slice(1), k.env, k.k);
              matched = true;
              break;
            }
          }
          if (!matched) { val = { tag: 'void' }; k = k.k; }
          break;
        }

        case 'cond': {
          const clause = k.cl[k.ci];
          if (isTruthy(val)) {
            if (clause.tag === 'list' && clause.value.length === 1) {
              k = k.k; break;
            }
            if (clause.tag === 'list') {
              evalBody(clause.value.slice(1), k.env, k.k);
            } else {
              k = k.k;
            }
            break;
          }
          const nextCi = k.ci + 1;
          if (nextCi >= k.cl.length) { val = { tag: 'void' }; k = k.k; break; }
          const nextClause = k.cl[nextCi];
          if (nextClause.tag !== 'list' || nextClause.value.length < 1) throw new EvalError('cond: invalid clause');
          const nextTest = nextClause.value[0];
          if (nextTest.tag === 'symbol' && nextTest.value === 'else') {
            evalBody(nextClause.value.slice(1), k.env, k.k);
            break;
          }
          expr = nextTest; env = k.env;
          k = { tag: 'cond', cl: k.cl, ci: nextCi, env: k.env, k: k.k };
          isEval = true; break;
        }

        case 'do-init': {
          const done = [...k.done, val];
          if (k.todo.length === 0) {
            const doEnv = new Env(k.oEnv);
            for (let i = 0; i < k.vars.length; i++) doEnv.set(k.vars[i].name, done[i]);
            const kOuter = k.k;
            const vars = k.vars; const test = k.test; const body = k.body;
            k = { tag: 'do-test', vars, dEnv: doEnv, test, body, k: kOuter };
            expr = (test as any).value[0]; env = doEnv;
            isEval = true; break;
          }
          expr = k.todo[0]; env = k.oEnv;
          k = { tag: 'do-init', vars: k.vars, done, todo: k.todo.slice(1), oEnv: k.oEnv, test: k.test, body: k.body, k: k.k };
          isEval = true; break;
        }

        case 'do-test': {
          const testClause = k.test as SchemeVal & { tag: 'list' };
          if (isTruthy(val)) {
            if (testClause.value.length === 1) { val = { tag: 'void' }; k = k.k; break; }
            evalBody(testClause.value.slice(1), k.dEnv, k.k);
            break;
          }
          // Test false: eval body then steps
          const stepK: Kont = { tag: 'do-step', vars: k.vars, nv: new Array(k.vars.length).fill(undefined), si: -1, dEnv: k.dEnv, test: k.test, body: k.body, k: k.k };
          if (k.body.length === 0) {
            val = { tag: 'void' }; k = stepK; break;
          }
          if (k.body.length === 1) {
            expr = k.body[0]; env = k.dEnv; k = stepK; isEval = true; break;
          }
          const bExprs = k.body; const bEnv = k.dEnv;
          k = { tag: 'seq', rest: bExprs.slice(1), env: bEnv, k: stepK };
          expr = bExprs[0]; env = bEnv; isEval = true; break;
        }

        case 'do-step': {
          const nv = [...k.nv];
          if (k.si >= 0) nv[k.si] = val;
          // Find next var with step
          let nextSi = k.si + 1;
          while (nextSi < k.vars.length && k.vars[nextSi].step === undefined) nextSi++;
          if (nextSi >= k.vars.length) {
            // All steps done, update and loop to test
            for (let i = 0; i < k.vars.length; i++) {
              if (nv[i] !== undefined) k.dEnv.set(k.vars[i].name, nv[i]!);
            }
            const testK: Kont = { tag: 'do-test', vars: k.vars, dEnv: k.dEnv, test: k.test, body: k.body, k: k.k };
            expr = (k.test as any).value[0]; env = k.dEnv; k = testK;
            isEval = true; break;
          }
          expr = k.vars[nextSi].step!; env = k.dEnv;
          k = { tag: 'do-step', vars: k.vars, nv, si: nextSi, dEnv: k.dEnv, test: k.test, body: k.body, k: k.k };
          isEval = true; break;
        }

        case 'dw-pre': {
          // in-thunk completed, push winder and call body-thunk
          winders.push(k.winder);
          const dwK: Kont = { tag: 'dw-body', outThunk: k.outThunk, k: k.k };
          doApply(k.bodyThunk, [], undefined, dwK);
          break;
        }

        case 'dw-body': {
          // body-thunk completed, pop winder, save value, call out-thunk
          winders.pop();
          const savedVal = val;
          const dwK: Kont = { tag: 'dw-post', bodyVal: savedVal, k: k.k };
          doApply(k.outThunk, [], undefined, dwK);
          break;
        }

        case 'dw-post': {
          // out-thunk completed, return saved body value
          val = k.bodyVal; k = k.k; break;
        }

        case 'dw-wind': {
          // Wind operation thunk completed
          if (k.ops.length === 0) {
            val = k.targetVal; k = k.targetK;
          } else {
            const [next, ...rest] = k.ops;
            winders.length = 0; winders.push(...next.ws);
            const windK: Kont = { tag: 'dw-wind', ops: rest, targetK: k.targetK, targetVal: k.targetVal };
            doApply(next.thunk, [], undefined, windK);
          }
          break;
        }

        case 'weh': {
          exnHandlers.pop();
          k = k.k;
          break;
        }

        case 'guard-body': {
          exnHandlers.pop();
          k = k.k;
          break;
        }

        case 'guard-start': {
          const clauseEnv = new Env(k.gEnv);
          clauseEnv.set(k.gVar, k.exnVal);
          if (k.clauses.length === 0) throw new EvalError(`unhandled exception: ${writeVal(k.exnVal)}`);
          const firstClause = k.clauses[0];
          if (firstClause.tag !== 'list' || firstClause.value.length < 1) throw new EvalError('guard: invalid clause');
          if (firstClause.value[0].tag === 'symbol' && firstClause.value[0].value === 'else') {
            evalBody(firstClause.value.slice(1), clauseEnv, k.k);
            break;
          }
          k = { tag: 'guard-test', gVar: k.gVar, clauses: k.clauses, ci: 0, exnVal: k.exnVal, gEnv: k.gEnv, k: k.k };
          expr = firstClause.value[0]; env = clauseEnv;
          isEval = true; break;
        }

        case 'guard-test': {
          if (val.tag !== 'boolean' || val.value !== false) {
            // Test passed — evaluate clause body
            const clause = k.clauses[k.ci];
            if (clause.tag !== 'list') throw new EvalError('guard: invalid clause');
            const clauseEnv = new Env(k.gEnv);
            clauseEnv.set(k.gVar, k.exnVal);
            if (clause.value.length > 1) {
              evalBody((clause.value as SchemeVal[]).slice(1), clauseEnv, k.k);
            } else {
              k = k.k; // no body, return test value
            }
            break;
          }
          // Test failed — try next clause
          const nextCi = k.ci + 1;
          if (nextCi >= k.clauses.length) {
            throw new EvalError(`unhandled exception: ${writeVal(k.exnVal)}`);
          }
          const nextClause = k.clauses[nextCi];
          if (nextClause.tag !== 'list' || nextClause.value.length < 1) throw new EvalError('guard: invalid clause');
          const clauseEnv = new Env(k.gEnv);
          clauseEnv.set(k.gVar, k.exnVal);
          if (nextClause.value[0].tag === 'symbol' && nextClause.value[0].value === 'else') {
            evalBody(nextClause.value.slice(1), clauseEnv, k.k);
            break;
          }
          k = { tag: 'guard-test', gVar: k.gVar, clauses: k.clauses, ci: nextCi, exnVal: k.exnVal, gEnv: k.gEnv, k: k.k };
          expr = nextClause.value[0]; env = clauseEnv;
          isEval = true; break;
        }

        case 'raise-err': {
          throw new EvalError('exception handler returned from raise');
        }

        case 'cwv': {
          const consumer = k.consumer;
          const cArgs = val.tag === 'values' ? val.vals : [val];
          doApply(consumer, cArgs, k.pos, k.k);
          break;
        }
      }
      continue;
    }

    // ── EVAL ──
    if (expr.tag === 'number' || expr.tag === 'rational' || expr.tag === 'boolean' || expr.tag === 'string' || expr.tag === 'char') {
      val = expr; isEval = false; continue;
    }
    if (expr.tag === 'nil' || expr.tag === 'pair') { val = expr; isEval = false; continue; }

    if (expr.tag === 'symbol') {
      const envVal = env.lookup(expr.value);
      if (envVal !== undefined) { val = envVal; isEval = false; continue; }
      if (BUILTIN_NAMES.has(expr.value)) { val = { tag: 'builtin', name: expr.value, pos: expr.pos }; isEval = false; continue; }
      throw new EvalError(`${posStr(expr.pos)}unbound variable: ${expr.value}`);
    }

    if (expr.tag !== 'list') { val = expr; isEval = false; continue; }

    const elems = expr.value;
    if (elems.length === 0) throw new EvalError(`${posStr(expr.pos)}empty application`);
    const head = elems[0];

    if (head.tag === 'symbol') {
      switch (head.value) {
        case 'quote': {
          if (elems.length !== 2) throw new EvalError(`${posStr(expr.pos)}quote: wrong number of arguments`);
          val = quoteDatum(elems[1]); isEval = false; continue;
        }
        case 'if': {
          if (elems.length < 3 || elems.length > 4) throw new EvalError(`${posStr(expr.pos)}if: wrong number of arguments`);
          k = { tag: 'if', t: elems[2], f: elems.length === 4 ? elems[3] : undefined, env, k };
          expr = elems[1]; continue;
        }
        case 'define': {
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}define: wrong number of arguments`);
          const target = elems[1];
          if (target.tag === 'symbol') {
            k = { tag: 'def', name: target.value, env, k };
            expr = elems[2]; continue;
          }
          if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
            const name = target.value[0].value;
            const paramList: SchemeVal = { tag: 'list', value: target.value.slice(1) };
            const { params, rest } = parseParams(paramList, expr.pos);
            const body = elems.slice(2);
            env.set(name, { tag: 'lambda', params, rest, body, env });
            val = { tag: 'void' }; isEval = false; continue;
          }
          throw new EvalError(`${posStr(expr.pos)}define: invalid syntax`);
        }
        case 'lambda': {
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}lambda: wrong number of arguments`);
          const { params, rest } = parseParams(elems[1], expr.pos);
          val = { tag: 'lambda', params, rest, body: elems.slice(2), env };
          isEval = false; continue;
        }
        case 'case-lambda': {
          if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}case-lambda: need at least 1 clause`);
          const clauses: { params: string[]; rest?: string; body: SchemeVal[] }[] = [];
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.value.length < 2)
              throw new EvalError(`${posStr(expr.pos)}case-lambda: invalid clause`);
            const { params, rest } = parseParams(clause.value[0], expr.pos);
            clauses.push({ params, rest, body: clause.value.slice(1) });
          }
          val = { tag: 'case-lambda', clauses, env, pos: expr.pos };
          isEval = false; continue;
        }
        case 'set!': {
          if (elems.length !== 3) throw new EvalError(`${posStr(expr.pos)}set!: wrong number of arguments`);
          const target = elems[1];
          if (target.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}set!: target must be a symbol`);
          k = { tag: 'set', name: target.value, env, pos: expr.pos, k };
          expr = elems[2]; continue;
        }
        case 'begin': {
          if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}begin: need at least 1 expression`);
          evalBody(elems.slice(1), env, k);
          continue;
        }
        case 'let': {
          // Named let: (let name ((var init) ...) body...)
          if (elems.length >= 3 && elems[1].tag === 'symbol') {
            const loopName = elems[1].value;
            const bindingsList = elems[2];
            if (bindingsList.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}let: invalid bindings`);
            const paramNames: string[] = [];
            const initExprs: SchemeVal[] = [];
            for (const b of bindingsList.value) {
              if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
                throw new EvalError(`${posStr(expr.pos)}let: invalid binding`);
              paramNames.push(b.value[0].value);
              initExprs.push(b.value[1]);
            }
            const body = elems.slice(3);
            if (initExprs.length === 0) {
              const loopLam: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
              const loopEnv = new Env(env);
              loopEnv.set(loopName, loopLam);
              (loopLam as any).env = loopEnv;
              const callEnv = new Env(loopEnv);
              evalBody(body, callEnv, k);
              continue;
            }
            k = { tag: 'nlet', loop: loopName, params: paramNames, done: [], todo: initExprs.slice(1), oEnv: env, body, k };
            expr = initExprs[0]; continue;
          }
          // Regular let
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}let: wrong number of arguments`);
          const bindings = elems[1];
          if (bindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}let: invalid bindings`);
          const body = elems.slice(2);
          if (bindings.value.length === 0) {
            evalBody(body, new Env(env), k);
            continue;
          }
          const parsed = bindings.value.map(b => {
            if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
              throw new EvalError(`${posStr(expr.pos)}let: invalid binding`);
            return { n: (b.value[0] as any).value as string, e: b.value[1] };
          });
          k = { tag: 'let', nm: parsed[0].n, done: [], todo: parsed.slice(1), oEnv: env, body, k };
          expr = parsed[0].e; continue;
        }
        case 'let*': {
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}let*: wrong number of arguments`);
          const lsBindings = elems[1];
          if (lsBindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}let*: invalid bindings`);
          const body = elems.slice(2);
          if (lsBindings.value.length === 0) {
            evalBody(body, new Env(env), k);
            continue;
          }
          const parsed = lsBindings.value.map(b => {
            if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
              throw new EvalError(`${posStr(expr.pos)}let*: invalid binding`);
            return { n: (b.value[0] as any).value as string, e: b.value[1] };
          });
          const lsEnv = new Env(env);
          k = { tag: 'lets', nm: parsed[0].n, todo: parsed.slice(1), env: lsEnv, body, k };
          expr = parsed[0].e; env = lsEnv; continue;
        }
        case 'letrec': {
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}letrec: wrong number of arguments`);
          const bindings = elems[1];
          if (bindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}letrec: invalid bindings`);
          const body = elems.slice(2);
          if (bindings.value.length === 0) {
            evalBody(body, new Env(env), k);
            continue;
          }
          const names: string[] = [];
          const inits: SchemeVal[] = [];
          const letrecEnv = new Env(env);
          for (const b of bindings.value) {
            if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
              throw new EvalError(`${posStr(expr.pos)}letrec: invalid binding`);
            const nm = (b.value[0] as any).value as string;
            names.push(nm);
            inits.push(b.value[1]);
            letrecEnv.set(nm, { tag: 'void' });
          }
          k = { tag: 'letrec', names, idx: 0, inits, env: letrecEnv, body, k };
          expr = inits[0]; env = letrecEnv; continue;
        }
        case 'letrec*': {
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}letrec*: wrong number of arguments`);
          const bindings = elems[1];
          if (bindings.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}letrec*: invalid bindings`);
          const body = elems.slice(2);
          if (bindings.value.length === 0) {
            evalBody(body, new Env(env), k);
            continue;
          }
          const names: string[] = [];
          const inits: SchemeVal[] = [];
          const letrecEnv = new Env(env);
          for (const b of bindings.value) {
            if (b.tag !== 'list' || b.value.length !== 2 || b.value[0].tag !== 'symbol')
              throw new EvalError(`${posStr(expr.pos)}letrec*: invalid binding`);
            const nm = (b.value[0] as any).value as string;
            names.push(nm);
            inits.push(b.value[1]);
            letrecEnv.set(nm, { tag: 'void' });
          }
          k = { tag: 'letrec', names, idx: 0, inits, env: letrecEnv, body, k };
          expr = inits[0]; env = letrecEnv; continue;
        }
        case 'case': {
          if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}case: wrong number of arguments`);
          k = { tag: 'case', clauses: elems.slice(2), env, pos: expr.pos, k };
          expr = elems[1]; continue;
        }
        case 'do': {
          if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}do: wrong number of arguments`);
          const varSpecs = elems[1];
          const testClause = elems[2];
          if (varSpecs.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}do: invalid variable specs`);
          if (testClause.tag !== 'list' || testClause.value.length < 1)
            throw new EvalError(`${posStr(expr.pos)}do: invalid test clause`);
          const vars: DoVar[] = [];
          const initExprs: SchemeVal[] = [];
          for (const spec of varSpecs.value) {
            if (spec.tag !== 'list' || spec.value.length < 2 || spec.value[0].tag !== 'symbol')
              throw new EvalError(`${posStr(expr.pos)}do: invalid variable spec`);
            vars.push({ name: spec.value[0].value, step: spec.value.length >= 3 ? spec.value[2] : undefined });
            initExprs.push(spec.value[1]);
          }
          const bodyExprs = elems.slice(3);
          if (initExprs.length === 0) {
            const doEnv = new Env(env);
            k = { tag: 'do-test', vars, dEnv: doEnv, test: testClause, body: bodyExprs, k };
            expr = testClause.value[0]; env = doEnv; continue;
          }
          k = { tag: 'do-init', vars, done: [], todo: initExprs.slice(1), oEnv: env, test: testClause, body: bodyExprs, k };
          expr = initExprs[0]; continue;
        }
        case 'cond': {
          if (elems.length < 2) throw new EvalError(`${posStr(expr.pos)}cond: need at least 1 clause`);
          const clauses = elems.slice(1);
          const first = clauses[0];
          if (first.tag !== 'list' || first.value.length < 1) throw new EvalError(`${posStr(expr.pos)}cond: invalid clause`);
          if (first.value[0].tag === 'symbol' && first.value[0].value === 'else') {
            evalBody(first.value.slice(1), env, k);
            continue;
          }
          k = { tag: 'cond', cl: clauses, ci: 0, env, k };
          expr = first.value[0]; continue;
        }
        case 'and': {
          if (elems.length === 1) { val = { tag: 'boolean', value: true }; isEval = false; continue; }
          if (elems.length === 2) { expr = elems[1]; continue; }
          k = { tag: 'and', rest: elems.slice(2), env, k };
          expr = elems[1]; continue;
        }
        case 'or': {
          if (elems.length === 1) { val = { tag: 'boolean', value: false }; isEval = false; continue; }
          if (elems.length === 2) { expr = elems[1]; continue; }
          k = { tag: 'or', rest: elems.slice(2), env, k };
          expr = elems[1]; continue;
        }
        case 'define-record-type': {
          if (elems.length < 4) throw new EvalError(`${posStr(expr.pos)}define-record-type: invalid syntax`);
          const rtId = newRecordTypeId();
          const ctorForm = elems[2];
          if (ctorForm.tag !== 'list' || ctorForm.value.length < 1 || ctorForm.value[0].tag !== 'symbol')
            throw new EvalError(`${posStr(expr.pos)}define-record-type: invalid constructor`);
          const ctorName = ctorForm.value[0].value;
          const ctorFields = ctorForm.value.slice(1).map(f => {
            if (f.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}define-record-type: field must be symbol`);
            return f.value;
          });
          const ctorKey = `__native_${ctorName}_${_recordTypeCounter}`;
          _nativeFns.set(ctorKey, (args: SchemeVal[]) => {
            if (args.length !== ctorFields.length)
              throw new EvalError(`${ctorName}: expected ${ctorFields.length} arguments, got ${args.length}`);
            const fields = new Map<string, SchemeVal>();
            for (let i = 0; i < ctorFields.length; i++) fields.set(ctorFields[i], args[i]);
            return { tag: 'record', type: rtId, fields };
          });
          env.set(ctorName, { tag: 'builtin', name: ctorKey });
          const predName = elems[3];
          if (predName.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}define-record-type: predicate must be symbol`);
          const predKey = `__native_${predName.value}_${_recordTypeCounter}`;
          _nativeFns.set(predKey, (args: SchemeVal[]) => {
            if (args.length !== 1) throw new EvalError(`${predName.value}: expected 1 argument`);
            return { tag: 'boolean', value: args[0].tag === 'record' && args[0].type === rtId };
          });
          env.set(predName.value, { tag: 'builtin', name: predKey });
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
          val = { tag: 'void' }; isEval = false; continue;
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
          val = { tag: 'void' }; isEval = false; continue;
        }
        case 'guard': {
          // (guard (var clause ...) body ...)
          const spec = elems[1];
          if (spec.tag !== 'list' || spec.value.length < 1) throw new EvalError(`${posStr(expr.pos)}guard: invalid syntax`);
          const varSym = spec.value[0];
          if (varSym.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}guard: variable must be a symbol`);
          const clauses = spec.value.slice(1);
          const body = elems.slice(2);
          exnHandlers.push({ tag: 'guard', gVar: varSym.value, clauses, gEnv: env, guardK: k, ws: [...winders] });
          const gk: Kont = { tag: 'guard-body', k };
          evalBody(body, env, gk);
          continue;
        }
      }

      // Check for macro invocation
      if (!BUILTIN_NAMES.has(head.value)) {
        const macroVal = env.lookup(head.value);
        if (macroVal && macroVal.tag === 'syntax') {
          expr = expandMacro(macroVal, expr as SchemeVal & { tag: 'list' }, env);
          continue;
        }
      }
    }

    // General application: evaluate head, then args right-to-left
    k = { tag: 'head', argExprs: elems.slice(1), env, pos: expr.pos, k };
    expr = head;
    continue;
  }
}

// Initialize _callProc now that evaluate is defined
_callProc = function callProc(proc: SchemeVal, args: SchemeVal[], callPos?: Pos): SchemeVal {
  if (proc.tag === 'builtin') return evalBuiltin(proc.name, args, callPos);
  if (proc.tag === 'lambda') {
    const callEnv = bindLambdaArgs(proc, args, proc.env, callPos);
    if (proc.body.length === 0) return { tag: 'void' };
    if (proc.body.length === 1) return evaluate(proc.body[0], callEnv);
    const k: Kont = { tag: 'seq', rest: proc.body.slice(1), env: callEnv, k: { tag: 'halt' } };
    return evaluate(proc.body[0], callEnv, k);
  }
  if (proc.tag === 'case-lambda') {
    for (const clause of proc.clauses) {
      const ok = clause.rest ? args.length >= clause.params.length : args.length === clause.params.length;
      if (ok) {
        const callEnv = bindLambdaArgs(clause, args, proc.env, callPos);
        if (clause.body.length === 0) return { tag: 'void' };
        if (clause.body.length === 1) return evaluate(clause.body[0], callEnv);
        const k: Kont = { tag: 'seq', rest: clause.body.slice(1), env: callEnv, k: { tag: 'halt' } };
        return evaluate(clause.body[0], callEnv, k);
      }
    }
    throw new EvalError(`${posStr(callPos)}case-lambda: no matching clause for ${args.length} arguments`);
  }
  if (proc.tag === 'continuation') {
    throw new EvalError('cannot invoke continuation from builtin callback');
  }
  throw new EvalError(`${posStr(callPos)}not a procedure`);
};

// ── Display ────────────────────────────────────────────────────────

// writeVal: like Scheme's `write` — strings get quotes
function writeVal(val: SchemeVal, seen?: Set<SchemeVal>): string {
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
      if (!seen) seen = new Set();
      if (seen.has(val)) return '(...)';
      seen.add(val);
      let result = '(' + writeVal(val.car, seen);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { result += ' ...'; break; }
        seen.add(cur);
        result += ' ' + writeVal(cur.car, seen);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil' && !(cur.tag === 'pair' && seen.has(cur))) {
        result += ' . ' + writeVal(cur, seen);
      }
      result += ')';
      return result;
    }
    case 'list': return `(${val.value.map(v => writeVal(v, seen)).join(' ')})`;
    case 'lambda': return '#<procedure>';
    case 'builtin': return '#<procedure>';
    case 'case-lambda': return '#<procedure>';
    case 'continuation': return '#<procedure>';
    case 'syntax': return '#<syntax>';
    case 'record': return '#<record>';
    case 'vector': return `#(${val.value.map(v => writeVal(v, seen)).join(' ')})`;
    case 'values': return val.vals.map(v => writeVal(v, seen)).join('\n');
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
  let k: Kont = { tag: 'halt' };
  if (exprs.length > 1) {
    k = { tag: 'seq', rest: exprs.slice(1), env: globalEnv, k: { tag: 'halt' } };
  }
  const result = evaluate(exprs[0], globalEnv, k);
  return writeVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const globalEnv = new Env();
  _outputBuf = [];
  let k: Kont = { tag: 'halt' };
  if (exprs.length > 1) {
    k = { tag: 'seq', rest: exprs.slice(1), env: globalEnv, k: { tag: 'halt' } };
  }
  const result = evaluate(exprs[0], globalEnv, k);
  return { result: writeVal(result), output: _outputBuf.join('') };
}
