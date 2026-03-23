import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; exact?: boolean; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'list'; value: SchemeVal[]; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; rest?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; fn: (args: SchemeVal[]) => SchemeVal; pos?: Pos }
  | { tag: 'continuation'; k: K; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'vector'; value: SchemeVal[]; pos?: Pos }
  | { tag: 'syntax'; literals: string[]; rules: { pattern: SchemeVal; template: SchemeVal }[]; defEnv: Env; pos?: Pos }
  | { tag: 'values'; values: SchemeVal[]; pos?: Pos }
  | { tag: 'record'; type: number; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'syntaxTransformer'; proc: SchemeVal; defEnv: Env; pos?: Pos }
  | { tag: 'syntaxObj'; datum: SchemeVal; pos?: Pos }
  | { tag: 'caseLambda'; clauses: { params: string[]; rest?: string; body: SchemeVal[] }[]; env: Env; pos?: Pos };

// ── Rational / Exact number helpers ───────────────────────────────

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { const t = b; b = a % b; a = t; }
  return a;
}

function makeRational(num: number, den: number): SchemeVal {
  if (den === 0) throw new EvalError('division by zero');
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(num, den);
  num = num / g; den = den / g;
  if (den === 1) return { tag: 'number', value: num, exact: true };
  return { tag: 'rational', num, den };
}

function isNumeric(v: SchemeVal): boolean {
  return v.tag === 'number' || v.tag === 'rational';
}

function isExact(v: SchemeVal): boolean {
  if (v.tag === 'rational') return true;
  if (v.tag === 'number') {
    if (v.exact !== undefined) return v.exact;
    return Number.isInteger(v.value);
  }
  return false;
}

function toFloat(v: SchemeVal): number {
  if (v.tag === 'rational') return v.num / v.den;
  if (v.tag === 'number') return v.value;
  throw new EvalError('expected number');
}

type Frac = { num: number; den: number };

function toFrac(v: SchemeVal): Frac {
  if (v.tag === 'rational') return { num: v.num, den: v.den };
  if (v.tag === 'number') return { num: v.value, den: 1 };
  throw new EvalError('expected number');
}

function fracAdd(a: Frac, b: Frac): Frac {
  return { num: a.num * b.den + b.num * a.den, den: a.den * b.den };
}

function fracSub(a: Frac, b: Frac): Frac {
  return { num: a.num * b.den - b.num * a.den, den: a.den * b.den };
}

function fracMul(a: Frac, b: Frac): Frac {
  return { num: a.num * b.num, den: a.den * b.den };
}

function fracDiv(a: Frac, b: Frac): Frac {
  return { num: a.num * b.den, den: a.den * b.num };
}

// ── Record type identity ──────────────────────────────────────────
let recordTypeCounter = 0;

// ── CPS / Trampoline types ─────────────────────────────────────────

type K = (val: SchemeVal) => TResult;
type TResult = { done: true; value: SchemeVal } | { done: false; thunk: () => TResult };

function bounce(thunk: () => TResult): TResult {
  return { done: false, thunk };
}

function done(value: SchemeVal): TResult {
  return { done: true, value };
}

// Depth-limited direct calls: avoid bounce allocation in common case
let callDepth = 0;
const MAX_CALL_DEPTH = 200;

function callK(k: K, val: SchemeVal): TResult {
  if (callDepth < MAX_CALL_DEPTH) {
    callDepth++;
    try {
      return k(val);
    } finally {
      callDepth--;
    }
  }
  return bounce(() => { callDepth = 0; return k(val); });
}

function runTrampoline(result: TResult): SchemeVal {
  while (!result.done) result = result.thunk();
  return result.value;
}

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

  set(name: string, val: SchemeVal): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, val);
      return;
    }
    if (this.parent) {
      this.parent.set(name, val);
      return;
    }
    throw new EvalError(`set!: unbound variable: ${name}`);
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
    if (ch === '#' && i + 1 < input.length && input[i + 1] === "'") {
      tokens.push({ text: "#'", pos: startPos });
      advance(); advance();
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
  if (tok.text === "#'") {
    const [val, next] = parseTokens(tokens, pos + 1);
    return [{ tag: 'list', value: [{ tag: 'symbol', value: 'syntax', pos: tok.pos }, val], pos: tok.pos }, next];
  }
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
  if (tok.startsWith('#\\')) {
    const charName = tok.slice(2);
    if (charName === 'space') return { tag: 'char', value: ' ' };
    if (charName === 'newline') return { tag: 'char', value: '\n' };
    if (charName === 'tab') return { tag: 'char', value: '\t' };
    if (charName.length === 1) return { tag: 'char', value: charName };
    throw new EvalError(`unknown character name: ${tok}`);
  }
  if (tok.startsWith('"') && tok.endsWith('"')) {
    const inner = tok.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return { tag: 'string', value: inner };
  }
  // Rational literal: n/d (e.g., 1/3, -5/2)
  const ratMatch = tok.match(/^(-?\d+)\/(\d+)$/);
  if (ratMatch) {
    const n = parseInt(ratMatch[1], 10);
    const d = parseInt(ratMatch[2], 10);
    return makeRational(n, d);
  }
  const num = Number(tok);
  if (!isNaN(num) && tok !== '') {
    if (tok.includes('.')) return { tag: 'number', value: num, exact: false };
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

// ── Helpers ────────────────────────────────────────────────────────

function posStr(p?: Pos): string {
  return p ? `${p.line}:${p.col}: ` : '';
}

const NIL: SchemeVal = { tag: 'nil' };
const VOID: SchemeVal = { tag: 'void' };

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
  if (val.tag === 'number') return val.value;
  if (val.tag === 'rational') return val.num / val.den;
  throw new EvalError(`${op}: expected number`);
}

function expectNumeric(val: SchemeVal, op: string): SchemeVal {
  if (val.tag === 'number' || val.tag === 'rational') return val;
  throw new EvalError(`${op}: expected number`);
}

function parseDotParams(paramExprs: SchemeVal[]): { params: string[]; rest?: string } {
  const dotIdx = paramExprs.findIndex(p => p.tag === 'symbol' && p.value === '.');
  if (dotIdx >= 0) {
    const params = paramExprs.slice(0, dotIdx).map(p => (p as { tag: 'symbol'; value: string }).value);
    const rest = (paramExprs[dotIdx + 1] as { tag: 'symbol'; value: string }).value;
    return { params, rest };
  }
  return { params: paramExprs.map(p => (p as { tag: 'symbol'; value: string }).value) };
}

// ── Hygienic Macros (syntax-rules) ─────────────────────────────────

let gensymCounter = 0;
function gensym(base: string): string {
  return `${base}__hyg_${++gensymCounter}`;
}

const SPECIAL_FORMS = new Set([
  'if', 'define', 'lambda', 'and', 'or', 'let', 'let*', 'begin',
  'set!', 'string-set!', 'cond', 'quote', 'define-syntax',
  'syntax-case', 'syntax', 'with-syntax', 'letrec', 'letrec*',
  'case', 'do', 'guard', 'define-record-type', 'case-lambda',
]);

// ── syntax-case dynamic context ────────────────────────────────────
type SyntaxCaseFrame = { bindings: PatternBindings; literals: string[] };
let syntaxCaseStack: SyntaxCaseFrame[] = [];
let macroUseSiteEnvStack: Env[] = [];
let macroDefEnvStack: Env[] = [];

type PatternBindings = Map<string, SchemeVal | SchemeVal[]>;

function collectPatternVars(pattern: SchemeVal, literals: string[], vars: Set<string>): void {
  if (pattern.tag === 'symbol') {
    if (!literals.includes(pattern.value) && pattern.value !== '_' && pattern.value !== '...') {
      vars.add(pattern.value);
    }
  } else if (pattern.tag === 'list') {
    for (const elem of pattern.value) {
      if (elem.tag === 'symbol' && elem.value === '...') continue;
      collectPatternVars(elem, literals, vars);
    }
  }
}

function matchPattern(pattern: SchemeVal, input: SchemeVal, literals: string[], bindings: PatternBindings): boolean {
  if (pattern.tag === 'symbol') {
    if (pattern.value === '_') return true;
    if (pattern.value === '...') return false;
    if (literals.includes(pattern.value)) {
      return input.tag === 'symbol' && input.value === pattern.value;
    }
    bindings.set(pattern.value, input);
    return true;
  }

  if (pattern.tag === 'list' && input.tag === 'list') {
    const pats = pattern.value;
    const inps = input.value;

    const ellipsisIdx = pats.findIndex(p => p.tag === 'symbol' && p.value === '...');

    if (ellipsisIdx >= 0) {
      const beforePats = pats.slice(0, ellipsisIdx - 1);
      const ellipsisPat = pats[ellipsisIdx - 1];
      const afterPats = pats.slice(ellipsisIdx + 1);

      if (inps.length < beforePats.length + afterPats.length) return false;

      for (let i = 0; i < beforePats.length; i++) {
        if (!matchPattern(beforePats[i], inps[i], literals, bindings)) return false;
      }

      const ellipsisVars = new Set<string>();
      collectPatternVars(ellipsisPat, literals, ellipsisVars);
      for (const v of ellipsisVars) bindings.set(v, []);

      const ellipsisCount = inps.length - beforePats.length - afterPats.length;
      for (let i = 0; i < ellipsisCount; i++) {
        const sub = new Map<string, SchemeVal | SchemeVal[]>();
        if (!matchPattern(ellipsisPat, inps[beforePats.length + i], literals, sub)) return false;
        for (const v of ellipsisVars) (bindings.get(v) as SchemeVal[]).push(sub.get(v) as SchemeVal);
      }

      for (let i = 0; i < afterPats.length; i++) {
        if (!matchPattern(afterPats[i], inps[inps.length - afterPats.length + i], literals, bindings)) return false;
      }
      return true;
    }

    if (pats.length !== inps.length) return false;
    for (let i = 0; i < pats.length; i++) {
      if (!matchPattern(pats[i], inps[i], literals, bindings)) return false;
    }
    return true;
  }

  if (isNumeric(pattern) && isNumeric(input)) return toFloat(pattern) === toFloat(input);
  if (pattern.tag === input.tag) {
    if (pattern.tag === 'boolean' && input.tag === 'boolean') return pattern.value === input.value;
  }

  return false;
}

function collectTemplateEllipsisVars(tmpl: SchemeVal, bindings: PatternBindings, vars: Set<string>): void {
  if (tmpl.tag === 'symbol' && bindings.has(tmpl.value) && Array.isArray(bindings.get(tmpl.value))) {
    vars.add(tmpl.value);
  } else if (tmpl.tag === 'list') {
    for (const elem of tmpl.value) collectTemplateEllipsisVars(elem, bindings, vars);
  }
}

function expandTemplate(tmpl: SchemeVal, bindings: PatternBindings, renames: Map<string, string>): SchemeVal {
  if (tmpl.tag === 'symbol') {
    const name = tmpl.value;
    if (bindings.has(name)) {
      const val = bindings.get(name)!;
      if (Array.isArray(val)) throw new EvalError('syntax-rules: ellipsis variable used outside ellipsis context');
      return val;
    }
    if (renames.has(name)) return { tag: 'symbol', value: renames.get(name)! };
    return tmpl;
  }

  if (tmpl.tag === 'list') {
    // Don't expand inside quoted forms
    if (tmpl.value.length >= 1 && tmpl.value[0].tag === 'symbol' && tmpl.value[0].value === 'quote') {
      return tmpl;
    }
    const result: SchemeVal[] = [];
    const elems = tmpl.value;
    for (let i = 0; i < elems.length; i++) {
      const next = i + 1 < elems.length ? elems[i + 1] : null;
      if (next && next.tag === 'symbol' && next.value === '...') {
        const ellVars = new Set<string>();
        collectTemplateEllipsisVars(elems[i], bindings, ellVars);
        if (ellVars.size > 0) {
          const firstVar = ellVars.values().next().value!;
          const count = (bindings.get(firstVar) as SchemeVal[]).length;
          for (let j = 0; j < count; j++) {
            const sub = new Map(bindings);
            for (const v of ellVars) sub.set(v, (bindings.get(v) as SchemeVal[])[j]);
            result.push(expandTemplate(elems[i], sub, renames));
          }
        }
        i++;
        continue;
      }
      result.push(expandTemplate(elems[i], bindings, renames));
    }
    return { tag: 'list', value: result };
  }

  return tmpl;
}

function collectAllSymbols(tmpl: SchemeVal, syms: Set<string>): void {
  if (tmpl.tag === 'symbol') syms.add(tmpl.value);
  else if (tmpl.tag === 'list') {
    // Skip quoted forms — symbols inside quote are data, not references
    if (tmpl.value.length >= 1 && tmpl.value[0].tag === 'symbol' && tmpl.value[0].value === 'quote') return;
    for (const e of tmpl.value) collectAllSymbols(e, syms);
  }
}

function expandMacro(transformer: SchemeVal & { tag: 'syntax' }, inputExpr: SchemeVal, useSiteEnv: Env): SchemeVal | null {
  const inputItems = (inputExpr as { tag: 'list'; value: SchemeVal[] }).value;

  for (const rule of transformer.rules) {
    const patItems = (rule.pattern as { tag: 'list'; value: SchemeVal[] }).value;
    const bindings: PatternBindings = new Map();

    // Match arguments (skip first element = macro name in both pattern and input)
    const patArgs: SchemeVal = { tag: 'list', value: patItems.slice(1) };
    const inpArgs: SchemeVal = { tag: 'list', value: inputItems.slice(1) };

    if (!matchPattern(patArgs, inpArgs, transformer.literals, bindings)) continue;

    // Collect pattern variable names
    const patVars = new Set<string>();
    collectPatternVars(patArgs, transformer.literals, patVars);

    // Collect all symbols in template
    const tmplSyms = new Set<string>();
    collectAllSymbols(rule.template, tmplSyms);

    // Build rename map for hygienic expansion
    const renames = new Map<string, string>();
    for (const sym of tmplSyms) {
      if (patVars.has(sym)) continue;
      if (sym === '...') continue;
      if (SPECIAL_FORMS.has(sym)) continue;
      const renamed = gensym(sym);
      renames.set(sym, renamed);
      // Inject definition-site binding if available
      try {
        const defVal = transformer.defEnv.get(sym);
        useSiteEnv.define(renamed, defVal);
      } catch (_) {
        // Not bound in def env — that's fine, it's a macro-introduced name
      }
    }

    return expandTemplate(rule.template, bindings, renames);
  }

  return null;
}

// ── Equality helpers ────────────────────────────────────────────────

function schemeEq(a: SchemeVal, b: SchemeVal): boolean {
  if (a.tag === 'number' && b.tag === 'number') return a.value === b.value;
  if (a.tag === 'rational' && b.tag === 'rational') return a.num === b.num && a.den === b.den;
  if ((a.tag === 'number' || a.tag === 'rational') && (b.tag === 'number' || b.tag === 'rational')) {
    return toFloat(a) === toFloat(b);
  }
  if (a.tag !== b.tag) return false;
  if (a.tag === 'nil') return true;
  if (a.tag === 'boolean' && b.tag === 'boolean') return a.value === b.value;
  if (a.tag === 'symbol' && b.tag === 'symbol') return a.value === b.value;
  if (a.tag === 'char' && b.tag === 'char') return a.value === b.value;
  if (a.tag === 'string' && b.tag === 'string') return a.value === b.value;
  return a === b;
}

function schemeEqual(a: SchemeVal, b: SchemeVal, seen?: Set<string>): boolean {
  if (a === b) return true;
  if (a.tag === 'pair' && b.tag === 'pair') {
    if (!seen) seen = new Set();
    // Use identity-based cycle detection: if we've seen this exact pair combo, assume equal
    const key = `${idOf(a)},${idOf(b)}`;
    if (seen.has(key)) return true;
    seen.add(key);
    return schemeEqual(a.car, b.car, seen) && schemeEqual(a.cdr, b.cdr, seen);
  }
  if (a.tag === 'vector' && b.tag === 'vector') {
    if (a.value.length !== b.value.length) return false;
    for (let i = 0; i < a.value.length; i++) {
      if (!schemeEqual(a.value[i], b.value[i], seen)) return false;
    }
    return true;
  }
  return schemeEq(a, b);
}

// Unique ID for object identity in cycle detection
let _idCounter = 0;
const _idMap = new WeakMap<object, number>();
function idOf(obj: object): number {
  let id = _idMap.get(obj);
  if (id === undefined) { id = ++_idCounter; _idMap.set(obj, id); }
  return id;
}

// ── Global Environment ─────────────────────────────────────────────

function makeGlobalEnv(): Env {
  const env = new Env();

  function defBuiltin(name: string, fn: (args: SchemeVal[]) => SchemeVal) {
    env.define(name, { tag: 'builtin', name, fn });
  }

  function hasRational(args: SchemeVal[]): boolean {
    for (let i = 0; i < args.length; i++) if (args[i].tag === 'rational') return true;
    return false;
  }

  defBuiltin('+', (args) => {
    if (hasRational(args)) {
      let r: Frac = { num: 0, den: 1 };
      for (const a of args) { expectNumeric(a, '+'); r = fracAdd(r, toFrac(a)); }
      return makeRational(r.num, r.den);
    }
    let sum = 0;
    for (const a of args) sum += expectNumber(a, '+');
    return { tag: 'number', value: sum };
  });

  defBuiltin('-', (args) => {
    if (args.length < 1) throw new EvalError('-: need at least 1 argument');
    if (hasRational(args)) {
      if (args.length === 1) { const f = toFrac(args[0]); return makeRational(-f.num, f.den); }
      let r = toFrac(args[0]);
      for (let i = 1; i < args.length; i++) { expectNumeric(args[i], '-'); r = fracSub(r, toFrac(args[i])); }
      return makeRational(r.num, r.den);
    }
    if (args.length === 1) return { tag: 'number', value: -expectNumber(args[0], '-') };
    let result = expectNumber(args[0], '-');
    for (let i = 1; i < args.length; i++) result -= expectNumber(args[i], '-');
    return { tag: 'number', value: result };
  });

  defBuiltin('*', (args) => {
    if (hasRational(args)) {
      let r: Frac = { num: 1, den: 1 };
      for (const a of args) { expectNumeric(a, '*'); r = fracMul(r, toFrac(a)); }
      return makeRational(r.num, r.den);
    }
    let prod = 1;
    for (const a of args) prod *= expectNumber(a, '*');
    return { tag: 'number', value: prod };
  });

  defBuiltin('/', (args) => {
    if (args.length < 2) throw new EvalError('/: need at least 2 arguments');
    // Exact division for all exact args (integers or rationals)
    if (args.every(a => isExact(a))) {
      let r = toFrac(args[0]);
      for (let i = 1; i < args.length; i++) {
        expectNumeric(args[i], '/');
        const d = toFrac(args[i]);
        if (d.num === 0) throw new EvalError('division by zero');
        r = fracDiv(r, d);
      }
      return makeRational(r.num, r.den);
    }
    let result = toFloat(args[0]);
    for (let i = 1; i < args.length; i++) {
      const d = toFloat(args[i]);
      if (d === 0) throw new EvalError('division by zero');
      result /= d;
    }
    return { tag: 'number', value: result, exact: false };
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
  defBuiltin('set-car!', (args) => {
    if (args[0].tag !== 'pair') throw new EvalError('set-car!: not a pair');
    (args[0] as any).car = args[1];
    return VOID;
  });
  defBuiltin('set-cdr!', (args) => {
    if (args[0].tag !== 'pair') throw new EvalError('set-cdr!: not a pair');
    (args[0] as any).cdr = args[1];
    return VOID;
  });
  defBuiltin('null?', (args) => ({ tag: 'boolean', value: args[0].tag === 'nil' }));
  // cxr combinations
  defBuiltin('caar', (args) => {
    if (args[0].tag !== 'pair' || args[0].car.tag !== 'pair') throw new EvalError('caar: not a pair');
    return args[0].car.car;
  });
  defBuiltin('cadr', (args) => {
    if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw new EvalError('cadr: not a pair');
    return args[0].cdr.car;
  });
  defBuiltin('cdar', (args) => {
    if (args[0].tag !== 'pair' || args[0].car.tag !== 'pair') throw new EvalError('cdar: not a pair');
    return args[0].car.cdr;
  });
  defBuiltin('cddr', (args) => {
    if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw new EvalError('cddr: not a pair');
    return args[0].cdr.cdr;
  });
  defBuiltin('caddr', (args) => {
    if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw new EvalError('caddr: not a pair');
    const cddr = args[0].cdr.cdr;
    if (cddr.tag !== 'pair') throw new EvalError('caddr: not a pair');
    return cddr.car;
  });
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

  defBuiltin('reverse', (args) => {
    const elems = pairsToArray(args[0]);
    let result: SchemeVal = NIL;
    for (const e of elems) {
      result = { tag: 'pair', car: e, cdr: result };
    }
    return result;
  });

  // apply is handled specially in applyCPS — this is just a placeholder so it's a first-class value
  defBuiltin('apply', (_args) => { throw new EvalError('apply: internal error — should be handled by applyCPS'); });

  // call/cc is handled specially in applyCPS
  defBuiltin('call/cc', (_args) => { throw new EvalError('call/cc: internal error'); });
  env.define('call-with-current-continuation', env.get('call/cc'));

  // dynamic-wind is handled specially in applyCPS
  defBuiltin('dynamic-wind', (_args) => { throw new EvalError('dynamic-wind: internal error'); });

  // raise and with-exception-handler are handled specially in applyCPS
  defBuiltin('raise', (_args) => { throw new EvalError('raise: internal error'); });
  defBuiltin('with-exception-handler', (_args) => { throw new EvalError('with-exception-handler: internal error'); });

  // values and call-with-values are handled specially in applyCPS
  defBuiltin('values', (_args) => { throw new EvalError('values: internal error'); });
  defBuiltin('call-with-values', (_args) => { throw new EvalError('call-with-values: internal error'); });

  defBuiltin('number?', (args) => ({ tag: 'boolean', value: isNumeric(args[0]) }));
  defBuiltin('integer?', (args) => {
    const v = args[0];
    if (v.tag === 'rational') return { tag: 'boolean', value: false }; // already simplified, den>1
    if (v.tag === 'number') return { tag: 'boolean', value: Number.isInteger(v.value) };
    return { tag: 'boolean', value: false };
  });
  defBuiltin('rational?', (args) => ({ tag: 'boolean', value: isNumeric(args[0]) && isExact(args[0]) }));
  defBuiltin('exact?', (args) => ({ tag: 'boolean', value: isNumeric(args[0]) && isExact(args[0]) }));
  defBuiltin('inexact?', (args) => ({ tag: 'boolean', value: isNumeric(args[0]) && !isExact(args[0]) }));
  defBuiltin('exact->inexact', (args) => {
    expectNumeric(args[0], 'exact->inexact');
    return { tag: 'number', value: toFloat(args[0]), exact: false };
  });
  defBuiltin('inexact->exact', (args) => {
    expectNumeric(args[0], 'inexact->exact');
    const f = toFloat(args[0]);
    // Convert float to rational via continued fraction / simple approach
    if (Number.isInteger(f)) return { tag: 'number', value: f, exact: true };
    // Use power-of-2 denominator approach for binary floats
    const den = 2 ** 53;
    const num = Math.round(f * den);
    return makeRational(num, den);
  });
  defBuiltin('numerator', (args) => {
    const v = expectNumeric(args[0], 'numerator');
    if (v.tag === 'rational') return { tag: 'number', value: v.num, exact: true };
    if (v.tag === 'number') return { tag: 'number', value: v.value, exact: true };
    throw new EvalError('numerator: expected number');
  });
  defBuiltin('denominator', (args) => {
    const v = expectNumeric(args[0], 'denominator');
    if (v.tag === 'rational') return { tag: 'number', value: v.den, exact: true };
    return { tag: 'number', value: 1, exact: true };
  });
  defBuiltin('string?', (args) => ({ tag: 'boolean', value: args[0].tag === 'string' }));
  defBuiltin('boolean?', (args) => ({ tag: 'boolean', value: args[0].tag === 'boolean' }));
  defBuiltin('pair?', (args) => ({ tag: 'boolean', value: args[0].tag === 'pair' }));
  defBuiltin('symbol?', (args) => ({ tag: 'boolean', value: args[0].tag === 'symbol' }));
  defBuiltin('char?', (args) => ({ tag: 'boolean', value: args[0].tag === 'char' }));

  // Display / Write / Newline
  defBuiltin('display', (args) => {
    outputBuffer += displayVal(args[0]);
    return VOID;
  });
  defBuiltin('write', (args) => {
    outputBuffer += writeVal(args[0]);
    return VOID;
  });
  defBuiltin('newline', (_args) => {
    outputBuffer += '\n';
    return VOID;
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
    if (args[0].tag !== 'string') throw new EvalError('string-length: expected string');
    return { tag: 'number', value: args[0].value.length };
  });
  defBuiltin('substring', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('substring: expected string');
    const s = args[0].value;
    const start = expectNumber(args[1], 'substring');
    const end = expectNumber(args[2], 'substring');
    return { tag: 'string', value: s.slice(start, end) };
  });
  defBuiltin('string->number', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string->number: expected string');
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  });
  defBuiltin('number->string', (args) => {
    if (!isNumeric(args[0])) throw new EvalError('number->string: expected number');
    return { tag: 'string', value: writeVal(args[0]) };
  });
  defBuiltin('symbol->string', (args) => {
    if (args[0].tag !== 'symbol') throw new EvalError('symbol->string: expected symbol');
    return { tag: 'string', value: args[0].value };
  });
  defBuiltin('string->symbol', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string->symbol: expected string');
    return { tag: 'symbol', value: args[0].value };
  });
  defBuiltin('syntax->datum', (args) => {
    const stx = args[0];
    if (stx.tag === 'syntaxObj') return stx.datum;
    return stx;
  });
  defBuiltin('datum->syntax', (args) => {
    // (datum->syntax template-id datum) → syntax object
    const datum = args[1];
    return { tag: 'syntaxObj', datum } as SchemeVal;
  });

  defBuiltin('string-ref', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string-ref: expected string');
    const idx = expectNumber(args[1], 'string-ref');
    return { tag: 'char', value: args[0].value[idx] };
  });
  defBuiltin('string-copy', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string-copy: expected string');
    return { tag: 'string', value: args[0].value };
  });
  defBuiltin('string->list', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string->list: expected string');
    const chars: SchemeVal[] = Array.from(args[0].value).map(c => ({ tag: 'char' as const, value: c }));
    let result: SchemeVal = NIL;
    for (let i = chars.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: chars[i], cdr: result };
    }
    return result;
  });
  defBuiltin('list->string', (args) => {
    let node = args[0];
    let str = '';
    while (node.tag === 'pair') {
      if (node.car.tag !== 'char') throw new EvalError('list->string: expected list of chars');
      str += node.car.value;
      node = node.cdr;
    }
    return { tag: 'string', value: str };
  });
  defBuiltin('char->integer', (args) => {
    if (args[0].tag !== 'char') throw new EvalError('char->integer: expected char');
    return { tag: 'number', value: args[0].value.codePointAt(0)! };
  });
  defBuiltin('integer->char', (args) => {
    const n = expectNumber(args[0], 'integer->char');
    return { tag: 'char', value: String.fromCodePoint(n) };
  });

  defBuiltin('eq?', (args) => ({ tag: 'boolean', value: schemeEq(args[0], args[1]) }));
  defBuiltin('eqv?', (args) => ({ tag: 'boolean', value: schemeEq(args[0], args[1]) }));
  defBuiltin('equal?', (args) => ({ tag: 'boolean', value: schemeEqual(args[0], args[1]) }));

  // Vector operations
  defBuiltin('vector', (args) => ({ tag: 'vector', value: [...args] }));
  defBuiltin('make-vector', (args) => {
    const len = expectNumber(args[0], 'make-vector');
    const fill = args.length > 1 ? args[1] : { tag: 'number' as const, value: 0 };
    return { tag: 'vector', value: Array(len).fill(fill) };
  });
  defBuiltin('vector-ref', (args) => {
    if (args[0].tag !== 'vector') throw new EvalError('vector-ref: expected vector');
    const idx = expectNumber(args[1], 'vector-ref');
    return args[0].value[idx];
  });
  defBuiltin('vector-set!', (args) => {
    if (args[0].tag !== 'vector') throw new EvalError('vector-set!: expected vector');
    const idx = expectNumber(args[1], 'vector-set!');
    args[0].value[idx] = args[2];
    return VOID;
  });
  defBuiltin('vector-length', (args) => {
    if (args[0].tag !== 'vector') throw new EvalError('vector-length: expected vector');
    return { tag: 'number', value: args[0].value.length };
  });
  defBuiltin('vector?', (args) => ({ tag: 'boolean', value: args[0].tag === 'vector' }));
  defBuiltin('vector->list', (args) => {
    if (args[0].tag !== 'vector') throw new EvalError('vector->list: expected vector');
    return listToPairs(args[0].value);
  });
  defBuiltin('list->vector', (args) => {
    return { tag: 'vector', value: pairsToArray(args[0]) };
  });

  // map (supports multiple lists)
  defBuiltin('map', (args) => {
    const proc = args[0];
    const lists = args.slice(1).map(pairsToArray);
    const len = lists[0].length;
    const result: SchemeVal[] = [];
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map(l => l[i]);
      // Synchronous apply for builtins/lambdas
      const r = runTrampoline(applyCPS(proc, callArgs, (v) => done(v)));
      result.push(r);
    }
    return listToPairs(result);
  });

  defBuiltin('for-each', (args) => {
    const proc = args[0];
    const lists = args.slice(1).map(pairsToArray);
    const len = lists[0].length;
    for (let i = 0; i < len; i++) {
      const callArgs = lists.map(l => l[i]);
      runTrampoline(applyCPS(proc, callArgs, (v) => done(v)));
    }
    return VOID;
  });

  defBuiltin('error', (args) => {
    const msg = args.map(a => a.tag === 'string' ? a.value : writeVal(a)).join(' ');
    throw new EvalError(msg);
  });

  defBuiltin('procedure?', (args) => ({
    tag: 'boolean',
    value: args[0].tag === 'lambda' || args[0].tag === 'builtin' || args[0].tag === 'continuation' || args[0].tag === 'caseLambda',
  }));

  // L13: Numeric utilities
  defBuiltin('abs', (args) => ({ tag: 'number', value: Math.abs(expectNumber(args[0], 'abs')) }));
  defBuiltin('modulo', (args) => {
    const a = expectNumber(args[0], 'modulo');
    const b = expectNumber(args[1], 'modulo');
    return { tag: 'number', value: ((a % b) + b) % b };
  });
  defBuiltin('remainder', (args) => {
    const a = expectNumber(args[0], 'remainder');
    const b = expectNumber(args[1], 'remainder');
    return { tag: 'number', value: a % b };
  });
  defBuiltin('quotient', (args) => {
    const a = expectNumber(args[0], 'quotient');
    const b = expectNumber(args[1], 'quotient');
    return { tag: 'number', value: Math.trunc(a / b) };
  });
  defBuiltin('min', (args) => {
    let m = expectNumber(args[0], 'min');
    for (let i = 1; i < args.length; i++) m = Math.min(m, expectNumber(args[i], 'min'));
    return { tag: 'number', value: m };
  });
  defBuiltin('max', (args) => {
    let m = expectNumber(args[0], 'max');
    for (let i = 1; i < args.length; i++) m = Math.max(m, expectNumber(args[i], 'max'));
    return { tag: 'number', value: m };
  });
  defBuiltin('expt', (args) => {
    const base = expectNumber(args[0], 'expt');
    const exp = expectNumber(args[1], 'expt');
    return { tag: 'number', value: Math.pow(base, exp) };
  });
  defBuiltin('zero?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'zero?') === 0 }));
  defBuiltin('positive?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'positive?') > 0 }));
  defBuiltin('negative?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'negative?') < 0 }));
  defBuiltin('odd?', (args) => ({ tag: 'boolean', value: Math.abs(expectNumber(args[0], 'odd?')) % 2 === 1 }));
  defBuiltin('even?', (args) => ({ tag: 'boolean', value: expectNumber(args[0], 'even?') % 2 === 0 }));

  // L13: List utilities
  defBuiltin('list-ref', (args) => {
    let cur = args[0];
    let idx = expectNumber(args[1], 'list-ref');
    while (idx > 0 && cur.tag === 'pair') { cur = cur.cdr; idx--; }
    if (cur.tag !== 'pair') throw new EvalError('list-ref: index out of range');
    return cur.car;
  });
  defBuiltin('list-tail', (args) => {
    let cur = args[0];
    let idx = expectNumber(args[1], 'list-tail');
    while (idx > 0 && cur.tag === 'pair') { cur = cur.cdr; idx--; }
    if (idx > 0) throw new EvalError('list-tail: index out of range');
    return cur;
  });
  defBuiltin('list?', (args) => {
    // Tortoise-and-hare cycle detection
    let slow = args[0];
    let fast = args[0];
    while (fast.tag === 'pair') {
      slow = (slow as any).cdr;
      fast = fast.cdr;
      if (fast.tag !== 'pair') break;
      fast = fast.cdr;
      if (slow === fast) return { tag: 'boolean', value: false }; // cycle
    }
    return { tag: 'boolean', value: fast.tag === 'nil' };
  });
  defBuiltin('assoc', (args) => {
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (cur.car.tag === 'pair' && schemeEqual(cur.car.car, key)) return cur.car;
      cur = cur.cdr;
    }
    return { tag: 'boolean', value: false };
  });

  // L13: Character utilities
  defBuiltin('char-alphabetic?', (args) => {
    if (args[0].tag !== 'char') throw new EvalError('char-alphabetic?: expected char');
    return { tag: 'boolean', value: /^[a-zA-Z]$/.test(args[0].value) };
  });
  defBuiltin('char-numeric?', (args) => {
    if (args[0].tag !== 'char') throw new EvalError('char-numeric?: expected char');
    return { tag: 'boolean', value: /^[0-9]$/.test(args[0].value) };
  });
  defBuiltin('char-upcase', (args) => {
    if (args[0].tag !== 'char') throw new EvalError('char-upcase: expected char');
    return { tag: 'char', value: args[0].value.toUpperCase() };
  });
  defBuiltin('char-downcase', (args) => {
    if (args[0].tag !== 'char') throw new EvalError('char-downcase: expected char');
    return { tag: 'char', value: args[0].value.toLowerCase() };
  });
  defBuiltin('char=?', (args) => {
    if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char=?: expected char');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  });
  defBuiltin('char<?', (args) => {
    if (args[0].tag !== 'char' || args[1].tag !== 'char') throw new EvalError('char<?: expected char');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  });

  // L13: String comparison utilities
  defBuiltin('string=?', (args) => {
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string=?: expected string');
    return { tag: 'boolean', value: args[0].value === args[1].value };
  });
  defBuiltin('string<?', (args) => {
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string<?: expected string');
    return { tag: 'boolean', value: args[0].value < args[1].value };
  });
  defBuiltin('string-ci=?', (args) => {
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string-ci=?: expected string');
    return { tag: 'boolean', value: args[0].value.toLowerCase() === args[1].value.toLowerCase() };
  });
  defBuiltin('string-upcase', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string-upcase: expected string');
    return { tag: 'string', value: args[0].value.toUpperCase() };
  });
  defBuiltin('string-downcase', (args) => {
    if (args[0].tag !== 'string') throw new EvalError('string-downcase: expected string');
    return { tag: 'string', value: args[0].value.toLowerCase() };
  });

  // L21: Additional builtins for real-world fixtures
  defBuiltin('string>?', (args) => {
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string>?: expected string');
    return { tag: 'boolean', value: args[0].value > args[1].value };
  });
  defBuiltin('string<=?', (args) => {
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string<=?: expected string');
    return { tag: 'boolean', value: args[0].value <= args[1].value };
  });
  defBuiltin('string>=?', (args) => {
    if (args[0].tag !== 'string' || args[1].tag !== 'string') throw new EvalError('string>=?: expected string');
    return { tag: 'boolean', value: args[0].value >= args[1].value };
  });
  defBuiltin('gcd', (args) => {
    let result = 0;
    for (const a of args) result = gcd(result, expectNumber(a, 'gcd'));
    return { tag: 'number', value: result };
  });
  defBuiltin('lcm', (args) => {
    let result = 1;
    for (const a of args) {
      const n = expectNumber(a, 'lcm');
      if (n === 0) return { tag: 'number', value: 0 };
      result = Math.abs(result * n) / gcd(result, n);
    }
    return { tag: 'number', value: result };
  });
  defBuiltin('truncate', (args) => ({ tag: 'number', value: Math.trunc(expectNumber(args[0], 'truncate')) }));
  defBuiltin('round', (args) => ({ tag: 'number', value: Math.round(expectNumber(args[0], 'round')) }));
  defBuiltin('make-string', (args) => {
    const n = expectNumber(args[0], 'make-string');
    const ch = args.length > 1 && args[1].tag === 'char' ? args[1].value : '\0';
    return { tag: 'string', value: ch.repeat(n) };
  });
  defBuiltin('string', (args) => {
    return { tag: 'string', value: args.map(a => { if (a.tag !== 'char') throw new EvalError('string: expected char'); return a.value; }).join('') };
  });
  defBuiltin('member', (args) => {
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (schemeEqual(args[0], cur.car)) return cur;
      cur = cur.cdr;
    }
    return { tag: 'boolean', value: false };
  });
  defBuiltin('assv', (args) => {
    const key = args[0];
    let cur = args[1];
    while (cur.tag === 'pair') {
      if (cur.car.tag === 'pair' && schemeEq(cur.car.car, key)) return cur.car;
      cur = cur.cdr;
    }
    return { tag: 'boolean', value: false };
  });

  return env;
}

// ── CPS Evaluator ──────────────────────────────────────────────────

// Evaluate arguments right-to-left. R7RS does not specify evaluation order;
// right-to-left ensures that continuations captured by call/cc in later
// argument positions will re-evaluate earlier arguments when re-invoked.
function evaluateArgsCPS(exprs: SchemeVal[], startIdx: number, env: Env, _unused: SchemeVal[], k: (vals: SchemeVal[]) => TResult): TResult {
  if (startIdx >= exprs.length) return k([]);
  // First evaluate the rest (right), then evaluate this arg (left)
  return evaluateArgsCPS(exprs, startIdx + 1, env, [], (restVals) => {
    return evaluateCPS(exprs[startIdx], env, (val) => {
      return k([val, ...restVals]);
    });
  });
}

function evaluateSeqCPS(exprs: SchemeVal[], idx: number, env: Env, k: K): TResult {
  if (idx >= exprs.length) return callK(k, VOID);
  if (idx === exprs.length - 1) return evaluateCPS(exprs[idx], env, k); // tail position
  return evaluateCPS(exprs[idx], env, (_) => evaluateSeqCPS(exprs, idx + 1, env, k));
}

function evaluateAndCPS(items: SchemeVal[], idx: number, env: Env, k: K): TResult {
  if (idx === items.length - 1) return evaluateCPS(items[idx], env, k);
  return evaluateCPS(items[idx], env, (val) => {
    if (!isTruthy(val)) return callK(k, val);
    return evaluateAndCPS(items, idx + 1, env, k);
  });
}

function evaluateOrCPS(items: SchemeVal[], idx: number, env: Env, k: K): TResult {
  if (idx === items.length - 1) return evaluateCPS(items[idx], env, k);
  return evaluateCPS(items[idx], env, (val) => {
    if (isTruthy(val)) return callK(k, val);
    return evaluateOrCPS(items, idx + 1, env, k);
  });
}

function evaluateCondCPS(clauses: SchemeVal[], idx: number, env: Env, k: K): TResult {
  if (idx >= clauses.length) return callK(k, VOID);
  const clause = (clauses[idx] as { tag: 'list'; value: SchemeVal[] }).value;
  if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
    return evaluateSeqCPS(clause, 1, env, k);
  }
  return evaluateCPS(clause[0], env, (test) => {
    if (isTruthy(test)) {
      if (clause.length === 1) return callK(k, test);
      return evaluateSeqCPS(clause, 1, env, k);
    }
    return evaluateCondCPS(clauses, idx + 1, env, k);
  });
}

function evaluateLetBindingsCPS(bindings: SchemeVal[], idx: number, outerEnv: Env, letEnv: Env, body: SchemeVal[], k: K): TResult {
  if (idx >= bindings.length) return evaluateSeqCPS(body, 0, letEnv, k);
  const bv = (bindings[idx] as { tag: 'list'; value: SchemeVal[] }).value;
  const name = (bv[0] as { tag: 'symbol'; value: string }).value;
  return evaluateCPS(bv[1], outerEnv, (val) => {
    letEnv.define(name, val);
    return evaluateLetBindingsCPS(bindings, idx + 1, outerEnv, letEnv, body, k);
  });
}

function evaluateGuardClauses(clauses: SchemeVal[], idx: number, env: Env, k: K, exnVal: SchemeVal): TResult {
  if (idx >= clauses.length) {
    // No clause matched, re-raise
    if (exHandlerStack.length === 0) {
      throw new EvalError(`unhandled exception: ${writeVal(exnVal)}`);
    }
    const handler = exHandlerStack.pop()!;
    return handler(exnVal);
  }
  const clause = (clauses[idx] as { tag: 'list'; value: SchemeVal[] }).value;
  if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
    return evaluateSeqCPS(clause, 1, env, k);
  }
  return evaluateCPS(clause[0], env, (testVal) => {
    if (isTruthy(testVal)) {
      if (clause.length > 1) {
        return evaluateSeqCPS(clause, 1, env, k);
      }
      return callK(k, testVal);
    }
    return evaluateGuardClauses(clauses, idx + 1, env, k, exnVal);
  });
}

function doWindShift(from: WindEntry[], to: WindEntry[], then: () => TResult): TResult {
  // Find common prefix length (by identity)
  let common = 0;
  const minLen = Math.min(from.length, to.length);
  for (let i = 0; i < minLen; i++) {
    if (from[i] === to[i]) common++;
    else break;
  }

  // Unwind: call out-thunks from innermost to common
  function unwind(idx: number): TResult {
    if (idx < common) return rewind(common);
    const entry = from[idx];
    windStack.pop();
    return applyCPS(entry.outThunk, [], (_) => unwind(idx - 1));
  }

  // Rewind: call in-thunks from common to target
  function rewind(idx: number): TResult {
    if (idx >= to.length) return then();
    const entry = to[idx];
    return applyCPS(entry.inThunk, [], (_) => {
      windStack.push(entry);
      return rewind(idx + 1);
    });
  }

  if (from.length > common) return unwind(from.length - 1);
  return rewind(common);
}

function applyCPS(proc: SchemeVal, args: SchemeVal[], k: K, pos?: Pos): TResult {
  if (proc.tag === 'builtin') {
    // call/cc: capture current continuation and wind stack
    if (proc.name === 'call/cc') {
      const savedWind = [...windStack];
      const savedK = k;
      const contK: K = (val) => {
        const currentWind = [...windStack];
        return doWindShift(currentWind, savedWind, () => callK(savedK, val));
      };
      const contVal: SchemeVal = { tag: 'continuation', k: contK };
      return applyCPS(args[0], [contVal], k);
    }

    // dynamic-wind: in-thunk, body-thunk, out-thunk
    if (proc.name === 'dynamic-wind') {
      const [inThunk, bodyThunk, outThunk] = args;
      const entry: WindEntry = { inThunk, outThunk };
      return applyCPS(inThunk, [], (_) => {
        windStack.push(entry);
        return applyCPS(bodyThunk, [], (bodyResult) => {
          windStack.pop();
          return applyCPS(outThunk, [], (_) => {
            return callK(k, bodyResult);
          });
        });
      });
    }
    // raise: signal an exception
    if (proc.name === 'raise') {
      const val = args[0];
      if (exHandlerStack.length === 0) {
        throw new EvalError(`unhandled exception: ${writeVal(val)}`);
      }
      const handler = exHandlerStack.pop()!;
      return handler(val);
    }

    // with-exception-handler: install low-level handler
    if (proc.name === 'with-exception-handler') {
      const [handlerProc, thunkProc] = args;
      const handler: ExHandler = (val) => {
        return applyCPS(handlerProc, [val], k);
      };
      exHandlerStack.push(handler);
      return applyCPS(thunkProc, [], (result) => {
        exHandlerStack.pop();
        return callK(k, result);
      });
    }

    // values: return multiple values (single value is transparent)
    if (proc.name === 'values') {
      if (args.length === 1) {
        return callK(k, args[0]);
      }
      return callK(k, { tag: 'values', values: args });
    }

    // call-with-values: producer -> consumer
    if (proc.name === 'call-with-values') {
      const [producer, consumer] = args;
      return applyCPS(producer, [], (produced) => {
        if (produced.tag === 'values') {
          return applyCPS(consumer, produced.values, k);
        }
        return applyCPS(consumer, [produced], k);
      });
    }

    // apply: restructure args and delegate
    if (proc.name === 'apply') {
      const actualProc = args[0];
      const lastArg = args[args.length - 1];
      const prefixArgs = args.slice(1, args.length - 1);
      const tailArgs = pairsToArray(lastArg);
      return applyCPS(actualProc, [...prefixArgs, ...tailArgs], k);
    }
    try {
      const result = proc.fn(args);
      return callK(k, result);
    } catch (e) {
      if (e instanceof EvalError && pos && !e.message.match(/^\d+:/)) {
        throw new EvalError(`${posStr(pos)}${e.message}`);
      }
      throw e;
    }
  }

  if (proc.tag === 'lambda') {
    const callEnv = new Env(proc.env);
    for (let i = 0; i < proc.params.length; i++) {
      callEnv.define(proc.params[i], args[i]);
    }
    if (proc.rest) {
      callEnv.define(proc.rest, listToPairs(args.slice(proc.params.length)));
    }
    return evaluateSeqCPS(proc.body, 0, callEnv, k);
  }

  if (proc.tag === 'caseLambda') {
    for (const clause of proc.clauses) {
      if (clause.rest !== undefined) {
        if (args.length >= clause.params.length) {
          const callEnv = new Env(proc.env);
          for (let i = 0; i < clause.params.length; i++) {
            callEnv.define(clause.params[i], args[i]);
          }
          callEnv.define(clause.rest, listToPairs(args.slice(clause.params.length)));
          return evaluateSeqCPS(clause.body, 0, callEnv, k);
        }
      } else {
        if (args.length === clause.params.length) {
          const callEnv = new Env(proc.env);
          for (let i = 0; i < clause.params.length; i++) {
            callEnv.define(clause.params[i], args[i]);
          }
          return evaluateSeqCPS(clause.body, 0, callEnv, k);
        }
      }
    }
    throw new EvalError(`${posStr(pos)}case-lambda: no matching clause for ${args.length} arguments`);
  }

  if (proc.tag === 'continuation') {
    let val: SchemeVal;
    if (args.length === 1) {
      val = args[0];
    } else if (args.length === 0) {
      val = VOID;
    } else {
      val = { tag: 'values', values: args };
    }
    return callK(proc.k, val);
  }

  throw new EvalError(`${posStr(pos)}not a procedure`);
}

function evaluateCPS(expr: SchemeVal, env: Env, k: K): TResult {
  if (expr.tag === 'symbol') {
    try {
      const val = env.get(expr.value);
      return callK(k, val);
    } catch (e) {
      if (e instanceof EvalError && expr.pos) {
        throw new EvalError(`${posStr(expr.pos)}${e.message}`);
      }
      throw e;
    }
  }

  if (expr.tag !== 'list') {
    return callK(k, expr); // self-evaluating
  }

  const items = expr.value;
  if (items.length === 0) throw new EvalError(`${posStr(expr.pos)}empty application`);

  const head = items[0];
  if (head.tag === 'symbol') {
    const op = head.value;

    if (op === 'quote') {
      const quoted = astToPairs(items[1]);
      return callK(k, quoted);
    }

    if (op === 'if') {
      if (items.length < 3) throw new EvalError(`${posStr(expr.pos)}if: bad syntax`);
      return evaluateCPS(items[1], env, (cond) => {
        if (isTruthy(cond)) {
          return evaluateCPS(items[2], env, k);
        } else if (items.length > 3) {
          return evaluateCPS(items[3], env, k);
        }
        return callK(k, VOID);
      });
    }

    if (op === 'define') {
      if (items.length < 3) throw new EvalError(`${posStr(expr.pos)}define: bad syntax`);
      if (items[1].tag === 'list') {
        const nameAndParams = items[1].value;
        const name = (nameAndParams[0] as { tag: 'symbol'; value: string }).value;
        const { params, rest } = parseDotParams(nameAndParams.slice(1));
        const body = items.slice(2);
        env.define(name, { tag: 'lambda', params, rest, body, env });
        return callK(k, VOID);
      }
      const name = (items[1] as { tag: 'symbol'; value: string }).value;
      return evaluateCPS(items[2], env, (val) => {
        env.define(name, val);
        return callK(k, VOID);
      });
    }

    if (op === 'lambda') {
      const paramList = items[1];
      if (paramList.tag === 'symbol') {
        const lam: SchemeVal = { tag: 'lambda', params: [], rest: paramList.value, body: items.slice(2), env };
        return callK(k, lam);
      }
      const { params, rest } = parseDotParams((paramList as { tag: 'list'; value: SchemeVal[] }).value);
      const lam: SchemeVal = { tag: 'lambda', params, rest, body: items.slice(2), env };
      return callK(k, lam);
    }

    if (op === 'case-lambda') {
      const clauses = items.slice(1).map(clause => {
        if (clause.tag !== 'list') throw new EvalError('case-lambda: bad clause');
        const paramList = clause.value[0];
        const body = clause.value.slice(1);
        if (paramList.tag === 'symbol') {
          return { params: [] as string[], rest: paramList.value, body };
        }
        const { params, rest } = parseDotParams((paramList as { tag: 'list'; value: SchemeVal[] }).value);
        return { params, rest, body };
      });
      return callK(k, { tag: 'caseLambda', clauses, env });
    }

    if (op === 'and') {
      if (items.length === 1) return callK(k, { tag: 'boolean', value: true });
      return evaluateAndCPS(items, 1, env, k);
    }

    if (op === 'or') {
      if (items.length === 1) return callK(k, { tag: 'boolean', value: false });
      return evaluateOrCPS(items, 1, env, k);
    }

    if (op === 'let') {
      if (items[1].tag === 'symbol') {
        // Named let: (let name ((var val) ...) body...)
        const name = items[1].value;
        const bindings = (items[2] as { tag: 'list'; value: SchemeVal[] }).value;
        const body = items.slice(3);
        const params: string[] = [];
        const initExprs: SchemeVal[] = [];
        for (const b of bindings) {
          const bv = (b as { tag: 'list'; value: SchemeVal[] }).value;
          params.push((bv[0] as { tag: 'symbol'; value: string }).value);
          initExprs.push(bv[1]);
        }
        const letEnv = new Env(env);
        const lambda: SchemeVal = { tag: 'lambda', params, body, env: letEnv };
        letEnv.define(name, lambda);
        return evaluateArgsCPS(initExprs, 0, env, [], (argVals) => {
          const callEnv = new Env(letEnv);
          for (let i = 0; i < params.length; i++) {
            callEnv.define(params[i], argVals[i]);
          }
          return evaluateSeqCPS(body, 0, callEnv, k);
        });
      }
      // Regular let: (let ((var val) ...) body...)
      const bindings = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const body = items.slice(2);
      const letEnv = new Env(env);
      return evaluateLetBindingsCPS(bindings, 0, env, letEnv, body, k);
    }

    if (op === 'let*') {
      const bindings = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const body = items.slice(2);
      // let* evaluates each binding sequentially in an environment that includes previous bindings
      const letStarEnv = new Env(env);
      function evalLetStarBindings(idx: number): TResult {
        if (idx >= bindings.length) {
          return evaluateSeqCPS(body, 0, letStarEnv, k);
        }
        const b = (bindings[idx] as { tag: 'list'; value: SchemeVal[] }).value;
        const name = (b[0] as { tag: 'symbol'; value: string }).value;
        return evaluateCPS(b[1], letStarEnv, (val) => {
          letStarEnv.define(name, val);
          return bounce(() => evalLetStarBindings(idx + 1));
        });
      }
      return evalLetStarBindings(0);
    }

    if (op === 'begin') {
      if (items.length <= 1) return callK(k, VOID);
      return evaluateSeqCPS(items, 1, env, k);
    }

    if (op === 'set!') {
      const varExpr = items[1];
      if (varExpr.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}set!: bad syntax`);
      return evaluateCPS(items[2], env, (val) => {
        env.set(varExpr.value, val);
        return callK(k, VOID);
      });
    }

    if (op === 'string-set!') {
      const varExpr = items[1];
      if (varExpr.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}string-set!: first argument must be a variable`);
      const str = env.get(varExpr.value);
      if (str.tag !== 'string') throw new EvalError(`${posStr(expr.pos)}string-set!: expected string`);
      return evaluateCPS(items[2], env, (idxVal) => {
        const idx = expectNumber(idxVal, 'string-set!');
        return evaluateCPS(items[3], env, (chVal) => {
          if (chVal.tag !== 'char') throw new EvalError(`${posStr(expr.pos)}string-set!: expected char`);
          const newStr = str.value.substring(0, idx) + chVal.value + str.value.substring(idx + 1);
          env.set(varExpr.value, { tag: 'string', value: newStr });
          return callK(k, VOID);
        });
      });
    }

    if (op === 'cond') {
      return evaluateCondCPS(items, 1, env, k);
    }

    if (op === 'define-syntax') {
      const name = (items[1] as { tag: 'symbol'; value: string }).value;
      const bodyExpr = items[2];
      // Check if body is (syntax-rules ...)
      if (bodyExpr.tag === 'list' && bodyExpr.value.length > 0 &&
          bodyExpr.value[0].tag === 'symbol' && bodyExpr.value[0].value === 'syntax-rules') {
        const srItems = bodyExpr.value;
        const literals = (srItems[1] as { tag: 'list'; value: SchemeVal[] }).value.map(
          l => (l as { tag: 'symbol'; value: string }).value
        );
        const rules: { pattern: SchemeVal; template: SchemeVal }[] = [];
        for (let i = 2; i < srItems.length; i++) {
          const r = (srItems[i] as { tag: 'list'; value: SchemeVal[] }).value;
          rules.push({ pattern: r[0], template: r[1] });
        }
        env.define(name, { tag: 'syntax', literals, rules, defEnv: env });
        return callK(k, VOID);
      }
      // Otherwise evaluate as expression (lambda-based transformer)
      return evaluateCPS(bodyExpr, env, (proc) => {
        env.define(name, { tag: 'syntaxTransformer', proc, defEnv: env });
        return callK(k, VOID);
      });
    }

    if (op === 'syntax-case') {
      // (syntax-case expr (literals) clause ...)
      // clause = (pattern body) or (pattern fender body)
      const scrutineeExpr = items[1];
      const literalsList = (items[2] as { tag: 'list'; value: SchemeVal[] }).value.map(
        l => (l as { tag: 'symbol'; value: string }).value
      );
      const clauses = items.slice(3);
      return evaluateCPS(scrutineeExpr, env, (scrutinee) => {
        const datum = scrutinee.tag === 'syntaxObj' ? scrutinee.datum : scrutinee;
        function tryClauses(ci: number): TResult {
          if (ci >= clauses.length) throw new EvalError('syntax-case: no matching pattern');
          const clauseItems = (clauses[ci] as { tag: 'list'; value: SchemeVal[] }).value;
          const pattern = clauseItems[0];
          const hasFender = clauseItems.length === 3;
          const body = hasFender ? clauseItems[2] : clauseItems[1];
          const bindings: PatternBindings = new Map();
          if (!matchPattern(pattern, datum, literalsList, bindings)) {
            return bounce(() => tryClauses(ci + 1));
          }
          const frame: SyntaxCaseFrame = { bindings, literals: literalsList };
          syntaxCaseStack.push(frame);
          if (hasFender) {
            const fender = clauseItems[1];
            // Bind pattern vars as syntaxObj in a child env for fender
            const fenderEnv = new Env(env);
            for (const [pn, pv] of bindings) {
              if (Array.isArray(pv)) {
                fenderEnv.define(pn, { tag: 'list', value: pv.map(d => ({ tag: 'syntaxObj', datum: d } as SchemeVal)) });
              } else {
                fenderEnv.define(pn, { tag: 'syntaxObj', datum: pv });
              }
            }
            return evaluateCPS(fender, fenderEnv, (fResult) => {
              if (!isTruthy(fResult)) {
                syntaxCaseStack.pop();
                return bounce(() => tryClauses(ci + 1));
              }
              return evaluateCPS(body, env, (result) => {
                syntaxCaseStack.pop();
                return callK(k, result);
              });
            });
          }
          return evaluateCPS(body, env, (result) => {
            syntaxCaseStack.pop();
            return callK(k, result);
          });
        }
        return tryClauses(0);
      });
    }

    if (op === 'syntax') {
      // (syntax template) — aka #'template
      // Expand template using current syntax-case bindings
      const template = items[1];
      // Merge all bindings from syntax-case stack (innermost first)
      const mergedBindings: PatternBindings = new Map();
      for (let si = syntaxCaseStack.length - 1; si >= 0; si--) {
        for (const [bk, bv] of syntaxCaseStack[si].bindings) {
          if (!mergedBindings.has(bk)) mergedBindings.set(bk, bv);
        }
      }
      // Collect pattern variable names
      const patVars = new Set(mergedBindings.keys());
      // Collect all symbols in template
      const tmplSyms = new Set<string>();
      collectAllSymbols(template, tmplSyms);
      // Build rename map for hygiene
      const renames = new Map<string, string>();
      const defEnv = macroDefEnvStack.length > 0 ? macroDefEnvStack[macroDefEnvStack.length - 1] : env;
      const useSiteEnv = macroUseSiteEnvStack.length > 0 ? macroUseSiteEnvStack[macroUseSiteEnvStack.length - 1] : env;
      for (const sym of tmplSyms) {
        if (patVars.has(sym)) continue;
        if (sym === '...') continue;
        if (SPECIAL_FORMS.has(sym)) continue;
        const renamed = gensym(sym);
        renames.set(sym, renamed);
        try {
          const defVal = defEnv.get(sym);
          useSiteEnv.define(renamed, defVal);
        } catch (_) { /* not bound in def env */ }
      }
      const expanded = expandTemplate(template, mergedBindings, renames);
      return callK(k, { tag: 'syntaxObj', datum: expanded });
    }

    if (op === 'with-syntax') {
      // (with-syntax ((pattern expr) ...) body ...)
      const bindingForms = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const body = items.slice(2);
      const savedLen = syntaxCaseStack.length;
      function evalWithSyntaxBindings(bi: number): TResult {
        if (bi >= bindingForms.length) {
          return evaluateSeqCPS(body, 0, env, (result) => {
            // Pop all frames pushed by with-syntax
            syntaxCaseStack.length = savedLen;
            return callK(k, result);
          });
        }
        const bf = (bindingForms[bi] as { tag: 'list'; value: SchemeVal[] }).value;
        const pat = bf[0];
        const valExpr = bf[1];
        return evaluateCPS(valExpr, env, (val) => {
          const datum = val.tag === 'syntaxObj' ? val.datum : val;
          const bindings: PatternBindings = new Map();
          matchPattern(pat, datum, [], bindings);
          syntaxCaseStack.push({ bindings, literals: [] });
          return evalWithSyntaxBindings(bi + 1);
        });
      }
      return evalWithSyntaxBindings(0);
    }

    if (op === 'letrec') {
      const bindings = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const body = items.slice(2);
      const letEnv = new Env(env);
      // Define all variables first (as undefined placeholders)
      const names: string[] = [];
      const initExprs: SchemeVal[] = [];
      for (const b of bindings) {
        const bv = (b as { tag: 'list'; value: SchemeVal[] }).value;
        const name = (bv[0] as { tag: 'symbol'; value: string }).value;
        names.push(name);
        initExprs.push(bv[1]);
        letEnv.define(name, VOID);
      }
      // Evaluate all init expressions in letEnv, then set
      function evalLetrecBindings(idx: number): TResult {
        if (idx >= names.length) return evaluateSeqCPS(body, 0, letEnv, k);
        return evaluateCPS(initExprs[idx], letEnv, (val) => {
          letEnv.define(names[idx], val);
          return evalLetrecBindings(idx + 1);
        });
      }
      return evalLetrecBindings(0);
    }

    if (op === 'letrec*') {
      const bindings = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const body = items.slice(2);
      const letEnv = new Env(env);
      // Define all variables first
      for (const b of bindings) {
        const bv = (b as { tag: 'list'; value: SchemeVal[] }).value;
        const name = (bv[0] as { tag: 'symbol'; value: string }).value;
        letEnv.define(name, VOID);
      }
      // Evaluate sequentially, each becoming visible
      function evalLetrecStarBindings(idx: number): TResult {
        if (idx >= bindings.length) return evaluateSeqCPS(body, 0, letEnv, k);
        const bv = (bindings[idx] as { tag: 'list'; value: SchemeVal[] }).value;
        const name = (bv[0] as { tag: 'symbol'; value: string }).value;
        return evaluateCPS(bv[1], letEnv, (val) => {
          letEnv.define(name, val);
          return evalLetrecStarBindings(idx + 1);
        });
      }
      return evalLetrecStarBindings(0);
    }

    if (op === 'case') {
      return evaluateCPS(items[1], env, (keyVal) => {
        function evalCaseClauses(idx: number): TResult {
          if (idx >= items.length) return callK(k, VOID);
          const clause = (items[idx] as { tag: 'list'; value: SchemeVal[] }).value;
          if (clause[0].tag === 'symbol' && clause[0].value === 'else') {
            return evaluateSeqCPS(clause, 1, env, k);
          }
          const datums = (clause[0] as { tag: 'list'; value: SchemeVal[] }).value;
          for (const d of datums) {
            const datum = astToPairs(d);
            if (schemeEq(keyVal, datum)) {
              return evaluateSeqCPS(clause, 1, env, k);
            }
          }
          return evalCaseClauses(idx + 1);
        }
        return evalCaseClauses(2);
      });
    }

    if (op === 'do') {
      // (do ((var init step) ...) (test expr ...) body ...)
      const varSpecs = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const testClause = (items[2] as { tag: 'list'; value: SchemeVal[] }).value;
      const bodyExprs = items.slice(3);

      const names: string[] = [];
      const initExprs: SchemeVal[] = [];
      const stepExprs: (SchemeVal | null)[] = [];

      for (const spec of varSpecs) {
        const sv = (spec as { tag: 'list'; value: SchemeVal[] }).value;
        names.push((sv[0] as { tag: 'symbol'; value: string }).value);
        initExprs.push(sv[1]);
        stepExprs.push(sv.length > 2 ? sv[2] : null);
      }

      // Evaluate init expressions
      return evaluateArgsCPS(initExprs, 0, env, [], (initVals) => {
        const doEnv = new Env(env);
        for (let i = 0; i < names.length; i++) {
          doEnv.define(names[i], initVals[i]);
        }

        function doLoop(): TResult {
          return evaluateCPS(testClause[0], doEnv, (testVal) => {
            if (isTruthy(testVal)) {
              // Test true: evaluate result expressions
              if (testClause.length > 1) {
                return evaluateSeqCPS(testClause, 1, doEnv, k);
              }
              return callK(k, VOID);
            }
            // Test false: evaluate body, then step
            function afterBody(): TResult {
              // Evaluate all step expressions with current values
              const stepsToEval: { idx: number; expr: SchemeVal }[] = [];
              for (let i = 0; i < names.length; i++) {
                if (stepExprs[i] !== null) {
                  stepsToEval.push({ idx: i, expr: stepExprs[i]! });
                }
              }
              if (stepsToEval.length === 0) return bounce(doLoop);

              // Evaluate all steps, collecting results
              const stepVals: SchemeVal[] = new Array(names.length);
              function evalSteps(si: number): TResult {
                if (si >= stepsToEval.length) {
                  // Apply all step results
                  for (const s of stepsToEval) {
                    doEnv.define(names[s.idx], stepVals[s.idx]);
                  }
                  return bounce(doLoop);
                }
                const { idx, expr } = stepsToEval[si];
                return evaluateCPS(expr, doEnv, (val) => {
                  stepVals[idx] = val;
                  return evalSteps(si + 1);
                });
              }
              return evalSteps(0);
            }

            if (bodyExprs.length > 0) {
              return evaluateSeqCPS(bodyExprs, 0, doEnv, (_) => afterBody());
            }
            return afterBody();
          });
        }

        return doLoop();
      });
    }

    if (op === 'guard') {
      // (guard (var clause ...) body ...)
      const guardSpec = (items[1] as { tag: 'list'; value: SchemeVal[] }).value;
      const exnVar = (guardSpec[0] as { tag: 'symbol'; value: string }).value;
      const clauses = guardSpec.slice(1);
      const body = items.slice(2);

      const guardK = k;
      const guardWind = [...windStack];

      const handler: ExHandler = (val) => {
        const raiseWind = [...windStack];
        return doWindShift(raiseWind, guardWind, () => {
          const guardEnv = new Env(env);
          guardEnv.define(exnVar, val);
          return evaluateGuardClauses(clauses, 0, guardEnv, guardK, val);
        });
      };

      exHandlerStack.push(handler);
      return evaluateSeqCPS(body, 0, env, (result) => {
        exHandlerStack.pop();
        return callK(k, result);
      });
    }

    if (op === 'define-record-type') {
      // (define-record-type <name> (constructor field-name ...) predicate (field-name accessor) ...)
      const constructorSpec = (items[2] as { tag: 'list'; value: SchemeVal[] }).value;
      const constructorName = (constructorSpec[0] as { tag: 'symbol'; value: string }).value;
      const constructorFields = constructorSpec.slice(1).map(f => (f as { tag: 'symbol'; value: string }).value);
      const predicateName = (items[3] as { tag: 'symbol'; value: string }).value;
      const typeId = recordTypeCounter++;

      // Field accessors: (field-name accessor) pairs starting at items[4]
      const fieldAccessors: { field: string; accessor: string }[] = [];
      for (let i = 4; i < items.length; i++) {
        const spec = (items[i] as { tag: 'list'; value: SchemeVal[] }).value;
        const field = (spec[0] as { tag: 'symbol'; value: string }).value;
        const accessor = (spec[1] as { tag: 'symbol'; value: string }).value;
        fieldAccessors.push({ field, accessor });
      }

      // Define constructor
      env.define(constructorName, { tag: 'builtin', name: constructorName, fn: (args) => {
        const fields = new Map<string, SchemeVal>();
        for (let i = 0; i < constructorFields.length; i++) {
          fields.set(constructorFields[i], args[i]);
        }
        return { tag: 'record', type: typeId, fields };
      }});

      // Define predicate
      env.define(predicateName, { tag: 'builtin', name: predicateName, fn: (args) => {
        return { tag: 'boolean', value: args[0].tag === 'record' && args[0].type === typeId };
      }});

      // Define accessors
      for (const fa of fieldAccessors) {
        env.define(fa.accessor, { tag: 'builtin', name: fa.accessor, fn: (args) => {
          if (args[0].tag !== 'record') throw new EvalError(`${fa.accessor}: not a record`);
          return args[0].fields.get(fa.field)!;
        }});
      }

      return callK(k, VOID);
    }

    // Check for macro application
    try {
      const headVal = env.get(op);
      if (headVal.tag === 'syntax') {
        const expanded = expandMacro(headVal, expr, env);
        if (expanded) return evaluateCPS(expanded, env, k);
      }
      if (headVal.tag === 'syntaxTransformer') {
        // Lambda-based macro transformer (syntax-case style)
        const stx: SchemeVal = { tag: 'syntaxObj', datum: expr };
        macroUseSiteEnvStack.push(env);
        macroDefEnvStack.push(headVal.defEnv);
        return applyCPS(headVal.proc, [stx], (result) => {
          macroUseSiteEnvStack.pop();
          macroDefEnvStack.pop();
          const expansion = result.tag === 'syntaxObj' ? result.datum : result;
          return evaluateCPS(expansion, env, k);
        }, expr.pos);
      }
    } catch (_) { /* not bound or not a macro */ }
  }

  // Function application: evaluate head, then args, then apply
  return evaluateCPS(head, env, (proc) => {
    return evaluateArgsCPS(items, 1, env, [], (args) => {
      return applyCPS(proc, args, k, expr.pos);
    });
  });
}

// ── Output buffer ──────────────────────────────────────────────────

let outputBuffer = '';

// ── dynamic-wind support ────────────────────────────────────────────

type WindEntry = { inThunk: SchemeVal; outThunk: SchemeVal };
let windStack: WindEntry[] = [];

// ── Exception handler stack ─────────────────────────────────────────

type ExHandler = (val: SchemeVal) => TResult;
let exHandlerStack: ExHandler[] = [];

// ── Display / Write formatting ─────────────────────────────────────

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return val.value;
    default: return writeVal(val);
  }
}

function writeVal(val: SchemeVal, seen?: Set<SchemeVal>): string {
  switch (val.tag) {
    case 'number': {
      if (val.exact === false && Number.isInteger(val.value)) {
        return val.value.toFixed(1);
      }
      return String(val.value);
    }
    case 'rational': return `${val.num}/${val.den}`;
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'char': {
      if (val.value === ' ') return '#\\space';
      if (val.value === '\n') return '#\\newline';
      if (val.value === '\t') return '#\\tab';
      return `#\\${val.value}`;
    }
    case 'list': return `(${val.value.map(v => writeVal(v, seen)).join(' ')})`;
    case 'nil': return '()';
    case 'pair': {
      if (!seen) seen = new Set();
      if (seen.has(val)) return '(...)';
      seen.add(val);
      let s = '(' + writeVal(val.car, seen);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { s += ' ...'; break; }
        seen.add(cur);
        s += ' ' + writeVal(cur.car, seen);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil' && !(cur.tag === 'pair' && seen.has(cur))) {
        s += ' . ' + writeVal(cur, seen);
      }
      s += ')';
      return s;
    }
    case 'vector': return `#(${val.value.map(v => writeVal(v, seen)).join(' ')})`;
    case 'lambda': return '#<procedure>';
    case 'caseLambda': return '#<procedure>';
    case 'builtin': return `#<builtin:${val.name}>`;
    case 'continuation': return '#<continuation>';
    case 'void': return '';
    case 'syntax': return '#<syntax>';
    case 'syntaxTransformer': return '#<syntax-transformer>';
    case 'syntaxObj': return writeVal(val.datum, seen);
    case 'values': return val.values.map(v => writeVal(v, seen)).join('\n');
    case 'record': return '#<record>';
  }
}

const display = writeVal;

// ── Public API ─────────────────────────────────────────────────────

function evaluateProgram(exprs: SchemeVal[], env: Env): SchemeVal {
  windStack = [];
  exHandlerStack = [];
  recordTypeCounter = 0;
  syntaxCaseStack = [];
  macroUseSiteEnvStack = [];
  macroDefEnvStack = [];
  const topK: K = (val) => done(val);
  const result = evaluateSeqCPS(exprs, 0, env, topK);
  return runTrampoline(result);
}

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('empty input');
  const env = makeGlobalEnv();
  const result = evaluateProgram(exprs, env);
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('empty input');
  outputBuffer = '';
  const env = makeGlobalEnv();
  const result = evaluateProgram(exprs, env);
  return { result: display(result), output: outputBuffer };
}
