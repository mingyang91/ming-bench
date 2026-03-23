import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type Cont = (val: SchemeVal) => Bounce;
type Bounce = SchemeVal | (() => Bounce);

interface WindEntry { inThunk: SchemeVal; outThunk: SchemeVal; }
let currentWinds: WindEntry[] = [];
let exceptionHandlerStack: ((val: SchemeVal) => Bounce)[] = [];
let syntaxBindingsStack: { bindings: MacroBindings; literals: Set<string>; defEnv: Env }[] = [];
let pendingSyntaxRenames: { origName: string; gsName: string; defEnv: Env }[] = [];

type SchemeVal =
  | { tag: 'number'; val: number; exact?: boolean; num?: number; den?: number; pos?: Pos }
  | { tag: 'boolean'; val: boolean; pos?: Pos }
  | { tag: 'string'; val: string; immutable?: boolean; pos?: Pos }
  | { tag: 'char'; val: string; pos?: Pos }
  | { tag: 'symbol'; val: string; pos?: Pos }
  | { tag: 'list'; val: SchemeVal[]; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'lambda'; params: string[]; rest?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'continuation'; cont: Cont; winds: WindEntry[]; pos?: Pos }
  | { tag: 'macro'; rules: MacroRule[]; literals: Set<string>; defEnv: Env; pos?: Pos }
  | { tag: 'transformer-macro'; proc: SchemeVal; defEnv: Env; pos?: Pos }
  | { tag: 'vector'; val: SchemeVal[]; pos?: Pos }
  | { tag: 'values'; vals: SchemeVal[]; pos?: Pos }
  | { tag: 'record'; typeId: symbol; typeName: string; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'native'; fn: (args: SchemeVal[], p?: Pos) => SchemeVal; pos?: Pos };

interface MacroRule {
  pattern: SchemeVal[];  // pattern elements (excluding macro name)
  template: SchemeVal;
}

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
    if (ch === '#' && i + 1 < input.length && input[i + 1] === '\'') {
      tokens.push({ text: "#'", pos: { line, col } });
      advance(); advance();
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
  if (tok.text === "#'") {
    const [val, next] = parseTokens(tokens, pos + 1);
    return [{ tag: 'list', val: [{ tag: 'symbol', val: 'syntax', pos: tok.pos }, val], pos: tok.pos }, next];
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
    return { tag: 'string', val: inner, immutable: true, pos: p };
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
    const n = parseInt(tok.text, 10);
    return { tag: 'number', val: n, exact: true, num: n, den: 1, pos: p };
  }
  // Rational literal: n/d
  if (/^-?\d+\/\d+$/.test(tok.text)) {
    const [ns, ds] = tok.text.split('/');
    return mkExactNum(parseInt(ns, 10), parseInt(ds, 10), p);
  }
  // Float literal
  if (/^-?(\d+\.\d*|\.\d+)([eE][+-]?\d+)?$/.test(tok.text) || /^-?\d+[eE][+-]?\d+$/.test(tok.text)) {
    return mkInexactNum(parseFloat(tok.text), p);
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

// ── Rational/Exact helpers ──────────────────────────────────────────

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { [a, b] = [b, a % b]; }
  return a;
}

function isExact(v: SchemeVal): boolean {
  return v.tag === 'number' && v.exact !== false;
}

function numOf(v: SchemeVal): number {
  if (v.tag !== 'number') return 0;
  if (v.num !== undefined) return v.num;
  return v.val;
}

function denOf(v: SchemeVal): number {
  if (v.tag !== 'number') return 1;
  if (v.den !== undefined) return v.den;
  return 1;
}

function mkExactNum(num: number, den: number, pos?: Pos): SchemeVal {
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(Math.abs(num), den);
  num = num / g;
  den = den / g;
  return { tag: 'number', val: num / den, exact: true, num, den, pos };
}

function mkInexactNum(val: number, pos?: Pos): SchemeVal {
  return { tag: 'number', val, exact: false, pos };
}

function floatToRational(x: number): [number, number] {
  if (Number.isInteger(x)) return [x, 1];
  // Multiply by powers of 2 until integer (works for binary fractions)
  let den = 1;
  let num = x;
  for (let i = 0; i < 53 && !Number.isInteger(num); i++) {
    den *= 2;
    num = x * den;
  }
  num = Math.round(num);
  const g = gcd(Math.abs(num), den);
  return [num / g, den / g];
}

function exactArith(op: string, args: SchemeVal[], p?: Pos): SchemeVal {
  const allExact = args.every(a => isExact(a));
  if (!allExact) {
    // Inexact: use regular float arithmetic
    let vals = args.map(a => (a as any).val as number);
    let r: number;
    switch (op) {
      case '+': r = vals.reduce((a, b) => a + b, 0); break;
      case '-':
        if (vals.length === 1) r = -vals[0];
        else r = vals.slice(1).reduce((a, b) => a - b, vals[0]);
        break;
      case '*': r = vals.reduce((a, b) => a * b, 1); break;
      case '/':
        r = vals[0];
        for (let i = 1; i < vals.length; i++) {
          if (vals[i] === 0) throw posError('division by zero', p);
          r /= vals[i];
        }
        break;
      default: r = 0;
    }
    return mkInexactNum(r, p);
  }
  // Exact rational arithmetic
  let rn: number, rd: number;
  switch (op) {
    case '+':
      rn = 0; rd = 1;
      for (const a of args) {
        const an = numOf(a), ad = denOf(a);
        rn = rn * ad + an * rd;
        rd = rd * ad;
      }
      break;
    case '-':
      if (args.length === 1) {
        rn = -numOf(args[0]); rd = denOf(args[0]);
      } else {
        rn = numOf(args[0]); rd = denOf(args[0]);
        for (let i = 1; i < args.length; i++) {
          const an = numOf(args[i]), ad = denOf(args[i]);
          rn = rn * ad - an * rd;
          rd = rd * ad;
        }
      }
      break;
    case '*':
      rn = 1; rd = 1;
      for (const a of args) {
        rn *= numOf(a);
        rd *= denOf(a);
      }
      break;
    case '/':
      rn = numOf(args[0]); rd = denOf(args[0]);
      for (let i = 1; i < args.length; i++) {
        const an = numOf(args[i]), ad = denOf(args[i]);
        if (an === 0) throw posError('division by zero', p);
        rn *= ad;
        rd *= an;
      }
      break;
    default:
      rn = 0; rd = 1;
  }
  return mkExactNum(rn, rd, p);
}

// ── Pair/List Helpers ─────────────────────────────────────────────────

const NIL: SchemeVal = { tag: 'list', val: [] };

function makePair(car: SchemeVal, cdr: SchemeVal): SchemeVal {
  return { tag: 'pair', car, cdr } as SchemeVal;
}

function listToPairs(items: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = items.length - 1; i >= 0; i--) {
    result = makePair(items[i], result);
  }
  return result;
}

function isNullVal(v: SchemeVal): boolean {
  return v.tag === 'list' && v.val.length === 0;
}

function isPairVal(v: SchemeVal): boolean {
  return v.tag === 'pair' || (v.tag === 'list' && v.val.length > 0);
}

function getCar(v: SchemeVal, name: string, p?: Pos): SchemeVal {
  if (v.tag === 'pair') return v.car;
  if (v.tag === 'list' && v.val.length > 0) return v.val[0];
  throw posError(`${name}: not a pair`, p);
}

function getCdr(v: SchemeVal, name: string, p?: Pos): SchemeVal {
  if (v.tag === 'pair') return v.cdr;
  if (v.tag === 'list' && v.val.length > 0) {
    if (v.val.length === 3 && v.val[1].tag === 'symbol' && v.val[1].val === '.') {
      return v.val[2];
    }
    if (v.val.length === 1) return NIL;
    return { tag: 'list', val: v.val.slice(1) };
  }
  throw posError(`${name}: not a pair`, p);
}

// Convert pair chain or list to a JS array. Returns null if circular.
function toArray(v: SchemeVal): SchemeVal[] | null {
  if (v.tag === 'list') return v.val;
  if (v.tag === 'pair') {
    const result: SchemeVal[] = [];
    let slow: SchemeVal = v, fast: SchemeVal = v;
    let toggle = false;
    while (fast.tag === 'pair') {
      result.push((fast as any).car);
      fast = (fast as any).cdr;
      if (toggle) {
        slow = (slow as any).cdr;
        if (slow === fast) return null; // cycle
      }
      toggle = !toggle;
    }
    if (fast.tag === 'list') {
      for (const el of fast.val) result.push(el);
    }
    return result;
  }
  return [];
}

// Like toArray but throws on non-list
function toArrayChecked(v: SchemeVal, name: string, p?: Pos): SchemeVal[] {
  const arr = toArray(v);
  if (arr === null) throw posError(`${name}: circular list`, p);
  return arr;
}

// Check if v is a proper list (terminates in nil, no cycles)
function isProperList(v: SchemeVal): boolean {
  if (v.tag === 'list') {
    // Check for dotted pair notation
    if (v.val.length >= 3 && v.val[v.val.length - 2].tag === 'symbol' && (v.val[v.val.length - 2] as any).val === '.') {
      return false;
    }
    return true;
  }
  if (v.tag !== 'pair') return false;
  // Floyd's cycle detection
  let slow: SchemeVal = v, fast: SchemeVal = v;
  while (true) {
    if (fast.tag !== 'pair') {
      return isNullVal(fast);
    }
    fast = fast.cdr;
    if (fast.tag !== 'pair') {
      return isNullVal(fast);
    }
    fast = fast.cdr;
    slow = (slow as any).cdr;
    if (slow === fast) return false; // cycle
  }
}

function displayVal(v: SchemeVal, seen?: Set<SchemeVal>): string {
  switch (v.tag) {
    case 'number':
      if (v.exact === false) {
        const s = String(v.val);
        return Number.isInteger(v.val) ? s + '.0' : s;
      }
      if (v.den !== undefined && v.den !== 1) {
        return `${v.num}/${v.den}`;
      }
      return String(v.num !== undefined ? v.num : v.val);
    case 'boolean':
      return v.val ? '#t' : '#f';
    case 'string':
      return `"${v.val}"`;
    case 'char':
      return `#\\${v.val}`;
    case 'symbol':
      return v.val;
    case 'list':
      return '(' + v.val.map(el => displayVal(el, seen)).join(' ') + ')';
    case 'pair': {
      if (!seen) seen = new Set();
      if (seen.has(v)) return '(...)';
      seen.add(v);
      let parts: string[] = [];
      let cur: SchemeVal = v;
      while (cur.tag === 'pair') {
        if (cur !== v && seen.has(cur)) { parts.push('...'); break; }
        if (cur !== v) seen.add(cur);
        parts.push(displayVal(cur.car, seen));
        cur = cur.cdr;
      }
      if (cur.tag === 'list' && cur.val.length === 0) {
        return '(' + parts.join(' ') + ')';
      }
      if (cur.tag === 'list') {
        // Shouldn't normally happen but handle gracefully
        for (const el of cur.val) parts.push(displayVal(el, seen));
        return '(' + parts.join(' ') + ')';
      }
      return '(' + parts.join(' ') + ' . ' + displayVal(cur, seen) + ')';
    }
    case 'lambda':
    case 'builtin':
    case 'continuation':
    case 'native':
      return '#<procedure>';
    case 'macro':
    case 'transformer-macro':
      return '#<macro>';
    case 'vector':
      return '#(' + v.val.map(el => displayVal(el, seen)).join(' ') + ')';
    case 'record':
      return `#<record:${v.typeName}>`;
    case 'values':
      return v.vals.map(el => displayVal(el, seen)).join('\n');
  }
}

function displayValUnquoted(v: SchemeVal, seen?: Set<SchemeVal>): string {
  switch (v.tag) {
    case 'string':
      return v.val;
    case 'char':
      return v.val;
    default:
      return displayVal(v, seen);
  }
}

function quoteToScheme(v: SchemeVal): SchemeVal {
  return v;
}

// ── Hygienic Macros ─────────────────────────────────────────────────

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'set!', 'lambda', 'and', 'or', 'begin',
  'cond', 'let', 'let*', 'letrec', 'letrec*', 'case', 'do',
  'define-syntax', 'syntax-rules', 'syntax-case', 'syntax', 'with-syntax', 'else', 'define-record-type',
  'guard', 'dynamic-wind', 'when', 'unless', 'call-with-values', 'call/cc', 'call-with-current-continuation',
  'values', 'with-exception-handler', 'raise', 'raise-continuable',
]);

let gensymCounter = 0;
function gensym(name: string): string {
  return `${name}__gs${gensymCounter++}`;
}

type MacroBindings = Map<string, SchemeVal | SchemeVal[]>;

function matchPatternEl(
  pat: SchemeVal, inp: SchemeVal, literals: Set<string>, bindings: MacroBindings
): boolean {
  if (pat.tag === 'symbol') {
    if (pat.val === '_') return true;
    if (literals.has(pat.val)) return inp.tag === 'symbol' && inp.val === pat.val;
    bindings.set(pat.val, inp);
    return true;
  }
  if (pat.tag === 'boolean' && inp.tag === 'boolean') return pat.val === inp.val;
  if (pat.tag === 'number' && inp.tag === 'number') return pat.val === inp.val;
  if (pat.tag === 'string' && inp.tag === 'string') return pat.val === inp.val;
  if (pat.tag === 'list' && inp.tag === 'list') return matchPatternList(pat.val, inp.val, literals, bindings);
  return false;
}

function matchPatternList(
  pat: SchemeVal[], inp: SchemeVal[], literals: Set<string>, bindings: MacroBindings
): boolean {
  // Find ellipsis position: pattern[i] followed by ...
  let ellIdx = -1;
  for (let i = 0; i < pat.length - 1; i++) {
    const nxt = pat[i + 1]; if (nxt.tag === 'symbol' && nxt.val === '...') { ellIdx = i; break; }
  }

  if (ellIdx < 0) {
    if (pat.length !== inp.length) return false;
    for (let i = 0; i < pat.length; i++) {
      if (!matchPatternEl(pat[i], inp[i], literals, bindings)) return false;
    }
    return true;
  }

  // Elements before ellipsis
  for (let i = 0; i < ellIdx; i++) {
    if (i >= inp.length) return false;
    if (!matchPatternEl(pat[i], inp[i], literals, bindings)) return false;
  }

  const afterCount = pat.length - ellIdx - 2;
  const ellEnd = inp.length - afterCount;
  if (ellEnd < ellIdx) return false;

  // Ellipsis-matched elements
  const ellPat = pat[ellIdx];
  if (ellPat.tag === 'symbol' && !literals.has(ellPat.val)) {
    const matched: SchemeVal[] = [];
    for (let i = ellIdx; i < ellEnd; i++) matched.push(inp[i]);
    bindings.set(ellPat.val, matched);
  }

  // Elements after ellipsis
  for (let i = 0; i < afterCount; i++) {
    if (!matchPatternEl(pat[ellIdx + 2 + i], inp[ellEnd + i], literals, bindings)) return false;
  }
  return true;
}

function findEllipsisVars(tmpl: SchemeVal, bindings: MacroBindings): string[] {
  if (tmpl.tag === 'symbol' && bindings.has(tmpl.val) && Array.isArray(bindings.get(tmpl.val))) {
    return [tmpl.val];
  }
  if (tmpl.tag === 'list') {
    const result: string[] = [];
    for (const el of tmpl.val) result.push(...findEllipsisVars(el, bindings));
    return result;
  }
  return [];
}

function instantiate(
  tmpl: SchemeVal, bindings: MacroBindings, renames: Map<string, string>
): SchemeVal {
  if (tmpl.tag === 'symbol') {
    const name = tmpl.val;
    if (bindings.has(name)) {
      const val = bindings.get(name)!;
      if (Array.isArray(val)) throw new EvalError('macro: ellipsis var outside ellipsis');
      return val as SchemeVal;
    }
    if (renames.has(name)) return { tag: 'symbol', val: renames.get(name)! };
    return tmpl;
  }
  if (tmpl.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < tmpl.val.length; i++) {
      const el = tmpl.val[i];
      const nxtT = i + 1 < tmpl.val.length ? tmpl.val[i + 1] : undefined;
      if (nxtT && nxtT.tag === 'symbol' && nxtT.val === '...') {
        const eVars = findEllipsisVars(el, bindings);
        if (eVars.length > 0) {
          const list0 = bindings.get(eVars[0]);
          if (Array.isArray(list0)) {
            for (let j = 0; j < list0.length; j++) {
              const iterBindings = new Map(bindings);
              for (const ev of eVars) {
                const evList = bindings.get(ev);
                if (Array.isArray(evList)) iterBindings.set(ev, evList[j]);
              }
              result.push(instantiate(el, iterBindings, renames));
            }
          }
        }
        i++; // skip ...
        continue;
      }
      result.push(instantiate(el, bindings, renames));
    }
    return { tag: 'list', val: result, pos: tmpl.pos };
  }
  return tmpl;
}

function expandMacro(macro: SchemeVal & { tag: 'macro' }, form: SchemeVal[], env: Env): SchemeVal {
  const input = form.slice(1); // skip macro name
  for (const rule of macro.rules) {
    const bindings: MacroBindings = new Map();
    if (matchPatternList(rule.pattern, input, macro.literals, bindings)) {
      const patVars = new Set(bindings.keys());

      // Collect macro-introduced symbols and create renames
      const renames = new Map<string, string>();
      const collectIntroduced = (t: SchemeVal) => {
        if (t.tag === 'symbol' && !patVars.has(t.val) && !SPECIAL_FORMS.has(t.val) && t.val !== '...') {
          if (!renames.has(t.val)) renames.set(t.val, gensym(t.val));
        }
        if (t.tag === 'list') for (const el of t.val) collectIntroduced(el);
      };
      collectIntroduced(rule.template);

      const expanded = instantiate(rule.template, bindings, renames);

      // Pre-bind gensyms to definition-site values for hygiene
      for (const [origName, gsName] of renames) {
        try {
          const val = macro.defEnv.get(origName);
          env.define(gsName, val);
        } catch (_) { /* fresh introduced binding, no pre-bind needed */ }
      }

      return expanded;
    }
  }
  throw new EvalError('macro: no matching pattern');
}

// ── Builtins ────────────────────────────────────────────────────────

function applyBuiltin(op: string, evalArgs: SchemeVal[], p?: Pos, out?: string[]): SchemeVal {
  switch (op) {
    case '+': {
      for (const a of evalArgs) toNumber(a, '+', p);
      if (evalArgs.length === 0) return mkExactNum(0, 1, p);
      return exactArith('+', evalArgs, p);
    }
    case '-': {
      if (evalArgs.length === 0) throw posError('-: need at least one arg', p);
      for (const a of evalArgs) toNumber(a, '-', p);
      return exactArith('-', evalArgs, p);
    }
    case '*': {
      for (const a of evalArgs) toNumber(a, '*', p);
      if (evalArgs.length === 0) return mkExactNum(1, 1, p);
      return exactArith('*', evalArgs, p);
    }
    case '/': {
      if (evalArgs.length < 2) throw posError('/: need at least two args', p);
      for (const a of evalArgs) toNumber(a, '/', p);
      return exactArith('/', evalArgs, p);
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
      return makePair(evalArgs[0], evalArgs[1]);
    }
    case 'car': {
      if (evalArgs.length !== 1) throw posError('car: need exactly one arg', p);
      return getCar(evalArgs[0], 'car', p);
    }
    case 'cdr': {
      if (evalArgs.length !== 1) throw posError('cdr: need exactly one arg', p);
      return getCdr(evalArgs[0], 'cdr', p);
    }
    case 'set-car!': {
      if (evalArgs.length !== 2) throw posError('set-car!: need exactly two args', p);
      const target = evalArgs[0];
      if (target.tag === 'pair') { (target as any).car = evalArgs[1]; return evalArgs[1]; }
      if (target.tag === 'list' && target.val.length > 0) { target.val[0] = evalArgs[1]; return evalArgs[1]; }
      throw posError('set-car!: not a pair', p);
    }
    case 'set-cdr!': {
      if (evalArgs.length !== 2) throw posError('set-cdr!: need exactly two args', p);
      const target = evalArgs[0];
      if (target.tag === 'pair') { (target as any).cdr = evalArgs[1]; return evalArgs[1]; }
      throw posError('set-cdr!: not a pair', p);
    }
    case 'cadr': {
      if (evalArgs.length !== 1) throw posError('cadr: need exactly one arg', p);
      return getCar(getCdr(evalArgs[0], 'cadr', p), 'cadr', p);
    }
    case 'caar': {
      if (evalArgs.length !== 1) throw posError('caar: need exactly one arg', p);
      return getCar(getCar(evalArgs[0], 'caar', p), 'caar', p);
    }
    case 'cdar': {
      if (evalArgs.length !== 1) throw posError('cdar: need exactly one arg', p);
      return getCdr(getCar(evalArgs[0], 'cdar', p), 'cdar', p);
    }
    case 'cddr': {
      if (evalArgs.length !== 1) throw posError('cddr: need exactly one arg', p);
      return getCdr(getCdr(evalArgs[0], 'cddr', p), 'cddr', p);
    }
    case 'caddr': {
      if (evalArgs.length !== 1) throw posError('caddr: need exactly one arg', p);
      return getCar(getCdr(getCdr(evalArgs[0], 'caddr', p), 'caddr', p), 'caddr', p);
    }
    case 'cadddr': {
      if (evalArgs.length !== 1) throw posError('cadddr: need exactly one arg', p);
      return getCar(getCdr(getCdr(getCdr(evalArgs[0], 'cadddr', p), 'cadddr', p), 'cadddr', p), 'cadddr', p);
    }
    case 'caddar': {
      if (evalArgs.length !== 1) throw posError('caddar: need exactly one arg', p);
      return getCar(getCdr(getCdr(getCar(evalArgs[0], 'caddar', p), 'caddar', p), 'caddar', p), 'caddar', p);
    }
    case 'null?': {
      if (evalArgs.length !== 1) throw posError('null?: need exactly one arg', p);
      return { tag: 'boolean', val: isNullVal(evalArgs[0]) };
    }
    case 'list': {
      return listToPairs(evalArgs);
    }
    case 'length': {
      if (evalArgs.length !== 1) throw posError('length: need exactly one arg', p);
      const arr = toArray(evalArgs[0]);
      if (arr === null) throw posError('length: circular list', p);
      if (evalArgs[0].tag !== 'list' && evalArgs[0].tag !== 'pair') throw posError('length: not a list', p);
      return { tag: 'number', val: arr.length };
    }
    case 'append': {
      if (evalArgs.length === 0) return NIL;
      if (evalArgs.length === 1) return evalArgs[0];
      let result: SchemeVal[] = [];
      for (let i = 0; i < evalArgs.length - 1; i++) {
        const a = evalArgs[i];
        const arr = toArrayChecked(a, 'append', p);
        result = result.concat(arr);
      }
      const last = evalArgs[evalArgs.length - 1];
      if (result.length === 0) return last;
      // Build pair chain with last as the tail
      let tail = last;
      for (let i = result.length - 1; i >= 0; i--) {
        tail = makePair(result[i], tail);
      }
      return tail;
    }
    case 'reverse': {
      if (evalArgs.length !== 1) throw posError('reverse: need exactly one arg', p);
      const arr = toArrayChecked(evalArgs[0], 'reverse', p);
      return listToPairs([...arr].reverse());
    }
    case 'pair?': {
      if (evalArgs.length !== 1) throw posError('pair?: need exactly one arg', p);
      return { tag: 'boolean', val: isPairVal(evalArgs[0]) };
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
      if (out) out.push(displayValUnquoted(evalArgs[0], new Set()));
      return { tag: 'boolean', val: false };
    }
    case 'write': {
      if (evalArgs.length !== 1) throw posError('write: need exactly one arg', p);
      if (out) out.push(displayVal(evalArgs[0], new Set()));
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
      const s = evalArgs[0].val;
      const n = Number(s);
      if (isNaN(n)) return { tag: 'boolean', val: false };
      if (/^-?\d+$/.test(s)) return mkExactNum(n, 1, p);
      return mkInexactNum(n, p);
    }
    case 'number->string': {
      if (evalArgs.length !== 1) throw posError('number->string: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') throw posError('number->string: expected number', p);
      return { tag: 'string', val: displayVal(evalArgs[0]) };
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
      if (evalArgs[0].tag !== 'string') throw posError('string-set!: expected string', p);
      if (evalArgs[0].immutable) throw posError('string-set!: strings are immutable', p);
      const si = toNumber(evalArgs[1], 'string-set!', p);
      if (evalArgs[2].tag !== 'char') throw posError('string-set!: expected char', p);
      const s0 = evalArgs[0].val;
      if (si < 0 || si >= s0.length) throw posError('string-set!: index out of range', p);
      evalArgs[0].val = s0.slice(0, si) + evalArgs[2].val + s0.slice(si + 1);
      return { tag: 'boolean', val: false };
    }
    // ── L14 String/char conversion ──
    case 'string->list': {
      if (evalArgs.length !== 1) throw posError('string->list: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string->list: expected string', p);
      const chars: SchemeVal[] = [...evalArgs[0].val].map(c => ({ tag: 'char' as const, val: c }));
      return listToPairs(chars);
    }
    case 'list->string': {
      if (evalArgs.length !== 1) throw posError('list->string: need exactly one arg', p);
      const elems = toArrayChecked(evalArgs[0], 'list->string', p);
      let str = '';
      for (const el of elems) {
        if (el.tag !== 'char') throw posError('list->string: expected char in list', p);
        str += el.val;
      }
      return { tag: 'string', val: str };
    }
    case 'char->integer': {
      if (evalArgs.length !== 1) throw posError('char->integer: need exactly one arg', p);
      if (evalArgs[0].tag !== 'char') throw posError('char->integer: expected char', p);
      const cp = evalArgs[0].val.codePointAt(0)!;
      return mkExactNum(cp, 1, p);
    }
    case 'integer->char': {
      if (evalArgs.length !== 1) throw posError('integer->char: need exactly one arg', p);
      const n = toNumber(evalArgs[0], 'integer->char', p);
      return { tag: 'char', val: String.fromCodePoint(n) };
    }
    case 'char?': {
      if (evalArgs.length !== 1) throw posError('char?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'char' };
    }
    // ── L13 Numeric builtins ──
    case 'abs': {
      if (evalArgs.length !== 1) throw posError('abs: need exactly one arg', p);
      return { tag: 'number', val: Math.abs(toNumber(evalArgs[0], 'abs', p)) };
    }
    case 'modulo': {
      if (evalArgs.length !== 2) throw posError('modulo: need exactly two args', p);
      const a = toNumber(evalArgs[0], 'modulo', p);
      const b = toNumber(evalArgs[1], 'modulo', p);
      if (b === 0) throw posError('modulo: division by zero', p);
      return { tag: 'number', val: ((a % b) + b) % b };
    }
    case 'remainder': {
      if (evalArgs.length !== 2) throw posError('remainder: need exactly two args', p);
      const a = toNumber(evalArgs[0], 'remainder', p);
      const b = toNumber(evalArgs[1], 'remainder', p);
      if (b === 0) throw posError('remainder: division by zero', p);
      return { tag: 'number', val: a % b };
    }
    case 'quotient': {
      if (evalArgs.length !== 2) throw posError('quotient: need exactly two args', p);
      const a = toNumber(evalArgs[0], 'quotient', p);
      const b = toNumber(evalArgs[1], 'quotient', p);
      if (b === 0) throw posError('quotient: division by zero', p);
      return { tag: 'number', val: Math.trunc(a / b) };
    }
    case 'min': {
      if (evalArgs.length === 0) throw posError('min: need at least one arg', p);
      let m = toNumber(evalArgs[0], 'min', p);
      for (let i = 1; i < evalArgs.length; i++) {
        const v = toNumber(evalArgs[i], 'min', p);
        if (v < m) m = v;
      }
      return { tag: 'number', val: m };
    }
    case 'max': {
      if (evalArgs.length === 0) throw posError('max: need at least one arg', p);
      let m = toNumber(evalArgs[0], 'max', p);
      for (let i = 1; i < evalArgs.length; i++) {
        const v = toNumber(evalArgs[i], 'max', p);
        if (v > m) m = v;
      }
      return { tag: 'number', val: m };
    }
    case 'expt': {
      if (evalArgs.length !== 2) throw posError('expt: need exactly two args', p);
      const base = toNumber(evalArgs[0], 'expt', p);
      const exp = toNumber(evalArgs[1], 'expt', p);
      return { tag: 'number', val: Math.pow(base, exp) };
    }
    case 'zero?': {
      if (evalArgs.length !== 1) throw posError('zero?: need exactly one arg', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], 'zero?', p) === 0 };
    }
    case 'positive?': {
      if (evalArgs.length !== 1) throw posError('positive?: need exactly one arg', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], 'positive?', p) > 0 };
    }
    case 'negative?': {
      if (evalArgs.length !== 1) throw posError('negative?: need exactly one arg', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], 'negative?', p) < 0 };
    }
    case 'odd?': {
      if (evalArgs.length !== 1) throw posError('odd?: need exactly one arg', p);
      return { tag: 'boolean', val: Math.abs(toNumber(evalArgs[0], 'odd?', p)) % 2 === 1 };
    }
    case 'even?': {
      if (evalArgs.length !== 1) throw posError('even?: need exactly one arg', p);
      return { tag: 'boolean', val: toNumber(evalArgs[0], 'even?', p) % 2 === 0 };
    }
    // ── L19 Exact/Rational builtins ──
    case 'exact?': {
      if (evalArgs.length !== 1) throw posError('exact?: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') return { tag: 'boolean', val: false };
      return { tag: 'boolean', val: isExact(evalArgs[0]) };
    }
    case 'inexact?': {
      if (evalArgs.length !== 1) throw posError('inexact?: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') return { tag: 'boolean', val: false };
      return { tag: 'boolean', val: !isExact(evalArgs[0]) };
    }
    case 'exact->inexact': {
      if (evalArgs.length !== 1) throw posError('exact->inexact: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') throw posError('exact->inexact: expected number', p);
      return mkInexactNum(evalArgs[0].val, p);
    }
    case 'inexact->exact': {
      if (evalArgs.length !== 1) throw posError('inexact->exact: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') throw posError('inexact->exact: expected number', p);
      const [rn, rd] = floatToRational(evalArgs[0].val);
      return mkExactNum(rn, rd, p);
    }
    case 'numerator': {
      if (evalArgs.length !== 1) throw posError('numerator: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') throw posError('numerator: expected number', p);
      const v = evalArgs[0];
      if (isExact(v)) {
        return mkExactNum(numOf(v), 1, p);
      }
      const [fn] = floatToRational(v.val);
      return mkInexactNum(fn, p);
    }
    case 'denominator': {
      if (evalArgs.length !== 1) throw posError('denominator: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') throw posError('denominator: expected number', p);
      const v = evalArgs[0];
      if (isExact(v)) {
        return mkExactNum(denOf(v), 1, p);
      }
      const [, fd] = floatToRational(v.val);
      return mkInexactNum(fd, p);
    }
    case 'integer?': {
      if (evalArgs.length !== 1) throw posError('integer?: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') return { tag: 'boolean', val: false };
      return { tag: 'boolean', val: Number.isInteger(evalArgs[0].val) };
    }
    case 'rational?': {
      if (evalArgs.length !== 1) throw posError('rational?: need exactly one arg', p);
      if (evalArgs[0].tag !== 'number') return { tag: 'boolean', val: false };
      return { tag: 'boolean', val: isExact(evalArgs[0]) };
    }
    // ── L13 List builtins ──
    case 'list-ref': {
      if (evalArgs.length !== 2) throw posError('list-ref: need exactly two args', p);
      const lrIdx = toNumber(evalArgs[1], 'list-ref', p);
      let lrCur = evalArgs[0];
      for (let lri = 0; lri < lrIdx; lri++) {
        lrCur = getCdr(lrCur, 'list-ref', p);
      }
      return getCar(lrCur, 'list-ref', p);
    }
    case 'list-tail': {
      if (evalArgs.length !== 2) throw posError('list-tail: need exactly two args', p);
      const ltIdx = toNumber(evalArgs[1], 'list-tail', p);
      let ltCur = evalArgs[0];
      for (let lti = 0; lti < ltIdx; lti++) {
        ltCur = getCdr(ltCur, 'list-tail', p);
      }
      return ltCur;
    }
    case 'list?': {
      if (evalArgs.length !== 1) throw posError('list?: need exactly one arg', p);
      return { tag: 'boolean', val: isNullVal(evalArgs[0]) || isProperList(evalArgs[0]) };
    }
    case 'assoc': {
      if (evalArgs.length !== 2) throw posError('assoc: need exactly two args', p);
      const key = evalArgs[0];
      const alist = evalArgs[1];
      const assocArr = toArrayChecked(alist, 'assoc', p);
      for (const pair of assocArr) {
        if (!isPairVal(pair)) continue;
        if (schemeEqual(key, getCar(pair, 'assoc', p))) return pair;
      }
      return { tag: 'boolean', val: false };
    }
    case 'eq?': {
      if (evalArgs.length !== 2) throw posError('eq?: need exactly two args', p);
      const [a, b] = evalArgs;
      if (a.tag !== b.tag) return { tag: 'boolean', val: false };
      if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'string' && b.tag === 'string') return { tag: 'boolean', val: a === b }; // identity
      if (a.tag === 'list' && b.tag === 'list') return { tag: 'boolean', val: a === b }; // identity
      if (a.tag === 'pair' && b.tag === 'pair') return { tag: 'boolean', val: a === b }; // identity
      if (a.tag === 'vector' && b.tag === 'vector') return { tag: 'boolean', val: a === b }; // identity
      return { tag: 'boolean', val: false };
    }
    case 'equal?': {
      if (evalArgs.length !== 2) throw posError('equal?: need exactly two args', p);
      return { tag: 'boolean', val: schemeEqual(evalArgs[0], evalArgs[1]) };
    }
    // ── L13 Char builtins ──
    case 'char-alphabetic?': {
      if (evalArgs.length !== 1) throw posError('char-alphabetic?: need exactly one arg', p);
      if (evalArgs[0].tag !== 'char') throw posError('char-alphabetic?: expected char', p);
      return { tag: 'boolean', val: /^[a-zA-Z]$/.test(evalArgs[0].val) };
    }
    case 'char-numeric?': {
      if (evalArgs.length !== 1) throw posError('char-numeric?: need exactly one arg', p);
      if (evalArgs[0].tag !== 'char') throw posError('char-numeric?: expected char', p);
      return { tag: 'boolean', val: /^[0-9]$/.test(evalArgs[0].val) };
    }
    case 'char-upcase': {
      if (evalArgs.length !== 1) throw posError('char-upcase: need exactly one arg', p);
      if (evalArgs[0].tag !== 'char') throw posError('char-upcase: expected char', p);
      return { tag: 'char', val: evalArgs[0].val.toUpperCase() };
    }
    case 'char-downcase': {
      if (evalArgs.length !== 1) throw posError('char-downcase: need exactly one arg', p);
      if (evalArgs[0].tag !== 'char') throw posError('char-downcase: expected char', p);
      return { tag: 'char', val: evalArgs[0].val.toLowerCase() };
    }
    case 'char=?': {
      if (evalArgs.length !== 2) throw posError('char=?: need exactly two args', p);
      if (evalArgs[0].tag !== 'char' || evalArgs[1].tag !== 'char') throw posError('char=?: expected chars', p);
      return { tag: 'boolean', val: evalArgs[0].val === evalArgs[1].val };
    }
    case 'char<?': {
      if (evalArgs.length !== 2) throw posError('char<?: need exactly two args', p);
      if (evalArgs[0].tag !== 'char' || evalArgs[1].tag !== 'char') throw posError('char<?: expected chars', p);
      return { tag: 'boolean', val: evalArgs[0].val.charCodeAt(0) < evalArgs[1].val.charCodeAt(0) };
    }
    // ── L13 String builtins ──
    case 'string=?': {
      if (evalArgs.length !== 2) throw posError('string=?: need exactly two args', p);
      if (evalArgs[0].tag !== 'string' || evalArgs[1].tag !== 'string') throw posError('string=?: expected strings', p);
      return { tag: 'boolean', val: evalArgs[0].val === evalArgs[1].val };
    }
    case 'string<?': {
      if (evalArgs.length !== 2) throw posError('string<?: need exactly two args', p);
      if (evalArgs[0].tag !== 'string' || evalArgs[1].tag !== 'string') throw posError('string<?: expected strings', p);
      return { tag: 'boolean', val: evalArgs[0].val < evalArgs[1].val };
    }
    case 'string>?': {
      if (evalArgs.length !== 2) throw posError('string>?: need exactly two args', p);
      if (evalArgs[0].tag !== 'string' || evalArgs[1].tag !== 'string') throw posError('string>?: expected strings', p);
      return { tag: 'boolean', val: evalArgs[0].val > evalArgs[1].val };
    }
    case 'string<=?': {
      if (evalArgs.length !== 2) throw posError('string<=?: need exactly two args', p);
      if (evalArgs[0].tag !== 'string' || evalArgs[1].tag !== 'string') throw posError('string<=?: expected strings', p);
      return { tag: 'boolean', val: evalArgs[0].val <= evalArgs[1].val };
    }
    case 'string>=?': {
      if (evalArgs.length !== 2) throw posError('string>=?: need exactly two args', p);
      if (evalArgs[0].tag !== 'string' || evalArgs[1].tag !== 'string') throw posError('string>=?: expected strings', p);
      return { tag: 'boolean', val: evalArgs[0].val >= evalArgs[1].val };
    }
    case 'string-ci=?': {
      if (evalArgs.length !== 2) throw posError('string-ci=?: need exactly two args', p);
      if (evalArgs[0].tag !== 'string' || evalArgs[1].tag !== 'string') throw posError('string-ci=?: expected strings', p);
      return { tag: 'boolean', val: evalArgs[0].val.toLowerCase() === evalArgs[1].val.toLowerCase() };
    }
    case 'string-upcase': {
      if (evalArgs.length !== 1) throw posError('string-upcase: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string-upcase: expected string', p);
      return { tag: 'string', val: evalArgs[0].val.toUpperCase() };
    }
    case 'string-downcase': {
      if (evalArgs.length !== 1) throw posError('string-downcase: need exactly one arg', p);
      if (evalArgs[0].tag !== 'string') throw posError('string-downcase: expected string', p);
      return { tag: 'string', val: evalArgs[0].val.toLowerCase() };
    }
    case 'eqv?': {
      if (evalArgs.length !== 2) throw posError('eqv?: need exactly two args', p);
      const [a, b] = evalArgs;
      if (a.tag !== b.tag) return { tag: 'boolean', val: false };
      if (a.tag === 'number' && b.tag === 'number') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'boolean' && b.tag === 'boolean') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'symbol' && b.tag === 'symbol') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'char' && b.tag === 'char') return { tag: 'boolean', val: a.val === b.val };
      if (a.tag === 'string' && b.tag === 'string') return { tag: 'boolean', val: a === b };
      if (a.tag === 'list' && b.tag === 'list') return { tag: 'boolean', val: a === b };
      if (a.tag === 'vector' && b.tag === 'vector') return { tag: 'boolean', val: a === b };
      return { tag: 'boolean', val: false };
    }
    case 'vector': {
      return { tag: 'vector', val: [...evalArgs] };
    }
    case 'make-vector': {
      if (evalArgs.length < 1 || evalArgs.length > 2) throw posError('make-vector: need 1-2 args', p);
      const n = toNumber(evalArgs[0], 'make-vector', p);
      const fill: SchemeVal = evalArgs.length === 2 ? evalArgs[1] : { tag: 'number', val: 0 };
      const arr: SchemeVal[] = [];
      for (let i = 0; i < n; i++) arr.push(fill);
      return { tag: 'vector', val: arr };
    }
    case 'vector-ref': {
      if (evalArgs.length !== 2) throw posError('vector-ref: need exactly two args', p);
      if (evalArgs[0].tag !== 'vector') throw posError('vector-ref: expected vector', p);
      const idx = toNumber(evalArgs[1], 'vector-ref', p);
      if (idx < 0 || idx >= evalArgs[0].val.length) throw posError('vector-ref: index out of range', p);
      return evalArgs[0].val[idx];
    }
    case 'vector-set!': {
      if (evalArgs.length !== 3) throw posError('vector-set!: need exactly three args', p);
      if (evalArgs[0].tag !== 'vector') throw posError('vector-set!: expected vector', p);
      const setIdx = toNumber(evalArgs[1], 'vector-set!', p);
      if (setIdx < 0 || setIdx >= evalArgs[0].val.length) throw posError('vector-set!: index out of range', p);
      evalArgs[0].val[setIdx] = evalArgs[2];
      return { tag: 'boolean', val: false };
    }
    case 'vector-length': {
      if (evalArgs.length !== 1) throw posError('vector-length: need exactly one arg', p);
      if (evalArgs[0].tag !== 'vector') throw posError('vector-length: expected vector', p);
      return { tag: 'number', val: evalArgs[0].val.length };
    }
    case 'vector?': {
      if (evalArgs.length !== 1) throw posError('vector?: need exactly one arg', p);
      return { tag: 'boolean', val: evalArgs[0].tag === 'vector' };
    }
    case 'vector->list': {
      if (evalArgs.length !== 1) throw posError('vector->list: need exactly one arg', p);
      if (evalArgs[0].tag !== 'vector') throw posError('vector->list: expected vector', p);
      return listToPairs([...evalArgs[0].val]);
    }
    case 'list->vector': {
      if (evalArgs.length !== 1) throw posError('list->vector: need exactly one arg', p);
      const lvArr = toArrayChecked(evalArgs[0], 'list->vector', p);
      return { tag: 'vector', val: [...lvArr] };
    }
    case 'memv': {
      if (evalArgs.length !== 2) throw posError('memv: need exactly two args', p);
      let cur = evalArgs[1];
      while (isPairVal(cur)) {
        if (schemeEqv(evalArgs[0], getCar(cur, 'memv', p))) return cur;
        cur = getCdr(cur, 'memv', p);
      }
      return { tag: 'boolean', val: false };
    }
    case 'assv': {
      if (evalArgs.length !== 2) throw posError('assv: need exactly two args', p);
      const avKey = evalArgs[0];
      const avList = toArrayChecked(evalArgs[1], 'assv', p);
      for (const pair of avList) {
        if (!isPairVal(pair)) continue;
        if (schemeEqv(avKey, getCar(pair, 'assv', p))) return pair;
      }
      return { tag: 'boolean', val: false };
    }
    case 'assq': {
      if (evalArgs.length !== 2) throw posError('assq: need exactly two args', p);
      const aqKey = evalArgs[0];
      const aqList = toArrayChecked(evalArgs[1], 'assq', p);
      for (const pair of aqList) {
        if (!isPairVal(pair)) continue;
        const h = getCar(pair, 'assq', p);
        if (h === aqKey) return pair;
        if (h.tag === aqKey.tag) {
          if ((h.tag === 'number' && aqKey.tag === 'number' && h.val === aqKey.val) ||
              (h.tag === 'boolean' && aqKey.tag === 'boolean' && h.val === aqKey.val) ||
              (h.tag === 'symbol' && aqKey.tag === 'symbol' && h.val === aqKey.val) ||
              (h.tag === 'char' && aqKey.tag === 'char' && h.val === aqKey.val)) return pair;
        }
      }
      return { tag: 'boolean', val: false };
    }
    case 'member': {
      if (evalArgs.length !== 2) throw posError('member: need exactly two args', p);
      let cur = evalArgs[1];
      while (isPairVal(cur)) {
        if (schemeEqual(evalArgs[0], getCar(cur, 'member', p))) return cur;
        cur = getCdr(cur, 'member', p);
      }
      return { tag: 'boolean', val: false };
    }
    case 'memq': {
      if (evalArgs.length !== 2) throw posError('memq: need exactly two args', p);
      let cur = evalArgs[1];
      const target = evalArgs[0];
      while (isPairVal(cur)) {
        const h = getCar(cur, 'memq', p);
        if (h === target) return cur;
        if (h.tag === target.tag) {
          if ((h.tag === 'number' && target.tag === 'number' && h.val === target.val) ||
              (h.tag === 'boolean' && target.tag === 'boolean' && h.val === target.val) ||
              (h.tag === 'symbol' && target.tag === 'symbol' && h.val === target.val) ||
              (h.tag === 'char' && target.tag === 'char' && h.val === target.val)) return cur;
        }
        cur = getCdr(cur, 'memq', p);
      }
      return { tag: 'boolean', val: false };
    }
    case 'gcd': {
      if (evalArgs.length === 0) return { tag: 'number', val: 0, exact: true, num: 0, den: 1 };
      let result = Math.abs(toNumber(evalArgs[0], 'gcd', p));
      for (let gi = 1; gi < evalArgs.length; gi++) {
        let b = Math.abs(toNumber(evalArgs[gi], 'gcd', p));
        let a = result;
        while (b) { [a, b] = [b, a % b]; }
        result = a;
      }
      return { tag: 'number', val: result, exact: true, num: result, den: 1 };
    }
    case 'lcm': {
      if (evalArgs.length === 0) return { tag: 'number', val: 1, exact: true, num: 1, den: 1 };
      let result = Math.abs(toNumber(evalArgs[0], 'lcm', p));
      for (let li = 1; li < evalArgs.length; li++) {
        const b = Math.abs(toNumber(evalArgs[li], 'lcm', p));
        if (result === 0 && b === 0) { result = 0; continue; }
        result = (result / gcd(result, b)) * b;
      }
      return { tag: 'number', val: result, exact: true, num: result, den: 1 };
    }
    case 'truncate': {
      if (evalArgs.length !== 1) throw posError('truncate: need exactly one arg', p);
      const tv = toNumber(evalArgs[0], 'truncate', p);
      return { tag: 'number', val: Math.trunc(tv), exact: true, num: Math.trunc(tv), den: 1 };
    }
    case 'round': {
      if (evalArgs.length !== 1) throw posError('round: need exactly one arg', p);
      const rv = toNumber(evalArgs[0], 'round', p);
      return { tag: 'number', val: Math.round(rv), exact: true, num: Math.round(rv), den: 1 };
    }
    case 'floor': {
      if (evalArgs.length !== 1) throw posError('floor: need exactly one arg', p);
      const fv = toNumber(evalArgs[0], 'floor', p);
      return { tag: 'number', val: Math.floor(fv), exact: true, num: Math.floor(fv), den: 1 };
    }
    case 'ceiling': {
      if (evalArgs.length !== 1) throw posError('ceiling: need exactly one arg', p);
      const cv = toNumber(evalArgs[0], 'ceiling', p);
      return { tag: 'number', val: Math.ceil(cv), exact: true, num: Math.ceil(cv), den: 1 };
    }
    case 'string-copy!': {
      if (evalArgs.length < 3) throw posError('string-copy!: need at least 3 args', p);
      // (string-copy! to at from [start [end]])
      return { tag: 'boolean', val: false };
    }
    case 'string': {
      let strResult = '';
      for (const a of evalArgs) {
        if (a.tag !== 'char') throw posError('string: expected char', p);
        strResult += a.val;
      }
      return { tag: 'string', val: strResult };
    }
    case 'make-string': {
      if (evalArgs.length < 1) throw posError('make-string: need at least 1 arg', p);
      const msLen = toNumber(evalArgs[0], 'make-string', p);
      const msCh = evalArgs.length >= 2 && evalArgs[1].tag === 'char' ? evalArgs[1].val : '\0';
      return { tag: 'string', val: msCh.repeat(msLen) };
    }
    case 'syntax->datum': {
      if (evalArgs.length !== 1) throw posError('syntax->datum: need exactly 1 arg', p);
      return evalArgs[0]; // In our representation, syntax objects are plain values
    }
    case 'datum->syntax': {
      if (evalArgs.length !== 2) throw posError('datum->syntax: need exactly 2 args', p);
      return evalArgs[1]; // In our representation, just return the datum
    }
    default:
      throw posError(`unknown procedure: ${op}`, p);
  }
}

function schemeEqv(a: SchemeVal, b: SchemeVal): boolean {
  if (a.tag !== b.tag) return false;
  if (a.tag === 'number' && b.tag === 'number') return a.val === b.val;
  if (a.tag === 'boolean' && b.tag === 'boolean') return a.val === b.val;
  if (a.tag === 'symbol' && b.tag === 'symbol') return a.val === b.val;
  if (a.tag === 'char' && b.tag === 'char') return a.val === b.val;
  return a === b;
}

function schemeEqual(a: SchemeVal, b: SchemeVal, seen?: Set<string>): boolean {
  if (a === b) return true;
  if (a.tag === 'number' && b.tag === 'number') return a.val === b.val;
  if (a.tag === 'boolean' && b.tag === 'boolean') return a.val === b.val;
  if (a.tag === 'string' && b.tag === 'string') return a.val === b.val;
  if (a.tag === 'char' && b.tag === 'char') return a.val === b.val;
  if (a.tag === 'symbol' && b.tag === 'symbol') return a.val === b.val;
  if (a.tag === 'list' && b.tag === 'list') {
    if (a.val.length !== b.val.length) return false;
    for (let i = 0; i < a.val.length; i++) {
      if (!schemeEqual(a.val[i], b.val[i], seen)) return false;
    }
    return true;
  }
  // pair == pair or pair == list comparison
  if (isPairVal(a) && isPairVal(b)) {
    if (!seen) seen = new Set();
    // Use object identity pair to detect cycles
    const key = `${idOf(a)},${idOf(b)}`;
    if (seen.has(key)) return true; // assume equal if we've seen this pair before
    seen.add(key);
    return schemeEqual(getCar(a, 'equal?'), getCar(b, 'equal?'), seen) &&
           schemeEqual(getCdr(a, 'equal?'), getCdr(b, 'equal?'), seen);
  }
  if (a.tag === 'vector' && b.tag === 'vector') {
    if (a.val.length !== b.val.length) return false;
    for (let i = 0; i < a.val.length; i++) {
      if (!schemeEqual(a.val[i], b.val[i], seen)) return false;
    }
    return true;
  }
  return false;
}

let _nextId = 1;
const _idMap = new WeakMap<object, number>();
function idOf(v: SchemeVal): number {
  let id = _idMap.get(v as any);
  if (id === undefined) { id = _nextId++; _idMap.set(v as any, id); }
  return id;
}

const BUILTINS = new Set(['+', '-', '*', '/', '<', '>', '=', '<=', '>=', 'not',
  'cons', 'car', 'cdr', 'null?', 'list', 'length', 'append',
  'pair?', 'number?', 'string?', 'boolean?', 'symbol?', 'procedure?', 'char?',
  'display', 'write', 'newline',
  'string-append', 'string-length', 'substring', 'string->number', 'number->string',
  'symbol->string', 'string->symbol', 'string-ref', 'string-copy', 'string-set!',
  'abs', 'modulo', 'remainder', 'quotient', 'min', 'max', 'expt',
  'zero?', 'positive?', 'negative?', 'odd?', 'even?',
  'list-ref', 'list-tail', 'list?', 'assoc', 'eq?', 'equal?', 'eqv?',
  'char-alphabetic?', 'char-numeric?', 'char-upcase', 'char-downcase', 'char=?', 'char<?',
  'string=?', 'string<?', 'string-ci=?', 'string-upcase', 'string-downcase',
  'string->list', 'list->string', 'char->integer', 'integer->char',
  'vector', 'make-vector', 'vector-ref', 'vector-set!', 'vector-length', 'vector?',
  'vector->list', 'list->vector', 'reverse',
  'exact?', 'inexact?', 'exact->inexact', 'inexact->exact',
  'numerator', 'denominator', 'integer?', 'rational?',
  'set-car!', 'set-cdr!', 'cadr', 'caar', 'cdar', 'cddr', 'caddr', 'cadddr', 'caddar',
  'memv', 'memq', 'member', 'assv', 'assq',
  'gcd', 'lcm', 'truncate', 'round', 'floor', 'ceiling',
  'string-copy!', 'make-string', 'string',
  'string>?', 'string<=?', 'string>=?',
  'syntax->datum', 'datum->syntax']);

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
    const val: SchemeVal = args.length === 1 ? args[0] : { tag: 'values', vals: args };

    const targetWinds = proc.winds;
    // Find common prefix
    let common = 0;
    while (common < currentWinds.length && common < targetWinds.length
           && currentWinds[common] === targetWinds[common]) {
      common++;
    }
    // Unwind current (innermost first), then rewind to target (outermost first)
    const toUnwind = currentWinds.slice(common).reverse();
    const toRewind = targetWinds.slice(common);
    const doUnwind = (i: number): Bounce => {
      if (i >= toUnwind.length) return () => doRewind(0);
      currentWinds = currentWinds.slice(0, currentWinds.length - 1);
      return () => applyCPS(toUnwind[i].outThunk, [], _ => () => doUnwind(i + 1), p, out);
    };
    const doRewind = (i: number): Bounce => {
      if (i >= toRewind.length) {
        currentWinds = [...targetWinds];
        return () => proc.cont(val);
      }
      return () => applyCPS(toRewind[i].inThunk, [], _ => {
        currentWinds = [...targetWinds.slice(0, common + i + 1)];
        return () => doRewind(i + 1);
      }, p, out);
    };
    return doUnwind(0);
  }
  if (proc.tag === 'native') {
    return k(proc.fn(args, p));
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
    if (proc.rest) callEnv.define(proc.rest, listToPairs(args.slice(proc.params.length)));
    return evalBodyCPS(proc.body, 0, callEnv, k, out);
  }
  if (proc.tag === 'builtin') {
    if (proc.name === 'call/cc' || proc.name === 'call-with-current-continuation') {
      if (args.length !== 1) throw posError(`${proc.name}: need exactly one arg`, p);
      const capturedWinds = [...currentWinds];
      const contVal: SchemeVal = { tag: 'continuation', cont: k, winds: capturedWinds };
      return () => applyCPS(args[0], [contVal], k, p, out);
    }
    if (proc.name === 'dynamic-wind') {
      if (args.length !== 3) throw posError('dynamic-wind: need exactly three args', p);
      const [inThunk, bodyThunk, outThunk] = args;
      const entry: WindEntry = { inThunk, outThunk };
      return () => applyCPS(inThunk, [], _ => {
        currentWinds = [...currentWinds, entry];
        return () => applyCPS(bodyThunk, [], bodyResult => {
          currentWinds = currentWinds.slice(0, -1);
          return () => applyCPS(outThunk, [], _ => k(bodyResult), p, out);
        }, p, out);
      }, p, out);
    }
    if (proc.name === 'raise') {
      if (args.length !== 1) throw posError('raise: need exactly one arg', p);
      if (exceptionHandlerStack.length === 0) {
        throw new EvalError(`unhandled exception: ${displayVal(args[0])}`);
      }
      const handler = exceptionHandlerStack.pop()!;
      return () => handler(args[0]);
    }
    if (proc.name === 'with-exception-handler') {
      if (args.length !== 2) throw posError('with-exception-handler: need exactly two args', p);
      const [handlerProc, thunk] = args;
      const cpsHandler = (exnVal: SchemeVal): Bounce => {
        return () => applyCPS(handlerProc, [exnVal], _ => {
          throw new EvalError('raise: handler returned');
        }, p, out);
      };
      exceptionHandlerStack.push(cpsHandler);
      return () => applyCPS(thunk, [], result => {
        exceptionHandlerStack.pop();
        return k(result);
      }, p, out);
    }
    if (proc.name === 'values') {
      if (args.length === 1) return k(args[0]);
      return k({ tag: 'values', vals: args });
    }
    if (proc.name === 'call-with-values') {
      if (args.length !== 2) throw posError('call-with-values: need exactly two args', p);
      const [producer, consumer] = args;
      return () => applyCPS(producer, [], result => {
        const consumerArgs = (result.tag === 'values') ? result.vals : [result];
        return () => applyCPS(consumer, consumerArgs, k, p, out);
      }, p, out);
    }
    if (proc.name === 'map') {
      if (args.length < 2) throw posError('map: need at least two args', p);
      const fn = args[0];
      const listArrays = args.slice(1).map(l => toArrayChecked(l, 'map', p));
      const len = listArrays[0].length;
      const mapLoop = (i: number, acc: SchemeVal[]): Bounce => {
        if (i >= len) return k(listToPairs(acc));
        const callArgs = listArrays.map(la => la[i]);
        return () => applyCPS(fn, callArgs, val => {
          return () => mapLoop(i + 1, [...acc, val]);
        }, p, out);
      };
      return mapLoop(0, []);
    }
    if (proc.name === 'for-each') {
      if (args.length < 2) throw posError('for-each: need at least two args', p);
      const fn = args[0];
      const listArrays = args.slice(1).map(l => toArrayChecked(l, 'for-each', p));
      const len = listArrays[0].length;
      const feLoop = (i: number): Bounce => {
        if (i >= len) return k({ tag: 'boolean', val: false });
        const callArgs = listArrays.map(la => la[i]);
        return () => applyCPS(fn, callArgs, _ => {
          return () => feLoop(i + 1);
        }, p, out);
      };
      return feLoop(0);
    }
    if (proc.name === 'apply') {
      if (args.length < 2) throw posError('apply: need at least two args', p);
      const applyProc = args[0];
      const lastArg = args[args.length - 1];
      const lastArr = toArrayChecked(lastArg, 'apply', p);
      const allArgs = [...args.slice(1, -1), ...lastArr];
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
        if (clause.val.length < 1) throw posError('cond: bad clause', p);
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

    if (op === 'let*') {
      if (args.length < 2) throw posError('let*: bad syntax', p);
      const lsBindings = args[0];
      const lsBody = args.slice(1);
      if (lsBindings.tag !== 'list') throw posError('let*: bad bindings', p);
      const lsEnv = new Env(env);
      const lsBs = lsBindings.val;
      const evalLsBindings = (i: number): Bounce => {
        if (i >= lsBs.length) return evalBodyCPS(lsBody, 0, lsEnv, k, out);
        const b = lsBs[i];
        if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
          throw posError('let*: bad binding', p);
        const nm = b.val[0].val;
        return evalCPS(b.val[1], lsEnv, val => {
          lsEnv.define(nm, val);
          return () => evalLsBindings(i + 1);
        }, out);
      };
      return evalLsBindings(0);
    }

    if (op === 'letrec') {
      if (args.length < 2) throw posError('letrec: bad syntax', p);
      const bindingsExpr = args[0];
      const body = args.slice(1);
      if (bindingsExpr.tag !== 'list') throw posError('letrec: bad bindings', p);
      const letEnv = new Env(env);
      const paramNames: string[] = [];
      const initExprs: SchemeVal[] = [];
      for (const b of bindingsExpr.val) {
        if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
          throw posError('letrec: bad binding', p);
        paramNames.push(b.val[0].val);
        initExprs.push(b.val[1]);
        letEnv.define(b.val[0].val, { tag: 'boolean', val: false });
      }
      return evalListCPS(initExprs, letEnv, initVals => {
        for (let i = 0; i < paramNames.length; i++) letEnv.set(paramNames[i], initVals[i]);
        return evalBodyCPS(body, 0, letEnv, k, out);
      }, out);
    }

    if (op === 'letrec*') {
      if (args.length < 2) throw posError('letrec*: bad syntax', p);
      const bindingsExpr = args[0];
      const body = args.slice(1);
      if (bindingsExpr.tag !== 'list') throw posError('letrec*: bad bindings', p);
      const letEnv = new Env(env);
      for (const b of bindingsExpr.val) {
        if (b.tag !== 'list' || b.val.length !== 2 || b.val[0].tag !== 'symbol')
          throw posError('letrec*: bad binding', p);
        letEnv.define(b.val[0].val, { tag: 'boolean', val: false });
      }
      const bindings = bindingsExpr.val;
      const evalBindings = (i: number): Bounce => {
        if (i >= bindings.length) return evalBodyCPS(body, 0, letEnv, k, out);
        const b = bindings[i];
        if (b.tag !== 'list') throw posError('letrec*: bad binding', p);
        const name = (b.val[0] as SchemeVal & { tag: 'symbol' }).val;
        return evalCPS(b.val[1], letEnv, val => {
          letEnv.set(name, val);
          return () => evalBindings(i + 1);
        }, out);
      };
      return evalBindings(0);
    }

    if (op === 'case') {
      if (args.length < 1) throw posError('case: bad syntax', p);
      const keyExpr = args[0];
      const clauses = args.slice(1);
      return evalCPS(keyExpr, env, keyVal => {
        const evalClauses = (i: number): Bounce => {
          if (i >= clauses.length) return k({ tag: 'boolean', val: false });
          const clause = clauses[i];
          if (clause.tag !== 'list' || clause.val.length < 2)
            throw posError('case: bad clause', p);
          const datums = clause.val[0];
          if (datums.tag === 'symbol' && datums.val === 'else') {
            return evalBodyCPS(clause.val, 1, env, k, out);
          }
          if (datums.tag !== 'list') throw posError('case: expected datum list', p);
          for (const d of datums.val) {
            if (schemeEqv(keyVal, d)) {
              return evalBodyCPS(clause.val, 1, env, k, out);
            }
          }
          return () => evalClauses(i + 1);
        };
        return evalClauses(0);
      }, out);
    }

    if (op === 'do') {
      if (args.length < 2) throw posError('do: bad syntax', p);
      const varSpecs = args[0];
      const testClause = args[1];
      const commands = args.slice(2);
      if (varSpecs.tag !== 'list') throw posError('do: bad variable specs', p);
      if (testClause.tag !== 'list' || testClause.val.length < 1)
        throw posError('do: bad test clause', p);

      const varNames: string[] = [];
      const initExprs: SchemeVal[] = [];
      const stepExprs: (SchemeVal | null)[] = [];
      for (const spec of varSpecs.val) {
        if (spec.tag !== 'list' || spec.val.length < 2 || spec.val.length > 3 || spec.val[0].tag !== 'symbol')
          throw posError('do: bad variable spec', p);
        varNames.push(spec.val[0].val);
        initExprs.push(spec.val[1]);
        stepExprs.push(spec.val.length === 3 ? spec.val[2] : null);
      }

      const testExpr = testClause.val[0];
      const resultExprs = testClause.val.slice(1);

      return evalListCPS(initExprs, env, initVals => {
        const doEnv = new Env(env);
        for (let i = 0; i < varNames.length; i++) doEnv.define(varNames[i], initVals[i]);

        const doLoop = (): Bounce => {
          return evalCPS(testExpr, doEnv, testVal => {
            if (isTruthy(testVal)) {
              if (resultExprs.length === 0) return k({ tag: 'boolean', val: false });
              return evalBodyCPS(resultExprs, 0, doEnv, k, out);
            }
            // Execute commands (body)
            const runCommands = (ci: number): Bounce => {
              if (ci >= commands.length) {
                // Evaluate all step expressions with current values, then update
                const stepsToEval: { idx: number; expr: SchemeVal }[] = [];
                for (let i = 0; i < varNames.length; i++) {
                  if (stepExprs[i] !== null) stepsToEval.push({ idx: i, expr: stepExprs[i]! });
                }
                if (stepsToEval.length === 0) return () => doLoop();
                const stepExprList = stepsToEval.map(s => s.expr);
                return evalListCPS(stepExprList, doEnv, stepVals => {
                  for (let j = 0; j < stepsToEval.length; j++) {
                    doEnv.set(varNames[stepsToEval[j].idx], stepVals[j]);
                  }
                  return () => doLoop();
                }, out);
              }
              return evalCPS(commands[ci], doEnv, _ => () => runCommands(ci + 1), out);
            };
            return runCommands(0);
          }, out);
        };
        return doLoop();
      }, out);
    }

    if (op === 'define-syntax') {
      if (args.length !== 2) throw posError('define-syntax: bad syntax', p);
      if (args[0].tag !== 'symbol') throw posError('define-syntax: expected symbol', p);
      const macroName = args[0].val;
      const transformer = args[1];
      // syntax-rules form
      if (transformer.tag === 'list' && transformer.val.length >= 2 &&
          transformer.val[0].tag === 'symbol' && transformer.val[0].val === 'syntax-rules') {
        const srArgs = transformer.val.slice(1);
        if (srArgs[0].tag !== 'list') throw posError('syntax-rules: expected literals list', p);
        const literals = new Set(srArgs[0].val.map(v => {
          if (v.tag !== 'symbol') throw posError('syntax-rules: literal must be symbol', p);
          return v.val;
        }));
        const rules: MacroRule[] = [];
        for (let i = 1; i < srArgs.length; i++) {
          const rule = srArgs[i];
          if (rule.tag !== 'list' || rule.val.length !== 2) throw posError('syntax-rules: bad rule', p);
          const pat = rule.val[0];
          if (pat.tag !== 'list') throw posError('syntax-rules: pattern must be list', p);
          rules.push({ pattern: pat.val.slice(1), template: rule.val[1] });
        }
        const macroVal: SchemeVal = { tag: 'macro', rules, literals, defEnv: env };
        env.define(macroName, macroVal);
        return k(macroVal);
      }
      // lambda/procedure transformer (for syntax-case macros)
      return evalCPS(transformer, env, proc => {
        const macroVal: SchemeVal = { tag: 'transformer-macro', proc, defEnv: env };
        env.define(macroName, macroVal);
        return k(macroVal);
      }, out);
    }

    if (op === 'syntax-case') {
      // (syntax-case expr (literals) clause ...)
      // clause = (pattern output-expr) or (pattern fender output-expr)
      if (args.length < 2) throw posError('syntax-case: bad syntax', p);
      const exprForm = args[0];
      const literalsList = args[1];
      if (literalsList.tag !== 'list') throw posError('syntax-case: expected literals list', p);
      const literals = new Set(literalsList.val.map(v => {
        if (v.tag !== 'symbol') throw posError('syntax-case: literal must be symbol', p);
        return v.val;
      }));
      const clauses = args.slice(2);
      return evalCPS(exprForm, env, stxVal => {
        // Convert stxVal to a list of elements for matching
        const inputElems: SchemeVal[] = stxVal.tag === 'list' ? stxVal.val : [stxVal];
        for (const clause of clauses) {
          if (clause.tag !== 'list' || clause.val.length < 2)
            throw posError('syntax-case: bad clause', p);
          const pat = clause.val[0];
          if (pat.tag !== 'list') throw posError('syntax-case: pattern must be list', p);
          const patElems = pat.val.slice(0); // full pattern including _ placeholder
          const bindings: MacroBindings = new Map();
          // Match: first element is _ (matches anything), rest are the pattern
          if (matchPatternList(patElems.slice(1), inputElems.slice(1), literals, bindings)) {
            // Determine if there's a fender
            const hasFender = clause.val.length === 3;
            const outputExpr = hasFender ? clause.val[2] : clause.val[1];
            // Find the defEnv for hygiene — use the closest transformer-macro env
            const defEnv = env;
            syntaxBindingsStack.push({ bindings, literals, defEnv });
            if (hasFender) {
              // TODO: evaluate fender; for now just use the output
            }
            return evalCPS(outputExpr, env, result => {
              syntaxBindingsStack.pop();
              return k(result);
            }, out);
          }
        }
        throw posError('syntax-case: no matching pattern', p);
      }, out);
    }

    if (op === 'syntax') {
      // (syntax template) — instantiate template with current syntax-case bindings
      if (args.length !== 1) throw posError('syntax: expected one argument', p);
      const tmpl = args[0];
      if (syntaxBindingsStack.length === 0) throw posError('syntax: not in syntax-case context', p);
      const { bindings, defEnv } = syntaxBindingsStack[syntaxBindingsStack.length - 1];
      // Collect introduced symbols for hygiene renaming
      const patVars = new Set(bindings.keys());
      const renames = new Map<string, string>();
      const collectIntroduced = (t: SchemeVal) => {
        if (t.tag === 'symbol' && !patVars.has(t.val) && !SPECIAL_FORMS.has(t.val) && t.val !== '...') {
          if (!renames.has(t.val)) renames.set(t.val, gensym(t.val));
        }
        if (t.tag === 'list') {
          // Skip quoted forms — symbols inside (quote ...) are data, not identifiers
          if (t.val.length >= 1 && t.val[0].tag === 'symbol' && t.val[0].val === 'quote') return;
          for (const el of t.val) collectIntroduced(el);
        }
      };
      collectIntroduced(tmpl);
      const expanded = instantiate(tmpl, bindings, renames);
      // Store renames for the transformer-macro handler to pre-bind in the calling env
      for (const [origName, gsName] of renames) {
        pendingSyntaxRenames.push({ origName, gsName, defEnv });
      }
      return k(expanded);
    }

    if (op === 'with-syntax') {
      // (with-syntax ((pattern expr) ...) body ...)
      if (args.length < 2) throw posError('with-syntax: bad syntax', p);
      const bindingSpecs = args[0];
      if (bindingSpecs.tag !== 'list') throw posError('with-syntax: expected bindings list', p);
      const bodyExprs = args.slice(1);
      const allBindings: MacroBindings = new Map();
      // Copy current syntax bindings if any
      if (syntaxBindingsStack.length > 0) {
        const cur = syntaxBindingsStack[syntaxBindingsStack.length - 1];
        for (const [k2, v] of cur.bindings) allBindings.set(k2, v);
      }
      const evalBindingSpecs = (i: number): Bounce => {
        if (i >= bindingSpecs.val.length) {
          syntaxBindingsStack.push({ bindings: allBindings, literals: new Set(), defEnv: env });
          return evalBodyCPS(bodyExprs, 0, env, result => {
            syntaxBindingsStack.pop();
            return k(result);
          }, out);
        }
        const spec = bindingSpecs.val[i];
        if (spec.tag !== 'list' || spec.val.length !== 2) throw posError('with-syntax: bad binding', p);
        const pat = spec.val[0];
        const expr = spec.val[1];
        return evalCPS(expr, env, val => {
          // Match pattern against val
          if (pat.tag === 'symbol') {
            allBindings.set(pat.val, val);
          } else if (pat.tag === 'list') {
            const patElems = pat.val;
            const valElems = val.tag === 'list' ? val.val : [val];
            const bindings: MacroBindings = new Map();
            if (matchPatternList(patElems, valElems, new Set(), bindings)) {
              for (const [bk, bv] of bindings) allBindings.set(bk, bv);
            }
          }
          return () => evalBindingSpecs(i + 1);
        }, out);
      };
      return evalBindingSpecs(0);
    }

    if (op === 'define-record-type') {
      // (define-record-type <name> (ctor field ...) pred? (field accessor) ...)
      if (args.length < 3) throw posError('define-record-type: bad syntax', p);
      const typeId = Symbol('record-type');
      const typeName = args[0].tag === 'symbol' ? args[0].val : '';
      const ctorSpec = args[1];
      if (ctorSpec.tag !== 'list' || ctorSpec.val.length < 1)
        throw posError('define-record-type: bad constructor spec', p);
      const ctorName = ctorSpec.val[0].tag === 'symbol' ? ctorSpec.val[0].val : '';
      const ctorFields = ctorSpec.val.slice(1).map(f => {
        if (f.tag !== 'symbol') throw posError('define-record-type: field must be symbol', p);
        return f.val;
      });
      const predName = args[2].tag === 'symbol' ? args[2].val : '';

      // Constructor
      env.define(ctorName, { tag: 'native', fn: (cArgs: SchemeVal[]) => {
        if (cArgs.length !== ctorFields.length)
          throw posError(`${ctorName}: expected ${ctorFields.length} args, got ${cArgs.length}`, p);
        const fields = new Map<string, SchemeVal>();
        for (let i = 0; i < ctorFields.length; i++) fields.set(ctorFields[i], cArgs[i]);
        return { tag: 'record' as const, typeId, typeName, fields };
      }});

      // Predicate
      env.define(predName, { tag: 'native', fn: (cArgs: SchemeVal[]) => {
        if (cArgs.length !== 1) throw posError(`${predName}: expected 1 arg`, p);
        return { tag: 'boolean' as const, val: cArgs[0].tag === 'record' && cArgs[0].typeId === typeId };
      }});

      // Field accessors
      for (let i = 3; i < args.length; i++) {
        const spec = args[i];
        if (spec.tag !== 'list' || spec.val.length < 2)
          throw posError('define-record-type: bad field spec', p);
        const fieldName = spec.val[0].tag === 'symbol' ? spec.val[0].val : '';
        const accessorName = spec.val[1].tag === 'symbol' ? spec.val[1].val : '';
        env.define(accessorName, { tag: 'native', fn: ((fn: string) => (cArgs: SchemeVal[]) => {
          if (cArgs.length !== 1) throw posError(`${accessorName}: expected 1 arg`, p);
          const rec = cArgs[0];
          if (rec.tag !== 'record' || rec.typeId !== typeId)
            throw posError(`${accessorName}: not a ${typeName}`, p);
          return rec.fields.get(fn)!;
        })(fieldName) });
      }

      return k({ tag: 'boolean', val: false });
    }

    if (op === 'guard') {
      if (args.length < 2) throw posError('guard: bad syntax', p);
      const guardSpec = args[0];
      if (guardSpec.tag !== 'list' || guardSpec.val.length < 1)
        throw posError('guard: bad syntax', p);
      const varSym = guardSpec.val[0];
      if (varSym.tag !== 'symbol') throw posError('guard: expected variable', p);
      const clauses = guardSpec.val.slice(1);
      const body = args.slice(1);

      const guardWinds = [...currentWinds];

      const handler = (exnVal: SchemeVal): Bounce => {
        // Unwind dynamic-winds back to guard point
        const targetWinds = guardWinds;
        let common = 0;
        while (common < currentWinds.length && common < targetWinds.length
               && currentWinds[common] === targetWinds[common]) {
          common++;
        }
        const toUnwind = currentWinds.slice(common).reverse();

        const doUnwind = (i: number): Bounce => {
          if (i >= toUnwind.length) {
            currentWinds = [...targetWinds];
            return () => evalGuardClauses();
          }
          currentWinds = currentWinds.slice(0, currentWinds.length - 1);
          return () => applyCPS(toUnwind[i].outThunk, [], _ => () => doUnwind(i + 1), p, out);
        };

        const evalGuardClauses = (): Bounce => {
          const clauseEnv = new Env(env);
          clauseEnv.define(varSym.val, exnVal);

          const tryClause = (i: number): Bounce => {
            if (i >= clauses.length) {
              // No clause matched, re-raise
              if (exceptionHandlerStack.length === 0) {
                throw new EvalError(`unhandled exception: ${displayVal(exnVal)}`);
              }
              const prevHandler = exceptionHandlerStack.pop()!;
              return () => prevHandler(exnVal);
            }
            const clause = clauses[i];
            if (clause.tag !== 'list' || clause.val.length < 1)
              throw posError('guard: bad clause', p);
            const test = clause.val[0];
            if (test.tag === 'symbol' && test.val === 'else') {
              if (clause.val.length < 2) return k({ tag: 'boolean', val: false });
              return evalBodyCPS(clause.val, 1, clauseEnv, k, out);
            }
            return evalCPS(test, clauseEnv, condVal => {
              if (isTruthy(condVal)) {
                if (clause.val.length < 2) return k(condVal);
                return evalBodyCPS(clause.val, 1, clauseEnv, k, out);
              }
              return () => tryClause(i + 1);
            }, out);
          };
          return tryClause(0);
        };

        return doUnwind(0);
      };

      exceptionHandlerStack.push(handler);
      return evalBodyCPS(body, 0, env, bodyVal => {
        exceptionHandlerStack.pop();
        return () => k(bodyVal);
      }, out);
    }

    // Check for macro application
    {
      let macroVal: SchemeVal | undefined;
      try { macroVal = env.get(op); } catch (_) { /* unbound */ }
      if (macroVal && macroVal.tag === 'macro') {
        const expanded = expandMacro(macroVal, items, env);
        return evalCPS(expanded, env, k, out);
      }
      if (macroVal && macroVal.tag === 'transformer-macro') {
        // Call the transformer procedure with the full form as a list
        const form: SchemeVal = { tag: 'list', val: items, pos: p };
        pendingSyntaxRenames = [];
        return () => applyCPS(macroVal.proc, [form], expanded => {
          // Pre-bind gensyms in the calling env for hygiene
          for (const { origName, gsName, defEnv } of pendingSyntaxRenames) {
            try {
              const val = defEnv.get(origName);
              env.define(gsName, val);
            } catch (_) { /* fresh introduced binding */ }
          }
          pendingSyntaxRenames = [];
          return evalCPS(expanded, env, k, out);
        }, p, out);
      }
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
  env.define('map', { tag: 'builtin', name: 'map' });
  env.define('for-each', { tag: 'builtin', name: 'for-each' });
  env.define('call/cc', { tag: 'builtin', name: 'call/cc' });
  env.define('call-with-current-continuation', { tag: 'builtin', name: 'call-with-current-continuation' });
  env.define('dynamic-wind', { tag: 'builtin', name: 'dynamic-wind' });
  env.define('raise', { tag: 'builtin', name: 'raise' });
  env.define('with-exception-handler', { tag: 'builtin', name: 'with-exception-handler' });
  env.define('values', { tag: 'builtin', name: 'values' });
  env.define('call-with-values', { tag: 'builtin', name: 'call-with-values' });
  for (const name of BUILTINS) {
    env.define(name, { tag: 'builtin', name });
  }
  // Generate all c[ad]{3,4}r combinations as native functions
  const ops = ['a', 'd'];
  const makeCxr = (pattern: string): ((v: SchemeVal) => SchemeVal) => {
    return (v: SchemeVal) => {
      let cur = v;
      for (let ci = pattern.length - 1; ci >= 0; ci--) {
        cur = pattern[ci] === 'a' ? getCar(cur, 'c' + pattern + 'r') : getCdr(cur, 'c' + pattern + 'r');
      }
      return cur;
    };
  };
  // 3-letter combinations
  for (const a of ops) for (const b of ops) for (const c of ops) {
    const pat = a + b + c;
    const name = 'c' + pat + 'r';
    const fn = makeCxr(pat);
    env.define(name, { tag: 'native', fn: (args: SchemeVal[]) => fn(args[0]) });
  }
  // 4-letter combinations
  for (const a of ops) for (const b of ops) for (const c of ops) for (const d of ops) {
    const pat = a + b + c + d;
    const name = 'c' + pat + 'r';
    const fn = makeCxr(pat);
    env.define(name, { tag: 'native', fn: (args: SchemeVal[]) => fn(args[0]) });
  }
  return env;
}

// ── Public API ──────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  currentWinds = [];
  exceptionHandlerStack = [];
  syntaxBindingsStack = [];
  pendingSyntaxRenames = [];
  const env = makeGlobalEnv();
  const result = trampoline(evalBodyCPS(exprs, 0, env, v => v));
  return displayVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  currentWinds = [];
  exceptionHandlerStack = [];
  syntaxBindingsStack = [];
  pendingSyntaxRenames = [];
  const env = makeGlobalEnv();
  const out: string[] = [];
  const result = trampoline(evalBodyCPS(exprs, 0, env, v => v, out));
  return { result: displayVal(result), output: out.join('') };
}
