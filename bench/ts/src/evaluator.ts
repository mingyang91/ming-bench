import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type Cont = (val: SchemeVal) => Bounce;
type Bounce = SchemeVal | (() => Bounce);

interface WindEntry { inThunk: SchemeVal; outThunk: SchemeVal; }
let currentWinds: WindEntry[] = [];
let exceptionHandlerStack: ((val: SchemeVal) => Bounce)[] = [];

type SchemeVal =
  | { tag: 'number'; val: number; pos?: Pos }
  | { tag: 'boolean'; val: boolean; pos?: Pos }
  | { tag: 'string'; val: string; immutable?: boolean; pos?: Pos }
  | { tag: 'char'; val: string; pos?: Pos }
  | { tag: 'symbol'; val: string; pos?: Pos }
  | { tag: 'list'; val: SchemeVal[]; pos?: Pos }
  | { tag: 'lambda'; params: string[]; rest?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'continuation'; cont: Cont; winds: WindEntry[]; pos?: Pos }
  | { tag: 'macro'; rules: MacroRule[]; literals: Set<string>; defEnv: Env; pos?: Pos }
  | { tag: 'vector'; val: SchemeVal[]; pos?: Pos }
  | { tag: 'values'; vals: SchemeVal[]; pos?: Pos };

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
    case 'macro':
      return '#<macro>';
    case 'vector':
      return '#(' + v.val.map(displayVal).join(' ') + ')';
    case 'values':
      return v.vals.map(displayVal).join('\n');
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

// ── Hygienic Macros ─────────────────────────────────────────────────

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'set!', 'lambda', 'and', 'or', 'begin',
  'cond', 'let', 'letrec', 'letrec*', 'case', 'do',
  'define-syntax', 'syntax-rules', 'else',
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
      // Dotted pair: (x . y) is stored as [x, '.', y] — cdr returns y
      if (a.val.length === 3 && a.val[1].tag === 'symbol' && a.val[1].val === '.') {
        return a.val[2];
      }
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
    case 'reverse': {
      if (evalArgs.length !== 1) throw posError('reverse: need exactly one arg', p);
      if (evalArgs[0].tag !== 'list') throw posError('reverse: not a list', p);
      return { tag: 'list', val: [...evalArgs[0].val].reverse() };
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
      const chars = [...evalArgs[0].val].map(c => ({ tag: 'char' as const, val: c }));
      return { tag: 'list', val: chars };
    }
    case 'list->string': {
      if (evalArgs.length !== 1) throw posError('list->string: need exactly one arg', p);
      if (evalArgs[0].tag !== 'list') throw posError('list->string: expected list', p);
      let str = '';
      for (const el of evalArgs[0].val) {
        if (el.tag !== 'char') throw posError('list->string: expected char in list', p);
        str += el.val;
      }
      return { tag: 'string', val: str };
    }
    case 'char->integer': {
      if (evalArgs.length !== 1) throw posError('char->integer: need exactly one arg', p);
      if (evalArgs[0].tag !== 'char') throw posError('char->integer: expected char', p);
      return { tag: 'number', val: evalArgs[0].val.codePointAt(0)! };
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
    // ── L13 List builtins ──
    case 'list-ref': {
      if (evalArgs.length !== 2) throw posError('list-ref: need exactly two args', p);
      if (evalArgs[0].tag !== 'list') throw posError('list-ref: expected list', p);
      const idx = toNumber(evalArgs[1], 'list-ref', p);
      const lst = evalArgs[0].val;
      if (idx < 0 || idx >= lst.length) throw posError('list-ref: index out of range', p);
      return lst[idx];
    }
    case 'list-tail': {
      if (evalArgs.length !== 2) throw posError('list-tail: need exactly two args', p);
      if (evalArgs[0].tag !== 'list') throw posError('list-tail: expected list', p);
      const idx = toNumber(evalArgs[1], 'list-tail', p);
      const lst = evalArgs[0].val;
      if (idx < 0 || idx > lst.length) throw posError('list-tail: index out of range', p);
      return { tag: 'list', val: lst.slice(idx) };
    }
    case 'list?': {
      if (evalArgs.length !== 1) throw posError('list?: need exactly one arg', p);
      const v = evalArgs[0];
      if (v.tag !== 'list') return { tag: 'boolean', val: false };
      // Check for proper list (no dotted pair)
      const items = v.val;
      if (items.length >= 3 && items[items.length - 2].tag === 'symbol' && (items[items.length - 2] as any).val === '.') {
        return { tag: 'boolean', val: false };
      }
      return { tag: 'boolean', val: true };
    }
    case 'assoc': {
      if (evalArgs.length !== 2) throw posError('assoc: need exactly two args', p);
      const key = evalArgs[0];
      const alist = evalArgs[1];
      if (alist.tag !== 'list') throw posError('assoc: expected list', p);
      for (const pair of alist.val) {
        if (pair.tag !== 'list' || pair.val.length === 0) continue;
        if (schemeEqual(key, pair.val[0])) return pair;
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
      return { tag: 'list', val: [...evalArgs[0].val] };
    }
    case 'list->vector': {
      if (evalArgs.length !== 1) throw posError('list->vector: need exactly one arg', p);
      if (evalArgs[0].tag !== 'list') throw posError('list->vector: expected list', p);
      return { tag: 'vector', val: [...evalArgs[0].val] };
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

function schemeEqual(a: SchemeVal, b: SchemeVal): boolean {
  if (a.tag !== b.tag) return false;
  if (a.tag === 'number' && b.tag === 'number') return a.val === b.val;
  if (a.tag === 'boolean' && b.tag === 'boolean') return a.val === b.val;
  if (a.tag === 'string' && b.tag === 'string') return a.val === b.val;
  if (a.tag === 'char' && b.tag === 'char') return a.val === b.val;
  if (a.tag === 'symbol' && b.tag === 'symbol') return a.val === b.val;
  if (a.tag === 'list' && b.tag === 'list') {
    if (a.val.length !== b.val.length) return false;
    for (let i = 0; i < a.val.length; i++) {
      if (!schemeEqual(a.val[i], b.val[i])) return false;
    }
    return true;
  }
  if (a.tag === 'vector' && b.tag === 'vector') {
    if (a.val.length !== b.val.length) return false;
    for (let i = 0; i < a.val.length; i++) {
      if (!schemeEqual(a.val[i], b.val[i])) return false;
    }
    return true;
  }
  return false;
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
  'vector->list', 'list->vector', 'reverse']);

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
    const val = args[0];
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
      const lists = args.slice(1);
      for (const l of lists) {
        if (l.tag !== 'list') throw posError('map: expected list', p);
      }
      const len = (lists[0] as SchemeVal & { tag: 'list' }).val.length;
      const mapLoop = (i: number, acc: SchemeVal[]): Bounce => {
        if (i >= len) return k({ tag: 'list', val: acc });
        const callArgs = lists.map(l => (l as SchemeVal & { tag: 'list' }).val[i]);
        return () => applyCPS(fn, callArgs, val => {
          return () => mapLoop(i + 1, [...acc, val]);
        }, p, out);
      };
      return mapLoop(0, []);
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
      if (transformer.tag !== 'list' || transformer.val.length < 2 ||
          transformer.val[0].tag !== 'symbol' || transformer.val[0].val !== 'syntax-rules') {
        throw posError('define-syntax: expected syntax-rules', p);
      }
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
        return k(bodyVal);
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
  return env;
}

// ── Public API ──────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  currentWinds = [];
  exceptionHandlerStack = [];
  const env = makeGlobalEnv();
  const result = trampoline(evalBodyCPS(exprs, 0, env, v => v));
  return displayVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parse(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  currentWinds = [];
  exceptionHandlerStack = [];
  const env = makeGlobalEnv();
  const out: string[] = [];
  const result = trampoline(evalBodyCPS(exprs, 0, env, v => v, out));
  return { result: displayVal(result), output: out.join('') };
}
