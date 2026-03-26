import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

// CPS types
type Cont = (val: SchemeVal) => Bounce;
type Bounce = { tag: 'done'; value: SchemeVal } | { tag: 'bounce'; fn: () => Bounce };

function done(v: SchemeVal): Bounce { return { tag: 'done', value: v }; }
function bounce(fn: () => Bounce): Bounce { return { tag: 'bounce', fn }; }

function trampoline(b: Bounce): SchemeVal {
  while (b.tag === 'bounce') b = b.fn();
  return b.value;
}

type SchemeVal =
  | { tag: 'number'; value: number; exact?: boolean; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; immutable?: boolean; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }  // parse-time only
  | { tag: 'lambda'; params: string[]; restParam?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'macro'; rules: MacroRule[]; defEnv: Env; pos?: Pos }
  | { tag: 'record'; typeName: string; typeId: symbol; fields: Map<string, SchemeVal>; pos?: Pos }
  | { tag: 'case-lambda'; clauses: { params: string[]; restParam?: string; body: SchemeVal[]; env: Env }[]; pos?: Pos }
  | { tag: 'vector'; elements: SchemeVal[]; pos?: Pos }
  | { tag: 'continuation'; k: Cont; windStack: WindEntry[]; pos?: Pos };

interface MacroRule {
  pattern: SchemeVal[];  // pattern elements (after macro name)
  template: SchemeVal;
  literals: string[];
}

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}: ` : '';
}

function errAt(msg: string, pos?: Pos): EvalError {
  return new EvalError(`${posStr(pos)}${msg}`);
}

const SCM_NIL: SchemeVal = { tag: 'nil' };
const SCM_TRUE: SchemeVal = { tag: 'boolean', value: true };
const SCM_FALSE: SchemeVal = { tag: 'boolean', value: false };

// ── Rational number helpers ──────────────────────────────────────────

function gcd(a: number, b: number): number {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { [a, b] = [b, a % b]; }
  return a;
}

function makeRational(num: number, den: number): SchemeVal {
  if (den === 0) throw new EvalError('division by zero');
  if (den < 0) { num = -num; den = -den; }
  const g = gcd(Math.abs(num), den);
  num = num / g; den = den / g;
  if (den === 1) return { tag: 'number', value: num, exact: true };
  return { tag: 'rational', num, den };
}

function isExact(val: SchemeVal): boolean {
  if (val.tag === 'rational') return true;
  if (val.tag === 'number') return val.exact ?? Number.isInteger(val.value);
  return false;
}

function isNumeric(val: SchemeVal): boolean {
  return val.tag === 'number' || val.tag === 'rational';
}

function toFloat(val: SchemeVal): number {
  if (val.tag === 'number') return val.value;
  if (val.tag === 'rational') return val.num / val.den;
  throw new EvalError('expected number');
}

// Convert to rational representation (num, den) — returns [num, den]
function toRational(val: SchemeVal): [number, number] {
  if (val.tag === 'rational') return [val.num, val.den];
  if (val.tag === 'number' && isExact(val)) return [val.value, 1];
  throw new EvalError('expected exact number');
}

function numericAdd(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (isExact(a) && isExact(b)) {
    const [an, ad] = toRational(a);
    const [bn, bd] = toRational(b);
    return makeRational(an * bd + bn * ad, ad * bd);
  }
  return { tag: 'number', value: toFloat(a) + toFloat(b), exact: false };
}

function numericSub(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (isExact(a) && isExact(b)) {
    const [an, ad] = toRational(a);
    const [bn, bd] = toRational(b);
    return makeRational(an * bd - bn * ad, ad * bd);
  }
  return { tag: 'number', value: toFloat(a) - toFloat(b), exact: false };
}

function numericMul(a: SchemeVal, b: SchemeVal): SchemeVal {
  if (isExact(a) && isExact(b)) {
    const [an, ad] = toRational(a);
    const [bn, bd] = toRational(b);
    return makeRational(an * bn, ad * bd);
  }
  return { tag: 'number', value: toFloat(a) * toFloat(b), exact: false };
}

function numericDiv(a: SchemeVal, b: SchemeVal, pos?: Pos): SchemeVal {
  if (isExact(a) && isExact(b)) {
    const [an, ad] = toRational(a);
    const [bn, bd] = toRational(b);
    if (bn === 0) throw errAt('division by zero', pos);
    return makeRational(an * bd, ad * bn);
  }
  const bv = toFloat(b);
  if (bv === 0) throw errAt('division by zero', pos);
  return { tag: 'number', value: toFloat(a) / bv, exact: false };
}

function numericCompare(a: SchemeVal, b: SchemeVal): number {
  // Use exact comparison when both are exact
  if (isExact(a) && isExact(b)) {
    const [an, ad] = toRational(a);
    const [bn, bd] = toRational(b);
    return an * bd - bn * ad;
  }
  return toFloat(a) - toFloat(b);
}

function requireNumeric(name: string, args: SchemeVal[], pos?: Pos): void {
  for (const a of args) {
    if (!isNumeric(a)) throw errAt(`${name}: expected number`, pos);
  }
}

function makePair(car: SchemeVal, cdr: SchemeVal): SchemeVal {
  return { tag: 'pair', car, cdr };
}

// Convert a JS array to a proper Scheme list (pair chain ending in nil)
function arrayToList(arr: SchemeVal[]): SchemeVal {
  let result: SchemeVal = SCM_NIL;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = makePair(arr[i], result);
  }
  return result;
}

// Convert a parse-time list to a runtime pair chain
function listToPairs(val: SchemeVal): SchemeVal {
  if (val.tag === 'list') {
    return arrayToList(val.elements.map(listToPairs));
  }
  return val;
}

// Convert a runtime pair chain to a JS array (returns null if improper)
function pairToArray(val: SchemeVal): SchemeVal[] | null {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag === 'nil') return result;
  return null; // improper list
}

// ── Gensym ────────────────────────────────────────────────────────────

let gensymCounter = 0;
function gensym(prefix: string): string {
  return `##${prefix}~${++gensymCounter}`;
}

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent?: Env) {}

  get(name: string): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
  }

  set(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }

  setExisting(name: string, val: SchemeVal): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, val);
      return;
    }
    if (this.parent) {
      this.parent.setExisting(name, val);
      return;
    }
    throw new EvalError(`set!: unbound variable: ${name}`);
  }
}

// ── Tokenizer ──────────────────────────────────────────────────────────

interface Token { text: string; pos: Pos }

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;

  function advance(): string {
    const ch = input[i++];
    if (ch === '\n') { line++; col = 1; } else { col++; }
    return ch;
  }

  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { advance(); continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') advance(); continue; }
    const startPos: Pos = { line, col };
    if (ch === '(' || ch === ')') { advance(); tokens.push({ text: ch, pos: startPos }); continue; }
    if (ch === '\'') { advance(); tokens.push({ text: "'", pos: startPos }); continue; }
    if (ch === '"') {
      let s = '"';
      advance();
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += advance(); }
        s += advance();
      }
      if (i < input.length) { s += '"'; advance(); }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    let tok = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      tok += advance();
    }
    tokens.push({ text: tok, pos: startPos });
  }
  return tokens;
}

// ── Parser ─────────────────────────────────────────────────────────────

function parse(tokens: Token[]): SchemeVal[] {
  let idx = 0;

  function parseExpr(): SchemeVal {
    if (idx >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[idx++];
    if (tok.text === "'") {
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === '(') {
      const elements: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elements.push(parseExpr());
      }
      if (idx >= tokens.length) throw new EvalError(`${tok.pos.line}:${tok.pos.col}: missing closing parenthesis`);
      idx++;
      return { tag: 'list', elements, pos: tok.pos };
    }
    if (tok.text === ')') throw new EvalError(`${tok.pos.line}:${tok.pos.col}: unexpected )`);
    return parseAtom(tok);
  }

  function parseAtom(tok: Token): SchemeVal {
    if (tok.text === '#t') return { tag: 'boolean', value: true, pos: tok.pos };
    if (tok.text === '#f') return { tag: 'boolean', value: false, pos: tok.pos };
    if (tok.text.startsWith('#\\')) {
      const rest = tok.text.slice(2);
      if (rest === 'space') return { tag: 'char', value: ' ', pos: tok.pos };
      if (rest === 'newline') return { tag: 'char', value: '\n', pos: tok.pos };
      if (rest === 'tab') return { tag: 'char', value: '\t', pos: tok.pos };
      if (rest.length === 1) return { tag: 'char', value: rest, pos: tok.pos };
      throw new EvalError(`${tok.pos.line}:${tok.pos.col}: unknown character literal: ${tok.text}`);
    }
    if (tok.text.startsWith('"') && tok.text.endsWith('"')) {
      return { tag: 'string', value: tok.text.slice(1, -1), immutable: true, pos: tok.pos };
    }
    // Rational literal: n/d (e.g. 1/3, -5/2)
    const ratMatch = /^(-?\d+)\/(\d+)$/.exec(tok.text);
    if (ratMatch) {
      const rn = parseInt(ratMatch[1], 10);
      const rd = parseInt(ratMatch[2], 10);
      const rv = makeRational(rn, rd);
      rv.pos = tok.pos;
      return rv;
    }
    const num = Number(tok.text);
    if (!isNaN(num) && tok.text !== '') {
      const hasDecimal = tok.text.includes('.') || tok.text.includes('e') || tok.text.includes('E');
      return { tag: 'number', value: num, exact: !hasDecimal, pos: tok.pos };
    }
    return { tag: 'symbol', value: tok.text, pos: tok.pos };
  }

  const exprs: SchemeVal[] = [];
  while (idx < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Evaluator helpers ──────────────────────────────────────────────────

function parseParams(elements: SchemeVal[], pos?: Pos): { params: string[]; restParam?: string } {
  const params: string[] = [];
  let restParam: string | undefined;
  for (let i = 0; i < elements.length; i++) {
    const p = elements[i];
    if (p.tag === 'symbol' && p.value === '.') {
      if (i + 1 >= elements.length) throw errAt('bad dot syntax in params', pos);
      const rest = elements[i + 1];
      if (rest.tag !== 'symbol') throw errAt('rest param must be symbol', pos);
      restParam = rest.value;
      break;
    }
    if (p.tag !== 'symbol') throw errAt('param must be symbol', pos);
    params.push(p.value);
  }
  return { params, restParam };
}

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

// ── Macros (syntax-rules) ─────────────────────────────────────────────

const SPECIAL_FORMS_SET = new Set([
  'quote', 'if', 'define', 'lambda', 'and', 'or', 'not', 'begin',
  'set!', 'cond', 'let', 'let*', 'letrec', 'letrec*', 'case', 'do',
  'define-syntax', 'syntax-rules',
]);

function isEllipsis(val: SchemeVal): boolean {
  return val.tag === 'symbol' && val.value === '...';
}

function matchPatternHelper(
  pattern: SchemeVal[],
  form: SchemeVal[],
  literals: string[],
  bindings: Map<string, SchemeVal | SchemeVal[]>,
): boolean {
  let pi = 0, fi = 0;
  while (pi < pattern.length) {
    if (pi + 1 < pattern.length && isEllipsis(pattern[pi + 1])) {
      const subPat = pattern[pi];
      pi += 2;
      const remainingPatterns = pattern.length - pi;
      const availableForEllipsis = form.length - fi - remainingPatterns;
      if (availableForEllipsis < 0) return false;
      if (subPat.tag === 'symbol' && !literals.includes(subPat.value)) {
        const matches: SchemeVal[] = [];
        for (let k = 0; k < availableForEllipsis; k++) matches.push(form[fi + k]);
        bindings.set(subPat.value, matches);
      }
      fi += availableForEllipsis;
      continue;
    }
    if (fi >= form.length) return false;
    if (!matchSingle(pattern[pi], form[fi], literals, bindings)) return false;
    pi++;
    fi++;
  }
  return fi === form.length;
}

function matchSingle(
  pattern: SchemeVal, form: SchemeVal, literals: string[],
  bindings: Map<string, SchemeVal | SchemeVal[]>,
): boolean {
  if (pattern.tag === 'symbol') {
    if (literals.includes(pattern.value))
      return form.tag === 'symbol' && form.value === pattern.value;
    if (pattern.value === '_') return true;
    bindings.set(pattern.value, form);
    return true;
  }
  if (pattern.tag === 'list') {
    if (form.tag !== 'list') return false;
    return matchPatternHelper(pattern.elements, form.elements, literals, bindings);
  }
  return false;
}

function matchPattern(
  pattern: SchemeVal[], form: SchemeVal[], literals: string[],
): Map<string, SchemeVal | SchemeVal[]> | null {
  const bindings = new Map<string, SchemeVal | SchemeVal[]>();
  if (!matchPatternHelper(pattern, form, literals, bindings)) return null;
  return bindings;
}

function collectFreeVars(template: SchemeVal, patVars: Set<string>, result: Set<string>): void {
  if (template.tag === 'symbol') {
    if (!patVars.has(template.value) && !SPECIAL_FORMS_SET.has(template.value) &&
        !BUILTINS.has(template.value) && template.value !== '...')
      result.add(template.value);
    return;
  }
  if (template.tag === 'list') {
    for (const e of template.elements) collectFreeVars(e, patVars, result);
  }
}

function findEllipsisVars(template: SchemeVal, bindings: Map<string, SchemeVal | SchemeVal[]>): string[] {
  const vars: string[] = [];
  if (template.tag === 'symbol') {
    if (Array.isArray(bindings.get(template.value))) vars.push(template.value);
    return vars;
  }
  if (template.tag === 'list') {
    for (const e of template.elements) vars.push(...findEllipsisVars(e, bindings));
  }
  return vars;
}

function expandTemplate(
  template: SchemeVal,
  bindings: Map<string, SchemeVal | SchemeVal[]>,
  renames: Map<string, string>,
): SchemeVal {
  if (template.tag === 'symbol') {
    const bound = bindings.get(template.value);
    if (bound !== undefined && !Array.isArray(bound)) return bound;
    const renamed = renames.get(template.value);
    if (renamed !== undefined) return { tag: 'symbol', value: renamed };
    return template;
  }
  if (template.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < template.elements.length; i++) {
      if (i + 1 < template.elements.length && isEllipsis(template.elements[i + 1])) {
        const subTemplate = template.elements[i];
        const ellipsisVars = findEllipsisVars(subTemplate, bindings);
        if (ellipsisVars.length > 0) {
          const count = (bindings.get(ellipsisVars[0]) as SchemeVal[]).length;
          for (let k = 0; k < count; k++) {
            const subBindings = new Map(bindings);
            for (const v of ellipsisVars) subBindings.set(v, (bindings.get(v) as SchemeVal[])[k]);
            result.push(expandTemplate(subTemplate, subBindings, renames));
          }
        }
        i++; // skip ...
        continue;
      }
      result.push(expandTemplate(template.elements[i], bindings, renames));
    }
    return { tag: 'list', elements: result, pos: template.pos };
  }
  return template;
}

function expandMacro(
  macro: SchemeVal & { tag: 'macro' }, form: SchemeVal[], env: Env, pos?: Pos,
): SchemeVal {
  for (const rule of macro.rules) {
    const bindings = matchPattern(rule.pattern, form.slice(1), rule.literals);
    if (bindings !== null) {
      const patVars = new Set(bindings.keys());
      const freeVars = new Set<string>();
      collectFreeVars(rule.template, patVars, freeVars);
      const renames = new Map<string, string>();
      for (const v of freeVars) renames.set(v, gensym(v));
      for (const [original, renamed] of renames) {
        try { env.set(renamed, macro.defEnv.get(original)); }
        catch (e) { /* macro-introduced binding, no injection needed */ }
      }
      return expandTemplate(rule.template, bindings, renames);
    }
  }
  throw errAt('no matching pattern in syntax-rules', pos);
}

// ── CPS Evaluator ──────────────────────────────────────────────────────

// Evaluate a sequence of expressions, passing the last result to k
function evalBeginK(exprs: SchemeVal[], env: Env, k: Cont): Bounce {
  if (exprs.length === 0) return k(SCM_FALSE);
  function loop(i: number): Bounce {
    if (i === exprs.length - 1) return bounce(() => evalK(exprs[i], env, k));
    return evalK(exprs[i], env, (_) => loop(i + 1));
  }
  return loop(0);
}

// Evaluate a list of expressions right-to-left (Chez Scheme order),
// returning the values array in left-to-right order
function evalListK(exprs: SchemeVal[], env: Env, k: (vals: SchemeVal[]) => Bounce): Bounce {
  function loop(i: number, acc: SchemeVal[]): Bounce {
    if (i < 0) return k(acc);
    return evalK(exprs[i], env, (val) => {
      return loop(i - 1, [val, ...acc]);
    });
  }
  return loop(exprs.length - 1, []);
}

function evalK(expr: SchemeVal, env: Env, k: Cont): Bounce {
  switch (expr.tag) {
    case 'number':
    case 'rational':
    case 'boolean':
    case 'string':
    case 'char':
    case 'nil':
    case 'pair':
    case 'vector':
      return k(expr);
    case 'symbol':
      try { return k(env.get(expr.value)); }
      catch (e) { throw errAt(`unbound variable: ${expr.value}`, expr.pos); }
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw errAt('empty application', expr.pos);

      const head = elems[0];

      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'quote': {
            if (elems.length !== 2) throw errAt('quote: expected 1 argument', expr.pos);
            return k(listToPairs(elems[1]));
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4)
              throw errAt('if: expected 2-3 arguments', expr.pos);
            return evalK(elems[1], env, (cond) => {
              if (isTruthy(cond)) return bounce(() => evalK(elems[2], env, k));
              if (elems.length === 4) return bounce(() => evalK(elems[3], env, k));
              return k(SCM_FALSE);
            });
          }
          case 'define': {
            if (elems.length < 3) throw errAt('define: bad syntax', expr.pos);
            const target = elems[1];
            if (target.tag === 'symbol') {
              return evalK(elems[2], env, (val) => {
                env.set(target.value, val);
                return k(val);
              });
            }
            if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
              const name = target.elements[0].value;
              const { params, restParam } = parseParams(target.elements.slice(1), expr.pos);
              const body = elems.slice(2);
              const lambda: SchemeVal = { tag: 'lambda', params, restParam, body, env };
              env.set(name, lambda);
              return k(lambda);
            }
            throw errAt('define: bad syntax', expr.pos);
          }
          case 'lambda': {
            if (elems.length < 3) throw errAt('lambda: bad syntax', expr.pos);
            const paramList = elems[1];
            if (paramList.tag === 'symbol') {
              const body = elems.slice(2);
              return k({ tag: 'lambda', params: [], restParam: paramList.value, body, env });
            }
            if (paramList.tag !== 'list') throw errAt('lambda: params must be a list', expr.pos);
            const { params, restParam } = parseParams(paramList.elements, expr.pos);
            const body = elems.slice(2);
            return k({ tag: 'lambda', params, restParam, body, env });
          }
          case 'case-lambda': {
            const clauses: { params: string[]; restParam?: string; body: SchemeVal[]; env: Env }[] = [];
            for (let i = 1; i < elems.length; i++) {
              const clause = elems[i];
              if (clause.tag !== 'list' || clause.elements.length < 2)
                throw errAt('case-lambda: bad clause', expr.pos);
              const paramList = clause.elements[0];
              if (paramList.tag === 'symbol') {
                clauses.push({ params: [], restParam: paramList.value, body: clause.elements.slice(1), env });
              } else if (paramList.tag === 'list') {
                const { params, restParam } = parseParams(paramList.elements, expr.pos);
                clauses.push({ params, restParam, body: clause.elements.slice(1), env });
              } else if (paramList.tag === 'nil') {
                clauses.push({ params: [], body: clause.elements.slice(1), env });
              } else {
                throw errAt('case-lambda: bad formals', expr.pos);
              }
            }
            return k({ tag: 'case-lambda' as const, clauses, pos: expr.pos });
          }
          case 'and': {
            if (elems.length === 1) return k(SCM_TRUE);
            function evalAnd(i: number): Bounce {
              if (i === elems.length - 1) return bounce(() => evalK(elems[i], env, k));
              return evalK(elems[i], env, (val) => {
                if (!isTruthy(val)) return k(val);
                return evalAnd(i + 1);
              });
            }
            return evalAnd(1);
          }
          case 'or': {
            if (elems.length === 1) return k(SCM_FALSE);
            function evalOr(i: number): Bounce {
              if (i === elems.length - 1) return bounce(() => evalK(elems[i], env, k));
              return evalK(elems[i], env, (val) => {
                if (isTruthy(val)) return k(val);
                return evalOr(i + 1);
              });
            }
            return evalOr(1);
          }
          case 'not': {
            if (elems.length !== 2) throw errAt('not: expected 1 argument', expr.pos);
            return evalK(elems[1], env, (val) =>
              k(isTruthy(val) ? SCM_FALSE : SCM_TRUE)
            );
          }
          case 'begin': {
            if (elems.length === 1) return k(SCM_FALSE);
            return evalBeginK(elems.slice(1), env, k);
          }
          case 'set!': {
            if (elems.length !== 3) throw errAt('set!: bad syntax', expr.pos);
            const target = elems[1];
            if (target.tag !== 'symbol') throw errAt('set!: expected symbol', expr.pos);
            return evalK(elems[2], env, (val) => {
              env.setExisting(target.value, val);
              return k(SCM_FALSE);
            });
          }
          case 'cond': {
            function tryCond(ci: number): Bounce {
              if (ci >= elems.length) return k(SCM_FALSE);
              const clause = elems[ci];
              if (clause.tag !== 'list' || clause.elements.length < 1)
                throw errAt('cond: bad clause', expr.pos);
              const test = clause.elements[0];
              if (test.tag === 'symbol' && test.value === 'else') {
                return evalBeginK(clause.elements.slice(1), env, k);
              }
              return evalK(test, env, (testVal) => {
                if (isTruthy(testVal)) {
                  if (clause.elements.length === 1) return k(testVal);
                  return evalBeginK(clause.elements.slice(1), env, k);
                }
                return tryCond(ci + 1);
              });
            }
            return tryCond(1);
          }
          case 'let': {
            // Named let: (let name ((var init) ...) body...)
            if (elems.length >= 3 && elems[1].tag === 'symbol') {
              const name = elems[1].value;
              const bindingList = elems[2];
              if (bindingList.tag !== 'list') throw errAt('let: bad syntax', expr.pos);
              const paramNames: string[] = [];
              const initExprs: SchemeVal[] = [];
              for (const b of bindingList.elements) {
                if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                  throw errAt('let: bad binding', expr.pos);
                paramNames.push(b.elements[0].value);
                initExprs.push(b.elements[1]);
              }
              const body = elems.slice(3);
              // Evaluate inits left-to-right (named let semantics)
              return evalListLeftK(initExprs, env, (initVals) => {
                const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
                const callEnv = new Env(env);
                callEnv.set(name, lambda);
                (lambda as any).env = callEnv;
                for (let i = 0; i < paramNames.length; i++) {
                  callEnv.set(paramNames[i], initVals[i]);
                }
                return evalBeginK(body, callEnv, k);
              });
            }
            // Regular let: (let ((var init) ...) body...)
            if (elems.length < 3) throw errAt('let: bad syntax', expr.pos);
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('let: bad syntax', expr.pos);
            const bindExprs = bindings.elements.map(b => {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('let: bad binding', expr.pos);
              return b.elements[1];
            });
            const bindNames = bindings.elements.map(b => (b as any).elements[0].value as string);
            return evalListLeftK(bindExprs, env, (vals) => {
              const letEnv = new Env(env);
              for (let i = 0; i < bindNames.length; i++) {
                letEnv.set(bindNames[i], vals[i]);
              }
              return evalBeginK(elems.slice(2), letEnv, k);
            });
          }
          case 'let*': {
            if (elems.length < 3) throw errAt('let*: bad syntax', expr.pos);
            const bindingsLS = elems[1];
            if (bindingsLS.tag !== 'list') throw errAt('let*: bad syntax', expr.pos);
            const bindingsElems = bindingsLS.elements;
            function bindLetStar(bi: number, curEnv: Env): Bounce {
              if (bi >= bindingsElems.length) {
                return evalBeginK(elems.slice(2), curEnv, k);
              }
              const b = bindingsElems[bi];
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('let*: bad binding', expr.pos);
              const bName = b.elements[0].value;
              const bInit = b.elements[1];
              return evalK(bInit, curEnv, (val) => {
                const nextEnv = new Env(curEnv);
                nextEnv.set(bName, val);
                return bindLetStar(bi + 1, nextEnv);
              });
            }
            return bindLetStar(0, new Env(env));
          }
          case 'letrec': {
            if (elems.length < 3) throw errAt('letrec: bad syntax', expr.pos);
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('letrec: bad syntax', expr.pos);
            const letrecEnv = new Env(env);
            const names: string[] = [];
            for (const b of bindings.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('letrec: bad binding', expr.pos);
              names.push(b.elements[0].value);
              letrecEnv.set(b.elements[0].value, SCM_FALSE);
            }
            const initExprs = bindings.elements.map(b => (b as any).elements[1] as SchemeVal);
            return evalListLeftK(initExprs, letrecEnv, (vals) => {
              for (let i = 0; i < names.length; i++) {
                letrecEnv.set(names[i], vals[i]);
              }
              return evalBeginK(elems.slice(2), letrecEnv, k);
            });
          }
          case 'letrec*': {
            if (elems.length < 3) throw errAt('letrec*: bad syntax', expr.pos);
            const bindingsLRS = elems[1];
            if (bindingsLRS.tag !== 'list') throw errAt('letrec*: bad syntax', expr.pos);
            const lrsElems = bindingsLRS.elements;
            const letrecStarEnv = new Env(env);
            function bindLetrecStar(bi: number): Bounce {
              if (bi >= lrsElems.length) {
                return evalBeginK(elems.slice(2), letrecStarEnv, k);
              }
              const b = lrsElems[bi];
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('letrec*: bad binding', expr.pos);
              const lrsName = b.elements[0].value;
              const lrsInit = b.elements[1];
              return evalK(lrsInit, letrecStarEnv, (val) => {
                letrecStarEnv.set(lrsName, val);
                return bindLetrecStar(bi + 1);
              });
            }
            return bindLetrecStar(0);
          }
          case 'case': {
            if (elems.length < 2) throw errAt('case: bad syntax', expr.pos);
            return evalK(elems[1], env, (key) => {
              function tryClause(ci: number): Bounce {
                if (ci >= elems.length) return k(SCM_FALSE);
                const clause = elems[ci];
                if (clause.tag !== 'list' || clause.elements.length < 2)
                  throw errAt('case: bad clause', expr.pos);
                const datums = clause.elements[0];
                if (datums.tag === 'symbol' && datums.value === 'else') {
                  return evalBeginK(clause.elements.slice(1), env, k);
                }
                if (datums.tag !== 'list') throw errAt('case: expected datum list', expr.pos);
                for (const d of datums.elements) {
                  const datum = listToPairs(d);
                  if (schemeEqv(key, datum)) {
                    return evalBeginK(clause.elements.slice(1), env, k);
                  }
                }
                return tryClause(ci + 1);
              }
              return tryClause(2);
            });
          }
          case 'do': {
            if (elems.length < 3) throw errAt('do: bad syntax', expr.pos);
            const bindingList = elems[1];
            if (bindingList.tag !== 'list') throw errAt('do: bad syntax', expr.pos);
            const testClauseRaw = elems[2];
            if (testClauseRaw.tag !== 'list' || testClauseRaw.elements.length < 1)
              throw errAt('do: bad test clause', expr.pos);
            const testElems = testClauseRaw.elements;

            const doEnv = new Env(env);
            const varNames: string[] = [];
            const stepExprs: (SchemeVal | null)[] = [];
            const initExprs: SchemeVal[] = [];

            for (const b of bindingList.elements) {
              if (b.tag !== 'list' || b.elements.length < 2 || b.elements[0].tag !== 'symbol')
                throw errAt('do: bad variable spec', expr.pos);
              varNames.push(b.elements[0].value);
              initExprs.push(b.elements[1]);
              stepExprs.push(b.elements.length >= 3 ? b.elements[2] : null);
            }

            return evalListLeftK(initExprs, env, (initVals) => {
              for (let i = 0; i < varNames.length; i++) {
                doEnv.set(varNames[i], initVals[i]);
              }
              function doLoop(): Bounce {
                return evalK(testElems[0], doEnv, (testVal) => {
                  if (isTruthy(testVal)) {
                    if (testElems.length === 1) return k(testVal);
                    return evalBeginK(testElems.slice(1), doEnv, k);
                  }
                  const bodyExprs = elems.slice(3);
                  function afterBody(): Bounce {
                    // Parallel step
                    const stepsToEval: { idx: number; stepExpr: SchemeVal }[] = [];
                    for (let j = 0; j < varNames.length; j++) {
                      if (stepExprs[j] !== null) stepsToEval.push({ idx: j, stepExpr: stepExprs[j]! });
                    }
                    if (stepsToEval.length === 0) return bounce(doLoop);
                    const stepVals: (SchemeVal | null)[] = new Array(varNames.length).fill(null);
                    function evalSteps(si: number): Bounce {
                      if (si >= stepsToEval.length) {
                        for (let j = 0; j < varNames.length; j++) {
                          if (stepVals[j] !== null) doEnv.set(varNames[j], stepVals[j]!);
                        }
                        return bounce(doLoop);
                      }
                      const { idx, stepExpr } = stepsToEval[si];
                      return evalK(stepExpr, doEnv, (val) => {
                        stepVals[idx] = val;
                        return evalSteps(si + 1);
                      });
                    }
                    return evalSteps(0);
                  }
                  if (bodyExprs.length > 0) {
                    return evalBeginK(bodyExprs, doEnv, (_) => afterBody());
                  }
                  return afterBody();
                });
              }
              return doLoop();
            });
          }
          case 'define-record-type': {
            if (elems.length < 4) throw errAt('define-record-type: bad syntax', expr.pos);
            const rtName = elems[1];
            if (rtName.tag !== 'symbol') throw errAt('define-record-type: expected type name', expr.pos);
            const ctorForm = elems[2];
            if (ctorForm.tag !== 'list' || ctorForm.elements.length < 1)
              throw errAt('define-record-type: bad constructor', expr.pos);
            const ctorName = ctorForm.elements[0];
            if (ctorName.tag !== 'symbol') throw errAt('define-record-type: expected constructor name', expr.pos);
            const ctorFields = ctorForm.elements.slice(1).map(e => {
              if (e.tag !== 'symbol') throw errAt('define-record-type: expected field name', expr.pos);
              return e.value;
            });
            const predName = elems[3];
            if (predName.tag !== 'symbol') throw errAt('define-record-type: expected predicate name', expr.pos);

            const accessors: { field: string; accessor: string }[] = [];
            for (let i = 4; i < elems.length; i++) {
              const fd = elems[i];
              if (fd.tag !== 'list' || fd.elements.length < 2)
                throw errAt('define-record-type: bad field spec', expr.pos);
              const fname = fd.elements[0];
              const acc = fd.elements[1];
              if (fname.tag !== 'symbol' || acc.tag !== 'symbol')
                throw errAt('define-record-type: expected symbols in field spec', expr.pos);
              accessors.push({ field: fname.value, accessor: acc.value });
            }

            const typeId = Symbol(rtName.value);

            const ctorFieldsCopy = [...ctorFields];
            const ctorLambda: SchemeVal = {
              tag: 'lambda', params: ctorFieldsCopy, body: [], env: new Env(),
            };
            (ctorLambda as any).nativeFn = (...args: SchemeVal[]): SchemeVal => {
              const fields = new Map<string, SchemeVal>();
              for (let i = 0; i < ctorFieldsCopy.length; i++) {
                fields.set(ctorFieldsCopy[i], args[i]);
              }
              return { tag: 'record', typeName: rtName.value, typeId, fields };
            };
            env.set(ctorName.value, ctorLambda);

            const predLambda: SchemeVal = {
              tag: 'lambda', params: ['__x__'], body: [], env: new Env(),
            };
            (predLambda as any).nativeFn = (x: SchemeVal): SchemeVal => {
              return x.tag === 'record' && x.typeId === typeId ? SCM_TRUE : SCM_FALSE;
            };
            env.set(predName.value, predLambda);

            for (const { field, accessor } of accessors) {
              const accLambda: SchemeVal = {
                tag: 'lambda', params: ['__x__'], body: [], env: new Env(),
              };
              (accLambda as any).nativeFn = (x: SchemeVal): SchemeVal => {
                if (x.tag !== 'record' || x.typeId !== typeId)
                  throw new EvalError(`${accessor}: not a ${rtName.value}`);
                return x.fields.get(field)!;
              };
              env.set(accessor, accLambda);
            }

            return k(SCM_FALSE);
          }
          case 'define-syntax': {
            if (elems.length !== 3) throw errAt('define-syntax: bad syntax', expr.pos);
            const nameElem = elems[1];
            if (nameElem.tag !== 'symbol') throw errAt('define-syntax: expected symbol', expr.pos);
            const transformer = elems[2];
            if (transformer.tag !== 'list' || transformer.elements.length < 2 ||
                transformer.elements[0].tag !== 'symbol' || transformer.elements[0].value !== 'syntax-rules')
              throw errAt('define-syntax: expected syntax-rules', expr.pos);
            const srElems = transformer.elements;
            const litList = srElems[1];
            if (litList.tag !== 'list') throw errAt('syntax-rules: expected literal list', expr.pos);
            const literals = litList.elements.map(e => {
              if (e.tag !== 'symbol') throw errAt('syntax-rules: literals must be symbols', expr.pos);
              return e.value;
            });
            const rules: MacroRule[] = [];
            for (let i = 2; i < srElems.length; i++) {
              const clause = srElems[i];
              if (clause.tag !== 'list' || clause.elements.length !== 2)
                throw errAt('syntax-rules: bad clause', expr.pos);
              const pattern = clause.elements[0];
              if (pattern.tag !== 'list' || pattern.elements.length === 0)
                throw errAt('syntax-rules: bad pattern', expr.pos);
              rules.push({ pattern: pattern.elements.slice(1), template: clause.elements[1], literals });
            }
            const macro: SchemeVal = { tag: 'macro', rules, defEnv: env };
            env.set(nameElem.value, macro);
            return k(SCM_FALSE);
          }
          case 'dynamic-wind': {
            if (elems.length !== 4) throw errAt('dynamic-wind: expected 3 arguments', expr.pos);
            return evalK(elems[1], env, (inThunk) =>
              evalK(elems[2], env, (bodyThunk) =>
                evalK(elems[3], env, (outThunk) =>
                  bounce(() => applyK(inThunk, [], (ignored) => {
                    const entry: WindEntry = { inThunk, outThunk };
                    windStack.push(entry);
                    return bounce(() => applyK(bodyThunk, [], (bodyVal) => {
                      windStack.pop();
                      return bounce(() => applyK(outThunk, [], (ignored2) => k(bodyVal)));
                    }));
                  }))
                )
              )
            );
          }
        }
        // Check for macro
        try {
          const resolved = env.get(head.value);
          if (resolved.tag === 'macro') {
            const expanded = expandMacro(resolved, elems, env, expr.pos);
            return bounce(() => evalK(expanded, env, k));
          }
        } catch (e) { /* not bound, fall through */ }
      }

      // Function application: evaluate args right-to-left, then head
      return evalListK(elems.slice(1), env, (args) =>
        evalK(head, env, (proc) =>
          bounce(() => applyK(proc, args, k, expr.pos))
        )
      );
    }
    default:
      throw errAt(`cannot evaluate: ${display(expr)}`, expr.pos);
  }
}

// Evaluate list left-to-right (for let bindings, do inits)
function evalListLeftK(exprs: SchemeVal[], env: Env, k: (vals: SchemeVal[]) => Bounce): Bounce {
  function loop(i: number, acc: SchemeVal[]): Bounce {
    if (i >= exprs.length) return k(acc);
    return evalK(exprs[i], env, (val) => {
      return loop(i + 1, [...acc, val]);
    });
  }
  return loop(0, []);
}

function applyK(proc: SchemeVal, args: SchemeVal[], k: Cont, pos?: Pos): Bounce {
  if (proc.tag === 'continuation') {
    if (args.length !== 1) throw errAt('continuation: expected 1 argument', pos);
    const targetStack = proc.windStack;
    const val = args[0];
    return bounce(() => doWindSwitch(windStack, targetStack, () => proc.k(val)));
  }

  if (proc.tag === 'lambda') {
    const nativeFn = (proc as any).nativeFn;
    if (nativeFn) return k(nativeFn(...args));
    if (proc.restParam) {
      if (args.length < proc.params.length)
        throw errAt(`expected at least ${proc.params.length} arguments, got ${args.length}`, pos);
    } else {
      if (args.length !== proc.params.length)
        throw errAt(`expected ${proc.params.length} arguments, got ${args.length}`, pos);
    }
    const callEnv = new Env(proc.env);
    for (let i = 0; i < proc.params.length; i++) {
      callEnv.set(proc.params[i], args[i]);
    }
    if (proc.restParam) {
      callEnv.set(proc.restParam, arrayToList(args.slice(proc.params.length)));
    }
    return evalBeginK(proc.body, callEnv, k);
  }

  if (proc.tag === 'case-lambda') {
    for (const clause of proc.clauses) {
      if (clause.restParam) {
        if (args.length >= clause.params.length) {
          const callEnv = new Env(clause.env);
          for (let i = 0; i < clause.params.length; i++) {
            callEnv.set(clause.params[i], args[i]);
          }
          callEnv.set(clause.restParam, arrayToList(args.slice(clause.params.length)));
          return evalBeginK(clause.body, callEnv, k);
        }
      } else {
        if (args.length === clause.params.length) {
          const callEnv = new Env(clause.env);
          for (let i = 0; i < clause.params.length; i++) {
            callEnv.set(clause.params[i], args[i]);
          }
          return evalBeginK(clause.body, callEnv, k);
        }
      }
    }
    throw errAt(`case-lambda: no matching clause for ${args.length} arguments`, pos);
  }

  if (proc.tag === 'builtin') {
    switch (proc.name) {
      case 'call/cc':
      case 'call-with-current-continuation': {
        if (args.length !== 1) throw errAt('call/cc: expected 1 argument', pos);
        const contVal: SchemeVal = { tag: 'continuation', k, windStack: [...windStack] };
        return bounce(() => applyK(args[0], [contVal], k, pos));
      }
      case 'apply': {
        if (args.length < 2) throw errAt('apply: expected at least 2 arguments', pos);
        const applyProc = args[0];
        const lastArg = args[args.length - 1];
        const tailList = pairToArray(lastArg);
        if (tailList === null && lastArg.tag !== 'nil')
          throw errAt('apply: last argument must be a proper list', pos);
        const prefixArgs = args.slice(1, args.length - 1);
        const allArgs = prefixArgs.concat(tailList ?? []);
        return bounce(() => applyK(applyProc, allArgs, k, pos));
      }
      case 'map': {
        if (args.length < 2) throw errAt('map: expected at least 2 arguments', pos);
        const mapProc = args[0];
        const lists = args.slice(1).map(a => {
          const arr = pairToArray(a);
          if (arr === null) throw errAt('map: expected proper list', pos);
          return arr;
        });
        const len = lists[0].length;
        for (const l of lists) {
          if (l.length !== len) throw errAt('map: lists must have equal length', pos);
        }
        const results: SchemeVal[] = [];
        function mapLoop(i: number): Bounce {
          if (i >= len) return k(arrayToList(results));
          const mapArgs = lists.map(l => l[i]);
          return applyK(mapProc, mapArgs, (val) => {
            results.push(val);
            return bounce(() => mapLoop(i + 1));
          }, pos);
        }
        return mapLoop(0);
      }
      case 'for-each': {
        if (args.length < 2) throw errAt('for-each: expected at least 2 arguments', pos);
        const feProc = args[0];
        const lists = args.slice(1).map(a => {
          const arr = pairToArray(a);
          if (arr === null) throw errAt('for-each: expected proper list', pos);
          return arr;
        });
        const len = lists[0].length;
        for (const l of lists) {
          if (l.length !== len) throw errAt('for-each: lists must have equal length', pos);
        }
        function feLoop(i: number): Bounce {
          if (i >= len) return k(SCM_NIL);
          const feArgs = lists.map(l => l[i]);
          return applyK(feProc, feArgs, (_) => bounce(() => feLoop(i + 1)), pos);
        }
        return feLoop(0);
      }
      default:
        return k(applyBuiltin(proc.name, args, pos));
    }
  }

  throw errAt(`not a procedure: ${display(proc)}`, pos);
}

// ── Equality ────────────────────────────────────────────────────────────

function schemeEq(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return numericCompare(a, b) === 0;
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'number': return a.value === (b as typeof a).value;
    case 'boolean': return a.value === (b as typeof a).value;
    case 'string': return a === b; // reference equality for strings in eq?
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'nil': return true;
    case 'pair': return a === b;
    default: return a === b;
  }
}

function schemeEqv(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return numericCompare(a, b) === 0;
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'number': return a.value === (b as typeof a).value;
    case 'boolean': return a.value === (b as typeof a).value;
    case 'string': return a.value === (b as typeof a).value;
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'nil': return true;
    case 'pair': return a === b;
    default: return a === b;
  }
}

function schemeEqual(a: SchemeVal, b: SchemeVal): boolean {
  if (isNumeric(a) && isNumeric(b)) return numericCompare(a, b) === 0;
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'number': return a.value === (b as typeof a).value;
    case 'boolean': return a.value === (b as typeof a).value;
    case 'string': return a.value === (b as typeof a).value;
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'nil': return true;
    case 'pair':
      return b.tag === 'pair' && schemeEqual(a.car, b.car) && schemeEqual(a.cdr, b.cdr);
    case 'vector': {
      if (b.tag !== 'vector') return false;
      if (a.elements.length !== b.elements.length) return false;
      for (let i = 0; i < a.elements.length; i++) {
        if (!schemeEqual(a.elements[i], b.elements[i])) return false;
      }
      return true;
    }
    default: return a === b;
  }
}

function requireNumbers(name: string, args: SchemeVal[], pos?: Pos): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw errAt(`${name}: expected number`, pos);
    return a.value;
  });
}

// ── Builtins ────────────────────────────────────────────────────────────

const BUILTINS = new Set([
  '+', '-', '*', '/', '<', '>', '=', '<=', '>=',
  'cons', 'car', 'cdr', 'null?', 'list', 'length', 'append',
  'number?', 'string?', 'boolean?', 'pair?', 'symbol?', 'char?',
  'display', 'write', 'newline',
  'string-append', 'string-length', 'substring',
  'string->number', 'number->string',
  'symbol->string', 'string->symbol',
  'string-ref',
  'string-copy', 'string-set!',
  'string->list', 'list->string', 'char->integer', 'integer->char',
  'apply',
  'abs', 'modulo', 'remainder', 'quotient', 'min', 'max', 'expt',
  'zero?', 'positive?', 'negative?', 'odd?', 'even?',
  'list-ref', 'list-tail', 'list?', 'assoc', 'map', 'for-each',
  'reverse', 'member', 'assv',
  'set-car!', 'set-cdr!',
  'caar', 'cadr', 'cdar', 'cddr', 'caddr',
  'eq?', 'eqv?', 'equal?',
  'vector', 'make-vector', 'vector-ref', 'vector-set!',
  'vector-length', 'vector?', 'vector->list', 'list->vector',
  'char-alphabetic?', 'char-numeric?', 'char-upcase', 'char-downcase',
  'char=?', 'char<?',
  'string=?', 'string<?', 'string>?', 'string<=?', 'string>=?',
  'string-ci=?',
  'string-upcase', 'string-downcase',
  'make-string', 'string',
  'exact?', 'inexact?', 'exact->inexact', 'inexact->exact',
  'numerator', 'denominator', 'integer?', 'rational?',
  'gcd', 'lcm', 'truncate', 'round', 'floor', 'ceiling',
  'procedure?',
  'call/cc', 'call-with-current-continuation',
]);

function applyBuiltin(name: string, args: SchemeVal[], pos?: Pos): SchemeVal {
  switch (name) {
    case '+': {
      requireNumeric('+', args, pos);
      if (args.length === 0) return { tag: 'number', value: 0, exact: true };
      let result: SchemeVal = args[0];
      for (let i = 1; i < args.length; i++) result = numericAdd(result, args[i]);
      return result;
    }
    case '-': {
      if (args.length === 0) throw errAt('-: expected at least 1 argument', pos);
      requireNumeric('-', args, pos);
      if (args.length === 1) {
        const a0 = args[0];
        if (a0.tag === 'rational') return makeRational(-a0.num, a0.den);
        if (a0.tag === 'number') return { tag: 'number', value: -a0.value, exact: a0.exact };
        throw errAt('-: expected number', pos);
      }
      let result: SchemeVal = args[0];
      for (let i = 1; i < args.length; i++) result = numericSub(result, args[i]);
      return result;
    }
    case '*': {
      requireNumeric('*', args, pos);
      if (args.length === 0) return { tag: 'number', value: 1, exact: true };
      let result: SchemeVal = args[0];
      for (let i = 1; i < args.length; i++) result = numericMul(result, args[i]);
      return result;
    }
    case '/': {
      if (args.length < 2) throw errAt('/: expected at least 2 arguments', pos);
      requireNumeric('/', args, pos);
      let result: SchemeVal = args[0];
      for (let i = 1; i < args.length; i++) result = numericDiv(result, args[i], pos);
      return result;
    }
    case '<': {
      requireNumeric('<', args, pos);
      return numericCompare(args[0], args[1]) < 0 ? SCM_TRUE : SCM_FALSE;
    }
    case '>': {
      requireNumeric('>', args, pos);
      return numericCompare(args[0], args[1]) > 0 ? SCM_TRUE : SCM_FALSE;
    }
    case '=': {
      requireNumeric('=', args, pos);
      return numericCompare(args[0], args[1]) === 0 ? SCM_TRUE : SCM_FALSE;
    }
    case '<=': {
      requireNumeric('<=', args, pos);
      return numericCompare(args[0], args[1]) <= 0 ? SCM_TRUE : SCM_FALSE;
    }
    case '>=': {
      requireNumeric('>=', args, pos);
      return numericCompare(args[0], args[1]) >= 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'cons': {
      if (args.length !== 2) throw errAt('cons: expected 2 arguments', pos);
      return makePair(args[0], args[1]);
    }
    case 'car': {
      if (args.length !== 1) throw errAt('car: expected 1 argument', pos);
      if (args[0].tag !== 'pair') throw errAt('car: expected pair', pos);
      return args[0].car;
    }
    case 'cdr': {
      if (args.length !== 1) throw errAt('cdr: expected 1 argument', pos);
      if (args[0].tag !== 'pair') throw errAt('cdr: expected pair', pos);
      return args[0].cdr;
    }
    case 'null?': {
      if (args.length !== 1) throw errAt('null?: expected 1 argument', pos);
      return args[0].tag === 'nil' ? SCM_TRUE : SCM_FALSE;
    }
    case 'list': {
      return arrayToList(args);
    }
    case 'length': {
      if (args.length !== 1) throw errAt('length: expected 1 argument', pos);
      let count = 0;
      let cur = args[0];
      while (cur.tag === 'pair') {
        count++;
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') throw errAt('length: expected proper list', pos);
      return { tag: 'number', value: count };
    }
    case 'append': {
      if (args.length === 0) return SCM_NIL;
      if (args.length === 1) return args[0];
      let result = args[args.length - 1];
      for (let i = args.length - 2; i >= 0; i--) {
        const items = pairToArray(args[i]);
        if (items === null) throw errAt('append: expected proper list', pos);
        for (let j = items.length - 1; j >= 0; j--) {
          result = makePair(items[j], result);
        }
      }
      return result;
    }
    case 'number?':
      if (args.length !== 1) throw errAt('number?: expected 1 argument', pos);
      return isNumeric(args[0]) ? SCM_TRUE : SCM_FALSE;
    case 'string?':
      if (args.length !== 1) throw errAt('string?: expected 1 argument', pos);
      return args[0].tag === 'string' ? SCM_TRUE : SCM_FALSE;
    case 'boolean?':
      if (args.length !== 1) throw errAt('boolean?: expected 1 argument', pos);
      return args[0].tag === 'boolean' ? SCM_TRUE : SCM_FALSE;
    case 'pair?':
      if (args.length !== 1) throw errAt('pair?: expected 1 argument', pos);
      return args[0].tag === 'pair' ? SCM_TRUE : SCM_FALSE;
    case 'symbol?':
      if (args.length !== 1) throw errAt('symbol?: expected 1 argument', pos);
      return args[0].tag === 'symbol' ? SCM_TRUE : SCM_FALSE;
    case 'char?':
      if (args.length !== 1) throw errAt('char?: expected 1 argument', pos);
      return args[0].tag === 'char' ? SCM_TRUE : SCM_FALSE;
    case 'procedure?':
      if (args.length !== 1) throw errAt('procedure?: expected 1 argument', pos);
      return (args[0].tag === 'lambda' || args[0].tag === 'builtin' ||
              args[0].tag === 'case-lambda' || args[0].tag === 'continuation') ? SCM_TRUE : SCM_FALSE;
    case 'display': {
      if (args.length !== 1) throw errAt('display: expected 1 argument', pos);
      outputBuffer.push(displayFormat(args[0]));
      return SCM_FALSE;
    }
    case 'write': {
      if (args.length !== 1) throw errAt('write: expected 1 argument', pos);
      outputBuffer.push(display(args[0]));
      return SCM_FALSE;
    }
    case 'newline': {
      outputBuffer.push('\n');
      return SCM_FALSE;
    }
    case 'string-append': {
      const strs = args.map(a => {
        if (a.tag !== 'string') throw errAt('string-append: expected string', pos);
        return a.value;
      });
      return { tag: 'string', value: strs.join('') };
    }
    case 'string-length': {
      if (args.length !== 1 || args[0].tag !== 'string')
        throw errAt('string-length: expected string', pos);
      return { tag: 'number', value: args[0].value.length };
    }
    case 'substring': {
      if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'number')
        throw errAt('substring: expected string, start, end', pos);
      return { tag: 'string', value: args[0].value.slice(args[1].value, args[2].value) };
    }
    case 'string->number': {
      if (args.length !== 1 || args[0].tag !== 'string')
        throw errAt('string->number: expected string', pos);
      const n = Number(args[0].value);
      if (isNaN(n)) return SCM_FALSE;
      return { tag: 'number', value: n };
    }
    case 'number->string': {
      if (args.length !== 1 || !isNumeric(args[0]))
        throw errAt('number->string: expected number', pos);
      return { tag: 'string', value: display(args[0]) };
    }
    case 'symbol->string': {
      if (args.length !== 1 || args[0].tag !== 'symbol')
        throw errAt('symbol->string: expected symbol', pos);
      return { tag: 'string', value: args[0].value };
    }
    case 'string->symbol': {
      if (args.length !== 1 || args[0].tag !== 'string')
        throw errAt('string->symbol: expected string', pos);
      return { tag: 'symbol', value: args[0].value };
    }
    case 'string-ref': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'number')
        throw errAt('string-ref: expected string and index', pos);
      const idx = args[1].value;
      if (idx < 0 || idx >= args[0].value.length)
        throw errAt('string-ref: index out of range', pos);
      return { tag: 'char', value: args[0].value[idx] };
    }
    case 'string-copy': {
      if (args.length !== 1 || args[0].tag !== 'string')
        throw errAt('string-copy: expected string', pos);
      return { tag: 'string', value: args[0].value };
    }
    case 'string-set!': {
      if (args.length !== 3 || args[0].tag !== 'string' || args[1].tag !== 'number' || args[2].tag !== 'char')
        throw errAt('string-set!: expected string, index, char', pos);
      if (args[0].immutable)
        throw errAt('string-set!: strings are immutable', pos);
      const si = args[1].value;
      if (si < 0 || si >= args[0].value.length)
        throw errAt('string-set!: index out of range', pos);
      (args[0] as any).value = args[0].value.substring(0, si) + args[2].value + args[0].value.substring(si + 1);
      return SCM_FALSE;
    }
    case 'string->list': {
      if (args.length < 1 || args[0].tag !== 'string')
        throw errAt('string->list: expected string', pos);
      const str = args[0].value;
      let result: SchemeVal = SCM_NIL;
      for (let i = str.length - 1; i >= 0; i--) {
        result = { tag: 'pair', car: { tag: 'char', value: str[i] }, cdr: result };
      }
      return result;
    }
    case 'list->string': {
      if (args.length !== 1) throw errAt('list->string: expected 1 argument', pos);
      const chars: string[] = [];
      let cur = args[0];
      while (cur.tag === 'pair') {
        if (cur.car.tag !== 'char') throw errAt('list->string: expected list of characters', pos);
        chars.push(cur.car.value);
        cur = cur.cdr;
      }
      return { tag: 'string', value: chars.join('') };
    }
    case 'char->integer': {
      if (args.length !== 1 || args[0].tag !== 'char')
        throw errAt('char->integer: expected char', pos);
      return { tag: 'number', value: args[0].value.charCodeAt(0), exact: true };
    }
    case 'integer->char': {
      if (args.length !== 1 || args[0].tag !== 'number')
        throw errAt('integer->char: expected integer', pos);
      return { tag: 'char', value: String.fromCharCode(args[0].value) };
    }
    case 'abs': {
      if (args.length !== 1) throw errAt('abs: expected 1 argument', pos);
      requireNumeric('abs', args, pos);
      const a0abs = args[0];
      if (a0abs.tag === 'rational') return makeRational(Math.abs(a0abs.num), a0abs.den);
      if (a0abs.tag === 'number') return { tag: 'number', value: Math.abs(a0abs.value), exact: a0abs.exact };
      throw errAt('abs: expected number', pos);
    }
    case 'modulo': {
      if (args.length !== 2) throw errAt('modulo: expected 2 arguments', pos);
      const nums = requireNumbers('modulo', args, pos);
      if (nums[1] === 0) throw errAt('modulo: division by zero', pos);
      const r = nums[0] % nums[1];
      return { tag: 'number', value: (r !== 0 && Math.sign(r) !== Math.sign(nums[1])) ? r + nums[1] : r };
    }
    case 'remainder': {
      if (args.length !== 2) throw errAt('remainder: expected 2 arguments', pos);
      const nums = requireNumbers('remainder', args, pos);
      if (nums[1] === 0) throw errAt('remainder: division by zero', pos);
      return { tag: 'number', value: nums[0] % nums[1] };
    }
    case 'quotient': {
      if (args.length !== 2) throw errAt('quotient: expected 2 arguments', pos);
      const nums = requireNumbers('quotient', args, pos);
      if (nums[1] === 0) throw errAt('quotient: division by zero', pos);
      return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
    }
    case 'min': {
      if (args.length === 0) throw errAt('min: expected at least 1 argument', pos);
      requireNumeric('min', args, pos);
      let minVal = args[0];
      for (let i = 1; i < args.length; i++) {
        if (numericCompare(args[i], minVal) < 0) minVal = args[i];
      }
      return minVal;
    }
    case 'max': {
      if (args.length === 0) throw errAt('max: expected at least 1 argument', pos);
      requireNumeric('max', args, pos);
      let maxVal = args[0];
      for (let i = 1; i < args.length; i++) {
        if (numericCompare(args[i], maxVal) > 0) maxVal = args[i];
      }
      return maxVal;
    }
    case 'expt': {
      if (args.length !== 2) throw errAt('expt: expected 2 arguments', pos);
      requireNumeric('expt', args, pos);
      return { tag: 'number', value: Math.pow(toFloat(args[0]), toFloat(args[1])) };
    }
    case 'zero?': {
      if (args.length !== 1) throw errAt('zero?: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('zero?: expected number', pos);
      return toFloat(args[0]) === 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'positive?': {
      if (args.length !== 1) throw errAt('positive?: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('positive?: expected number', pos);
      return toFloat(args[0]) > 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'negative?': {
      if (args.length !== 1) throw errAt('negative?: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('negative?: expected number', pos);
      return toFloat(args[0]) < 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'odd?': {
      if (args.length !== 1) throw errAt('odd?: expected 1 argument', pos);
      if (args[0].tag !== 'number') throw errAt('odd?: expected number', pos);
      return Math.abs(args[0].value) % 2 === 1 ? SCM_TRUE : SCM_FALSE;
    }
    case 'even?': {
      if (args.length !== 1) throw errAt('even?: expected 1 argument', pos);
      if (args[0].tag !== 'number') throw errAt('even?: expected number', pos);
      return args[0].value % 2 === 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'list-ref': {
      if (args.length !== 2) throw errAt('list-ref: expected 2 arguments', pos);
      if (args[1].tag !== 'number') throw errAt('list-ref: expected number index', pos);
      let cur = args[0];
      let idx = args[1].value;
      while (idx > 0 && cur.tag === 'pair') {
        cur = cur.cdr;
        idx--;
      }
      if (cur.tag !== 'pair') throw errAt('list-ref: index out of range', pos);
      return cur.car;
    }
    case 'list-tail': {
      if (args.length !== 2) throw errAt('list-tail: expected 2 arguments', pos);
      if (args[1].tag !== 'number') throw errAt('list-tail: expected number index', pos);
      let cur = args[0];
      let idx = args[1].value;
      while (idx > 0) {
        if (cur.tag !== 'pair') throw errAt('list-tail: index out of range', pos);
        cur = cur.cdr;
        idx--;
      }
      return cur;
    }
    case 'list?': {
      let slow = args[0];
      let fast = args[0];
      while (fast.tag === 'pair') {
        slow = (slow as any).cdr;
        fast = fast.cdr;
        if (fast.tag !== 'pair') break;
        fast = fast.cdr;
        if (slow === fast) return SCM_FALSE;
      }
      return fast.tag === 'nil' ? SCM_TRUE : SCM_FALSE;
    }
    case 'assoc': {
      if (args.length !== 2) throw errAt('assoc: expected 2 arguments', pos);
      let cur = args[1];
      while (cur.tag === 'pair') {
        const entry = cur.car;
        if (entry.tag === 'pair' && schemeEqual(args[0], entry.car)) {
          return entry;
        }
        cur = cur.cdr;
      }
      return SCM_FALSE;
    }
    case 'eq?': {
      if (args.length !== 2) throw errAt('eq?: expected 2 arguments', pos);
      return schemeEq(args[0], args[1]) ? SCM_TRUE : SCM_FALSE;
    }
    case 'eqv?': {
      if (args.length !== 2) throw errAt('eqv?: expected 2 arguments', pos);
      return schemeEqv(args[0], args[1]) ? SCM_TRUE : SCM_FALSE;
    }
    case 'equal?': {
      if (args.length !== 2) throw errAt('equal?: expected 2 arguments', pos);
      return schemeEqual(args[0], args[1]) ? SCM_TRUE : SCM_FALSE;
    }
    case 'vector': {
      return { tag: 'vector', elements: [...args] };
    }
    case 'make-vector': {
      if (args.length < 1 || args.length > 2) throw errAt('make-vector: expected 1-2 arguments', pos);
      if (args[0].tag !== 'number') throw errAt('make-vector: expected number', pos);
      const size = args[0].value;
      const fill = args.length === 2 ? args[1] : { tag: 'number' as const, value: 0 };
      return { tag: 'vector', elements: Array.from({ length: size }, () => fill) };
    }
    case 'vector-ref': {
      if (args.length !== 2) throw errAt('vector-ref: expected 2 arguments', pos);
      if (args[0].tag !== 'vector') throw errAt('vector-ref: expected vector', pos);
      if (args[1].tag !== 'number') throw errAt('vector-ref: expected number index', pos);
      const vi = args[1].value;
      if (vi < 0 || vi >= args[0].elements.length) throw errAt('vector-ref: index out of range', pos);
      return args[0].elements[vi];
    }
    case 'vector-set!': {
      if (args.length !== 3) throw errAt('vector-set!: expected 3 arguments', pos);
      if (args[0].tag !== 'vector') throw errAt('vector-set!: expected vector', pos);
      if (args[1].tag !== 'number') throw errAt('vector-set!: expected number index', pos);
      const vsi = args[1].value;
      if (vsi < 0 || vsi >= args[0].elements.length) throw errAt('vector-set!: index out of range', pos);
      args[0].elements[vsi] = args[2];
      return SCM_FALSE;
    }
    case 'vector-length': {
      if (args.length !== 1) throw errAt('vector-length: expected 1 argument', pos);
      if (args[0].tag !== 'vector') throw errAt('vector-length: expected vector', pos);
      return { tag: 'number', value: args[0].elements.length };
    }
    case 'vector?': {
      if (args.length !== 1) throw errAt('vector?: expected 1 argument', pos);
      return args[0].tag === 'vector' ? SCM_TRUE : SCM_FALSE;
    }
    case 'vector->list': {
      if (args.length !== 1) throw errAt('vector->list: expected 1 argument', pos);
      if (args[0].tag !== 'vector') throw errAt('vector->list: expected vector', pos);
      return arrayToList(args[0].elements);
    }
    case 'list->vector': {
      if (args.length !== 1) throw errAt('list->vector: expected 1 argument', pos);
      const items = pairToArray(args[0]);
      if (items === null) throw errAt('list->vector: expected proper list', pos);
      return { tag: 'vector', elements: items };
    }
    case 'char-alphabetic?': {
      if (args.length !== 1 || args[0].tag !== 'char')
        throw errAt('char-alphabetic?: expected char', pos);
      return /[a-zA-Z]/.test(args[0].value) ? SCM_TRUE : SCM_FALSE;
    }
    case 'char-numeric?': {
      if (args.length !== 1 || args[0].tag !== 'char')
        throw errAt('char-numeric?: expected char', pos);
      return /[0-9]/.test(args[0].value) ? SCM_TRUE : SCM_FALSE;
    }
    case 'char-upcase': {
      if (args.length !== 1 || args[0].tag !== 'char')
        throw errAt('char-upcase: expected char', pos);
      return { tag: 'char', value: args[0].value.toUpperCase() };
    }
    case 'char-downcase': {
      if (args.length !== 1 || args[0].tag !== 'char')
        throw errAt('char-downcase: expected char', pos);
      return { tag: 'char', value: args[0].value.toLowerCase() };
    }
    case 'char=?': {
      if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char')
        throw errAt('char=?: expected 2 chars', pos);
      return args[0].value === args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'char<?': {
      if (args.length !== 2 || args[0].tag !== 'char' || args[1].tag !== 'char')
        throw errAt('char<?: expected 2 chars', pos);
      return args[0].value < args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'string=?': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
        throw errAt('string=?: expected 2 strings', pos);
      return args[0].value === args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'string<?': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
        throw errAt('string<?: expected 2 strings', pos);
      return args[0].value < args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'string>?': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
        throw errAt('string>?: expected 2 strings', pos);
      return args[0].value > args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'string<=?': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
        throw errAt('string<=?: expected 2 strings', pos);
      return args[0].value <= args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'string>=?': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
        throw errAt('string>=?: expected 2 strings', pos);
      return args[0].value >= args[1].value ? SCM_TRUE : SCM_FALSE;
    }
    case 'string-ci=?': {
      if (args.length !== 2 || args[0].tag !== 'string' || args[1].tag !== 'string')
        throw errAt('string-ci=?: expected 2 strings', pos);
      return args[0].value.toLowerCase() === args[1].value.toLowerCase() ? SCM_TRUE : SCM_FALSE;
    }
    case 'string-upcase': {
      if (args.length !== 1 || args[0].tag !== 'string')
        throw errAt('string-upcase: expected string', pos);
      return { tag: 'string', value: args[0].value.toUpperCase() };
    }
    case 'string-downcase': {
      if (args.length !== 1 || args[0].tag !== 'string')
        throw errAt('string-downcase: expected string', pos);
      return { tag: 'string', value: args[0].value.toLowerCase() };
    }
    case 'exact?': {
      if (args.length !== 1) throw errAt('exact?: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('exact?: expected number', pos);
      return isExact(args[0]) ? SCM_TRUE : SCM_FALSE;
    }
    case 'inexact?': {
      if (args.length !== 1) throw errAt('inexact?: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('inexact?: expected number', pos);
      return isExact(args[0]) ? SCM_FALSE : SCM_TRUE;
    }
    case 'exact->inexact': {
      if (args.length !== 1) throw errAt('exact->inexact: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('exact->inexact: expected number', pos);
      return { tag: 'number', value: toFloat(args[0]), exact: false };
    }
    case 'inexact->exact': {
      if (args.length !== 1) throw errAt('inexact->exact: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('inexact->exact: expected number', pos);
      if (isExact(args[0])) return args[0];
      const v = toFloat(args[0]);
      if (Number.isInteger(v)) return { tag: 'number', value: v, exact: true };
      const sign = v < 0 ? -1 : 1;
      const av = Math.abs(v);
      const scale = Math.pow(10, 15);
      const p = Math.round(av * scale);
      const q = scale;
      const g = gcd(p, q);
      return makeRational(sign * p / g, q / g);
    }
    case 'numerator': {
      if (args.length !== 1) throw errAt('numerator: expected 1 argument', pos);
      const a0n = args[0];
      if (a0n.tag === 'rational') return { tag: 'number', value: a0n.num, exact: true };
      if (a0n.tag === 'number') return { tag: 'number', value: a0n.value, exact: a0n.exact };
      throw errAt('numerator: expected number', pos);
    }
    case 'denominator': {
      if (args.length !== 1) throw errAt('denominator: expected 1 argument', pos);
      const a0d = args[0];
      if (a0d.tag === 'rational') return { tag: 'number', value: a0d.den, exact: true };
      if (a0d.tag === 'number') return { tag: 'number', value: 1, exact: a0d.exact };
      throw errAt('denominator: expected number', pos);
    }
    case 'integer?': {
      if (args.length !== 1) throw errAt('integer?: expected 1 argument', pos);
      if (args[0].tag === 'rational') return SCM_FALSE;
      if (args[0].tag === 'number') return Number.isInteger(args[0].value) ? SCM_TRUE : SCM_FALSE;
      return SCM_FALSE;
    }
    case 'rational?': {
      if (args.length !== 1) throw errAt('rational?: expected 1 argument', pos);
      return isNumeric(args[0]) && isExact(args[0]) ? SCM_TRUE : SCM_FALSE;
    }
    case 'caar': {
      if (args.length !== 1) throw errAt('caar: expected 1 argument', pos);
      if (args[0].tag !== 'pair' || args[0].car.tag !== 'pair') throw errAt('caar: expected pair', pos);
      return args[0].car.car;
    }
    case 'cadr': {
      if (args.length !== 1) throw errAt('cadr: expected 1 argument', pos);
      if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw errAt('cadr: expected pair', pos);
      return args[0].cdr.car;
    }
    case 'cdar': {
      if (args.length !== 1) throw errAt('cdar: expected 1 argument', pos);
      if (args[0].tag !== 'pair' || args[0].car.tag !== 'pair') throw errAt('cdar: expected pair', pos);
      return args[0].car.cdr;
    }
    case 'cddr': {
      if (args.length !== 1) throw errAt('cddr: expected 1 argument', pos);
      if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw errAt('cddr: expected pair', pos);
      return args[0].cdr.cdr;
    }
    case 'caddr': {
      if (args.length !== 1) throw errAt('caddr: expected 1 argument', pos);
      if (args[0].tag !== 'pair' || args[0].cdr.tag !== 'pair') throw errAt('caddr: expected pair', pos);
      const cddr = args[0].cdr.cdr;
      if (cddr.tag !== 'pair') throw errAt('caddr: expected pair', pos);
      return cddr.car;
    }
    case 'set-car!': {
      if (args.length !== 2) throw errAt('set-car!: expected 2 arguments', pos);
      if (args[0].tag !== 'pair') throw errAt('set-car!: expected pair', pos);
      (args[0] as any).car = args[1];
      return SCM_NIL;
    }
    case 'set-cdr!': {
      if (args.length !== 2) throw errAt('set-cdr!: expected 2 arguments', pos);
      if (args[0].tag !== 'pair') throw errAt('set-cdr!: expected pair', pos);
      (args[0] as any).cdr = args[1];
      return SCM_NIL;
    }
    case 'reverse': {
      if (args.length !== 1) throw errAt('reverse: expected 1 argument', pos);
      const items = pairToArray(args[0]);
      if (items === null) throw errAt('reverse: expected proper list', pos);
      return arrayToList(items.reverse());
    }
    case 'member': {
      if (args.length !== 2) throw errAt('member: expected 2 arguments', pos);
      let cur = args[1];
      while (cur.tag === 'pair') {
        if (schemeEqual(args[0], cur.car)) return cur;
        cur = cur.cdr;
      }
      return SCM_FALSE;
    }
    case 'assv': {
      if (args.length !== 2) throw errAt('assv: expected 2 arguments', pos);
      let cur = args[1];
      while (cur.tag === 'pair') {
        const entry = cur.car;
        if (entry.tag === 'pair' && schemeEqv(args[0], entry.car)) return entry;
        cur = cur.cdr;
      }
      return SCM_FALSE;
    }
    case 'make-string': {
      if (args.length < 1 || args.length > 2) throw errAt('make-string: expected 1-2 arguments', pos);
      if (args[0].tag !== 'number') throw errAt('make-string: expected number', pos);
      const ch = args.length === 2 && args[1].tag === 'char' ? args[1].value : '\0';
      return { tag: 'string', value: ch.repeat(args[0].value) };
    }
    case 'string': {
      for (const a of args) {
        if (a.tag !== 'char') throw errAt('string: expected char arguments', pos);
      }
      return { tag: 'string', value: args.map(a => (a as any).value).join('') };
    }
    case 'gcd': {
      requireNumeric('gcd', args, pos);
      if (args.length === 0) return { tag: 'number', value: 0, exact: true };
      let result = Math.abs(toFloat(args[0]));
      for (let i = 1; i < args.length; i++) result = gcd(result, Math.abs(toFloat(args[i])));
      return { tag: 'number', value: result, exact: true };
    }
    case 'lcm': {
      requireNumeric('lcm', args, pos);
      if (args.length === 0) return { tag: 'number', value: 1, exact: true };
      let result = Math.abs(toFloat(args[0]));
      for (let i = 1; i < args.length; i++) {
        const b = Math.abs(toFloat(args[i]));
        result = result === 0 && b === 0 ? 0 : (result / gcd(result, b)) * b;
      }
      return { tag: 'number', value: result, exact: true };
    }
    case 'truncate': {
      if (args.length !== 1) throw errAt('truncate: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('truncate: expected number', pos);
      return { tag: 'number', value: Math.trunc(toFloat(args[0])), exact: isExact(args[0]) };
    }
    case 'round': {
      if (args.length !== 1) throw errAt('round: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('round: expected number', pos);
      return { tag: 'number', value: Math.round(toFloat(args[0])), exact: isExact(args[0]) };
    }
    case 'floor': {
      if (args.length !== 1) throw errAt('floor: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('floor: expected number', pos);
      return { tag: 'number', value: Math.floor(toFloat(args[0])), exact: isExact(args[0]) };
    }
    case 'ceiling': {
      if (args.length !== 1) throw errAt('ceiling: expected 1 argument', pos);
      if (!isNumeric(args[0])) throw errAt('ceiling: expected number', pos);
      return { tag: 'number', value: Math.ceil(toFloat(args[0])), exact: isExact(args[0]) };
    }
    default:
      throw errAt(`unbound variable: ${name}`, pos);
  }
}

// ── Output buffer ──────────────────────────────────────────────────────

let outputBuffer: string[] = [];

// ── Dynamic-wind stack ────────────────────────────────────────────────

type WindEntry = { inThunk: SchemeVal; outThunk: SchemeVal };
let windStack: WindEntry[] = [];

// Find common prefix length between two wind stacks
function windCommonPrefix(from: WindEntry[], to: WindEntry[]): number {
  const len = Math.min(from.length, to.length);
  for (let i = 0; i < len; i++) {
    if (from[i] !== to[i]) return i;
  }
  return len;
}

// Switch from current wind stack to target, calling out/in thunks as needed
function doWindSwitch(from: WindEntry[], to: WindEntry[], then: () => Bounce): Bounce {
  const common = windCommonPrefix(from, to);

  // Unwind: call out-thunks from innermost to common prefix
  function unwind(i: number): Bounce {
    if (i <= common) return rewind(common);
    const entry = from[i - 1];
    windStack = from.slice(0, i - 1);
    return bounce(() => applyK(entry.outThunk, [], (ignored) => bounce(() => unwind(i - 1))));
  }

  // Rewind: call in-thunks from common prefix to target
  function rewind(i: number): Bounce {
    if (i >= to.length) {
      windStack = [...to];
      return bounce(then);
    }
    const entry = to[i];
    windStack = to.slice(0, i);
    return bounce(() => applyK(entry.inThunk, [], (ignored) => {
      windStack = to.slice(0, i + 1);
      return bounce(() => rewind(i + 1));
    }));
  }

  return unwind(from.length);
}

// ── Display ────────────────────────────────────────────────────────────

// write format: strings quoted
function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': {
      const s = String(val.value);
      if (val.exact === false && Number.isInteger(val.value) && !s.includes('.')) return s + '.0';
      return s;
    }
    case 'rational': return `${val.num}/${val.den}`;
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'nil': return '()';
    case 'pair': {
      const seen = new Set<SchemeVal>();
      seen.add(val);
      let result = '(' + display(val.car);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        if (seen.has(cur)) { result += ' ...'; break; }
        seen.add(cur);
        result += ' ' + display(cur.car);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil' && !seen.has(cur)) {
        result += ' . ' + display(cur);
      }
      result += ')';
      return result;
    }
    case 'list': return `(${val.elements.map(display).join(' ')})`;
    case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
    case 'lambda': return '#<procedure>';
    case 'case-lambda': return '#<procedure>';
    case 'builtin': return '#<procedure>';
    case 'continuation': return '#<procedure>';
    case 'macro': return '#<macro>';
    case 'vector': return `#(${val.elements.map(display).join(' ')})`;
    case 'record': return `#<record ${val.typeName}>`;
  }
}

// display format: strings unquoted
function displayFormat(val: SchemeVal): string {
  if (val.tag === 'string') return val.value;
  if (val.tag === 'char') return val.value;
  if (val.tag === 'pair') {
    const seen = new Set<SchemeVal>();
    seen.add(val);
    let result = '(' + displayFormat(val.car);
    let cur: SchemeVal = val.cdr;
    while (cur.tag === 'pair') {
      if (seen.has(cur)) { result += ' ...'; break; }
      seen.add(cur);
      result += ' ' + displayFormat(cur.car);
      cur = cur.cdr;
    }
    if (cur.tag !== 'nil' && !seen.has(cur)) {
      result += ' . ' + displayFormat(cur);
    }
    result += ')';
    return result;
  }
  return display(val);
}

// ── Public API ─────────────────────────────────────────────────────────

function makeGlobalEnv(): Env {
  const env = new Env();
  for (const name of BUILTINS) {
    env.set(name, { tag: 'builtin', name });
  }
  return env;
}

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  windStack = [];
  const env = makeGlobalEnv();
  const result = trampoline(evalBeginK(exprs, env, (v) => done(v)));
  return display(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  outputBuffer = [];
  windStack = [];
  const env = makeGlobalEnv();
  const result = trampoline(evalBeginK(exprs, env, (v) => done(v)));
  return { result: display(result), output: outputBuffer.join('') };
}
