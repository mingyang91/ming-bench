import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; exact?: boolean; pos?: Pos }
  | { tag: 'rational'; num: number; den: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
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
  | { tag: 'vector'; elements: SchemeVal[]; pos?: Pos };

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
      return { tag: 'string', value: tok.text.slice(1, -1), pos: tok.pos };
    }
    // Rational literal: n/d (e.g. 1/3, -5/2)
    const ratMatch = /^(-?\d+)\/(\d+)$/.exec(tok.text);
    if (ratMatch) {
      const rn = parseInt(ratMatch[1], 10);
      const rd = parseInt(ratMatch[2], 10);
      const rv = makeRational(rn, rd);
      if (rv.pos === undefined) rv.pos = tok.pos;
      else rv.pos = tok.pos;
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

// ── Evaluator ──────────────────────────────────────────────────────────

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
  'set!', 'cond', 'let', 'letrec', 'letrec*', 'case', 'do',
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

function expandAndEvalMacro(
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
      const expanded = expandTemplate(rule.template, bindings, renames);
      return evalExpr(expanded, env);
    }
  }
  throw errAt('no matching pattern in syntax-rules', pos);
}

function evalExpr(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'rational':
    case 'boolean':
    case 'string':
    case 'char':
    case 'nil':
    case 'pair':
    case 'vector':
      return expr;
    case 'symbol':
      try { return env.get(expr.value); }
      catch (e) { throw errAt(`unbound variable: ${expr.value}`, expr.pos); }
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw errAt('empty application', expr.pos);

      const head = elems[0];

      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'quote': {
            if (elems.length !== 2) throw errAt('quote: expected 1 argument', expr.pos);
            return listToPairs(elems[1]);
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4)
              throw errAt('if: expected 2-3 arguments', expr.pos);
            const cond = evalExpr(elems[1], env);
            if (isTruthy(cond)) return evalExpr(elems[2], env);
            if (elems.length === 4) return evalExpr(elems[3], env);
            return SCM_FALSE;
          }
          case 'define': {
            if (elems.length < 3) throw errAt('define: bad syntax', expr.pos);
            const target = elems[1];
            if (target.tag === 'symbol') {
              const val = evalExpr(elems[2], env);
              env.set(target.value, val);
              return val;
            }
            if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
              const name = target.elements[0].value;
              const { params, restParam } = parseParams(target.elements.slice(1), expr.pos);
              const body = elems.slice(2);
              const lambda: SchemeVal = { tag: 'lambda', params, restParam, body, env };
              env.set(name, lambda);
              return lambda;
            }
            throw errAt('define: bad syntax', expr.pos);
          }
          case 'lambda': {
            if (elems.length < 3) throw errAt('lambda: bad syntax', expr.pos);
            const paramList = elems[1];
            // (lambda args body) — single symbol captures all args
            if (paramList.tag === 'symbol') {
              const body = elems.slice(2);
              return { tag: 'lambda', params: [], restParam: paramList.value, body, env };
            }
            if (paramList.tag !== 'list') throw errAt('lambda: params must be a list', expr.pos);
            const { params, restParam } = parseParams(paramList.elements, expr.pos);
            const body = elems.slice(2);
            return { tag: 'lambda', params, restParam, body, env };
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
            return { tag: 'case-lambda' as const, clauses, pos: expr.pos };
          }
          case 'and': {
            let result: SchemeVal = SCM_TRUE;
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
              if (!isTruthy(result)) return result;
            }
            return result;
          }
          case 'or': {
            let result: SchemeVal = SCM_FALSE;
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
              if (isTruthy(result)) return result;
            }
            return result;
          }
          case 'not': {
            if (elems.length !== 2) throw errAt('not: expected 1 argument', expr.pos);
            const val = evalExpr(elems[1], env);
            return isTruthy(val) ? SCM_FALSE : SCM_TRUE;
          }
          case 'begin': {
            let result: SchemeVal = SCM_FALSE;
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i], env);
            }
            return result;
          }
          case 'set!': {
            if (elems.length !== 3) throw errAt('set!: bad syntax', expr.pos);
            const target = elems[1];
            if (target.tag !== 'symbol') throw errAt('set!: expected symbol', expr.pos);
            const val = evalExpr(elems[2], env);
            env.setExisting(target.value, val);
            return SCM_FALSE;
          }
          case 'cond': {
            for (let i = 1; i < elems.length; i++) {
              const clause = elems[i];
              if (clause.tag !== 'list' || clause.elements.length < 2)
                throw errAt('cond: bad clause', expr.pos);
              const test = clause.elements[0];
              if (test.tag === 'symbol' && test.value === 'else') {
                let result: SchemeVal = SCM_FALSE;
                for (let j = 1; j < clause.elements.length; j++) {
                  result = evalExpr(clause.elements[j], env);
                }
                return result;
              }
              const testVal = evalExpr(test, env);
              if (isTruthy(testVal)) {
                let result: SchemeVal = testVal;
                for (let j = 1; j < clause.elements.length; j++) {
                  result = evalExpr(clause.elements[j], env);
                }
                return result;
              }
            }
            return SCM_FALSE;
          }
          case 'let': {
            // Named let: (let name ((var init) ...) body...)
            if (elems.length >= 3 && elems[1].tag === 'symbol') {
              const name = elems[1].value;
              const bindingList = elems[2];
              if (bindingList.tag !== 'list') throw errAt('let: bad syntax', expr.pos);
              const paramNames: string[] = [];
              const initVals: SchemeVal[] = [];
              for (const b of bindingList.elements) {
                if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                  throw errAt('let: bad binding', expr.pos);
                paramNames.push(b.elements[0].value);
                initVals.push(evalExpr(b.elements[1], env));
              }
              const body = elems.slice(3);
              const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env };
              // Create env where name is bound to the lambda (for recursion)
              const callEnv = new Env(env);
              callEnv.set(name, lambda);
              // Also update the lambda's env to include itself
              (lambda as any).env = callEnv;
              for (let i = 0; i < paramNames.length; i++) {
                callEnv.set(paramNames[i], initVals[i]);
              }
              let result: SchemeVal = SCM_FALSE;
              for (const bodyExpr of body) {
                result = evalExpr(bodyExpr, callEnv);
              }
              return result;
            }
            // Regular let: (let ((var init) ...) body...)
            if (elems.length < 3) throw errAt('let: bad syntax', expr.pos);
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('let: bad syntax', expr.pos);
            const letEnv = new Env(env);
            for (const b of bindings.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('let: bad binding', expr.pos);
              const val = evalExpr(b.elements[1], env);
              letEnv.set(b.elements[0].value, val);
            }
            let result: SchemeVal = SCM_FALSE;
            for (let i = 2; i < elems.length; i++) {
              result = evalExpr(elems[i], letEnv);
            }
            return result;
          }
          case 'letrec': {
            if (elems.length < 3) throw errAt('letrec: bad syntax', expr.pos);
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('letrec: bad syntax', expr.pos);
            const letrecEnv = new Env(env);
            // First set all to undefined placeholder
            const names: string[] = [];
            for (const b of bindings.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('letrec: bad binding', expr.pos);
              names.push(b.elements[0].value);
              letrecEnv.set(b.elements[0].value, SCM_FALSE);
            }
            // Then evaluate init expressions in the letrec env
            for (let i = 0; i < bindings.elements.length; i++) {
              const b = bindings.elements[i];
              if (b.tag === 'list') {
                const val = evalExpr(b.elements[1], letrecEnv);
                letrecEnv.set(names[i], val);
              }
            }
            let result: SchemeVal = SCM_FALSE;
            for (let i = 2; i < elems.length; i++) {
              result = evalExpr(elems[i], letrecEnv);
            }
            return result;
          }
          case 'letrec*': {
            if (elems.length < 3) throw errAt('letrec*: bad syntax', expr.pos);
            const bindings = elems[1];
            if (bindings.tag !== 'list') throw errAt('letrec*: bad syntax', expr.pos);
            const letrecStarEnv = new Env(env);
            for (const b of bindings.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw errAt('letrec*: bad binding', expr.pos);
              const val = evalExpr(b.elements[1], letrecStarEnv);
              letrecStarEnv.set(b.elements[0].value, val);
            }
            let result: SchemeVal = SCM_FALSE;
            for (let i = 2; i < elems.length; i++) {
              result = evalExpr(elems[i], letrecStarEnv);
            }
            return result;
          }
          case 'case': {
            if (elems.length < 2) throw errAt('case: bad syntax', expr.pos);
            const key = evalExpr(elems[1], env);
            for (let i = 2; i < elems.length; i++) {
              const clause = elems[i];
              if (clause.tag !== 'list' || clause.elements.length < 2)
                throw errAt('case: bad clause', expr.pos);
              const datums = clause.elements[0];
              if (datums.tag === 'symbol' && datums.value === 'else') {
                let result: SchemeVal = SCM_FALSE;
                for (let j = 1; j < clause.elements.length; j++) {
                  result = evalExpr(clause.elements[j], env);
                }
                return result;
              }
              if (datums.tag !== 'list') throw errAt('case: expected datum list', expr.pos);
              for (const d of datums.elements) {
                const datum = listToPairs(d);
                if (schemeEqv(key, datum)) {
                  let result: SchemeVal = SCM_FALSE;
                  for (let j = 1; j < clause.elements.length; j++) {
                    result = evalExpr(clause.elements[j], env);
                  }
                  return result;
                }
              }
            }
            return SCM_FALSE; // no match, no else => void (we use #f)
          }
          case 'do': {
            // (do ((var init step) ...) (test expr ...) body ...)
            if (elems.length < 3) throw errAt('do: bad syntax', expr.pos);
            const bindingList = elems[1];
            if (bindingList.tag !== 'list') throw errAt('do: bad syntax', expr.pos);
            const testClause = elems[2];
            if (testClause.tag !== 'list' || testClause.elements.length < 1)
              throw errAt('do: bad test clause', expr.pos);

            const doEnv = new Env(env);
            const varNames: string[] = [];
            const stepExprs: (SchemeVal | null)[] = [];

            // Initialize variables
            for (const b of bindingList.elements) {
              if (b.tag !== 'list' || b.elements.length < 2 || b.elements[0].tag !== 'symbol')
                throw errAt('do: bad variable spec', expr.pos);
              const name = b.elements[0].value;
              const init = evalExpr(b.elements[1], env);
              varNames.push(name);
              stepExprs.push(b.elements.length >= 3 ? b.elements[2] : null);
              doEnv.set(name, init);
            }

            // Iteration loop
            while (true) {
              // Test
              const testVal = evalExpr(testClause.elements[0], doEnv);
              if (isTruthy(testVal)) {
                // Evaluate result expressions
                if (testClause.elements.length === 1) return testVal;
                let result: SchemeVal = SCM_FALSE;
                for (let j = 1; j < testClause.elements.length; j++) {
                  result = evalExpr(testClause.elements[j], doEnv);
                }
                return result;
              }
              // Execute body
              for (let j = 3; j < elems.length; j++) {
                evalExpr(elems[j], doEnv);
              }
              // Parallel step: evaluate all step expressions before updating
              const newVals: (SchemeVal | null)[] = [];
              for (let j = 0; j < varNames.length; j++) {
                if (stepExprs[j] !== null) {
                  newVals.push(evalExpr(stepExprs[j]!, doEnv));
                } else {
                  newVals.push(null);
                }
              }
              for (let j = 0; j < varNames.length; j++) {
                if (newVals[j] !== null) {
                  doEnv.set(varNames[j], newVals[j]!);
                }
              }
            }
          }
          case 'define-record-type': {
            // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
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

            // Parse field accessors
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

            // Constructor
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

            // Predicate
            const predLambda: SchemeVal = {
              tag: 'lambda', params: ['__x__'], body: [], env: new Env(),
            };
            (predLambda as any).nativeFn = (x: SchemeVal): SchemeVal => {
              return x.tag === 'record' && x.typeId === typeId ? SCM_TRUE : SCM_FALSE;
            };
            env.set(predName.value, predLambda);

            // Accessors
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

            return SCM_FALSE;
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
            return SCM_FALSE;
          }
        }
        // Check for macro
        try {
          const resolved = env.get(head.value);
          if (resolved.tag === 'macro') {
            return expandAndEvalMacro(resolved, elems, env, expr.pos);
          }
        } catch (e) { /* not bound, fall through */ }
      }

      // Function application
      const args = elems.slice(1).map(a => evalExpr(a, env));

      const proc = evalExpr(head, env);

      if (proc.tag === 'lambda') {
        return applyLambda(proc, args, expr.pos);
      }

      if (proc.tag === 'case-lambda') {
        return applyCaseLambda(proc, args, expr.pos);
      }

      if (proc.tag === 'builtin') {
        return applyBuiltin(proc.name, args, expr.pos);
      }

      throw errAt(`not a procedure: ${display(proc)}`, expr.pos);
    }
    default:
      throw errAt(`cannot evaluate: ${display(expr)}`, expr.pos);
  }
}

function applyLambda(proc: SchemeVal & { tag: 'lambda' }, args: SchemeVal[], pos?: Pos): SchemeVal {
  // Native functions (record constructors, predicates, accessors)
  const nativeFn = (proc as any).nativeFn;
  if (nativeFn) return nativeFn(...args);
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
  let result: SchemeVal = SCM_FALSE;
  for (const bodyExpr of proc.body) {
    result = evalExpr(bodyExpr, callEnv);
  }
  return result;
}

function applyCaseLambda(proc: SchemeVal & { tag: 'case-lambda' }, args: SchemeVal[], pos?: Pos): SchemeVal {
  for (const clause of proc.clauses) {
    if (clause.restParam) {
      if (args.length >= clause.params.length) {
        const callEnv = new Env(clause.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.set(clause.params[i], args[i]);
        }
        callEnv.set(clause.restParam, arrayToList(args.slice(clause.params.length)));
        let result: SchemeVal = SCM_FALSE;
        for (const bodyExpr of clause.body) {
          result = evalExpr(bodyExpr, callEnv);
        }
        return result;
      }
    } else {
      if (args.length === clause.params.length) {
        const callEnv = new Env(clause.env);
        for (let i = 0; i < clause.params.length; i++) {
          callEnv.set(clause.params[i], args[i]);
        }
        let result: SchemeVal = SCM_FALSE;
        for (const bodyExpr of clause.body) {
          result = evalExpr(bodyExpr, callEnv);
        }
        return result;
      }
    }
  }
  throw errAt(`case-lambda: no matching clause for ${args.length} arguments`, pos);
}

function applyProc(proc: SchemeVal, args: SchemeVal[], pos?: Pos): SchemeVal {
  if (proc.tag === 'lambda') return applyLambda(proc, args, pos);
  if (proc.tag === 'case-lambda') return applyCaseLambda(proc, args, pos);
  if (proc.tag === 'builtin') return applyBuiltin(proc.name, args, pos);
  throw errAt(`not a procedure: ${display(proc)}`, pos);
}

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
  'apply',
  'abs', 'modulo', 'remainder', 'quotient', 'min', 'max', 'expt',
  'zero?', 'positive?', 'negative?', 'odd?', 'even?',
  'list-ref', 'list-tail', 'list?', 'assoc', 'map',
  'eq?', 'eqv?', 'equal?',
  'vector', 'make-vector', 'vector-ref', 'vector-set!',
  'vector-length', 'vector?', 'vector->list', 'list->vector',
  'char-alphabetic?', 'char-numeric?', 'char-upcase', 'char-downcase',
  'char=?', 'char<?',
  'string=?', 'string<?', 'string-ci=?',
  'string-upcase', 'string-downcase',
  'exact?', 'inexact?', 'exact->inexact', 'inexact->exact',
  'numerator', 'denominator', 'integer?', 'rational?',
  'procedure?',
]);

function isBuiltin(name: string): boolean {
  return BUILTINS.has(name);
}

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
      return (args[0].tag === 'lambda' || args[0].tag === 'builtin' || args[0].tag === 'case-lambda') ? SCM_TRUE : SCM_FALSE;
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
      const si = args[1].value;
      if (si < 0 || si >= args[0].value.length)
        throw errAt('string-set!: index out of range', pos);
      (args[0] as any).value = args[0].value.substring(0, si) + args[2].value + args[0].value.substring(si + 1);
      return SCM_FALSE;
    }
    case 'apply': {
      if (args.length < 2) throw errAt('apply: expected at least 2 arguments', pos);
      const proc = args[0];
      const lastArg = args[args.length - 1];
      const tailList = pairToArray(lastArg);
      if (tailList === null && lastArg.tag !== 'nil')
        throw errAt('apply: last argument must be a proper list', pos);
      const prefixArgs = args.slice(1, args.length - 1);
      const allArgs = prefixArgs.concat(tailList ?? []);
      return applyProc(proc, allArgs, pos);
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
      let cur = args[0];
      while (cur.tag === 'pair') {
        cur = cur.cdr;
      }
      return cur.tag === 'nil' ? SCM_TRUE : SCM_FALSE;
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
    case 'map': {
      if (args.length < 2) throw errAt('map: expected at least 2 arguments', pos);
      const proc = args[0];
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
      for (let i = 0; i < len; i++) {
        const mapArgs = lists.map(l => l[i]);
        results.push(applyProc(proc, mapArgs, pos));
      }
      return arrayToList(results);
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
      // Convert float to rational: find closest fraction
      const v = toFloat(args[0]);
      if (Number.isInteger(v)) return { tag: 'number', value: v, exact: true };
      // Use continued fraction approximation
      const sign = v < 0 ? -1 : 1;
      const av = Math.abs(v);
      // Express as p/q by multiplying out the decimal
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
      if (args[0].tag === 'rational') return SCM_FALSE; // rationals that simplify to integers become numbers
      if (args[0].tag === 'number') return Number.isInteger(args[0].value) ? SCM_TRUE : SCM_FALSE;
      return SCM_FALSE;
    }
    case 'rational?': {
      if (args.length !== 1) throw errAt('rational?: expected 1 argument', pos);
      return isNumeric(args[0]) && isExact(args[0]) ? SCM_TRUE : SCM_FALSE;
    }
    default:
      throw errAt(`unbound variable: ${name}`, pos);
  }
}

// ── Output buffer ──────────────────────────────────────────────────────

let outputBuffer: string[] = [];

// ── Display ────────────────────────────────────────────────────────────

// write format: strings quoted
function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': {
      const s = String(val.value);
      // Inexact numbers should show decimal point
      if (val.exact === false && Number.isInteger(val.value) && !s.includes('.')) return s + '.0';
      return s;
    }
    case 'rational': return `${val.num}/${val.den}`;
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'nil': return '()';
    case 'pair': {
      let result = '(' + display(val.car);
      let cur: SchemeVal = val.cdr;
      while (cur.tag === 'pair') {
        result += ' ' + display(cur.car);
        cur = cur.cdr;
      }
      if (cur.tag !== 'nil') {
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
    let result = '(' + displayFormat(val.car);
    let cur: SchemeVal = val.cdr;
    while (cur.tag === 'pair') {
      result += ' ' + displayFormat(cur.car);
      cur = cur.cdr;
    }
    if (cur.tag !== 'nil') {
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
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return display(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  outputBuffer = [];
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return { result: display(result!), output: outputBuffer.join('') };
}
