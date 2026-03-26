import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }  // parse-time only
  | { tag: 'lambda'; params: string[]; restParam?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; pos?: Pos }
  | { tag: 'macro'; rules: MacroRule[]; defEnv: Env; pos?: Pos };

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
    const num = Number(tok.text);
    if (!isNaN(num) && tok.text !== '') {
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
  'set!', 'cond', 'let', 'define-syntax', 'syntax-rules',
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
    case 'boolean':
    case 'string':
    case 'char':
    case 'nil':
    case 'pair':
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

function applyProc(proc: SchemeVal, args: SchemeVal[], pos?: Pos): SchemeVal {
  if (proc.tag === 'lambda') return applyLambda(proc, args, pos);
  if (proc.tag === 'builtin') return applyBuiltin(proc.name, args, pos);
  throw errAt(`not a procedure: ${display(proc)}`, pos);
}

function schemeEq(a: SchemeVal, b: SchemeVal): boolean {
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

function schemeEqual(a: SchemeVal, b: SchemeVal): boolean {
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
  'eq?', 'equal?',
  'char-alphabetic?', 'char-numeric?', 'char-upcase', 'char-downcase',
  'char=?', 'char<?',
  'string=?', 'string<?', 'string-ci=?',
  'string-upcase', 'string-downcase',
]);

function isBuiltin(name: string): boolean {
  return BUILTINS.has(name);
}

function applyBuiltin(name: string, args: SchemeVal[], pos?: Pos): SchemeVal {
  switch (name) {
    case '+': {
      const nums = requireNumbers('+', args, pos);
      return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
    }
    case '-': {
      if (args.length === 0) throw errAt('-: expected at least 1 argument', pos);
      const nums = requireNumbers('-', args, pos);
      if (nums.length === 1) return { tag: 'number', value: -nums[0] };
      return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
    }
    case '*': {
      const nums = requireNumbers('*', args, pos);
      return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
    }
    case '/': {
      if (args.length < 2) throw errAt('/: expected at least 2 arguments', pos);
      const nums = requireNumbers('/', args, pos);
      if (nums[1] === 0) throw errAt('division by zero', pos);
      return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
    }
    case '<': {
      const nums = requireNumbers('<', args, pos);
      return { tag: 'boolean', value: nums[0] < nums[1] };
    }
    case '>': {
      const nums = requireNumbers('>', args, pos);
      return { tag: 'boolean', value: nums[0] > nums[1] };
    }
    case '=': {
      const nums = requireNumbers('=', args, pos);
      return { tag: 'boolean', value: nums[0] === nums[1] };
    }
    case '<=': {
      const nums = requireNumbers('<=', args, pos);
      return { tag: 'boolean', value: nums[0] <= nums[1] };
    }
    case '>=': {
      const nums = requireNumbers('>=', args, pos);
      return { tag: 'boolean', value: nums[0] >= nums[1] };
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
      return args[0].tag === 'number' ? SCM_TRUE : SCM_FALSE;
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
      if (args.length !== 1 || args[0].tag !== 'number')
        throw errAt('number->string: expected number', pos);
      return { tag: 'string', value: String(args[0].value) };
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
      const nums = requireNumbers('abs', args, pos);
      return { tag: 'number', value: Math.abs(nums[0]) };
    }
    case 'modulo': {
      if (args.length !== 2) throw errAt('modulo: expected 2 arguments', pos);
      const nums = requireNumbers('modulo', args, pos);
      if (nums[1] === 0) throw errAt('modulo: division by zero', pos);
      const r = nums[0] % nums[1];
      // modulo takes the sign of the divisor
      return { tag: 'number', value: (r !== 0 && Math.sign(r) !== Math.sign(nums[1])) ? r + nums[1] : r };
    }
    case 'remainder': {
      if (args.length !== 2) throw errAt('remainder: expected 2 arguments', pos);
      const nums = requireNumbers('remainder', args, pos);
      if (nums[1] === 0) throw errAt('remainder: division by zero', pos);
      // remainder takes the sign of the dividend (JS % does this)
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
      const nums = requireNumbers('min', args, pos);
      return { tag: 'number', value: Math.min(...nums) };
    }
    case 'max': {
      if (args.length === 0) throw errAt('max: expected at least 1 argument', pos);
      const nums = requireNumbers('max', args, pos);
      return { tag: 'number', value: Math.max(...nums) };
    }
    case 'expt': {
      if (args.length !== 2) throw errAt('expt: expected 2 arguments', pos);
      const nums = requireNumbers('expt', args, pos);
      return { tag: 'number', value: Math.pow(nums[0], nums[1]) };
    }
    case 'zero?': {
      if (args.length !== 1) throw errAt('zero?: expected 1 argument', pos);
      if (args[0].tag !== 'number') throw errAt('zero?: expected number', pos);
      return args[0].value === 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'positive?': {
      if (args.length !== 1) throw errAt('positive?: expected 1 argument', pos);
      if (args[0].tag !== 'number') throw errAt('positive?: expected number', pos);
      return args[0].value > 0 ? SCM_TRUE : SCM_FALSE;
    }
    case 'negative?': {
      if (args.length !== 1) throw errAt('negative?: expected 1 argument', pos);
      if (args[0].tag !== 'number') throw errAt('negative?: expected number', pos);
      return args[0].value < 0 ? SCM_TRUE : SCM_FALSE;
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
    case 'equal?': {
      if (args.length !== 2) throw errAt('equal?: expected 2 arguments', pos);
      return schemeEqual(args[0], args[1]) ? SCM_TRUE : SCM_FALSE;
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
    case 'number': return String(val.value);
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
    case 'builtin': return '#<procedure>';
    case 'macro': return '#<macro>';
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
