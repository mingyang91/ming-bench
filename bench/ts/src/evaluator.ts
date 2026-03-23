import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────

type Pos = { line: number; col: number };

type Bounce = { done: true; value: SchemeVal } | { done: false; thunk: () => Bounce };
type Kont = (val: SchemeVal) => Bounce;

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; mutable?: boolean; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'vector'; elements: SchemeVal[]; pos?: Pos }
  | { tag: 'lambda'; params: string[]; restParam?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; name: string; fn: (args: SchemeVal[], callPos?: Pos) => SchemeVal; pos?: Pos }
  | { tag: 'continuation'; fn: Kont; pos?: Pos }
  | { tag: 'callcc'; pos?: Pos }
  | { tag: 'macro'; literals: string[]; rules: SyntaxRule[]; defEnv: Env; pos?: Pos };

type SyntaxRule = { pattern: SchemeVal[]; template: SchemeVal };

function posError(msg: string, pos?: Pos): EvalError {
  if (pos) return new EvalError(`${pos.line}:${pos.col}: ${msg}`);
  return new EvalError(msg);
}

// ── Environment ───────────────────────────────────────────────────

class Env {
  private bindings: Map<string, SchemeVal> = new Map();
  constructor(private parent: Env | null = null) {}

  get(name: string, pos?: Pos): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name, pos);
    throw posError(`unbound variable: ${name}`, pos);
  }

  set(name: string, val: SchemeVal): void {
    this.bindings.set(name, val);
  }

  mutate(name: string, val: SchemeVal, pos?: Pos): void {
    if (this.bindings.has(name)) { this.bindings.set(name, val); return; }
    if (this.parent) { this.parent.mutate(name, val, pos); return; }
    throw posError(`set!: unbound variable: ${name}`, pos);
  }
}

// ── Parser ─────────────────────────────────────────────────────────

type Token = { text: string; pos: Pos };

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  let line = 1;
  let col = 1;
  while (i < input.length) {
    const ch = input[i];
    if (ch === '\n') { i++; line++; col = 1; continue; }
    if (ch === ' ' || ch === '\t' || ch === '\r') { i++; col++; continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') { i++; col++; } continue; }
    const startPos: Pos = { line, col };
    if (ch === '(' || ch === ')') { tokens.push({ text: ch, pos: startPos }); i++; col++; continue; }
    if (ch === "'") { tokens.push({ text: "'", pos: startPos }); i++; col++; continue; }
    if (ch === '"') {
      let s = '"';
      i++; col++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; i++; col++; if (i < input.length) { s += input[i]; i++; col++; } continue; }
        if (input[i] === '\n') { line++; col = 1; } else { col++; }
        s += input[i]; i++;
      }
      if (i < input.length) { s += '"'; i++; col++; }
      tokens.push({ text: s, pos: startPos });
      continue;
    }
    let atom = '';
    while (i < input.length && !" \t\n\r();\"'".includes(input[i])) { atom += input[i]; i++; col++; }
    if (atom.length > 0) tokens.push({ text: atom, pos: startPos });
  }
  return tokens;
}

function parse(tokens: Token[], cur: { i: number }): SchemeVal {
  if (cur.i >= tokens.length) throw new EvalError('unexpected end of input');
  const tok = tokens[cur.i];
  if (tok.text === "'") {
    cur.i++;
    const quoted = parse(tokens, cur);
    return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, quoted], pos: tok.pos };
  }
  if (tok.text === '(') {
    cur.i++;
    const elements: SchemeVal[] = [];
    while (cur.i < tokens.length && tokens[cur.i].text !== ')') elements.push(parse(tokens, cur));
    if (cur.i >= tokens.length) throw posError('missing closing paren', tok.pos);
    cur.i++;
    return { tag: 'list', elements, pos: tok.pos };
  }
  if (tok.text === ')') throw posError('unexpected )', tok.pos);
  cur.i++;
  return parseAtom(tok.text, tok.pos);
}

function parseAtom(token: string, pos: Pos): SchemeVal {
  if (token === '#t') return { tag: 'boolean', value: true, pos };
  if (token === '#f') return { tag: 'boolean', value: false, pos };
  if (token.startsWith('#\\')) {
    const charName = token.slice(2);
    if (charName === 'space') return { tag: 'char', value: ' ', pos };
    if (charName === 'newline') return { tag: 'char', value: '\n', pos };
    if (charName === 'tab') return { tag: 'char', value: '\t', pos };
    if (charName.length === 1) return { tag: 'char', value: charName, pos };
    throw posError(`bad character literal: ${token}`, pos);
  }
  if (token.startsWith('"') && token.endsWith('"')) {
    const inner = token.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
    return { tag: 'string', value: inner, pos };
  }
  const num = Number(token);
  if (!isNaN(num) && token !== '') return { tag: 'number', value: num, pos };
  return { tag: 'symbol', value: token, pos };
}

function parseAll(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const exprs: SchemeVal[] = [];
  const cur = { i: 0 };
  while (cur.i < tokens.length) exprs.push(parse(tokens, cur));
  return exprs;
}

function parseParams(elements: SchemeVal[], errPos?: Pos): { params: string[]; restParam?: string } {
  const dotIdx = elements.findIndex(e => e.tag === 'symbol' && e.value === '.');
  if (dotIdx === -1) {
    return { params: elements.map(p => { if (p.tag !== 'symbol') throw posError('param must be symbol', errPos); return p.value; }) };
  }
  if (dotIdx !== elements.length - 2) throw posError('bad dot syntax in params', errPos);
  const last = elements[elements.length - 1];
  if (last.tag !== 'symbol') throw posError('rest param must be symbol', errPos);
  const params = elements.slice(0, dotIdx).map(p => { if (p.tag !== 'symbol') throw posError('param must be symbol', errPos); return p.value; });
  return { params, restParam: last.value };
}

// ── Macro Support ─────────────────────────────────────────────────

let gensymCounter = 0;
function gensym(base: string): string { return `##${base}_${gensymCounter++}`; }

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'lambda', 'set!', 'begin', 'let', 'letrec', 'letrec*',
  'case', 'do', 'cond', 'and', 'or', 'define-syntax',
]);

function matchPattern(
  pattern: SchemeVal[], args: SchemeVal[], literals: string[],
): Map<string, SchemeVal | SchemeVal[]> | null {
  const bindings = new Map<string, SchemeVal | SchemeVal[]>();
  let pi = 0, ai = 0;
  while (pi < pattern.length) {
    const nextPat = pi + 1 < pattern.length ? pattern[pi + 1] : undefined;
    if (nextPat && nextPat.tag === 'symbol' && nextPat.value === '...') {
      const varPat = pattern[pi];
      if (varPat.tag !== 'symbol') return null;
      const remainingFixed = pattern.length - pi - 2;
      const available = args.length - ai - remainingFixed;
      if (available < 0) return null;
      bindings.set(varPat.value, args.slice(ai, ai + available));
      ai += available;
      pi += 2;
    } else {
      if (ai >= args.length) return null;
      const pat = pattern[pi];
      if (pat.tag === 'symbol') {
        if (literals.includes(pat.value)) {
          const arg = args[ai];
          if (arg.tag !== 'symbol' || arg.value !== pat.value) return null;
        } else {
          bindings.set(pat.value, args[ai]);
        }
      } else {
        return null;
      }
      pi++;
      ai++;
    }
  }
  if (ai !== args.length) return null;
  return bindings;
}

function findEllipsisVar(
  template: SchemeVal, bindings: Map<string, SchemeVal | SchemeVal[]>,
): string | null {
  if (template.tag === 'symbol') {
    const val = bindings.get(template.value);
    if (val !== undefined && Array.isArray(val)) return template.value;
    return null;
  }
  if (template.tag === 'list') {
    for (const elem of template.elements) {
      const found = findEllipsisVar(elem, bindings);
      if (found) return found;
    }
  }
  return null;
}

function instantiateTemplate(
  template: SchemeVal, bindings: Map<string, SchemeVal | SchemeVal[]>, renames: Map<string, string>,
): SchemeVal {
  if (template.tag === 'symbol') {
    const bound = bindings.get(template.value);
    if (bound !== undefined && !Array.isArray(bound)) return bound as SchemeVal;
    const renamed = renames.get(template.value);
    if (renamed !== undefined) return { tag: 'symbol', value: renamed };
    return template;
  }
  if (template.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < template.elements.length; i++) {
      const nextTpl = i + 1 < template.elements.length ? template.elements[i + 1] : undefined;
      if (nextTpl && nextTpl.tag === 'symbol' && nextTpl.value === '...') {
        const elem = template.elements[i];
        const listVar = findEllipsisVar(elem, bindings);
        if (listVar) {
          const listVals = bindings.get(listVar) as SchemeVal[];
          for (const val of listVals) {
            const singleBindings = new Map(bindings);
            singleBindings.set(listVar, val);
            result.push(instantiateTemplate(elem, singleBindings, renames));
          }
        }
        i++; // skip ellipsis
      } else {
        result.push(instantiateTemplate(template.elements[i], bindings, renames));
      }
    }
    return { tag: 'list', elements: result };
  }
  return template;
}

function collectTemplateSymbols(
  template: SchemeVal, patVars: Set<string>, renames: Map<string, string>,
): void {
  if (template.tag === 'symbol' && !patVars.has(template.value) &&
      !SPECIAL_FORMS.has(template.value) && !renames.has(template.value) &&
      template.value !== '...') {
    renames.set(template.value, gensym(template.value));
  }
  if (template.tag === 'list') {
    for (const elem of template.elements) collectTemplateSymbols(elem, patVars, renames);
  }
}

function expandMacro(
  macro: SchemeVal & { tag: 'macro' }, args: SchemeVal[], callEnv: Env, pos?: Pos,
): { expanded: SchemeVal; evalEnv: Env } {
  for (const rule of macro.rules) {
    const bindings = matchPattern(rule.pattern, args, macro.literals);
    if (bindings !== null) {
      const patVars = new Set(bindings.keys());
      const renames = new Map<string, string>();
      collectTemplateSymbols(rule.template, patVars, renames);
      const expanded = instantiateTemplate(rule.template, bindings, renames);
      const evalEnv = new Env(callEnv);
      for (const [origName, gensymName] of renames) {
        try { evalEnv.set(gensymName, macro.defEnv.get(origName)); } catch (_) { /* not bound */ }
      }
      return { expanded, evalEnv };
    }
  }
  throw posError('no matching syntax-rules pattern', pos);
}

// ── CPS Evaluator ──────────────────────────────────────────────────

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function expectNumber(val: SchemeVal, op: string, p?: Pos): number {
  if (val.tag !== 'number') throw posError(`${op}: expected number`, p);
  return val.value;
}

function done(v: SchemeVal): Bounce { return { done: true, value: v }; }
function bounce(thunk: () => Bounce): Bounce { return { done: false, thunk }; }

function trampoline(b: Bounce): SchemeVal {
  while (!b.done) b = b.thunk();
  return b.value;
}

let fuel = 0;

function evalK(expr: SchemeVal, env: Env, k: Kont): Bounce {
  if (++fuel > 500) {
    fuel = 0;
    return bounce(() => evalK(expr, env, k));
  }
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
    case 'void':
      return k(expr);
    case 'symbol':
      return k(env.get(expr.value, expr.pos));
    case 'list':
      break;
    default:
      return k(expr);
  }

  const elems = expr.elements;
  if (elems.length === 0) throw posError('empty application', expr.pos);
  const head = elems[0];

  if (head.tag === 'symbol') {
    switch (head.value) {
      case 'quote':
        if (elems.length !== 2) throw posError('quote: need 1 argument', expr.pos);
        return k(elems[1]);

      case 'if':
        if (elems.length < 3 || elems.length > 4) throw posError('if: bad syntax', expr.pos);
        return evalK(elems[1], env, cond => {
          if (isTruthy(cond)) return evalK(elems[2], env, k);
          if (elems.length === 4) return evalK(elems[3], env, k);
          return k({ tag: 'void' });
        });

      case 'define': {
        if (elems.length < 3) throw posError('define: bad syntax', expr.pos);
        const target = elems[1];
        if (target.tag === 'symbol') {
          return evalK(elems[2], env, val => {
            env.set(target.value, val);
            return k({ tag: 'void' });
          });
        }
        if (target.tag === 'list' && target.elements.length > 0 && target.elements[0].tag === 'symbol') {
          const fnName = target.elements[0].value;
          const { params, restParam } = parseParams(target.elements.slice(1), expr.pos);
          env.set(fnName, { tag: 'lambda', params, restParam, body: elems.slice(2), env });
          return k({ tag: 'void' });
        }
        throw posError('define: bad syntax', expr.pos);
      }

      case 'lambda': {
        if (elems.length < 3) throw posError('lambda: bad syntax', expr.pos);
        const paramList = elems[1];
        if (paramList.tag === 'symbol') {
          return k({ tag: 'lambda', params: [], restParam: paramList.value, body: elems.slice(2), env });
        }
        if (paramList.tag !== 'list') throw posError('lambda: params must be a list', expr.pos);
        const { params, restParam } = parseParams(paramList.elements, expr.pos);
        return k({ tag: 'lambda', params, restParam, body: elems.slice(2), env });
      }

      case 'set!': {
        if (elems.length !== 3) throw posError('set!: bad syntax', expr.pos);
        const target = elems[1];
        if (target.tag !== 'symbol') throw posError('set!: target must be a symbol', expr.pos);
        return evalK(elems[2], env, val => {
          env.mutate(target.value, val, expr.pos);
          return k({ tag: 'void' });
        });
      }

      case 'begin':
        if (elems.length === 1) return k({ tag: 'void' });
        return evalSeq(elems, 1, env, k);

      case 'let': {
        if (elems.length < 3) throw posError('let: bad syntax', expr.pos);

        // Named let
        if (elems[1].tag === 'symbol') {
          const loopName = elems[1].value;
          if (elems.length < 4) throw posError('let: bad syntax', expr.pos);
          const bindingList = elems[2];
          if (bindingList.tag !== 'list') throw posError('let: bindings must be a list', expr.pos);
          const paramNames: string[] = [];
          const initExprs: SchemeVal[] = [];
          for (const b of bindingList.elements) {
            if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
              throw posError('let: bad binding', expr.pos);
            paramNames.push(b.elements[0].value);
            initExprs.push(b.elements[1]);
          }
          const bodyExprs = elems.slice(3);
          const loopLambda: SchemeVal = { tag: 'lambda', params: paramNames, body: bodyExprs, env };
          const letEnv = new Env(env);
          letEnv.set(loopName, loopLambda);
          (loopLambda as any).env = letEnv;

          return evalList(initExprs, env, args => {
            const callEnv = new Env(letEnv);
            for (let i = 0; i < paramNames.length; i++) callEnv.set(paramNames[i], args[i]);
            return evalSeqArr(bodyExprs, callEnv, k);
          });
        }

        // Regular let
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw posError('let: bindings must be a list', expr.pos);
        const letEnv = new Env(env);

        const evalBindings = (i: number): Bounce => {
          if (i >= bindings.elements.length) {
            return evalSeq(elems, 2, letEnv, k);
          }
          const b = bindings.elements[i];
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw posError('let: bad binding', expr.pos);
          const bindName = b.elements[0].value;
          return evalK(b.elements[1], env, val => {
            letEnv.set(bindName, val);
            return evalBindings(i + 1);
          });
        };
        return evalBindings(0);
      }

      case 'letrec': {
        if (elems.length < 3) throw posError('letrec: bad syntax', expr.pos);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw posError('letrec: bindings must be a list', expr.pos);
        const letrecEnv = new Env(env);
        const names: string[] = [];
        const initExprs: SchemeVal[] = [];
        for (const b of bindings.elements) {
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw posError('letrec: bad binding', expr.pos);
          names.push(b.elements[0].value);
          initExprs.push(b.elements[1]);
          letrecEnv.set(b.elements[0].value, { tag: 'void' });
        }
        return evalList(initExprs, letrecEnv, vals => {
          for (let i = 0; i < names.length; i++) letrecEnv.set(names[i], vals[i]);
          return evalSeq(elems, 2, letrecEnv, k);
        });
      }

      case 'letrec*': {
        if (elems.length < 3) throw posError('letrec*: bad syntax', expr.pos);
        const bindings = elems[1];
        if (bindings.tag !== 'list') throw posError('letrec*: bindings must be a list', expr.pos);
        const letrecStarEnv = new Env(env);
        for (const b of bindings.elements) {
          if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
            throw posError('letrec*: bad binding', expr.pos);
          letrecStarEnv.set(b.elements[0].value, { tag: 'void' });
        }
        const evalLetrecStarBindings = (i: number): Bounce => {
          if (i >= bindings.elements.length) return evalSeq(elems, 2, letrecStarEnv, k);
          const b = bindings.elements[i];
          const name = (b as Extract<SchemeVal, {tag:'list'}>).elements[0] as Extract<SchemeVal, {tag:'symbol'}>;
          const initExpr = (b as Extract<SchemeVal, {tag:'list'}>).elements[1];
          return evalK(initExpr, letrecStarEnv, val => {
            letrecStarEnv.set(name.value, val);
            return evalLetrecStarBindings(i + 1);
          });
        };
        return evalLetrecStarBindings(0);
      }

      case 'case': {
        if (elems.length < 2) throw posError('case: bad syntax', expr.pos);
        return evalK(elems[1], env, keyVal => {
          const evalCaseClauses = (i: number): Bounce => {
            if (i >= elems.length) return k({ tag: 'void' });
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.elements.length < 2) throw posError('case: bad clause', expr.pos);
            if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
              return evalSeqArr(clause.elements.slice(1), env, k);
            }
            if (clause.elements[0].tag !== 'list') throw posError('case: datums must be a list', expr.pos);
            const datums = clause.elements[0].elements;
            let matched = false;
            for (const datum of datums) {
              if (schemeEqv(keyVal, datum)) { matched = true; break; }
            }
            if (matched) return evalSeqArr(clause.elements.slice(1), env, k);
            return evalCaseClauses(i + 1);
          };
          return evalCaseClauses(2);
        });
      }

      case 'do': {
        if (elems.length < 3) throw posError('do: bad syntax', expr.pos);
        const varSpecs = elems[1];
        if (varSpecs.tag !== 'list') throw posError('do: var specs must be a list', expr.pos);
        const testClause = elems[2];
        if (testClause.tag !== 'list' || testClause.elements.length < 1) throw posError('do: bad test clause', expr.pos);
        const bodyExprs = elems.slice(3);
        const varNames: string[] = [];
        const initExprs: SchemeVal[] = [];
        const stepExprs: (SchemeVal | null)[] = [];
        for (const spec of varSpecs.elements) {
          if (spec.tag !== 'list' || spec.elements.length < 2 || spec.elements[0].tag !== 'symbol')
            throw posError('do: bad var spec', expr.pos);
          varNames.push(spec.elements[0].value);
          initExprs.push(spec.elements[1]);
          stepExprs.push(spec.elements.length >= 3 ? spec.elements[2] : null);
        }
        return evalList(initExprs, env, initVals => {
          const doEnv = new Env(env);
          for (let i = 0; i < varNames.length; i++) doEnv.set(varNames[i], initVals[i]);
          const doLoop = (): Bounce => {
            return evalK(testClause.elements[0], doEnv, testResult => {
              if (isTruthy(testResult)) {
                if (testClause.elements.length > 1) {
                  return evalSeqArr(testClause.elements.slice(1), doEnv, k);
                }
                return k({ tag: 'void' });
              }
              const afterBody = (): Bounce => {
                // Evaluate all step expressions with current values (parallel)
                const stepsToEval: SchemeVal[] = [];
                const stepIndices: number[] = [];
                for (let i = 0; i < stepExprs.length; i++) {
                  if (stepExprs[i] !== null) {
                    stepsToEval.push(stepExprs[i]!);
                    stepIndices.push(i);
                  }
                }
                if (stepsToEval.length === 0) return bounce(doLoop);
                return evalList(stepsToEval, doEnv, stepVals => {
                  for (let j = 0; j < stepIndices.length; j++) {
                    doEnv.set(varNames[stepIndices[j]], stepVals[j]);
                  }
                  return bounce(doLoop);
                });
              };
              if (bodyExprs.length > 0) {
                return evalSeqArr(bodyExprs, doEnv, _ => afterBody());
              }
              return afterBody();
            });
          };
          return doLoop();
        });
      }

      case 'cond': {
        const evalClauses = (i: number): Bounce => {
          if (i >= elems.length) return k({ tag: 'void' });
          const clause = elems[i];
          if (clause.tag !== 'list' || clause.elements.length < 2) throw posError('cond: bad clause', expr.pos);
          if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
            return evalSeqArr(clause.elements.slice(1), env, k);
          }
          return evalK(clause.elements[0], env, test => {
            if (isTruthy(test)) return evalSeqArr(clause.elements.slice(1), env, k);
            return evalClauses(i + 1);
          });
        };
        return evalClauses(1);
      }

      case 'and': {
        if (elems.length === 1) return k({ tag: 'boolean', value: true });
        const evalAnds = (i: number): Bounce => {
          if (i === elems.length - 1) return evalK(elems[i], env, k);
          return evalK(elems[i], env, val => {
            if (!isTruthy(val)) return k(val);
            return evalAnds(i + 1);
          });
        };
        return evalAnds(1);
      }

      case 'or': {
        if (elems.length === 1) return k({ tag: 'boolean', value: false });
        const evalOrs = (i: number): Bounce => {
          if (i === elems.length - 1) return evalK(elems[i], env, k);
          return evalK(elems[i], env, val => {
            if (isTruthy(val)) return k(val);
            return evalOrs(i + 1);
          });
        };
        return evalOrs(1);
      }

      case 'define-syntax': {
        if (elems.length !== 3) throw posError('define-syntax: bad syntax', expr.pos);
        const name = elems[1];
        if (name.tag !== 'symbol') throw posError('define-syntax: name must be symbol', expr.pos);
        const transformer = elems[2];
        if (transformer.tag !== 'list' || transformer.elements.length < 2 ||
            transformer.elements[0].tag !== 'symbol' || transformer.elements[0].value !== 'syntax-rules')
          throw posError('define-syntax: expected syntax-rules', expr.pos);
        const litList = transformer.elements[1];
        if (litList.tag !== 'list') throw posError('syntax-rules: literals must be list', expr.pos);
        const literals = litList.elements.map(e => {
          if (e.tag !== 'symbol') throw posError('syntax-rules: literal must be symbol', expr.pos);
          return e.value;
        });
        const rules: SyntaxRule[] = [];
        for (let i = 2; i < transformer.elements.length; i++) {
          const clause = transformer.elements[i];
          if (clause.tag !== 'list' || clause.elements.length !== 2)
            throw posError('syntax-rules: bad clause', expr.pos);
          const pat = clause.elements[0];
          if (pat.tag !== 'list') throw posError('syntax-rules: pattern must be list', expr.pos);
          rules.push({ pattern: pat.elements.slice(1), template: clause.elements[1] });
        }
        env.set(name.value, { tag: 'macro', literals, rules, defEnv: env });
        return k({ tag: 'void' });
      }
    }

    // Check for macro application
    let macroVal: SchemeVal | undefined;
    try { macroVal = env.get(head.value); } catch (_) { /* unbound */ }
    if (macroVal && macroVal.tag === 'macro') {
      const { expanded, evalEnv } = expandMacro(macroVal, elems.slice(1), env, expr.pos);
      return evalK(expanded, evalEnv, k);
    }
  }

  // Function application: evaluate head, then args, then apply
  return evalK(head, env, proc =>
    evalList(elems.slice(1), env, args =>
      applyK(proc, args, k, expr.pos)
    )
  );
}

// Evaluate a sequence of expressions from elems[start..], return last value
function evalSeq(elems: SchemeVal[], start: number, env: Env, k: Kont): Bounce {
  if (start >= elems.length) return k({ tag: 'void' });
  const loop = (i: number): Bounce => {
    if (i === elems.length - 1) return evalK(elems[i], env, k);
    return evalK(elems[i], env, _ => loop(i + 1));
  };
  return loop(start);
}

// Same but for a standalone array
function evalSeqArr(exprs: SchemeVal[], env: Env, k: Kont): Bounce {
  if (exprs.length === 0) return k({ tag: 'void' });
  const loop = (i: number): Bounce => {
    if (i === exprs.length - 1) return evalK(exprs[i], env, k);
    return evalK(exprs[i], env, _ => loop(i + 1));
  };
  return loop(0);
}

// Evaluate a list of expressions left-to-right, collect results
function evalList(exprs: SchemeVal[], env: Env, k: (vals: SchemeVal[]) => Bounce): Bounce {
  const n = exprs.length;
  const results: SchemeVal[] = new Array(n);
  const loop = (i: number): Bounce => {
    if (i < 0) return k(results);
    return evalK(exprs[i], env, val => {
      results[i] = val;
      return loop(i - 1);
    });
  };
  return loop(n - 1);
}

// Apply a procedure to arguments in CPS
function applyK(proc: SchemeVal, args: SchemeVal[], k: Kont, pos?: Pos): Bounce {
  if (proc.tag === 'callcc') {
    if (args.length !== 1) throw posError('call/cc: need 1 argument', pos);
    const kontVal: SchemeVal = { tag: 'continuation', fn: k };
    return applyK(args[0], [kontVal], k, pos);
  }

  if (proc.tag === 'continuation') {
    return proc.fn(args.length > 0 ? args[0] : { tag: 'void' });
  }

  if (proc.tag === 'builtin' && proc.name === 'map') {
    if (args.length < 2) throw posError('map: need at least 2 arguments', pos);
    const fn = args[0];
    const lists = args.slice(1);
    for (const l of lists) if (l.tag !== 'list') throw posError('map: expected list', pos);
    const len = (lists[0] as Extract<SchemeVal, {tag:'list'}>).elements.length;
    const results: SchemeVal[] = [];
    const mapLoop = (i: number): Bounce => {
      if (i >= len) return k({ tag: 'list', elements: results });
      const fnArgs = lists.map(l => (l as Extract<SchemeVal, {tag:'list'}>).elements[i]);
      return applyK(fn, fnArgs, val => { results.push(val); return mapLoop(i + 1); }, pos);
    };
    return mapLoop(0);
  }

  if (proc.tag === 'builtin' && proc.name === 'apply') {
    if (args.length < 2) throw posError('apply: need at least 2 arguments', pos);
    const fn = args[0];
    const lastArg = args[args.length - 1];
    if (lastArg.tag !== 'list') throw posError('apply: last argument must be a list', pos);
    const allArgs = [...args.slice(1, -1), ...lastArg.elements];
    return applyK(fn, allArgs, k, pos);
  }

  if (proc.tag === 'lambda') {
    if (proc.restParam) {
      if (args.length < proc.params.length) throw posError('wrong number of arguments', pos);
    } else {
      if (args.length !== proc.params.length) throw posError('wrong number of arguments', pos);
    }
    const callEnv = new Env(proc.env);
    for (let i = 0; i < proc.params.length; i++) callEnv.set(proc.params[i], args[i]);
    if (proc.restParam) callEnv.set(proc.restParam, { tag: 'list', elements: args.slice(proc.params.length) });
    return bounce(() => evalSeqArr(proc.body, callEnv, k));
  }

  if (proc.tag === 'builtin') {
    return k(proc.fn(args, pos));
  }

  throw posError('not a procedure', pos);
}

// ── Helpers ────────────────────────────────────────────────────────

function schemeEqv(a: SchemeVal, b: SchemeVal): boolean {
  if (a.tag !== b.tag) return false;
  switch (a.tag) {
    case 'number': return a.value === (b as typeof a).value;
    case 'boolean': return a.value === (b as typeof a).value;
    case 'symbol': return a.value === (b as typeof a).value;
    case 'char': return a.value === (b as typeof a).value;
    case 'void': return true;
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
    case 'void': return true;
    case 'list': {
      const bList = b as typeof a;
      if (a.elements.length !== bList.elements.length) return false;
      return a.elements.every((e, i) => schemeEqual(e, bList.elements[i]));
    }
    case 'vector': {
      const bVec = b as typeof a;
      if (a.elements.length !== bVec.elements.length) return false;
      return a.elements.every((e, i) => schemeEqual(e, bVec.elements[i]));
    }
    default: return a === b;
  }
}

function expectChar(val: SchemeVal, op: string, p?: Pos): string {
  if (val.tag !== 'char') throw posError(`${op}: expected char`, p);
  return val.value;
}

function expectString(val: SchemeVal, op: string, p?: Pos): string {
  if (val.tag !== 'string') throw posError(`${op}: expected string`, p);
  return val.value;
}

// ── Builtins ──────────────────────────────────────────────────────

function makeGlobalEnv(output: string[] = []): Env {
  const env = new Env();

  function defBuiltin(name: string, fn: (args: SchemeVal[], p?: Pos) => SchemeVal) {
    env.set(name, { tag: 'builtin', name, fn });
  }

  // call/cc
  env.set('call/cc', { tag: 'callcc' });
  env.set('call-with-current-continuation', { tag: 'callcc' });

  // apply (handled specially in applyK, but needs a value in the env)
  env.set('apply', { tag: 'builtin', name: 'apply', fn: () => { throw new EvalError('internal: apply handled by applyK'); } });

  defBuiltin('+', (args, p) => { let s = 0; for (const a of args) s += expectNumber(a, '+', p); return { tag: 'number', value: s }; });
  defBuiltin('-', (args, p) => {
    if (args.length === 0) throw posError('-: need at least 1 argument', p);
    if (args.length === 1) return { tag: 'number', value: -expectNumber(args[0], '-', p) };
    let r = expectNumber(args[0], '-', p);
    for (let i = 1; i < args.length; i++) r -= expectNumber(args[i], '-', p);
    return { tag: 'number', value: r };
  });
  defBuiltin('*', (args, p) => { let s = 1; for (const a of args) s *= expectNumber(a, '*', p); return { tag: 'number', value: s }; });
  defBuiltin('/', (args, p) => {
    if (args.length < 2) throw posError('/: need at least 2 arguments', p);
    let r = expectNumber(args[0], '/', p);
    for (let i = 1; i < args.length; i++) { const d = expectNumber(args[i], '/', p); if (d === 0) throw posError('division by zero', p); r = Math.trunc(r / d); }
    return { tag: 'number', value: r };
  });
  defBuiltin('<', (args, p) => { if (args.length !== 2) throw posError('<: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '<', p) < expectNumber(args[1], '<', p) }; });
  defBuiltin('>', (args, p) => { if (args.length !== 2) throw posError('>: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '>', p) > expectNumber(args[1], '>', p) }; });
  defBuiltin('=', (args, p) => { if (args.length !== 2) throw posError('=: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '=', p) === expectNumber(args[1], '=', p) }; });
  defBuiltin('<=', (args, p) => { if (args.length !== 2) throw posError('<=: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '<=', p) <= expectNumber(args[1], '<=', p) }; });
  defBuiltin('>=', (args, p) => { if (args.length !== 2) throw posError('>=: need 2 arguments', p); return { tag: 'boolean', value: expectNumber(args[0], '>=', p) >= expectNumber(args[1], '>=', p) }; });
  defBuiltin('not', (args, p) => { if (args.length !== 1) throw posError('not: need 1 argument', p); return { tag: 'boolean', value: !isTruthy(args[0]) }; });

  // List operations
  defBuiltin('cons', (args, p) => {
    if (args.length !== 2) throw posError('cons: need 2 arguments', p);
    const cdr = args[1];
    if (cdr.tag === 'list') return { tag: 'list', elements: [args[0], ...cdr.elements] };
    return { tag: 'list', elements: [args[0], { tag: 'symbol', value: '.' }, cdr] };
  });
  defBuiltin('car', (args, p) => {
    if (args.length !== 1) throw posError('car: need 1 argument', p);
    if (args[0].tag !== 'list' || args[0].elements.length === 0) throw posError('car: not a pair', p);
    return args[0].elements[0];
  });
  defBuiltin('cdr', (args, p) => {
    if (args.length !== 1) throw posError('cdr: need 1 argument', p);
    if (args[0].tag !== 'list' || args[0].elements.length === 0) throw posError('cdr: not a pair', p);
    const elems = args[0].elements;
    // Dotted pair: (a . b) stored as [a, '.', b]
    if (elems.length === 3 && elems[1].tag === 'symbol' && elems[1].value === '.') {
      return elems[2];
    }
    // Dotted list with more elements: (a b . c) stored as [a, b, '.', c]
    const penult = elems[elems.length - 2];
    if (elems.length >= 4 && penult.tag === 'symbol' && penult.value === '.') {
      return { tag: 'list', elements: elems.slice(1) };
    }
    return { tag: 'list', elements: elems.slice(1) };
  });
  defBuiltin('null?', (args, p) => {
    if (args.length !== 1) throw posError('null?: need 1 argument', p);
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length === 0 };
  });
  defBuiltin('list', args => {
    return { tag: 'list', elements: args };
  });
  defBuiltin('length', (args, p) => {
    if (args.length !== 1 || args[0].tag !== 'list') throw posError('length: need a list', p);
    return { tag: 'number', value: args[0].elements.length };
  });
  defBuiltin('append', (args, p) => {
    const result: SchemeVal[] = [];
    for (const a of args) {
      if (a.tag !== 'list') throw posError('append: not a list', p);
      result.push(...a.elements);
    }
    return { tag: 'list', elements: result };
  });

  // Type predicates
  defBuiltin('number?', (args, p) => { if (args.length !== 1) throw posError('number?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'number' }; });
  defBuiltin('string?', (args, p) => { if (args.length !== 1) throw posError('string?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'string' }; });
  defBuiltin('boolean?', (args, p) => { if (args.length !== 1) throw posError('boolean?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'boolean' }; });
  defBuiltin('pair?', (args, p) => { if (args.length !== 1) throw posError('pair?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length > 0 }; });
  defBuiltin('symbol?', (args, p) => { if (args.length !== 1) throw posError('symbol?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'symbol' }; });
  defBuiltin('char?', (args, p) => { if (args.length !== 1) throw posError('char?: need 1 argument', p); return { tag: 'boolean', value: args[0].tag === 'char' }; });
  defBuiltin('procedure?', (args, p) => {
    if (args.length !== 1) throw posError('procedure?: need 1 argument', p);
    const t = args[0].tag;
    return { tag: 'boolean', value: t === 'lambda' || t === 'builtin' || t === 'continuation' || t === 'callcc' };
  });

  // Output
  defBuiltin('display', (args, p) => {
    if (args.length !== 1) throw posError('display: need 1 argument', p);
    output.push(displayForDisplay(args[0]));
    return { tag: 'void' };
  });
  defBuiltin('write', (args, p) => {
    if (args.length !== 1) throw posError('write: need 1 argument', p);
    output.push(writeVal(args[0]));
    return { tag: 'void' };
  });
  defBuiltin('newline', (args, p) => {
    if (args.length !== 0) throw posError('newline: need 0 arguments', p);
    output.push('\n');
    return { tag: 'void' };
  });

  // String operations
  defBuiltin('string-append', (args, p) => {
    let result = '';
    for (const a of args) {
      if (a.tag !== 'string') throw posError('string-append: expected string', p);
      result += a.value;
    }
    return { tag: 'string', value: result };
  });
  defBuiltin('string-length', (args, p) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw posError('string-length: expected string', p);
    return { tag: 'number', value: args[0].value.length };
  });
  defBuiltin('substring', (args, p) => {
    if (args.length !== 3) throw posError('substring: need 3 arguments', p);
    if (args[0].tag !== 'string') throw posError('substring: expected string', p);
    const start = expectNumber(args[1], 'substring', p);
    const end = expectNumber(args[2], 'substring', p);
    return { tag: 'string', value: args[0].value.slice(start, end) };
  });
  defBuiltin('string->number', (args, p) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw posError('string->number: expected string', p);
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  });
  defBuiltin('number->string', (args, p) => {
    if (args.length !== 1) throw posError('number->string: need 1 argument', p);
    return { tag: 'string', value: String(expectNumber(args[0], 'number->string', p)) };
  });
  defBuiltin('symbol->string', (args, p) => {
    if (args.length !== 1 || args[0].tag !== 'symbol') throw posError('symbol->string: expected symbol', p);
    return { tag: 'string', value: args[0].value };
  });
  defBuiltin('string->symbol', (args, p) => {
    if (args.length !== 1 || args[0].tag !== 'string') throw posError('string->symbol: expected string', p);
    return { tag: 'symbol', value: args[0].value };
  });
  defBuiltin('string-ref', (args, p) => {
    if (args.length !== 2) throw posError('string-ref: need 2 arguments', p);
    if (args[0].tag !== 'string') throw posError('string-ref: expected string', p);
    const idx = expectNumber(args[1], 'string-ref', p);
    if (idx < 0 || idx >= args[0].value.length) throw posError('string-ref: index out of range', p);
    return { tag: 'char', value: args[0].value[idx] };
  });
  defBuiltin('string-set!', (args, p) => {
    if (args.length !== 3) throw posError('string-set!: need 3 arguments', p);
    if (args[0].tag !== 'string') throw posError('string-set!: expected string', p);
    if (!args[0].mutable) throw posError('string-set!: string is immutable', p);
    const idx = expectNumber(args[1], 'string-set!', p);
    if (args[2].tag !== 'char') throw posError('string-set!: expected char', p);
    const s = args[0].value;
    if (idx < 0 || idx >= s.length) throw posError('string-set!: index out of range', p);
    (args[0] as any).value = s.substring(0, idx) + args[2].value + s.substring(idx + 1);
    return { tag: 'void' as const };
  });
  defBuiltin('string->list', (args, p) => {
    if (args.length !== 1) throw posError('string->list: need 1 argument', p);
    if (args[0].tag !== 'string') throw posError('string->list: expected string', p);
    const chars: SchemeVal[] = [];
    for (const ch of args[0].value) {
      chars.push({ tag: 'char', value: ch });
    }
    return { tag: 'list', elements: chars };
  });
  defBuiltin('list->string', (args, p) => {
    if (args.length !== 1) throw posError('list->string: need 1 argument', p);
    if (args[0].tag !== 'list') throw posError('list->string: expected list', p);
    let s = '';
    for (const item of args[0].elements) {
      if (item.tag !== 'char') throw posError('list->string: expected list of chars', p);
      s += item.value;
    }
    return { tag: 'string', value: s };
  });
  defBuiltin('char->integer', (args, p) => {
    if (args.length !== 1) throw posError('char->integer: need 1 argument', p);
    if (args[0].tag !== 'char') throw posError('char->integer: expected char', p);
    return { tag: 'number', value: args[0].value.codePointAt(0)! };
  });
  defBuiltin('integer->char', (args, p) => {
    if (args.length !== 1) throw posError('integer->char: need 1 argument', p);
    const n = expectNumber(args[0], 'integer->char', p);
    return { tag: 'char', value: String.fromCodePoint(n) };
  });
  defBuiltin('string-copy', (args, p) => {
    if (args.length !== 1) throw posError('string-copy: need 1 argument', p);
    if (args[0].tag !== 'string') throw posError('string-copy: expected string', p);
    return { tag: 'string', value: args[0].value, mutable: true };
  });

  // eq? / equal?
  defBuiltin('eq?', (args, p) => {
    if (args.length !== 2) throw posError('eq?: need 2 arguments', p);
    const [a, b] = args;
    if (a.tag !== b.tag) return { tag: 'boolean', value: false };
    switch (a.tag) {
      case 'number': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'boolean': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'symbol': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'char': return { tag: 'boolean', value: a.value === (b as typeof a).value };
      case 'void': return { tag: 'boolean', value: true };
      default: return { tag: 'boolean', value: a === b };
    }
  });
  defBuiltin('equal?', (args, p) => {
    if (args.length !== 2) throw posError('equal?: need 2 arguments', p);
    return { tag: 'boolean', value: schemeEqual(args[0], args[1]) };
  });
  defBuiltin('eqv?', (args, p) => {
    if (args.length !== 2) throw posError('eqv?: need 2 arguments', p);
    return { tag: 'boolean', value: schemeEqv(args[0], args[1]) };
  });

  // Vector operations
  defBuiltin('vector', args => ({ tag: 'vector', elements: [...args] }));
  defBuiltin('make-vector', (args, p) => {
    if (args.length < 1 || args.length > 2) throw posError('make-vector: need 1-2 arguments', p);
    const n = expectNumber(args[0], 'make-vector', p);
    const fill: SchemeVal = args.length === 2 ? args[1] : { tag: 'number', value: 0 };
    const elements: SchemeVal[] = new Array(n);
    for (let i = 0; i < n; i++) elements[i] = fill;
    return { tag: 'vector', elements };
  });
  defBuiltin('vector-ref', (args, p) => {
    if (args.length !== 2) throw posError('vector-ref: need 2 arguments', p);
    if (args[0].tag !== 'vector') throw posError('vector-ref: expected vector', p);
    const idx = expectNumber(args[1], 'vector-ref', p);
    if (idx < 0 || idx >= args[0].elements.length) throw posError('vector-ref: index out of range', p);
    return args[0].elements[idx];
  });
  defBuiltin('vector-set!', (args, p) => {
    if (args.length !== 3) throw posError('vector-set!: need 3 arguments', p);
    if (args[0].tag !== 'vector') throw posError('vector-set!: expected vector', p);
    const idx = expectNumber(args[1], 'vector-set!', p);
    if (idx < 0 || idx >= args[0].elements.length) throw posError('vector-set!: index out of range', p);
    args[0].elements[idx] = args[2];
    return { tag: 'void' };
  });
  defBuiltin('vector-length', (args, p) => {
    if (args.length !== 1) throw posError('vector-length: need 1 argument', p);
    if (args[0].tag !== 'vector') throw posError('vector-length: expected vector', p);
    return { tag: 'number', value: args[0].elements.length };
  });
  defBuiltin('vector?', (args, p) => {
    if (args.length !== 1) throw posError('vector?: need 1 argument', p);
    return { tag: 'boolean', value: args[0].tag === 'vector' };
  });
  defBuiltin('vector->list', (args, p) => {
    if (args.length !== 1) throw posError('vector->list: need 1 argument', p);
    if (args[0].tag !== 'vector') throw posError('vector->list: expected vector', p);
    return { tag: 'list', elements: [...args[0].elements] };
  });
  defBuiltin('list->vector', (args, p) => {
    if (args.length !== 1) throw posError('list->vector: need 1 argument', p);
    if (args[0].tag !== 'list') throw posError('list->vector: expected list', p);
    return { tag: 'vector', elements: [...args[0].elements] };
  });

  // error
  defBuiltin('error', (args, p) => {
    const msg = args.map(a => a.tag === 'string' ? a.value : displayVal(a)).join(' ');
    throw posError(msg, p);
  });

  // Numeric utilities
  defBuiltin('abs', (args, p) => {
    if (args.length !== 1) throw posError('abs: need 1 argument', p);
    return { tag: 'number', value: Math.abs(expectNumber(args[0], 'abs', p)) };
  });
  defBuiltin('modulo', (args, p) => {
    if (args.length !== 2) throw posError('modulo: need 2 arguments', p);
    const a = expectNumber(args[0], 'modulo', p);
    const b = expectNumber(args[1], 'modulo', p);
    return { tag: 'number', value: ((a % b) + b) % b };
  });
  defBuiltin('remainder', (args, p) => {
    if (args.length !== 2) throw posError('remainder: need 2 arguments', p);
    const a = expectNumber(args[0], 'remainder', p);
    const b = expectNumber(args[1], 'remainder', p);
    return { tag: 'number', value: a % b };
  });
  defBuiltin('quotient', (args, p) => {
    if (args.length !== 2) throw posError('quotient: need 2 arguments', p);
    const a = expectNumber(args[0], 'quotient', p);
    const b = expectNumber(args[1], 'quotient', p);
    return { tag: 'number', value: Math.trunc(a / b) };
  });
  defBuiltin('min', (args, p) => {
    if (args.length < 1) throw posError('min: need at least 1 argument', p);
    let m = expectNumber(args[0], 'min', p);
    for (let i = 1; i < args.length; i++) { const v = expectNumber(args[i], 'min', p); if (v < m) m = v; }
    return { tag: 'number', value: m };
  });
  defBuiltin('max', (args, p) => {
    if (args.length < 1) throw posError('max: need at least 1 argument', p);
    let m = expectNumber(args[0], 'max', p);
    for (let i = 1; i < args.length; i++) { const v = expectNumber(args[i], 'max', p); if (v > m) m = v; }
    return { tag: 'number', value: m };
  });
  defBuiltin('expt', (args, p) => {
    if (args.length !== 2) throw posError('expt: need 2 arguments', p);
    const base = expectNumber(args[0], 'expt', p);
    const exp = expectNumber(args[1], 'expt', p);
    return { tag: 'number', value: Math.pow(base, exp) };
  });

  // Numeric predicates
  defBuiltin('zero?', (args, p) => { if (args.length !== 1) throw posError('zero?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'zero?', p) === 0 }; });
  defBuiltin('positive?', (args, p) => { if (args.length !== 1) throw posError('positive?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'positive?', p) > 0 }; });
  defBuiltin('negative?', (args, p) => { if (args.length !== 1) throw posError('negative?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'negative?', p) < 0 }; });
  defBuiltin('odd?', (args, p) => { if (args.length !== 1) throw posError('odd?: need 1 argument', p); return { tag: 'boolean', value: Math.abs(expectNumber(args[0], 'odd?', p)) % 2 === 1 }; });
  defBuiltin('even?', (args, p) => { if (args.length !== 1) throw posError('even?: need 1 argument', p); return { tag: 'boolean', value: expectNumber(args[0], 'even?', p) % 2 === 0 }; });

  // List utilities
  defBuiltin('list-ref', (args, p) => {
    if (args.length !== 2) throw posError('list-ref: need 2 arguments', p);
    if (args[0].tag !== 'list') throw posError('list-ref: expected list', p);
    const idx = expectNumber(args[1], 'list-ref', p);
    if (idx < 0 || idx >= args[0].elements.length) throw posError('list-ref: index out of range', p);
    return args[0].elements[idx];
  });
  defBuiltin('list-tail', (args, p) => {
    if (args.length !== 2) throw posError('list-tail: need 2 arguments', p);
    if (args[0].tag !== 'list') throw posError('list-tail: expected list', p);
    const idx = expectNumber(args[1], 'list-tail', p);
    return { tag: 'list', elements: args[0].elements.slice(idx) };
  });
  defBuiltin('list?', (args, p) => {
    if (args.length !== 1) throw posError('list?: need 1 argument', p);
    const v = args[0];
    if (v.tag !== 'list') return { tag: 'boolean', value: false };
    for (const e of v.elements) {
      if (e.tag === 'symbol' && e.value === '.') return { tag: 'boolean', value: false };
    }
    return { tag: 'boolean', value: true };
  });
  defBuiltin('assoc', (args, p) => {
    if (args.length !== 2) throw posError('assoc: need 2 arguments', p);
    const key = args[0];
    const alist = args[1];
    if (alist.tag !== 'list') throw posError('assoc: expected list', p);
    for (const entry of alist.elements) {
      if (entry.tag === 'list' && entry.elements.length >= 1 && schemeEqual(key, entry.elements[0])) return entry;
    }
    return { tag: 'boolean', value: false };
  });

  // map (CPS-aware, handled in applyK)
  env.set('map', { tag: 'builtin', name: 'map', fn: () => { throw new EvalError('internal: map handled by applyK'); } });

  // Character operations
  defBuiltin('char-alphabetic?', (args, p) => { if (args.length !== 1) throw posError('char-alphabetic?: need 1 argument', p); const c = expectChar(args[0], 'char-alphabetic?', p); return { tag: 'boolean', value: /^[a-zA-Z]$/.test(c) }; });
  defBuiltin('char-numeric?', (args, p) => { if (args.length !== 1) throw posError('char-numeric?: need 1 argument', p); const c = expectChar(args[0], 'char-numeric?', p); return { tag: 'boolean', value: /^[0-9]$/.test(c) }; });
  defBuiltin('char-upcase', (args, p) => { if (args.length !== 1) throw posError('char-upcase: need 1 argument', p); return { tag: 'char', value: expectChar(args[0], 'char-upcase', p).toUpperCase() }; });
  defBuiltin('char-downcase', (args, p) => { if (args.length !== 1) throw posError('char-downcase: need 1 argument', p); return { tag: 'char', value: expectChar(args[0], 'char-downcase', p).toLowerCase() }; });
  defBuiltin('char=?', (args, p) => { if (args.length !== 2) throw posError('char=?: need 2 arguments', p); return { tag: 'boolean', value: expectChar(args[0], 'char=?', p) === expectChar(args[1], 'char=?', p) }; });
  defBuiltin('char<?', (args, p) => { if (args.length !== 2) throw posError('char<?: need 2 arguments', p); return { tag: 'boolean', value: expectChar(args[0], 'char<?', p) < expectChar(args[1], 'char<?', p) }; });

  // String comparison operations
  defBuiltin('string=?', (args, p) => { if (args.length !== 2) throw posError('string=?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string=?', p) === expectString(args[1], 'string=?', p) }; });
  defBuiltin('string<?', (args, p) => { if (args.length !== 2) throw posError('string<?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string<?', p) < expectString(args[1], 'string<?', p) }; });
  defBuiltin('string-ci=?', (args, p) => { if (args.length !== 2) throw posError('string-ci=?: need 2 arguments', p); return { tag: 'boolean', value: expectString(args[0], 'string-ci=?', p).toLowerCase() === expectString(args[1], 'string-ci=?', p).toLowerCase() }; });
  defBuiltin('string-upcase', (args, p) => { if (args.length !== 1) throw posError('string-upcase: need 1 argument', p); return { tag: 'string', value: expectString(args[0], 'string-upcase', p).toUpperCase() }; });
  defBuiltin('string-downcase', (args, p) => { if (args.length !== 1) throw posError('string-downcase: need 1 argument', p); return { tag: 'string', value: expectString(args[0], 'string-downcase', p).toLowerCase() }; });

  return env;
}

// ── Display ────────────────────────────────────────────────────────

function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
    case 'vector': return `#(${val.elements.map(displayVal).join(' ')})`;
    case 'void': return '';
    case 'lambda': return '#<procedure>';
    case 'builtin': return '#<procedure>';
    case 'continuation': return '#<procedure>';
    case 'callcc': return '#<procedure>';
    case 'macro': return '#<macro>';
  }
}

function writeVal(val: SchemeVal): string {
  return displayVal(val);
}

function displayForDisplay(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return val.value;
    case 'list': return `(${val.elements.map(displayForDisplay).join(' ')})`;
    case 'vector': return `#(${val.elements.map(displayForDisplay).join(' ')})`;
    case 'char': return val.value;
    default: return displayVal(val);
  }
}

// ── Public API ─────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  const result = trampoline(evalSeqArr(exprs, env, v => done(v)));
  return displayVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const output: string[] = [];
  const env = makeGlobalEnv(output);
  const result = trampoline(evalSeqArr(exprs, env, v => done(v)));
  return { result: displayVal(result), output: output.join('') };
}
