import { EvalError } from './evalError.js';

// --- Types ---

interface Pos { line: number; col: number }

type SchemeVal =
  | { tag: 'number'; value: number; pos?: Pos }
  | { tag: 'boolean'; value: boolean; pos?: Pos }
  | { tag: 'string'; value: string; pos?: Pos }
  | { tag: 'char'; value: string; pos?: Pos }
  | { tag: 'symbol'; value: string; pos?: Pos }
  | { tag: 'list'; elements: SchemeVal[]; pos?: Pos }  // AST only (parsed s-expr)
  | { tag: 'pair'; car: SchemeVal; cdr: SchemeVal; pos?: Pos }
  | { tag: 'nil'; pos?: Pos }
  | { tag: 'void'; pos?: Pos }
  | { tag: 'lambda'; params: string[]; restParam?: string; body: SchemeVal[]; env: Env; pos?: Pos }
  | { tag: 'builtin'; fn: (args: SchemeVal[]) => SchemeVal; pos?: Pos }
  | { tag: 'continuation'; id: symbol; callPos: string; exprIndex: number; bodyInfo?: { body: SchemeVal[]; idx: number; env: Env }; pos?: Pos }
  | { tag: 'callcc'; pos?: Pos }
  | { tag: 'macro'; literals: string[]; clauses: { pattern: SchemeVal; template: SchemeVal }[]; defEnv: Env; pos?: Pos };

// --- Continuation support ---

class ContinuationJump {
  constructor(
    public id: symbol,
    public value: SchemeVal,
    public exprIndex: number,
    public callPos: string,
    public bodyInfo?: { body: SchemeVal[]; idx: number; env: Env }
  ) {}
}

let currentExprIndex = 0;
let currentBodyInfo: { body: SchemeVal[]; idx: number; env: Env } | null = null;
let pendingContinuation: { callPos: string; value: SchemeVal; exprIndex: number } | null = null;

// --- Output buffer (for display/write/newline) ---
let outputBuffer = '';

function posStr(pos?: Pos): string {
  return pos ? `${pos.line}:${pos.col}` : '?:?';
}

const NIL: SchemeVal = { tag: 'nil' };

// --- Environment ---

interface Env {
  bindings: Map<string, SchemeVal>;
  parent: Env | null;
}

function makeEnv(parent: Env | null): Env {
  return { bindings: new Map(), parent };
}

function envLookup(env: Env, name: string, p?: Pos): SchemeVal {
  let cur: Env | null = env;
  while (cur) {
    const val = cur.bindings.get(name);
    if (val !== undefined) return val;
    cur = cur.parent;
  }
  throw new EvalError(`${posStr(p)}: unbound variable: ${name}`);
}

function envDefine(env: Env, name: string, val: SchemeVal): void {
  env.bindings.set(name, val);
}

function envSet(env: Env, name: string, val: SchemeVal, p?: Pos): void {
  let cur: Env | null = env;
  while (cur) {
    if (cur.bindings.has(name)) {
      cur.bindings.set(name, val);
      return;
    }
    cur = cur.parent;
  }
  throw new EvalError(`${posStr(p)}: set!: unbound variable: ${name}`);
}

// --- Tokenizer ---

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
    if (ch === "'") {
      tokens.push({ text: "'", pos: startPos });
      advance();
      continue;
    }
    if (ch === '(' || ch === ')') {
      tokens.push({ text: ch, pos: startPos });
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
    while (i < input.length && !' \t\n\r();"\''.includes(input[i])) {
      atom += input[i];
      advance();
    }
    if (atom.length > 0) {
      tokens.push({ text: atom, pos: startPos });
    }
  }
  return tokens;
}

// --- Parser ---

function parse(tokens: Token[]): SchemeVal[] {
  let idx = 0;

  function parseExpr(): SchemeVal {
    if (idx >= tokens.length) {
      throw new EvalError('unexpected end of input');
    }
    const tok = tokens[idx];
    if (tok.text === "'") {
      idx++;
      const inner = parseExpr();
      return { tag: 'list', elements: [{ tag: 'symbol', value: 'quote', pos: tok.pos }, inner], pos: tok.pos };
    }
    if (tok.text === '(') {
      idx++;
      const elements: SchemeVal[] = [];
      while (idx < tokens.length && tokens[idx].text !== ')') {
        elements.push(parseExpr());
      }
      if (idx >= tokens.length) {
        throw new EvalError(`${posStr(tok.pos)}: missing closing parenthesis`);
      }
      idx++;
      return { tag: 'list', elements, pos: tok.pos };
    }
    if (tok.text === ')') {
      throw new EvalError(`${posStr(tok.pos)}: unexpected )`);
    }
    idx++;
    return parseAtom(tok.text, tok.pos);
  }

  function parseAtom(token: string, p: Pos): SchemeVal {
    if (token === '#t') return { tag: 'boolean', value: true, pos: p };
    if (token === '#f') return { tag: 'boolean', value: false, pos: p };
    if (token.startsWith('#\\')) {
      const charName = token.slice(2);
      if (charName === 'space') return { tag: 'char', value: ' ', pos: p };
      if (charName === 'newline') return { tag: 'char', value: '\n', pos: p };
      if (charName === 'tab') return { tag: 'char', value: '\t', pos: p };
      if (charName.length === 1) return { tag: 'char', value: charName, pos: p };
      throw new EvalError(`${posStr(p)}: unknown character name: ${charName}`);
    }
    if (token.startsWith('"') && token.endsWith('"')) {
      const inner = token.slice(1, -1)
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
      return { tag: 'string', value: inner, pos: p };
    }
    const num = Number(token);
    if (!isNaN(num) && token !== '') {
      return { tag: 'number', value: num, pos: p };
    }
    return { tag: 'symbol', value: token, pos: p };
  }

  const exprs: SchemeVal[] = [];
  while (idx < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// --- Quote: convert AST list to runtime pair chain ---

function quoteDatum(val: SchemeVal): SchemeVal {
  if (val.tag === 'list') {
    let result: SchemeVal = NIL;
    for (let i = val.elements.length - 1; i >= 0; i--) {
      result = { tag: 'pair', car: quoteDatum(val.elements[i]), cdr: result };
    }
    return result;
  }
  return val;
}

// --- Helpers ---

function listToArray(val: SchemeVal): SchemeVal[] {
  const result: SchemeVal[] = [];
  let cur = val;
  while (cur.tag === 'pair') {
    result.push(cur.car);
    cur = cur.cdr;
  }
  if (cur.tag !== 'nil') throw new EvalError('not a proper list');
  return result;
}

function arrayToList(arr: SchemeVal[]): SchemeVal {
  let result: SchemeVal = NIL;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = { tag: 'pair', car: arr[i], cdr: result };
  }
  return result;
}

// --- Parameter parsing (dot notation) ---

function parseParamList(elements: SchemeVal[], pos?: Pos): { params: string[]; restParam?: string } {
  const dotIdx = elements.findIndex(e => e.tag === 'symbol' && e.value === '.');
  if (dotIdx === -1) {
    return { params: elements.map(p => {
      if (p.tag !== 'symbol') throw new EvalError(`${posStr(pos)}: parameter must be a symbol`);
      return p.value;
    }) };
  }
  if (dotIdx !== elements.length - 2) throw new EvalError(`${posStr(pos)}: invalid dot in parameter list`);
  const last = elements[elements.length - 1];
  if (last.tag !== 'symbol') throw new EvalError(`${posStr(pos)}: rest parameter must be a symbol`);
  const params = elements.slice(0, dotIdx).map(p => {
    if (p.tag !== 'symbol') throw new EvalError(`${posStr(pos)}: parameter must be a symbol`);
    return p.value;
  });
  return { params, restParam: last.value };
}

function applyLambda(proc: SchemeVal & { tag: 'lambda' }, args: SchemeVal[], pos?: Pos): Env {
  if (proc.restParam) {
    if (args.length < proc.params.length) {
      throw new EvalError(`${posStr(pos)}: expected at least ${proc.params.length} arguments, got ${args.length}`);
    }
  } else {
    if (args.length !== proc.params.length) {
      throw new EvalError(`${posStr(pos)}: expected ${proc.params.length} arguments, got ${args.length}`);
    }
  }
  const callEnv = makeEnv(proc.env);
  for (let i = 0; i < proc.params.length; i++) {
    envDefine(callEnv, proc.params[i], args[i]);
  }
  if (proc.restParam) {
    envDefine(callEnv, proc.restParam, arrayToList(args.slice(proc.params.length)));
  }
  return callEnv;
}

// --- Macro support ---

const SPECIAL_FORMS = new Set([
  'quote', 'if', 'define', 'lambda', 'and', 'or', 'not',
  'let', 'set!', 'begin', 'cond', 'define-syntax',
]);

let gensymCounter = 0;
function gensym(base: string): string {
  return `${base}$$${++gensymCounter}`;
}

type Bindings = Map<string, SchemeVal | SchemeVal[]>;

function collectPatternVars(pat: SchemeVal, literals: string[]): Set<string> {
  const vars = new Set<string>();
  function walk(p: SchemeVal) {
    if (p.tag === 'symbol') {
      if (p.value !== '...' && p.value !== '_' && !literals.includes(p.value)) {
        vars.add(p.value);
      }
    } else if (p.tag === 'list') {
      for (const e of p.elements) walk(e);
    }
  }
  walk(pat);
  return vars;
}

function matchPattern(pat: SchemeVal, input: SchemeVal, literals: string[], bindings: Bindings): boolean {
  if (pat.tag === 'symbol') {
    if (pat.value === '_') return true;
    if (pat.value === '...') return true;
    if (literals.includes(pat.value)) {
      return input.tag === 'symbol' && input.value === pat.value;
    }
    bindings.set(pat.value, input);
    return true;
  }
  if (pat.tag === 'list' && input.tag === 'list') {
    return matchListElems(pat.elements, input.elements, literals, bindings);
  }
  if (pat.tag === 'number' && input.tag === 'number') return pat.value === input.value;
  if (pat.tag === 'boolean' && input.tag === 'boolean') return pat.value === input.value;
  return false;
}

function matchListElems(pats: SchemeVal[], inputs: SchemeVal[], literals: string[], bindings: Bindings): boolean {
  let ellipsisAt = -1;
  for (let i = 1; i < pats.length; i++) {
    const p = pats[i];
    if (p.tag === 'symbol' && p.value === '...') {
      ellipsisAt = i;
      break;
    }
  }

  if (ellipsisAt === -1) {
    if (pats.length !== inputs.length) return false;
    for (let i = 0; i < pats.length; i++) {
      if (!matchPattern(pats[i], inputs[i], literals, bindings)) return false;
    }
    return true;
  }

  const beforeCount = ellipsisAt - 1;
  const afterCount = pats.length - ellipsisAt - 1;
  const ellipsisPat = pats[ellipsisAt - 1];

  if (inputs.length < beforeCount + afterCount) return false;

  for (let i = 0; i < beforeCount; i++) {
    if (!matchPattern(pats[i], inputs[i], literals, bindings)) return false;
  }

  const repeatCount = inputs.length - beforeCount - afterCount;
  if (ellipsisPat.tag === 'symbol' && !literals.includes(ellipsisPat.value) && ellipsisPat.value !== '_') {
    const matches: SchemeVal[] = [];
    for (let i = 0; i < repeatCount; i++) {
      matches.push(inputs[beforeCount + i]);
    }
    bindings.set(ellipsisPat.value, matches);
  }

  for (let i = 0; i < afterCount; i++) {
    if (!matchPattern(afterPats(pats, ellipsisAt, i), inputs[inputs.length - afterCount + i], literals, bindings)) return false;
  }

  return true;
}

function afterPats(pats: SchemeVal[], ellipsisAt: number, i: number): SchemeVal {
  return pats[ellipsisAt + 1 + i];
}

function findEllipsisVars(tmpl: SchemeVal, bindings: Bindings): string[] {
  const result: string[] = [];
  function walk(t: SchemeVal) {
    if (t.tag === 'symbol' && bindings.has(t.value) && Array.isArray(bindings.get(t.value))) {
      if (!result.includes(t.value)) result.push(t.value);
    } else if (t.tag === 'list') {
      for (const e of t.elements) walk(e);
    }
  }
  walk(tmpl);
  return result;
}

function instantiateTemplate(tmpl: SchemeVal, bindings: Bindings): SchemeVal {
  if (tmpl.tag === 'symbol') {
    if (bindings.has(tmpl.value)) {
      const val = bindings.get(tmpl.value)!;
      if (Array.isArray(val)) {
        throw new EvalError('ellipsis variable used outside ellipsis context');
      }
      return val;
    }
    return tmpl;
  }
  if (tmpl.tag === 'list') {
    const result: SchemeVal[] = [];
    for (let i = 0; i < tmpl.elements.length; i++) {
      const elem = tmpl.elements[i];
      const next = i + 1 < tmpl.elements.length ? tmpl.elements[i + 1] : null;
      if (next && next.tag === 'symbol' && next.value === '...') {
        const ellVars = findEllipsisVars(elem, bindings);
        if (ellVars.length > 0) {
          const listBinding = bindings.get(ellVars[0]);
          if (Array.isArray(listBinding)) {
            for (let j = 0; j < listBinding.length; j++) {
              const subBindings = new Map(bindings);
              for (const v of ellVars) {
                const vBinding = bindings.get(v);
                if (Array.isArray(vBinding)) {
                  subBindings.set(v, vBinding[j]);
                }
              }
              result.push(instantiateTemplate(elem, subBindings));
            }
          }
        }
        i++; // skip ...
        continue;
      }
      result.push(instantiateTemplate(elem, bindings));
    }
    return { tag: 'list', elements: result, pos: tmpl.pos };
  }
  return tmpl;
}

function collectIntroducedSymbols(tmpl: SchemeVal, patVars: Set<string>): Set<string> {
  const syms = new Set<string>();
  function walk(t: SchemeVal) {
    if (t.tag === 'symbol' && !patVars.has(t.value) && !SPECIAL_FORMS.has(t.value) && t.value !== '...') {
      syms.add(t.value);
    } else if (t.tag === 'list') {
      // Don't walk into quote
      if (t.elements.length > 0 && t.elements[0].tag === 'symbol' && t.elements[0].value === 'quote') return;
      for (const e of t.elements) walk(e);
    }
  }
  walk(tmpl);
  return syms;
}

function renameSymbols(ast: SchemeVal, renames: Map<string, string>): SchemeVal {
  if (ast.tag === 'symbol' && renames.has(ast.value)) {
    return { tag: 'symbol', value: renames.get(ast.value)!, pos: ast.pos };
  }
  if (ast.tag === 'list') {
    // Don't rename inside quote
    if (ast.elements.length > 0 && ast.elements[0].tag === 'symbol' && ast.elements[0].value === 'quote') return ast;
    return { tag: 'list', elements: ast.elements.map(e => renameSymbols(e, renames)), pos: ast.pos };
  }
  return ast;
}

function expandMacro(macro: SchemeVal & { tag: 'macro' }, form: SchemeVal[], useEnv: Env): { expanded: SchemeVal; hygieneEnv: Env } {
  for (const clause of macro.clauses) {
    const bindings: Bindings = new Map();
    const patElems = (clause.pattern as { tag: 'list'; elements: SchemeVal[] }).elements;
    // Match skipping macro name (first element)
    if (matchListElems(patElems.slice(1), form.slice(1), macro.literals, bindings)) {
      const patVars = collectPatternVars(clause.pattern, macro.literals);
      // Remove the macro name from patVars (first element of pattern)
      if (patElems[0].tag === 'symbol') patVars.delete(patElems[0].value);

      let expanded = instantiateTemplate(clause.template, bindings);

      // Hygiene: rename introduced symbols
      const introduced = collectIntroducedSymbols(clause.template, patVars);
      const renames = new Map<string, string>();
      const hygieneBindings = new Map<string, SchemeVal>();

      for (const sym of introduced) {
        const fresh = gensym(sym);
        renames.set(sym, fresh);
        // Try to resolve in definition env
        try {
          const val = envLookup(macro.defEnv, sym);
          hygieneBindings.set(fresh, val);
        } catch (_) {
          // Not found in def env - that's fine, it's a locally introduced name
        }
      }

      if (renames.size > 0) {
        expanded = renameSymbols(expanded, renames);
      }

      let hygieneEnv = useEnv;
      if (hygieneBindings.size > 0) {
        hygieneEnv = makeEnv(useEnv);
        for (const [name, val] of hygieneBindings) {
          envDefine(hygieneEnv, name, val);
        }
      }

      return { expanded, hygieneEnv };
    }
  }
  throw new EvalError('no matching pattern in syntax-rules');
}

// --- Evaluator ---

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function evalExpr(expr: SchemeVal, env: Env): SchemeVal {
  while (true) {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      return envLookup(env, expr.value, expr.pos);
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) {
        throw new EvalError(`${posStr(expr.pos)}: empty application`);
      }
      const head = elems[0];
      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'quote': {
            if (elems.length !== 2) throw new EvalError(`${posStr(expr.pos)}: quote: expected 1 argument`);
            return quoteDatum(elems[1]);
          }
          case 'if': {
            if (elems.length < 3 || elems.length > 4) throw new EvalError(`${posStr(expr.pos)}: if: expected 2 or 3 arguments`);
            const cond = evalExpr(elems[1], env);
            if (isTruthy(cond)) {
              expr = elems[2]; continue; // TCO
            } else if (elems.length === 4) {
              expr = elems[3]; continue; // TCO
            }
            return { tag: 'void' };
          }
          case 'define': {
            if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}: define: expected at least 2 arguments`);
            const target = elems[1];
            if (target.tag === 'symbol') {
              const val = evalExpr(elems[2], env);
              envDefine(env, target.value, val);
              return { tag: 'void' };
            }
            if (target.tag === 'list' && target.elements.length >= 1 && target.elements[0].tag === 'symbol') {
              const name = target.elements[0].value;
              const { params: defParams, restParam: defRest } = parseParamList(target.elements.slice(1), expr.pos);
              const body = elems.slice(2);
              const lambda: SchemeVal = { tag: 'lambda', params: defParams, restParam: defRest, body, env, pos: expr.pos };
              envDefine(env, name, lambda);
              return { tag: 'void' };
            }
            throw new EvalError(`${posStr(expr.pos)}: define: invalid syntax`);
          }
          case 'lambda': {
            if (elems.length < 3) throw new EvalError(`${posStr(expr.pos)}: lambda: expected at least 2 arguments`);
            const paramList = elems[1];
            if (paramList.tag === 'symbol') {
              // (lambda args body...) - all args as rest
              const body = elems.slice(2);
              return { tag: 'lambda', params: [], restParam: paramList.value, body, env, pos: expr.pos };
            }
            if (paramList.tag !== 'list') throw new EvalError(`${posStr(expr.pos)}: lambda: parameters must be a list`);
            const { params: lamParams, restParam: lamRest } = parseParamList(paramList.elements, expr.pos);
            const body = elems.slice(2);
            return { tag: 'lambda', params: lamParams, restParam: lamRest, body, env, pos: expr.pos };
          }
          case 'and': {
            const andExprs = elems.slice(1);
            if (andExprs.length === 0) return { tag: 'boolean', value: true };
            for (let i = 0; i < andExprs.length - 1; i++) {
              const result = evalExpr(andExprs[i], env);
              if (!isTruthy(result)) return result;
            }
            expr = andExprs[andExprs.length - 1]; continue; // TCO
          }
          case 'or': {
            const orExprs = elems.slice(1);
            if (orExprs.length === 0) return { tag: 'boolean', value: false };
            for (let i = 0; i < orExprs.length - 1; i++) {
              const result = evalExpr(orExprs[i], env);
              if (isTruthy(result)) return result;
            }
            expr = orExprs[orExprs.length - 1]; continue; // TCO
          }
          case 'not': {
            if (elems.length !== 2) throw new EvalError(`${posStr(expr.pos)}: not: expected 1 argument`);
            const val = evalExpr(elems[1], env);
            return { tag: 'boolean', value: !isTruthy(val) };
          }
          case 'let': {
            let idx = 1;
            let loopName: string | null = null;
            const first = elems[idx];
            if (first.tag === 'symbol') {
              loopName = first.value;
              idx++;
            }
            const bindingList = elems[idx];
            if (bindingList.tag !== 'list') throw new EvalError('let: bindings must be a list');
            idx++;
            const body = elems.slice(idx);
            const paramNames: string[] = [];
            const initVals: SchemeVal[] = [];
            for (const b of bindingList.elements) {
              if (b.tag !== 'list' || b.elements.length !== 2) throw new EvalError('let: invalid binding');
              if (b.elements[0].tag !== 'symbol') throw new EvalError('let: binding name must be a symbol');
              paramNames.push(b.elements[0].value);
              initVals.push(evalExpr(b.elements[1], env));
            }
            const letEnv = makeEnv(env);
            if (loopName) {
              const lambda: SchemeVal = { tag: 'lambda', params: paramNames, body, env: letEnv };
              envDefine(letEnv, loopName, lambda);
            }
            for (let i = 0; i < paramNames.length; i++) {
              envDefine(letEnv, paramNames[i], initVals[i]);
            }
            if (body.length === 0) return { tag: 'void' };
            const prevBodyInfo = currentBodyInfo;
            for (let i = 0; i < body.length - 1; i++) {
              currentBodyInfo = { body, idx: i, env: letEnv };
              evalExpr(body[i], letEnv);
            }
            currentBodyInfo = { body, idx: body.length - 1, env: letEnv };
            expr = body[body.length - 1]; env = letEnv; continue; // TCO
          }
          case 'set!': {
            if (elems.length !== 3) throw new EvalError(`${posStr(expr.pos)}: set!: expected 2 arguments`);
            const target = elems[1];
            if (target.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}: set!: expected symbol`);
            const val = evalExpr(elems[2], env);
            envSet(env, target.value, val, expr.pos);
            return { tag: 'void' };
          }
          case 'begin': {
            const bodyExprs = elems.slice(1);
            if (bodyExprs.length === 0) return { tag: 'void' };
            const prevBodyInfoBegin = currentBodyInfo;
            for (let i = 0; i < bodyExprs.length - 1; i++) {
              currentBodyInfo = { body: bodyExprs, idx: i, env };
              evalExpr(bodyExprs[i], env);
            }
            currentBodyInfo = { body: bodyExprs, idx: bodyExprs.length - 1, env };
            expr = bodyExprs[bodyExprs.length - 1]; continue; // TCO
          }
          case 'cond': {
            const clauses = elems.slice(1);
            let found = false;
            for (const clause of clauses) {
              if (clause.tag !== 'list' || clause.elements.length < 2) throw new EvalError('cond: invalid clause');
              const test = clause.elements[0];
              if (test.tag === 'symbol' && test.value === 'else') {
                for (let i = 1; i < clause.elements.length - 1; i++) {
                  evalExpr(clause.elements[i], env);
                }
                expr = clause.elements[clause.elements.length - 1];
                found = true;
                break;
              }
              const testVal = evalExpr(test, env);
              if (isTruthy(testVal)) {
                if (clause.elements.length === 1) return testVal;
                for (let i = 1; i < clause.elements.length - 1; i++) {
                  evalExpr(clause.elements[i], env);
                }
                expr = clause.elements[clause.elements.length - 1];
                found = true;
                break;
              }
            }
            if (found) continue; // TCO
            return { tag: 'void' };
          }
          case 'define-syntax': {
            if (elems.length !== 3) throw new EvalError(`${posStr(expr.pos)}: define-syntax: expected 2 arguments`);
            const nameNode = elems[1];
            if (nameNode.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}: define-syntax: expected symbol`);
            const transformer = elems[2];
            if (transformer.tag !== 'list' || transformer.elements.length < 2 ||
                transformer.elements[0].tag !== 'symbol' || transformer.elements[0].value !== 'syntax-rules') {
              throw new EvalError(`${posStr(expr.pos)}: define-syntax: expected syntax-rules`);
            }
            const srElems = transformer.elements;
            // (syntax-rules (literals...) clause...)
            if (srElems[1].tag !== 'list') throw new EvalError(`${posStr(expr.pos)}: syntax-rules: expected literal list`);
            const literals = srElems[1].elements.map(e => {
              if (e.tag !== 'symbol') throw new EvalError(`${posStr(expr.pos)}: syntax-rules: literal must be a symbol`);
              return e.value;
            });
            const macClauses: { pattern: SchemeVal; template: SchemeVal }[] = [];
            for (let ci = 2; ci < srElems.length; ci++) {
              const c = srElems[ci];
              if (c.tag !== 'list' || c.elements.length !== 2) throw new EvalError(`${posStr(expr.pos)}: syntax-rules: invalid clause`);
              macClauses.push({ pattern: c.elements[0], template: c.elements[1] });
            }
            const macroVal: SchemeVal = { tag: 'macro', literals, clauses: macClauses, defEnv: env, pos: expr.pos };
            envDefine(env, nameNode.value, macroVal);
            return { tag: 'void' };
          }
        }
        // Check if head symbol resolves to a macro
        try {
          const headVal = envLookup(env, head.value);
          if (headVal.tag === 'macro') {
            const { expanded, hygieneEnv } = expandMacro(headVal, elems, env);
            expr = expanded; env = hygieneEnv; continue; // TCO
          }
        } catch (_) { /* not bound or not a macro, fall through */ }
      }
      // Procedure application
      const proc = evalExpr(head, env);
      if (proc.tag === 'callcc') {
        const callPosKey = posStr(expr.pos);
        // Check for pending reentrant continuation at this source position
        if (pendingContinuation && pendingContinuation.callPos === callPosKey) {
          const val = pendingContinuation.value;
          pendingContinuation = null;
          return val;
        }
        const theLambda = evalExpr(elems[1], env);
        const id = Symbol();
        const capturedBodyInfo = currentBodyInfo ? { ...currentBodyInfo } : undefined;
        const contVal: SchemeVal = { tag: 'continuation', id, callPos: callPosKey, exprIndex: currentExprIndex, bodyInfo: capturedBodyInfo };
        // Apply the lambda to the continuation, with try/catch for escape
        try {
          return applyProcCallCC(theLambda, [contVal], expr.pos);
        } catch (e) {
          if (e instanceof ContinuationJump && e.id === id) {
            return e.value;
          }
          throw e;
        }
      }
      if (proc.tag === 'continuation') {
        const args = elems.slice(1).map(e => evalExpr(e, env));
        throw new ContinuationJump(proc.id, args[0] ?? { tag: 'void' }, proc.exprIndex, proc.callPos, proc.bodyInfo);
      }
      if (proc.tag === 'builtin') {
        const args = elems.slice(1).map(e => evalExpr(e, env));
        try {
          return proc.fn(args);
        } catch (e) {
          if (e instanceof ContinuationJump) throw e;
          if (e instanceof EvalError && !/^\d/.test(e.message)) {
            throw new EvalError(`${posStr(expr.pos)}: ${e.message}`);
          }
          throw e;
        }
      }
      if (proc.tag === 'lambda') {
        const args = elems.slice(1).map(e => evalExpr(e, env));
        const callEnv = applyLambda(proc, args, expr.pos);
        const prevBodyInfo2 = currentBodyInfo;
        for (let i = 0; i < proc.body.length - 1; i++) {
          currentBodyInfo = { body: proc.body, idx: i, env: callEnv };
          evalExpr(proc.body[i], callEnv);
        }
        currentBodyInfo = { body: proc.body, idx: proc.body.length - 1, env: callEnv };
        expr = proc.body[proc.body.length - 1]; env = callEnv; continue; // TCO
      }
      throw new EvalError(`${posStr(expr.pos)}: not a procedure`);
    }
    default:
      return expr;
  }
  }
}


// --- Helper for call/cc: apply proc without TCO so try/catch works ---

function applyProcCallCC(proc: SchemeVal, args: SchemeVal[], pos?: Pos): SchemeVal {
  if (proc.tag === 'lambda') {
    const callEnv = applyLambda(proc, args, pos);
    let result: SchemeVal = { tag: 'void' };
    for (const bodyExpr of proc.body) {
      result = evalExpr(bodyExpr, callEnv);
    }
    return result;
  }
  if (proc.tag === 'builtin') {
    return proc.fn(args);
  }
  if (proc.tag === 'continuation') {
    throw new ContinuationJump(proc.id, args[0] ?? { tag: 'void' }, proc.exprIndex, proc.callPos);
  }
  throw new EvalError(`${posStr(pos)}: call/cc: argument must be a procedure`);
}

// --- Arithmetic & Comparison ---

function requireNumbers(args: SchemeVal[], name: string): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number`);
    return a.value;
  });
}

function arith(args: SchemeVal[], op: string): SchemeVal {
  const nums = requireNumbers(args, op);
  if (nums.length === 0) {
    if (op === '+') return { tag: 'number', value: 0 };
    if (op === '*') return { tag: 'number', value: 1 };
    throw new EvalError(`${op}: expected at least 1 argument`);
  }
  if (op === '-' && nums.length === 1) {
    return { tag: 'number', value: -nums[0] };
  }
  let result = nums[0];
  for (let i = 1; i < nums.length; i++) {
    switch (op) {
      case '+': result += nums[i]; break;
      case '-': result -= nums[i]; break;
      case '*': result *= nums[i]; break;
      case '/':
        if (nums[i] === 0) throw new EvalError('division by zero');
        result = Math.trunc(result / nums[i]);
        break;
    }
  }
  return { tag: 'number', value: result };
}

function schemeCompare(args: SchemeVal[], op: string): SchemeVal {
  if (args.length !== 2) throw new EvalError(`${op}: expected 2 arguments`);
  const nums = requireNumbers(args, op);
  let result: boolean;
  switch (op) {
    case '<': result = nums[0] < nums[1]; break;
    case '>': result = nums[0] > nums[1]; break;
    case '=': result = nums[0] === nums[1]; break;
    case '<=': result = nums[0] <= nums[1]; break;
    default: result = false;
  }
  return { tag: 'boolean', value: result };
}

// --- Builtins ---

function schemeAppend(args: SchemeVal[]): SchemeVal {
  if (args.length === 0) return NIL;
  if (args.length === 1) return args[0];
  // append all lists
  let result = args[args.length - 1];
  for (let i = args.length - 2; i >= 0; i--) {
    const elems = listToArray(args[i]);
    for (let j = elems.length - 1; j >= 0; j--) {
      result = { tag: 'pair', car: elems[j], cdr: result };
    }
  }
  return result;
}

function makeGlobalEnv(): Env {
  const env = makeEnv(null);

  function defBuiltin(name: string, fn: (args: SchemeVal[]) => SchemeVal) {
    env.bindings.set(name, { tag: 'builtin', fn });
  }

  defBuiltin('+', args => arith(args, '+'));
  defBuiltin('-', args => arith(args, '-'));
  defBuiltin('*', args => arith(args, '*'));
  defBuiltin('/', args => arith(args, '/'));
  defBuiltin('<', args => schemeCompare(args, '<'));
  defBuiltin('>', args => schemeCompare(args, '>'));
  defBuiltin('=', args => schemeCompare(args, '='));
  defBuiltin('<=', args => schemeCompare(args, '<='));

  defBuiltin('cons', args => {
    if (args.length !== 2) throw new EvalError('cons: expected 2 arguments');
    return { tag: 'pair', car: args[0], cdr: args[1] };
  });
  defBuiltin('car', args => {
    if (args.length !== 1) throw new EvalError('car: expected 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('car: expected pair');
    return args[0].car;
  });
  defBuiltin('cdr', args => {
    if (args.length !== 1) throw new EvalError('cdr: expected 1 argument');
    if (args[0].tag !== 'pair') throw new EvalError('cdr: expected pair');
    return args[0].cdr;
  });
  defBuiltin('null?', args => {
    if (args.length !== 1) throw new EvalError('null?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'nil' };
  });
  defBuiltin('list', args => arrayToList(args));
  defBuiltin('length', args => {
    if (args.length !== 1) throw new EvalError('length: expected 1 argument');
    const elems = listToArray(args[0]);
    return { tag: 'number', value: elems.length };
  });
  defBuiltin('append', args => schemeAppend(args));

  defBuiltin('apply', args => {
    if (args.length < 2) throw new EvalError('apply: expected at least 2 arguments');
    const proc = args[0];
    const lastArg = args[args.length - 1];
    const tailArgs = listToArray(lastArg);
    const prefixArgs = args.slice(1, -1);
    const allArgs = [...prefixArgs, ...tailArgs];
    if (proc.tag === 'builtin') {
      return proc.fn(allArgs);
    }
    if (proc.tag === 'lambda') {
      const callEnv = applyLambda(proc, allArgs);
      let result: SchemeVal = { tag: 'void' };
      for (const bodyExpr of proc.body) {
        result = evalExpr(bodyExpr, callEnv);
      }
      return result;
    }
    if (proc.tag === 'continuation') {
      throw new ContinuationJump(proc.id, allArgs[0] ?? { tag: 'void' }, proc.exprIndex, proc.callPos, proc.bodyInfo);
    }
    throw new EvalError('apply: first argument must be a procedure');
  });

  // Type predicates
  defBuiltin('string?', args => {
    if (args.length !== 1) throw new EvalError('string?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'string' };
  });
  defBuiltin('number?', args => {
    if (args.length !== 1) throw new EvalError('number?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'number' };
  });
  defBuiltin('boolean?', args => {
    if (args.length !== 1) throw new EvalError('boolean?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  });
  defBuiltin('pair?', args => {
    if (args.length !== 1) throw new EvalError('pair?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'pair' };
  });
  defBuiltin('symbol?', args => {
    if (args.length !== 1) throw new EvalError('symbol?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  // I/O builtins
  defBuiltin('display', args => {
    if (args.length !== 1) throw new EvalError('display: expected 1 argument');
    outputBuffer += displayValUnquoted(args[0]);
    return { tag: 'void' };
  });
  defBuiltin('write', args => {
    if (args.length !== 1) throw new EvalError('write: expected 1 argument');
    outputBuffer += displayVal(args[0]);
    return { tag: 'void' };
  });
  defBuiltin('newline', args => {
    if (args.length !== 0) throw new EvalError('newline: expected 0 arguments');
    outputBuffer += '\n';
    return { tag: 'void' };
  });

  // String builtins
  defBuiltin('string-append', args => {
    let result = '';
    for (const a of args) {
      if (a.tag !== 'string') throw new EvalError('string-append: expected string');
      result += a.value;
    }
    return { tag: 'string', value: result };
  });
  defBuiltin('string-length', args => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-length: expected string');
    return { tag: 'number', value: args[0].value.length };
  });
  defBuiltin('substring', args => {
    if (args.length < 2 || args.length > 3) throw new EvalError('substring: expected 2 or 3 arguments');
    if (args[0].tag !== 'string') throw new EvalError('substring: expected string');
    if (args[1].tag !== 'number') throw new EvalError('substring: expected number');
    const start = args[1].value;
    const end = args.length === 3 ? (args[2].tag === 'number' ? args[2].value : (() => { throw new EvalError('substring: expected number'); })()) : args[0].value.length;
    return { tag: 'string', value: args[0].value.slice(start, end) };
  });
  defBuiltin('string->number', args => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->number: expected string');
    const n = Number(args[0].value);
    if (isNaN(n)) return { tag: 'boolean', value: false };
    return { tag: 'number', value: n };
  });
  defBuiltin('number->string', args => {
    if (args.length !== 1 || args[0].tag !== 'number') throw new EvalError('number->string: expected number');
    return { tag: 'string', value: String(args[0].value) };
  });
  defBuiltin('symbol->string', args => {
    if (args.length !== 1 || args[0].tag !== 'symbol') throw new EvalError('symbol->string: expected symbol');
    return { tag: 'string', value: args[0].value };
  });
  defBuiltin('string->symbol', args => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string->symbol: expected string');
    return { tag: 'symbol', value: args[0].value };
  });
  defBuiltin('string-ref', args => {
    if (args.length !== 2) throw new EvalError('string-ref: expected 2 arguments');
    if (args[0].tag !== 'string') throw new EvalError('string-ref: expected string');
    if (args[1].tag !== 'number') throw new EvalError('string-ref: expected number');
    const idx = args[1].value;
    if (idx < 0 || idx >= args[0].value.length) throw new EvalError('string-ref: index out of range');
    return { tag: 'char', value: args[0].value[idx] };
  });
  defBuiltin('char?', args => {
    if (args.length !== 1) throw new EvalError('char?: expected 1 argument');
    return { tag: 'boolean', value: args[0].tag === 'char' };
  });
  defBuiltin('string-copy', args => {
    if (args.length !== 1 || args[0].tag !== 'string') throw new EvalError('string-copy: expected string');
    return { tag: 'string', value: args[0].value };
  });
  defBuiltin('string-set!', args => {
    if (args.length !== 3) throw new EvalError('string-set!: expected 3 arguments');
    if (args[0].tag !== 'string') throw new EvalError('string-set!: expected string');
    if (args[1].tag !== 'number') throw new EvalError('string-set!: expected number');
    if (args[2].tag !== 'char') throw new EvalError('string-set!: expected char');
    const idx = args[1].value;
    const str = args[0].value;
    if (idx < 0 || idx >= str.length) throw new EvalError('string-set!: index out of range');
    (args[0] as any).value = str.substring(0, idx) + args[2].value + str.substring(idx + 1);
    return { tag: 'void' };
  });

  // call/cc as first-class value
  env.bindings.set('call/cc', { tag: 'callcc' });
  env.bindings.set('call-with-current-continuation', { tag: 'callcc' });

  return env;
}

// --- Display ---

// displayVal: external representation (with quotes on strings)
function displayVal(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'char': return `#\\${val.value === ' ' ? 'space' : val.value === '\n' ? 'newline' : val.value}`;
    case 'symbol': return val.value;
    case 'nil': return '()';
    case 'pair': {
      let s = '(';
      let cur: SchemeVal = val;
      let first = true;
      while (cur.tag === 'pair') {
        if (!first) s += ' ';
        s += displayVal(cur.car);
        cur = cur.cdr;
        first = false;
      }
      if (cur.tag !== 'nil') {
        s += ' . ' + displayVal(cur);
      }
      s += ')';
      return s;
    }
    case 'list': return `(${val.elements.map(displayVal).join(' ')})`;
    case 'void': return '#<void>';
    case 'lambda': return '#<procedure>';
    case 'continuation': return '#<procedure>';
    case 'callcc': return '#<procedure>';
    case 'macro': return '#<macro>';
    default: return '#<builtin>';
  }
}

// displayValUnquoted: display representation (no quotes on strings)
function displayValUnquoted(val: SchemeVal): string {
  switch (val.tag) {
    case 'string': return val.value;
    case 'char': return val.value;
    case 'pair': {
      let s = '(';
      let cur: SchemeVal = val;
      let first = true;
      while (cur.tag === 'pair') {
        if (!first) s += ' ';
        s += displayValUnquoted(cur.car);
        cur = cur.cdr;
        first = false;
      }
      if (cur.tag !== 'nil') {
        s += ' . ' + displayValUnquoted(cur);
      }
      s += ')';
      return s;
    }
    default: return displayVal(val);
  }
}

// --- Public API ---

function evalProgram(exprs: SchemeVal[], env: Env): SchemeVal {
  let startIndex = 0;
  pendingContinuation = null;
  while (true) {
    try {
      let result: SchemeVal = { tag: 'void' };
      for (let i = startIndex; i < exprs.length; i++) {
        currentExprIndex = i;
        result = evalExpr(exprs[i], env);
      }
      return result;
    } catch (e) {
      if (e instanceof ContinuationJump) {
        // If the continuation was invoked within the same top-level expression
        // and has body context, resume from the body (preserving let/lambda env)
        if (e.bodyInfo && currentExprIndex === e.exprIndex) {
          pendingContinuation = { callPos: e.callPos, value: e.value, exprIndex: e.exprIndex };
          // Resume from the body context, then continue with remaining top-level exprs
          let bodyResult: SchemeVal = { tag: 'void' };
          let jumpInfo = e;
          // Loop in case the continuation is invoked again within the body
          while (true) {
            try {
              pendingContinuation = { callPos: jumpInfo.callPos, value: jumpInfo.value, exprIndex: jumpInfo.exprIndex };
              bodyResult = { tag: 'void' };
              for (let bi = jumpInfo.bodyInfo!.idx; bi < jumpInfo.bodyInfo!.body.length; bi++) {
                currentExprIndex = jumpInfo.exprIndex;
                bodyResult = evalExpr(jumpInfo.bodyInfo!.body[bi], jumpInfo.bodyInfo!.env);
              }
              // Body completed - continue with remaining top-level expressions
              for (let i = jumpInfo.exprIndex + 1; i < exprs.length; i++) {
                currentExprIndex = i;
                bodyResult = evalExpr(exprs[i], env);
              }
              return bodyResult;
            } catch (e2) {
              if (e2 instanceof ContinuationJump && e2.bodyInfo) {
                jumpInfo = e2;
                continue;
              }
              if (e2 instanceof ContinuationJump) {
                // No bodyInfo - fall through to re-execution
                pendingContinuation = { callPos: e2.callPos, value: e2.value, exprIndex: e2.exprIndex };
                startIndex = e2.exprIndex;
                break;
              }
              throw e2;
            }
          }
          continue; // continue the outer re-execution loop
        }
        pendingContinuation = {
          callPos: e.callPos,
          value: e.value,
          exprIndex: e.exprIndex
        };
        startIndex = e.exprIndex;
        continue;
      }
      throw e;
    }
  }
}

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) {
    throw new EvalError('no expressions');
  }
  const env = makeGlobalEnv();
  const result = evalProgram(exprs, env);
  return displayVal(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  outputBuffer = '';
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) {
    throw new EvalError('no expressions');
  }
  const env = makeGlobalEnv();
  const result = evalProgram(exprs, env);
  return { result: displayVal(result), output: outputBuffer };
}
