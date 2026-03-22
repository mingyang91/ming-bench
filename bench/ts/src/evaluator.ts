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
  | { tag: 'builtin'; fn: (args: SchemeVal[]) => SchemeVal; pos?: Pos };

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
            for (let i = 0; i < body.length - 1; i++) {
              evalExpr(body[i], letEnv);
            }
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
            for (let i = 0; i < bodyExprs.length - 1; i++) {
              evalExpr(bodyExprs[i], env);
            }
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
        }
      }
      // Procedure application
      const proc = evalExpr(head, env);
      if (proc.tag === 'builtin') {
        const args = elems.slice(1).map(e => evalExpr(e, env));
        try {
          return proc.fn(args);
        } catch (e) {
          if (e instanceof EvalError && !/^\d/.test(e.message)) {
            throw new EvalError(`${posStr(expr.pos)}: ${e.message}`);
          }
          throw e;
        }
      }
      if (proc.tag === 'lambda') {
        const args = elems.slice(1).map(e => evalExpr(e, env));
        const callEnv = applyLambda(proc, args, expr.pos);
        for (let i = 0; i < proc.body.length - 1; i++) {
          evalExpr(proc.body[i], callEnv);
        }
        expr = proc.body[proc.body.length - 1]; env = callEnv; continue; // TCO
      }
      throw new EvalError(`${posStr(expr.pos)}: not a procedure`);
    }
    default:
      return expr;
  }
  }
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

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) {
    throw new EvalError('no expressions');
  }
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return displayVal(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  outputBuffer = '';
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) {
    throw new EvalError('no expressions');
  }
  const env = makeGlobalEnv();
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }
  return { result: displayVal(result!), output: outputBuffer };
}
