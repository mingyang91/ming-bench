import { EvalError } from './evalError.js';

// --- Types ---

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] }
  | { tag: 'builtin'; name: string; func: (args: SchemeVal[]) => SchemeVal }
  | { tag: 'lambda'; params: string[]; body: SchemeVal[]; env: Env }
  | { tag: 'void' };

// --- Parser ---

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) { i++; continue; }
    if (ch === ';') { while (i < input.length && input[i] !== '\n') i++; continue; }
    if (ch === '(' || ch === ')') { tokens.push(ch); i++; continue; }
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i]; i++; if (i < input.length) { s += input[i]; i++; } }
        else { s += input[i]; i++; }
      }
      if (i < input.length) { s += '"'; i++; }
      tokens.push(s);
      continue;
    }
    if (ch === "'") { tokens.push("'"); i++; continue; }
    let atom = '';
    while (i < input.length && !/[\s();]/.test(input[i])) { atom += input[i]; i++; }
    if (atom.length > 0) tokens.push(atom);
  }
  return tokens;
}

function parse(tokens: string[], pos: number): [SchemeVal, number] {
  if (pos >= tokens.length) throw new EvalError('unexpected end of input');
  const token = tokens[pos];

  if (token === '(') {
    const elements: SchemeVal[] = [];
    pos++;
    while (pos < tokens.length && tokens[pos] !== ')') {
      const [val, next] = parse(tokens, pos);
      elements.push(val);
      pos = next;
    }
    if (pos >= tokens.length) throw new EvalError('missing closing paren');
    return [{ tag: 'list', elements }, pos + 1];
  }

  if (token === ')') throw new EvalError('unexpected )');

  if (token === "'") {
    const [val, next] = parse(tokens, pos + 1);
    return [{ tag: 'list', elements: [{ tag: 'symbol', value: 'quote' }, val] }, next];
  }

  if (token === '#t') return [{ tag: 'boolean', value: true }, pos + 1];
  if (token === '#f') return [{ tag: 'boolean', value: false }, pos + 1];

  if (token.startsWith('"')) {
    const inner = token.slice(1, -1)
      .replace(/\\n/g, '\n')
      .replace(/\\t/g, '\t')
      .replace(/\\"/g, '"')
      .replace(/\\\\/g, '\\');
    return [{ tag: 'string', value: inner }, pos + 1];
  }

  const num = Number(token);
  if (!isNaN(num) && token !== '') {
    return [{ tag: 'number', value: num }, pos + 1];
  }

  return [{ tag: 'symbol', value: token }, pos + 1];
}

function parseAll(input: string): SchemeVal[] {
  const tokens = tokenize(input);
  const exprs: SchemeVal[] = [];
  let pos = 0;
  while (pos < tokens.length) {
    const [val, next] = parse(tokens, pos);
    exprs.push(val);
    pos = next;
  }
  return exprs;
}

// --- Helpers ---

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function schemeToString(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(schemeToString).join(' ')})`;
    case 'builtin': return `#<procedure:${val.name}>`;
    case 'lambda': return '#<procedure>';
    case 'void': return '';
  }
}

function expectNumbers(args: SchemeVal[], name: string): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number, got ${schemeToString(a)}`);
    return a.value;
  });
}

// --- Env ---

type Env = Map<string, SchemeVal>;

function makeGlobalEnv(): Env {
  const env: Env = new Map();

  const defBuiltin = (name: string, func: (args: SchemeVal[]) => SchemeVal) => {
    env.set(name, { tag: 'builtin', name, func });
  };

  defBuiltin('+', (args) => {
    const nums = expectNumbers(args, '+');
    return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
  });

  defBuiltin('-', (args) => {
    if (args.length === 0) throw new EvalError('-: need at least 1 arg');
    const nums = expectNumbers(args, '-');
    if (nums.length === 1) return { tag: 'number', value: -nums[0] };
    return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  });

  defBuiltin('*', (args) => {
    const nums = expectNumbers(args, '*');
    return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
  });

  defBuiltin('/', (args) => {
    if (args.length !== 2) throw new EvalError('/: expected 2 args');
    const nums = expectNumbers(args, '/');
    if (nums[1] === 0) throw new EvalError('division by zero');
    return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
  });

  defBuiltin('<', (args) => {
    if (args.length !== 2) throw new EvalError('<: expected 2 args');
    const nums = expectNumbers(args, '<');
    return { tag: 'boolean', value: nums[0] < nums[1] };
  });

  defBuiltin('>', (args) => {
    if (args.length !== 2) throw new EvalError('>: expected 2 args');
    const nums = expectNumbers(args, '>');
    return { tag: 'boolean', value: nums[0] > nums[1] };
  });

  defBuiltin('=', (args) => {
    if (args.length !== 2) throw new EvalError('=: expected 2 args');
    const nums = expectNumbers(args, '=');
    return { tag: 'boolean', value: nums[0] === nums[1] };
  });

  defBuiltin('<=', (args) => {
    if (args.length !== 2) throw new EvalError('<=: expected 2 args');
    const nums = expectNumbers(args, '<=');
    return { tag: 'boolean', value: nums[0] <= nums[1] };
  });

  defBuiltin('>=', (args) => {
    if (args.length !== 2) throw new EvalError('>=: expected 2 args');
    const nums = expectNumbers(args, '>=');
    return { tag: 'boolean', value: nums[0] >= nums[1] };
  });

  // List operations
  defBuiltin('cons', (args) => {
    if (args.length !== 2) throw new EvalError('cons: expected 2 args');
    const [head, tail] = args;
    if (tail.tag === 'list') {
      return { tag: 'list', elements: [head, ...tail.elements] };
    }
    // Improper pair — for now treat as a 2-element list (dotted pairs come later)
    return { tag: 'list', elements: [head, tail] };
  });

  defBuiltin('car', (args) => {
    if (args.length !== 1) throw new EvalError('car: expected 1 arg');
    if (args[0].tag !== 'list' || args[0].elements.length === 0)
      throw new EvalError('car: expected non-empty list');
    return args[0].elements[0];
  });

  defBuiltin('cdr', (args) => {
    if (args.length !== 1) throw new EvalError('cdr: expected 1 arg');
    if (args[0].tag !== 'list' || args[0].elements.length === 0)
      throw new EvalError('cdr: expected non-empty list');
    return { tag: 'list', elements: args[0].elements.slice(1) };
  });

  defBuiltin('null?', (args) => {
    if (args.length !== 1) throw new EvalError('null?: expected 1 arg');
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length === 0 };
  });

  defBuiltin('list', (args) => {
    return { tag: 'list', elements: args };
  });

  defBuiltin('length', (args) => {
    if (args.length !== 1) throw new EvalError('length: expected 1 arg');
    if (args[0].tag !== 'list') throw new EvalError('length: expected list');
    return { tag: 'number', value: args[0].elements.length };
  });

  defBuiltin('append', (args) => {
    const result: SchemeVal[] = [];
    for (const arg of args) {
      if (arg.tag !== 'list') throw new EvalError('append: expected list');
      result.push(...arg.elements);
    }
    return { tag: 'list', elements: result };
  });

  // Type predicates
  defBuiltin('number?', (args) => {
    if (args.length !== 1) throw new EvalError('number?: expected 1 arg');
    return { tag: 'boolean', value: args[0].tag === 'number' };
  });

  defBuiltin('string?', (args) => {
    if (args.length !== 1) throw new EvalError('string?: expected 1 arg');
    return { tag: 'boolean', value: args[0].tag === 'string' };
  });

  defBuiltin('boolean?', (args) => {
    if (args.length !== 1) throw new EvalError('boolean?: expected 1 arg');
    return { tag: 'boolean', value: args[0].tag === 'boolean' };
  });

  defBuiltin('pair?', (args) => {
    if (args.length !== 1) throw new EvalError('pair?: expected 1 arg');
    return { tag: 'boolean', value: args[0].tag === 'list' && args[0].elements.length > 0 };
  });

  defBuiltin('symbol?', (args) => {
    if (args.length !== 1) throw new EvalError('symbol?: expected 1 arg');
    return { tag: 'boolean', value: args[0].tag === 'symbol' };
  });

  return env;
}

// --- Eval ---

function evalScheme(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;

    case 'symbol': {
      const val = env.get(expr.value);
      if (val === undefined) throw new EvalError(`unbound variable: ${expr.value}`);
      return val;
    }

    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) throw new EvalError('empty application');

      if (elems[0].tag === 'symbol') {
        const name = elems[0].value;

        if (name === 'quote') {
          if (elems.length !== 2) throw new EvalError('quote: wrong argument count');
          return elems[1];
        }

        if (name === 'if') {
          if (elems.length < 3 || elems.length > 4) throw new EvalError('if: wrong argument count');
          const cond = evalScheme(elems[1], env);
          if (isTruthy(cond)) return evalScheme(elems[2], env);
          if (elems.length === 4) return evalScheme(elems[3], env);
          return { tag: 'void' };
        }

        if (name === 'define') {
          if (elems.length < 3) throw new EvalError('define: wrong argument count');
          if (elems[1].tag === 'symbol') {
            const val = evalScheme(elems[2], env);
            env.set(elems[1].value, val);
            return { tag: 'void' };
          }
          if (elems[1].tag === 'list' && elems[1].elements.length > 0 && elems[1].elements[0].tag === 'symbol') {
            const fnName = elems[1].elements[0].value;
            const params = elems[1].elements.slice(1).map(p => {
              if (p.tag !== 'symbol') throw new EvalError('define: parameter must be a symbol');
              return p.value;
            });
            const lambda: SchemeVal = { tag: 'lambda', params, body: elems.slice(2), env };
            env.set(fnName, lambda);
            return { tag: 'void' };
          }
          throw new EvalError('define: invalid syntax');
        }

        if (name === 'lambda') {
          if (elems.length < 3) throw new EvalError('lambda: wrong argument count');
          if (elems[1].tag !== 'list') throw new EvalError('lambda: params must be a list');
          const params = elems[1].elements.map(p => {
            if (p.tag !== 'symbol') throw new EvalError('lambda: parameter must be a symbol');
            return p.value;
          });
          return { tag: 'lambda', params, body: elems.slice(2), env };
        }

        if (name === 'and') {
          let result: SchemeVal = { tag: 'boolean', value: true };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
            if (!isTruthy(result)) return result;
          }
          return result;
        }

        if (name === 'or') {
          let result: SchemeVal = { tag: 'boolean', value: false };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
            if (isTruthy(result)) return result;
          }
          return result;
        }

        if (name === 'not') {
          if (elems.length !== 2) throw new EvalError('not: wrong argument count');
          const val = evalScheme(elems[1], env);
          return { tag: 'boolean', value: !isTruthy(val) };
        }

        if (name === 'begin') {
          let result: SchemeVal = { tag: 'void' };
          for (let i = 1; i < elems.length; i++) {
            result = evalScheme(elems[i], env);
          }
          return result;
        }

        if (name === 'cond') {
          for (let i = 1; i < elems.length; i++) {
            const clause = elems[i];
            if (clause.tag !== 'list' || clause.elements.length < 2)
              throw new EvalError('cond: invalid clause');
            if (clause.elements[0].tag === 'symbol' && clause.elements[0].value === 'else') {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.elements.length; j++) {
                result = evalScheme(clause.elements[j], env);
              }
              return result;
            }
            const test = evalScheme(clause.elements[0], env);
            if (isTruthy(test)) {
              let result: SchemeVal = { tag: 'void' };
              for (let j = 1; j < clause.elements.length; j++) {
                result = evalScheme(clause.elements[j], env);
              }
              return result;
            }
          }
          return { tag: 'void' };
        }

        if (name === 'let') {
          if (elems.length < 3) throw new EvalError('let: wrong argument count');
          // Named let: (let name ((var init) ...) body ...)
          if (elems[1].tag === 'symbol') {
            const loopName = elems[1].value;
            if (elems[2].tag !== 'list') throw new EvalError('let: bindings must be a list');
            const bindings = elems[2].elements;
            const params: string[] = [];
            const inits: SchemeVal[] = [];
            for (const b of bindings) {
              if (b.tag !== 'list' || b.elements.length !== 2 || b.elements[0].tag !== 'symbol')
                throw new EvalError('let: invalid binding');
              params.push(b.elements[0].value);
              inits.push(evalScheme(b.elements[1], env));
            }
            const body = elems.slice(3);
            const lambda: SchemeVal = { tag: 'lambda', params, body, env };
            // Create env where the loop name is bound to the lambda
            const loopEnv: Env = new Map(env);
            loopEnv.set(loopName, lambda);
            // Update the lambda's closure to include itself
            (lambda as any).env = loopEnv;
            // Call with initial values
            const childEnv: Env = new Map(loopEnv);
            for (let i = 0; i < params.length; i++) {
              childEnv.set(params[i], inits[i]);
            }
            let result: SchemeVal = { tag: 'void' };
            for (const bodyExpr of body) {
              result = evalScheme(bodyExpr, childEnv);
            }
            return result;
          }
          // Regular let: (let ((var init) ...) body ...)
          if (elems[1].tag !== 'list') throw new EvalError('let: bindings must be a list');
          const childEnv: Env = new Map(env);
          for (const binding of elems[1].elements) {
            if (binding.tag !== 'list' || binding.elements.length !== 2 || binding.elements[0].tag !== 'symbol')
              throw new EvalError('let: invalid binding');
            const val = evalScheme(binding.elements[1], env);
            childEnv.set(binding.elements[0].value, val);
          }
          let letResult: SchemeVal = { tag: 'void' };
          for (let i = 2; i < elems.length; i++) {
            letResult = evalScheme(elems[i], childEnv);
          }
          return letResult;
        }
      }

      // Function application
      const func = evalScheme(elems[0], env);
      const args = elems.slice(1).map(a => evalScheme(a, env));

      if (func.tag === 'builtin') {
        return func.func(args);
      }

      if (func.tag === 'lambda') {
        if (args.length !== func.params.length) throw new EvalError('wrong number of arguments');
        const childEnv: Env = new Map(func.env);
        for (let i = 0; i < func.params.length; i++) {
          childEnv.set(func.params[i], args[i]);
        }
        let result: SchemeVal = { tag: 'void' };
        for (const bodyExpr of func.body) {
          result = evalScheme(bodyExpr, childEnv);
        }
        return result;
      }

      throw new EvalError(`not a procedure: ${schemeToString(func)}`);
    }

    case 'builtin':
    case 'lambda':
    case 'void':
      return expr;
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const exprs = parseAll(input);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evalScheme(expr, env);
  }
  return schemeToString(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
