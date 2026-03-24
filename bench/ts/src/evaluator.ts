import { EvalError } from './evalError.js';

// --- Types ---

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; value: SchemeVal[] }
  | { tag: 'procedure'; value: (...args: SchemeVal[]) => SchemeVal }
  | { tag: 'void' };

// --- Parser ---

interface Token {
  type: 'lparen' | 'rparen' | 'quote' | 'atom' | 'string' | 'dot';
  value: string;
}

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    // skip whitespace
    if (/\s/.test(ch)) { i++; continue; }
    // skip line comments
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }
    if (ch === '(') { tokens.push({ type: 'lparen', value: '(' }); i++; continue; }
    if (ch === ')') { tokens.push({ type: 'rparen', value: ')' }); i++; continue; }
    if (ch === '\'') { tokens.push({ type: 'quote', value: '\'' }); i++; continue; }
    if (ch === '"') {
      let s = '';
      i++; // skip opening quote
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') {
          i++;
          if (i < input.length) {
            if (input[i] === 'n') s += '\n';
            else if (input[i] === 't') s += '\t';
            else if (input[i] === '"') s += '"';
            else if (input[i] === '\\') s += '\\';
            else s += input[i];
          }
        } else {
          s += input[i];
        }
        i++;
      }
      i++; // skip closing quote
      tokens.push({ type: 'string', value: s });
      continue;
    }
    // atom
    let atom = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      atom += input[i];
      i++;
    }
    if (atom === '.') {
      tokens.push({ type: 'dot', value: '.' });
    } else {
      tokens.push({ type: 'atom', value: atom });
    }
  }
  return tokens;
}

function parse(tokens: Token[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[pos];

    if (tok.type === 'lparen') {
      pos++; // skip (
      const elements: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos].type !== 'rparen') {
        elements.push(parseExpr());
      }
      if (pos >= tokens.length) throw new EvalError('missing closing parenthesis');
      pos++; // skip )
      return { tag: 'list', value: elements };
    }

    if (tok.type === 'rparen') {
      throw new EvalError('unexpected )');
    }

    if (tok.type === 'quote') {
      pos++;
      const quoted = parseExpr();
      return { tag: 'list', value: [{ tag: 'symbol', value: 'quote' }, quoted] };
    }

    if (tok.type === 'string') {
      pos++;
      return { tag: 'string', value: tok.value };
    }

    // atom
    pos++;
    const v = tok.value;
    if (v === '#t') return { tag: 'boolean', value: true };
    if (v === '#f') return { tag: 'boolean', value: false };
    if (/^-?\d+$/.test(v)) return { tag: 'number', value: parseInt(v, 10) };
    return { tag: 'symbol', value: v };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// --- Environment ---

class Env {
  private bindings: Map<string, SchemeVal>;
  private parent: Env | null;

  constructor(parent: Env | null = null) {
    this.bindings = new Map();
    this.parent = parent;
  }

  get(name: string): SchemeVal {
    const val = this.bindings.get(name);
    if (val !== undefined) return val;
    if (this.parent) return this.parent.get(name);
    throw new EvalError(`unbound variable: ${name}`);
  }

  set(name: string, value: SchemeVal): void {
    this.bindings.set(name, value);
  }
}

function makeGlobalEnv(): Env {
  const env = new Env();

  const numOp = (op: (a: number, b: number) => number, identity: number) =>
    ({ tag: 'procedure' as const, value: (...args: SchemeVal[]) => {
      const nums = args.map(a => {
        if (a.tag !== 'number') throw new EvalError('expected number');
        return a.value;
      });
      return { tag: 'number' as const, value: nums.reduce(op, identity) };
    }});

  env.set('+', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
  }});

  env.set('*', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
  }});

  env.set('-', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length === 0) throw new EvalError('- requires at least one argument');
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    if (nums.length === 1) return { tag: 'number' as const, value: -nums[0] };
    return { tag: 'number' as const, value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
  }});

  env.set('/', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length < 2) throw new EvalError('/ requires at least two arguments');
    const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
    return { tag: 'number' as const, value: nums.slice(1).reduce((a, b) => {
      if (b === 0) throw new EvalError('division by zero');
      return Math.trunc(a / b);
    }, nums[0]) };
  }});

  const cmpOp = (op: (a: number, b: number) => boolean) =>
    ({ tag: 'procedure' as const, value: (...args: SchemeVal[]) => {
      if (args.length < 2) throw new EvalError('comparison requires at least two arguments');
      const nums = args.map(a => { if (a.tag !== 'number') throw new EvalError('expected number'); return a.value; });
      for (let i = 0; i < nums.length - 1; i++) {
        if (!op(nums[i], nums[i + 1])) return { tag: 'boolean' as const, value: false };
      }
      return { tag: 'boolean' as const, value: true };
    }});

  env.set('<', cmpOp((a, b) => a < b));
  env.set('>', cmpOp((a, b) => a > b));
  env.set('=', cmpOp((a, b) => a === b));
  env.set('<=', cmpOp((a, b) => a <= b));
  env.set('>=', cmpOp((a, b) => a >= b));

  env.set('not', { tag: 'procedure', value: (...args: SchemeVal[]) => {
    if (args.length !== 1) throw new EvalError('not requires exactly one argument');
    return { tag: 'boolean', value: isFalsy(args[0]) };
  }});

  return env;
}

// --- Evaluator ---

function isFalsy(val: SchemeVal): boolean {
  return val.tag === 'boolean' && val.value === false;
}

function evaluate(expr: SchemeVal, env: Env): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;

    case 'symbol':
      return env.get(expr.value);

    case 'list': {
      const elems = expr.value;
      if (elems.length === 0) throw new EvalError('empty application');

      const first = elems[0];

      // Special forms
      if (first.tag === 'symbol') {
        switch (first.value) {
          case 'define': {
            if (elems.length < 3) throw new EvalError('define requires at least 2 arguments');
            const target = elems[1];
            if (target.tag === 'symbol') {
              // (define x expr)
              const val = evaluate(elems[2], env);
              env.set(target.value, val);
              return { tag: 'void' };
            }
            if (target.tag === 'list' && target.value.length > 0 && target.value[0].tag === 'symbol') {
              // (define (f params...) body...)
              const name = target.value[0].value;
              const paramNames = target.value.slice(1).map(p => {
                if (p.tag !== 'symbol') throw new EvalError('parameter must be a symbol');
                return p.value;
              });
              const bodyExprs = elems.slice(2);
              const proc: SchemeVal = { tag: 'procedure', value: (...args: SchemeVal[]) => {
                const childEnv = new Env(env);
                for (let i = 0; i < paramNames.length; i++) {
                  childEnv.set(paramNames[i], args[i]);
                }
                let result: SchemeVal = { tag: 'void' };
                for (const b of bodyExprs) {
                  result = evaluate(b, childEnv);
                }
                return result;
              }};
              env.set(name, proc);
              return { tag: 'void' };
            }
            throw new EvalError('invalid define syntax');
          }
          case 'if': {
            if (elems.length < 3) throw new EvalError('if requires at least 2 arguments');
            const cond = evaluate(elems[1], env);
            if (!isFalsy(cond)) {
              return evaluate(elems[2], env);
            } else if (elems.length > 3) {
              return evaluate(elems[3], env);
            }
            return { tag: 'void' };
          }
          case 'quote': {
            if (elems.length !== 2) throw new EvalError('quote requires exactly 1 argument');
            return elems[1];
          }
          case 'lambda': {
            if (elems.length < 3) throw new EvalError('lambda requires params and body');
            const params = elems[1];
            if (params.tag !== 'list') throw new EvalError('lambda params must be a list');
            const paramNames = params.value.map(p => {
              if (p.tag !== 'symbol') throw new EvalError('parameter must be a symbol');
              return p.value;
            });
            const bodyExprs = elems.slice(2);
            return { tag: 'procedure', value: (...args: SchemeVal[]) => {
              const childEnv = new Env(env);
              for (let i = 0; i < paramNames.length; i++) {
                childEnv.set(paramNames[i], args[i]);
              }
              let result: SchemeVal = { tag: 'void' };
              for (const b of bodyExprs) {
                result = evaluate(b, childEnv);
              }
              return result;
            }};
          }
          case 'and': {
            if (elems.length === 1) return { tag: 'boolean', value: true };
            let result: SchemeVal = { tag: 'boolean', value: true };
            for (let i = 1; i < elems.length; i++) {
              result = evaluate(elems[i], env);
              if (isFalsy(result)) return result;
            }
            return result;
          }
          case 'or': {
            if (elems.length === 1) return { tag: 'boolean', value: false };
            let result: SchemeVal = { tag: 'boolean', value: false };
            for (let i = 1; i < elems.length; i++) {
              result = evaluate(elems[i], env);
              if (!isFalsy(result)) return result;
            }
            return result;
          }
        }
      }

      // Function application
      const func = evaluate(first, env);
      if (func.tag !== 'procedure') throw new EvalError('not a procedure');
      const args = elems.slice(1).map(a => evaluate(a, env));
      return func.value(...args);
    }

    default:
      throw new EvalError('cannot evaluate');
  }
}

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.value.map(display).join(' ')})`;
    case 'void': return '';
    case 'procedure': return '#<procedure>';
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  const env = makeGlobalEnv();
  let result: SchemeVal = { tag: 'void' };
  for (const expr of exprs) {
    result = evaluate(expr, env);
  }
  return display(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
