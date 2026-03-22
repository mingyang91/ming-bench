import { EvalError } from './evalError.js';

// --- Types ---

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] };

// --- Tokenizer ---

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    // Whitespace
    if (ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r') {
      i++;
      continue;
    }
    // Comment
    if (ch === ';') {
      while (i < input.length && input[i] !== '\n') i++;
      continue;
    }
    // Parens
    if (ch === '(' || ch === ')') {
      tokens.push(ch);
      i++;
      continue;
    }
    // String literal
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') {
          s += input[i];
          i++;
          if (i < input.length) {
            s += input[i];
            i++;
          }
          continue;
        }
        s += input[i];
        i++;
      }
      if (i < input.length) {
        s += '"';
        i++; // closing quote
      }
      tokens.push(s);
      continue;
    }
    // Atom
    let atom = '';
    while (i < input.length && !' \t\n\r();"'.includes(input[i])) {
      atom += input[i];
      i++;
    }
    if (atom.length > 0) {
      tokens.push(atom);
    }
  }
  return tokens;
}

// --- Parser ---

function parse(tokens: string[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) {
      throw new EvalError('unexpected end of input');
    }
    const token = tokens[pos];
    if (token === '(') {
      pos++; // skip '('
      const elements: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos] !== ')') {
        elements.push(parseExpr());
      }
      if (pos >= tokens.length) {
        throw new EvalError('missing closing parenthesis');
      }
      pos++; // skip ')'
      return { tag: 'list', elements };
    }
    if (token === ')') {
      throw new EvalError('unexpected )');
    }
    pos++;
    return parseAtom(token);
  }

  function parseAtom(token: string): SchemeVal {
    // Boolean
    if (token === '#t') return { tag: 'boolean', value: true };
    if (token === '#f') return { tag: 'boolean', value: false };
    // String
    if (token.startsWith('"') && token.endsWith('"')) {
      const inner = token.slice(1, -1)
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\"/g, '"')
        .replace(/\\\\/g, '\\');
      return { tag: 'string', value: inner };
    }
    // Number
    const num = Number(token);
    if (!isNaN(num) && token !== '') {
      return { tag: 'number', value: num };
    }
    // Symbol
    return { tag: 'symbol', value: token };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// --- Evaluator ---

function isTruthy(val: SchemeVal): boolean {
  return !(val.tag === 'boolean' && val.value === false);
}

function evalExpr(expr: SchemeVal): SchemeVal {
  switch (expr.tag) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      throw new EvalError(`unbound variable: ${expr.value}`);
    case 'list': {
      const elems = expr.elements;
      if (elems.length === 0) {
        throw new EvalError('empty application');
      }
      const head = elems[0];
      // Special forms
      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'and':
            return evalAnd(elems.slice(1));
          case 'or':
            return evalOr(elems.slice(1));
          case 'not': {
            if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
            const val = evalExpr(elems[1]);
            return { tag: 'boolean', value: !isTruthy(val) };
          }
        }
        // Built-in procedures
        const op = head.value;
        const args = elems.slice(1).map(e => evalExpr(e));
        switch (op) {
          case '+': return arith(args, '+');
          case '-': return arith(args, '-');
          case '*': return arith(args, '*');
          case '/': return arith(args, '/');
          case '<': return compare(args, '<');
          case '>': return compare(args, '>');
          case '=': return compare(args, '=');
          case '<=': return compare(args, '<=');
          default:
            throw new EvalError(`unknown procedure: ${op}`);
        }
      }
      throw new EvalError('not a procedure');
    }
  }
}

function evalAnd(exprs: SchemeVal[]): SchemeVal {
  let result: SchemeVal = { tag: 'boolean', value: true };
  for (const expr of exprs) {
    result = evalExpr(expr);
    if (!isTruthy(result)) return result;
  }
  return result;
}

function evalOr(exprs: SchemeVal[]): SchemeVal {
  let result: SchemeVal = { tag: 'boolean', value: false };
  for (const expr of exprs) {
    result = evalExpr(expr);
    if (isTruthy(result)) return result;
  }
  return result;
}

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

function compare(args: SchemeVal[], op: string): SchemeVal {
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

// --- Display ---

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(display).join(' ')})`;
  }
}

// --- Public API ---

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) {
    throw new EvalError('no expressions');
  }
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr);
  }
  return display(result!);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  return { result: evalStr(input), output: '' };
}
