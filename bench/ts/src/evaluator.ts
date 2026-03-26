import { EvalError } from './evalError.js';

// ── Types ──────────────────────────────────────────────────────────────

type SchemeVal =
  | { tag: 'number'; value: number }
  | { tag: 'boolean'; value: boolean }
  | { tag: 'string'; value: string }
  | { tag: 'symbol'; value: string }
  | { tag: 'list'; elements: SchemeVal[] };

// ── Tokenizer ──────────────────────────────────────────────────────────

function tokenize(input: string): string[] {
  const tokens: string[] = [];
  let i = 0;
  while (i < input.length) {
    const ch = input[i];
    // whitespace
    if (/\s/.test(ch)) { i++; continue; }
    // comment
    if (ch === ';') { while (i < input.length && input[i] !== '\n') i++; continue; }
    // parens
    if (ch === '(' || ch === ')') { tokens.push(ch); i++; continue; }
    // string literal
    if (ch === '"') {
      let s = '"';
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === '\\') { s += input[i++]; }
        s += input[i++];
      }
      if (i < input.length) { s += '"'; i++; }
      tokens.push(s);
      continue;
    }
    // atom
    let tok = '';
    while (i < input.length && !/[\s()";]/.test(input[i])) {
      tok += input[i++];
    }
    tokens.push(tok);
  }
  return tokens;
}

// ── Parser ─────────────────────────────────────────────────────────────

function parse(tokens: string[]): SchemeVal[] {
  let pos = 0;

  function parseExpr(): SchemeVal {
    if (pos >= tokens.length) throw new EvalError('unexpected end of input');
    const tok = tokens[pos++];
    if (tok === '(') {
      const elements: SchemeVal[] = [];
      while (pos < tokens.length && tokens[pos] !== ')') {
        elements.push(parseExpr());
      }
      if (pos >= tokens.length) throw new EvalError('missing closing parenthesis');
      pos++; // skip ')'
      return { tag: 'list', elements };
    }
    if (tok === ')') throw new EvalError('unexpected )');
    return parseAtom(tok);
  }

  function parseAtom(tok: string): SchemeVal {
    if (tok === '#t') return { tag: 'boolean', value: true };
    if (tok === '#f') return { tag: 'boolean', value: false };
    if (tok.startsWith('"') && tok.endsWith('"')) {
      return { tag: 'string', value: tok.slice(1, -1) };
    }
    const num = Number(tok);
    if (!isNaN(num) && tok !== '') {
      return { tag: 'number', value: num };
    }
    return { tag: 'symbol', value: tok };
  }

  const exprs: SchemeVal[] = [];
  while (pos < tokens.length) {
    exprs.push(parseExpr());
  }
  return exprs;
}

// ── Evaluator ──────────────────────────────────────────────────────────

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
      if (elems.length === 0) throw new EvalError('empty application');

      const head = elems[0];

      // Special forms: and, or, not
      if (head.tag === 'symbol') {
        switch (head.value) {
          case 'and': {
            let result: SchemeVal = { tag: 'boolean', value: true };
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i]);
              if (!isTruthy(result)) return result;
            }
            return result;
          }
          case 'or': {
            let result: SchemeVal = { tag: 'boolean', value: false };
            for (let i = 1; i < elems.length; i++) {
              result = evalExpr(elems[i]);
              if (isTruthy(result)) return result;
            }
            return result;
          }
          case 'not': {
            if (elems.length !== 2) throw new EvalError('not: expected 1 argument');
            const val = evalExpr(elems[1]);
            return { tag: 'boolean', value: !isTruthy(val) };
          }
        }
      }

      // Function application — for L1, operator must be a builtin symbol
      if (head.tag === 'symbol') {
        const args = elems.slice(1).map(evalExpr);
        return applyBuiltin(head.value, args);
      }
      throw new EvalError(`not a procedure: ${display(head)}`);
    }
  }
}

function requireNumbers(name: string, args: SchemeVal[]): number[] {
  return args.map(a => {
    if (a.tag !== 'number') throw new EvalError(`${name}: expected number`);
    return a.value;
  });
}

function applyBuiltin(name: string, args: SchemeVal[]): SchemeVal {
  switch (name) {
    case '+': {
      const nums = requireNumbers('+', args);
      return { tag: 'number', value: nums.reduce((a, b) => a + b, 0) };
    }
    case '-': {
      if (args.length === 0) throw new EvalError('-: expected at least 1 argument');
      const nums = requireNumbers('-', args);
      if (nums.length === 1) return { tag: 'number', value: -nums[0] };
      return { tag: 'number', value: nums.slice(1).reduce((a, b) => a - b, nums[0]) };
    }
    case '*': {
      const nums = requireNumbers('*', args);
      return { tag: 'number', value: nums.reduce((a, b) => a * b, 1) };
    }
    case '/': {
      if (args.length < 2) throw new EvalError('/: expected at least 2 arguments');
      const nums = requireNumbers('/', args);
      if (nums[1] === 0) throw new EvalError('division by zero');
      return { tag: 'number', value: Math.trunc(nums[0] / nums[1]) };
    }
    case '<': {
      const nums = requireNumbers('<', args);
      return { tag: 'boolean', value: nums[0] < nums[1] };
    }
    case '>': {
      const nums = requireNumbers('>', args);
      return { tag: 'boolean', value: nums[0] > nums[1] };
    }
    case '=': {
      const nums = requireNumbers('=', args);
      return { tag: 'boolean', value: nums[0] === nums[1] };
    }
    case '<=': {
      const nums = requireNumbers('<=', args);
      return { tag: 'boolean', value: nums[0] <= nums[1] };
    }
    case '>=': {
      const nums = requireNumbers('>=', args);
      return { tag: 'boolean', value: nums[0] >= nums[1] };
    }
    default:
      throw new EvalError(`unbound variable: ${name}`);
  }
}

// ── Display ────────────────────────────────────────────────────────────

function display(val: SchemeVal): string {
  switch (val.tag) {
    case 'number': return String(val.value);
    case 'boolean': return val.value ? '#t' : '#f';
    case 'string': return `"${val.value}"`;
    case 'symbol': return val.value;
    case 'list': return `(${val.elements.map(display).join(' ')})`;
  }
}

// ── Public API ─────────────────────────────────────────────────────────

export function evalStr(input: string): string {
  const tokens = tokenize(input);
  const exprs = parse(tokens);
  if (exprs.length === 0) throw new EvalError('no expressions');
  let result: SchemeVal | undefined;
  for (const expr of exprs) {
    result = evalExpr(expr);
  }
  return display(result!);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  throw new EvalError('not implemented');
}
