import { EvalError } from './evalError.js';

type Token =
  | { kind: 'paren'; value: '(' | ')' }
  | { kind: 'atom'; value: string }
  | { kind: 'string'; value: string };

type Expr =
  | { type: 'number'; value: number }
  | { type: 'boolean'; value: boolean }
  | { type: 'string'; value: string }
  | { type: 'symbol'; name: string }
  | { type: 'list'; elements: Expr[] };

type SchemeValue =
  | { type: 'number'; value: number }
  | { type: 'boolean'; value: boolean }
  | { type: 'string'; value: string };

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('empty input');
  }

  let result: SchemeValue | undefined;
  for (const expr of expressions) {
    result = evaluate(expr);
  }

  return formatValue(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  return { result: evalStr(input), output: '' };
}

function parseProgram(input: string): Expr[] {
  const tokens = tokenize(input);
  let index = 0;
  const expressions: Expr[] = [];

  while (index < tokens.length) {
    expressions.push(parseExpr());
  }

  return expressions;

  function parseExpr(): Expr {
    const token = tokens[index];
    if (!token) {
      throw new EvalError('unexpected end of input');
    }

    index += 1;

    if (token.kind === 'paren') {
      if (token.value === ')') {
        throw new EvalError('unexpected )');
      }

      const elements: Expr[] = [];
      while (index < tokens.length) {
        const next = tokens[index];
        if (next.kind === 'paren' && next.value === ')') {
          index += 1;
          return { type: 'list', elements };
        }

        elements.push(parseExpr());
      }

      throw new EvalError('missing )');
    }

    if (token.kind === 'string') {
      return { type: 'string', value: token.value };
    }

    if (token.value === '#t') {
      return { type: 'boolean', value: true };
    }

    if (token.value === '#f') {
      return { type: 'boolean', value: false };
    }

    if (/^-?\d+$/.test(token.value)) {
      return { type: 'number', value: Number(token.value) };
    }

    return { type: 'symbol', name: token.value };
  }
}

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let index = 0;

  while (index < input.length) {
    const ch = input[index];

    if (/\s/.test(ch)) {
      index += 1;
      continue;
    }

    if (ch === ';') {
      while (index < input.length && input[index] !== '\n') {
        index += 1;
      }
      continue;
    }

    if (ch === '(' || ch === ')') {
      tokens.push({ kind: 'paren', value: ch });
      index += 1;
      continue;
    }

    if (ch === '"') {
      index += 1;
      let value = '';
      let terminated = false;

      while (index < input.length) {
        const current = input[index];

        if (current === '"') {
          index += 1;
          tokens.push({ kind: 'string', value });
          terminated = true;
          break;
        }

        if (current === '\\') {
          index += 1;
          if (index >= input.length) {
            throw new EvalError('unterminated string literal');
          }

          const escaped = input[index];
          switch (escaped) {
            case 'n':
              value += '\n';
              break;
            case 'r':
              value += '\r';
              break;
            case 't':
              value += '\t';
              break;
            case '"':
              value += '"';
              break;
            case '\\':
              value += '\\';
              break;
            default:
              value += escaped;
              break;
          }

          index += 1;
          continue;
        }

        value += current;
        index += 1;
      }

      if (!terminated) {
        throw new EvalError('unterminated string literal');
      }

      continue;
    }

    let end = index;
    while (end < input.length) {
      const current = input[end];
      if (/\s/.test(current) || current === '(' || current === ')' || current === ';') {
        break;
      }
      end += 1;
    }

    tokens.push({ kind: 'atom', value: input.slice(index, end) });
    index = end;
  }

  return tokens;
}

function evaluate(expr: Expr): SchemeValue {
  switch (expr.type) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      throw new EvalError(`unbound variable: ${expr.name}`);
    case 'list':
      return evaluateList(expr.elements);
  }
}

function evaluateList(elements: Expr[]): SchemeValue {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const operator = elements[0];
  if (operator.type !== 'symbol') {
    throw new EvalError('operator must be a symbol');
  }

  const args = elements.slice(1);

  switch (operator.name) {
    case '+':
      return numberValue(evaluateNumberArgs(args).reduce((sum, value) => sum + value, 0));
    case '*':
      return numberValue(evaluateNumberArgs(args).reduce((product, value) => product * value, 1));
    case '-': {
      const values = evaluateNumberArgs(args);
      requireArgCountAtLeast('-', values.length, 1);
      if (values.length === 1) {
        return numberValue(-values[0]);
      }
      return numberValue(values.slice(1).reduce((result, value) => result - value, values[0]));
    }
    case '/': {
      const values = evaluateNumberArgs(args);
      requireArgCountAtLeast('/', values.length, 1);
      let result = values[0];
      if (values.length === 1) {
        if (result === 0) {
          throw new EvalError('division by zero');
        }
        return numberValue(1 / result);
      }
      for (const value of values.slice(1)) {
        if (value === 0) {
          throw new EvalError('division by zero');
        }
        result /= value;
      }
      return numberValue(result);
    }
    case '<':
      return booleanValue(compareNumberArgs('<', args, (left, right) => left < right));
    case '>':
      return booleanValue(compareNumberArgs('>', args, (left, right) => left > right));
    case '=':
      return booleanValue(compareNumberArgs('=', args, (left, right) => left === right));
    case '<=':
      return booleanValue(compareNumberArgs('<=', args, (left, right) => left <= right));
    case 'not':
      requireArgCount('not', args.length, 1);
      return booleanValue(!isTruthy(evaluate(args[0])));
    case 'and': {
      let result: SchemeValue = booleanValue(true);
      for (const arg of args) {
        result = evaluate(arg);
        if (!isTruthy(result)) {
          return result;
        }
      }
      return result;
    }
    case 'or': {
      let result: SchemeValue = booleanValue(false);
      for (const arg of args) {
        result = evaluate(arg);
        if (isTruthy(result)) {
          return result;
        }
      }
      return result;
    }
    default:
      throw new EvalError(`unknown procedure: ${operator.name}`);
  }
}

function evaluateNumberArgs(args: Expr[]): number[] {
  return args.map((arg) => expectNumber(evaluate(arg)));
}

function compareNumberArgs(
  name: string,
  args: Expr[],
  predicate: (left: number, right: number) => boolean,
): boolean {
  const values = evaluateNumberArgs(args);
  requireArgCountAtLeast(name, values.length, 1);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function requireArgCount(name: string, actual: number, expected: number): void {
  if (actual !== expected) {
    throw new EvalError(`${name}: expected ${expected} argument(s), got ${actual}`);
  }
}

function requireArgCountAtLeast(name: string, actual: number, minimum: number): void {
  if (actual < minimum) {
    throw new EvalError(`${name}: expected at least ${minimum} argument(s), got ${actual}`);
  }
}

function expectNumber(value: SchemeValue): number {
  if (value.type !== 'number') {
    throw new EvalError('expected number');
  }

  return value.value;
}

function isTruthy(value: SchemeValue): boolean {
  return value.type !== 'boolean' || value.value;
}

function numberValue(value: number): SchemeValue {
  if (!Number.isFinite(value)) {
    throw new EvalError('invalid number');
  }

  return { type: 'number', value };
}

function booleanValue(value: boolean): SchemeValue {
  return { type: 'boolean', value };
}

function formatValue(value: SchemeValue | undefined): string {
  if (!value) {
    throw new EvalError('no result');
  }

  switch (value.type) {
    case 'number':
      return String(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return JSON.stringify(value.value);
  }
}
