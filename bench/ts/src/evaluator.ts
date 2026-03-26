import { EvalError } from './evalError.js';

type Value =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string };

type Expr =
  | Value
  | { kind: 'symbol'; name: string }
  | { kind: 'list'; elements: Expr[] };

type Token =
  | { kind: 'paren'; value: '(' | ')' }
  | { kind: 'atom'; value: string }
  | { kind: 'string'; value: string };

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  return formatValue(evaluateProgram(input).result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const evaluation = evaluateProgram(input);
  return {
    result: formatValue(evaluation.result),
    output: evaluation.output,
  };
}

function evaluateProgram(input: string): { result: Value; output: string } {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('expected at least one expression');
  }

  let result: Value | undefined;
  for (const expr of expressions) {
    result = evaluateExpr(expr);
  }

  return {
    result: result!,
    output: '',
  };
}

function parseProgram(input: string): Expr[] {
  const tokens = tokenize(input);
  const expressions: Expr[] = [];
  let index = 0;

  while (index < tokens.length) {
    const parsed = parseExpr(tokens, index);
    expressions.push(parsed.expr);
    index = parsed.nextIndex;
  }

  return expressions;
}

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let index = 0;

  while (index < input.length) {
    const char = input[index];

    if (isWhitespace(char)) {
      index += 1;
      continue;
    }

    if (char === ';') {
      index = skipComment(input, index);
      continue;
    }

    if (char === '(' || char === ')') {
      tokens.push({ kind: 'paren', value: char });
      index += 1;
      continue;
    }

    if (char === '"') {
      const parsed = parseStringToken(input, index);
      tokens.push({ kind: 'string', value: parsed.value });
      index = parsed.nextIndex;
      continue;
    }

    let end = index;
    while (end < input.length && !isDelimiter(input[end])) {
      end += 1;
    }

    tokens.push({ kind: 'atom', value: input.slice(index, end) });
    index = end;
  }

  return tokens;
}

function skipComment(input: string, start: number): number {
  let index = start;
  while (index < input.length && input[index] !== '\n') {
    index += 1;
  }
  return index;
}

function parseStringToken(input: string, start: number): { value: string; nextIndex: number } {
  let index = start + 1;
  let value = '';

  while (index < input.length) {
    const char = input[index];

    if (char === '"') {
      return { value, nextIndex: index + 1 };
    }

    if (char === '\\') {
      index += 1;
      if (index >= input.length) {
        throw new EvalError('unterminated string literal');
      }

      const escaped = input[index];
      switch (escaped) {
        case '"':
          value += '"';
          break;
        case '\\':
          value += '\\';
          break;
        case 'n':
          value += '\n';
          break;
        case 'r':
          value += '\r';
          break;
        case 't':
          value += '\t';
          break;
        default:
          value += escaped;
          break;
      }

      index += 1;
      continue;
    }

    value += char;
    index += 1;
  }

  throw new EvalError('unterminated string literal');
}

function parseExpr(tokens: Token[], index: number): { expr: Expr; nextIndex: number } {
  const token = tokens[index];
  if (token === undefined) {
    throw new EvalError('unexpected end of input');
  }

  if (token.kind === 'paren') {
    if (token.value === ')') {
      throw new EvalError('unexpected )');
    }

    const elements: Expr[] = [];
    let nextIndex = index + 1;

    while (nextIndex < tokens.length) {
      const nextToken = tokens[nextIndex];
      if (nextToken.kind === 'paren' && nextToken.value === ')') {
        return {
          expr: { kind: 'list', elements },
          nextIndex: nextIndex + 1,
        };
      }

      const parsed = parseExpr(tokens, nextIndex);
      elements.push(parsed.expr);
      nextIndex = parsed.nextIndex;
    }

    throw new EvalError('unterminated list');
  }

  if (token.kind === 'string') {
    return {
      expr: { kind: 'string', value: token.value },
      nextIndex: index + 1,
    };
  }

  return {
    expr: parseAtom(token.value),
    nextIndex: index + 1,
  };
}

function parseAtom(text: string): Expr {
  if (text === '#t') {
    return { kind: 'boolean', value: true };
  }

  if (text === '#f') {
    return { kind: 'boolean', value: false };
  }

  if (/^[+-]?\d+$/.test(text)) {
    return { kind: 'number', value: Number.parseInt(text, 10) };
  }

  return { kind: 'symbol', name: text };
}

function evaluateExpr(expr: Expr): Value {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      throw new EvalError(`unbound symbol: ${expr.name}`);
    case 'list':
      return evaluateList(expr.elements);
  }
}

function evaluateList(elements: Expr[]): Value {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const [head, ...argExprs] = elements;
  if (head.kind !== 'symbol') {
    throw new EvalError('first element in a list must be a procedure name');
  }

  if (head.name === 'and') {
    return evaluateAnd(argExprs);
  }

  if (head.name === 'or') {
    return evaluateOr(argExprs);
  }

  const args = argExprs.map((expr) => evaluateExpr(expr));
  return applyBuiltin(head.name, args);
}

function evaluateAnd(argExprs: Expr[]): Value {
  let lastValue: Value = { kind: 'boolean', value: true };

  for (const expr of argExprs) {
    lastValue = evaluateExpr(expr);
    if (!isTruthy(lastValue)) {
      return lastValue;
    }
  }

  return lastValue;
}

function evaluateOr(argExprs: Expr[]): Value {
  for (const expr of argExprs) {
    const value = evaluateExpr(expr);
    if (isTruthy(value)) {
      return value;
    }
  }

  return { kind: 'boolean', value: false };
}

function applyBuiltin(name: string, args: Value[]): Value {
  switch (name) {
    case '+':
      return makeNumber(args.map((arg) => expectNumber(arg, '+')).reduce((sum, value) => sum + value, 0));
    case '-':
      return applySubtraction(args);
    case '*':
      return makeNumber(args.map((arg) => expectNumber(arg, '*')).reduce((product, value) => product * value, 1));
    case '/':
      return applyDivision(args);
    case '<':
      return applyComparison(args, '<', (left, right) => left < right);
    case '>':
      return applyComparison(args, '>', (left, right) => left > right);
    case '=':
      return applyComparison(args, '=', (left, right) => left === right);
    case '<=':
      return applyComparison(args, '<=', (left, right) => left <= right);
    case 'not':
      if (args.length !== 1) {
        throw new EvalError('not expects exactly 1 argument');
      }
      return { kind: 'boolean', value: !isTruthy(args[0]) };
    default:
      throw new EvalError(`unknown procedure: ${name}`);
  }
}

function applySubtraction(args: Value[]): Value {
  if (args.length === 0) {
    throw new EvalError('- expects at least 1 argument');
  }

  const numbers = args.map((arg) => expectNumber(arg, '-'));
  if (numbers.length === 1) {
    return makeNumber(-numbers[0]);
  }

  const [first, ...rest] = numbers;
  return makeNumber(rest.reduce((result, value) => result - value, first));
}

function applyDivision(args: Value[]): Value {
  if (args.length === 0) {
    throw new EvalError('/ expects at least 1 argument');
  }

  const numbers = args.map((arg) => expectNumber(arg, '/'));
  if (numbers.length === 1) {
    if (numbers[0] === 0) {
      throw new EvalError('division by zero');
    }
    return makeNumber(1 / numbers[0]);
  }

  const [first, ...rest] = numbers;
  let result = first;

  for (const value of rest) {
    if (value === 0) {
      throw new EvalError('division by zero');
    }
    result /= value;
  }

  return makeNumber(result);
}

function applyComparison(
  args: Value[],
  name: string,
  predicate: (left: number, right: number) => boolean,
): Value {
  if (args.length < 2) {
    throw new EvalError(`${name} expects at least 2 arguments`);
  }

  const numbers = args.map((arg) => expectNumber(arg, name));
  for (let index = 0; index < numbers.length - 1; index += 1) {
    if (!predicate(numbers[index], numbers[index + 1])) {
      return { kind: 'boolean', value: false };
    }
  }

  return { kind: 'boolean', value: true };
}

function expectNumber(value: Value, procedure: string): number {
  if (value.kind !== 'number') {
    throw new EvalError(`${procedure} expects numeric arguments`);
  }
  return value.value;
}

function isTruthy(value: Value): boolean {
  return value.kind !== 'boolean' || value.value;
}

function makeNumber(value: number): Value {
  return {
    kind: 'number',
    value: Object.is(value, -0) ? 0 : value,
  };
}

function formatValue(value: Value): string {
  switch (value.kind) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return `"${escapeString(value.value)}"`;
  }
}

function formatNumber(value: number): string {
  if (Object.is(value, -0)) {
    return '0';
  }

  if (Number.isInteger(value)) {
    return value.toString(10);
  }

  return String(value);
}

function escapeString(value: string): string {
  return value
    .replaceAll('\\', '\\\\')
    .replaceAll('"', '\\"')
    .replaceAll('\n', '\\n')
    .replaceAll('\r', '\\r')
    .replaceAll('\t', '\\t');
}

function isWhitespace(char: string): boolean {
  return /\s/.test(char);
}

function isDelimiter(char: string): boolean {
  return isWhitespace(char) || char === '(' || char === ')' || char === ';';
}
