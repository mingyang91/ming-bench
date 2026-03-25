import { EvalError } from './evalError.js';

type Expr =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; value: string }
  | { kind: 'list'; items: Expr[] };

type Token =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; value: string }
  | { kind: 'paren'; value: '(' | ')' };

type SchemeValue =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | ProcedureValue;

type Environment = ReadonlyMap<string, ProcedureValue>;

interface ProcedureValue {
  kind: 'procedure';
  name: string;
  apply(args: SchemeValue[]): SchemeValue;
}

interface ParserState {
  tokens: Token[];
  index: number;
}

const TRUE_VALUE: SchemeValue = { kind: 'boolean', value: true };
const FALSE_VALUE: SchemeValue = { kind: 'boolean', value: false };

const GLOBAL_ENV: Environment = new Map<string, ProcedureValue>([
  ['+', makeProcedure('+', (args) => numberValue(sum(asNumbers(args, '+'))))],
  ['-', makeProcedure('-', (args) => numberValue(subtract(asNumbers(args, '-'))))],
  ['*', makeProcedure('*', (args) => numberValue(product(asNumbers(args, '*'))))],
  ['/', makeProcedure('/', (args) => numberValue(divide(asNumbers(args, '/'))))],
  ['<', makeProcedure('<', (args) => booleanValue(compareChain(asNumbers(args, '<'), (a, b) => a < b, '<')))],
  ['>', makeProcedure('>', (args) => booleanValue(compareChain(asNumbers(args, '>'), (a, b) => a > b, '>')))],
  ['=', makeProcedure('=', (args) => booleanValue(compareChain(asNumbers(args, '='), (a, b) => a === b, '=')))],
  ['<=', makeProcedure('<=', (args) => booleanValue(compareChain(asNumbers(args, '<='), (a, b) => a <= b, '<=')))],
  ['not', makeProcedure('not', (args) => applyNot(args))],
]);

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const expressions = parseProgram(input);
  if (expressions.length === 0) {
    throw new EvalError('empty input');
  }

  let result: SchemeValue = FALSE_VALUE;
  for (const expression of expressions) {
    result = evaluate(expression, GLOBAL_ENV);
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
  const state: ParserState = { tokens: tokenize(input), index: 0 };
  const expressions: Expr[] = [];

  while (state.index < state.tokens.length) {
    expressions.push(parseExpression(state));
  }

  return expressions;
}

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let index = 0;

  while (index < input.length) {
    const char = input[index];

    if (char === undefined) {
      break;
    }

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
      const parsed = readStringToken(input, index);
      tokens.push({ kind: 'string', value: parsed.value });
      index = parsed.nextIndex;
      continue;
    }

    const parsed = readAtomToken(input, index);
    tokens.push(parsed.token);
    index = parsed.nextIndex;
  }

  return tokens;
}

function parseExpression(state: ParserState): Expr {
  const token = state.tokens[state.index];
  if (token === undefined) {
    throw new EvalError('unexpected end of input');
  }

  state.index += 1;

  switch (token.kind) {
    case 'number':
      return { kind: 'number', value: token.value };
    case 'boolean':
      return { kind: 'boolean', value: token.value };
    case 'string':
      return { kind: 'string', value: token.value };
    case 'symbol':
      return { kind: 'symbol', value: token.value };
    case 'paren':
      if (token.value === ')') {
        throw new EvalError('unexpected )');
      }

      return parseList(state);
  }
}

function parseList(state: ParserState): Expr {
  const items: Expr[] = [];

  while (true) {
    const token = state.tokens[state.index];
    if (token === undefined) {
      throw new EvalError('unterminated list');
    }

    if (token.kind === 'paren' && token.value === ')') {
      state.index += 1;
      return { kind: 'list', items };
    }

    items.push(parseExpression(state));
  }
}

function evaluate(expression: Expr, env: Environment): SchemeValue {
  switch (expression.kind) {
    case 'number':
      return numberValue(expression.value);
    case 'boolean':
      return booleanValue(expression.value);
    case 'string':
      return stringValue(expression.value);
    case 'symbol': {
      const value = env.get(expression.value);
      if (value === undefined) {
        throw new EvalError(`unbound variable: ${expression.value}`);
      }
      return value;
    }
    case 'list':
      return evaluateList(expression.items, env);
  }
}

function evaluateList(items: Expr[], env: Environment): SchemeValue {
  if (items.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const head = items[0];
  if (head.kind === 'symbol') {
    if (head.value === 'and') {
      return evaluateAnd(items.slice(1), env);
    }

    if (head.value === 'or') {
      return evaluateOr(items.slice(1), env);
    }
  }

  const procedure = evaluate(head, env);
  const args = items.slice(1).map((item) => evaluate(item, env));
  return applyProcedure(procedure, args);
}

function evaluateAnd(items: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = TRUE_VALUE;
  for (const item of items) {
    result = evaluate(item, env);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(items: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = FALSE_VALUE;
  for (const item of items) {
    result = evaluate(item, env);
    if (isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function applyProcedure(value: SchemeValue, args: SchemeValue[]): SchemeValue {
  if (value.kind !== 'procedure') {
    throw new EvalError('attempted to call a non-procedure');
  }

  return value.apply(args);
}

function makeProcedure(name: string, apply: (args: SchemeValue[]) => SchemeValue): ProcedureValue {
  return { kind: 'procedure', name, apply };
}

function applyNot(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('not', args, 1);
  return booleanValue(!isTruthy(args[0]));
}

function asNumbers(args: SchemeValue[], name: string): number[] {
  return args.map((arg) => expectNumber(arg, name));
}

function expectNumber(value: SchemeValue, name: string): number {
  if (value.kind !== 'number') {
    throw new EvalError(`${name} expected numbers`);
  }

  return value.value;
}

function expectExactArgCount(name: string, args: SchemeValue[], expected: number): void {
  if (args.length !== expected) {
    throw new EvalError(`${name} expected ${expected} argument(s), got ${args.length}`);
  }
}

function expectAtLeastArgCount(name: string, values: number[], minimum: number): void {
  if (values.length < minimum) {
    throw new EvalError(`${name} expected at least ${minimum} argument(s), got ${values.length}`);
  }
}

function sum(values: number[]): number {
  return values.reduce((total, value) => total + value, 0);
}

function subtract(values: number[]): number {
  expectAtLeastArgCount('-', values, 1);
  if (values.length === 1) {
    return -values[0];
  }

  return values.slice(1).reduce((total, value) => total - value, values[0]);
}

function product(values: number[]): number {
  return values.reduce((total, value) => total * value, 1);
}

function divide(values: number[]): number {
  expectAtLeastArgCount('/', values, 1);

  if (values.length === 1) {
    return checkedDivision(1, values[0]);
  }

  let result = values[0];
  for (const value of values.slice(1)) {
    result = checkedDivision(result, value);
  }

  return result;
}

function checkedDivision(left: number, right: number): number {
  if (right === 0) {
    throw new EvalError('division by zero');
  }

  return left / right;
}

function compareChain(
  values: number[],
  predicate: (left: number, right: number) => boolean,
  name: string,
): boolean {
  expectAtLeastArgCount(name, values, 2);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function formatValue(value: SchemeValue): string {
  switch (value.kind) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return JSON.stringify(value.value);
    case 'procedure':
      return `#<procedure:${value.name}>`;
  }
}

function formatNumber(value: number): string {
  if (Object.is(value, -0)) {
    return '0';
  }

  return Number.isInteger(value) ? String(Math.trunc(value)) : String(value);
}

function isTruthy(value: SchemeValue): boolean {
  return value.kind !== 'boolean' || value.value;
}

function numberValue(value: number): SchemeValue {
  return { kind: 'number', value };
}

function booleanValue(value: boolean): SchemeValue {
  return value ? TRUE_VALUE : FALSE_VALUE;
}

function stringValue(value: string): SchemeValue {
  return { kind: 'string', value };
}

function isWhitespace(char: string): boolean {
  return /\s/u.test(char);
}

function skipComment(input: string, index: number): number {
  let nextIndex = index;
  while (nextIndex < input.length && input[nextIndex] !== '\n') {
    nextIndex += 1;
  }
  return nextIndex;
}

function readStringToken(input: string, startIndex: number): { value: string; nextIndex: number } {
  let value = '';
  let index = startIndex + 1;

  while (index < input.length) {
    const char = input[index];
    if (char === undefined) {
      break;
    }

    if (char === '"') {
      return { value, nextIndex: index + 1 };
    }

    if (char === '\\') {
      index += 1;
      const escaped = input[index];
      if (escaped === undefined) {
        throw new EvalError('unterminated string literal');
      }

      value += decodeEscape(escaped);
      index += 1;
      continue;
    }

    value += char;
    index += 1;
  }

  throw new EvalError('unterminated string literal');
}

function decodeEscape(char: string): string {
  switch (char) {
    case '"':
      return '"';
    case '\\':
      return '\\';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    default:
      return char;
  }
}

function readAtomToken(input: string, startIndex: number): { token: Token; nextIndex: number } {
  let index = startIndex;
  while (index < input.length && !isTokenBoundary(input[index])) {
    index += 1;
  }

  const raw = input.slice(startIndex, index);
  return { token: tokenFromAtom(raw), nextIndex: index };
}

function isTokenBoundary(char: string | undefined): boolean {
  return char === undefined || isWhitespace(char) || char === '(' || char === ')' || char === ';';
}

function tokenFromAtom(raw: string): Token {
  if (raw === '#t') {
    return { kind: 'boolean', value: true };
  }

  if (raw === '#f') {
    return { kind: 'boolean', value: false };
  }

  if (/^[+-]?\d+$/u.test(raw) && raw !== '+' && raw !== '-') {
    return { kind: 'number', value: Number(raw) };
  }

  return { kind: 'symbol', value: raw };
}
