import { EvalError } from './evalError.js';

type Expr =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; name: string }
  | { kind: 'list'; elements: Expr[] };

type BuiltinValue = {
  kind: 'builtin';
  name: string;
  apply: (args: SchemeValue[]) => SchemeValue;
};

type SchemeValue = number | boolean | string | BuiltinValue;

type Token =
  | { kind: 'lparen' }
  | { kind: 'rparen' }
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; value: string };

const builtins = new Map<string, BuiltinValue>([
  ['+', builtin('+', (args) => sum(args, 0))],
  ['-', builtin('-', (args) => subtract(args))],
  ['*', builtin('*', (args) => product(args, 1))],
  ['/', builtin('/', (args) => divide(args))],
  ['<', builtin('<', (args) => compareChain('<', args, (left, right) => left < right))],
  ['>', builtin('>', (args) => compareChain('>', args, (left, right) => left > right))],
  ['=', builtin('=', (args) => compareChain('=', args, (left, right) => left === right))],
  ['<=', builtin('<=', (args) => compareChain('<=', args, (left, right) => left <= right))],
  [
    'not',
    builtin('not', (args) => {
      expectArity('not', args, 1);
      return isFalse(args[0]);
    }),
  ],
]);

export function evalStr(input: string): string {
  const parser = new Parser(tokenize(input));
  const expressions = parser.parseProgram();

  if (expressions.length === 0) {
    throw new EvalError('expected at least one expression');
  }

  let result: SchemeValue = false;

  for (const expression of expressions) {
    result = evaluate(expression);
  }

  return formatValue(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  return {
    result: evalStr(input),
    output: '',
  };
}

function evaluate(expression: Expr): SchemeValue {
  switch (expression.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expression.value;
    case 'symbol': {
      const value = builtins.get(expression.name);
      if (value === undefined) {
        throw new EvalError(`unbound symbol: ${expression.name}`);
      }
      return value;
    }
    case 'list':
      return evaluateList(expression.elements);
  }
}

function evaluateList(elements: Expr[]): SchemeValue {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const [operatorExpr, ...argumentExprs] = elements;

  if (operatorExpr.kind === 'symbol') {
    if (operatorExpr.name === 'and') {
      return evaluateAnd(argumentExprs);
    }

    if (operatorExpr.name === 'or') {
      return evaluateOr(argumentExprs);
    }
  }

  const operator = evaluate(operatorExpr);

  if (!isBuiltin(operator)) {
    throw new EvalError('attempted to call a non-procedure value');
  }

  const args = argumentExprs.map((argument) => evaluate(argument));
  return operator.apply(args);
}

function evaluateAnd(expressions: Expr[]): SchemeValue {
  let result: SchemeValue = true;

  for (const expression of expressions) {
    result = evaluate(expression);
    if (isFalse(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(expressions: Expr[]): SchemeValue {
  let result: SchemeValue = false;

  for (const expression of expressions) {
    result = evaluate(expression);
    if (!isFalse(result)) {
      return result;
    }
  }

  return result;
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
      while (index < input.length && input[index] !== '\n') {
        index += 1;
      }
      continue;
    }

    if (char === '(') {
      tokens.push({ kind: 'lparen' });
      index += 1;
      continue;
    }

    if (char === ')') {
      tokens.push({ kind: 'rparen' });
      index += 1;
      continue;
    }

    if (char === '"') {
      const { value, nextIndex } = readStringLiteral(input, index);
      tokens.push({ kind: 'string', value });
      index = nextIndex;
      continue;
    }

    let end = index;

    while (end < input.length) {
      const next = input[end];
      if (isWhitespace(next) || next === '(' || next === ')' || next === ';') {
        break;
      }
      end += 1;
    }

    const rawToken = input.slice(index, end);

    if (rawToken.length === 0) {
      throw new EvalError('unexpected token');
    }

    if (rawToken === '#t') {
      tokens.push({ kind: 'boolean', value: true });
    } else if (rawToken === '#f') {
      tokens.push({ kind: 'boolean', value: false });
    } else if (/^[+-]?\d+$/.test(rawToken)) {
      tokens.push({ kind: 'number', value: Number(rawToken) });
    } else {
      tokens.push({ kind: 'symbol', value: rawToken });
    }

    index = end;
  }

  return tokens;
}

function readStringLiteral(input: string, startIndex: number): { value: string; nextIndex: number } {
  let index = startIndex + 1;
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
      if (escaped === 'n') {
        value += '\n';
      } else if (escaped === 't') {
        value += '\t';
      } else if (escaped === '"' || escaped === '\\') {
        value += escaped;
      } else {
        value += escaped;
      }

      index += 1;
      continue;
    }

    value += char;
    index += 1;
  }

  throw new EvalError('unterminated string literal');
}

class Parser {
  private readonly tokens: Token[];
  private index = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  parseProgram(): Expr[] {
    const expressions: Expr[] = [];

    while (!this.isAtEnd()) {
      expressions.push(this.parseExpr());
    }

    return expressions;
  }

  private parseExpr(): Expr {
    const token = this.advance();

    if (token === undefined) {
      throw new EvalError('unexpected end of input');
    }

    switch (token.kind) {
      case 'number':
        return { kind: 'number', value: token.value };
      case 'boolean':
        return { kind: 'boolean', value: token.value };
      case 'string':
        return { kind: 'string', value: token.value };
      case 'symbol':
        return { kind: 'symbol', name: token.value };
      case 'lparen': {
        const elements: Expr[] = [];

        while (true) {
          const next = this.peek();

          if (next === undefined) {
            throw new EvalError('unterminated list');
          }

          if (next.kind === 'rparen') {
            this.advance();
            return { kind: 'list', elements };
          }

          elements.push(this.parseExpr());
        }
      }
      case 'rparen':
        throw new EvalError('unexpected )');
    }
  }

  private peek(): Token | undefined {
    return this.tokens[this.index];
  }

  private advance(): Token | undefined {
    const token = this.tokens[this.index];
    this.index += 1;
    return token;
  }

  private isAtEnd(): boolean {
    return this.index >= this.tokens.length;
  }
}

function builtin(name: string, apply: (args: SchemeValue[]) => SchemeValue): BuiltinValue {
  return { kind: 'builtin', name, apply };
}

function isBuiltin(value: SchemeValue): value is BuiltinValue {
  return typeof value === 'object' && value !== null && value.kind === 'builtin';
}

function expectArity(name: string, args: SchemeValue[], expected: number): void {
  if (args.length !== expected) {
    throw new EvalError(`${name} expected ${expected} argument(s), got ${args.length}`);
  }
}

function expectAtLeastArity(name: string, args: SchemeValue[], min: number): void {
  if (args.length < min) {
    throw new EvalError(`${name} expected at least ${min} argument(s), got ${args.length}`);
  }
}

function expectNumber(value: SchemeValue, name: string): number {
  if (typeof value !== 'number') {
    throw new EvalError(`${name} expected a number`);
  }

  return value;
}

function sum(args: SchemeValue[], identity: number): number {
  let total = identity;

  for (const arg of args) {
    total += expectNumber(arg, '+');
  }

  return normalizeNumber(total);
}

function subtract(args: SchemeValue[]): number {
  expectAtLeastArity('-', args, 1);

  if (args.length === 1) {
    return normalizeNumber(-expectNumber(args[0], '-'));
  }

  let total = expectNumber(args[0], '-');

  for (const arg of args.slice(1)) {
    total -= expectNumber(arg, '-');
  }

  return normalizeNumber(total);
}

function product(args: SchemeValue[], identity: number): number {
  let total = identity;

  for (const arg of args) {
    total *= expectNumber(arg, '*');
  }

  return normalizeNumber(total);
}

function divide(args: SchemeValue[]): number {
  expectAtLeastArity('/', args, 2);

  let total = expectNumber(args[0], '/');

  for (const arg of args.slice(1)) {
    const divisor = expectNumber(arg, '/');

    if (divisor === 0) {
      throw new EvalError('division by zero');
    }

    total /= divisor;
  }

  return normalizeNumber(total);
}

function compareChain(
  name: string,
  args: SchemeValue[],
  predicate: (left: number, right: number) => boolean,
): boolean {
  expectAtLeastArity(name, args, 2);

  const numbers = args.map((arg) => expectNumber(arg, name));

  for (let index = 0; index < numbers.length - 1; index += 1) {
    if (!predicate(numbers[index], numbers[index + 1])) {
      return false;
    }
  }

  return true;
}

function normalizeNumber(value: number): number {
  return Object.is(value, -0) ? 0 : value;
}

function isFalse(value: SchemeValue): boolean {
  return value === false;
}

function isWhitespace(value: string): boolean {
  return /\s/.test(value);
}

function formatValue(value: SchemeValue): string {
  if (typeof value === 'number') {
    return String(normalizeNumber(value));
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  if (typeof value === 'string') {
    return JSON.stringify(value);
  }

  if (isBuiltin(value)) {
    return `#<procedure:${value.name}>`;
  }

  throw new EvalError('cannot format value');
}
