import { EvalError } from './evalError.js';

type Expr =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; name: string }
  | { kind: 'list'; items: Expr[] };

type Value = number | boolean | string | BuiltinProc;

interface BuiltinProc {
  kind: 'builtin';
  name: string;
  apply: (args: Value[]) => Value;
}

class Parser {
  private index = 0;

  constructor(private readonly input: string) {}

  parseProgram(): Expr[] {
    const exprs: Expr[] = [];

    this.skipIgnored();
    while (!this.isAtEnd()) {
      exprs.push(this.parseExpr());
      this.skipIgnored();
    }

    return exprs;
  }

  private parseExpr(): Expr {
    this.skipIgnored();

    if (this.isAtEnd()) {
      throw new EvalError('unexpected end of input');
    }

    const ch = this.peek();

    if (ch === '(') {
      return this.parseList();
    }

    if (ch === ')') {
      throw new EvalError('unexpected )');
    }

    if (ch === '"') {
      return this.parseString();
    }

    return this.parseAtom();
  }

  private parseList(): Expr {
    this.advance();

    const items: Expr[] = [];
    this.skipIgnored();

    while (!this.isAtEnd() && this.peek() !== ')') {
      items.push(this.parseExpr());
      this.skipIgnored();
    }

    if (this.isAtEnd()) {
      throw new EvalError('unterminated list');
    }

    this.advance();
    return { kind: 'list', items };
  }

  private parseString(): Expr {
    this.advance();

    let value = '';
    while (!this.isAtEnd()) {
      const ch = this.advance();

      if (ch === '"') {
        return { kind: 'string', value };
      }

      if (ch === '\\') {
        if (this.isAtEnd()) {
          throw new EvalError('unterminated string');
        }

        const escaped = this.advance();
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
          case 't':
            value += '\t';
            break;
          default:
            value += escaped;
            break;
        }
      } else {
        value += ch;
      }
    }

    throw new EvalError('unterminated string');
  }

  private parseAtom(): Expr {
    const start = this.index;

    while (!this.isAtEnd() && !isDelimiter(this.peek())) {
      this.advance();
    }

    const token = this.input.slice(start, this.index);
    if (token.length === 0) {
      throw new EvalError('expected expression');
    }

    if (token === '#t') {
      return { kind: 'boolean', value: true };
    }

    if (token === '#f') {
      return { kind: 'boolean', value: false };
    }

    if (/^[+-]?\d+$/.test(token) && token !== '+' && token !== '-') {
      return { kind: 'number', value: Number.parseInt(token, 10) };
    }

    return { kind: 'symbol', name: token };
  }

  private skipIgnored(): void {
    while (!this.isAtEnd()) {
      const ch = this.peek();

      if (isWhitespace(ch)) {
        this.advance();
        continue;
      }

      if (ch === ';') {
        while (!this.isAtEnd() && this.peek() !== '\n') {
          this.advance();
        }
        continue;
      }

      return;
    }
  }

  private isAtEnd(): boolean {
    return this.index >= this.input.length;
  }

  private peek(): string {
    return this.input[this.index]!;
  }

  private advance(): string {
    const ch = this.input[this.index]!;
    this.index += 1;
    return ch;
  }
}

const BUILTINS = new Map<string, BuiltinProc>([
  builtin('+', (args) => sumNumbers('+', args, 0)),
  builtin('*', (args) => productNumbers('*', args, 1)),
  builtin('-', (args) => subtractNumbers(args)),
  builtin('/', (args) => divideNumbers(args)),
  builtin('<', (args) => compareNumbers('<', args, (left, right) => left < right)),
  builtin('>', (args) => compareNumbers('>', args, (left, right) => left > right)),
  builtin('=', (args) => compareNumbers('=', args, (left, right) => left === right)),
  builtin('<=', (args) => compareNumbers('<=', args, (left, right) => left <= right)),
  builtin('not', (args) => {
    assertExactArity('not', args, 1);
    return !isTruthy(args[0]!);
  }),
]);

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  return evalStrWithOutput(input).result;
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const program = new Parser(input).parseProgram();
  if (program.length === 0) {
    throw new EvalError('empty input');
  }

  let lastValue: Value | undefined;
  for (const expr of program) {
    lastValue = evalExpr(expr);
  }

  return {
    result: formatValue(lastValue!),
    output: '',
  };
}

function evalExpr(expr: Expr): Value {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr.value;

    case 'symbol': {
      const value = BUILTINS.get(expr.name);
      if (value === undefined) {
        throw new EvalError(`unbound symbol: ${expr.name}`);
      }
      return value;
    }

    case 'list':
      return evalList(expr.items);
  }
}

function evalList(items: Expr[]): Value {
  if (items.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const first = items[0]!;
  if (first.kind === 'symbol') {
    if (first.name === 'and') {
      return evalAnd(items.slice(1));
    }

    if (first.name === 'or') {
      return evalOr(items.slice(1));
    }
  }

  const proc = evalExpr(first);
  if (!isBuiltinProc(proc)) {
    throw new EvalError('attempted to call a non-procedure');
  }

  const args = items.slice(1).map((item) => evalExpr(item));
  return proc.apply(args);
}

function evalAnd(args: Expr[]): Value {
  let result: Value = true;

  for (const arg of args) {
    result = evalExpr(arg);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evalOr(args: Expr[]): Value {
  for (const arg of args) {
    const value = evalExpr(arg);
    if (isTruthy(value)) {
      return value;
    }
  }

  return false;
}

function builtin(name: string, apply: (args: Value[]) => Value): [string, BuiltinProc] {
  return [name, { kind: 'builtin', name, apply }];
}

function sumNumbers(name: string, args: Value[], initial: number): number {
  const numbers = expectNumbers(name, args);
  return normalizeNumber(numbers.reduce((sum, value) => sum + value, initial));
}

function productNumbers(name: string, args: Value[], initial: number): number {
  const numbers = expectNumbers(name, args);
  return normalizeNumber(numbers.reduce((product, value) => product * value, initial));
}

function subtractNumbers(args: Value[]): number {
  const numbers = expectNumbers('-', args);
  assertAtLeastArity('-', numbers, 1);

  if (numbers.length === 1) {
    return normalizeNumber(-numbers[0]!);
  }

  return normalizeNumber(numbers.slice(1).reduce((acc, value) => acc - value, numbers[0]!));
}

function divideNumbers(args: Value[]): number {
  const numbers = expectNumbers('/', args);
  assertAtLeastArity('/', numbers, 1);

  if (numbers.length === 1) {
    if (numbers[0] === 0) {
      throw new EvalError('division by zero');
    }

    return normalizeNumber(1 / numbers[0]!);
  }

  let result = numbers[0]!;
  for (const divisor of numbers.slice(1)) {
    if (divisor === 0) {
      throw new EvalError('division by zero');
    }
    result /= divisor;
  }

  return normalizeNumber(result);
}

function compareNumbers(
  name: string,
  args: Value[],
  compare: (left: number, right: number) => boolean,
): boolean {
  const numbers = expectNumbers(name, args);
  assertAtLeastArity(name, numbers, 2);

  for (let index = 1; index < numbers.length; index += 1) {
    if (!compare(numbers[index - 1]!, numbers[index]!)) {
      return false;
    }
  }

  return true;
}

function expectNumbers(name: string, args: Value[]): number[] {
  return args.map((arg) => {
    if (typeof arg !== 'number') {
      throw new EvalError(`${name} expects number arguments`);
    }
    return arg;
  });
}

function assertExactArity(name: string, args: readonly unknown[], expected: number): void {
  if (args.length !== expected) {
    throw new EvalError(`${name} expects exactly ${expected} argument(s)`);
  }
}

function assertAtLeastArity(name: string, args: readonly unknown[], minimum: number): void {
  if (args.length < minimum) {
    throw new EvalError(`${name} expects at least ${minimum} argument(s)`);
  }
}

function formatValue(value: Value): string {
  if (typeof value === 'number') {
    return formatNumber(value);
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  if (typeof value === 'string') {
    return JSON.stringify(value);
  }

  return `#<procedure:${value.name}>`;
}

function formatNumber(value: number): string {
  const normalized = normalizeNumber(value);
  return Number.isInteger(normalized) ? normalized.toString() : String(normalized);
}

function normalizeNumber(value: number): number {
  return Object.is(value, -0) ? 0 : value;
}

function isTruthy(value: Value): boolean {
  return value !== false;
}

function isBuiltinProc(value: Value): value is BuiltinProc {
  return typeof value === 'object' && value !== null && value.kind === 'builtin';
}

function isWhitespace(ch: string): boolean {
  return /\s/.test(ch);
}

function isDelimiter(ch: string): boolean {
  return isWhitespace(ch) || ch === '(' || ch === ')' || ch === ';';
}
