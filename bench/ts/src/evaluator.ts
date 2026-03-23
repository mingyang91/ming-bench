import { EvalError } from './evalError.js';

type Expr =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; name: string }
  | { kind: 'list'; elements: Expr[] };

type Value = boolean | number | string;

class Parser {
  private index = 0;

  constructor(private readonly input: string) {}

  parseProgram(): Expr[] {
    const expressions: Expr[] = [];

    this.skipIgnored();
    while (!this.isAtEnd()) {
      expressions.push(this.parseExpr());
      this.skipIgnored();
    }

    return expressions;
  }

  private parseExpr(): Expr {
    this.skipIgnored();

    if (this.isAtEnd()) {
      throw new EvalError('unexpected end of input');
    }

    const char = this.peek();
    if (char === '(') {
      return this.parseList();
    }
    if (char === ')') {
      throw new EvalError("unexpected ')'");
    }
    if (char === '"') {
      return this.parseString();
    }

    return this.parseAtom();
  }

  private parseList(): Expr {
    this.advance(); // (
    const elements: Expr[] = [];

    while (true) {
      this.skipIgnored();

      if (this.isAtEnd()) {
        throw new EvalError("unterminated list: missing ')'");
      }
      if (this.peek() === ')') {
        this.advance();
        return { kind: 'list', elements };
      }

      elements.push(this.parseExpr());
    }
  }

  private parseString(): Expr {
    this.advance(); // opening quote
    let value = '';

    while (!this.isAtEnd()) {
      const char = this.advance();
      if (char === '"') {
        return { kind: 'string', value };
      }
      if (char === '\\') {
        if (this.isAtEnd()) {
          throw new EvalError('unterminated string escape');
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
        continue;
      }

      value += char;
    }

    throw new EvalError('unterminated string literal');
  }

  private parseAtom(): Expr {
    const start = this.index;

    while (!this.isAtEnd() && !this.isDelimiter(this.peek())) {
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
    if (/^[+-]?\d+$/.test(token)) {
      return { kind: 'number', value: Number.parseInt(token, 10) };
    }

    return { kind: 'symbol', name: token };
  }

  private skipIgnored(): void {
    while (!this.isAtEnd()) {
      const char = this.peek();
      if (/\s/u.test(char)) {
        this.advance();
        continue;
      }
      if (char === ';') {
        while (!this.isAtEnd() && this.peek() !== '\n') {
          this.advance();
        }
        continue;
      }

      return;
    }
  }

  private isDelimiter(char: string): boolean {
    return /\s/u.test(char) || char === '(' || char === ')' || char === ';';
  }

  private peek(): string {
    return this.input[this.index]!;
  }

  private advance(): string {
    return this.input[this.index++]!;
  }

  private isAtEnd(): boolean {
    return this.index >= this.input.length;
  }
}

function evaluate(expr: Expr): Value {
  switch (expr.kind) {
    case 'number':
      return expr.value;
    case 'boolean':
      return expr.value;
    case 'string':
      return expr.value;
    case 'symbol':
      throw new EvalError(`unbound variable: ${expr.name}`);
    case 'list':
      return evaluateList(expr.elements);
  }
}

function evaluateList(elements: Expr[]): Value {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate an empty list');
  }

  const [operator, ...arguments_] = elements;
  if (operator.kind !== 'symbol') {
    throw new EvalError('operator must be a symbol');
  }

  switch (operator.name) {
    case 'and':
      return evaluateAnd(arguments_);
    case 'or':
      return evaluateOr(arguments_);
    case 'not':
      return evaluateNot(arguments_);
    case '+':
      return evaluateAdd(arguments_);
    case '-':
      return evaluateSubtract(arguments_);
    case '*':
      return evaluateMultiply(arguments_);
    case '/':
      return evaluateDivide(arguments_);
    case '<':
      return evaluateComparison(arguments_, '<', (left, right) => left < right);
    case '>':
      return evaluateComparison(arguments_, '>', (left, right) => left > right);
    case '=':
      return evaluateComparison(arguments_, '=', (left, right) => left === right);
    case '<=':
      return evaluateComparison(arguments_, '<=', (left, right) => left <= right);
    default:
      throw new EvalError(`unknown procedure: ${operator.name}`);
  }
}

function evaluateAnd(arguments_: Expr[]): Value {
  let result: Value = true;

  for (const argument of arguments_) {
    result = evaluate(argument);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(arguments_: Expr[]): Value {
  for (const argument of arguments_) {
    const value = evaluate(argument);
    if (isTruthy(value)) {
      return value;
    }
  }

  return false;
}

function evaluateNot(arguments_: Expr[]): Value {
  assertArity('not', arguments_, 1);
  return !isTruthy(evaluate(arguments_[0]!));
}

function evaluateAdd(arguments_: Expr[]): Value {
  const numbers = evaluateNumberArguments('+', arguments_, 0);
  return numbers.reduce((sum, value) => sum + value, 0);
}

function evaluateSubtract(arguments_: Expr[]): Value {
  const numbers = evaluateNumberArguments('-', arguments_, 1);
  if (numbers.length === 1) {
    return normalizeNumber(-numbers[0]!);
  }

  const [first, ...rest] = numbers;
  return normalizeNumber(rest.reduce((difference, value) => difference - value, first!));
}

function evaluateMultiply(arguments_: Expr[]): Value {
  const numbers = evaluateNumberArguments('*', arguments_, 0);
  return numbers.reduce((product, value) => product * value, 1);
}

function evaluateDivide(arguments_: Expr[]): Value {
  const numbers = evaluateNumberArguments('/', arguments_, 1);
  if (numbers.length === 1) {
    return normalizeNumber(divideNumbers(1, numbers[0]!));
  }

  const [first, ...rest] = numbers;
  return normalizeNumber(rest.reduce((quotient, value) => divideNumbers(quotient, value), first!));
}

function evaluateComparison(
  arguments_: Expr[],
  name: string,
  comparator: (left: number, right: number) => boolean,
): Value {
  const numbers = evaluateNumberArguments(name, arguments_, 2);

  for (let index = 0; index < numbers.length - 1; index += 1) {
    if (!comparator(numbers[index]!, numbers[index + 1]!)) {
      return false;
    }
  }

  return true;
}

function evaluateNumberArguments(name: string, arguments_: Expr[], minimum: number): number[] {
  if (arguments_.length < minimum) {
    const plural = minimum === 1 ? '' : 's';
    throw new EvalError(`${name}: expected at least ${minimum} argument${plural}`);
  }

  return arguments_.map((argument) => {
    const value = evaluate(argument);
    if (typeof value !== 'number') {
      throw new EvalError(`${name}: expected number`);
    }
    return value;
  });
}

function assertArity(name: string, arguments_: Expr[], expected: number): void {
  if (arguments_.length !== expected) {
    throw new EvalError(`${name}: expected ${expected} argument${expected === 1 ? '' : 's'}`);
  }
}

function divideNumbers(left: number, right: number): number {
  if (right === 0) {
    throw new EvalError('division by zero');
  }
  return left / right;
}

function isTruthy(value: Value): boolean {
  return value !== false;
}

function normalizeNumber(value: number): number {
  if (!Number.isFinite(value)) {
    throw new EvalError('numeric result is not finite');
  }
  return Object.is(value, -0) ? 0 : value;
}

function renderValue(value: Value): string {
  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }
  if (typeof value === 'number') {
    return String(normalizeNumber(value));
  }

  return JSON.stringify(value);
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const program = new Parser(input).parseProgram();
  if (program.length === 0) {
    throw new EvalError('expected at least one expression');
  }

  let lastValue: Value = false;
  for (const expression of program) {
    lastValue = evaluate(expression);
  }

  return renderValue(lastValue);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  return { result: evalStr(input), output: '' };
}
