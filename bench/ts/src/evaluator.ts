import { EvalError } from './evalError.js';

type Expr = NumberExpr | BooleanExpr | StringExpr | SymbolExpr | ListExpr;

interface SourceLoc {
  line: number;
  col: number;
}

interface NumberExpr extends SourceLoc {
  type: 'number';
  value: number;
}

interface BooleanExpr extends SourceLoc {
  type: 'boolean';
  value: boolean;
}

interface StringExpr extends SourceLoc {
  type: 'string';
  value: string;
}

interface SymbolExpr extends SourceLoc {
  type: 'symbol';
  value: string;
}

interface ListExpr extends SourceLoc {
  type: 'list';
  elements: Expr[];
}

interface SchemeString {
  kind: 'string';
  value: string;
}

type Value = number | boolean | SchemeString;

class Reader {
  private index = 0;
  private line = 1;
  private col = 1;

  constructor(private readonly input: string) {}

  parseProgram(): Expr[] {
    const expressions: Expr[] = [];
    this.skipWhitespaceAndComments();

    while (!this.isEof()) {
      expressions.push(this.parseExpr());
      this.skipWhitespaceAndComments();
    }

    return expressions;
  }

  private parseExpr(): Expr {
    this.skipWhitespaceAndComments();

    if (this.isEof()) {
      this.raise('unexpected end of input');
    }

    const loc = this.currentLoc();
    const ch = this.peek();

    if (ch === '(') {
      return this.parseList(loc);
    }

    if (ch === ')') {
      this.raise('unexpected )', loc);
    }

    if (ch === '"') {
      return this.parseString(loc);
    }

    return this.parseAtom(loc);
  }

  private parseList(loc: SourceLoc): ListExpr {
    this.advance();

    const elements: Expr[] = [];
    this.skipWhitespaceAndComments();

    while (!this.isEof() && this.peek() !== ')') {
      elements.push(this.parseExpr());
      this.skipWhitespaceAndComments();
    }

    if (this.isEof()) {
      this.raise('unterminated list', loc);
    }

    this.advance();
    return { type: 'list', elements, ...loc };
  }

  private parseString(loc: SourceLoc): StringExpr {
    this.advance();

    let value = '';
    let terminated = false;

    while (!this.isEof()) {
      const ch = this.advance();

      if (ch === '"') {
        terminated = true;
        break;
      }

      if (ch === '\\') {
        if (this.isEof()) {
          this.raise('unterminated string escape', loc);
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
            this.raise(`unsupported escape \\${escaped}`, loc);
        }

        continue;
      }

      value += ch;
    }

    if (!terminated) {
      this.raise('unterminated string literal', loc);
    }

    return { type: 'string', value, ...loc };
  }

  private parseAtom(loc: SourceLoc): Expr {
    let token = '';

    while (!this.isEof()) {
      const ch = this.peek();
      if (isWhitespace(ch) || ch === '(' || ch === ')' || ch === ';') {
        break;
      }

      token += this.advance();
    }

    if (token.length === 0) {
      this.raise('expected expression', loc);
    }

    if (token === '#t') {
      return { type: 'boolean', value: true, ...loc };
    }

    if (token === '#f') {
      return { type: 'boolean', value: false, ...loc };
    }

    if (/^-?\d+$/.test(token)) {
      return { type: 'number', value: Number(token), ...loc };
    }

    return { type: 'symbol', value: token, ...loc };
  }

  private skipWhitespaceAndComments(): void {
    while (!this.isEof()) {
      const ch = this.peek();

      if (isWhitespace(ch)) {
        this.advance();
        continue;
      }

      if (ch === ';') {
        while (!this.isEof() && this.peek() !== '\n') {
          this.advance();
        }
        continue;
      }

      break;
    }
  }

  private currentLoc(): SourceLoc {
    return { line: this.line, col: this.col };
  }

  private isEof(): boolean {
    return this.index >= this.input.length;
  }

  private peek(): string {
    return this.input[this.index] ?? '';
  }

  private advance(): string {
    const ch = this.input[this.index] ?? '';
    this.index += 1;

    if (ch === '\n') {
      this.line += 1;
      this.col = 1;
    } else {
      this.col += 1;
    }

    return ch;
  }

  private raise(message: string, loc: SourceLoc = this.currentLoc()): never {
    throw new EvalError(`${loc.line}:${loc.col}: ${message}`);
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const program = new Reader(input).parseProgram();

  if (program.length === 0) {
    throw new EvalError('1:1: expected expression');
  }

  let result: Value = false;
  for (const expr of program) {
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

function evaluate(expr: Expr): Value {
  switch (expr.type) {
    case 'number':
    case 'boolean':
      return expr.value;
    case 'string':
      return { kind: 'string', value: expr.value };
    case 'symbol':
      throw new EvalError(`${expr.line}:${expr.col}: unbound variable ${expr.value}`);
    case 'list':
      return evaluateList(expr);
  }
}

function evaluateList(expr: ListExpr): Value {
  if (expr.elements.length === 0) {
    throw new EvalError(`${expr.line}:${expr.col}: cannot evaluate empty list`);
  }

  const [head, ...args] = expr.elements;
  if (head.type !== 'symbol') {
    throw new EvalError(`${head.line}:${head.col}: operator must be a symbol`);
  }

  switch (head.value) {
    case '+':
      return evalNumericFold(args, head, 0, (left, right) => left + right);
    case '*':
      return evalNumericFold(args, head, 1, (left, right) => left * right);
    case '-':
      return evalSubtraction(args, head);
    case '/':
      return evalDivision(args, head);
    case '<':
      return evalComparison(args, head, (left, right) => left < right);
    case '>':
      return evalComparison(args, head, (left, right) => left > right);
    case '=':
      return evalComparison(args, head, (left, right) => left === right);
    case '<=':
      return evalComparison(args, head, (left, right) => left <= right);
    case 'not':
      return evalNot(args, head);
    case 'and':
      return evalAnd(args);
    case 'or':
      return evalOr(args);
    default:
      throw new EvalError(`${head.line}:${head.col}: unknown procedure ${head.value}`);
  }
}

function evalNumericFold(
  args: Expr[],
  head: SymbolExpr,
  initial: number,
  op: (left: number, right: number) => number,
): number {
  let result = initial;
  for (const arg of args) {
    result = op(result, expectNumber(evaluate(arg), arg));
  }
  return result;
}

function evalSubtraction(args: Expr[], head: SymbolExpr): number {
  if (args.length === 0) {
    throw new EvalError(`${head.line}:${head.col}: - expects at least 1 argument`);
  }

  const first = expectNumber(evaluate(args[0]), args[0]);
  if (args.length === 1) {
    return -first;
  }

  let result = first;
  for (const arg of args.slice(1)) {
    result -= expectNumber(evaluate(arg), arg);
  }
  return result;
}

function evalDivision(args: Expr[], head: SymbolExpr): number {
  if (args.length < 2) {
    throw new EvalError(`${head.line}:${head.col}: / expects at least 2 arguments`);
  }

  let result = expectNumber(evaluate(args[0]), args[0]);
  for (const arg of args.slice(1)) {
    const value = expectNumber(evaluate(arg), arg);
    if (value === 0) {
      throw new EvalError(`${arg.line}:${arg.col}: division by zero`);
    }
    result /= value;
  }
  return result;
}

function evalComparison(
  args: Expr[],
  head: SymbolExpr,
  predicate: (left: number, right: number) => boolean,
): boolean {
  if (args.length < 2) {
    throw new EvalError(`${head.line}:${head.col}: ${head.value} expects at least 2 arguments`);
  }

  const values = args.map((arg) => expectNumber(evaluate(arg), arg));
  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }
  return true;
}

function evalNot(args: Expr[], head: SymbolExpr): boolean {
  if (args.length !== 1) {
    throw new EvalError(`${head.line}:${head.col}: not expects exactly 1 argument`);
  }

  return !isTruthy(evaluate(args[0]));
}

function evalAnd(args: Expr[]): Value {
  let result: Value = true;

  for (const arg of args) {
    result = evaluate(arg);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evalOr(args: Expr[]): Value {
  let result: Value = false;

  for (const arg of args) {
    result = evaluate(arg);
    if (isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function expectNumber(value: Value, expr: Expr): number {
  if (typeof value !== 'number') {
    throw new EvalError(`${expr.line}:${expr.col}: expected number`);
  }
  return value;
}

function isTruthy(value: Value): boolean {
  return value !== false;
}

function formatValue(value: Value): string {
  if (typeof value === 'number') {
    return Object.is(value, -0) ? '0' : String(value);
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  return `"${escapeString(value.value)}"`;
}

function escapeString(value: string): string {
  return value
    .replaceAll('\\', '\\\\')
    .replaceAll('"', '\\"')
    .replaceAll('\n', '\\n')
    .replaceAll('\t', '\\t');
}

function isWhitespace(ch: string): boolean {
  return ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r';
}
