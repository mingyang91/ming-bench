import { EvalError } from './evalError.js';

type Expr =
  | { kind: 'number'; value: number }
  | { kind: 'boolean'; value: boolean }
  | { kind: 'string'; value: string }
  | { kind: 'symbol'; name: string }
  | { kind: 'list'; elements: Expr[] };

type SymbolValue = { kind: 'symbol'; name: string };
type ListValue = { kind: 'list'; elements: Value[] };
type BuiltinValue = { kind: 'builtin'; name: string; apply: (arguments_: Value[]) => Value };
type ClosureValue = { kind: 'closure'; params: string[]; body: Expr[]; env: Environment };
type VoidValue = { kind: 'void' };

type Value =
  | boolean
  | number
  | string
  | SymbolValue
  | ListValue
  | BuiltinValue
  | ClosureValue
  | VoidValue;

const VOID_VALUE: VoidValue = { kind: 'void' };

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
    if (char === "'") {
      return this.parseQuoteShorthand();
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

  private parseQuoteShorthand(): Expr {
    this.advance(); // '
    return {
      kind: 'list',
      elements: [{ kind: 'symbol', name: 'quote' }, this.parseExpr()],
    };
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
    return /\s/u.test(char) || char === '(' || char === ')' || char === ';' || char === "'";
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

class Environment {
  private readonly bindings = new Map<string, Value>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: Value): void {
    this.bindings.set(name, value);
  }

  lookup(name: string): Value {
    if (this.bindings.has(name)) {
      return this.bindings.get(name)!;
    }
    if (this.parent !== undefined) {
      return this.parent.lookup(name);
    }

    throw new EvalError(`unbound variable: ${name}`);
  }
}

function evaluate(expr: Expr, env: Environment): Value {
  switch (expr.kind) {
    case 'number':
      return expr.value;
    case 'boolean':
      return expr.value;
    case 'string':
      return expr.value;
    case 'symbol':
      return env.lookup(expr.name);
    case 'list':
      return evaluateList(expr.elements, env);
  }
}

function evaluateList(elements: Expr[], env: Environment): Value {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate an empty list');
  }

  const [operator, ...arguments_] = elements;
  if (operator.kind === 'symbol') {
    switch (operator.name) {
      case 'quote':
        return evaluateQuote(arguments_);
      case 'if':
        return evaluateIf(arguments_, env);
      case 'define':
        return evaluateDefine(arguments_, env);
      case 'lambda':
        return evaluateLambda(arguments_, env);
      case 'and':
        return evaluateAnd(arguments_, env);
      case 'or':
        return evaluateOr(arguments_, env);
      default:
        break;
    }
  }

  const procedure = evaluate(operator, env);
  const evaluatedArguments = arguments_.map((argument) => evaluate(argument, env));
  return applyProcedure(procedure, evaluatedArguments);
}

function evaluateQuote(arguments_: Expr[]): Value {
  assertExprArity('quote', arguments_, 1);
  return quoteExpr(arguments_[0]!);
}

function quoteExpr(expr: Expr): Value {
  switch (expr.kind) {
    case 'number':
      return expr.value;
    case 'boolean':
      return expr.value;
    case 'string':
      return expr.value;
    case 'symbol':
      return { kind: 'symbol', name: expr.name };
    case 'list':
      return { kind: 'list', elements: expr.elements.map((element) => quoteExpr(element)) };
  }
}

function evaluateIf(arguments_: Expr[], env: Environment): Value {
  assertExprArity('if', arguments_, 3);
  const [condition, thenBranch, elseBranch] = arguments_;
  return isTruthy(evaluate(condition!, env))
    ? evaluate(thenBranch!, env)
    : evaluate(elseBranch!, env);
}

function evaluateDefine(arguments_: Expr[], env: Environment): Value {
  if (arguments_.length < 2) {
    throw new EvalError('define: expected a name and a value');
  }

  const [target, ...rest] = arguments_;
  if (target!.kind === 'symbol') {
    assertExprArity('define', arguments_, 2);
    const value = evaluate(rest[0]!, env);
    env.define(target.name, value);
    return VOID_VALUE;
  }

  if (target!.kind === 'list' && target.elements.length > 0) {
    const [nameExpr, ...parameterExprs] = target.elements;
    if (nameExpr!.kind !== 'symbol') {
      throw new EvalError('define: expected a function name');
    }
    if (rest.length === 0) {
      throw new EvalError('define: expected a function body');
    }

    const closure: ClosureValue = {
      kind: 'closure',
      params: parseParameterNames(parameterExprs),
      body: rest,
      env,
    };
    env.define(nameExpr.name, closure);
    return VOID_VALUE;
  }

  throw new EvalError('define: invalid binding target');
}

function evaluateLambda(arguments_: Expr[], env: Environment): Value {
  if (arguments_.length < 2) {
    throw new EvalError('lambda: expected parameters and a body');
  }

  const [parameterList, ...body] = arguments_;
  return {
    kind: 'closure',
    params: parseParameterList(parameterList!),
    body,
    env,
  };
}

function parseParameterList(expr: Expr): string[] {
  if (expr.kind !== 'list') {
    throw new EvalError('lambda: expected a parameter list');
  }

  return parseParameterNames(expr.elements);
}

function parseParameterNames(parameters: Expr[]): string[] {
  return parameters.map((parameter) => {
    if (parameter.kind !== 'symbol') {
      throw new EvalError('lambda: expected parameter names to be symbols');
    }
    return parameter.name;
  });
}

function evaluateAnd(arguments_: Expr[], env: Environment): Value {
  let result: Value = true;

  for (const argument of arguments_) {
    result = evaluate(argument, env);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(arguments_: Expr[], env: Environment): Value {
  for (const argument of arguments_) {
    const value = evaluate(argument, env);
    if (isTruthy(value)) {
      return value;
    }
  }

  return false;
}

function applyProcedure(procedure: Value, arguments_: Value[]): Value {
  if (isBuiltinValue(procedure)) {
    return procedure.apply(arguments_);
  }
  if (isClosureValue(procedure)) {
    if (arguments_.length !== procedure.params.length) {
      throw new EvalError(
        `expected ${procedure.params.length} argument${
          procedure.params.length === 1 ? '' : 's'
        }`,
      );
    }

    const callEnv = new Environment(procedure.env);
    for (let index = 0; index < procedure.params.length; index += 1) {
      callEnv.define(procedure.params[index]!, arguments_[index]!);
    }

    let result: Value = VOID_VALUE;
    for (const expression of procedure.body) {
      result = evaluate(expression, callEnv);
    }
    return result;
  }

  throw new EvalError('attempted to call a non-procedure');
}

function isBuiltinValue(value: Value): value is BuiltinValue {
  return typeof value === 'object' && value !== null && value.kind === 'builtin';
}

function isClosureValue(value: Value): value is ClosureValue {
  return typeof value === 'object' && value !== null && value.kind === 'closure';
}

function createGlobalEnvironment(): Environment {
  const env = new Environment();

  env.define('not', createBuiltin('not', (arguments_) => {
    assertValueArity('not', arguments_, 1);
    return !isTruthy(arguments_[0]!);
  }));
  env.define('+', createBuiltin('+', (arguments_) => {
    const numbers = evaluateNumberArguments('+', arguments_, 0);
    return numbers.reduce((sum, value) => sum + value, 0);
  }));
  env.define('-', createBuiltin('-', (arguments_) => {
    const numbers = evaluateNumberArguments('-', arguments_, 1);
    if (numbers.length === 1) {
      return normalizeNumber(-numbers[0]!);
    }

    const [first, ...rest] = numbers;
    return normalizeNumber(rest.reduce((difference, value) => difference - value, first!));
  }));
  env.define('*', createBuiltin('*', (arguments_) => {
    const numbers = evaluateNumberArguments('*', arguments_, 0);
    return numbers.reduce((product, value) => product * value, 1);
  }));
  env.define('/', createBuiltin('/', (arguments_) => {
    const numbers = evaluateNumberArguments('/', arguments_, 1);
    if (numbers.length === 1) {
      return normalizeNumber(divideNumbers(1, numbers[0]!));
    }

    const [first, ...rest] = numbers;
    return normalizeNumber(rest.reduce((quotient, value) => divideNumbers(quotient, value), first!));
  }));
  env.define('<', createBuiltin('<', (arguments_) => evaluateComparison(arguments_, '<', (left, right) => left < right)));
  env.define('>', createBuiltin('>', (arguments_) => evaluateComparison(arguments_, '>', (left, right) => left > right)));
  env.define('=', createBuiltin('=', (arguments_) => evaluateComparison(arguments_, '=', (left, right) => left === right)));
  env.define('<=', createBuiltin('<=', (arguments_) => evaluateComparison(arguments_, '<=', (left, right) => left <= right)));

  return env;
}

function createBuiltin(name: string, apply: (arguments_: Value[]) => Value): BuiltinValue {
  return { kind: 'builtin', name, apply };
}

function evaluateComparison(
  arguments_: Value[],
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

function evaluateNumberArguments(name: string, arguments_: Value[], minimum: number): number[] {
  if (arguments_.length < minimum) {
    const plural = minimum === 1 ? '' : 's';
    throw new EvalError(`${name}: expected at least ${minimum} argument${plural}`);
  }

  return arguments_.map((argument) => {
    if (typeof argument !== 'number') {
      throw new EvalError(`${name}: expected number`);
    }
    return argument;
  });
}

function assertExprArity(name: string, arguments_: Expr[], expected: number): void {
  if (arguments_.length !== expected) {
    throw new EvalError(`${name}: expected ${expected} argument${expected === 1 ? '' : 's'}`);
  }
}

function assertValueArity(name: string, arguments_: Value[], expected: number): void {
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
  if (typeof value === 'string') {
    return JSON.stringify(value);
  }

  switch (value.kind) {
    case 'symbol':
      return value.name;
    case 'list':
      return `(${value.elements.map((element) => renderValue(element)).join(' ')})`;
    case 'builtin':
    case 'closure':
      return '#<procedure>';
    case 'void':
      return '#<void>';
  }
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

  const env = createGlobalEnvironment();

  let lastValue: Value = VOID_VALUE;
  for (const expression of program) {
    lastValue = evaluate(expression, env);
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
