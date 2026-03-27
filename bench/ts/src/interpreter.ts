import { EvalError, attachPosition, type SourcePosition } from './evalError.js';

type ExprBase = { position: SourcePosition };

type Expr =
  | (ExprBase & { kind: 'number'; value: number })
  | (ExprBase & { kind: 'boolean'; value: boolean })
  | (ExprBase & { kind: 'string'; value: string })
  | (ExprBase & { kind: 'symbol'; name: string })
  | (ExprBase & { kind: 'list'; items: Expr[] });

type CharValue = { kind: 'char'; value: string };
type SchemeSymbol = { kind: 'symbol-value'; name: string };
type EmptyList = { kind: 'empty-list' };
type PairValue = { kind: 'pair'; car: Value; cdr: Value };
type VoidValue = { kind: 'void' };
type BuiltinProc = {
  kind: 'builtin';
  name: string;
  apply: (args: Value[]) => Value;
};
type UserProc = {
  kind: 'lambda';
  name?: string;
  params: string[];
  body: Expr[];
  env: Env;
};

type Value =
  | number
  | boolean
  | string
  | CharValue
  | SchemeSymbol
  | EmptyList
  | PairValue
  | VoidValue
  | BuiltinProc
  | UserProc;

type BindingSpec = { name: string; init: Expr };

const EMPTY_LIST: EmptyList = { kind: 'empty-list' };
const VOID: VoidValue = { kind: 'void' };
const SCHEME_NUMBER_PATTERN = /^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$/;

class OutputBuffer {
  private readonly parts: string[] = [];

  write(text: string): void {
    this.parts.push(text);
  }

  toString(): string {
    return this.parts.join('');
  }
}

class Env {
  private readonly bindings = new Map<string, Value>();

  constructor(private readonly parent?: Env) {}

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

    throw new EvalError(`unbound symbol: ${name}`);
  }
}

class Parser {
  private index = 0;
  private line = 1;
  private column = 1;

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
      this.error('unexpected end of input');
    }

    const position = this.currentPosition();
    const ch = this.peek();

    if (ch === '\'') {
      this.advance();
      return {
        kind: 'list',
        position,
        items: [{ kind: 'symbol', name: 'quote', position }, this.parseExpr()],
      };
    }

    if (ch === '(') {
      return this.parseList();
    }

    if (ch === ')') {
      this.error('unexpected )', position);
    }

    if (ch === '"') {
      return this.parseString();
    }

    return this.parseAtom();
  }

  private parseList(): Expr {
    const position = this.currentPosition();
    this.advance();

    const items: Expr[] = [];
    this.skipIgnored();

    while (!this.isAtEnd() && this.peek() !== ')') {
      items.push(this.parseExpr());
      this.skipIgnored();
    }

    if (this.isAtEnd()) {
      this.error('unterminated list', position);
    }

    this.advance();
    return { kind: 'list', items, position };
  }

  private parseString(): Expr {
    const position = this.currentPosition();
    this.advance();

    let value = '';
    while (!this.isAtEnd()) {
      const ch = this.advance();

      if (ch === '"') {
        return { kind: 'string', value, position };
      }

      if (ch === '\\') {
        if (this.isAtEnd()) {
          this.error('unterminated string', position);
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

    this.error('unterminated string', position);
  }

  private parseAtom(): Expr {
    const position = this.currentPosition();
    const start = this.index;

    while (!this.isAtEnd() && !isDelimiter(this.peek())) {
      this.advance();
    }

    const token = this.input.slice(start, this.index);
    if (token.length === 0) {
      this.error('expected expression', position);
    }

    if (token === '#t') {
      return { kind: 'boolean', value: true, position };
    }

    if (token === '#f') {
      return { kind: 'boolean', value: false, position };
    }

    if (/^[+-]?\d+$/.test(token) && token !== '+' && token !== '-') {
      return { kind: 'number', value: Number.parseInt(token, 10), position };
    }

    return { kind: 'symbol', name: token, position };
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

    if (ch === '\n') {
      this.line += 1;
      this.column = 1;
    } else {
      this.column += 1;
    }

    return ch;
  }

  private currentPosition(): SourcePosition {
    return { line: this.line, column: this.column };
  }

  private error(message: string, position: SourcePosition = this.currentPosition()): never {
    throw new EvalError(message, position);
  }
}

function createBuiltins(output: OutputBuffer): Map<string, BuiltinProc> {
  return new Map<string, BuiltinProc>([
    builtin('+', (args) => sumNumbers('+', args, 0)),
    builtin('*', (args) => productNumbers('*', args, 1)),
    builtin('-', (args) => subtractNumbers(args)),
    builtin('/', (args) => divideNumbers(args)),
    builtin('<', (args) => compareNumbers('<', args, (left, right) => left < right)),
    builtin('>', (args) => compareNumbers('>', args, (left, right) => left > right)),
    builtin('=', (args) => compareNumbers('=', args, (left, right) => left === right)),
    builtin('<=', (args) => compareNumbers('<=', args, (left, right) => left <= right)),
    builtin('append', (args) => appendValues(args)),
    builtin('boolean?', (args) => unaryPredicate('boolean?', args, (value) => typeof value === 'boolean')),
    builtin('car', (args) => {
      assertExactArity('car', args, 1);
      return expectPair('car', args[0]!).car;
    }),
    builtin('cdr', (args) => {
      assertExactArity('cdr', args, 1);
      return expectPair('cdr', args[0]!).cdr;
    }),
    builtin('char?', (args) => unaryPredicate('char?', args, isCharValue)),
    builtin('cons', (args) => {
      assertExactArity('cons', args, 2);
      return { kind: 'pair', car: args[0]!, cdr: args[1]! };
    }),
    builtin('display', (args) => {
      assertExactArity('display', args, 1);
      output.write(formatDisplayValue(args[0]!));
      return VOID;
    }),
    builtin('length', (args) => {
      assertExactArity('length', args, 1);
      return expectProperList('length', args[0]!).length;
    }),
    builtin('list', (args) => makeList(args)),
    builtin('newline', (args) => {
      assertExactArity('newline', args, 0);
      output.write('\n');
      return VOID;
    }),
    builtin('not', (args) => {
      assertExactArity('not', args, 1);
      return !isTruthy(args[0]!);
    }),
    builtin('null?', (args) => unaryPredicate('null?', args, isEmptyList)),
    builtin('number->string', (args) => {
      assertExactArity('number->string', args, 1);
      return formatNumber(expectNumberValue('number->string', args[0]!));
    }),
    builtin('number?', (args) => unaryPredicate('number?', args, (value) => typeof value === 'number')),
    builtin('pair?', (args) => unaryPredicate('pair?', args, isPair)),
    builtin('string->number', (args) => {
      assertExactArity('string->number', args, 1);
      return parseStringNumber(expectStringValue('string->number', args[0]!));
    }),
    builtin('string->symbol', (args) => {
      assertExactArity('string->symbol', args, 1);
      return { kind: 'symbol-value', name: expectStringValue('string->symbol', args[0]!) };
    }),
    builtin('string-append', (args) =>
      args.map((arg) => expectStringValue('string-append', arg)).join(''),
    ),
    builtin('string-length', (args) => {
      assertExactArity('string-length', args, 1);
      return stringChars(expectStringValue('string-length', args[0]!)).length;
    }),
    builtin('string-ref', (args) => {
      assertExactArity('string-ref', args, 2);
      const chars = stringChars(expectStringValue('string-ref', args[0]!));
      const index = expectIndex('string-ref', args[1]!);
      if (index >= chars.length) {
        throw new EvalError('string-ref index out of bounds');
      }
      return { kind: 'char', value: chars[index]! };
    }),
    builtin('string?', (args) => unaryPredicate('string?', args, (value) => typeof value === 'string')),
    builtin('substring', (args) => {
      assertExactArity('substring', args, 3);
      const chars = stringChars(expectStringValue('substring', args[0]!));
      const start = expectIndex('substring', args[1]!);
      const end = expectIndex('substring', args[2]!);
      if (start > end || end > chars.length) {
        throw new EvalError('substring expects valid start/end indices');
      }
      return chars.slice(start, end).join('');
    }),
    builtin('symbol->string', (args) => {
      assertExactArity('symbol->string', args, 1);
      return expectSymbolValue('symbol->string', args[0]!).name;
    }),
    builtin('symbol?', (args) => unaryPredicate('symbol?', args, isSymbolValue)),
    builtin('write', (args) => {
      assertExactArity('write', args, 1);
      output.write(formatValue(args[0]!));
      return VOID;
    }),
  ]);
}

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
    throw new EvalError('empty input', { line: 1, column: 1 });
  }

  const output = new OutputBuffer();
  const env = createGlobalEnv(output);
  const lastValue = evalSequence(program, env);

  return {
    result: formatValue(lastValue),
    output: output.toString(),
  };
}

function createGlobalEnv(output: OutputBuffer): Env {
  const env = new Env();
  for (const [name, value] of createBuiltins(output)) {
    env.define(name, value);
  }
  return env;
}

function evalSequence(exprs: Expr[], env: Env): Value {
  let result: Value = VOID;

  for (const expr of exprs) {
    result = evalExpr(expr, env);
  }

  return result;
}

function evalExpr(expr: Expr, env: Env): Value {
  try {
    switch (expr.kind) {
      case 'number':
      case 'boolean':
      case 'string':
        return expr.value;

      case 'symbol':
        return env.lookup(expr.name);

      case 'list':
        return evalList(expr.items, env);
    }
  } catch (error) {
    throw attachPosition(error, expr.position);
  }
}

function evalList(items: Expr[], env: Env): Value {
  if (items.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const first = items[0]!;
  if (first.kind === 'symbol') {
    switch (first.name) {
      case 'and':
        return evalAnd(items.slice(1), env);
      case 'begin':
        return evalSequence(items.slice(1), env);
      case 'cond':
        return evalCond(items.slice(1), env);
      case 'define':
        return evalDefine(items.slice(1), env);
      case 'if':
        return evalIf(items.slice(1), env);
      case 'lambda':
        return evalLambda(items.slice(1), env);
      case 'let':
        return evalLet(items.slice(1), env);
      case 'or':
        return evalOr(items.slice(1), env);
      case 'quote':
        return evalQuote(items.slice(1));
    }
  }

  const proc = evalExpr(first, env);
  if (!isProcedure(proc)) {
    throw new EvalError('attempted to call a non-procedure');
  }

  const args = items.slice(1).map((item) => evalExpr(item, env));
  return applyProcedure(proc, args, first.position);
}

function evalAnd(args: Expr[], env: Env): Value {
  let result: Value = true;

  for (const arg of args) {
    result = evalExpr(arg, env);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evalCond(clauses: Expr[], env: Env): Value {
  for (let index = 0; index < clauses.length; index += 1) {
    const clauseExpr = clauses[index]!;
    if (clauseExpr.kind !== 'list' || clauseExpr.items.length === 0) {
      throw new EvalError('cond expects non-empty clauses');
    }

    const [testExpr, ...body] = clauseExpr.items;
    if (testExpr!.kind === 'symbol' && testExpr.name === 'else') {
      if (index !== clauses.length - 1) {
        throw new EvalError('cond else clause must be last');
      }
      return evalSequence(body, env);
    }

    const testValue = evalExpr(testExpr!, env);
    if (isTruthy(testValue)) {
      return body.length === 0 ? testValue : evalSequence(body, env);
    }
  }

  return VOID;
}

function evalOr(args: Expr[], env: Env): Value {
  for (const arg of args) {
    const value = evalExpr(arg, env);
    if (isTruthy(value)) {
      return value;
    }
  }

  return false;
}

function evalDefine(args: Expr[], env: Env): Value {
  assertAtLeastArity('define', args, 2);

  const target = args[0]!;
  const body = args.slice(1);

  if (target.kind === 'symbol') {
    assertExactArity('define', body, 1);
    env.define(target.name, evalExpr(body[0]!, env));
    return VOID;
  }

  if (target.kind === 'list' && target.items.length > 0) {
    const nameExpr = target.items[0]!;
    if (nameExpr.kind !== 'symbol') {
      throw new EvalError('define expects a symbol name');
    }

    assertAtLeastArity('define', body, 1);
    env.define(nameExpr.name, {
      kind: 'lambda',
      name: nameExpr.name,
      params: parseParams(target.items.slice(1)),
      body,
      env,
    });
    return VOID;
  }

  throw new EvalError('define expects a symbol name');
}

function evalIf(args: Expr[], env: Env): Value {
  assertExactArity('if', args, 3);
  const [conditionExpr, thenExpr, elseExpr] = args;
  return isTruthy(evalExpr(conditionExpr!, env))
    ? evalExpr(thenExpr!, env)
    : evalExpr(elseExpr!, env);
}

function evalLambda(args: Expr[], env: Env): Value {
  assertAtLeastArity('lambda', args, 2);

  const paramsExpr = args[0]!;
  if (paramsExpr.kind !== 'list') {
    throw new EvalError('lambda expects a parameter list');
  }

  return {
    kind: 'lambda',
    params: parseParams(paramsExpr.items),
    body: args.slice(1),
    env,
  };
}

function evalLet(args: Expr[], env: Env): Value {
  assertAtLeastArity('let', args, 2);

  const firstArg = args[0]!;
  if (firstArg.kind === 'symbol') {
    assertAtLeastArity('let', args, 3);
    return evalNamedLet(firstArg.name, args[1]!, args.slice(2), env);
  }

  const bindings = parseBindings(firstArg);
  const body = args.slice(1);
  const values = bindings.map((binding) => evalExpr(binding.init, env));
  const letEnv = new Env(env);

  bindings.forEach((binding, index) => {
    letEnv.define(binding.name, values[index]!);
  });

  return evalSequence(body, letEnv);
}

function evalNamedLet(name: string, bindingsExpr: Expr, body: Expr[], env: Env): Value {
  const bindings = parseBindings(bindingsExpr);
  const values = bindings.map((binding) => evalExpr(binding.init, env));
  const letEnv = new Env(env);
  const proc: UserProc = {
    kind: 'lambda',
    name,
    params: bindings.map((binding) => binding.name),
    body,
    env: letEnv,
  };

  letEnv.define(name, proc);
  return applyProcedure(proc, values, bindingsExpr.position);
}

function evalQuote(args: Expr[]): Value {
  assertExactArity('quote', args, 1);
  return quoteExpr(args[0]!);
}

function parseParams(items: Expr[]): string[] {
  return items.map((item) => {
    if (item.kind !== 'symbol') {
      throw new EvalError('lambda parameters must be symbols');
    }
    return item.name;
  });
}

function parseBindings(expr: Expr): BindingSpec[] {
  if (expr.kind !== 'list') {
    throw new EvalError('let expects a binding list');
  }

  return expr.items.map((bindingExpr) => {
    if (bindingExpr.kind !== 'list' || bindingExpr.items.length !== 2) {
      throw new EvalError('let bindings must be pairs');
    }

    const nameExpr = bindingExpr.items[0]!;
    if (nameExpr.kind !== 'symbol') {
      throw new EvalError('let bindings must start with a symbol');
    }

    return {
      name: nameExpr.name,
      init: bindingExpr.items[1]!,
    };
  });
}

function quoteExpr(expr: Expr): Value {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr.value;

    case 'symbol':
      return { kind: 'symbol-value', name: expr.name };

    case 'list':
      return makeList(expr.items.map((item) => quoteExpr(item)));
  }
}

function applyProcedure(proc: BuiltinProc | UserProc, args: Value[], position: SourcePosition): Value {
  try {
    if (proc.kind === 'builtin') {
      return proc.apply(args);
    }

    assertExactArity(proc.name ?? 'lambda', args, proc.params.length);

    const callEnv = new Env(proc.env);
    proc.params.forEach((param, index) => {
      callEnv.define(param, args[index]!);
    });

    return evalSequence(proc.body, callEnv);
  } catch (error) {
    throw attachPosition(error, position);
  }
}

function builtin(name: string, apply: (args: Value[]) => Value): [string, BuiltinProc] {
  return [name, { kind: 'builtin', name, apply }];
}

function unaryPredicate(name: string, args: Value[], predicate: (value: Value) => boolean): boolean {
  assertExactArity(name, args, 1);
  return predicate(args[0]!);
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

function appendValues(args: Value[]): Value {
  if (args.length === 0) {
    return EMPTY_LIST;
  }

  let result = args[args.length - 1]!;
  for (let index = args.length - 2; index >= 0; index -= 1) {
    result = appendList(args[index]!, result);
  }

  return result;
}

function appendList(list: Value, tail: Value): Value {
  if (isEmptyList(list)) {
    return tail;
  }

  if (!isPair(list)) {
    throw new EvalError('append expects list arguments');
  }

  return {
    kind: 'pair',
    car: list.car,
    cdr: appendList(list.cdr, tail),
  };
}

function makeList(items: Value[]): Value {
  let result: Value = EMPTY_LIST;

  for (let index = items.length - 1; index >= 0; index -= 1) {
    result = {
      kind: 'pair',
      car: items[index]!,
      cdr: result,
    };
  }

  return result;
}

function expectPair(name: string, value: Value): PairValue {
  if (!isPair(value)) {
    throw new EvalError(`${name} expects a pair`);
  }

  return value;
}

function expectProperList(name: string, value: Value): Value[] {
  const items: Value[] = [];
  let current = value;

  while (isPair(current)) {
    items.push(current.car);
    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${name} expects a proper list`);
  }

  return items;
}

function expectNumbers(name: string, args: Value[]): number[] {
  return args.map((arg) => {
    if (typeof arg !== 'number') {
      throw new EvalError(`${name} expects number arguments`);
    }
    return arg;
  });
}

function expectNumberValue(name: string, value: Value): number {
  if (typeof value !== 'number') {
    throw new EvalError(`${name} expects a number`);
  }

  return value;
}

function expectStringValue(name: string, value: Value): string {
  if (typeof value !== 'string') {
    throw new EvalError(`${name} expects a string`);
  }

  return value;
}

function expectSymbolValue(name: string, value: Value): SchemeSymbol {
  if (!isSymbolValue(value)) {
    throw new EvalError(`${name} expects a symbol`);
  }

  return value;
}

function expectIndex(name: string, value: Value): number {
  const index = expectNumberValue(name, value);
  if (!Number.isInteger(index) || index < 0) {
    throw new EvalError(`${name} expects a non-negative integer index`);
  }

  return index;
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
  return formatValueWithMode(value, 'write');
}

function formatDisplayValue(value: Value): string {
  return formatValueWithMode(value, 'display');
}

function formatValueWithMode(value: Value, mode: 'display' | 'write'): string {
  if (typeof value === 'number') {
    return formatNumber(value);
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  if (typeof value === 'string') {
    return mode === 'display' ? value : JSON.stringify(value);
  }

  switch (value.kind) {
    case 'char':
      return mode === 'display' ? value.value : formatCharLiteral(value.value);
    case 'symbol-value':
      return value.name;
    case 'empty-list':
      return '()';
    case 'pair':
      return `(${formatPairContents(value, mode)})`;
    case 'void':
      return '';
    case 'builtin':
      return `#<procedure:${value.name}>`;
    case 'lambda':
      return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
  }
}

function formatPairContents(pair: PairValue, mode: 'display' | 'write'): string {
  const parts: string[] = [];
  let current: Value = pair;

  while (isPair(current)) {
    parts.push(formatValueWithMode(current.car, mode));
    current = current.cdr;
  }

  if (isEmptyList(current)) {
    return parts.join(' ');
  }

  return `${parts.join(' ')} . ${formatValueWithMode(current, mode)}`;
}

function formatCharLiteral(value: string): string {
  switch (value) {
    case ' ':
      return '#\\space';
    case '\n':
      return '#\\newline';
    default:
      return `#\\${value}`;
  }
}

function formatNumber(value: number): string {
  const normalized = normalizeNumber(value);
  return Number.isInteger(normalized) ? normalized.toString() : String(normalized);
}

function parseStringNumber(value: string): number | boolean {
  const trimmed = value.trim();
  if (trimmed.length === 0 || !SCHEME_NUMBER_PATTERN.test(trimmed)) {
    return false;
  }

  return normalizeNumber(Number(trimmed));
}

function stringChars(value: string): string[] {
  return Array.from(value);
}

function normalizeNumber(value: number): number {
  return Object.is(value, -0) ? 0 : value;
}

function isTruthy(value: Value): boolean {
  return value !== false;
}

function isProcedure(value: Value): value is BuiltinProc | UserProc {
  return typeof value === 'object' && value !== null && (value.kind === 'builtin' || value.kind === 'lambda');
}

function isPair(value: Value): value is PairValue {
  return typeof value === 'object' && value !== null && value.kind === 'pair';
}

function isCharValue(value: Value): value is CharValue {
  return typeof value === 'object' && value !== null && value.kind === 'char';
}

function isEmptyList(value: Value): value is EmptyList {
  return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}

function isSymbolValue(value: Value): value is SchemeSymbol {
  return typeof value === 'object' && value !== null && value.kind === 'symbol-value';
}

function isWhitespace(ch: string): boolean {
  return /\s/.test(ch);
}

function isDelimiter(ch: string): boolean {
  return isWhitespace(ch) || ch === '(' || ch === ')' || ch === ';' || ch === '\'';
}
