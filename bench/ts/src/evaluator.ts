import { EvalError, type SourcePosition } from './evalError.js';

type Expr =
  | { kind: 'number'; value: number; pos: SourcePosition }
  | { kind: 'boolean'; value: boolean; pos: SourcePosition }
  | { kind: 'string'; value: string; pos: SourcePosition }
  | { kind: 'symbol'; name: string; pos: SourcePosition }
  | { kind: 'list'; elements: Expr[]; pos: SourcePosition };

type BuiltinValue = {
  kind: 'builtin';
  name: string;
  apply: (args: SchemeValue[], pos: SourcePosition) => SchemeValue;
};

type ClosureValue = {
  kind: 'closure';
  params: string[];
  body: Expr[];
  env: Environment;
  name?: string;
};

type SymbolValue = {
  kind: 'symbol';
  name: string;
};

type PairValue = {
  kind: 'pair';
  car: SchemeValue;
  cdr: SchemeValue;
};

type EmptyListValue = {
  kind: 'empty-list';
};

type VoidValue = {
  kind: 'void';
};

type ProcedureValue = BuiltinValue | ClosureValue;

type SchemeValue =
  | number
  | boolean
  | string
  | SymbolValue
  | PairValue
  | EmptyListValue
  | VoidValue
  | ProcedureValue;

type Token =
  | { kind: 'lparen'; pos: SourcePosition }
  | { kind: 'rparen'; pos: SourcePosition }
  | { kind: 'quote'; pos: SourcePosition }
  | { kind: 'number'; value: number; pos: SourcePosition }
  | { kind: 'boolean'; value: boolean; pos: SourcePosition }
  | { kind: 'string'; value: string; pos: SourcePosition }
  | { kind: 'symbol'; value: string; pos: SourcePosition };

type TokenStream = {
  tokens: Token[];
  eofPosition: SourcePosition;
};

const EMPTY_LIST: EmptyListValue = { kind: 'empty-list' };
const VOID: VoidValue = { kind: 'void' };

const builtins = new Map<string, BuiltinValue>([
  ['+', builtin('+', (args, pos) => sum(args, 0, pos))],
  ['-', builtin('-', (args, pos) => subtract(args, pos))],
  ['*', builtin('*', (args, pos) => product(args, 1, pos))],
  ['/', builtin('/', (args, pos) => divide(args, pos))],
  [
    '<',
    builtin('<', (args, pos) => compareChain('<', args, (left, right) => left < right, pos)),
  ],
  [
    '>',
    builtin('>', (args, pos) => compareChain('>', args, (left, right) => left > right, pos)),
  ],
  [
    '=',
    builtin('=', (args, pos) => compareChain('=', args, (left, right) => left === right, pos)),
  ],
  [
    '<=',
    builtin('<=', (args, pos) => compareChain('<=', args, (left, right) => left <= right, pos)),
  ],
  [
    'not',
    builtin('not', (args, pos) => {
      expectArity('not', args, 1, pos);
      return isFalse(args[0]);
    }),
  ],
  [
    'cons',
    builtin('cons', (args, pos) => {
      expectArity('cons', args, 2, pos);
      return { kind: 'pair', car: args[0], cdr: args[1] };
    }),
  ],
  [
    'car',
    builtin('car', (args, pos) => {
      expectArity('car', args, 1, pos);
      return expectPair(args[0], 'car', pos).car;
    }),
  ],
  [
    'cdr',
    builtin('cdr', (args, pos) => {
      expectArity('cdr', args, 1, pos);
      return expectPair(args[0], 'cdr', pos).cdr;
    }),
  ],
  [
    'null?',
    builtin('null?', (args, pos) => {
      expectArity('null?', args, 1, pos);
      return isEmptyList(args[0]);
    }),
  ],
  ['list', builtin('list', (args) => listToPairs(args))],
  [
    'length',
    builtin('length', (args, pos) => {
      expectArity('length', args, 1, pos);
      return listToArray(args[0], 'length', pos).length;
    }),
  ],
  ['append', builtin('append', (args, pos) => appendLists(args, pos))],
  [
    'string?',
    builtin('string?', (args, pos) => {
      expectArity('string?', args, 1, pos);
      return typeof args[0] === 'string';
    }),
  ],
  [
    'number?',
    builtin('number?', (args, pos) => {
      expectArity('number?', args, 1, pos);
      return typeof args[0] === 'number';
    }),
  ],
  [
    'boolean?',
    builtin('boolean?', (args, pos) => {
      expectArity('boolean?', args, 1, pos);
      return typeof args[0] === 'boolean';
    }),
  ],
  [
    'pair?',
    builtin('pair?', (args, pos) => {
      expectArity('pair?', args, 1, pos);
      return isPair(args[0]);
    }),
  ],
  [
    'symbol?',
    builtin('symbol?', (args, pos) => {
      expectArity('symbol?', args, 1, pos);
      return isSymbolValue(args[0]);
    }),
  ],
]);

export function evalStr(input: string): string {
  const parser = new Parser(tokenize(input));
  const expressions = parser.parseProgram();

  if (expressions.length === 0) {
    throw new EvalError('expected at least one expression', { line: 1, col: 1 });
  }

  const env = createGlobalEnvironment();
  let result: SchemeValue = VOID;

  for (const expression of expressions) {
    result = evaluate(expression, env);
  }

  return formatValue(result);
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  return {
    result: evalStr(input),
    output: '',
  };
}

class Environment {
  private readonly bindings = new Map<string, SchemeValue>();
  private readonly parent?: Environment;

  constructor(parent?: Environment) {
    this.parent = parent;
  }

  define(name: string, value: SchemeValue): void {
    this.bindings.set(name, value);
  }

  lookup(name: string, pos: SourcePosition): SchemeValue {
    if (this.bindings.has(name)) {
      return this.bindings.get(name) as SchemeValue;
    }

    if (this.parent !== undefined) {
      return this.parent.lookup(name, pos);
    }

    throw new EvalError(`unbound symbol: ${name}`, pos);
  }
}

function createGlobalEnvironment(): Environment {
  const env = new Environment();

  for (const [name, value] of builtins) {
    env.define(name, value);
  }

  return env;
}

function evaluate(expression: Expr, env: Environment): SchemeValue {
  try {
    switch (expression.kind) {
      case 'number':
      case 'boolean':
      case 'string':
        return expression.value;
      case 'symbol':
        return env.lookup(expression.name, expression.pos);
      case 'list':
        return evaluateList(expression.elements, env, expression.pos);
    }
  } catch (error) {
    throw errorWithPosition(error, expression.pos);
  }
}

function evaluateList(elements: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list', pos);
  }

  const [operatorExpr, ...argumentExprs] = elements;

  if (operatorExpr.kind === 'symbol') {
    switch (operatorExpr.name) {
      case 'and':
        return evaluateAnd(argumentExprs, env);
      case 'or':
        return evaluateOr(argumentExprs, env);
      case 'if':
        return evaluateIf(argumentExprs, env, operatorExpr.pos);
      case 'define':
        return evaluateDefine(argumentExprs, env, operatorExpr.pos);
      case 'quote':
        return evaluateQuote(argumentExprs, operatorExpr.pos);
      case 'lambda':
        return evaluateLambda(argumentExprs, env, operatorExpr.pos);
      case 'begin':
        return evaluateBegin(argumentExprs, env);
      case 'cond':
        return evaluateCond(argumentExprs, env, operatorExpr.pos);
      case 'let':
        return evaluateLet(argumentExprs, env, operatorExpr.pos);
    }
  }

  const operator = evaluate(operatorExpr, env);
  const args = argumentExprs.map((argument) => evaluate(argument, env));
  return applyProcedure(operator, args, operatorExpr.pos);
}

function evaluateAnd(expressions: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = true;

  for (const expression of expressions) {
    result = evaluate(expression, env);
    if (isFalse(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(expressions: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = false;

  for (const expression of expressions) {
    result = evaluate(expression, env);
    if (!isFalse(result)) {
      return result;
    }
  }

  return result;
}

function evaluateIf(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length !== 3) {
    throw new EvalError(`if expected 3 argument(s), got ${expressions.length}`, pos);
  }

  const [conditionExpr, thenExpr, elseExpr] = expressions;
  const condition = evaluate(conditionExpr, env);

  if (isFalse(condition)) {
    return evaluate(elseExpr, env);
  }

  return evaluate(thenExpr, env);
}

function evaluateDefine(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length < 2) {
    throw new EvalError(`define expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  const [targetExpr, ...valueExprs] = expressions;

  if (targetExpr.kind === 'symbol') {
    if (valueExprs.length !== 1) {
      throw new EvalError(`define expected 1 value expression, got ${valueExprs.length}`, pos);
    }

    const value = evaluate(valueExprs[0], env);
    env.define(targetExpr.name, value);
    return VOID;
  }

  if (targetExpr.kind !== 'list' || targetExpr.elements.length === 0) {
    throw new EvalError('define expected a symbol or function signature', targetExpr.pos);
  }

  const [nameExpr, ...paramExprs] = targetExpr.elements;
  const name = expectSymbolExpr(nameExpr, 'define');
  const params = paramExprs.map((expr) => expectSymbolExpr(expr, 'define'));

  if (valueExprs.length === 0) {
    throw new EvalError('define expected at least one function body expression', pos);
  }

  const closure: ClosureValue = {
    kind: 'closure',
    name,
    params,
    body: valueExprs,
    env,
  };

  env.define(name, closure);
  return VOID;
}

function evaluateQuote(expressions: Expr[], pos: SourcePosition): SchemeValue {
  if (expressions.length !== 1) {
    throw new EvalError(`quote expected 1 argument(s), got ${expressions.length}`, pos);
  }

  return quoteExpr(expressions[0]);
}

function evaluateLambda(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length < 2) {
    throw new EvalError(`lambda expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  const [paramsExpr, ...body] = expressions;

  if (paramsExpr.kind !== 'list') {
    throw new EvalError('lambda expected a parameter list', paramsExpr.pos);
  }

  const params = paramsExpr.elements.map((expr) => expectSymbolExpr(expr, 'lambda'));

  return {
    kind: 'closure',
    params,
    body,
    env,
  };
}

function evaluateBegin(expressions: Expr[], env: Environment): SchemeValue {
  return evaluateSequence(expressions, env);
}

function evaluateCond(clauses: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  for (let index = 0; index < clauses.length; index += 1) {
    const clause = clauses[index];

    if (clause.kind !== 'list' || clause.elements.length === 0) {
      throw new EvalError('cond expected a non-empty clause', clause.pos);
    }

    const [testExpr, ...bodyExprs] = clause.elements;
    const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';

    if (isElseClause) {
      if (index !== clauses.length - 1) {
        throw new EvalError('cond else clause must be last', clause.pos);
      }

      return evaluateSequence(bodyExprs, env);
    }

    const testValue = evaluate(testExpr, env);
    if (!isFalse(testValue)) {
      if (bodyExprs.length === 0) {
        return testValue;
      }

      return evaluateSequence(bodyExprs, env);
    }
  }

  return VOID;
}

function evaluateLet(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length < 2) {
    throw new EvalError(`let expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  if (expressions[0].kind === 'symbol') {
    const [nameExpr, bindingsExpr, ...bodyExprs] = expressions;

    if (bindingsExpr === undefined || bodyExprs.length === 0) {
      throw new EvalError('let expected bindings and a body', pos);
    }

    const bindings = parseBindings(bindingsExpr, 'let');
    const values = bindings.map((binding) => evaluate(binding.valueExpr, env));
    const letEnv = new Environment(env);
    const closure: ClosureValue = {
      kind: 'closure',
      name: nameExpr.name,
      params: bindings.map((binding) => binding.name),
      body: bodyExprs,
      env: letEnv,
    };

    letEnv.define(nameExpr.name, closure);
    return applyProcedure(closure, values, nameExpr.pos);
  }

  const [bindingsExpr, ...bodyExprs] = expressions;
  const bindings = parseBindings(bindingsExpr, 'let');
  const values = bindings.map((binding) => evaluate(binding.valueExpr, env));
  const letEnv = new Environment(env);

  for (let index = 0; index < bindings.length; index += 1) {
    letEnv.define(bindings[index].name, values[index]);
  }

  return evaluateSequence(bodyExprs, letEnv);
}

function applyProcedure(value: SchemeValue, args: SchemeValue[], pos: SourcePosition): SchemeValue {
  if (isBuiltin(value)) {
    return value.apply(args, pos);
  }

  if (!isClosure(value)) {
    throw new EvalError('attempted to call a non-procedure value', pos);
  }

  if (args.length !== value.params.length) {
    throw new EvalError(
      `${value.name ?? 'lambda'} expected ${value.params.length} argument(s), got ${args.length}`,
      pos,
    );
  }

  const callEnv = new Environment(value.env);

  for (let index = 0; index < value.params.length; index += 1) {
    callEnv.define(value.params[index], args[index]);
  }

  return evaluateSequence(value.body, callEnv);
}

function quoteExpr(expression: Expr): SchemeValue {
  switch (expression.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expression.value;
    case 'symbol':
      return { kind: 'symbol', name: expression.name };
    case 'list':
      return listToPairs(expression.elements.map((element) => quoteExpr(element)));
  }
}

function listToPairs(elements: SchemeValue[]): SchemeValue {
  let result: SchemeValue = EMPTY_LIST;

  for (let index = elements.length - 1; index >= 0; index -= 1) {
    result = {
      kind: 'pair',
      car: elements[index],
      cdr: result,
    };
  }

  return result;
}

function tokenize(input: string): TokenStream {
  const tokens: Token[] = [];
  let index = 0;
  let line = 1;
  let col = 1;

  const currentPosition = (): SourcePosition => ({ line, col });
  const peekChar = (): string | undefined => input[index];
  const advanceChar = (): string => {
    const char = input[index];
    index += 1;

    if (char === '\n') {
      line += 1;
      col = 1;
    } else {
      col += 1;
    }

    return char;
  };

  while (index < input.length) {
    const char = peekChar() as string;

    if (isWhitespace(char)) {
      advanceChar();
      continue;
    }

    if (char === ';') {
      while (index < input.length && peekChar() !== '\n') {
        advanceChar();
      }
      continue;
    }

    const pos = currentPosition();

    if (char === '(') {
      advanceChar();
      tokens.push({ kind: 'lparen', pos });
      continue;
    }

    if (char === ')') {
      advanceChar();
      tokens.push({ kind: 'rparen', pos });
      continue;
    }

    if (char === '\'') {
      advanceChar();
      tokens.push({ kind: 'quote', pos });
      continue;
    }

    if (char === '"') {
      const value = readStringLiteral(peekChar, advanceChar, pos);
      tokens.push({ kind: 'string', value, pos });
      continue;
    }

    let rawToken = '';

    while (index < input.length) {
      const next = peekChar() as string;
      if (isWhitespace(next) || next === '(' || next === ')' || next === ';') {
        break;
      }
      rawToken += advanceChar();
    }

    if (rawToken.length === 0) {
      throw new EvalError('unexpected token', pos);
    }

    if (rawToken === '#t') {
      tokens.push({ kind: 'boolean', value: true, pos });
    } else if (rawToken === '#f') {
      tokens.push({ kind: 'boolean', value: false, pos });
    } else if (/^[+-]?\d+$/.test(rawToken)) {
      tokens.push({ kind: 'number', value: Number(rawToken), pos });
    } else {
      tokens.push({ kind: 'symbol', value: rawToken, pos });
    }
  }

  return { tokens, eofPosition: currentPosition() };
}

function readStringLiteral(
  peekChar: () => string | undefined,
  advanceChar: () => string,
  startPosition: SourcePosition,
): string {
  advanceChar();
  let value = '';

  while (peekChar() !== undefined) {
    const char = advanceChar();

    if (char === '"') {
      return value;
    }

    if (char === '\\') {
      if (peekChar() === undefined) {
        throw new EvalError('unterminated string literal', startPosition);
      }

      const escaped = advanceChar();
      if (escaped === 'n') {
        value += '\n';
      } else if (escaped === 't') {
        value += '\t';
      } else if (escaped === '"' || escaped === '\\') {
        value += escaped;
      } else {
        value += escaped;
      }

      continue;
    }

    value += char;
  }

  throw new EvalError('unterminated string literal', startPosition);
}

class Parser {
  private readonly tokens: Token[];
  private readonly eofPosition: SourcePosition;
  private index = 0;

  constructor(tokenStream: TokenStream) {
    this.tokens = tokenStream.tokens;
    this.eofPosition = tokenStream.eofPosition;
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
      throw new EvalError('unexpected end of input', this.eofPosition);
    }

    switch (token.kind) {
      case 'number':
        return { kind: 'number', value: token.value, pos: token.pos };
      case 'boolean':
        return { kind: 'boolean', value: token.value, pos: token.pos };
      case 'string':
        return { kind: 'string', value: token.value, pos: token.pos };
      case 'symbol':
        return { kind: 'symbol', name: token.value, pos: token.pos };
      case 'quote':
        return {
          kind: 'list',
          pos: token.pos,
          elements: [
            { kind: 'symbol', name: 'quote', pos: token.pos },
            this.parseExpr(),
          ],
        };
      case 'lparen': {
        const elements: Expr[] = [];

        while (true) {
          const next = this.peek();

          if (next === undefined) {
            throw new EvalError('unterminated list', this.eofPosition);
          }

          if (next.kind === 'rparen') {
            this.advance();
            return { kind: 'list', elements, pos: token.pos };
          }

          elements.push(this.parseExpr());
        }
      }
      case 'rparen':
        throw new EvalError('unexpected )', token.pos);
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

function builtin(
  name: string,
  apply: (args: SchemeValue[], pos: SourcePosition) => SchemeValue,
): BuiltinValue {
  return { kind: 'builtin', name, apply };
}

function errorWithPosition(error: unknown, pos: SourcePosition): EvalError {
  if (error instanceof EvalError) {
    return error.position === undefined ? new EvalError(error.message, pos) : error;
  }

  if (error instanceof Error) {
    return new EvalError(error.message, pos);
  }

  return new EvalError(String(error), pos);
}

function isBuiltin(value: SchemeValue): value is BuiltinValue {
  return typeof value === 'object' && value !== null && value.kind === 'builtin';
}

function isClosure(value: SchemeValue): value is ClosureValue {
  return typeof value === 'object' && value !== null && value.kind === 'closure';
}

function isSymbolValue(value: SchemeValue): value is SymbolValue {
  return typeof value === 'object' && value !== null && value.kind === 'symbol';
}

function isPair(value: SchemeValue): value is PairValue {
  return typeof value === 'object' && value !== null && value.kind === 'pair';
}

function isEmptyList(value: SchemeValue): value is EmptyListValue {
  return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}

function expectArity(
  name: string,
  args: SchemeValue[],
  expected: number,
  pos: SourcePosition,
): void {
  if (args.length !== expected) {
    throw new EvalError(`${name} expected ${expected} argument(s), got ${args.length}`, pos);
  }
}

function expectAtLeastArity(
  name: string,
  args: SchemeValue[],
  min: number,
  pos: SourcePosition,
): void {
  if (args.length < min) {
    throw new EvalError(`${name} expected at least ${min} argument(s), got ${args.length}`, pos);
  }
}

function expectNumber(value: SchemeValue, name: string, pos: SourcePosition): number {
  if (typeof value !== 'number') {
    throw new EvalError(`${name} expected a number`, pos);
  }

  return value;
}

function expectPair(value: SchemeValue, name: string, pos: SourcePosition): PairValue {
  if (!isPair(value)) {
    throw new EvalError(`${name} expected a pair`, pos);
  }

  return value;
}

function expectSymbolExpr(expression: Expr, name: string): string {
  if (expression.kind !== 'symbol') {
    throw new EvalError(`${name} expected a symbol`, expression.pos);
  }

  return expression.name;
}

function parseBindings(bindingsExpr: Expr, name: string): Array<{ name: string; valueExpr: Expr }> {
  if (bindingsExpr.kind !== 'list') {
    throw new EvalError(`${name} expected a bindings list`, bindingsExpr.pos);
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (bindingExpr.kind !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError(`${name} expected bindings of the form (name value)`, bindingExpr.pos);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    return {
      name: expectSymbolExpr(nameExpr, name),
      valueExpr,
    };
  });
}

function evaluateSequence(expressions: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = VOID;

  for (const expression of expressions) {
    result = evaluate(expression, env);
  }

  return result;
}

function listToArray(value: SchemeValue, name: string, pos: SourcePosition): SchemeValue[] {
  const elements: SchemeValue[] = [];
  let current = value;

  while (isPair(current)) {
    elements.push(current.car);
    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${name} expected a list`, pos);
  }

  return elements;
}

function appendLists(args: SchemeValue[], pos: SourcePosition): SchemeValue {
  const elements: SchemeValue[] = [];

  for (const arg of args) {
    elements.push(...listToArray(arg, 'append', pos));
  }

  return listToPairs(elements);
}

function sum(args: SchemeValue[], identity: number, pos: SourcePosition): number {
  let total = identity;

  for (const arg of args) {
    total += expectNumber(arg, '+', pos);
  }

  return normalizeNumber(total);
}

function subtract(args: SchemeValue[], pos: SourcePosition): number {
  expectAtLeastArity('-', args, 1, pos);

  if (args.length === 1) {
    return normalizeNumber(-expectNumber(args[0], '-', pos));
  }

  let total = expectNumber(args[0], '-', pos);

  for (const arg of args.slice(1)) {
    total -= expectNumber(arg, '-', pos);
  }

  return normalizeNumber(total);
}

function product(args: SchemeValue[], identity: number, pos: SourcePosition): number {
  let total = identity;

  for (const arg of args) {
    total *= expectNumber(arg, '*', pos);
  }

  return normalizeNumber(total);
}

function divide(args: SchemeValue[], pos: SourcePosition): number {
  expectAtLeastArity('/', args, 2, pos);

  let total = expectNumber(args[0], '/', pos);

  for (const arg of args.slice(1)) {
    const divisor = expectNumber(arg, '/', pos);

    if (divisor === 0) {
      throw new EvalError('division by zero', pos);
    }

    total /= divisor;
  }

  return normalizeNumber(total);
}

function compareChain(
  name: string,
  args: SchemeValue[],
  predicate: (left: number, right: number) => boolean,
  pos: SourcePosition,
): boolean {
  expectAtLeastArity(name, args, 2, pos);

  const numbers = args.map((arg) => expectNumber(arg, name, pos));

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

  if (isClosure(value)) {
    return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
  }

  if (isPair(value)) {
    return formatPair(value);
  }

  if (isEmptyList(value)) {
    return '()';
  }

  if (isSymbolValue(value)) {
    return value.name;
  }

  if (typeof value === 'object' && value !== null && value.kind === 'void') {
    return '#<void>';
  }

  throw new EvalError('cannot format value');
}

function formatPair(pair: PairValue): string {
  const parts: string[] = [];
  let current: SchemeValue = pair;

  while (isPair(current)) {
    parts.push(formatValue(current.car));
    current = current.cdr;
  }

  if (isEmptyList(current)) {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatValue(current)})`;
}
