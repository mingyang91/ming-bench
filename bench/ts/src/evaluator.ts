import { EvalError, type SourceLocation } from './evalError.js';

type Located<T> = T & { location: SourceLocation };

type Expr =
  | Located<{ kind: 'number'; value: number }>
  | Located<{ kind: 'boolean'; value: boolean }>
  | Located<{ kind: 'string'; value: string }>
  | Located<{ kind: 'symbol'; value: string }>
  | Located<{ kind: 'list'; items: Expr[] }>;

type Token =
  | Located<{ kind: 'number'; value: number }>
  | Located<{ kind: 'boolean'; value: boolean }>
  | Located<{ kind: 'string'; value: string }>
  | Located<{ kind: 'symbol'; value: string }>
  | Located<{ kind: 'paren'; value: '(' | ')' }>
  | Located<{ kind: 'quote' }>;

interface NumberValue {
  kind: 'number';
  value: number;
}

interface BooleanValue {
  kind: 'boolean';
  value: boolean;
}

interface StringValue {
  kind: 'string';
  value: string;
}

interface SymbolValue {
  kind: 'symbol';
  value: string;
}

interface EmptyListValue {
  kind: 'empty-list';
}

interface PairValue {
  kind: 'pair';
  car: SchemeValue;
  cdr: SchemeValue;
}

interface VoidValue {
  kind: 'void';
}

type SchemeValue =
  | NumberValue
  | BooleanValue
  | StringValue
  | SymbolValue
  | EmptyListValue
  | PairValue
  | VoidValue
  | ProcedureValue;

type ProcedureValue = BuiltinProcedureValue | ClosureProcedureValue;

interface BuiltinProcedureValue {
  kind: 'procedure';
  procedureKind: 'builtin';
  name: string;
  apply(args: SchemeValue[]): SchemeValue;
}

interface ClosureProcedureValue {
  kind: 'procedure';
  procedureKind: 'closure';
  name: string;
  params: string[];
  body: Expr[];
  env: Environment;
}

interface Binding {
  value: SchemeValue;
}

interface ParserState {
  tokens: Token[];
  index: number;
  eofLocation: SourceLocation;
}

interface LetBindingSpec {
  name: string;
  valueExpression: Expr;
}

interface TokenizationResult {
  tokens: Token[];
  eofLocation: SourceLocation;
}

class Environment {
  private readonly bindings = new Map<string, Binding>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: SchemeValue): Binding {
    const binding = { value };
    this.bindings.set(name, binding);
    return binding;
  }

  defineBinding(name: string, binding: Binding): void {
    this.bindings.set(name, binding);
  }

  lookup(name: string): SchemeValue | undefined {
    return this.lookupBinding(name)?.value;
  }

  child(): Environment {
    return new Environment(this);
  }

  private lookupBinding(name: string): Binding | undefined {
    return this.bindings.get(name) ?? this.parent?.lookupBinding(name);
  }
}

const TRUE_VALUE: BooleanValue = { kind: 'boolean', value: true };
const FALSE_VALUE: BooleanValue = { kind: 'boolean', value: false };
const EMPTY_LIST_VALUE: EmptyListValue = { kind: 'empty-list' };
const VOID_VALUE: VoidValue = { kind: 'void' };

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const expressions = parseProgram(input);
  if (expressions.length === 0) {
    throw new EvalError('empty input', { line: 1, column: 1 });
  }

  const env = createGlobalEnvironment();
  const result = evaluateSequence(expressions, env);
  return formatValue(result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  return { result: evalStr(input), output: '' };
}

function createGlobalEnvironment(): Environment {
  const env = new Environment();

  env.define('+', makeBuiltinProcedure('+', (args) => numberValue(sum(asNumbers(args, '+')))));
  env.define('-', makeBuiltinProcedure('-', (args) => numberValue(subtract(asNumbers(args, '-')))));
  env.define('*', makeBuiltinProcedure('*', (args) => numberValue(product(asNumbers(args, '*')))));
  env.define('/', makeBuiltinProcedure('/', (args) => numberValue(divide(asNumbers(args, '/')))));
  env.define(
    '<',
    makeBuiltinProcedure('<', (args) =>
      booleanValue(compareChain(asNumbers(args, '<'), (left, right) => left < right, '<')),
    ),
  );
  env.define(
    '>',
    makeBuiltinProcedure('>', (args) =>
      booleanValue(compareChain(asNumbers(args, '>'), (left, right) => left > right, '>')),
    ),
  );
  env.define(
    '=',
    makeBuiltinProcedure('=', (args) =>
      booleanValue(compareChain(asNumbers(args, '='), (left, right) => left === right, '=')),
    ),
  );
  env.define(
    '<=',
    makeBuiltinProcedure('<=', (args) =>
      booleanValue(compareChain(asNumbers(args, '<='), (left, right) => left <= right, '<=')),
    ),
  );
  env.define('not', makeBuiltinProcedure('not', (args) => applyNot(args)));

  env.define('cons', makeBuiltinProcedure('cons', (args) => applyCons(args)));
  env.define('car', makeBuiltinProcedure('car', (args) => applyCar(args)));
  env.define('cdr', makeBuiltinProcedure('cdr', (args) => applyCdr(args)));
  env.define('null?', makeBuiltinProcedure('null?', (args) => applyNullPredicate(args)));
  env.define('list', makeBuiltinProcedure('list', (args) => arrayToList(args)));
  env.define('length', makeBuiltinProcedure('length', (args) => applyLength(args)));
  env.define('append', makeBuiltinProcedure('append', (args) => applyAppend(args)));

  env.define('string?', makePredicateProcedure('string?', (value) => value.kind === 'string'));
  env.define('number?', makePredicateProcedure('number?', (value) => value.kind === 'number'));
  env.define('boolean?', makePredicateProcedure('boolean?', (value) => value.kind === 'boolean'));
  env.define('pair?', makePredicateProcedure('pair?', (value) => value.kind === 'pair'));
  env.define('symbol?', makePredicateProcedure('symbol?', (value) => value.kind === 'symbol'));

  return env;
}

function parseProgram(input: string): Expr[] {
  const { tokens, eofLocation } = tokenize(input);
  const state: ParserState = { tokens, index: 0, eofLocation };
  const expressions: Expr[] = [];

  while (state.index < state.tokens.length) {
    expressions.push(parseExpression(state));
  }

  return expressions;
}

function tokenize(input: string): TokenizationResult {
  const tokens: Token[] = [];
  let index = 0;
  let line = 1;
  let column = 1;

  while (index < input.length) {
    const char = input[index];
    if (char === undefined) {
      break;
    }

    if (isWhitespace(char)) {
      ({ line, column } = advanceLocation(char, line, column));
      index += 1;
      continue;
    }

    if (char === ';') {
      const next = skipComment(input, index, line, column);
      index = next.nextIndex;
      line = next.nextLine;
      column = next.nextColumn;
      continue;
    }

    const location: SourceLocation = { line, column };

    if (char === '(' || char === ')') {
      tokens.push({ kind: 'paren', value: char, location });
      ({ line, column } = advanceLocation(char, line, column));
      index += 1;
      continue;
    }

    if (char === "'") {
      tokens.push({ kind: 'quote', location });
      ({ line, column } = advanceLocation(char, line, column));
      index += 1;
      continue;
    }

    if (char === '"') {
      const parsed = readStringToken(input, index, line, column);
      tokens.push({ kind: 'string', value: parsed.value, location });
      index = parsed.nextIndex;
      line = parsed.nextLine;
      column = parsed.nextColumn;
      continue;
    }

    const parsed = readAtomToken(input, index, line, column);
    tokens.push(parsed.token);
    index = parsed.nextIndex;
    line = parsed.nextLine;
    column = parsed.nextColumn;
  }

  return { tokens, eofLocation: { line, column } };
}

function parseExpression(state: ParserState): Expr {
  const token = state.tokens[state.index];
  if (token === undefined) {
    throw new EvalError('unexpected end of input', state.eofLocation);
  }

  state.index += 1;

  switch (token.kind) {
    case 'number':
      return { kind: 'number', value: token.value, location: token.location };
    case 'boolean':
      return { kind: 'boolean', value: token.value, location: token.location };
    case 'string':
      return { kind: 'string', value: token.value, location: token.location };
    case 'symbol':
      return { kind: 'symbol', value: token.value, location: token.location };
    case 'quote':
      return {
        kind: 'list',
        items: [
          { kind: 'symbol', value: 'quote', location: token.location },
          parseExpression(state),
        ],
        location: token.location,
      };
    case 'paren':
      if (token.value === ')') {
        throw new EvalError('unexpected )', token.location);
      }

      return parseList(state, token.location);
  }
}

function parseList(state: ParserState, location: SourceLocation): Expr {
  const items: Expr[] = [];

  while (true) {
    const token = state.tokens[state.index];
    if (token === undefined) {
      throw new EvalError('unterminated list', location);
    }

    if (token.kind === 'paren' && token.value === ')') {
      state.index += 1;
      return { kind: 'list', items, location };
    }

    items.push(parseExpression(state));
  }
}

function evaluate(expression: Expr, env: Environment): SchemeValue {
  try {
    switch (expression.kind) {
      case 'number':
        return numberValue(expression.value);
      case 'boolean':
        return booleanValue(expression.value);
      case 'string':
        return stringValue(expression.value);
      case 'symbol': {
        const value = env.lookup(expression.value);
        if (value === undefined) {
          throw new EvalError(`unbound variable: ${expression.value}`);
        }
        return value;
      }
      case 'list':
        return evaluateList(expression.items, env);
    }
  } catch (error) {
    rethrowWithLocation(error, expression.location);
  }
}

function evaluateList(items: Expr[], env: Environment): SchemeValue {
  if (items.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const head = items[0];
  if (head.kind === 'symbol') {
    switch (head.value) {
      case 'and':
        return evaluateAnd(items.slice(1), env);
      case 'or':
        return evaluateOr(items.slice(1), env);
      case 'begin':
        return evaluateBegin(items.slice(1), env);
      case 'cond':
        return evaluateCond(items.slice(1), env);
      case 'define':
        return evaluateDefine(items.slice(1), env);
      case 'if':
        return evaluateIf(items.slice(1), env);
      case 'lambda':
        return evaluateLambda(items.slice(1), env);
      case 'let':
        return evaluateLet(items.slice(1), env);
      case 'quote':
        return evaluateQuote(items.slice(1));
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

function evaluateBegin(items: Expr[], env: Environment): SchemeValue {
  return evaluateSequence(items, env);
}

function evaluateCond(items: Expr[], env: Environment): SchemeValue {
  for (let index = 0; index < items.length; index += 1) {
    const clause = items[index];
    if (clause.kind !== 'list' || clause.items.length === 0) {
      throw new EvalError('cond clauses must be non-empty lists');
    }

    const [testExpression, ...body] = clause.items;
    if (testExpression.kind === 'symbol' && testExpression.value === 'else') {
      if (index !== items.length - 1) {
        throw new EvalError('cond else clause must be last');
      }
      return evaluateCondBody(body, TRUE_VALUE, env);
    }

    const testValue = evaluate(testExpression, env);
    if (isTruthy(testValue)) {
      return evaluateCondBody(body, testValue, env);
    }
  }

  return VOID_VALUE;
}

function evaluateCondBody(body: Expr[], testValue: SchemeValue, env: Environment): SchemeValue {
  if (body.length === 0) {
    return testValue;
  }

  return evaluateSequence(body, env);
}

function evaluateDefine(items: Expr[], env: Environment): SchemeValue {
  if (items.length < 2) {
    throw new EvalError('define requires a name and value');
  }

  const target = items[0];
  if (target.kind === 'symbol') {
    if (items.length !== 2) {
      throw new EvalError('define expected exactly 2 argument(s)');
    }

    return defineVariable(target.value, items[1], env);
  }

  if (target.kind !== 'list' || target.items.length === 0) {
    throw new EvalError('define requires a symbol or parameter list');
  }

  const name = target.items[0];
  if (name.kind !== 'symbol') {
    throw new EvalError('function name must be a symbol');
  }

  const params = readParameterListItems(target.items.slice(1));
  const body = items.slice(1);
  if (body.length === 0) {
    throw new EvalError('function definition requires a body');
  }

  const binding: Binding = { value: VOID_VALUE };
  env.defineBinding(name.value, binding);
  binding.value = makeClosure(name.value, params, body, env);
  return VOID_VALUE;
}

function defineVariable(name: string, valueExpression: Expr, env: Environment): SchemeValue {
  if (isLambdaExpression(valueExpression)) {
    const binding: Binding = { value: VOID_VALUE };
    env.defineBinding(name, binding);
    binding.value = evaluateNamedLambda(valueExpression, env, name);
    return VOID_VALUE;
  }

  env.define(name, evaluate(valueExpression, env));
  return VOID_VALUE;
}

function isLambdaExpression(expression: Expr): expression is Extract<Expr, { kind: 'list' }> {
  return (
    expression.kind === 'list' &&
    expression.items.length > 0 &&
    expression.items[0].kind === 'symbol' &&
    expression.items[0].value === 'lambda'
  );
}

function evaluateNamedLambda(
  expression: Extract<Expr, { kind: 'list' }>,
  env: Environment,
  name: string,
): ClosureProcedureValue {
  const { params, body } = parseLambdaParts(expression.items.slice(1));
  return makeClosure(name, params, body, env);
}

function evaluateIf(items: Expr[], env: Environment): SchemeValue {
  if (items.length < 2 || items.length > 3) {
    throw new EvalError('if expected 2 or 3 argument(s)');
  }

  const condition = evaluate(items[0], env);
  if (isTruthy(condition)) {
    return evaluate(items[1], env);
  }

  if (items[2] === undefined) {
    return VOID_VALUE;
  }

  return evaluate(items[2], env);
}

function evaluateLambda(items: Expr[], env: Environment): SchemeValue {
  const { params, body } = parseLambdaParts(items);
  return makeClosure('lambda', params, body, env);
}

function evaluateLet(items: Expr[], env: Environment): SchemeValue {
  if (items.length < 2) {
    throw new EvalError('let requires bindings and a body');
  }

  if (items[0].kind === 'symbol') {
    return evaluateNamedLet(items, env);
  }

  const bindings = parseLetBindings(items[0]);
  const values = bindings.map((binding) => evaluate(binding.valueExpression, env));
  const letEnv = env.child();

  for (let index = 0; index < bindings.length; index += 1) {
    letEnv.define(bindings[index].name, values[index]);
  }

  return evaluateSequence(items.slice(1), letEnv);
}

function evaluateNamedLet(items: Expr[], env: Environment): SchemeValue {
  if (items.length < 3) {
    throw new EvalError('named let requires a name, bindings, and a body');
  }

  const nameExpression = items[0];
  if (nameExpression.kind !== 'symbol') {
    throw new EvalError('named let requires a symbol name');
  }

  const bindings = parseLetBindings(items[1]);
  const args = bindings.map((binding) => evaluate(binding.valueExpression, env));
  const letEnv = env.child();
  const binding: Binding = { value: VOID_VALUE };
  letEnv.defineBinding(nameExpression.value, binding);

  const closure = makeClosure(
    nameExpression.value,
    bindings.map((bindingSpec) => bindingSpec.name),
    items.slice(2),
    letEnv,
  );
  binding.value = closure;

  return applyProcedure(closure, args);
}

function parseLetBindings(expression: Expr): LetBindingSpec[] {
  if (expression.kind !== 'list') {
    throw new EvalError('let bindings must be a list');
  }

  return expression.items.map((bindingExpression) => {
    if (bindingExpression.kind !== 'list' || bindingExpression.items.length !== 2) {
      throw new EvalError('let bindings must contain exactly a name and value');
    }

    const nameExpression = bindingExpression.items[0];
    if (nameExpression.kind !== 'symbol') {
      throw new EvalError('let binding names must be symbols');
    }

    return {
      name: nameExpression.value,
      valueExpression: bindingExpression.items[1],
    };
  });
}

function parseLambdaParts(items: Expr[]): { params: string[]; body: Expr[] } {
  if (items.length < 2) {
    throw new EvalError('lambda requires parameters and a body');
  }

  const params = readParameterList(items[0]);
  const body = items.slice(1);
  return { params, body };
}

function readParameterList(expression: Expr): string[] {
  if (expression.kind !== 'list') {
    throw new EvalError('lambda parameters must be a list');
  }

  return readParameterListItems(expression.items);
}

function readParameterListItems(items: Expr[]): string[] {
  return items.map((item) => {
    if (item.kind !== 'symbol') {
      throw new EvalError('lambda parameters must be symbols');
    }

    return item.value;
  });
}

function makeClosure(name: string, params: string[], body: Expr[], env: Environment): ClosureProcedureValue {
  return { kind: 'procedure', procedureKind: 'closure', name, params, body, env };
}

function evaluateQuote(items: Expr[]): SchemeValue {
  expectExactExprCount('quote', items, 1);
  return quoteExpression(items[0]);
}

function quoteExpression(expression: Expr): SchemeValue {
  switch (expression.kind) {
    case 'number':
      return numberValue(expression.value);
    case 'boolean':
      return booleanValue(expression.value);
    case 'string':
      return stringValue(expression.value);
    case 'symbol':
      return symbolValue(expression.value);
    case 'list': {
      let result: SchemeValue = EMPTY_LIST_VALUE;
      for (let index = expression.items.length - 1; index >= 0; index -= 1) {
        result = pairValue(quoteExpression(expression.items[index]), result);
      }
      return result;
    }
  }
}

function evaluateSequence(expressions: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = VOID_VALUE;
  for (const expression of expressions) {
    result = evaluate(expression, env);
  }
  return result;
}

function applyProcedure(value: SchemeValue, args: SchemeValue[]): SchemeValue {
  if (value.kind !== 'procedure') {
    throw new EvalError('attempted to call a non-procedure');
  }

  if (value.procedureKind === 'builtin') {
    return value.apply(args);
  }

  expectExactArgCount(value.name, args, value.params.length);

  const callEnv = value.env.child();
  for (let index = 0; index < value.params.length; index += 1) {
    callEnv.define(value.params[index], args[index]);
  }

  return evaluateSequence(value.body, callEnv);
}

function makeBuiltinProcedure(
  name: string,
  apply: (args: SchemeValue[]) => SchemeValue,
): BuiltinProcedureValue {
  return { kind: 'procedure', procedureKind: 'builtin', name, apply };
}

function makePredicateProcedure(
  name: string,
  predicate: (value: SchemeValue) => boolean,
): BuiltinProcedureValue {
  return makeBuiltinProcedure(name, (args) => {
    expectExactArgCount(name, args, 1);
    return booleanValue(predicate(args[0]));
  });
}

function applyNot(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('not', args, 1);
  return booleanValue(!isTruthy(args[0]));
}

function applyCons(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('cons', args, 2);
  return pairValue(args[0], args[1]);
}

function applyCar(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('car', args, 1);
  return expectPair(args[0], 'car').car;
}

function applyCdr(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('cdr', args, 1);
  return expectPair(args[0], 'cdr').cdr;
}

function applyNullPredicate(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('null?', args, 1);
  return booleanValue(args[0].kind === 'empty-list');
}

function applyLength(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('length', args, 1);
  return numberValue(expectList(args[0], 'length').length);
}

function applyAppend(args: SchemeValue[]): SchemeValue {
  if (args.length === 0) {
    return EMPTY_LIST_VALUE;
  }

  let result = args[args.length - 1];
  for (let index = args.length - 2; index >= 0; index -= 1) {
    const elements = expectList(args[index], 'append');
    for (let elementIndex = elements.length - 1; elementIndex >= 0; elementIndex -= 1) {
      result = pairValue(elements[elementIndex], result);
    }
  }

  return result;
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

function expectPair(value: SchemeValue, name: string): PairValue {
  if (value.kind !== 'pair') {
    throw new EvalError(`${name} expected a pair`);
  }

  return value;
}

function expectList(value: SchemeValue, name: string): SchemeValue[] {
  const elements: SchemeValue[] = [];
  let current = value;

  while (current.kind === 'pair') {
    elements.push(current.car);
    current = current.cdr;
  }

  if (current.kind !== 'empty-list') {
    throw new EvalError(`${name} expected a proper list`);
  }

  return elements;
}

function arrayToList(values: SchemeValue[]): SchemeValue {
  let result: SchemeValue = EMPTY_LIST_VALUE;
  for (let index = values.length - 1; index >= 0; index -= 1) {
    result = pairValue(values[index], result);
  }
  return result;
}

function expectExactArgCount(name: string, args: ArrayLike<unknown>, expected: number): void {
  if (args.length !== expected) {
    throw new EvalError(`${name} expected ${expected} argument(s), got ${args.length}`);
  }
}

function expectExactExprCount(name: string, expressions: Expr[], expected: number): void {
  if (expressions.length !== expected) {
    throw new EvalError(`${name} expected ${expected} argument(s), got ${expressions.length}`);
  }
}

function expectAtLeastArgCount(name: string, values: ArrayLike<unknown>, minimum: number): void {
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
    case 'symbol':
      return value.value;
    case 'empty-list':
      return '()';
    case 'pair':
      return formatPair(value);
    case 'void':
      return '#<void>';
    case 'procedure':
      return `#<procedure:${value.name}>`;
  }
}

function formatPair(pair: PairValue): string {
  const parts: string[] = [];
  let current: SchemeValue = pair;

  while (current.kind === 'pair') {
    parts.push(formatValue(current.car));
    current = current.cdr;
  }

  if (current.kind === 'empty-list') {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatValue(current)})`;
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

function numberValue(value: number): NumberValue {
  return { kind: 'number', value };
}

function booleanValue(value: boolean): BooleanValue {
  return value ? TRUE_VALUE : FALSE_VALUE;
}

function stringValue(value: string): StringValue {
  return { kind: 'string', value };
}

function symbolValue(value: string): SymbolValue {
  return { kind: 'symbol', value };
}

function pairValue(car: SchemeValue, cdr: SchemeValue): PairValue {
  return { kind: 'pair', car, cdr };
}

function isWhitespace(char: string): boolean {
  return /\s/u.test(char);
}

function skipComment(
  input: string,
  index: number,
  line: number,
  column: number,
): { nextIndex: number; nextLine: number; nextColumn: number } {
  let nextIndex = index;
  let nextLine = line;
  let nextColumn = column;

  while (nextIndex < input.length && input[nextIndex] !== '\n') {
    ({ line: nextLine, column: nextColumn } = advanceLocation(
      input[nextIndex],
      nextLine,
      nextColumn,
    ));
    nextIndex += 1;
  }

  return { nextIndex, nextLine, nextColumn };
}

function readStringToken(
  input: string,
  startIndex: number,
  startLine: number,
  startColumn: number,
): { value: string; nextIndex: number; nextLine: number; nextColumn: number } {
  const startLocation: SourceLocation = { line: startLine, column: startColumn };
  let value = '';
  let index = startIndex + 1;
  let line = startLine;
  let column = startColumn;

  ({ line, column } = advanceLocation('"', line, column));

  while (index < input.length) {
    const char = input[index];
    if (char === undefined) {
      break;
    }

    if (char === '"') {
      const next = advanceLocation(char, line, column);
      return {
        value,
        nextIndex: index + 1,
        nextLine: next.line,
        nextColumn: next.column,
      };
    }

    if (char === '\\') {
      ({ line, column } = advanceLocation(char, line, column));
      index += 1;
      const escaped = input[index];
      if (escaped === undefined) {
        throw new EvalError('unterminated string literal', startLocation);
      }

      value += decodeEscape(escaped);
      ({ line, column } = advanceLocation(escaped, line, column));
      index += 1;
      continue;
    }

    value += char;
    ({ line, column } = advanceLocation(char, line, column));
    index += 1;
  }

  throw new EvalError('unterminated string literal', startLocation);
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

function readAtomToken(
  input: string,
  startIndex: number,
  startLine: number,
  startColumn: number,
): { token: Token; nextIndex: number; nextLine: number; nextColumn: number } {
  let index = startIndex;
  let line = startLine;
  let column = startColumn;

  while (index < input.length && !isTokenBoundary(input[index])) {
    ({ line, column } = advanceLocation(input[index], line, column));
    index += 1;
  }

  const raw = input.slice(startIndex, index);
  return {
    token: tokenFromAtom(raw, { line: startLine, column: startColumn }),
    nextIndex: index,
    nextLine: line,
    nextColumn: column,
  };
}

function isTokenBoundary(char: string | undefined): boolean {
  return (
    char === undefined ||
    isWhitespace(char) ||
    char === '(' ||
    char === ')' ||
    char === "'" ||
    char === ';'
  );
}

function tokenFromAtom(raw: string, location: SourceLocation): Token {
  if (raw === '#t') {
    return { kind: 'boolean', value: true, location };
  }

  if (raw === '#f') {
    return { kind: 'boolean', value: false, location };
  }

  if (/^[+-]?\d+$/u.test(raw) && raw !== '+' && raw !== '-') {
    return { kind: 'number', value: Number(raw), location };
  }

  return { kind: 'symbol', value: raw, location };
}

function advanceLocation(char: string, line: number, column: number): SourceLocation {
  if (char === '\n') {
    return { line: line + 1, column: 1 };
  }

  return { line, column: column + 1 };
}

function rethrowWithLocation(error: unknown, location: SourceLocation): never {
  if (error instanceof EvalError) {
    throw error.withLocation(location);
  }

  throw error;
}
