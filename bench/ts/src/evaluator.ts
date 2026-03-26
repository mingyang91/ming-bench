import { EvalError } from './evalError.js';

type NumberExpr = { kind: 'number'; value: number };
type BooleanExpr = { kind: 'boolean'; value: boolean };
type StringExpr = { kind: 'string'; value: string };
type SymbolExpr = { kind: 'symbol'; name: string };
type ListExpr = { kind: 'list'; elements: Expr[] };

type Expr = NumberExpr | BooleanExpr | StringExpr | SymbolExpr | ListExpr;

type SymbolValue = { kind: 'symbol'; name: string };
type NilValue = { kind: 'nil' };
type PairValue = { kind: 'pair'; car: RuntimeValue; cdr: RuntimeValue };
type BuiltinProcedure = { kind: 'builtin'; name: BuiltinName };
type ClosureProcedure = { kind: 'closure'; params: string[]; body: Expr[]; env: Environment };
type VoidValue = { kind: 'void' };

type RuntimeValue =
  | NumberExpr
  | BooleanExpr
  | StringExpr
  | SymbolValue
  | NilValue
  | PairValue
  | BuiltinProcedure
  | ClosureProcedure
  | VoidValue;

type Token =
  | { kind: 'paren'; value: '(' | ')' }
  | { kind: 'atom'; value: string }
  | { kind: 'string'; value: string }
  | { kind: 'quote' };

const BUILTIN_NAMES = [
  '+',
  '-',
  '*',
  '/',
  '<',
  '>',
  '=',
  '<=',
  'not',
  'cons',
  'car',
  'cdr',
  'null?',
  'list',
  'append',
  'length',
  'string?',
  'number?',
  'boolean?',
  'pair?',
  'symbol?',
] as const;
type BuiltinName = (typeof BUILTIN_NAMES)[number];

const NIL_VALUE: NilValue = { kind: 'nil' };
const VOID_VALUE: VoidValue = { kind: 'void' };

class Environment {
  private readonly bindings = new Map<string, RuntimeValue>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: RuntimeValue): void {
    this.bindings.set(name, value);
  }

  lookup(name: string): RuntimeValue {
    if (this.bindings.has(name)) {
      return this.bindings.get(name)!;
    }

    if (this.parent !== undefined) {
      return this.parent.lookup(name);
    }

    throw new EvalError(`unbound symbol: ${name}`);
  }
}

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

function evaluateProgram(input: string): { result: RuntimeValue; output: string } {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('expected at least one expression');
  }

  const env = createGlobalEnv();
  let result: RuntimeValue = VOID_VALUE;

  for (const expr of expressions) {
    result = evaluateExpr(expr, env);
  }

  return {
    result,
    output: '',
  };
}

function createGlobalEnv(): Environment {
  const env = new Environment();

  for (const name of BUILTIN_NAMES) {
    env.define(name, { kind: 'builtin', name });
  }

  return env;
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

    if (char === "'") {
      tokens.push({ kind: 'quote' });
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

  if (token.kind === 'quote') {
    const parsed = parseExpr(tokens, index + 1);
    return {
      expr: {
        kind: 'list',
        elements: [
          { kind: 'symbol', name: 'quote' },
          parsed.expr,
        ],
      },
      nextIndex: parsed.nextIndex,
    };
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

function evaluateExpr(expr: Expr, env: Environment): RuntimeValue {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      return env.lookup(expr.name);
    case 'list':
      return evaluateList(expr.elements, env);
  }
}

function evaluateList(elements: Expr[], env: Environment): RuntimeValue {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const [head, ...argExprs] = elements;

  if (head.kind === 'symbol') {
    switch (head.name) {
      case 'define':
        return evaluateDefine(argExprs, env);
      case 'if':
        return evaluateIf(argExprs, env);
      case 'quote':
        return evaluateQuote(argExprs);
      case 'lambda':
        return evaluateLambda(argExprs, env);
      case 'and':
        return evaluateAnd(argExprs, env);
      case 'or':
        return evaluateOr(argExprs, env);
      case 'begin':
        return evaluateBegin(argExprs, env);
      case 'let':
        return evaluateLet(argExprs, env);
      case 'cond':
        return evaluateCond(argExprs, env);
    }
  }

  const procedure = evaluateExpr(head, env);
  const args = argExprs.map((expr) => evaluateExpr(expr, env));
  return applyProcedure(procedure, args);
}

function evaluateDefine(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length < 2) {
    throw new EvalError('define expects a target and a value');
  }

  const [target, ...body] = argExprs;

  if (target.kind === 'symbol') {
    if (body.length !== 1) {
      throw new EvalError('define variable form expects exactly 1 value expression');
    }

    const value = evaluateExpr(body[0], env);
    env.define(target.name, value);
    return VOID_VALUE;
  }

  if (target.kind === 'list' && target.elements.length > 0) {
    const [nameExpr, ...paramExprs] = target.elements;
    if (nameExpr.kind !== 'symbol') {
      throw new EvalError('define function form expects a function name');
    }

    const params = readParameterList(paramExprs);
    const procedure: ClosureProcedure = {
      kind: 'closure',
      params,
      body,
      env,
    };

    env.define(nameExpr.name, procedure);
    return VOID_VALUE;
  }

  throw new EvalError('invalid define form');
}

function evaluateIf(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length !== 3) {
    throw new EvalError('if expects exactly 3 arguments');
  }

  const condition = evaluateExpr(argExprs[0], env);
  return isTruthy(condition) ? evaluateExpr(argExprs[1], env) : evaluateExpr(argExprs[2], env);
}

function evaluateQuote(argExprs: Expr[]): RuntimeValue {
  if (argExprs.length !== 1) {
    throw new EvalError('quote expects exactly 1 argument');
  }

  return quoteExpr(argExprs[0]);
}

function quoteExpr(expr: Expr): RuntimeValue {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      return { kind: 'symbol', name: expr.name };
    case 'list':
      return buildList(expr.elements.map((element) => quoteExpr(element)));
  }
}

function evaluateLambda(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length < 2) {
    throw new EvalError('lambda expects parameters and a body');
  }

  const [paramsExpr, ...body] = argExprs;
  if (paramsExpr.kind !== 'list') {
    throw new EvalError('lambda parameters must be a list');
  }

  return {
    kind: 'closure',
    params: readParameterList(paramsExpr.elements),
    body,
    env,
  };
}

function readParameterList(exprs: Expr[]): string[] {
  const params: string[] = [];

  for (const expr of exprs) {
    if (expr.kind !== 'symbol') {
      throw new EvalError('parameter list must contain only symbols');
    }

    params.push(expr.name);
  }

  return params;
}

function evaluateAnd(argExprs: Expr[], env: Environment): RuntimeValue {
  let lastValue: RuntimeValue = { kind: 'boolean', value: true };

  for (const expr of argExprs) {
    lastValue = evaluateExpr(expr, env);
    if (!isTruthy(lastValue)) {
      return lastValue;
    }
  }

  return lastValue;
}

function evaluateOr(argExprs: Expr[], env: Environment): RuntimeValue {
  for (const expr of argExprs) {
    const value = evaluateExpr(expr, env);
    if (isTruthy(value)) {
      return value;
    }
  }

  return { kind: 'boolean', value: false };
}

function evaluateBegin(argExprs: Expr[], env: Environment): RuntimeValue {
  return evaluateSequence(argExprs, env);
}

function evaluateLet(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length < 2) {
    throw new EvalError('let expects bindings and a body');
  }

  let name: string | undefined;
  let bindingsExpr: Expr;
  let body: Expr[];

  if (argExprs[0].kind === 'symbol') {
    if (argExprs.length < 3) {
      throw new EvalError('named let expects a name, bindings, and a body');
    }

    name = argExprs[0].name;
    bindingsExpr = argExprs[1];
    body = argExprs.slice(2);
  } else {
    bindingsExpr = argExprs[0];
    body = argExprs.slice(1);
  }

  const bindings = readLetBindings(bindingsExpr);
  const values = bindings.initExprs.map((expr) => evaluateExpr(expr, env));

  if (name === undefined) {
    const letEnv = new Environment(env);
    for (let index = 0; index < bindings.names.length; index += 1) {
      letEnv.define(bindings.names[index], values[index]);
    }

    return evaluateSequence(body, letEnv);
  }

  const letEnv = new Environment(env);
  const procedure: ClosureProcedure = {
    kind: 'closure',
    params: bindings.names,
    body,
    env: letEnv,
  };
  letEnv.define(name, procedure);
  return applyClosure(procedure, values);
}

function readLetBindings(bindingsExpr: Expr): { names: string[]; initExprs: Expr[] } {
  if (bindingsExpr.kind !== 'list') {
    throw new EvalError('let bindings must be a list');
  }

  const names: string[] = [];
  const initExprs: Expr[] = [];

  for (const bindingExpr of bindingsExpr.elements) {
    if (bindingExpr.kind !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError('let bindings must contain (name value) pairs');
    }

    const [nameExpr, initExpr] = bindingExpr.elements;
    if (nameExpr.kind !== 'symbol') {
      throw new EvalError('let binding name must be a symbol');
    }

    names.push(nameExpr.name);
    initExprs.push(initExpr);
  }

  return { names, initExprs };
}

function evaluateCond(argExprs: Expr[], env: Environment): RuntimeValue {
  for (let index = 0; index < argExprs.length; index += 1) {
    const clauseExpr = argExprs[index];
    if (clauseExpr.kind !== 'list' || clauseExpr.elements.length === 0) {
      throw new EvalError('cond clauses must be non-empty lists');
    }

    const [testExpr, ...body] = clauseExpr.elements;
    const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';

    if (isElseClause) {
      if (index !== argExprs.length - 1) {
        throw new EvalError('cond else clause must be last');
      }
      if (body.length === 0) {
        throw new EvalError('cond else clause expects at least 1 expression');
      }
      return evaluateSequence(body, env);
    }

    const testValue = evaluateExpr(testExpr, env);
    if (!isTruthy(testValue)) {
      continue;
    }

    if (body.length === 0) {
      return testValue;
    }

    return evaluateSequence(body, env);
  }

  return VOID_VALUE;
}

function applyProcedure(procedure: RuntimeValue, args: RuntimeValue[]): RuntimeValue {
  switch (procedure.kind) {
    case 'builtin':
      return applyBuiltin(procedure.name, args);
    case 'closure':
      return applyClosure(procedure, args);
    default:
      throw new EvalError('attempted to call a non-procedure');
  }
}

function applyClosure(procedure: ClosureProcedure, args: RuntimeValue[]): RuntimeValue {
  if (args.length !== procedure.params.length) {
    throw new EvalError(`expected ${procedure.params.length} arguments, got ${args.length}`);
  }

  const callEnv = new Environment(procedure.env);

  for (let index = 0; index < procedure.params.length; index += 1) {
    callEnv.define(procedure.params[index], args[index]);
  }

  return evaluateSequence(procedure.body, callEnv);
}

function evaluateSequence(exprs: Expr[], env: Environment): RuntimeValue {
  let result: RuntimeValue = VOID_VALUE;

  for (const expr of exprs) {
    result = evaluateExpr(expr, env);
  }

  return result;
}

function applyBuiltin(name: BuiltinName, args: RuntimeValue[]): RuntimeValue {
  switch (name) {
    case '+':
      return makeNumber(args.map((arg) => expectNumber(arg, '+')).reduce((sum, value) => sum + value, 0));
    case '-':
      return applySubtraction(args);
    case '*':
      return makeNumber(
        args.map((arg) => expectNumber(arg, '*')).reduce((product, value) => product * value, 1),
      );
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
    case 'cons':
      if (args.length !== 2) {
        throw new EvalError('cons expects exactly 2 arguments');
      }
      return { kind: 'pair', car: args[0], cdr: args[1] };
    case 'car':
      if (args.length !== 1) {
        throw new EvalError('car expects exactly 1 argument');
      }
      return expectPair(args[0], 'car').car;
    case 'cdr':
      if (args.length !== 1) {
        throw new EvalError('cdr expects exactly 1 argument');
      }
      return expectPair(args[0], 'cdr').cdr;
    case 'null?':
      if (args.length !== 1) {
        throw new EvalError('null? expects exactly 1 argument');
      }
      return { kind: 'boolean', value: args[0].kind === 'nil' };
    case 'list':
      return buildList(args);
    case 'append':
      return applyAppend(args);
    case 'length':
      if (args.length !== 1) {
        throw new EvalError('length expects exactly 1 argument');
      }
      return makeNumber(listLength(args[0]));
    case 'string?':
      return applyTypePredicate(args, 'string?', (value) => value.kind === 'string');
    case 'number?':
      return applyTypePredicate(args, 'number?', (value) => value.kind === 'number');
    case 'boolean?':
      return applyTypePredicate(args, 'boolean?', (value) => value.kind === 'boolean');
    case 'pair?':
      return applyTypePredicate(args, 'pair?', (value) => value.kind === 'pair');
    case 'symbol?':
      return applyTypePredicate(args, 'symbol?', (value) => value.kind === 'symbol');
  }
}

function applySubtraction(args: RuntimeValue[]): RuntimeValue {
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

function applyDivision(args: RuntimeValue[]): RuntimeValue {
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
  args: RuntimeValue[],
  name: string,
  predicate: (left: number, right: number) => boolean,
): RuntimeValue {
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

function expectNumber(value: RuntimeValue, procedure: string): number {
  if (value.kind !== 'number') {
    throw new EvalError(`${procedure} expects numeric arguments`);
  }
  return value.value;
}

function expectPair(value: RuntimeValue, procedure: string): PairValue {
  if (value.kind !== 'pair') {
    throw new EvalError(`${procedure} expects a pair`);
  }
  return value;
}

function buildList(elements: RuntimeValue[]): RuntimeValue {
  let list: RuntimeValue = NIL_VALUE;

  for (let index = elements.length - 1; index >= 0; index -= 1) {
    list = {
      kind: 'pair',
      car: elements[index],
      cdr: list,
    };
  }

  return list;
}

function applyAppend(args: RuntimeValue[]): RuntimeValue {
  if (args.length === 0) {
    return NIL_VALUE;
  }

  let result = args[args.length - 1];

  for (let index = args.length - 2; index >= 0; index -= 1) {
    const elements = listToArray(args[index], 'append');
    for (let elementIndex = elements.length - 1; elementIndex >= 0; elementIndex -= 1) {
      result = {
        kind: 'pair',
        car: elements[elementIndex],
        cdr: result,
      };
    }
  }

  return result;
}

function listLength(value: RuntimeValue): number {
  return listToArray(value, 'length').length;
}

function listToArray(value: RuntimeValue, procedure: string): RuntimeValue[] {
  const elements: RuntimeValue[] = [];
  let current = value;

  while (current.kind === 'pair') {
    elements.push(current.car);
    current = current.cdr;
  }

  if (current.kind !== 'nil') {
    throw new EvalError(`${procedure} expects a proper list`);
  }

  return elements;
}

function applyTypePredicate(
  args: RuntimeValue[],
  name: string,
  predicate: (value: RuntimeValue) => boolean,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  return { kind: 'boolean', value: predicate(args[0]) };
}

function isTruthy(value: RuntimeValue): boolean {
  return value.kind !== 'boolean' || value.value;
}

function makeNumber(value: number): NumberExpr {
  return {
    kind: 'number',
    value: Object.is(value, -0) ? 0 : value,
  };
}

function formatValue(value: RuntimeValue): string {
  switch (value.kind) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return `"${escapeString(value.value)}"`;
    case 'symbol':
      return value.name;
    case 'nil':
      return '()';
    case 'pair':
      return formatPair(value);
    case 'builtin':
    case 'closure':
      return '#<procedure>';
    case 'void':
      return '';
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
  return isWhitespace(char) || char === '(' || char === ')' || char === "'" || char === ';';
}

function formatPair(value: PairValue): string {
  const parts: string[] = [];
  let current: RuntimeValue = value;

  while (current.kind === 'pair') {
    parts.push(formatValue(current.car));
    current = current.cdr;
  }

  if (current.kind === 'nil') {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatValue(current)})`;
}
