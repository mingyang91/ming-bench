import { EvalError, type SourcePos } from './evalError.js';

type ExprBase = { pos: SourcePos };

type NumberExpr = ExprBase & { kind: 'number'; value: number };
type BooleanExpr = ExprBase & { kind: 'boolean'; value: boolean };
type StringExpr = ExprBase & { kind: 'string'; value: string };
type CharExpr = ExprBase & { kind: 'char'; value: string };
type SymbolExpr = ExprBase & { kind: 'symbol'; name: string };
type ListExpr = ExprBase & { kind: 'list'; elements: Expr[] };

type Expr = NumberExpr | BooleanExpr | StringExpr | CharExpr | SymbolExpr | ListExpr;

type CharValue = { kind: 'char'; value: string };
type StringValue = { kind: 'string'; value: string; mutable: boolean };
type SymbolValue = { kind: 'symbol'; name: string };
type NilValue = { kind: 'nil' };
type PairValue = { kind: 'pair'; car: RuntimeValue; cdr: RuntimeValue };
type BuiltinProcedure = { kind: 'builtin'; name: BuiltinName };
type ClosureProcedure = {
  kind: 'closure';
  params: string[];
  restParam?: string;
  body: Expr[];
  env: Environment;
};
type VoidValue = { kind: 'void' };

type RuntimeValue =
  | NumberExpr
  | BooleanExpr
  | StringValue
  | CharValue
  | SymbolValue
  | NilValue
  | PairValue
  | BuiltinProcedure
  | ClosureProcedure
  | VoidValue;

type EvalContext = {
  output: string[];
};

type Token =
  | { kind: 'paren'; value: '(' | ')'; pos: SourcePos }
  | { kind: 'atom'; value: string; pos: SourcePos }
  | { kind: 'string'; value: string; pos: SourcePos }
  | { kind: 'quote'; pos: SourcePos };

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
  'display',
  'write',
  'newline',
  'string-append',
  'string-length',
  'substring',
  'string->number',
  'number->string',
  'symbol->string',
  'string->symbol',
  'string-ref',
  'string-copy',
  'string-set!',
  'char?',
  'apply',
] as const;
type BuiltinName = (typeof BUILTIN_NAMES)[number];

const NIL_VALUE: NilValue = { kind: 'nil' };
const VOID_VALUE: VoidValue = { kind: 'void' };
const DEFAULT_SOURCE_POS: SourcePos = { line: 1, col: 1 };

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

  assign(name: string, value: RuntimeValue): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, value);
      return;
    }

    if (this.parent !== undefined) {
      this.parent.assign(name, value);
      return;
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
  const context: EvalContext = { output: [] };
  let result: RuntimeValue = VOID_VALUE;

  for (const expr of expressions) {
    result = evaluateExpr(expr, env, context);
  }

  return {
    result,
    output: context.output.join(''),
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
  let line = 1;
  let col = 1;

  const currentPos = (): SourcePos => ({ line, col });
  const advanceChar = (char: string): void => {
    if (char === '\n') {
      line += 1;
      col = 1;
      return;
    }

    col += 1;
  };

  while (index < input.length) {
    const char = input[index];

    if (isWhitespace(char)) {
      advanceChar(char);
      index += 1;
      continue;
    }

    if (char === ';') {
      while (index < input.length && input[index] !== '\n') {
        advanceChar(input[index]);
        index += 1;
      }
      continue;
    }

    if (char === '(' || char === ')') {
      tokens.push({ kind: 'paren', value: char, pos: currentPos() });
      advanceChar(char);
      index += 1;
      continue;
    }

    if (char === "'") {
      tokens.push({ kind: 'quote', pos: currentPos() });
      advanceChar(char);
      index += 1;
      continue;
    }

    if (char === '"') {
      const pos = currentPos();
      const parsed = parseStringToken(input, index, pos);
      tokens.push({ kind: 'string', value: parsed.value, pos });

      for (let scan = index; scan < parsed.nextIndex; scan += 1) {
        advanceChar(input[scan]);
      }

      index = parsed.nextIndex;
      continue;
    }

    const pos = currentPos();
    let end = index;
    while (end < input.length && !isDelimiter(input[end])) {
      end += 1;
    }

    tokens.push({ kind: 'atom', value: input.slice(index, end), pos });
    col += end - index;
    index = end;
  }

  return tokens;
}

function parseStringToken(
  input: string,
  start: number,
  pos: SourcePos,
): { value: string; nextIndex: number } {
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
        throw new EvalError('unterminated string literal', pos);
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

  throw new EvalError('unterminated string literal', pos);
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
        pos: token.pos,
        elements: [
          { kind: 'symbol', name: 'quote', pos: token.pos },
          parsed.expr,
        ],
      },
      nextIndex: parsed.nextIndex,
    };
  }

  if (token.kind === 'paren') {
    if (token.value === ')') {
      throw new EvalError('unexpected )', token.pos);
    }

    const elements: Expr[] = [];
    let nextIndex = index + 1;

    while (nextIndex < tokens.length) {
      const nextToken = tokens[nextIndex];
      if (nextToken.kind === 'paren' && nextToken.value === ')') {
        return {
          expr: { kind: 'list', pos: token.pos, elements },
          nextIndex: nextIndex + 1,
        };
      }

      const parsed = parseExpr(tokens, nextIndex);
      elements.push(parsed.expr);
      nextIndex = parsed.nextIndex;
    }

    throw new EvalError('unterminated list', token.pos);
  }

  if (token.kind === 'string') {
    return {
      expr: { kind: 'string', value: token.value, pos: token.pos },
      nextIndex: index + 1,
    };
  }

  return {
    expr: parseAtom(token),
    nextIndex: index + 1,
  };
}

function parseAtom(token: Extract<Token, { kind: 'atom' }>): Expr {
  if (token.value === '#t') {
    return { kind: 'boolean', value: true, pos: token.pos };
  }

  if (token.value === '#f') {
    return { kind: 'boolean', value: false, pos: token.pos };
  }

  if (token.value.startsWith('#\\')) {
    return { kind: 'char', value: parseCharLiteral(token), pos: token.pos };
  }

  if (/^[+-]?\d+$/.test(token.value)) {
    return { kind: 'number', value: Number.parseInt(token.value, 10), pos: token.pos };
  }

  return { kind: 'symbol', name: token.value, pos: token.pos };
}

function parseCharLiteral(token: Extract<Token, { kind: 'atom' }>): string {
  const literal = token.value.slice(2);

  switch (literal) {
    case 'space':
      return ' ';
    case 'newline':
      return '\n';
  }

  const chars = stringChars(literal);
  if (chars.length === 1) {
    return chars[0];
  }

  throw new EvalError('invalid character literal', token.pos);
}

function evaluateExpr(expr: Expr, env: Environment, context: EvalContext): RuntimeValue {
  try {
    switch (expr.kind) {
      case 'number':
      case 'boolean':
        return expr;
      case 'string':
        return makeString(expr.value);
      case 'char':
        return makeChar(expr.value);
      case 'symbol':
        return env.lookup(expr.name);
      case 'list':
        return evaluateList(expr.elements, env, context);
    }
  } catch (error) {
    throw attachPosition(error, expr.pos);
  }
}

function evaluateList(elements: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const [head, ...argExprs] = elements;

  if (head.kind === 'symbol') {
    switch (head.name) {
      case 'define':
        return evaluateDefine(argExprs, env, context);
      case 'set!':
        return evaluateSet(argExprs, env, context);
      case 'if':
        return evaluateIf(argExprs, env, context);
      case 'quote':
        return evaluateQuote(argExprs);
      case 'lambda':
        return evaluateLambda(argExprs, env);
      case 'and':
        return evaluateAnd(argExprs, env, context);
      case 'or':
        return evaluateOr(argExprs, env, context);
      case 'begin':
        return evaluateBegin(argExprs, env, context);
      case 'let':
        return evaluateLet(argExprs, env, context);
      case 'cond':
        return evaluateCond(argExprs, env, context);
    }
  }

  const procedure = evaluateExpr(head, env, context);
  const args = argExprs.map((expr) => evaluateExpr(expr, env, context));
  return applyProcedure(procedure, args, context);
}

function evaluateDefine(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  if (argExprs.length < 2) {
    throw new EvalError('define expects a target and a value');
  }

  const [target, ...body] = argExprs;

  if (target.kind === 'symbol') {
    if (body.length !== 1) {
      throw new EvalError('define variable form expects exactly 1 value expression');
    }

    const value = evaluateExpr(body[0], env, context);
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
      params: params.fixedParams,
      restParam: params.restParam,
      body,
      env,
    };

    env.define(nameExpr.name, procedure);
    return VOID_VALUE;
  }

  throw new EvalError('invalid define form');
}

function evaluateSet(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  if (argExprs.length !== 2) {
    throw new EvalError('set! expects exactly 2 arguments');
  }

  const [target, valueExpr] = argExprs;
  if (target.kind !== 'symbol') {
    throw new EvalError('set! expects a symbol target');
  }

  const value = evaluateExpr(valueExpr, env, context);
  env.assign(target.name, value);
  return VOID_VALUE;
}

function evaluateIf(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  if (argExprs.length !== 3) {
    throw new EvalError('if expects exactly 3 arguments');
  }

  const condition = evaluateExpr(argExprs[0], env, context);
  return isTruthy(condition)
    ? evaluateExpr(argExprs[1], env, context)
    : evaluateExpr(argExprs[2], env, context);
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
      return expr;
    case 'string':
      return makeString(expr.value);
    case 'char':
      return makeChar(expr.value);
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
  const params =
    paramsExpr.kind === 'symbol'
      ? { fixedParams: [], restParam: paramsExpr.name }
      : paramsExpr.kind === 'list'
        ? readParameterList(paramsExpr.elements)
        : undefined;

  if (params === undefined) {
    throw new EvalError('lambda parameters must be a list or symbol');
  }

  return {
    kind: 'closure',
    params: params.fixedParams,
    restParam: params.restParam,
    body,
    env,
  };
}

function readParameterList(exprs: Expr[]): { fixedParams: string[]; restParam?: string } {
  const fixedParams: string[] = [];

  for (let index = 0; index < exprs.length; index += 1) {
    const expr = exprs[index];
    if (expr.kind !== 'symbol') {
      throw new EvalError('parameter list must contain only symbols');
    }

    if (expr.name === '.') {
      const restExpr = exprs[index + 1];
      if (restExpr === undefined || restExpr.kind !== 'symbol' || index + 2 !== exprs.length) {
        throw new EvalError('invalid dotted parameter list');
      }

      return {
        fixedParams,
        restParam: restExpr.name,
      };
    }

    fixedParams.push(expr.name);
  }

  return { fixedParams };
}

function evaluateAnd(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  let lastValue: RuntimeValue = makeBoolean(true);

  for (const expr of argExprs) {
    lastValue = evaluateExpr(expr, env, context);
    if (!isTruthy(lastValue)) {
      return lastValue;
    }
  }

  return lastValue;
}

function evaluateOr(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  for (const expr of argExprs) {
    const value = evaluateExpr(expr, env, context);
    if (isTruthy(value)) {
      return value;
    }
  }

  return makeBoolean(false);
}

function evaluateBegin(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  return evaluateSequence(argExprs, env, context);
}

function evaluateLet(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
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
  const values = bindings.initExprs.map((expr) => evaluateExpr(expr, env, context));

  if (name === undefined) {
    const letEnv = new Environment(env);
    for (let index = 0; index < bindings.names.length; index += 1) {
      letEnv.define(bindings.names[index], values[index]);
    }

    return evaluateSequence(body, letEnv, context);
  }

  const letEnv = new Environment(env);
  const procedure: ClosureProcedure = {
    kind: 'closure',
    params: bindings.names,
    body,
    env: letEnv,
  };
  letEnv.define(name, procedure);
  return applyClosure(procedure, values, context);
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

function evaluateCond(argExprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
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
      return evaluateSequence(body, env, context);
    }

    const testValue = evaluateExpr(testExpr, env, context);
    if (!isTruthy(testValue)) {
      continue;
    }

    if (body.length === 0) {
      return testValue;
    }

    return evaluateSequence(body, env, context);
  }

  return VOID_VALUE;
}

function applyProcedure(
  procedure: RuntimeValue,
  args: RuntimeValue[],
  context: EvalContext,
): RuntimeValue {
  switch (procedure.kind) {
    case 'builtin':
      return applyBuiltin(procedure.name, args, context);
    case 'closure':
      return applyClosure(procedure, args, context);
    default:
      throw new EvalError('attempted to call a non-procedure');
  }
}

function applyClosure(
  procedure: ClosureProcedure,
  args: RuntimeValue[],
  context: EvalContext,
): RuntimeValue {
  if (procedure.restParam === undefined && args.length !== procedure.params.length) {
    throw new EvalError(`expected ${procedure.params.length} arguments, got ${args.length}`);
  }

  if (procedure.restParam !== undefined && args.length < procedure.params.length) {
    throw new EvalError(`expected at least ${procedure.params.length} arguments, got ${args.length}`);
  }

  const callEnv = new Environment(procedure.env);

  for (let index = 0; index < procedure.params.length; index += 1) {
    callEnv.define(procedure.params[index], args[index]);
  }

  if (procedure.restParam !== undefined) {
    callEnv.define(procedure.restParam, buildList(args.slice(procedure.params.length)));
  }

  return evaluateSequence(procedure.body, callEnv, context);
}

function evaluateSequence(exprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  let result: RuntimeValue = VOID_VALUE;

  for (const expr of exprs) {
    result = evaluateExpr(expr, env, context);
  }

  return result;
}

function applyBuiltin(name: BuiltinName, args: RuntimeValue[], context: EvalContext): RuntimeValue {
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
      return makeBoolean(!isTruthy(args[0]));
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
      return makeBoolean(args[0].kind === 'nil');
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
    case 'display':
      if (args.length !== 1) {
        throw new EvalError('display expects exactly 1 argument');
      }
      context.output.push(formatDisplayValue(args[0]));
      return VOID_VALUE;
    case 'write':
      if (args.length !== 1) {
        throw new EvalError('write expects exactly 1 argument');
      }
      context.output.push(formatValue(args[0]));
      return VOID_VALUE;
    case 'newline':
      if (args.length !== 0) {
        throw new EvalError('newline expects exactly 0 arguments');
      }
      context.output.push('\n');
      return VOID_VALUE;
    case 'string-append':
      return applyStringAppend(args);
    case 'string-length':
      if (args.length !== 1) {
        throw new EvalError('string-length expects exactly 1 argument');
      }
      return makeNumber(stringChars(expectStringValue(args[0], 'string-length').value).length);
    case 'substring':
      return applySubstring(args);
    case 'string->number':
      if (args.length !== 1) {
        throw new EvalError('string->number expects exactly 1 argument');
      }
      return parseNumberString(expectStringValue(args[0], 'string->number').value);
    case 'number->string':
      if (args.length !== 1) {
        throw new EvalError('number->string expects exactly 1 argument');
      }
      return makeString(formatNumber(expectNumber(args[0], 'number->string')));
    case 'symbol->string':
      if (args.length !== 1) {
        throw new EvalError('symbol->string expects exactly 1 argument');
      }
      return makeString(expectSymbol(args[0], 'symbol->string').name);
    case 'string->symbol':
      if (args.length !== 1) {
        throw new EvalError('string->symbol expects exactly 1 argument');
      }
      return { kind: 'symbol', name: expectStringValue(args[0], 'string->symbol').value };
    case 'string-ref':
      return applyStringRef(args);
    case 'string-copy':
      if (args.length !== 1) {
        throw new EvalError('string-copy expects exactly 1 argument');
      }
      return makeString(expectStringValue(args[0], 'string-copy').value);
    case 'string-set!':
      return applyStringSet(args);
    case 'char?':
      return applyTypePredicate(args, 'char?', (value) => value.kind === 'char');
    case 'apply':
      return applyApply(args, context);
  }
}

function applyApply(args: RuntimeValue[], context: EvalContext): RuntimeValue {
  if (args.length < 2) {
    throw new EvalError('apply expects at least 2 arguments');
  }

  const procedure = args[0];
  const prefixArgs = args.slice(1, -1);
  const tailArgs = listToArray(args[args.length - 1], 'apply');
  return applyProcedure(procedure, [...prefixArgs, ...tailArgs], context);
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
      return makeBoolean(false);
    }
  }

  return makeBoolean(true);
}

function applyStringAppend(args: RuntimeValue[]): RuntimeValue {
  return makeString(args.map((arg) => expectStringValue(arg, 'string-append').value).join(''));
}

function applySubstring(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 3) {
    throw new EvalError('substring expects exactly 3 arguments');
  }

  const chars = stringChars(expectStringValue(args[0], 'substring').value);
  const start = expectIndex(args[1], 'substring');
  const end = expectIndex(args[2], 'substring');

  if (start > end || end > chars.length) {
    throw new EvalError('substring indices out of range');
  }

  return makeString(chars.slice(start, end).join(''));
}

function applyStringRef(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError('string-ref expects exactly 2 arguments');
  }

  const chars = stringChars(expectStringValue(args[0], 'string-ref').value);
  const index = expectIndex(args[1], 'string-ref');

  if (index >= chars.length) {
    throw new EvalError('string-ref index out of range');
  }

  return makeChar(chars[index]);
}

function applyStringSet(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 3) {
    throw new EvalError('string-set! expects exactly 3 arguments');
  }

  const target = expectStringValue(args[0], 'string-set!');
  const index = expectIndex(args[1], 'string-set!');
  const char = expectChar(args[2], 'string-set!');
  const chars = stringChars(target.value);

  if (!target.mutable) {
    throw new EvalError('string-set! expects a mutable string');
  }

  if (index >= chars.length) {
    throw new EvalError('string-set! index out of range');
  }

  chars[index] = char.value;
  target.value = chars.join('');
  return VOID_VALUE;
}

function expectNumber(value: RuntimeValue, procedure: string): number {
  if (value.kind !== 'number') {
    throw new EvalError(`${procedure} expects numeric arguments`);
  }
  return value.value;
}

function expectStringValue(value: RuntimeValue, procedure: string): StringValue {
  if (value.kind !== 'string') {
    throw new EvalError(`${procedure} expects string arguments`);
  }
  return value;
}

function expectChar(value: RuntimeValue, procedure: string): CharValue {
  if (value.kind !== 'char') {
    throw new EvalError(`${procedure} expects a character`);
  }
  return value;
}

function expectSymbol(value: RuntimeValue, procedure: string): SymbolValue {
  if (value.kind !== 'symbol') {
    throw new EvalError(`${procedure} expects a symbol`);
  }
  return value;
}

function expectIndex(value: RuntimeValue, procedure: string): number {
  const numericValue = expectNumber(value, procedure);
  if (!Number.isInteger(numericValue) || numericValue < 0) {
    throw new EvalError(`${procedure} expects a non-negative integer index`);
  }
  return numericValue;
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

  return makeBoolean(predicate(args[0]));
}

function isTruthy(value: RuntimeValue): boolean {
  return value.kind !== 'boolean' || value.value;
}

function makeNumber(value: number): NumberExpr {
  return {
    kind: 'number',
    pos: DEFAULT_SOURCE_POS,
    value: Object.is(value, -0) ? 0 : value,
  };
}

function makeBoolean(value: boolean): BooleanExpr {
  return {
    kind: 'boolean',
    pos: DEFAULT_SOURCE_POS,
    value,
  };
}

function makeString(value: string, mutable = true): StringValue {
  return {
    kind: 'string',
    value,
    mutable,
  };
}

function makeChar(value: string): CharValue {
  if (stringChars(value).length !== 1) {
    throw new EvalError('character values must contain exactly 1 character');
  }

  return {
    kind: 'char',
    value,
  };
}

function formatValue(value: RuntimeValue): string {
  return formatValueWithMode(value, 'write');
}

function formatDisplayValue(value: RuntimeValue): string {
  return formatValueWithMode(value, 'display');
}

function formatValueWithMode(value: RuntimeValue, mode: 'write' | 'display'): string {
  switch (value.kind) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return mode === 'display' ? value.value : `"${escapeString(value.value)}"`;
    case 'char':
      return mode === 'display' ? value.value : formatChar(value.value);
    case 'symbol':
      return value.name;
    case 'nil':
      return '()';
    case 'pair':
      return formatPair(value, mode);
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

function formatPair(value: PairValue, mode: 'write' | 'display'): string {
  const parts: string[] = [];
  let current: RuntimeValue = value;

  while (current.kind === 'pair') {
    parts.push(formatValueWithMode(current.car, mode));
    current = current.cdr;
  }

  if (current.kind === 'nil') {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatValueWithMode(current, mode)})`;
}

function formatChar(value: string): string {
  switch (value) {
    case ' ':
      return '#\\space';
    case '\n':
      return '#\\newline';
    default:
      return `#\\${value}`;
  }
}

function stringChars(value: string): string[] {
  return Array.from(value);
}

function parseNumberString(value: string): RuntimeValue {
  const trimmed = value.trim();
  if (/^[+-]?(?:\d+|\d+\.\d+|\.\d+)$/.test(trimmed)) {
    return makeNumber(Number(trimmed));
  }

  return makeBoolean(false);
}

function attachPosition(error: unknown, pos: SourcePos): EvalError {
  if (error instanceof EvalError) {
    return error.withPosition(pos);
  }

  if (error instanceof Error) {
    return new EvalError(error.message, pos);
  }

  return new EvalError(String(error), pos);
}
