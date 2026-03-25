import { EvalError, type SourcePosition } from './evalError.js';

type Token =
  | { kind: 'paren'; value: '(' | ')'; position: SourcePosition }
  | { kind: 'atom'; value: string; position: SourcePosition }
  | { kind: 'string'; value: string; position: SourcePosition }
  | { kind: 'quote'; position: SourcePosition };

type Expr =
  | { type: 'number'; value: number; position: SourcePosition }
  | { type: 'boolean'; value: boolean; position: SourcePosition }
  | { type: 'string'; value: string; position: SourcePosition }
  | { type: 'symbol'; name: string; position: SourcePosition }
  | { type: 'list'; elements: Expr[]; position: SourcePosition };

type EvaluatedArg = {
  value: SchemeValue;
  position: SourcePosition;
};

type BuiltinProcedure = {
  type: 'builtin';
  name: string;
  invoke: (args: EvaluatedArg[], callPosition: SourcePosition) => SchemeValue;
};

type Closure = {
  type: 'closure';
  params: string[];
  body: Expr[];
  env: Environment;
};

type SchemeValue =
  | { type: 'number'; value: number }
  | { type: 'boolean'; value: boolean }
  | { type: 'string'; value: string }
  | { type: 'symbol'; name: string }
  | { type: 'list'; elements: SchemeValue[] }
  | BuiltinProcedure
  | Closure
  | { type: 'void' };

const START_POSITION: SourcePosition = { line: 1, column: 1 };
const VOID_VALUE: SchemeValue = { type: 'void' };

class Environment {
  private readonly bindings = new Map<string, SchemeValue>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: SchemeValue): void {
    this.bindings.set(name, value);
  }

  lookup(name: string, position: SourcePosition): SchemeValue {
    const value = this.bindings.get(name);
    if (value !== undefined) {
      return value;
    }

    if (this.parent) {
      return this.parent.lookup(name, position);
    }

    throw new EvalError(`unbound variable: ${name}`, position);
  }
}

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('empty input', START_POSITION);
  }

  const env = createGlobalEnv();
  let result: SchemeValue = VOID_VALUE;

  for (const expr of expressions) {
    result = evaluate(expr, env);
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

function parseProgram(input: string): Expr[] {
  const { tokens, eofPosition } = tokenize(input);
  let index = 0;
  const expressions: Expr[] = [];

  while (index < tokens.length) {
    expressions.push(parseExpr());
  }

  return expressions;

  function parseExpr(): Expr {
    const token = tokens[index];
    if (!token) {
      throw new EvalError('unexpected end of input', eofPosition);
    }

    index += 1;

    if (token.kind === 'paren') {
      if (token.value === ')') {
        throw new EvalError('unexpected )', token.position);
      }

      const elements: Expr[] = [];
      while (index < tokens.length) {
        const next = tokens[index];
        if (next.kind === 'paren' && next.value === ')') {
          index += 1;
          return { type: 'list', elements, position: token.position };
        }

        elements.push(parseExpr());
      }

      throw new EvalError('missing )', eofPosition);
    }

    if (token.kind === 'quote') {
      return {
        type: 'list',
        elements: [
          { type: 'symbol', name: 'quote', position: token.position },
          parseExpr(),
        ],
        position: token.position,
      };
    }

    if (token.kind === 'string') {
      return { type: 'string', value: token.value, position: token.position };
    }

    if (token.value === '#t') {
      return { type: 'boolean', value: true, position: token.position };
    }

    if (token.value === '#f') {
      return { type: 'boolean', value: false, position: token.position };
    }

    if (/^-?\d+$/.test(token.value)) {
      return { type: 'number', value: Number(token.value), position: token.position };
    }

    return { type: 'symbol', name: token.value, position: token.position };
  }
}

function tokenize(input: string): { tokens: Token[]; eofPosition: SourcePosition } {
  const tokens: Token[] = [];
  let index = 0;
  let line = 1;
  let column = 1;

  const currentPosition = (): SourcePosition => ({ line, column });

  const advanceChar = (ch: string): void => {
    index += 1;
    if (ch === '\n') {
      line += 1;
      column = 1;
      return;
    }

    column += 1;
  };

  while (index < input.length) {
    const ch = input[index];

    if (/\s/.test(ch)) {
      advanceChar(ch);
      continue;
    }

    if (ch === ';') {
      while (index < input.length && input[index] !== '\n') {
        advanceChar(input[index]);
      }
      continue;
    }

    const position = currentPosition();

    if (ch === '(' || ch === ')') {
      tokens.push({ kind: 'paren', value: ch, position });
      advanceChar(ch);
      continue;
    }

    if (ch === "'") {
      tokens.push({ kind: 'quote', position });
      advanceChar(ch);
      continue;
    }

    if (ch === '"') {
      advanceChar(ch);
      let value = '';
      let terminated = false;

      while (index < input.length) {
        const current = input[index];

        if (current === '"') {
          advanceChar(current);
          tokens.push({ kind: 'string', value, position });
          terminated = true;
          break;
        }

        if (current === '\\') {
          advanceChar(current);
          if (index >= input.length) {
            throw new EvalError('unterminated string literal', position);
          }

          const escaped = input[index];
          switch (escaped) {
            case 'n':
              value += '\n';
              break;
            case 'r':
              value += '\r';
              break;
            case 't':
              value += '\t';
              break;
            case '"':
              value += '"';
              break;
            case '\\':
              value += '\\';
              break;
            default:
              value += escaped;
              break;
          }

          advanceChar(escaped);
          continue;
        }

        value += current;
        advanceChar(current);
      }

      if (!terminated) {
        throw new EvalError('unterminated string literal', position);
      }

      continue;
    }

    let value = '';
    while (index < input.length) {
      const current = input[index];
      if (/\s/.test(current) || current === '(' || current === ')' || current === ';') {
        break;
      }

      value += current;
      advanceChar(current);
    }

    tokens.push({ kind: 'atom', value, position });
  }

  return { tokens, eofPosition: currentPosition() };
}

function evaluate(expr: Expr, env: Environment): SchemeValue {
  try {
    switch (expr.type) {
      case 'number':
      case 'boolean':
      case 'string':
        return expr;
      case 'symbol':
        return env.lookup(expr.name, expr.position);
      case 'list':
        return evaluateList(expr, env);
    }
  } catch (error) {
    throw attachPosition(error, expr.position);
  }
}

function evaluateList(expr: Extract<Expr, { type: 'list' }>, env: Environment): SchemeValue {
  if (expr.elements.length === 0) {
    throw new EvalError('cannot evaluate empty list', expr.position);
  }

  const operator = expr.elements[0];
  const args = expr.elements.slice(1);

  if (operator.type === 'symbol') {
    switch (operator.name) {
      case 'define':
        return evaluateDefine(args, env, operator.position);
      case 'if':
        return evaluateIf(args, env, operator.position);
      case 'quote':
        return evaluateQuote(args, operator.position);
      case 'lambda':
        return evaluateLambda(args, env, operator.position);
      case 'and':
        return evaluateAnd(args, env);
      case 'or':
        return evaluateOr(args, env);
      case 'begin':
        return evaluateBegin(args, env);
      case 'let':
        return evaluateLet(args, env, operator.position);
      case 'cond':
        return evaluateCond(args, env);
    }
  }

  const procedure = evaluate(operator, env);
  const evaluatedArgs = args.map((arg) => ({ value: evaluate(arg, env), position: arg.position }));
  return applyProcedure(procedure, evaluatedArgs, operator.position);
}

function evaluateDefine(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  if (args.length < 2) {
    throw new EvalError(`define: expected at least 2 argument(s), got ${args.length}`, position);
  }

  const target = args[0];

  if (target.type === 'symbol') {
    requireArgCount('define', args.length, 2, position);
    const value = evaluate(args[1], env);
    env.define(target.name, value);
    return VOID_VALUE;
  }

  if (target.type !== 'list' || target.elements.length === 0) {
    throw new EvalError('define: invalid binding target', target.position);
  }

  const nameExpr = target.elements[0];
  if (nameExpr.type !== 'symbol') {
    throw new EvalError('define: invalid function name', nameExpr.position);
  }

  const params = parseParameterNames(target.elements.slice(1));
  const body = args.slice(1);
  const closure: Closure = { type: 'closure', params, body, env };
  env.define(nameExpr.name, closure);
  return VOID_VALUE;
}

function evaluateIf(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('if', args.length, 3, position);
  return isTruthy(evaluate(args[0], env)) ? evaluate(args[1], env) : evaluate(args[2], env);
}

function evaluateQuote(args: Expr[], position: SourcePosition): SchemeValue {
  requireArgCount('quote', args.length, 1, position);
  return quoteExpr(args[0]);
}

function evaluateLambda(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('lambda', args.length, 2, position);
  const paramsExpr = args[0];
  if (paramsExpr.type !== 'list') {
    throw new EvalError('lambda: parameter list must be a list', paramsExpr.position);
  }

  return {
    type: 'closure',
    params: parseParameterNames(paramsExpr.elements),
    body: args.slice(1),
    env,
  };
}

function evaluateAnd(args: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = booleanValue(true);

  for (const arg of args) {
    result = evaluate(arg, env);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateOr(args: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = booleanValue(false);

  for (const arg of args) {
    result = evaluate(arg, env);
    if (isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evaluateBegin(args: Expr[], env: Environment): SchemeValue {
  return evaluateSequence(args, env);
}

function evaluateLet(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('let', args.length, 2, position);

  const firstArg = args[0];
  if (firstArg.type === 'symbol') {
    requireArgCountAtLeast('let', args.length, 3, position);

    const bindings = parseLetBindings(args[1]);
    const values = bindings.map((binding) => ({
      value: evaluate(binding.value, env),
      position: binding.value.position,
    }));
    const letEnv = new Environment(env);
    const closure: Closure = {
      type: 'closure',
      params: bindings.map((binding) => binding.name),
      body: args.slice(2),
      env: letEnv,
    };

    letEnv.define(firstArg.name, closure);
    return applyProcedure(closure, values, firstArg.position);
  }

  const bindings = parseLetBindings(firstArg);
  const letEnv = new Environment(env);

  for (const binding of bindings) {
    letEnv.define(binding.name, evaluate(binding.value, env));
  }

  return evaluateSequence(args.slice(1), letEnv);
}

function evaluateCond(args: Expr[], env: Environment): SchemeValue {
  for (const clause of args) {
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError('cond: expected non-empty clause', clause.position);
    }

    const [testExpr, ...body] = clause.elements;

    if (testExpr.type === 'symbol' && testExpr.name === 'else') {
      return body.length === 0 ? VOID_VALUE : evaluateSequence(body, env);
    }

    const testValue = evaluate(testExpr, env);
    if (isTruthy(testValue)) {
      return body.length === 0 ? testValue : evaluateSequence(body, env);
    }
  }

  return VOID_VALUE;
}

function quoteExpr(expr: Expr): SchemeValue {
  switch (expr.type) {
    case 'number':
    case 'boolean':
    case 'string':
      return expr;
    case 'symbol':
      return { type: 'symbol', name: expr.name };
    case 'list':
      return { type: 'list', elements: expr.elements.map(quoteExpr) };
  }
}

function parseParameterNames(params: Expr[]): string[] {
  return params.map((param) => {
    if (param.type !== 'symbol') {
      throw new EvalError('lambda: parameter names must be symbols', param.position);
    }

    return param.name;
  });
}

function parseLetBindings(bindingsExpr: Expr): Array<{ name: string; value: Expr }> {
  if (bindingsExpr.type !== 'list') {
    throw new EvalError('let: expected binding list', bindingsExpr.position);
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError('let: expected binding pair', bindingExpr.position);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    if (nameExpr.type !== 'symbol') {
      throw new EvalError('let: binding name must be a symbol', nameExpr.position);
    }

    return { name: nameExpr.name, value: valueExpr };
  });
}

function applyProcedure(
  value: SchemeValue,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
): SchemeValue {
  switch (value.type) {
    case 'builtin':
      return value.invoke(args, callPosition);
    case 'closure': {
      requireArgCount('lambda', args.length, value.params.length, callPosition);
      const callEnv = new Environment(value.env);
      for (let index = 0; index < value.params.length; index += 1) {
        callEnv.define(value.params[index], args[index].value);
      }

      return evaluateSequence(value.body, callEnv);
    }
    default:
      throw new EvalError('attempted to call a non-procedure', callPosition);
  }
}

function evaluateSequence(expressions: Expr[], env: Environment): SchemeValue {
  let result: SchemeValue = VOID_VALUE;

  for (const expr of expressions) {
    result = evaluate(expr, env);
  }

  return result;
}

function createGlobalEnv(): Environment {
  const env = new Environment();

  env.define(
    '+',
    builtin('+', (args, callPosition) =>
      numberValue(
        evaluateNumberArgs('+', args).reduce((sum, value) => sum + value, 0),
        callPosition,
      ),
    ),
  );

  env.define(
    '*',
    builtin('*', (args, callPosition) =>
      numberValue(
        evaluateNumberArgs('*', args).reduce((product, value) => product * value, 1),
        callPosition,
      ),
    ),
  );

  env.define(
    '-',
    builtin('-', (args, callPosition) => {
      const values = evaluateNumberArgs('-', args);
      requireArgCountAtLeast('-', values.length, 1, callPosition);
      if (values.length === 1) {
        return numberValue(-values[0], callPosition);
      }

      return numberValue(
        values.slice(1).reduce((result, value) => result - value, values[0]),
        callPosition,
      );
    }),
  );

  env.define(
    '/',
    builtin('/', (args, callPosition) => {
      const values = evaluateNumberArgs('/', args);
      requireArgCountAtLeast('/', values.length, 1, callPosition);

      let result = values[0];
      if (values.length === 1) {
        if (result === 0) {
          throw new EvalError('division by zero', args[0].position);
        }

        return numberValue(1 / result, callPosition);
      }

      for (let index = 1; index < values.length; index += 1) {
        const value = values[index];
        if (value === 0) {
          throw new EvalError('division by zero', args[index].position);
        }

        result /= value;
      }

      return numberValue(result, callPosition);
    }),
  );

  env.define(
    '<',
    builtin('<', (args, callPosition) =>
      booleanValue(compareNumberArgs('<', args, (a, b) => a < b, callPosition)),
    ),
  );
  env.define(
    '>',
    builtin('>', (args, callPosition) =>
      booleanValue(compareNumberArgs('>', args, (a, b) => a > b, callPosition)),
    ),
  );
  env.define(
    '=',
    builtin('=', (args, callPosition) =>
      booleanValue(compareNumberArgs('=', args, (a, b) => a === b, callPosition)),
    ),
  );
  env.define(
    '<=',
    builtin('<=', (args, callPosition) =>
      booleanValue(compareNumberArgs('<=', args, (a, b) => a <= b, callPosition)),
    ),
  );

  env.define(
    'not',
    builtin('not', (args, callPosition) => {
      requireArgCount('not', args.length, 1, callPosition);
      return booleanValue(!isTruthy(args[0].value));
    }),
  );

  env.define(
    'cons',
    builtin('cons', (args, callPosition) => {
      requireArgCount('cons', args.length, 2, callPosition);
      const tail = expectList('cons', args[1]);
      return { type: 'list', elements: [args[0].value, ...tail.elements] };
    }),
  );

  env.define(
    'car',
    builtin('car', (args, callPosition) => {
      requireArgCount('car', args.length, 1, callPosition);
      return expectPair('car', args[0]).elements[0];
    }),
  );

  env.define(
    'cdr',
    builtin('cdr', (args, callPosition) => {
      requireArgCount('cdr', args.length, 1, callPosition);
      return { type: 'list', elements: expectPair('cdr', args[0]).elements.slice(1) };
    }),
  );

  env.define('list', builtin('list', (args) => ({ type: 'list', elements: args.map((arg) => arg.value) })));

  env.define(
    'length',
    builtin('length', (args, callPosition) => {
      requireArgCount('length', args.length, 1, callPosition);
      return numberValue(expectList('length', args[0]).elements.length, callPosition);
    }),
  );

  env.define(
    'append',
    builtin('append', (args) => {
      const elements: SchemeValue[] = [];

      for (const arg of args) {
        elements.push(...expectList('append', arg).elements);
      }

      return { type: 'list', elements };
    }),
  );

  env.define(
    'null?',
    builtin('null?', (args, callPosition) => {
      requireArgCount('null?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'list' && args[0].value.elements.length === 0);
    }),
  );

  env.define(
    'pair?',
    builtin('pair?', (args, callPosition) => {
      requireArgCount('pair?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'list' && args[0].value.elements.length > 0);
    }),
  );

  env.define(
    'string?',
    builtin('string?', (args, callPosition) => {
      requireArgCount('string?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'string');
    }),
  );

  env.define(
    'number?',
    builtin('number?', (args, callPosition) => {
      requireArgCount('number?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number');
    }),
  );

  env.define(
    'boolean?',
    builtin('boolean?', (args, callPosition) => {
      requireArgCount('boolean?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'boolean');
    }),
  );

  env.define(
    'symbol?',
    builtin('symbol?', (args, callPosition) => {
      requireArgCount('symbol?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'symbol');
    }),
  );

  return env;
}

function builtin(
  name: string,
  invoke: (args: EvaluatedArg[], callPosition: SourcePosition) => SchemeValue,
): BuiltinProcedure {
  return { type: 'builtin', name, invoke };
}

function evaluateNumberArgs(name: string, args: EvaluatedArg[]): number[] {
  return args.map((arg) => expectNumber(name, arg));
}

function compareNumberArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (left: number, right: number) => boolean,
  position: SourcePosition,
): boolean {
  const values = evaluateNumberArgs(name, args);
  requireArgCountAtLeast(name, values.length, 1, position);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function requireArgCount(
  name: string,
  actual: number,
  expected: number,
  position: SourcePosition,
): void {
  if (actual !== expected) {
    throw new EvalError(`${name}: expected ${expected} argument(s), got ${actual}`, position);
  }
}

function requireArgCountAtLeast(
  name: string,
  actual: number,
  minimum: number,
  position: SourcePosition,
): void {
  if (actual < minimum) {
    throw new EvalError(`${name}: expected at least ${minimum} argument(s), got ${actual}`, position);
  }
}

function expectNumber(name: string, arg: EvaluatedArg): number {
  if (arg.value.type !== 'number') {
    throw new EvalError(`${name}: expected number`, arg.position);
  }

  return arg.value.value;
}

function expectList(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'list' }> {
  if (arg.value.type !== 'list') {
    throw new EvalError(`${name}: expected list`, arg.position);
  }

  return arg.value;
}

function expectPair(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'list' }> {
  const list = expectList(name, arg);
  if (list.elements.length === 0) {
    throw new EvalError(`${name}: expected non-empty list`, arg.position);
  }

  return list;
}

function isTruthy(value: SchemeValue): boolean {
  return value.type !== 'boolean' || value.value;
}

function numberValue(value: number, position?: SourcePosition): SchemeValue {
  if (!Number.isFinite(value)) {
    throw new EvalError('invalid number', position);
  }

  return { type: 'number', value };
}

function booleanValue(value: boolean): SchemeValue {
  return { type: 'boolean', value };
}

function attachPosition(error: unknown, position: SourcePosition): EvalError {
  if (error instanceof EvalError) {
    return error.position ? error : new EvalError(error.rawMessage, position);
  }

  if (error instanceof Error) {
    return new EvalError(error.message, position);
  }

  return new EvalError(String(error), position);
}

function formatValue(value: SchemeValue): string {
  switch (value.type) {
    case 'number':
      return String(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return JSON.stringify(value.value);
    case 'symbol':
      return value.name;
    case 'list':
      return `(${value.elements.map(formatValue).join(' ')})`;
    case 'builtin':
    case 'closure':
      return '#<procedure>';
    case 'void':
      return '#<void>';
  }
}
