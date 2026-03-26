import { EvalError } from './evalError.js';

type Expr = NumberExpr | BooleanExpr | StringExpr | CharExpr | SymbolExpr | ListExpr;

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

interface CharExpr extends SourceLoc {
  type: 'char';
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
  chars: string[];
  mutable: boolean;
}

interface SchemeSymbol {
  kind: 'symbol';
  value: string;
}

interface SchemeChar {
  kind: 'char';
  value: string;
}

interface EmptyListValue {
  kind: 'empty-list';
}

interface PairValue {
  kind: 'pair';
  car: Value;
  cdr: Value;
}

interface VoidValue {
  kind: 'void';
}

interface EvaluatedArg {
  expr: Expr;
  value: Value;
}

interface LetBinding {
  name: string;
  valueExpr: Expr;
}

interface BuiltinProcedure {
  kind: 'procedure';
  name: string;
  call(args: EvaluatedArg[], loc: SourceLoc): Value;
}

interface ClosureProcedure {
  kind: 'procedure';
  name?: string;
  params: string[];
  body: Expr[];
  env: Environment;
}

type ProcedureValue = BuiltinProcedure | ClosureProcedure;

type Value =
  | number
  | boolean
  | SchemeString
  | SchemeSymbol
  | SchemeChar
  | EmptyListValue
  | PairValue
  | VoidValue
  | ProcedureValue;

const EMPTY_LIST: EmptyListValue = { kind: 'empty-list' };
const VOID_VALUE: VoidValue = { kind: 'void' };

class Runtime {
  private readonly output: string[] = [];

  write(value: string): void {
    this.output.push(value);
  }

  readOutput(): string {
    return this.output.join('');
  }
}

class Environment {
  private readonly bindings = new Map<string, Value>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: Value): void {
    this.bindings.set(name, value);
  }

  lookup(name: string, loc: SourceLoc): Value {
    if (this.bindings.has(name)) {
      return this.bindings.get(name) as Value;
    }

    if (this.parent !== undefined) {
      return this.parent.lookup(name, loc);
    }

    throw new EvalError(`${loc.line}:${loc.col}: unbound variable ${name}`);
  }
}

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

    if (ch === '\'') {
      return this.parseQuoted(loc);
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

  private parseQuoted(loc: SourceLoc): ListExpr {
    this.advance();

    return {
      type: 'list',
      elements: [
        { type: 'symbol', value: 'quote', ...loc },
        this.parseExpr(),
      ],
      ...loc,
    };
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
      if (isWhitespace(ch) || ch === '(' || ch === ')' || ch === '\'' || ch === ';') {
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

    if (token.startsWith('#\\')) {
      return { type: 'char', value: parseCharLiteral(token, loc), ...loc };
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
  return formatValue(evaluateProgram(input).result);
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const { result, output } = evaluateProgram(input);
  return { result: formatValue(result), output };
}

function evaluateProgram(input: string): { result: Value; output: string } {
  const program = new Reader(input).parseProgram();

  if (program.length === 0) {
    throw new EvalError('1:1: expected expression');
  }

  const runtime = new Runtime();
  const env = createGlobalEnv(runtime);
  let result: Value = VOID_VALUE;

  for (const expr of program) {
    result = evaluate(expr, env);
  }

  return { result, output: runtime.readOutput() };
}

function createGlobalEnv(runtime: Runtime): Environment {
  const env = new Environment();

  env.define('+', builtin('+', (args) => {
    let result = 0;
    for (const arg of args) {
      result += expectNumber(arg);
    }
    return result;
  }));

  env.define('*', builtin('*', (args) => {
    let result = 1;
    for (const arg of args) {
      result *= expectNumber(arg);
    }
    return result;
  }));

  env.define('-', builtin('-', (args, loc) => {
    if (args.length === 0) {
      throw new EvalError(`${loc.line}:${loc.col}: - expects at least 1 argument`);
    }

    const first = expectNumber(args[0]);
    if (args.length === 1) {
      return -first;
    }

    let result = first;
    for (const arg of args.slice(1)) {
      result -= expectNumber(arg);
    }
    return result;
  }));

  env.define('/', builtin('/', (args, loc) => {
    if (args.length < 2) {
      throw new EvalError(`${loc.line}:${loc.col}: / expects at least 2 arguments`);
    }

    let result = expectNumber(args[0]);
    for (const arg of args.slice(1)) {
      const value = expectNumber(arg);
      if (value === 0) {
        throw new EvalError(`${arg.expr.line}:${arg.expr.col}: division by zero`);
      }
      result /= value;
    }
    return result;
  }));

  env.define('<', comparisonBuiltin('<', (left, right) => left < right));
  env.define('>', comparisonBuiltin('>', (left, right) => left > right));
  env.define('=', comparisonBuiltin('=', (left, right) => left === right));
  env.define('<=', comparisonBuiltin('<=', (left, right) => left <= right));

  env.define('not', builtin('not', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: not expects exactly 1 argument`);
    }

    return !isTruthy(args[0].value);
  }));

  env.define('cons', builtin('cons', (args, loc) => {
    if (args.length !== 2) {
      throw new EvalError(`${loc.line}:${loc.col}: cons expects exactly 2 arguments`);
    }

    return { kind: 'pair', car: args[0].value, cdr: args[1].value };
  }));

  env.define('car', builtin('car', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: car expects exactly 1 argument`);
    }

    return expectPairArg(args[0]).car;
  }));

  env.define('cdr', builtin('cdr', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: cdr expects exactly 1 argument`);
    }

    return expectPairArg(args[0]).cdr;
  }));

  env.define('null?', builtin('null?', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: null? expects exactly 1 argument`);
    }

    return isEmptyList(args[0].value);
  }));

  env.define('list', builtin('list', (args) => makeList(args.map((arg) => arg.value))));

  env.define('length', builtin('length', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: length expects exactly 1 argument`);
    }

    return expectProperList(args[0].value, args[0].expr).length;
  }));

  env.define('append', builtin('append', (args) => {
    if (args.length === 0) {
      return EMPTY_LIST;
    }

    let result = args[args.length - 1].value;

    for (let index = args.length - 2; index >= 0; index -= 1) {
      const elements = expectProperList(args[index].value, args[index].expr);
      for (let elementIndex = elements.length - 1; elementIndex >= 0; elementIndex -= 1) {
        result = { kind: 'pair', car: elements[elementIndex], cdr: result };
      }
    }

    return result;
  }));

  env.define('string?', predicateBuiltin('string?', isSchemeStringValue));
  env.define('number?', predicateBuiltin('number?', (value) => typeof value === 'number'));
  env.define('boolean?', predicateBuiltin('boolean?', (value) => typeof value === 'boolean'));
  env.define('pair?', predicateBuiltin('pair?', isPair));
  env.define('symbol?', predicateBuiltin('symbol?', isSchemeSymbolValue));
  env.define('char?', predicateBuiltin('char?', isSchemeCharValue));

  env.define('display', builtin('display', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: display expects exactly 1 argument`);
    }

    runtime.write(formatDisplayValue(args[0].value));
    return VOID_VALUE;
  }));

  env.define('write', builtin('write', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: write expects exactly 1 argument`);
    }

    runtime.write(formatValue(args[0].value));
    return VOID_VALUE;
  }));

  env.define('newline', builtin('newline', (args, loc) => {
    if (args.length !== 0) {
      throw new EvalError(`${loc.line}:${loc.col}: newline expects exactly 0 arguments`);
    }

    runtime.write('\n');
    return VOID_VALUE;
  }));

  env.define('string-append', builtin('string-append', (args) => ({
    kind: 'string',
    chars: Array.from(args.map(expectString).join('')),
    mutable: true,
  })));

  env.define('string-length', builtin('string-length', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: string-length expects exactly 1 argument`);
    }

    return stringChars(expectStringArg(args[0])).length;
  }));

  env.define('substring', builtin('substring', (args, loc) => {
    if (args.length !== 3) {
      throw new EvalError(`${loc.line}:${loc.col}: substring expects exactly 3 arguments`);
    }

    const value = expectStringArg(args[0]);
    const chars = stringChars(value);
    const start = expectIndexArg(args[1]);
    const end = expectIndexArg(args[2]);

    if (start < 0 || end < start || end > chars.length) {
      throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: substring indices out of bounds`);
    }

    return makeString(chars.slice(start, end).join(''));
  }));

  env.define('string->number', builtin('string->number', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: string->number expects exactly 1 argument`);
    }

    return parseStringNumber(expectStringArg(args[0]));
  }));

  env.define('number->string', builtin('number->string', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: number->string expects exactly 1 argument`);
    }

    return makeString(formatNumber(expectNumber(args[0])));
  }));

  env.define('symbol->string', builtin('symbol->string', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: symbol->string expects exactly 1 argument`);
    }

    return makeString(expectSymbolArg(args[0]).value);
  }));

  env.define('string->symbol', builtin('string->symbol', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: string->symbol expects exactly 1 argument`);
    }

    return {
      kind: 'symbol',
      value: expectStringArg(args[0]),
    };
  }));

  env.define('string-ref', builtin('string-ref', (args, loc) => {
    if (args.length !== 2) {
      throw new EvalError(`${loc.line}:${loc.col}: string-ref expects exactly 2 arguments`);
    }

    const chars = stringChars(expectStringArg(args[0]));
    const index = expectIndexArg(args[1]);

    if (index < 0 || index >= chars.length) {
      throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: string-ref index out of bounds`);
    }

    return { kind: 'char', value: chars[index] };
  }));

  env.define('string-copy', builtin('string-copy', (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: string-copy expects exactly 1 argument`);
    }

    const value = expectStringValue(args[0]);
    return { kind: 'string', chars: [...value.chars], mutable: true };
  }));

  env.define('string-set!', builtin('string-set!', (args, loc) => {
    if (args.length !== 3) {
      throw new EvalError(`${loc.line}:${loc.col}: string-set! expects exactly 3 arguments`);
    }

    const value = expectStringValue(args[0]);
    const index = expectIndexArg(args[1]);

    if (!value.mutable) {
      throw new EvalError(`${args[0].expr.line}:${args[0].expr.col}: immutable string`);
    }

    if (index < 0 || index >= value.chars.length) {
      throw new EvalError(`${args[1].expr.line}:${args[1].expr.col}: string-set! index out of bounds`);
    }

    value.chars[index] = expectCharArg(args[2]).value;
    return VOID_VALUE;
  }));

  return env;
}

function comparisonBuiltin(
  name: string,
  predicate: (left: number, right: number) => boolean,
): BuiltinProcedure {
  return builtin(name, (args, loc) => {
    if (args.length < 2) {
      throw new EvalError(`${loc.line}:${loc.col}: ${name} expects at least 2 arguments`);
    }

    for (let index = 0; index < args.length - 1; index += 1) {
      const left = expectNumber(args[index]);
      const right = expectNumber(args[index + 1]);
      if (!predicate(left, right)) {
        return false;
      }
    }

    return true;
  });
}

function predicateBuiltin(
  name: string,
  predicate: (value: Value) => boolean,
): BuiltinProcedure {
  return builtin(name, (args, loc) => {
    if (args.length !== 1) {
      throw new EvalError(`${loc.line}:${loc.col}: ${name} expects exactly 1 argument`);
    }

    return predicate(args[0].value);
  });
}

function builtin(
  name: string,
  call: (args: EvaluatedArg[], loc: SourceLoc) => Value,
): BuiltinProcedure {
  return { kind: 'procedure', name, call };
}

function evaluate(expr: Expr, env: Environment): Value {
  switch (expr.type) {
    case 'number':
    case 'boolean':
      return expr.value;
    case 'string':
      return makeString(expr.value);
    case 'char':
      return { kind: 'char', value: expr.value };
    case 'symbol':
      return env.lookup(expr.value, expr);
    case 'list':
      return evaluateList(expr, env);
  }
}

function evaluateList(expr: ListExpr, env: Environment): Value {
  if (expr.elements.length === 0) {
    throw new EvalError(`${expr.line}:${expr.col}: cannot evaluate empty list`);
  }

  const [head, ...args] = expr.elements;

  if (head.type === 'symbol') {
    switch (head.value) {
      case 'define':
        return evalDefine(args, head, env);
      case 'if':
        return evalIf(args, head, env);
      case 'quote':
        return evalQuote(args, head);
      case 'lambda':
        return evalLambda(args, head, env);
      case 'and':
        return evalAnd(args, env);
      case 'or':
        return evalOr(args, env);
      case 'begin':
        return evalBegin(args, env);
      case 'cond':
        return evalCond(args, head, env);
      case 'let':
        return evalLet(args, head, env);
    }
  }

  const operator = evaluate(head, env);
  const evaluatedArgs = args.map((arg) => ({ expr: arg, value: evaluate(arg, env) }));
  return applyProcedure(operator, evaluatedArgs, head);
}

function evalDefine(args: Expr[], head: SymbolExpr, env: Environment): Value {
  if (args.length < 2) {
    throw new EvalError(`${head.line}:${head.col}: define expects a name and value`);
  }

  const target = args[0];

  if (target.type === 'symbol') {
    if (args.length !== 2) {
      throw new EvalError(`${head.line}:${head.col}: define expects exactly 2 arguments`);
    }

    env.define(target.value, evaluate(args[1], env));
    return VOID_VALUE;
  }

  if (target.type !== 'list' || target.elements.length === 0) {
    throw new EvalError(`${head.line}:${head.col}: invalid define target`);
  }

  const [nameExpr, ...paramExprs] = target.elements;
  if (nameExpr.type !== 'symbol') {
    throw new EvalError(`${nameExpr.line}:${nameExpr.col}: function name must be a symbol`);
  }

  const params = paramExprs.map(expectParameterSymbol);
  const body = args.slice(1);
  const proc: ClosureProcedure = {
    kind: 'procedure',
    name: nameExpr.value,
    params,
    body,
    env,
  };

  env.define(nameExpr.value, proc);
  return VOID_VALUE;
}

function evalIf(args: Expr[], head: SymbolExpr, env: Environment): Value {
  if (args.length !== 3) {
    throw new EvalError(`${head.line}:${head.col}: if expects exactly 3 arguments`);
  }

  const condition = evaluate(args[0], env);
  if (isTruthy(condition)) {
    return evaluate(args[1], env);
  }

  return evaluate(args[2], env);
}

function evalQuote(args: Expr[], head: SymbolExpr): Value {
  if (args.length !== 1) {
    throw new EvalError(`${head.line}:${head.col}: quote expects exactly 1 argument`);
  }

  return quoteExpr(args[0]);
}

function evalLambda(args: Expr[], head: SymbolExpr, env: Environment): Value {
  if (args.length < 2) {
    throw new EvalError(`${head.line}:${head.col}: lambda expects parameters and a body`);
  }

  const paramsExpr = args[0];
  if (paramsExpr.type !== 'list') {
    throw new EvalError(`${paramsExpr.line}:${paramsExpr.col}: lambda parameters must be a list`);
  }

  return {
    kind: 'procedure',
    params: paramsExpr.elements.map(expectParameterSymbol),
    body: args.slice(1),
    env,
  };
}

function evalAnd(args: Expr[], env: Environment): Value {
  let result: Value = true;

  for (const arg of args) {
    result = evaluate(arg, env);
    if (!isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evalOr(args: Expr[], env: Environment): Value {
  let result: Value = false;

  for (const arg of args) {
    result = evaluate(arg, env);
    if (isTruthy(result)) {
      return result;
    }
  }

  return result;
}

function evalBegin(args: Expr[], env: Environment): Value {
  return evaluateSequence(args, env);
}

function evalCond(args: Expr[], head: SymbolExpr, env: Environment): Value {
  for (let index = 0; index < args.length; index += 1) {
    const clause = args[index];
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError(`${head.line}:${head.col}: cond clauses must be non-empty lists`);
    }

    const [testExpr, ...body] = clause.elements;
    const isElseClause = testExpr.type === 'symbol' && testExpr.value === 'else';

    if (isElseClause) {
      if (index !== args.length - 1) {
        throw new EvalError(`${testExpr.line}:${testExpr.col}: else must be the last cond clause`);
      }

      return evaluateSequence(body, env);
    }

    const testValue = evaluate(testExpr, env);
    if (isTruthy(testValue)) {
      if (body.length === 0) {
        return testValue;
      }

      return evaluateSequence(body, env);
    }
  }

  return VOID_VALUE;
}

function evalLet(args: Expr[], head: SymbolExpr, env: Environment): Value {
  if (args.length < 2) {
    throw new EvalError(`${head.line}:${head.col}: let expects bindings and a body`);
  }

  if (args[0].type === 'symbol') {
    return evalNamedLet(args, head, env);
  }

  const bindings = parseLetBindings(args[0], head);
  const body = args.slice(1);
  const letEnv = new Environment(env);

  for (const binding of bindings) {
    letEnv.define(binding.name, evaluate(binding.valueExpr, env));
  }

  return evaluateSequence(body, letEnv);
}

function evalNamedLet(args: Expr[], head: SymbolExpr, env: Environment): Value {
  if (args.length < 3) {
    throw new EvalError(`${head.line}:${head.col}: named let expects a name, bindings, and a body`);
  }

  const nameExpr = args[0];
  if (nameExpr.type !== 'symbol') {
    throw new EvalError(`${nameExpr.line}:${nameExpr.col}: named let name must be a symbol`);
  }

  const bindings = parseLetBindings(args[1], head);
  const evaluatedArgs = bindings.map((binding) => ({
    expr: binding.valueExpr,
    value: evaluate(binding.valueExpr, env),
  }));

  const letEnv = new Environment(env);
  const procedure: ClosureProcedure = {
    kind: 'procedure',
    name: nameExpr.value,
    params: bindings.map((binding) => binding.name),
    body: args.slice(2),
    env: letEnv,
  };

  letEnv.define(nameExpr.value, procedure);
  return applyProcedure(procedure, evaluatedArgs, nameExpr);
}

function applyProcedure(operator: Value, args: EvaluatedArg[], loc: SourceLoc): Value {
  if (!isProcedure(operator)) {
    throw new EvalError(`${loc.line}:${loc.col}: not a procedure`);
  }

  if (isBuiltinProcedure(operator)) {
    return operator.call(args, loc);
  }

  if (args.length !== operator.params.length) {
    throw new EvalError(
      `${loc.line}:${loc.col}: ${procedureDisplayName(operator)} expects exactly ${operator.params.length} arguments`,
    );
  }

  const callEnv = new Environment(operator.env);
  for (let index = 0; index < operator.params.length; index += 1) {
    callEnv.define(operator.params[index], args[index].value);
  }

  return evaluateSequence(operator.body, callEnv);
}

function quoteExpr(expr: Expr): Value {
  switch (expr.type) {
    case 'number':
    case 'boolean':
      return expr.value;
    case 'string':
      return makeString(expr.value);
    case 'char':
      return { kind: 'char', value: expr.value };
    case 'symbol':
      return { kind: 'symbol', value: expr.value };
    case 'list':
      return quoteList(expr.elements);
  }
}

function quoteList(elements: Expr[]): Value {
  return makeList(elements.map(quoteExpr));
}

function parseLetBindings(expr: Expr, head: SymbolExpr): LetBinding[] {
  if (expr.type !== 'list') {
    throw new EvalError(`${expr.line}:${expr.col}: let bindings must be a list`);
  }

  return expr.elements.map((bindingExpr) => {
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError(`${head.line}:${head.col}: let bindings must be pairs`);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    if (nameExpr.type !== 'symbol') {
      throw new EvalError(`${nameExpr.line}:${nameExpr.col}: let binding name must be a symbol`);
    }

    return { name: nameExpr.value, valueExpr };
  });
}

function evaluateSequence(exprs: Expr[], env: Environment): Value {
  let result: Value = VOID_VALUE;

  for (const expr of exprs) {
    result = evaluate(expr, env);
  }

  return result;
}

function makeList(elements: Value[]): Value {
  let result: Value = EMPTY_LIST;

  for (let index = elements.length - 1; index >= 0; index -= 1) {
    result = { kind: 'pair', car: elements[index], cdr: result };
  }

  return result;
}

function expectParameterSymbol(expr: Expr): string {
  if (expr.type !== 'symbol') {
    throw new EvalError(`${expr.line}:${expr.col}: parameter must be a symbol`);
  }

  return expr.value;
}

function expectNumber(arg: EvaluatedArg): number {
  if (typeof arg.value !== 'number') {
    throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected number`);
  }

  return arg.value;
}

function expectPairArg(arg: EvaluatedArg): PairValue {
  if (!isPair(arg.value)) {
    throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected pair`);
  }

  return arg.value;
}

function expectString(arg: EvaluatedArg): string {
  return schemeStringText(expectStringValue(arg));
}

function expectStringArg(arg: EvaluatedArg): string {
  return expectString(arg);
}

function expectStringValue(arg: EvaluatedArg): SchemeString {
  if (!isSchemeStringValue(arg.value)) {
    throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected string`);
  }

  return arg.value;
}

function expectSymbolArg(arg: EvaluatedArg): SchemeSymbol {
  if (!isSchemeSymbolValue(arg.value)) {
    throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected symbol`);
  }

  return arg.value;
}

function expectIndexArg(arg: EvaluatedArg): number {
  const value = expectNumber(arg);
  if (!Number.isInteger(value)) {
    throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected integer`);
  }

  return value;
}

function expectCharArg(arg: EvaluatedArg): SchemeChar {
  if (!isSchemeCharValue(arg.value)) {
    throw new EvalError(`${arg.expr.line}:${arg.expr.col}: expected char`);
  }

  return arg.value;
}

function expectProperList(value: Value, loc: SourceLoc): Value[] {
  const elements: Value[] = [];
  let current = value;

  while (isPair(current)) {
    elements.push(current.car);
    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${loc.line}:${loc.col}: expected proper list`);
  }

  return elements;
}

function isTruthy(value: Value): boolean {
  return value !== false;
}

function isProcedure(value: Value): value is ProcedureValue {
  return typeof value === 'object' && value !== null && value.kind === 'procedure';
}

function isBuiltinProcedure(value: ProcedureValue): value is BuiltinProcedure {
  return 'call' in value;
}

function isPair(value: Value): value is PairValue {
  return typeof value === 'object' && value !== null && value.kind === 'pair';
}

function isEmptyList(value: Value): value is EmptyListValue {
  return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}

function isSchemeStringValue(value: Value): value is SchemeString {
  return typeof value === 'object' && value !== null && value.kind === 'string';
}

function isSchemeSymbolValue(value: Value): value is SchemeSymbol {
  return typeof value === 'object' && value !== null && value.kind === 'symbol';
}

function isSchemeCharValue(value: Value): value is SchemeChar {
  return typeof value === 'object' && value !== null && value.kind === 'char';
}

function procedureDisplayName(proc: ClosureProcedure): string {
  return proc.name ?? 'lambda';
}

function makeString(value: string, mutable = true): SchemeString {
  return { kind: 'string', chars: Array.from(value), mutable };
}

function schemeStringText(value: SchemeString): string {
  return value.chars.join('');
}

function formatValue(value: Value): string {
  if (typeof value === 'number') {
    return formatNumber(value);
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  switch (value.kind) {
    case 'string':
      return `"${escapeString(schemeStringText(value))}"`;
    case 'symbol':
      return value.value;
    case 'char':
      return formatChar(value.value);
    case 'empty-list':
      return '()';
    case 'pair':
      return formatPair(value);
    case 'void':
      return '#<void>';
    case 'procedure':
      return '#<procedure>';
  }
}

function formatDisplayValue(value: Value): string {
  if (typeof value === 'number' || typeof value === 'boolean') {
    return formatValue(value);
  }

  switch (value.kind) {
    case 'string':
      return schemeStringText(value);
    case 'symbol':
      return value.value;
    case 'char':
      return value.value;
    case 'empty-list':
      return '()';
    case 'pair':
      return formatDisplayPair(value);
    case 'void':
      return '#<void>';
    case 'procedure':
      return '#<procedure>';
  }
}

function formatPair(value: PairValue): string {
  const parts: string[] = [];
  let tail: Value = value;

  while (isPair(tail)) {
    parts.push(formatValue(tail.car));
    tail = tail.cdr;
  }

  if (isEmptyList(tail)) {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatValue(tail)})`;
}

function formatDisplayPair(value: PairValue): string {
  const parts: string[] = [];
  let tail: Value = value;

  while (isPair(tail)) {
    parts.push(formatDisplayValue(tail.car));
    tail = tail.cdr;
  }

  if (isEmptyList(tail)) {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatDisplayValue(tail)})`;
}

function formatNumber(value: number): string {
  return Object.is(value, -0) ? '0' : String(value);
}

function formatChar(value: string): string {
  if (value === ' ') {
    return '#\\space';
  }

  if (value === '\n') {
    return '#\\newline';
  }

  return `#\\${value}`;
}

function escapeString(value: string): string {
  return value
    .replaceAll('\\', '\\\\')
    .replaceAll('"', '\\"')
    .replaceAll('\n', '\\n')
    .replaceAll('\t', '\\t');
}

function stringChars(value: string): string[] {
  return Array.from(value);
}

function parseCharLiteral(token: string, loc: SourceLoc): string {
  const body = token.slice(2);

  if (body.length === 1) {
    return body;
  }

  if (body === 'space') {
    return ' ';
  }

  if (body === 'newline') {
    return '\n';
  }

  throw new EvalError(`${loc.line}:${loc.col}: invalid character literal`);
}

function parseStringNumber(value: string): number | false {
  if (!/^[+-]?(?:\d+(?:\.\d+)?|\.\d+)$/.test(value)) {
    return false;
  }

  return Number(value);
}

function isWhitespace(ch: string): boolean {
  return ch === ' ' || ch === '\t' || ch === '\n' || ch === '\r';
}
