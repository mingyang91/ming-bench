import { EvalError, type SourcePos } from './evalError.js';
import {
  absNumber,
  addNumbers,
  compareNumbers,
  denominatorValue,
  divideNumbers,
  equalNumbers,
  exactToInexact,
  exptNumber,
  formatNumber as formatSchemeNumber,
  inexactToExact,
  integerDivision,
  isIntegerNumber,
  isRationalNumber,
  makeExactInteger,
  makeInexact,
  maxNumbers,
  minNumbers,
  multiplyNumbers,
  numeratorValue,
  numberToJsNumber,
  parseNumberLiteral,
  parseNumberStringValue,
  sameNumericSyntax,
  subtractNumbers,
  type SchemeNumber,
} from './numbers.js';

type ExprBase = { pos: SourcePos };

type NumberExpr = ExprBase & { kind: 'number'; value: SchemeNumber };
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
type VectorValue = { kind: 'vector'; elements: RuntimeValue[] };
type RecordTypeDescriptor = {
  name: string;
  fieldNames: string[];
};
type RecordValue = {
  kind: 'record';
  recordType: RecordTypeDescriptor;
  fields: RuntimeValue[];
};
type BuiltinProcedure = { kind: 'builtin'; name: BuiltinName };
type ClosureProcedure = {
  kind: 'closure';
  params: string[];
  restParam?: string;
  body: Expr[];
  env: Environment;
};
type CaseLambdaProcedure = {
  kind: 'case-lambda';
  clauses: ClosureProcedure[];
};
type RecordConstructorProcedure = {
  kind: 'record-constructor';
  name: string;
  recordType: RecordTypeDescriptor;
  fieldIndexes: number[];
};
type RecordPredicateProcedure = {
  kind: 'record-predicate';
  name: string;
  recordType: RecordTypeDescriptor;
};
type RecordAccessorProcedure = {
  kind: 'record-accessor';
  name: string;
  recordType: RecordTypeDescriptor;
  fieldIndex: number;
};
type RecordMutatorProcedure = {
  kind: 'record-mutator';
  name: string;
  recordType: RecordTypeDescriptor;
  fieldIndex: number;
};
type DynamicWindContext = {
  before: RuntimeValue;
  after: RuntimeValue;
};
type ExceptionHandlerContext = {
  procedure: RuntimeValue;
  stack: ContinuationFrame[];
  winds: DynamicWindContext[];
  handlers: ExceptionHandlerContext[];
  pos: SourcePos;
};
type ContinuationProcedure = {
  kind: 'continuation';
  stack: ContinuationFrame[];
  winds: DynamicWindContext[];
  handlers: ExceptionHandlerContext[];
};
type MultipleValuesValue = {
  kind: 'multiple-values';
  values: RuntimeValue[];
};
type VoidValue = { kind: 'void' };
type GuardHandlerProcedure = {
  kind: 'guard-handler';
  variable: string;
  clauses: Expr[];
  env: Environment;
};
type SyntaxRule = { pattern: Expr; template: Expr };
type SyntaxRulesMacro = {
  name: string;
  literals: Set<string>;
  rules: SyntaxRule[];
  env: Environment;
};
type PatternBinding =
  | { kind: 'single'; expr: Expr }
  | { kind: 'repeated'; exprs: Expr[] };

type RuntimeValue =
  | NumberExpr
  | BooleanExpr
  | StringValue
  | CharValue
  | SymbolValue
  | NilValue
  | PairValue
  | VectorValue
  | RecordValue
  | BuiltinProcedure
  | ClosureProcedure
  | CaseLambdaProcedure
  | RecordConstructorProcedure
  | RecordPredicateProcedure
  | RecordAccessorProcedure
  | RecordMutatorProcedure
  | ContinuationProcedure
  | MultipleValuesValue
  | GuardHandlerProcedure
  | VoidValue;

type EvalContext = {
  output: string[];
  immutableStrings: boolean;
};

type EvalAction =
  | { kind: 'value'; value: RuntimeValue }
  | { kind: 'expr'; expr: Expr; env: Environment }
  | { kind: 'sequence'; exprs: Expr[]; env: Environment; pos?: SourcePos }
  | { kind: 'apply'; procedure: RuntimeValue; args: RuntimeValue[]; pos?: SourcePos };

type SequenceFrame = {
  kind: 'sequence';
  remainingExprs: Expr[];
  env: Environment;
  pos: SourcePos;
};
type CallOperatorFrame = {
  kind: 'call-operator';
  argExprs: Expr[];
  env: Environment;
  pos: SourcePos;
};
type CallArgumentFrame = {
  kind: 'call-argument';
  procedure: RuntimeValue;
  evaluatedArgs: RuntimeValue[];
  remainingArgExprs: Expr[];
  env: Environment;
  pos: SourcePos;
};
type DefineValueFrame = {
  kind: 'define-value';
  name: string;
  env: Environment;
  pos: SourcePos;
};
type SetValueFrame = {
  kind: 'set-value';
  name: string;
  env: Environment;
  pos: SourcePos;
};
type IfFrame = {
  kind: 'if';
  consequent: Expr;
  alternate?: Expr;
  env: Environment;
  pos: SourcePos;
};
type AndFrame = {
  kind: 'and';
  remainingExprs: Expr[];
  env: Environment;
  pos: SourcePos;
};
type OrFrame = {
  kind: 'or';
  remainingExprs: Expr[];
  env: Environment;
  pos: SourcePos;
};
type LetInitFrame = {
  kind: 'let-init';
  name?: string;
  bindingNames: string[];
  remainingInitExprs: Expr[];
  collectedValues: RuntimeValue[];
  body: Expr[];
  env: Environment;
  pos: SourcePos;
};
type CondTestFrame = {
  kind: 'cond-test';
  body: Expr[];
  remainingClauses: Expr[];
  env: Environment;
  pos: SourcePos;
};
type CallWithValuesFrame = {
  kind: 'call-with-values';
  consumer: RuntimeValue;
  pos: SourcePos;
};
type ExceptionHandlerReturnFrame = {
  kind: 'exception-handler-return';
  handler: ExceptionHandlerContext;
  pos: SourcePos;
};
type DynamicWindEnterFrame = {
  kind: 'dynamic-wind-enter';
  wind: DynamicWindContext;
  bodyThunk: RuntimeValue;
  pos: SourcePos;
};
type DynamicWindBodyFrame = {
  kind: 'dynamic-wind-body';
  wind: DynamicWindContext;
  pos: SourcePos;
};
type DynamicWindAfterFrame = {
  kind: 'dynamic-wind-after';
  result: RuntimeValue;
  pos: SourcePos;
};
type WindTransferTarget = {
  stack: ContinuationFrame[];
  winds: DynamicWindContext[];
  handlers: ExceptionHandlerContext[];
};
type WindTransferCompletion =
  | {
      kind: 'value';
      value: RuntimeValue;
      target: WindTransferTarget;
    }
  | {
      kind: 'apply';
      procedure: RuntimeValue;
      args: RuntimeValue[];
      target: WindTransferTarget;
      pos?: SourcePos;
    };
type WindTransferAfterFrame = {
  kind: 'wind-transfer-after';
  exiting: DynamicWindContext[];
  entering: DynamicWindContext[];
  completion: WindTransferCompletion;
  pos: SourcePos;
};
type WindTransferBeforeFrame = {
  kind: 'wind-transfer-before';
  wind: DynamicWindContext;
  entering: DynamicWindContext[];
  completion: WindTransferCompletion;
  pos: SourcePos;
};

type ContinuationFrame =
  | SequenceFrame
  | CallOperatorFrame
  | CallArgumentFrame
  | DefineValueFrame
  | SetValueFrame
  | IfFrame
  | AndFrame
  | OrFrame
  | LetInitFrame
  | CondTestFrame
  | CallWithValuesFrame
  | ExceptionHandlerReturnFrame
  | DynamicWindEnterFrame
  | DynamicWindBodyFrame
  | DynamicWindAfterFrame
  | WindTransferAfterFrame
  | WindTransferBeforeFrame;

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
  'abs',
  'modulo',
  'remainder',
  'quotient',
  'min',
  'max',
  'expt',
  'gcd',
  'lcm',
  'truncate',
  'round',
  '<',
  '>',
  '=',
  '<=',
  '>=',
  'zero?',
  'positive?',
  'negative?',
  'odd?',
  'even?',
  'not',
  'cons',
  'car',
  'cdr',
  'caar',
  'cadr',
  'cdar',
  'cddr',
  'set-car!',
  'set-cdr!',
  'null?',
  'list',
  'append',
  'reverse',
  'length',
  'list-ref',
  'list-tail',
  'list?',
  'memq',
  'memv',
  'member',
  'assq',
  'assv',
  'assoc',
  'map',
  'for-each',
  'string?',
  'make-string',
  'string',
  'number?',
  'exact?',
  'inexact?',
  'integer?',
  'rational?',
  'boolean?',
  'pair?',
  'symbol?',
  'procedure?',
  'eq?',
  'eqv?',
  'equal?',
  'display',
  'write',
  'newline',
  'string-append',
  'string-length',
  'substring',
  'string->number',
  'number->string',
  'exact->inexact',
  'inexact->exact',
  'numerator',
  'denominator',
  'symbol->string',
  'string->symbol',
  'string-ref',
  'string-copy',
  'string-set!',
  'string->list',
  'list->string',
  'string=?',
  'string<?',
  'string>?',
  'string<=?',
  'string>=?',
  'string-ci=?',
  'string-upcase',
  'string-downcase',
  'char?',
  'char-alphabetic?',
  'char-numeric?',
  'char->integer',
  'integer->char',
  'char-upcase',
  'char-downcase',
  'char=?',
  'char<?',
  'vector',
  'make-vector',
  'vector-ref',
  'vector-set!',
  'vector-length',
  'vector?',
  'vector->list',
  'list->vector',
  'apply',
  'values',
  'call-with-values',
  'call/cc',
  'call-with-current-continuation',
  'dynamic-wind',
  'raise',
  'with-exception-handler',
  'error',
] as const;
type BuiltinName = (typeof BUILTIN_NAMES)[number];

const NIL_VALUE: NilValue = { kind: 'nil' };
const VOID_VALUE: VoidValue = { kind: 'void' };
const DEFAULT_SOURCE_POS: SourcePos = { line: 1, col: 1 };
const ALPHABETIC_CHAR_RE = /^\p{L}$/u;
const NUMERIC_CHAR_RE = /^\p{N}$/u;
let macroIdentifierCounter = 0;

class RaisedException {
  constructor(
    readonly value: RuntimeValue,
    public pos?: SourcePos,
  ) {}
}

function currentBenchLevel(): number {
  const globalWithProcess = globalThis as typeof globalThis & {
    process?: {
      env?: {
        BENCH_LEVEL?: string;
      };
    };
  };
  const rawLevel = globalWithProcess.process?.env?.BENCH_LEVEL;

  if (rawLevel === undefined) {
    return Number.POSITIVE_INFINITY;
  }

  const parsedLevel = Number.parseInt(rawLevel, 10);
  return Number.isFinite(parsedLevel) ? parsedLevel : Number.POSITIVE_INFINITY;
}

class Environment {
  private readonly bindings = new Map<string, RuntimeValue>();
  private readonly macros = new Map<string, SyntaxRulesMacro>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: RuntimeValue): void {
    this.bindings.set(name, value);
  }

  defineMacro(name: string, macroRules: SyntaxRulesMacro): void {
    this.macros.set(name, macroRules);
  }

  lookupOptional(name: string): RuntimeValue | undefined {
    if (this.bindings.has(name)) {
      return this.bindings.get(name);
    }

    return this.parent?.lookupOptional(name);
  }

  lookupMacro(name: string): SyntaxRulesMacro | undefined {
    if (this.macros.has(name)) {
      return this.macros.get(name);
    }

    return this.parent?.lookupMacro(name);
  }

  lookup(name: string): RuntimeValue {
    const value = this.lookupOptional(name);
    if (value !== undefined) {
      return value;
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
  return formatValue(expectSingleValue(evaluateProgram(input).result));
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  const evaluation = evaluateProgram(input);
  return {
    result: formatValue(expectSingleValue(evaluation.result)),
    output: evaluation.output,
  };
}

function evaluateProgram(input: string): { result: RuntimeValue; output: string } {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('expected at least one expression');
  }

  const env = createGlobalEnv();
  const context: EvalContext = {
    output: [],
    immutableStrings: currentBenchLevel() >= 15,
  };
  const result = evaluateSequence(expressions, env, context);

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

  const number = parseNumberLiteral(token.value);
  if (number !== undefined) {
    return { kind: 'number', value: number, pos: token.pos };
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
  return runEvaluation({ kind: 'expr', expr, env }, context);
}

function evaluateExprSingle(expr: Expr, env: Environment, context: EvalContext): RuntimeValue {
  return expectSingleValue(evaluateExpr(expr, env, context), expr.pos);
}

function runEvaluation(initialAction: EvalAction, context: EvalContext): RuntimeValue {
  let action = initialAction;
  const stack: ContinuationFrame[] = [];
  const winds: DynamicWindContext[] = [];
  const handlers: ExceptionHandlerContext[] = [];

  while (true) {
    let errorPos: SourcePos | undefined;

    try {
      switch (action.kind) {
        case 'value':
          if (stack.length === 0) {
            return action.value;
          }

          {
            const frame = stack.pop();
            if (frame === undefined) {
              return action.value;
            }

            errorPos = frame.pos;
            action = continueWithFrame(frame, action.value, stack, winds, handlers);
          }
          break;
        case 'expr':
          errorPos = action.expr.pos;
          action = evaluateExprAction(action.expr, action.env, context, stack, winds, handlers);
          break;
        case 'sequence':
          errorPos = action.pos ?? action.exprs[0]?.pos;
          action = startSequenceAction(
            action.exprs,
            action.env,
            stack,
            action.pos ?? action.exprs[0]?.pos ?? DEFAULT_SOURCE_POS,
          );
          break;
        case 'apply':
          errorPos = action.pos;

          if (
            action.procedure.kind === 'builtin' &&
            (action.procedure.name === 'call/cc' ||
              action.procedure.name === 'call-with-current-continuation')
          ) {
            if (action.args.length !== 1) {
              throw new EvalError(`${action.procedure.name} expects exactly 1 argument`);
            }

            action = {
              kind: 'apply',
              procedure: action.args[0],
              args: [
                {
                  kind: 'continuation',
                  stack: stack.slice(),
                  winds: winds.slice(),
                  handlers: handlers.slice(),
                },
              ],
              pos: action.pos,
            };
            break;
          }

          if (action.procedure.kind === 'builtin' && action.procedure.name === 'call-with-values') {
            if (action.args.length !== 2) {
              throw new EvalError('call-with-values expects exactly 2 arguments');
            }

            const [producer, consumer] = action.args;
            stack.push({
              kind: 'call-with-values',
              consumer,
              pos: action.pos ?? DEFAULT_SOURCE_POS,
            });
            action = {
              kind: 'apply',
              procedure: producer,
              args: [],
              pos: action.pos,
            };
            break;
          }

          if (action.procedure.kind === 'builtin' && action.procedure.name === 'dynamic-wind') {
            if (action.args.length !== 3) {
              throw new EvalError('dynamic-wind expects exactly 3 arguments');
            }

            const [beforeThunk, bodyThunk, afterThunk] = action.args;
            stack.push({
              kind: 'dynamic-wind-enter',
              wind: {
                before: beforeThunk,
                after: afterThunk,
              },
              bodyThunk,
              pos: action.pos ?? DEFAULT_SOURCE_POS,
            });
            action = {
              kind: 'apply',
              procedure: beforeThunk,
              args: [],
              pos: action.pos,
            };
            break;
          }

          if (
            action.procedure.kind === 'builtin' &&
            action.procedure.name === 'with-exception-handler'
          ) {
            if (action.args.length !== 2) {
              throw new EvalError('with-exception-handler expects exactly 2 arguments');
            }

            const [handlerProcedure, thunk] = action.args;
            const handlerContext: ExceptionHandlerContext = {
              procedure: handlerProcedure,
              stack: stack.slice(),
              winds: winds.slice(),
              handlers: handlers.slice(),
              pos: action.pos ?? DEFAULT_SOURCE_POS,
            };
            stack.push({
              kind: 'exception-handler-return',
              handler: handlerContext,
              pos: handlerContext.pos,
            });
            handlers.push(handlerContext);
            action = {
              kind: 'apply',
              procedure: thunk,
              args: [],
              pos: action.pos,
            };
            break;
          }

          if (action.procedure.kind === 'continuation') {
            if (action.args.length !== 1) {
              throw new EvalError('continuation expects exactly 1 argument');
            }

            const sharedPrefixLength = sharedDynamicWindPrefixLength(winds, action.procedure.winds);
            const exiting = winds.slice(sharedPrefixLength).reverse();
            const entering = action.procedure.winds.slice(sharedPrefixLength);

            action = startWindTransferAction(
              exiting,
              entering,
              {
                kind: 'value',
                value: action.args[0],
                target: {
                  stack: action.procedure.stack,
                  winds: action.procedure.winds,
                  handlers: action.procedure.handlers,
                },
              },
              stack,
              winds,
              handlers,
              action.pos ?? DEFAULT_SOURCE_POS,
            );
            break;
          }

          action = applyProcedureAction(action.procedure, action.args, context);
          break;
      }
    } catch (error) {
      if (error instanceof RaisedException) {
        if (error.pos === undefined) {
          error.pos = errorPos;
        }

        const handlerContext = handlers[handlers.length - 1];
        if (handlerContext === undefined) {
          throw new EvalError(`uncaught exception: ${formatValue(error.value)}`, error.pos);
        }

        replaceArrayContents(handlers, handlerContext.handlers);

        const sharedPrefixLength = sharedDynamicWindPrefixLength(winds, handlerContext.winds);
        const exiting = winds.slice(sharedPrefixLength).reverse();
        const entering = handlerContext.winds.slice(sharedPrefixLength);

        action = startWindTransferAction(
          exiting,
          entering,
          {
            kind: 'apply',
            procedure: handlerContext.procedure,
            args: [error.value],
            target: {
              stack: handlerContext.stack,
              winds: handlerContext.winds,
              handlers: handlerContext.handlers,
            },
            pos: handlerContext.pos,
          },
          stack,
          winds,
          handlers,
          handlerContext.pos,
        );
        continue;
      }

      if (errorPos !== undefined) {
        throw attachPosition(error, errorPos);
      }

      throw error;
    }
  }
}

function startSequenceAction(
  exprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (exprs.length === 0) {
    return { kind: 'value', value: VOID_VALUE };
  }

  if (exprs.length > 1) {
    stack.push({
      kind: 'sequence',
      remainingExprs: exprs.slice(1),
      env,
      pos,
    });
  }

  return { kind: 'expr', expr: exprs[0], env };
}

function continueWithFrame(
  frame: ContinuationFrame,
  value: RuntimeValue,
  stack: ContinuationFrame[],
  winds: DynamicWindContext[],
  handlers: ExceptionHandlerContext[],
): EvalAction {
  switch (frame.kind) {
    case 'sequence':
      expectSingleValue(value, frame.pos);
      return startSequenceAction(frame.remainingExprs, frame.env, stack, frame.pos);
    case 'call-operator':
      value = expectSingleValue(value, frame.pos);
      if (frame.argExprs.length === 0) {
        return {
          kind: 'apply',
          procedure: value,
          args: [],
          pos: frame.pos,
        };
      }

      stack.push({
        kind: 'call-argument',
        procedure: value,
        evaluatedArgs: [],
        remainingArgExprs: frame.argExprs.slice(0, -1),
        env: frame.env,
        pos: frame.pos,
      });
      return { kind: 'expr', expr: frame.argExprs[frame.argExprs.length - 1], env: frame.env };
    case 'call-argument': {
      value = expectSingleValue(value, frame.pos);
      const args = [value, ...frame.evaluatedArgs];
      if (frame.remainingArgExprs.length === 0) {
        return {
          kind: 'apply',
          procedure: frame.procedure,
          args,
          pos: frame.pos,
        };
      }

      stack.push({
        kind: 'call-argument',
        procedure: frame.procedure,
        evaluatedArgs: args,
        remainingArgExprs: frame.remainingArgExprs.slice(0, -1),
        env: frame.env,
        pos: frame.pos,
      });
      return {
        kind: 'expr',
        expr: frame.remainingArgExprs[frame.remainingArgExprs.length - 1],
        env: frame.env,
      };
    }
    case 'define-value':
      value = expectSingleValue(value, frame.pos);
      frame.env.define(frame.name, value);
      return { kind: 'value', value: VOID_VALUE };
    case 'set-value':
      value = expectSingleValue(value, frame.pos);
      frame.env.assign(frame.name, value);
      return { kind: 'value', value: VOID_VALUE };
    case 'if':
      value = expectSingleValue(value, frame.pos);
      if (isTruthy(value)) {
        return { kind: 'expr', expr: frame.consequent, env: frame.env };
      }

      if (frame.alternate === undefined) {
        return { kind: 'value', value: VOID_VALUE };
      }

      return { kind: 'expr', expr: frame.alternate, env: frame.env };
    case 'and':
      value = expectSingleValue(value, frame.pos);
      if (!isTruthy(value)) {
        return { kind: 'value', value };
      }

      return startShortCircuitAction('and', frame.remainingExprs, frame.env, stack, frame.pos);
    case 'or':
      value = expectSingleValue(value, frame.pos);
      if (isTruthy(value)) {
        return { kind: 'value', value };
      }

      return startShortCircuitAction('or', frame.remainingExprs, frame.env, stack, frame.pos);
    case 'let-init':
      value = expectSingleValue(value, frame.pos);
      return continueLetInitAction(frame, value, stack);
    case 'cond-test':
      value = expectSingleValue(value, frame.pos);
      if (!isTruthy(value)) {
        return startCondAction(frame.remainingClauses, frame.env, stack, frame.pos);
      }

      if (frame.body.length === 0) {
        return { kind: 'value', value };
      }

      return startSequenceAction(frame.body, frame.env, stack, frame.pos);
    case 'call-with-values':
      return {
        kind: 'apply',
        procedure: frame.consumer,
        args: unwrapValues(value),
        pos: frame.pos,
      };
    case 'exception-handler-return': {
      const currentHandler = handlers.pop();
      if (currentHandler !== frame.handler) {
        throw new EvalError('internal error: exception handler stack mismatch');
      }

      return { kind: 'value', value };
    }
    case 'dynamic-wind-enter':
      expectSingleValue(value, frame.pos);
      winds.push(frame.wind);
      stack.push({
        kind: 'dynamic-wind-body',
        wind: frame.wind,
        pos: frame.pos,
      });
      return {
        kind: 'apply',
        procedure: frame.bodyThunk,
        args: [],
        pos: frame.pos,
      };
    case 'dynamic-wind-body': {
      const currentWind = winds.pop();
      if (currentWind !== frame.wind) {
        throw new EvalError('internal error: dynamic-wind stack mismatch');
      }

      stack.push({
        kind: 'dynamic-wind-after',
        result: value,
        pos: frame.pos,
      });
      return {
        kind: 'apply',
        procedure: frame.wind.after,
        args: [],
        pos: frame.pos,
      };
    }
    case 'dynamic-wind-after':
      expectSingleValue(value, frame.pos);
      return { kind: 'value', value: frame.result };
    case 'wind-transfer-after':
      expectSingleValue(value, frame.pos);
      return startWindTransferAction(
        frame.exiting,
        frame.entering,
        frame.completion,
        stack,
        winds,
        handlers,
        frame.pos,
      );
    case 'wind-transfer-before':
      expectSingleValue(value, frame.pos);
      winds.push(frame.wind);
      return startWindTransferAction(
        [],
        frame.entering,
        frame.completion,
        stack,
        winds,
        handlers,
        frame.pos,
      );
  }
}

function replaceArrayContents<T>(target: T[], source: readonly T[]): void {
  target.length = 0;
  target.push(...source);
}

function sharedDynamicWindPrefixLength(
  left: readonly DynamicWindContext[],
  right: readonly DynamicWindContext[],
): number {
  let index = 0;

  while (index < left.length && index < right.length && left[index] === right[index]) {
    index += 1;
  }

  return index;
}

function startWindTransferAction(
  exiting: DynamicWindContext[],
  entering: DynamicWindContext[],
  completion: WindTransferCompletion,
  stack: ContinuationFrame[],
  winds: DynamicWindContext[],
  handlers: ExceptionHandlerContext[],
  pos: SourcePos,
): EvalAction {
  if (exiting.length > 0) {
    const [wind, ...remainingExits] = exiting;
    const currentWind = winds.pop();
    if (currentWind !== wind) {
      throw new EvalError('internal error: dynamic-wind stack mismatch');
    }

    stack.push({
      kind: 'wind-transfer-after',
      exiting: remainingExits,
      entering,
      completion,
      pos,
    });
    return {
      kind: 'apply',
      procedure: wind.after,
      args: [],
      pos,
    };
  }

  if (entering.length > 0) {
    const [wind, ...remainingEntries] = entering;
    stack.push({
      kind: 'wind-transfer-before',
      wind,
      entering: remainingEntries,
      completion,
      pos,
    });
    return {
      kind: 'apply',
      procedure: wind.before,
      args: [],
      pos,
    };
  }

  replaceArrayContents(stack, completion.target.stack);
  replaceArrayContents(winds, completion.target.winds);
  replaceArrayContents(handlers, completion.target.handlers);
  if (completion.kind === 'value') {
    return { kind: 'value', value: completion.value };
  }

  return {
    kind: 'apply',
    procedure: completion.procedure,
    args: completion.args,
    pos: completion.pos,
  };
}

function startShortCircuitAction(
  kind: 'and' | 'or',
  exprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (exprs.length === 0) {
    return { kind: 'value', value: makeBoolean(kind === 'and') };
  }

  if (exprs.length === 1) {
    return { kind: 'expr', expr: exprs[0], env };
  }

  stack.push({
    kind,
    remainingExprs: exprs.slice(1),
    env,
    pos,
  });
  return { kind: 'expr', expr: exprs[0], env };
}

function startApplicationAction(
  expr: ListExpr,
  env: Environment,
  stack: ContinuationFrame[],
): EvalAction {
  const [head, ...argExprs] = expr.elements;
  stack.push({
    kind: 'call-operator',
    argExprs,
    env,
    pos: expr.pos,
  });
  return { kind: 'expr', expr: head, env };
}

function evaluateExprAction(
  expr: Expr,
  env: Environment,
  context: EvalContext,
  stack: ContinuationFrame[],
  winds: DynamicWindContext[],
  handlers: ExceptionHandlerContext[],
): EvalAction {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
      return { kind: 'value', value: expr };
    case 'string':
      return { kind: 'value', value: makeString(expr.value) };
    case 'char':
      return { kind: 'value', value: makeChar(expr.value) };
    case 'symbol':
      return { kind: 'value', value: env.lookup(expr.name) };
    case 'list':
      return evaluateListAction(expr, env, context, stack, winds, handlers);
  }
}

function evaluateListAction(
  expr: ListExpr,
  env: Environment,
  context: EvalContext,
  stack: ContinuationFrame[],
  winds: DynamicWindContext[],
  handlers: ExceptionHandlerContext[],
): EvalAction {
  if (expr.elements.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const [head, ...argExprs] = expr.elements;

  if (head.kind === 'symbol') {
    switch (head.name) {
      case 'define-syntax':
        return { kind: 'value', value: evaluateDefineSyntax(argExprs, env) };
      case 'define':
        return evaluateDefine(argExprs, env, stack, expr.pos);
      case 'define-record-type':
        return { kind: 'value', value: evaluateDefineRecordType(argExprs, env) };
      case 'set!':
        return evaluateSet(argExprs, env, stack, expr.pos);
      case 'if':
        return evaluateIfAction(argExprs, env, stack, expr.pos);
      case 'quote':
        return { kind: 'value', value: evaluateQuote(argExprs) };
      case 'lambda':
        return { kind: 'value', value: evaluateLambda(argExprs, env) };
      case 'case-lambda':
        return { kind: 'value', value: evaluateCaseLambda(argExprs, env) };
      case 'and':
        return evaluateAndAction(argExprs, env, stack, expr.pos);
      case 'or':
        return evaluateOrAction(argExprs, env, stack, expr.pos);
      case 'begin':
        return evaluateBeginAction(argExprs, env, stack, expr.pos);
      case 'let':
        return evaluateLetAction(argExprs, env, stack, expr.pos);
      case 'let*':
        return evaluateLetStarAction(argExprs, env, context);
      case 'letrec':
        return evaluateLetrecAction(argExprs, env, context, false);
      case 'letrec*':
        return evaluateLetrecAction(argExprs, env, context, true);
      case 'cond':
        return evaluateCondAction(argExprs, env, stack, expr.pos);
      case 'case':
        return evaluateCaseAction(argExprs, env, context);
      case 'do':
        return evaluateDoAction(argExprs, env, context);
      case 'guard':
        return evaluateGuardAction(argExprs, env, stack, winds, handlers, expr.pos);
    }

    const macroRules = env.lookupMacro(head.name);
    if (macroRules !== undefined) {
      const expanded = expandMacroInvocation(expr.elements, macroRules, env);
      return { kind: 'expr', expr: expanded.expr, env: expanded.env };
    }
  }

  return startApplicationAction(expr, env, stack);
}

function evaluateDefine(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (argExprs.length < 2) {
    throw new EvalError('define expects a target and a value');
  }

  const [target, ...body] = argExprs;

  if (target.kind === 'symbol') {
    if (body.length !== 1) {
      throw new EvalError('define variable form expects exactly 1 value expression');
    }

    stack.push({
      kind: 'define-value',
      name: target.name,
      env,
      pos,
    });
    return { kind: 'expr', expr: body[0], env };
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
    return { kind: 'value', value: VOID_VALUE };
  }

  throw new EvalError('invalid define form');
}

function evaluateDefineRecordType(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length < 3) {
    throw new EvalError(
      'define-record-type expects a type name, constructor, predicate, and field specs',
    );
  }

  const [typeNameExpr, constructorExpr, predicateExpr, ...fieldExprs] = argExprs;
  const typeName = expectSymbolExpr(typeNameExpr, 'define-record-type type name');
  const predicateName = expectSymbolExpr(predicateExpr, 'define-record-type predicate name');

  if (constructorExpr.kind !== 'list' || constructorExpr.elements.length === 0) {
    throw new EvalError('define-record-type constructor spec must be a non-empty list');
  }

  const [constructorNameExpr, ...constructorFieldExprs] = constructorExpr.elements;
  const constructorName = expectSymbolExpr(
    constructorNameExpr,
    'define-record-type constructor name',
  );

  const fieldNames: string[] = [];
  const fieldIndexes = new Map<string, number>();
  const accessors: Array<{ name: string; fieldIndex: number }> = [];
  const mutators: Array<{ name: string; fieldIndex: number }> = [];

  for (const fieldExpr of fieldExprs) {
    if (
      fieldExpr.kind !== 'list' ||
      fieldExpr.elements.length < 2 ||
      fieldExpr.elements.length > 3
    ) {
      throw new EvalError(
        'define-record-type field specs must be (field accessor) or (field accessor mutator)',
      );
    }

    const fieldName = expectSymbolExpr(fieldExpr.elements[0], 'define-record-type field name');
    if (fieldIndexes.has(fieldName)) {
      throw new EvalError('define-record-type field names must be unique');
    }

    const fieldIndex = fieldNames.length;
    fieldNames.push(fieldName);
    fieldIndexes.set(fieldName, fieldIndex);

    accessors.push({
      name: expectSymbolExpr(fieldExpr.elements[1], 'define-record-type accessor name'),
      fieldIndex,
    });

    if (fieldExpr.elements[2] !== undefined) {
      mutators.push({
        name: expectSymbolExpr(fieldExpr.elements[2], 'define-record-type mutator name'),
        fieldIndex,
      });
    }
  }

  const constructorFieldIndexes: number[] = [];
  const constructorFields = new Set<string>();
  for (const fieldExpr of constructorFieldExprs) {
    const fieldName = expectSymbolExpr(fieldExpr, 'define-record-type constructor field');
    const fieldIndex = fieldIndexes.get(fieldName);
    if (fieldIndex === undefined) {
      throw new EvalError(`define-record-type constructor field is not defined: ${fieldName}`);
    }

    if (constructorFields.has(fieldName)) {
      throw new EvalError('define-record-type constructor fields must be unique');
    }

    constructorFields.add(fieldName);
    constructorFieldIndexes.push(fieldIndex);
  }

  if (constructorFieldIndexes.length !== fieldNames.length) {
    throw new EvalError('define-record-type constructor must initialize every field');
  }

  const recordType: RecordTypeDescriptor = {
    name: typeName,
    fieldNames,
  };

  env.define(constructorName, {
    kind: 'record-constructor',
    name: constructorName,
    recordType,
    fieldIndexes: constructorFieldIndexes,
  });
  env.define(predicateName, {
    kind: 'record-predicate',
    name: predicateName,
    recordType,
  });

  for (const accessor of accessors) {
    env.define(accessor.name, {
      kind: 'record-accessor',
      name: accessor.name,
      recordType,
      fieldIndex: accessor.fieldIndex,
    });
  }

  for (const mutator of mutators) {
    env.define(mutator.name, {
      kind: 'record-mutator',
      name: mutator.name,
      recordType,
      fieldIndex: mutator.fieldIndex,
    });
  }

  return VOID_VALUE;
}

function evaluateSet(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (argExprs.length !== 2) {
    throw new EvalError('set! expects exactly 2 arguments');
  }

  const [target, valueExpr] = argExprs;
  if (target.kind !== 'symbol') {
    throw new EvalError('set! expects a symbol target');
  }

  stack.push({
    kind: 'set-value',
    name: target.name,
    env,
    pos,
  });
  return { kind: 'expr', expr: valueExpr, env };
}

function evaluateIfAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (argExprs.length !== 2 && argExprs.length !== 3) {
    throw new EvalError('if expects exactly 2 or 3 arguments');
  }

  stack.push({
    kind: 'if',
    consequent: argExprs[1],
    alternate: argExprs[2],
    env,
    pos,
  });
  return { kind: 'expr', expr: argExprs[0], env };
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
  const params = readFormals(paramsExpr, 'lambda parameters must be a list or symbol');

  return {
    kind: 'closure',
    params: params.fixedParams,
    restParam: params.restParam,
    body,
    env,
  };
}

function evaluateCaseLambda(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length === 0) {
    throw new EvalError('case-lambda expects at least 1 clause');
  }

  return {
    kind: 'case-lambda',
    clauses: argExprs.map((clauseExpr) => {
      if (clauseExpr.kind !== 'list' || clauseExpr.elements.length < 2) {
        throw new EvalError('case-lambda clauses must be (formals body ...) lists');
      }

      const [paramsExpr, ...body] = clauseExpr.elements;
      const params = readFormals(
        paramsExpr,
        'case-lambda clause parameters must be a list or symbol',
      );

      return {
        kind: 'closure',
        params: params.fixedParams,
        restParam: params.restParam,
        body,
        env,
      };
    }),
  };
}

function readFormals(
  paramsExpr: Expr,
  errorMessage: string,
): { fixedParams: string[]; restParam?: string } {
  if (paramsExpr.kind === 'symbol') {
    return { fixedParams: [], restParam: paramsExpr.name };
  }

  if (paramsExpr.kind === 'list') {
    return readParameterList(paramsExpr.elements);
  }

  throw new EvalError(errorMessage);
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

function evaluateDefineSyntax(argExprs: Expr[], env: Environment): RuntimeValue {
  if (argExprs.length !== 2) {
    throw new EvalError('define-syntax expects exactly 2 arguments');
  }

  const [nameExpr, transformerExpr] = argExprs;
  if (nameExpr.kind !== 'symbol') {
    throw new EvalError('define-syntax expects a symbol name');
  }

  env.defineMacro(nameExpr.name, readSyntaxRules(nameExpr.name, transformerExpr, env));
  return VOID_VALUE;
}

function readSyntaxRules(
  name: string,
  transformerExpr: Expr,
  env: Environment,
): SyntaxRulesMacro {
  if (transformerExpr.kind !== 'list') {
    throw new EvalError('define-syntax expects a syntax-rules transformer');
  }

  const items = transformerExpr.elements;
  if (items.length < 3) {
    throw new EvalError('define-syntax expects a syntax-rules transformer');
  }

  if (items[0].kind !== 'symbol' || items[0].name !== 'syntax-rules') {
    throw new EvalError('define-syntax expects a syntax-rules transformer');
  }

  const literalExprs = items[1];
  if (literalExprs.kind !== 'list') {
    throw new EvalError('syntax-rules literals must be a list');
  }

  const literals = new Set<string>();
  for (const literalExpr of literalExprs.elements) {
    if (literalExpr.kind !== 'symbol') {
      throw new EvalError('syntax-rules literals must be identifiers');
    }
    literals.add(literalExpr.name);
  }

  const rules: SyntaxRule[] = [];
  for (const ruleExpr of items.slice(2)) {
    if (ruleExpr.kind !== 'list' || ruleExpr.elements.length !== 2) {
      throw new EvalError('syntax-rules rules must be (pattern template) pairs');
    }

    rules.push({
      pattern: ruleExpr.elements[0],
      template: ruleExpr.elements[1],
    });
  }

  return {
    name,
    literals,
    rules,
    env,
  };
}

function expandMacroInvocation(
  elements: Expr[],
  macroRules: SyntaxRulesMacro,
  callEnv: Environment,
): { expr: Expr; env: Environment } {
  const invocation: Expr = {
    kind: 'list',
    pos: elements[0]?.pos ?? DEFAULT_SOURCE_POS,
    elements,
  };

  for (const rule of macroRules.rules) {
    const bindings = matchSyntaxRule(rule.pattern, invocation, macroRules.literals);
    if (bindings === undefined) {
      continue;
    }

    const expansionEnv = new Environment(callEnv);
    const aliases = new Map<string, string>();
    const expr = instantiateTemplate(
      rule.template,
      bindings,
      macroRules,
      expansionEnv,
      aliases,
      new Map<string, string>(),
      undefined,
    );

    return { expr, env: expansionEnv };
  }

  throw new EvalError(`no matching syntax-rules clause for ${macroRules.name}`);
}

function matchSyntaxRule(
  pattern: Expr,
  invocation: Expr,
  literals: Set<string>,
): Map<string, PatternBinding> | undefined {
  if (pattern.kind !== 'list' || pattern.elements.length === 0) {
    throw new EvalError('syntax-rules patterns must be non-empty lists');
  }

  if (invocation.kind !== 'list') {
    return undefined;
  }

  return matchPatternList(
    pattern.elements,
    invocation.elements,
    literals,
    new Map<string, PatternBinding>(),
    true,
  );
}

function matchPatternList(
  patternElements: Expr[],
  exprElements: Expr[],
  literals: Set<string>,
  bindings: Map<string, PatternBinding>,
  isTopLevel: boolean,
): Map<string, PatternBinding> | undefined {
  const parts = splitEllipsisParts(patternElements);
  return matchPatternParts(parts, exprElements, literals, bindings, isTopLevel, 0, 0);
}

function matchPatternParts(
  parts: Array<{ expr: Expr; repeated: boolean }>,
  exprElements: Expr[],
  literals: Set<string>,
  bindings: Map<string, PatternBinding>,
  isTopLevel: boolean,
  partIndex: number,
  exprIndex: number,
): Map<string, PatternBinding> | undefined {
  if (partIndex === parts.length) {
    return exprIndex === exprElements.length ? bindings : undefined;
  }

  const part = parts[partIndex];
  if (!part.repeated) {
    const expr = exprElements[exprIndex];
    if (expr === undefined) {
      return undefined;
    }

    const nextBindings = clonePatternBindings(bindings);
    if (!matchPatternExpr(part.expr, expr, literals, nextBindings, isTopLevel && partIndex === 0)) {
      return undefined;
    }

    return matchPatternParts(
      parts,
      exprElements,
      literals,
      nextBindings,
      false,
      partIndex + 1,
      exprIndex + 1,
    );
  }

  const minRemaining = countRequiredPatternParts(parts, partIndex + 1);
  const maxRepeats = exprElements.length - exprIndex - minRemaining;
  if (maxRepeats < 0) {
    return undefined;
  }

  for (let repeatCount = 0; repeatCount <= maxRepeats; repeatCount += 1) {
    const nextBindings = clonePatternBindings(bindings);
    let matched = true;

    for (let offset = 0; offset < repeatCount; offset += 1) {
      const localBindings = new Map<string, PatternBinding>();
      if (!matchPatternExpr(part.expr, exprElements[exprIndex + offset], literals, localBindings, false)) {
        matched = false;
        break;
      }

      if (!mergeRepeatedPatternBindings(nextBindings, localBindings)) {
        matched = false;
        break;
      }
    }

    if (!matched) {
      continue;
    }

    if (!ensureRepeatedPatternBindings(part.expr, literals, nextBindings)) {
      continue;
    }

    const result = matchPatternParts(
      parts,
      exprElements,
      literals,
      nextBindings,
      false,
      partIndex + 1,
      exprIndex + repeatCount,
    );
    if (result !== undefined) {
      return result;
    }
  }

  return undefined;
}

function matchPatternExpr(
  pattern: Expr,
  expr: Expr,
  literals: Set<string>,
  bindings: Map<string, PatternBinding>,
  ignoreKeyword: boolean,
): boolean {
  switch (pattern.kind) {
    case 'number':
      return expr.kind === 'number' && sameNumericSyntax(expr.value, pattern.value);
    case 'boolean':
      return expr.kind === 'boolean' && expr.value === pattern.value;
    case 'string':
      return expr.kind === 'string' && expr.value === pattern.value;
    case 'char':
      return expr.kind === 'char' && expr.value === pattern.value;
    case 'symbol':
      if (pattern.name === '...') {
        throw new EvalError('invalid use of ellipsis');
      }

      if (ignoreKeyword) {
        return expr.kind === 'symbol';
      }

      if (literals.has(pattern.name)) {
        return expr.kind === 'symbol' && expr.name === pattern.name;
      }

      return bindPatternVariable(pattern.name, expr, bindings);
    case 'list': {
      if (expr.kind !== 'list') {
        return false;
      }

      const result = matchPatternList(
        pattern.elements,
        expr.elements,
        literals,
        clonePatternBindings(bindings),
        false,
      );
      if (result === undefined) {
        return false;
      }

      replacePatternBindings(bindings, result);
      return true;
    }
  }
}

function bindPatternVariable(
  name: string,
  expr: Expr,
  bindings: Map<string, PatternBinding>,
): boolean {
  const binding = bindings.get(name);
  if (binding === undefined) {
    bindings.set(name, { kind: 'single', expr });
    return true;
  }

  if (binding.kind === 'single') {
    return equalExprSyntax(binding.expr, expr);
  }

  return false;
}

function mergeRepeatedPatternBindings(
  target: Map<string, PatternBinding>,
  source: Map<string, PatternBinding>,
): boolean {
  for (const [name, binding] of source) {
    if (binding.kind !== 'single') {
      throw new EvalError('nested ellipsis patterns are not supported');
    }

    const existing = target.get(name);
    if (existing?.kind === 'repeated') {
      existing.exprs.push(binding.expr);
      continue;
    }

    if (existing?.kind === 'single') {
      return false;
    }

    target.set(name, { kind: 'repeated', exprs: [binding.expr] });
  }

  return true;
}

function ensureRepeatedPatternBindings(
  pattern: Expr,
  literals: Set<string>,
  bindings: Map<string, PatternBinding>,
): boolean {
  const names = new Set<string>();
  collectPatternVariables(pattern, literals, names, false);

  for (const name of names) {
    const binding = bindings.get(name);
    if (binding?.kind === 'single') {
      return false;
    }

    if (binding === undefined) {
      bindings.set(name, { kind: 'repeated', exprs: [] });
    }
  }

  return true;
}

function collectPatternVariables(
  pattern: Expr,
  literals: Set<string>,
  names: Set<string>,
  ignoreKeyword: boolean,
): void {
  switch (pattern.kind) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return;
    case 'symbol':
      if (pattern.name !== '...' && !ignoreKeyword && !literals.has(pattern.name)) {
        names.add(pattern.name);
      }
      return;
    case 'list':
      for (const part of splitEllipsisParts(pattern.elements)) {
        collectPatternVariables(part.expr, literals, names, false);
      }
  }
}

function countRequiredPatternParts(
  parts: Array<{ expr: Expr; repeated: boolean }>,
  startIndex: number,
): number {
  return parts.slice(startIndex).filter((part) => !part.repeated).length;
}

function splitEllipsisParts(elements: Expr[]): Array<{ expr: Expr; repeated: boolean }> {
  const parts: Array<{ expr: Expr; repeated: boolean }> = [];
  let index = 0;

  while (index < elements.length) {
    const expr = elements[index];
    if (isEllipsisExpr(expr)) {
      throw new EvalError('invalid use of ellipsis');
    }

    const repeated = isEllipsisExpr(elements[index + 1]);
    parts.push({ expr, repeated });
    index += repeated ? 2 : 1;
  }

  return parts;
}

function isEllipsisExpr(expr: Expr | undefined): boolean {
  return expr?.kind === 'symbol' && expr.name === '...';
}

function instantiateTemplate(
  template: Expr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  switch (template.kind) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return template;
    case 'symbol':
      return instantiateTemplateSymbol(
        template,
        bindings,
        macroRules,
        expansionEnv,
        aliases,
        localScope,
        repeatIndex,
      );
    case 'list':
      return instantiateTemplateList(
        template,
        bindings,
        macroRules,
        expansionEnv,
        aliases,
        localScope,
        repeatIndex,
      );
  }
}

function instantiateTemplateSymbol(
  template: SymbolExpr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  if (template.name === '...') {
    return template;
  }

  const scopedName = localScope.get(template.name);
  if (scopedName !== undefined) {
    return { kind: 'symbol', name: scopedName, pos: template.pos };
  }

  const binding = bindings.get(template.name);
  if (binding !== undefined) {
    if (binding.kind === 'single') {
      return binding.expr;
    }

    if (repeatIndex === undefined) {
      throw new EvalError('ellipsis-bound pattern variable used outside ellipsis');
    }

    const expr = binding.exprs[repeatIndex];
    if (expr === undefined) {
      throw new EvalError('ellipsis repetition mismatch');
    }

    return expr;
  }

  if (isSpecialFormName(template.name)) {
    return template;
  }

  return {
    kind: 'symbol',
    name: resolveMacroIdentifier(template.name, macroRules, expansionEnv, aliases),
    pos: template.pos,
  };
}

function instantiateTemplateList(
  template: ListExpr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  if (isQuoteForm(template.elements)) {
    return template;
  }

  const head = template.elements[0];
  if (head?.kind === 'symbol') {
    switch (head.name) {
      case 'lambda':
        if (template.elements.length >= 2) {
          return instantiateLambdaTemplate(
            template,
            bindings,
            macroRules,
            expansionEnv,
            aliases,
            localScope,
            repeatIndex,
          );
        }
        break;
      case 'let':
        if (template.elements.length >= 3) {
          return instantiateLetTemplate(
            template,
            bindings,
            macroRules,
            expansionEnv,
            aliases,
            localScope,
            repeatIndex,
          );
        }
        break;
    }
  }

  return instantiateGenericTemplateList(
    template,
    bindings,
    macroRules,
    expansionEnv,
    aliases,
    localScope,
    repeatIndex,
  );
}

function instantiateGenericTemplateList(
  template: ListExpr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  const elements: Expr[] = [];

  for (const part of splitEllipsisParts(template.elements)) {
    if (!part.repeated) {
      elements.push(
        instantiateTemplate(
          part.expr,
          bindings,
          macroRules,
          expansionEnv,
          aliases,
          localScope,
          repeatIndex,
        ),
      );
      continue;
    }

    const repeats = getTemplateRepeatCount(part.expr, bindings);
    for (let index = 0; index < repeats; index += 1) {
      elements.push(
        instantiateTemplate(
          part.expr,
          bindings,
          macroRules,
          expansionEnv,
          aliases,
          localScope,
          index,
        ),
      );
    }
  }

  return {
    kind: 'list',
    pos: template.pos,
    elements,
  };
}

function instantiateLambdaTemplate(
  template: ListExpr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  const bodyScope = new Map(localScope);
  const elements: Expr[] = [template.elements[0]];

  elements.push(
    instantiateBindingSpec(
      template.elements[1],
      bindings,
      macroRules,
      expansionEnv,
      aliases,
      localScope,
      bodyScope,
      repeatIndex,
    ),
  );

  for (const expr of template.elements.slice(2)) {
    elements.push(
      instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, bodyScope, repeatIndex),
    );
  }

  return {
    kind: 'list',
    pos: template.pos,
    elements,
  };
}

function instantiateLetTemplate(
  template: ListExpr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  const elements: Expr[] = [template.elements[0]];
  const bodyScope = new Map(localScope);

  let bindingsIndex = 1;
  let bodyStartIndex = 2;

  if (template.elements[1]?.kind === 'symbol' && template.elements[2] !== undefined) {
    elements.push(
      instantiateBindingName(
        template.elements[1],
        bindings,
        macroRules,
        expansionEnv,
        aliases,
        localScope,
        bodyScope,
        repeatIndex,
      ),
    );
    bindingsIndex = 2;
    bodyStartIndex = 3;
  }

  const bindingExpr = template.elements[bindingsIndex];
  if (bindingExpr?.kind !== 'list') {
    return instantiateGenericTemplateList(
      template,
      bindings,
      macroRules,
      expansionEnv,
      aliases,
      localScope,
      repeatIndex,
    );
  }

  const instantiatedBindings: Expr[] = [];
  for (const entry of bindingExpr.elements) {
    if (entry.kind === 'list' && entry.elements.length === 2) {
      instantiatedBindings.push({
        kind: 'list',
        pos: entry.pos,
        elements: [
          instantiateBindingName(
            entry.elements[0],
            bindings,
            macroRules,
            expansionEnv,
            aliases,
            localScope,
            bodyScope,
            repeatIndex,
          ),
          instantiateTemplate(
            entry.elements[1],
            bindings,
            macroRules,
            expansionEnv,
            aliases,
            localScope,
            repeatIndex,
          ),
        ],
      });
      continue;
    }

    instantiatedBindings.push(
      instantiateTemplate(entry, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex),
    );
  }

  elements.push({
    kind: 'list',
    pos: bindingExpr.pos,
    elements: instantiatedBindings,
  });

  for (const expr of template.elements.slice(bodyStartIndex)) {
    elements.push(
      instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, bodyScope, repeatIndex),
    );
  }

  return {
    kind: 'list',
    pos: template.pos,
    elements,
  };
}

function instantiateBindingSpec(
  spec: Expr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  bodyScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  if (spec.kind === 'symbol') {
    return instantiateBindingName(
      spec,
      bindings,
      macroRules,
      expansionEnv,
      aliases,
      localScope,
      bodyScope,
      repeatIndex,
    );
  }

  if (spec.kind === 'list') {
    return {
      kind: 'list',
      pos: spec.pos,
      elements: spec.elements.map((item) =>
        item.kind === 'symbol' && item.name === '.'
          ? item
          : instantiateBindingName(
              item,
              bindings,
              macroRules,
              expansionEnv,
              aliases,
              localScope,
              bodyScope,
              repeatIndex,
            ),
      ),
    };
  }

  return instantiateTemplate(spec, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
}

function instantiateBindingName(
  expr: Expr,
  bindings: Map<string, PatternBinding>,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
  localScope: Map<string, string>,
  bodyScope: Map<string, string>,
  repeatIndex: number | undefined,
): Expr {
  if (expr.kind !== 'symbol') {
    return instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
  }

  if (expr.name === '.') {
    return expr;
  }

  if (bindings.has(expr.name)) {
    return instantiateTemplate(expr, bindings, macroRules, expansionEnv, aliases, localScope, repeatIndex);
  }

  const alias = freshMacroIdentifier(expr.name);
  bodyScope.set(expr.name, alias);
  return {
    kind: 'symbol',
    name: alias,
    pos: expr.pos,
  };
}

function getTemplateRepeatCount(
  template: Expr,
  bindings: Map<string, PatternBinding>,
): number {
  const names = new Set<string>();
  collectRepeatedPatternVariables(template, bindings, names);
  if (names.size === 0) {
    throw new EvalError('template ellipsis has no repeated pattern variables');
  }

  let repeatCount: number | undefined;
  for (const name of names) {
    const binding = bindings.get(name);
    if (binding?.kind !== 'repeated') {
      continue;
    }

    if (repeatCount === undefined) {
      repeatCount = binding.exprs.length;
      continue;
    }

    if (repeatCount !== binding.exprs.length) {
      throw new EvalError(
        'ellipsis-bound pattern variables must repeat the same number of times',
      );
    }
  }

  return repeatCount ?? 0;
}

function collectRepeatedPatternVariables(
  expr: Expr,
  bindings: Map<string, PatternBinding>,
  names: Set<string>,
): void {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return;
    case 'symbol':
      if (bindings.get(expr.name)?.kind === 'repeated') {
        names.add(expr.name);
      }
      return;
    case 'list':
      if (isQuoteForm(expr.elements)) {
        return;
      }

      for (const part of splitEllipsisParts(expr.elements)) {
        collectRepeatedPatternVariables(part.expr, bindings, names);
      }
  }
}

function resolveMacroIdentifier(
  name: string,
  macroRules: SyntaxRulesMacro,
  expansionEnv: Environment,
  aliases: Map<string, string>,
): string {
  const existing = aliases.get(name);
  if (existing !== undefined) {
    return existing;
  }

  const alias = freshMacroIdentifier(name);
  aliases.set(name, alias);

  const capturedMacro = macroRules.env.lookupMacro(name);
  if (capturedMacro !== undefined) {
    expansionEnv.defineMacro(alias, capturedMacro);
  } else {
    const value = macroRules.env.lookupOptional(name);
    if (value !== undefined) {
      expansionEnv.define(alias, value);
    }
  }

  return alias;
}

function replacePatternBindings(
  target: Map<string, PatternBinding>,
  source: Map<string, PatternBinding>,
): void {
  target.clear();
  for (const [name, binding] of source) {
    target.set(name, clonePatternBinding(binding));
  }
}

function clonePatternBindings(
  bindings: Map<string, PatternBinding>,
): Map<string, PatternBinding> {
  const cloned = new Map<string, PatternBinding>();
  for (const [name, binding] of bindings) {
    cloned.set(name, clonePatternBinding(binding));
  }
  return cloned;
}

function clonePatternBinding(binding: PatternBinding): PatternBinding {
  if (binding.kind === 'single') {
    return binding;
  }

  return {
    kind: 'repeated',
    exprs: [...binding.exprs],
  };
}

function equalExprSyntax(left: Expr, right: Expr): boolean {
  if (left.kind !== right.kind) {
    return false;
  }

  switch (left.kind) {
    case 'number':
      return sameNumericSyntax(left.value, (right as NumberExpr).value);
    case 'boolean':
      return left.value === (right as BooleanExpr).value;
    case 'string':
      return left.value === (right as StringExpr).value;
    case 'char':
      return left.value === (right as CharExpr).value;
    case 'symbol':
      return left.name === (right as SymbolExpr).name;
    case 'list': {
      const rightList = right as ListExpr;
      return (
        left.elements.length === rightList.elements.length &&
        left.elements.every((expr, index) => equalExprSyntax(expr, rightList.elements[index]))
      );
    }
  }
}

function isQuoteForm(elements: Expr[]): boolean {
  return elements.length === 2 && elements[0].kind === 'symbol' && elements[0].name === 'quote';
}

function isSpecialFormName(name: string): boolean {
  switch (name) {
    case 'define':
    case 'define-record-type':
    case 'define-syntax':
    case 'set!':
    case 'if':
    case 'quote':
    case 'lambda':
    case 'case-lambda':
    case 'and':
    case 'or':
    case 'let':
    case 'letrec':
    case 'letrec*':
    case 'begin':
    case 'cond':
    case 'case':
    case 'do':
    case 'guard':
    case 'syntax-rules':
    case 'else':
    case '.':
      return true;
    default:
      return false;
  }
}

function freshMacroIdentifier(name: string): string {
  const counter = macroIdentifierCounter;
  macroIdentifierCounter += 1;

  const sanitized = Array.from(name)
    .map((char) => (/^[A-Za-z0-9_]$/.test(char) ? char : '_'))
    .join('');

  return `__macro_${counter}_${sanitized || 'id'}`;
}

function evaluateAndAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  return startShortCircuitAction('and', argExprs, env, stack, pos);
}

function evaluateOrAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  return startShortCircuitAction('or', argExprs, env, stack, pos);
}

function evaluateBeginAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  return startSequenceAction(argExprs, env, stack, pos);
}

function evaluateLetAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
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
  if (bindings.initExprs.length === 0) {
    return finishLetAction(name, bindings.names, [], body, env, stack, pos);
  }

  stack.push({
    kind: 'let-init',
    name,
    bindingNames: bindings.names,
    remainingInitExprs: bindings.initExprs.slice(1),
    collectedValues: [],
    body,
    env,
    pos,
  });
  return { kind: 'expr', expr: bindings.initExprs[0], env };
}

function continueLetInitAction(
  frame: LetInitFrame,
  value: RuntimeValue,
  stack: ContinuationFrame[],
): EvalAction {
  const values = [...frame.collectedValues, value];

  if (frame.remainingInitExprs.length > 0) {
    stack.push({
      kind: 'let-init',
      name: frame.name,
      bindingNames: frame.bindingNames,
      remainingInitExprs: frame.remainingInitExprs.slice(1),
      collectedValues: values,
      body: frame.body,
      env: frame.env,
      pos: frame.pos,
    });
    return { kind: 'expr', expr: frame.remainingInitExprs[0], env: frame.env };
  }

  return finishLetAction(frame.name, frame.bindingNames, values, frame.body, frame.env, stack, frame.pos);
}

function finishLetAction(
  name: string | undefined,
  bindingNames: string[],
  values: RuntimeValue[],
  body: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (name === undefined) {
    const letEnv = new Environment(env);
    for (let index = 0; index < bindingNames.length; index += 1) {
      letEnv.define(bindingNames[index], values[index]);
    }

    return startSequenceAction(body, letEnv, stack, pos);
  }

  const letEnv = new Environment(env);
  const procedure: ClosureProcedure = {
    kind: 'closure',
    params: bindingNames,
    body,
    env: letEnv,
  };
  letEnv.define(name, procedure);
  return applyClosureAction(procedure, values);
}

function evaluateLetStarAction(
  argExprs: Expr[],
  env: Environment,
  context: EvalContext,
): EvalAction {
  if (argExprs.length < 2) {
    throw new EvalError('let* expects bindings and a body');
  }

  const bindings = readLetBindings(argExprs[0]);
  const body = argExprs.slice(1);
  const letEnv = new Environment(env);

  for (let index = 0; index < bindings.names.length; index += 1) {
    letEnv.define(
      bindings.names[index],
      evaluateExprSingle(bindings.initExprs[index], letEnv, context),
    );
  }

  return { kind: 'sequence', exprs: body, env: letEnv };
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

function evaluateLetrecAction(
  argExprs: Expr[],
  env: Environment,
  context: EvalContext,
  sequential: boolean,
): EvalAction {
  if (argExprs.length < 2) {
    throw new EvalError(`${sequential ? 'letrec*' : 'letrec'} expects bindings and a body`);
  }

  const bindings = readLetBindings(argExprs[0]);
  const body = argExprs.slice(1);
  const letEnv = new Environment(env);

  if (sequential) {
    for (let index = 0; index < bindings.names.length; index += 1) {
      const name = bindings.names[index];
      letEnv.define(name, VOID_VALUE);
      letEnv.assign(name, evaluateExprSingle(bindings.initExprs[index], letEnv, context));
    }
  } else {
    for (const name of bindings.names) {
      letEnv.define(name, VOID_VALUE);
    }

    const values = bindings.initExprs.map((expr) => evaluateExprSingle(expr, letEnv, context));
    for (let index = 0; index < bindings.names.length; index += 1) {
      letEnv.assign(bindings.names[index], values[index]);
    }
  }

  return { kind: 'sequence', exprs: body, env: letEnv };
}

function evaluateCondAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  return startCondAction(argExprs, env, stack, pos);
}

function startCondAction(
  clauses: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  pos: SourcePos,
): EvalAction {
  if (clauses.length === 0) {
    return { kind: 'value', value: VOID_VALUE };
  }

  const clauseExpr = clauses[0];
  if (clauseExpr.kind !== 'list' || clauseExpr.elements.length === 0) {
    throw new EvalError('cond clauses must be non-empty lists');
  }

  const [testExpr, ...body] = clauseExpr.elements;
  const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';

  if (isElseClause) {
    if (clauses.length !== 1) {
      throw new EvalError('cond else clause must be last');
    }
    if (body.length === 0) {
      throw new EvalError('cond else clause expects at least 1 expression');
    }
    return startSequenceAction(body, env, stack, pos);
  }

  stack.push({
    kind: 'cond-test',
    body,
    remainingClauses: clauses.slice(1),
    env,
    pos,
  });
  return { kind: 'expr', expr: testExpr, env };
}

function evaluateCaseAction(argExprs: Expr[], env: Environment, context: EvalContext): EvalAction {
  if (argExprs.length < 1) {
    throw new EvalError('case expects a key and at least 1 clause');
  }

  const [keyExpr, ...clauseExprs] = argExprs;
  if (clauseExprs.length === 0) {
    throw new EvalError('case expects at least 1 clause');
  }

  const key = evaluateExprSingle(keyExpr, env, context);

  for (let index = 0; index < clauseExprs.length; index += 1) {
    const clauseExpr = clauseExprs[index];
    if (clauseExpr.kind !== 'list' || clauseExpr.elements.length === 0) {
      throw new EvalError('case clauses must be non-empty lists');
    }

    const [datumExpr, ...body] = clauseExpr.elements;
    const isElseClause = datumExpr.kind === 'symbol' && datumExpr.name === 'else';

    if (isElseClause) {
      if (index !== clauseExprs.length - 1) {
        throw new EvalError('case else clause must be last');
      }
      if (body.length === 0) {
        throw new EvalError('case else clause expects at least 1 expression');
      }
      return { kind: 'sequence', exprs: body, env };
    }

    if (datumExpr.kind !== 'list') {
      throw new EvalError('case clauses must start with a datum list or else');
    }

    for (const datum of datumExpr.elements) {
      if (eqValues(key, quoteExpr(datum))) {
        return body.length === 0
          ? { kind: 'value', value: VOID_VALUE }
          : { kind: 'sequence', exprs: body, env };
      }
    }
  }

  return { kind: 'value', value: VOID_VALUE };
}

function evaluateDoAction(argExprs: Expr[], env: Environment, context: EvalContext): EvalAction {
  if (argExprs.length < 2) {
    throw new EvalError('do expects bindings, a termination clause, and optional body expressions');
  }

  const bindings = readDoBindings(argExprs[0]);
  const terminationExpr = argExprs[1];
  const body = argExprs.slice(2);

  if (terminationExpr.kind !== 'list' || terminationExpr.elements.length === 0) {
    throw new EvalError('do termination clause must be a non-empty list');
  }

  const [testExpr, ...resultExprs] = terminationExpr.elements;
  const initialValues = bindings.map((binding) =>
    evaluateExprSingle(binding.initExpr, env, context),
  );
  const doEnv = new Environment(env);

  for (let index = 0; index < bindings.length; index += 1) {
    doEnv.define(bindings[index].name, initialValues[index]);
  }

  while (true) {
    if (isTruthy(evaluateExprSingle(testExpr, doEnv, context))) {
      return resultExprs.length === 0
        ? { kind: 'value', value: VOID_VALUE }
        : { kind: 'sequence', exprs: resultExprs, env: doEnv };
    }

    if (body.length > 0) {
      evaluateSequenceSingle(body, doEnv, context);
    }

    const nextValues = bindings.map((binding) =>
      binding.stepExpr === undefined
        ? doEnv.lookup(binding.name)
        : evaluateExprSingle(binding.stepExpr, doEnv, context),
    );

    for (let index = 0; index < bindings.length; index += 1) {
      doEnv.assign(bindings[index].name, nextValues[index]);
    }
  }
}

function evaluateGuardAction(
  argExprs: Expr[],
  env: Environment,
  stack: ContinuationFrame[],
  winds: DynamicWindContext[],
  handlers: ExceptionHandlerContext[],
  pos: SourcePos,
): EvalAction {
  if (argExprs.length < 2) {
    throw new EvalError('guard expects a clause list and a body');
  }

  const specExpr = argExprs[0];
  if (specExpr.kind !== 'list') {
    throw new EvalError('guard expects a clause list');
  }

  const [variableExpr, ...clauses] = specExpr.elements;
  if (variableExpr?.kind !== 'symbol') {
    throw new EvalError('guard expects an exception variable');
  }

  const handlerContext: ExceptionHandlerContext = {
    procedure: {
      kind: 'guard-handler',
      variable: variableExpr.name,
      clauses,
      env,
    },
    stack: stack.slice(),
    winds: winds.slice(),
    handlers: handlers.slice(),
    pos,
  };

  stack.push({
    kind: 'exception-handler-return',
    handler: handlerContext,
    pos,
  });
  handlers.push(handlerContext);
  return startSequenceAction(argExprs.slice(1), env, stack, pos);
}

function readDoBindings(
  bindingsExpr: Expr,
): Array<{ name: string; initExpr: Expr; stepExpr?: Expr }> {
  if (bindingsExpr.kind !== 'list') {
    throw new EvalError('do bindings must be a list');
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (
      bindingExpr.kind !== 'list' ||
      bindingExpr.elements.length < 2 ||
      bindingExpr.elements.length > 3
    ) {
      throw new EvalError('do bindings must contain (name init [step]) entries');
    }

    const [nameExpr, initExpr, stepExpr] = bindingExpr.elements;
    if (nameExpr.kind !== 'symbol') {
      throw new EvalError('do binding name must be a symbol');
    }

    return {
      name: nameExpr.name,
      initExpr,
      stepExpr,
    };
  });
}

function applyProcedure(
  procedure: RuntimeValue,
  args: RuntimeValue[],
  context: EvalContext,
  pos?: SourcePos,
): RuntimeValue {
  return runEvaluation({ kind: 'apply', procedure, args, pos }, context);
}

function applyProcedureSingle(
  procedure: RuntimeValue,
  args: RuntimeValue[],
  context: EvalContext,
  pos?: SourcePos,
): RuntimeValue {
  return expectSingleValue(applyProcedure(procedure, args, context, pos), pos);
}

function applyProcedureAction(
  procedure: RuntimeValue,
  args: RuntimeValue[],
  context: EvalContext,
): EvalAction {
  switch (procedure.kind) {
    case 'builtin':
      return { kind: 'value', value: applyBuiltin(procedure.name, args, context) };
    case 'closure':
      return applyClosureAction(procedure, args);
    case 'case-lambda':
      return applyCaseLambdaAction(procedure, args);
    case 'record-constructor':
      return { kind: 'value', value: applyRecordConstructor(procedure, args) };
    case 'record-predicate':
      return { kind: 'value', value: applyRecordPredicate(procedure, args) };
    case 'record-accessor':
      return { kind: 'value', value: applyRecordAccessor(procedure, args) };
    case 'record-mutator':
      return { kind: 'value', value: applyRecordMutator(procedure, args) };
    case 'guard-handler':
      return { kind: 'value', value: applyGuardHandler(procedure, args, context) };
    default:
      throw new EvalError('attempted to call a non-procedure');
  }
}

function applyGuardHandler(
  procedure: GuardHandlerProcedure,
  args: RuntimeValue[],
  context: EvalContext,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('guard handler expects exactly 1 argument');
  }

  const exceptionValue = args[0];
  const guardEnv = new Environment(procedure.env);
  guardEnv.define(procedure.variable, exceptionValue);

  return evaluateGuardClauses(procedure.clauses, guardEnv, context, exceptionValue);
}

function evaluateGuardClauses(
  clauses: Expr[],
  env: Environment,
  context: EvalContext,
  exceptionValue: RuntimeValue,
): RuntimeValue {
  for (let index = 0; index < clauses.length; index += 1) {
    const clauseExpr = clauses[index];
    if (clauseExpr.kind !== 'list' || clauseExpr.elements.length === 0) {
      throw new EvalError('guard clauses must be non-empty lists');
    }

    const [testExpr, ...body] = clauseExpr.elements;
    const isElseClause = testExpr.kind === 'symbol' && testExpr.name === 'else';

    if (isElseClause) {
      if (index !== clauses.length - 1) {
        throw new EvalError('else clause must be last in guard');
      }
      if (body.length === 0) {
        throw new EvalError('else clause requires a body');
      }

      return evaluateSequence(body, env, context);
    }

    const testValue = evaluateExprSingle(testExpr, env, context);
    if (isTruthy(testValue)) {
      return body.length === 0 ? testValue : evaluateSequence(body, env, context);
    }
  }

  throw new RaisedException(exceptionValue);
}

function applyRecordConstructor(
  procedure: RecordConstructorProcedure,
  args: RuntimeValue[],
): RuntimeValue {
  if (args.length !== procedure.fieldIndexes.length) {
    throw new EvalError(`expected ${procedure.fieldIndexes.length} arguments, got ${args.length}`);
  }

  const fields = procedure.recordType.fieldNames.map(() => VOID_VALUE as RuntimeValue);
  for (let index = 0; index < args.length; index += 1) {
    fields[procedure.fieldIndexes[index]] = args[index];
  }

  return {
    kind: 'record',
    recordType: procedure.recordType,
    fields,
  };
}

function applyRecordPredicate(
  procedure: RecordPredicateProcedure,
  args: RuntimeValue[],
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${procedure.name} expects exactly 1 argument`);
  }

  return makeBoolean(args[0].kind === 'record' && args[0].recordType === procedure.recordType);
}

function applyRecordAccessor(
  procedure: RecordAccessorProcedure,
  args: RuntimeValue[],
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${procedure.name} expects exactly 1 argument`);
  }

  return expectRecord(args[0], procedure.name, procedure.recordType).fields[procedure.fieldIndex];
}

function applyRecordMutator(
  procedure: RecordMutatorProcedure,
  args: RuntimeValue[],
): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError(`${procedure.name} expects exactly 2 arguments`);
  }

  expectRecord(args[0], procedure.name, procedure.recordType).fields[procedure.fieldIndex] = args[1];
  return VOID_VALUE;
}

function applyClosureAction(
  procedure: ClosureProcedure,
  args: RuntimeValue[],
): EvalAction {
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

  return { kind: 'sequence', exprs: procedure.body, env: callEnv };
}

function applyCaseLambdaAction(
  procedure: CaseLambdaProcedure,
  args: RuntimeValue[],
): EvalAction {
  for (const clause of procedure.clauses) {
    if (matchesArity(clause, args.length)) {
      return applyClosureAction(clause, args);
    }
  }

  throw new EvalError(`case-lambda: no matching clause for ${args.length} arguments`);
}

function matchesArity(
  procedure: Pick<ClosureProcedure, 'params' | 'restParam'>,
  argCount: number,
): boolean {
  return procedure.restParam === undefined
    ? argCount === procedure.params.length
    : argCount >= procedure.params.length;
}

function evaluateSequence(exprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  return runEvaluation({ kind: 'sequence', exprs, env }, context);
}

function evaluateSequenceSingle(exprs: Expr[], env: Environment, context: EvalContext): RuntimeValue {
  return expectSingleValue(
    evaluateSequence(exprs, env, context),
    exprs[exprs.length - 1]?.pos ?? DEFAULT_SOURCE_POS,
  );
}

function evaluateSequenceAction(exprs: Expr[], env: Environment, context: EvalContext): EvalAction {
  if (exprs.length === 0) {
    return { kind: 'value', value: VOID_VALUE };
  }

  for (let index = 0; index < exprs.length - 1; index += 1) {
    evaluateExprSingle(exprs[index], env, context);
  }

  return { kind: 'expr', expr: exprs[exprs.length - 1], env };
}

function applyBuiltin(name: BuiltinName, args: RuntimeValue[], context: EvalContext): RuntimeValue {
  switch (name) {
    case '+':
      return makeNumber(addNumbers(args.map((arg) => expectNumber(arg, '+'))));
    case '-':
      return applySubtraction(args);
    case '*':
      return makeNumber(multiplyNumbers(args.map((arg) => expectNumber(arg, '*'))));
    case '/':
      return applyDivision(args);
    case 'abs':
      return applyAbs(args);
    case 'modulo':
      return applyIntegerDivision(args, 'modulo', 'modulo');
    case 'remainder':
      return applyIntegerDivision(args, 'remainder', 'remainder');
    case 'quotient':
      return applyIntegerDivision(args, 'quotient', 'quotient');
    case 'min':
      return applyMinMax(args, 'min');
    case 'max':
      return applyMinMax(args, 'max');
    case 'expt':
      return applyExpt(args);
    case 'gcd':
      return applyGcd(args);
    case 'lcm':
      return applyLcm(args);
    case 'truncate':
      return applyTruncate(args);
    case 'round':
      return applyRound(args);
    case '<':
      return applyComparison(args, '<', (comparison) => comparison < 0);
    case '>':
      return applyComparison(args, '>', (comparison) => comparison > 0);
    case '=':
      return applyComparison(args, '=', (comparison) => comparison === 0);
    case '<=':
      return applyComparison(args, '<=', (comparison) => comparison <= 0);
    case '>=':
      return applyComparison(args, '>=', (comparison) => comparison >= 0);
    case 'zero?':
      return applyNumericPredicate(args, 'zero?', (value) => compareNumbers(value, makeExactInteger(0)) === 0);
    case 'positive?':
      return applyNumericPredicate(args, 'positive?', (value) => compareNumbers(value, makeExactInteger(0)) > 0);
    case 'negative?':
      return applyNumericPredicate(args, 'negative?', (value) => compareNumbers(value, makeExactInteger(0)) < 0);
    case 'odd?':
      return applyIntegerPredicate(
        args,
        'odd?',
        (value) =>
          value.exact
            ? value.numerator % 2n !== 0n
            : Math.abs(numberToJsNumber(value) % 2) === 1,
      );
    case 'even?':
      return applyIntegerPredicate(
        args,
        'even?',
        (value) => (value.exact ? value.numerator % 2n === 0n : numberToJsNumber(value) % 2 === 0),
      );
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
    case 'caar':
    case 'cadr':
    case 'cdar':
    case 'cddr':
      return applyCxr(args, name);
    case 'set-car!':
      return applySetPairField(args, 'set-car!', 'car');
    case 'set-cdr!':
      return applySetPairField(args, 'set-cdr!', 'cdr');
    case 'null?':
      if (args.length !== 1) {
        throw new EvalError('null? expects exactly 1 argument');
      }
      return makeBoolean(args[0].kind === 'nil');
    case 'list':
      return buildList(args);
    case 'append':
      return applyAppend(args);
    case 'reverse':
      return applyReverse(args);
    case 'length':
      if (args.length !== 1) {
        throw new EvalError('length expects exactly 1 argument');
      }
      return makeNumber(listLength(args[0]));
    case 'list-ref':
      return applyListRef(args);
    case 'list-tail':
      return applyListTail(args);
    case 'list?':
      return applyTypePredicate(args, 'list?', (value) => isProperList(value));
    case 'memq':
      return applyMember(args, 'memq', eqValues);
    case 'memv':
      return applyMember(args, 'memv', eqValues);
    case 'member':
      return applyMember(args, 'member', equalValues);
    case 'assq':
      return applyAssocWith(args, 'assq', eqValues);
    case 'assv':
      return applyAssocWith(args, 'assv', eqValues);
    case 'assoc':
      return applyAssoc(args);
    case 'map':
      return applyMap(args, context);
    case 'for-each':
      return applyForEach(args, context);
    case 'string?':
      return applyTypePredicate(args, 'string?', (value) => value.kind === 'string');
    case 'make-string':
      return applyMakeString(args);
    case 'string':
      return applyString(args);
    case 'number?':
      return applyTypePredicate(args, 'number?', (value) => value.kind === 'number');
    case 'exact?':
      return applyTypePredicate(args, 'exact?', (value) => value.kind === 'number' && value.value.exact);
    case 'inexact?':
      return applyTypePredicate(args, 'inexact?', (value) => value.kind === 'number' && !value.value.exact);
    case 'integer?':
      return applyTypePredicate(args, 'integer?', (value) => value.kind === 'number' && isIntegerNumber(value.value));
    case 'rational?':
      return applyTypePredicate(args, 'rational?', (value) => value.kind === 'number' && isRationalNumber(value.value));
    case 'boolean?':
      return applyTypePredicate(args, 'boolean?', (value) => value.kind === 'boolean');
    case 'pair?':
      return applyTypePredicate(args, 'pair?', (value) => value.kind === 'pair');
    case 'symbol?':
      return applyTypePredicate(args, 'symbol?', (value) => value.kind === 'symbol');
    case 'procedure?':
      return applyTypePredicate(args, 'procedure?', (value) => isCallableValue(value));
    case 'eq?':
      if (args.length !== 2) {
        throw new EvalError('eq? expects exactly 2 arguments');
      }
      return makeBoolean(eqValues(args[0], args[1]));
    case 'eqv?':
      if (args.length !== 2) {
        throw new EvalError('eqv? expects exactly 2 arguments');
      }
      return makeBoolean(eqValues(args[0], args[1]));
    case 'equal?':
      if (args.length !== 2) {
        throw new EvalError('equal? expects exactly 2 arguments');
      }
      return makeBoolean(equalValues(args[0], args[1]));
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
      return makeString(formatSchemeNumber(expectNumber(args[0], 'number->string')));
    case 'exact->inexact':
      if (args.length !== 1) {
        throw new EvalError('exact->inexact expects exactly 1 argument');
      }
      return makeNumber(exactToInexact(expectNumber(args[0], 'exact->inexact')));
    case 'inexact->exact':
      if (args.length !== 1) {
        throw new EvalError('inexact->exact expects exactly 1 argument');
      }
      return makeNumber(inexactToExact(expectNumber(args[0], 'inexact->exact')));
    case 'numerator':
      if (args.length !== 1) {
        throw new EvalError('numerator expects exactly 1 argument');
      }
      return makeNumber(numeratorValue(expectNumber(args[0], 'numerator')));
    case 'denominator':
      if (args.length !== 1) {
        throw new EvalError('denominator expects exactly 1 argument');
      }
      return makeNumber(denominatorValue(expectNumber(args[0], 'denominator')));
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
      return makeString(expectStringValue(args[0], 'string-copy').value, !context.immutableStrings);
    case 'string-set!':
      return applyStringSet(args);
    case 'string->list':
      return applyStringToList(args);
    case 'list->string':
      return applyListToString(args);
    case 'string=?':
      return applyStringComparison(args, 'string=?', (value) => value, (left, right) => left === right);
    case 'string<?':
      return applyStringComparison(args, 'string<?', (value) => value, (left, right) => left < right);
    case 'string>?':
      return applyStringComparison(args, 'string>?', (value) => value, (left, right) => left > right);
    case 'string<=?':
      return applyStringComparison(args, 'string<=?', (value) => value, (left, right) => left <= right);
    case 'string>=?':
      return applyStringComparison(args, 'string>=?', (value) => value, (left, right) => left >= right);
    case 'string-ci=?':
      return applyStringComparison(
        args,
        'string-ci=?',
        (value) => value.toLocaleLowerCase(),
        (left, right) => left === right,
      );
    case 'string-upcase':
      return applyStringCase(args, 'string-upcase', (value) => value.toLocaleUpperCase());
    case 'string-downcase':
      return applyStringCase(args, 'string-downcase', (value) => value.toLocaleLowerCase());
    case 'char?':
      return applyTypePredicate(args, 'char?', (value) => value.kind === 'char');
    case 'char-alphabetic?':
      return applyCharPredicate(args, 'char-alphabetic?', (value) => ALPHABETIC_CHAR_RE.test(value));
    case 'char-numeric?':
      return applyCharPredicate(args, 'char-numeric?', (value) => NUMERIC_CHAR_RE.test(value));
    case 'char->integer':
      if (args.length !== 1) {
        throw new EvalError('char->integer expects exactly 1 argument');
      }
      return makeNumber(charCodePoint(expectChar(args[0], 'char->integer').value));
    case 'integer->char':
      if (args.length !== 1) {
        throw new EvalError('integer->char expects exactly 1 argument');
      }
      return makeChar(integerToChar(expectInteger(args[0], 'integer->char'), 'integer->char'));
    case 'char-upcase':
      return applyCharCase(args, 'char-upcase', (value) => value.toLocaleUpperCase());
    case 'char-downcase':
      return applyCharCase(args, 'char-downcase', (value) => value.toLocaleLowerCase());
    case 'char=?':
      return applyCharComparison(args, 'char=?', (left, right) => left === right);
    case 'char<?':
      return applyCharComparison(args, 'char<?', (left, right) => left < right);
    case 'vector':
      return { kind: 'vector', elements: [...args] };
    case 'make-vector':
      return applyMakeVector(args);
    case 'vector-ref':
      return applyVectorRef(args);
    case 'vector-set!':
      return applyVectorSet(args);
    case 'vector-length':
      if (args.length !== 1) {
        throw new EvalError('vector-length expects exactly 1 argument');
      }
      return makeNumber(expectVector(args[0], 'vector-length').elements.length);
    case 'vector?':
      return applyTypePredicate(args, 'vector?', (value) => value.kind === 'vector');
    case 'vector->list':
      if (args.length !== 1) {
        throw new EvalError('vector->list expects exactly 1 argument');
      }
      return buildList([...expectVector(args[0], 'vector->list').elements]);
    case 'list->vector':
      if (args.length !== 1) {
        throw new EvalError('list->vector expects exactly 1 argument');
      }
      return { kind: 'vector', elements: listToArray(args[0], 'list->vector') };
    case 'apply':
      return applyApply(args, context);
    case 'values':
      return makeValuesResult(args);
    case 'call-with-values':
    case 'call/cc':
    case 'call-with-current-continuation':
    case 'dynamic-wind':
    case 'with-exception-handler':
      throw new EvalError('internal error: continuation application must be handled by the evaluator');
    case 'raise':
      return applyRaise(args);
    case 'error':
      return applyError(args);
  }
}

function applyCxr(args: RuntimeValue[], name: 'caar' | 'cadr' | 'cdar' | 'cddr'): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  let current = args[0];
  const operations = name.slice(1, -1);

  for (let index = operations.length - 1; index >= 0; index -= 1) {
    const pair = expectPair(current, name);
    current = operations[index] === 'a' ? pair.car : pair.cdr;
  }

  return current;
}

function applySetPairField(
  args: RuntimeValue[],
  name: 'set-car!' | 'set-cdr!',
  field: 'car' | 'cdr',
): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError(`${name} expects exactly 2 arguments`);
  }

  const pair = expectPair(args[0], name);
  pair[field] = args[1];
  return VOID_VALUE;
}

function applyMakeVector(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1 && args.length !== 2) {
    throw new EvalError('make-vector expects 1 or 2 arguments');
  }

  const length = expectIndex(args[0], 'make-vector');
  const fill = args[1] ?? VOID_VALUE;
  return {
    kind: 'vector',
    elements: Array.from({ length }, () => fill),
  };
}

function applyVectorRef(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError('vector-ref expects exactly 2 arguments');
  }

  const vector = expectVector(args[0], 'vector-ref');
  const index = expectIndex(args[1], 'vector-ref');
  if (index >= vector.elements.length) {
    throw new EvalError('vector-ref index out of range');
  }

  return vector.elements[index];
}

function applyVectorSet(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 3) {
    throw new EvalError('vector-set! expects exactly 3 arguments');
  }

  const vector = expectVector(args[0], 'vector-set!');
  const index = expectIndex(args[1], 'vector-set!');
  if (index >= vector.elements.length) {
    throw new EvalError('vector-set! index out of range');
  }

  vector.elements[index] = args[2];
  return VOID_VALUE;
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

function applyAbs(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('abs expects exactly 1 argument');
  }

  return makeNumber(absNumber(expectNumber(args[0], 'abs')));
}

function applyIntegerDivision(
  args: RuntimeValue[],
  name: 'modulo' | 'remainder' | 'quotient',
  operation: 'modulo' | 'remainder' | 'quotient',
): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError(`${name} expects exactly 2 arguments`);
  }

  return makeNumber(integerDivision(expectInteger(args[0], name), expectInteger(args[1], name), operation));
}

function applyMinMax(args: RuntimeValue[], name: 'min' | 'max'): RuntimeValue {
  if (args.length === 0) {
    throw new EvalError(`${name} expects at least 1 argument`);
  }

  const numbers = args.map((arg) => expectNumber(arg, name));
  return makeNumber(name === 'min' ? minNumbers(numbers) : maxNumbers(numbers));
}

function applyExpt(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError('expt expects exactly 2 arguments');
  }

  const base = expectNumber(args[0], 'expt');
  const exponent = expectInteger(args[1], 'expt');
  return makeNumber(exptNumber(base, exponent));
}

function applyGcd(args: RuntimeValue[]): RuntimeValue {
  if (args.length === 0) {
    return makeNumber(makeExactInteger(0));
  }

  const values = args.map((arg) => expectInteger(arg, 'gcd'));
  if (values.every((value) => value.exact)) {
    let result = 0n;
    for (const value of values) {
      result = bigintGcd(result, bigintAbs(value.numerator));
    }
    return makeNumber(makeExactInteger(result));
  }

  let result = 0;
  for (const value of values) {
    result = jsGcd(result, Math.abs(numberToJsNumber(value)));
  }
  return makeNumber(makeInexact(result));
}

function applyLcm(args: RuntimeValue[]): RuntimeValue {
  if (args.length === 0) {
    return makeNumber(makeExactInteger(1));
  }

  const values = args.map((arg) => expectInteger(arg, 'lcm'));
  if (values.every((value) => value.exact)) {
    let result = 1n;
    for (const value of values) {
      const current = bigintAbs(value.numerator);
      result = bigintLcm(result, current);
    }
    return makeNumber(makeExactInteger(result));
  }

  let result = 1;
  for (const value of values) {
    result = jsLcm(result, Math.abs(numberToJsNumber(value)));
  }
  return makeNumber(makeInexact(result));
}

function applyTruncate(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('truncate expects exactly 1 argument');
  }

  const value = expectNumber(args[0], 'truncate');
  if (value.exact) {
    return makeNumber(makeExactInteger(value.numerator / value.denominator));
  }

  return makeNumber(makeInexact(Math.trunc(value.value)));
}

function applyRound(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('round expects exactly 1 argument');
  }

  const value = expectNumber(args[0], 'round');
  if (value.exact) {
    return makeNumber(makeExactInteger(roundExactRational(value.numerator, value.denominator)));
  }

  return makeNumber(makeInexact(roundToEven(value.value)));
}

function applyNumericPredicate(
  args: RuntimeValue[],
  name: string,
  predicate: (value: SchemeNumber) => boolean,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  return makeBoolean(predicate(expectNumber(args[0], name)));
}

function applyIntegerPredicate(
  args: RuntimeValue[],
  name: string,
  predicate: (value: SchemeNumber) => boolean,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  return makeBoolean(predicate(expectInteger(args[0], name)));
}

function applySubtraction(args: RuntimeValue[]): RuntimeValue {
  if (args.length === 0) {
    throw new EvalError('- expects at least 1 argument');
  }

  return makeNumber(subtractNumbers(args.map((arg) => expectNumber(arg, '-'))));
}

function applyDivision(args: RuntimeValue[]): RuntimeValue {
  if (args.length === 0) {
    throw new EvalError('/ expects at least 1 argument');
  }

  return makeNumber(divideNumbers(args.map((arg) => expectNumber(arg, '/'))));
}

function applyComparison(
  args: RuntimeValue[],
  name: string,
  predicate: (comparison: -1 | 0 | 1) => boolean,
): RuntimeValue {
  if (args.length < 2) {
    throw new EvalError(`${name} expects at least 2 arguments`);
  }

  const numbers = args.map((arg) => expectNumber(arg, name));
  for (let index = 0; index < numbers.length - 1; index += 1) {
    if (!predicate(compareNumbers(numbers[index], numbers[index + 1]))) {
      return makeBoolean(false);
    }
  }

  return makeBoolean(true);
}

function applyStringAppend(args: RuntimeValue[]): RuntimeValue {
  return makeString(args.map((arg) => expectStringValue(arg, 'string-append').value).join(''));
}

function applyListRef(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError('list-ref expects exactly 2 arguments');
  }

  const tail = getListTail(args[0], expectIndex(args[1], 'list-ref'), 'list-ref');
  if (tail.kind !== 'pair') {
    throw new EvalError('list-ref index out of range');
  }

  return tail.car;
}

function applyListTail(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError('list-tail expects exactly 2 arguments');
  }

  return getListTail(args[0], expectIndex(args[1], 'list-tail'), 'list-tail');
}

function applyMember(
  args: RuntimeValue[],
  name: 'memq' | 'memv' | 'member',
  matches: (left: RuntimeValue, right: RuntimeValue) => boolean,
): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError(`${name} expects exactly 2 arguments`);
  }

  const [target, list] = args;
  let current = list;

  while (current.kind === 'pair') {
    if (matches(target, current.car)) {
      return current;
    }

    current = current.cdr;
  }

  if (current.kind !== 'nil') {
    throw new EvalError(`${name} expects a proper list`);
  }

  return makeBoolean(false);
}

function applyAssoc(args: RuntimeValue[]): RuntimeValue {
  return applyAssocWith(args, 'assoc', equalValues);
}

function applyAssocWith(
  args: RuntimeValue[],
  name: 'assq' | 'assv' | 'assoc',
  matches: (left: RuntimeValue, right: RuntimeValue) => boolean,
): RuntimeValue {
  if (args.length !== 2) {
    throw new EvalError(`${name} expects exactly 2 arguments`);
  }

  const [key, alist] = args;
  let current = alist;

  while (current.kind === 'pair') {
    const entry = current.car;
    if (entry.kind !== 'pair') {
      throw new EvalError(`${name} expects an association list`);
    }

    if (matches(key, entry.car)) {
      return entry;
    }

    current = current.cdr;
  }

  if (current.kind !== 'nil') {
    throw new EvalError(`${name} expects a proper list`);
  }

  return makeBoolean(false);
}

function applyMap(args: RuntimeValue[], context: EvalContext): RuntimeValue {
  if (args.length < 2) {
    throw new EvalError('map expects at least 2 arguments');
  }

  const [procedure, ...listArgs] = args;
  const lists = listArgs.map((listArg) => listToArray(listArg, 'map'));
  const resultLength = lists[0].length;

  for (const list of lists) {
    if (list.length !== resultLength) {
      throw new EvalError('map expects lists of equal length');
    }
  }

  const results: RuntimeValue[] = [];
  for (let index = 0; index < resultLength; index += 1) {
    results.push(applyProcedureSingle(procedure, lists.map((list) => list[index]), context));
  }

  return buildList(results);
}

function applyForEach(args: RuntimeValue[], context: EvalContext): RuntimeValue {
  if (args.length < 2) {
    throw new EvalError('for-each expects at least 2 arguments');
  }

  const [procedure, ...listArgs] = args;
  const lists = listArgs.map((listArg) => listToArray(listArg, 'for-each'));
  const resultLength = lists[0].length;

  for (const list of lists) {
    if (list.length !== resultLength) {
      throw new EvalError('for-each expects lists of equal length');
    }
  }

  for (let index = 0; index < resultLength; index += 1) {
    applyProcedureSingle(procedure, lists.map((list) => list[index]), context);
  }

  return VOID_VALUE;
}

function applyMakeString(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1 && args.length !== 2) {
    throw new EvalError('make-string expects 1 or 2 arguments');
  }

  const length = expectIndex(args[0], 'make-string');
  const fill = args[1] === undefined ? ' ' : expectChar(args[1], 'make-string').value;
  return makeString(Array.from({ length }, () => fill).join(''));
}

function applyString(args: RuntimeValue[]): RuntimeValue {
  return makeString(args.map((arg) => expectChar(arg, 'string').value).join(''));
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

  const stringValue = expectStringValue(args[0], 'string-set!');
  const index = expectIndex(args[1], 'string-set!');
  const replacement = expectChar(args[2], 'string-set!');

  if (!stringValue.mutable) {
    throw new EvalError('string-set! is not supported on immutable strings');
  }

  const chars = stringChars(stringValue.value);
  if (index >= chars.length) {
    throw new EvalError('string-set! index out of range');
  }

  chars[index] = replacement.value;
  stringValue.value = chars.join('');
  return VOID_VALUE;
}

function applyStringToList(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('string->list expects exactly 1 argument');
  }

  return buildList(
    stringChars(expectStringValue(args[0], 'string->list').value).map((char) => makeChar(char)),
  );
}

function applyListToString(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('list->string expects exactly 1 argument');
  }

  return makeString(
    listToArray(args[0], 'list->string')
      .map((value) => expectChar(value, 'list->string').value)
      .join(''),
  );
}

function applyStringComparison(
  args: RuntimeValue[],
  name: string,
  normalize: (value: string) => string,
  predicate: (left: string, right: string) => boolean,
): RuntimeValue {
  if (args.length < 2) {
    throw new EvalError(`${name} expects at least 2 arguments`);
  }

  const strings = args.map((arg) => normalize(expectStringValue(arg, name).value));
  for (let index = 0; index < strings.length - 1; index += 1) {
    if (!predicate(strings[index], strings[index + 1])) {
      return makeBoolean(false);
    }
  }

  return makeBoolean(true);
}

function applyStringCase(
  args: RuntimeValue[],
  name: string,
  transform: (value: string) => string,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  return makeString(transform(expectStringValue(args[0], name).value));
}

function applyCharPredicate(
  args: RuntimeValue[],
  name: string,
  predicate: (value: string) => boolean,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  return makeBoolean(predicate(expectChar(args[0], name).value));
}

function applyCharCase(
  args: RuntimeValue[],
  name: string,
  transform: (value: string) => string,
): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError(`${name} expects exactly 1 argument`);
  }

  return makeChar(transform(expectChar(args[0], name).value));
}

function applyCharComparison(
  args: RuntimeValue[],
  name: string,
  predicate: (left: number, right: number) => boolean,
): RuntimeValue {
  if (args.length < 2) {
    throw new EvalError(`${name} expects at least 2 arguments`);
  }

  const codePoints = args.map((arg) => charCodePoint(expectChar(arg, name).value));
  for (let index = 0; index < codePoints.length - 1; index += 1) {
    if (!predicate(codePoints[index], codePoints[index + 1])) {
      return makeBoolean(false);
    }
  }

  return makeBoolean(true);
}

function expectNumber(value: RuntimeValue, procedure: string): SchemeNumber {
  if (value.kind !== 'number') {
    throw new EvalError(`${procedure} expects numeric arguments`);
  }
  return value.value;
}

function expectInteger(value: RuntimeValue, procedure: string): SchemeNumber {
  const numericValue = expectNumber(value, procedure);
  if (!isIntegerNumber(numericValue)) {
    throw new EvalError(`${procedure} expects integer arguments`);
  }

  return numericValue;
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
  if (!isIntegerNumber(numericValue) || compareNumbers(numericValue, makeExactInteger(0)) < 0) {
    throw new EvalError(`${procedure} expects a non-negative integer index`);
  }

  const index = numberToJsNumber(numericValue);
  if (!Number.isSafeInteger(index)) {
    throw new EvalError(`${procedure} index is too large`);
  }

  return index;
}

function expectPair(value: RuntimeValue, procedure: string): PairValue {
  if (value.kind !== 'pair') {
    throw new EvalError(`${procedure} expects a pair`);
  }
  return value;
}

function expectVector(value: RuntimeValue, procedure: string): VectorValue {
  if (value.kind !== 'vector') {
    throw new EvalError(`${procedure} expects a vector`);
  }

  return value;
}

function expectRecord(
  value: RuntimeValue,
  procedure: string,
  recordType: RecordTypeDescriptor,
): RecordValue {
  if (value.kind !== 'record' || value.recordType !== recordType) {
    throw new EvalError(`${procedure} expects a ${recordType.name} record`);
  }

  return value;
}

function expectSymbolExpr(expr: Expr, context: string): string {
  if (expr.kind !== 'symbol') {
    throw new EvalError(`${context} must be a symbol`);
  }

  return expr.name;
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

function applyReverse(args: RuntimeValue[]): RuntimeValue {
  if (args.length !== 1) {
    throw new EvalError('reverse expects exactly 1 argument');
  }

  return buildList(listToArray(args[0], 'reverse').reverse());
}

function listLength(value: RuntimeValue): number {
  return listToArray(value, 'length').length;
}

function getListTail(value: RuntimeValue, index: number, procedure: string): RuntimeValue {
  let current = value;

  for (let remaining = index; remaining > 0; remaining -= 1) {
    if (current.kind === 'pair') {
      current = current.cdr;
      continue;
    }

    if (current.kind === 'nil') {
      throw new EvalError(`${procedure} index out of range`);
    }

    throw new EvalError(`${procedure} expects a proper list`);
  }

  if (current.kind !== 'pair' && current.kind !== 'nil') {
    throw new EvalError(`${procedure} expects a proper list`);
  }

  return current;
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

function isProperList(value: RuntimeValue): boolean {
  const seen = new Set<PairValue>();
  let current = value;

  while (current.kind === 'pair') {
    if (seen.has(current)) {
      return false;
    }

    seen.add(current);
    current = current.cdr;
  }

  return current.kind === 'nil';
}

function eqValues(left: RuntimeValue, right: RuntimeValue): boolean {
  if (left.kind === 'number' && right.kind === 'number') {
    return equalNumbers(left.value, right.value);
  }

  if (left.kind === 'boolean' && right.kind === 'boolean') {
    return left.value === right.value;
  }

  if (left.kind === 'string' && right.kind === 'string') {
    return left.value === right.value;
  }

  if (left.kind === 'char' && right.kind === 'char') {
    return left.value === right.value;
  }

  if (left.kind === 'symbol' && right.kind === 'symbol') {
    return left.name === right.name;
  }

  if (left.kind === 'nil' && right.kind === 'nil') {
    return true;
  }

  if (left.kind === 'builtin' && right.kind === 'builtin') {
    return left.name === right.name;
  }

  if (left.kind === 'void' && right.kind === 'void') {
    return true;
  }

  return left === right;
}

function equalValues(
  left: RuntimeValue,
  right: RuntimeValue,
  seen: WeakMap<object, WeakSet<object>> = new WeakMap(),
): boolean {
  if (left === right) {
    return true;
  }

  if (left.kind === 'pair' && right.kind === 'pair') {
    let seenRights = seen.get(left);
    if (seenRights?.has(right)) {
      return true;
    }

    if (seenRights === undefined) {
      seenRights = new WeakSet<PairValue>();
      seen.set(left, seenRights);
    }

    seenRights.add(right);
    return equalValues(left.car, right.car, seen) && equalValues(left.cdr, right.cdr, seen);
  }

  if (left.kind === 'vector' && right.kind === 'vector') {
    if (left.elements.length !== right.elements.length) {
      return false;
    }

    let seenRights = seen.get(left);
    if (seenRights?.has(right)) {
      return true;
    }

    if (seenRights === undefined) {
      seenRights = new WeakSet<object>();
      seen.set(left, seenRights);
    }

    seenRights.add(right);
    for (let index = 0; index < left.elements.length; index += 1) {
      if (!equalValues(left.elements[index], right.elements[index], seen)) {
        return false;
      }
    }

    return true;
  }

  if (left.kind === 'string' && right.kind === 'string') {
    return left.value === right.value;
  }

  return eqValues(left, right);
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

function isCallableValue(value: RuntimeValue): boolean {
  switch (value.kind) {
    case 'builtin':
    case 'closure':
    case 'case-lambda':
    case 'record-constructor':
    case 'record-predicate':
    case 'record-accessor':
    case 'record-mutator':
    case 'continuation':
    case 'guard-handler':
      return true;
    default:
      return false;
  }
}

function isTruthy(value: RuntimeValue): boolean {
  return value.kind !== 'boolean' || value.value;
}

function makeValuesResult(values: RuntimeValue[]): RuntimeValue {
  if (values.length === 1) {
    return values[0];
  }

  return {
    kind: 'multiple-values',
    values: [...values],
  };
}

function unwrapValues(value: RuntimeValue): RuntimeValue[] {
  return value.kind === 'multiple-values' ? [...value.values] : [value];
}

function expectSingleValue(value: RuntimeValue, pos?: SourcePos): RuntimeValue {
  if (value.kind !== 'multiple-values') {
    return value;
  }

  if (value.values.length === 1) {
    return value.values[0];
  }

  throw new EvalError(`expected 1 value, got ${value.values.length}`, pos);
}

function makeNumber(value: SchemeNumber | number): NumberExpr {
  return {
    kind: 'number',
    pos: DEFAULT_SOURCE_POS,
    value: typeof value === 'number' ? makeExactInteger(value) : value,
  };
}

function makeBoolean(value: boolean): BooleanExpr {
  return {
    kind: 'boolean',
    pos: DEFAULT_SOURCE_POS,
    value,
  };
}

function makeString(value: string, mutable = false): StringValue {
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
  return formatValueWithModeInternal(value, mode, new Set<object>());
}

function formatValueWithModeInternal(
  value: RuntimeValue,
  mode: 'write' | 'display',
  seen: Set<object>,
): string {
  switch (value.kind) {
    case 'number':
      return formatSchemeNumber(value.value);
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
      return formatPair(value, mode, seen);
    case 'vector':
      return formatVector(value, mode, seen);
    case 'record':
      return `#<record ${value.recordType.name}>`;
    case 'builtin':
    case 'closure':
    case 'case-lambda':
    case 'record-constructor':
    case 'record-predicate':
    case 'record-accessor':
    case 'record-mutator':
    case 'continuation':
    case 'guard-handler':
      return '#<procedure>';
    case 'multiple-values':
      return formatValueWithModeInternal(expectSingleValue(value), mode, seen);
    case 'void':
      return '';
  }
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

function formatPair(value: PairValue, mode: 'write' | 'display', seen: Set<object>): string {
  if (seen.has(value)) {
    return '#<cycle>';
  }

  const added: PairValue[] = [value];
  const parts: string[] = [];
  let current: RuntimeValue = value;
  seen.add(value);

  try {
    while (current.kind === 'pair') {
      parts.push(formatValueWithModeInternal(current.car, mode, seen));
      const next: RuntimeValue = current.cdr;

      if (next.kind === 'pair') {
        if (seen.has(next)) {
          return `(${parts.join(' ')} . #<cycle>)`;
        }

        seen.add(next);
        added.push(next);
      }

      current = next;
    }

    if (current.kind === 'nil') {
      return `(${parts.join(' ')})`;
    }

    return `(${parts.join(' ')} . ${formatValueWithModeInternal(current, mode, seen)})`;
  } finally {
    for (let index = added.length - 1; index >= 0; index -= 1) {
      seen.delete(added[index]);
    }
  }
}

function formatVector(value: VectorValue, mode: 'write' | 'display', seen: Set<object>): string {
  if (seen.has(value)) {
    return '#<cycle>';
  }

  seen.add(value);
  try {
    return `#(${value.elements
      .map((element) => formatValueWithModeInternal(element, mode, seen))
      .join(' ')})`;
  } finally {
    seen.delete(value);
  }
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

function charCodePoint(value: string): number {
  const codePoint = value.codePointAt(0);
  if (codePoint === undefined) {
    throw new EvalError('character values must contain exactly 1 character');
  }

  return codePoint;
}

function integerToChar(value: SchemeNumber, procedure: string): string {
  const codePoint = numberToJsNumber(value);
  if (!Number.isSafeInteger(codePoint)) {
    throw new EvalError(`${procedure} expects a valid character code point`);
  }

  if (codePoint < 0 || codePoint > 0x10ffff || (codePoint >= 0xd800 && codePoint <= 0xdfff)) {
    throw new EvalError(`${procedure} expects a valid character code point`);
  }

  const char = String.fromCodePoint(codePoint);
  if (stringChars(char).length !== 1) {
    throw new EvalError(`${procedure} expects a valid character code point`);
  }

  return char;
}

function parseNumberString(value: string): RuntimeValue {
  const parsed = parseNumberStringValue(value);
  if (parsed !== undefined) {
    return makeNumber(parsed);
  }

  return makeBoolean(false);
}

function bigintAbs(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function bigintGcd(left: bigint, right: bigint): bigint {
  let a = bigintAbs(left);
  let b = bigintAbs(right);

  while (b !== 0n) {
    const next = a % b;
    a = b;
    b = next;
  }

  return a;
}

function bigintLcm(left: bigint, right: bigint): bigint {
  if (left === 0n || right === 0n) {
    return 0n;
  }

  return (bigintAbs(left) / bigintGcd(left, right)) * bigintAbs(right);
}

function jsGcd(left: number, right: number): number {
  let a = Math.abs(Math.trunc(left));
  let b = Math.abs(Math.trunc(right));

  while (b !== 0) {
    const next = a % b;
    a = b;
    b = next;
  }

  return a;
}

function jsLcm(left: number, right: number): number {
  const a = Math.abs(Math.trunc(left));
  const b = Math.abs(Math.trunc(right));

  if (a === 0 || b === 0) {
    return 0;
  }

  return (a / jsGcd(a, b)) * b;
}

function roundExactRational(numerator: bigint, denominator: bigint): bigint {
  const quotient = numerator / denominator;
  const remainder = numerator % denominator;
  const doubledRemainder = bigintAbs(remainder) * 2n;

  if (doubledRemainder < denominator) {
    return quotient;
  }

  if (doubledRemainder > denominator) {
    return quotient + (numerator >= 0n ? 1n : -1n);
  }

  if (quotient % 2n === 0n) {
    return quotient;
  }

  return quotient + (numerator >= 0n ? 1n : -1n);
}

function roundToEven(value: number): number {
  if (!Number.isFinite(value)) {
    return value;
  }

  const truncated = Math.trunc(value);
  const fractional = Math.abs(value - truncated);

  if (fractional < 0.5) {
    return truncated;
  }

  if (fractional > 0.5) {
    return truncated + (value >= 0 ? 1 : -1);
  }

  if (truncated % 2 === 0) {
    return truncated;
  }

  return truncated + (value >= 0 ? 1 : -1);
}

function applyRaise(args: RuntimeValue[]): never {
  if (args.length !== 1) {
    throw new EvalError('raise expects exactly 1 argument');
  }

  throw new RaisedException(args[0]);
}

function applyError(args: RuntimeValue[]): never {
  if (args.length === 0) {
    throw new EvalError('error');
  }

  throw new EvalError(args.map((arg) => formatDisplayValue(arg)).join(' '));
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
