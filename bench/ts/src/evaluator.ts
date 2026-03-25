import { EvalError, type SourcePosition } from './evalError.js';
import {
  absNumber,
  addNumbers,
  compareNumbers,
  denominatorPart,
  divideNumbers,
  exactInteger,
  exactToInexact,
  exptNumber,
  formatNumber,
  inexactNumber,
  inexactToExact,
  integerToJs,
  isExactNumber,
  isInexactNumber,
  isIntegerNumber,
  isNegativeNumber,
  isPositiveNumber,
  isRationalNumber,
  isZeroNumber,
  maxNumber,
  minNumber,
  multiplyNumbers,
  numeratorPart,
  parseNumberLiteral,
  subtractNumbers,
  type SchemeNumber,
} from './numbers.js';

type Token =
  | { kind: 'paren'; value: '(' | ')'; position: SourcePosition }
  | { kind: 'atom'; value: string; position: SourcePosition }
  | { kind: 'string'; value: string; position: SourcePosition }
  | { kind: 'quote'; position: SourcePosition }
  | { kind: 'quasiquote'; position: SourcePosition }
  | { kind: 'unquote'; position: SourcePosition }
  | { kind: 'unquote-splicing'; position: SourcePosition }
  | { kind: 'syntax-quote'; position: SourcePosition };

type Expr =
  | { type: 'number'; value: SchemeNumber; position: SourcePosition; introduced?: boolean }
  | { type: 'boolean'; value: boolean; position: SourcePosition; introduced?: boolean }
  | { type: 'string'; value: string; position: SourcePosition; introduced?: boolean }
  | { type: 'char'; value: string; position: SourcePosition; introduced?: boolean }
  | {
      type: 'symbol';
      name: string;
      position: SourcePosition;
      introduced?: boolean;
      resolvedName?: string;
      capturedCell?: BindingCell;
      capturedSyntax?: MacroBinding;
    }
  | { type: 'list'; elements: Expr[]; position: SourcePosition; introduced?: boolean };

type EvaluatedArg = {
  value: SchemeValue;
  position: SourcePosition;
};

type EvalStep =
  | { kind: 'suspend'; thunk: () => EvalStep }
  | { kind: 'done'; value: SchemeValue };

type EvalContinuation = (value: SchemeValue) => EvalStep;

type BuiltinProcedure = {
  type: 'builtin';
  name: string;
  invoke: (args: EvaluatedArg[], callPosition: SourcePosition) => SchemeValue;
  invokeCps?: (
    args: EvaluatedArg[],
    callPosition: SourcePosition,
    continuation: EvalContinuation,
  ) => EvalStep;
};

type Closure = {
  type: 'closure';
  params: string[];
  restParam?: string;
  body: Expr[];
  env: Environment;
};

type CaseClosureClause = {
  params: string[];
  restParam?: string;
  body: Expr[];
};

type CaseClosure = {
  type: 'case-closure';
  clauses: CaseClosureClause[];
  env: Environment;
};

type BindingCell = {
  value: SchemeValue;
  initialized: boolean;
};

type SyntaxRule = {
  pattern: Expr;
  template: Expr;
};

type SyntaxRulesMacro = {
  type: 'syntax-rules';
  name: string;
  ellipsis: string;
  literals: ReadonlySet<string>;
  rules: SyntaxRule[];
  env: Environment;
};

type ProcedureMacro = {
  type: 'procedure-macro';
  name: string;
  transformer: SchemeValue;
  env: Environment;
};

type MacroBinding = SyntaxRulesMacro | ProcedureMacro;
type PatternMatcher = {
  ellipsis: string;
  literals: ReadonlySet<string>;
};
type TemplateContext = {
  ellipsis: string;
  name: string;
};

type RecordTypeDescriptor = {
  name: string;
  displayName: string;
  fieldTags: string[];
};

type RecordFieldSpec = {
  tag: string;
  accessorName: SymbolExpr;
  mutatorName?: SymbolExpr;
};

type DynamicWindFrame = {
  inThunk: SchemeValue;
  outThunk: SchemeValue;
};

type StackNode<T> = {
  value: T;
  parent?: StackNode<T>;
  depth: number;
};

type DynamicWindStack = StackNode<DynamicWindFrame> | undefined;

type ExceptionHandlerFrame = {
  windStack: DynamicWindStack;
  handle: (exception: SchemeValue, raisePosition: SourcePosition) => EvalStep;
};

type ExceptionHandlerStack = StackNode<ExceptionHandlerFrame> | undefined;

type EvaluationContext = {
  output: string[];
  dynamicWindStack: DynamicWindStack;
  exceptionHandlers: ExceptionHandlerStack;
};

type PairCell = {
  car: SchemeValue;
  cdr: SchemeValue;
};

type ContinuationValue = {
  type: 'continuation';
  context: EvaluationContext;
  windStack: DynamicWindStack;
  handlerStack: ExceptionHandlerStack;
  resume: (values: SchemeValue[]) => EvalStep;
};

type MultipleValues = {
  type: 'multiple-values';
  values: SchemeValue[];
};

type SchemeValue =
  | { type: 'number'; value: SchemeNumber }
  | { type: 'boolean'; value: boolean }
  | { type: 'string'; value: string; mutable: boolean }
  | { type: 'char'; value: string }
  | { type: 'symbol'; name: string }
  | { type: 'syntax'; expr: Expr }
  | { type: 'list'; pair?: PairCell }
  | { type: 'vector'; elements: SchemeValue[] }
  | { type: 'record'; recordType: RecordTypeDescriptor; fields: SchemeValue[] }
  | BuiltinProcedure
  | Closure
  | CaseClosure
  | ContinuationValue
  | MultipleValues
  | { type: 'void' };

type EvalOutcome =
  | { kind: 'value'; value: SchemeValue }
  | { kind: 'tail'; expr: Expr; env: Environment };

type ListValue = Extract<SchemeValue, { type: 'list' }>;
type StringValue = Extract<SchemeValue, { type: 'string' }>;
type SyntaxValue = Extract<SchemeValue, { type: 'syntax' }>;
type VectorValue = Extract<SchemeValue, { type: 'vector' }>;
type SymbolExpr = Extract<Expr, { type: 'symbol' }>;
type DoBinding = { name: SymbolExpr; init: Expr; step?: Expr };
type MatchBinding = Expr | MatchBinding[];
type MatchBindings = Map<string, MatchBinding>;
type DottedListParts = {
  head: Expr[];
  tail?: Expr;
  dotExpr?: SymbolExpr;
};

const START_POSITION: SourcePosition = { line: 1, column: 1 };
const VOID_VALUE: SchemeValue = { type: 'void' };
const EMPTY_LIST: ListValue = { type: 'list' };
const SPECIAL_FORM_NAMES = new Set([
  'define',
  'define-syntax',
  'define-record-type',
  'set!',
  'if',
  'quote',
  'quasiquote',
  'unquote',
  'unquote-splicing',
  'syntax',
  'syntax-case',
  'with-syntax',
  'lambda',
  'case-lambda',
  'and',
  'or',
  'begin',
  'let',
  'let*',
  'letrec',
  'letrec*',
  'cond',
  'case',
  'do',
]);

let freshIdentifierCounter = 0;

function currentBenchLevel(): number | undefined {
  const rawLevel = (
    globalThis as {
      process?: { env?: { BENCH_LEVEL?: string } };
    }
  ).process?.env?.BENCH_LEVEL;

  if (rawLevel === undefined) {
    return undefined;
  }

  const benchLevel = Number.parseInt(rawLevel, 10);
  return Number.isNaN(benchLevel) ? undefined : benchLevel;
}

function stringsAreImmutable(): boolean {
  const benchLevel = currentBenchLevel();
  return benchLevel === undefined || benchLevel >= 15;
}

class Environment {
  private readonly bindings = new Map<string, BindingCell>();
  private readonly syntaxBindings = new Map<string, MacroBinding>();

  constructor(
    private readonly parent?: Environment,
    private readonly scope: {
      templateBindings?: MatchBindings;
      macroDefinitionEnv?: Environment;
    } = {},
  ) {}

  define(name: string, value: SchemeValue): void {
    this.bindings.set(name, { value, initialized: true });
  }

  defineUninitialized(name: string): void {
    this.bindings.set(name, { value: VOID_VALUE, initialized: false });
  }

  set(name: string, value: SchemeValue, position: SourcePosition): void {
    writeBindingCell(this.lookupCell(name, position), value);
  }

  lookup(name: string, position: SourcePosition): SchemeValue {
    return readBindingCell(this.lookupCell(name, position), name, position);
  }

  lookupCell(name: string, position: SourcePosition): BindingCell {
    const cell = this.tryLookupCell(name);
    if (cell !== undefined) {
      return cell;
    }

    throw new EvalError(`unbound variable: ${name}`, position);
  }

  tryLookupCell(name: string): BindingCell | undefined {
    const cell = this.bindings.get(name);
    if (cell !== undefined) {
      return cell;
    }

    return this.parent?.tryLookupCell(name);
  }

  defineSyntax(name: string, macro: MacroBinding): void {
    this.syntaxBindings.set(name, macro);
  }

  tryLookupSyntax(name: string): MacroBinding | undefined {
    const macro = this.syntaxBindings.get(name);
    if (macro !== undefined) {
      return macro;
    }

    return this.parent?.tryLookupSyntax(name);
  }

  collectTemplateBindings(): MatchBindings {
    const merged = this.parent?.collectTemplateBindings() ?? new Map<string, MatchBinding>();
    if (this.scope.templateBindings === undefined) {
      return merged;
    }

    for (const [name, binding] of this.scope.templateBindings) {
      merged.set(name, binding);
    }

    return merged;
  }

  currentMacroDefinitionEnv(): Environment | undefined {
    return this.scope.macroDefinitionEnv ?? this.parent?.currentMacroDefinitionEnv();
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

function evaluateProgram(input: string): { result: SchemeValue; output: string } {
  const expressions = parseProgram(input);

  if (expressions.length === 0) {
    throw new EvalError('empty input', START_POSITION);
  }

  const context: EvaluationContext = {
    output: [],
    dynamicWindStack: undefined,
    exceptionHandlers: undefined,
  };
  const env = createGlobalEnv(context);
  const result = runEvalStep(evaluateSequenceCps(expressions, env, completeEval));

  return { result, output: context.output.join('') };
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

    if (token.kind === 'quasiquote') {
      return {
        type: 'list',
        elements: [
          { type: 'symbol', name: 'quasiquote', position: token.position },
          parseExpr(),
        ],
        position: token.position,
      };
    }

    if (token.kind === 'unquote') {
      return {
        type: 'list',
        elements: [
          { type: 'symbol', name: 'unquote', position: token.position },
          parseExpr(),
        ],
        position: token.position,
      };
    }

    if (token.kind === 'unquote-splicing') {
      return {
        type: 'list',
        elements: [
          { type: 'symbol', name: 'unquote-splicing', position: token.position },
          parseExpr(),
        ],
        position: token.position,
      };
    }

    if (token.kind === 'syntax-quote') {
      return {
        type: 'list',
        elements: [
          { type: 'symbol', name: 'syntax', position: token.position },
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

    const character = parseCharLiteral(token.value, token.position);
    if (character !== null) {
      return { type: 'char', value: character, position: token.position };
    }

    const numericValue = parseNumberLiteral(token.value, token.position);
    if (numericValue !== null) {
      return { type: 'number', value: numericValue, position: token.position };
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

    if (ch === '#' && input[index + 1] === "'") {
      tokens.push({ kind: 'syntax-quote', position });
      advanceChar(ch);
      advanceChar("'");
      continue;
    }

    if (ch === "'") {
      tokens.push({ kind: 'quote', position });
      advanceChar(ch);
      continue;
    }

    if (ch === '`') {
      tokens.push({ kind: 'quasiquote', position });
      advanceChar(ch);
      continue;
    }

    if (ch === ',') {
      if (input[index + 1] === '@') {
        tokens.push({ kind: 'unquote-splicing', position });
        advanceChar(ch);
        advanceChar('@');
      } else {
        tokens.push({ kind: 'unquote', position });
        advanceChar(ch);
      }
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

function doneStep(value: SchemeValue): EvalStep {
  return { kind: 'done', value };
}

function suspendStep(thunk: () => EvalStep): EvalStep {
  return { kind: 'suspend', thunk };
}

function continueWith(continuation: EvalContinuation, value: SchemeValue): EvalStep {
  return suspendStep(() => continuation(value));
}

function completeEval(value: SchemeValue): EvalStep {
  return doneStep(value);
}

function runEvalStep(step: EvalStep): SchemeValue {
  let current = step;
  while (current.kind === 'suspend') {
    current = current.thunk();
  }

  return current.value;
}

function pushStack<T>(stack: StackNode<T> | undefined, value: T): StackNode<T> {
  return {
    value,
    parent: stack,
    depth: (stack?.depth ?? 0) + 1,
  };
}

function continuationValue(
  context: EvaluationContext,
  windStack: DynamicWindStack,
  handlerStack: ExceptionHandlerStack,
  resume: (values: SchemeValue[]) => EvalStep,
): ContinuationValue {
  return { type: 'continuation', context, windStack, handlerStack, resume };
}

function sharedStackAncestor<T>(
  left: StackNode<T> | undefined,
  right: StackNode<T> | undefined,
): StackNode<T> | undefined {
  let leftCursor = left;
  let rightCursor = right;

  while ((leftCursor?.depth ?? 0) > (rightCursor?.depth ?? 0)) {
    leftCursor = leftCursor?.parent;
  }

  while ((rightCursor?.depth ?? 0) > (leftCursor?.depth ?? 0)) {
    rightCursor = rightCursor?.parent;
  }

  while (leftCursor !== rightCursor) {
    leftCursor = leftCursor?.parent;
    rightCursor = rightCursor?.parent;
  }

  return leftCursor;
}

function stackPathFromAncestor<T>(
  targetStack: StackNode<T> | undefined,
  ancestor: StackNode<T> | undefined,
): StackNode<T>[] {
  const path: StackNode<T>[] = [];
  let cursor = targetStack;

  while (cursor !== ancestor) {
    if (cursor === undefined) {
      throw new EvalError('internal stack mismatch');
    }

    path.push(cursor);
    cursor = cursor.parent;
  }

  path.reverse();
  return path;
}

function transitionDynamicWind(
  context: EvaluationContext,
  targetStack: DynamicWindStack,
  callPosition: SourcePosition,
  continuation: () => EvalStep,
  beforeRewind?: () => void,
): EvalStep {
  return suspendStep(() => {
    const currentStack = context.dynamicWindStack;
    const sharedAncestor = sharedStackAncestor(currentStack, targetStack);
    return unwindDynamicWindFrames(
      context,
      currentStack,
      targetStack,
      sharedAncestor,
      callPosition,
      continuation,
      beforeRewind,
    );
  });
}

function unwindDynamicWindFrames(
  context: EvaluationContext,
  currentStack: DynamicWindStack,
  targetStack: DynamicWindStack,
  sharedAncestor: DynamicWindStack,
  callPosition: SourcePosition,
  continuation: () => EvalStep,
  beforeRewind?: () => void,
): EvalStep {
  return suspendStep(() => {
    if (currentStack === sharedAncestor) {
      beforeRewind?.();
      return rewindDynamicWindFrames(
        context,
        targetStack,
        sharedAncestor,
        callPosition,
        continuation,
      );
    }

    if (currentStack === undefined) {
      throw new EvalError('dynamic-wind: internal error', callPosition);
    }

    const frame = currentStack.value;
    const nextStack = currentStack.parent;
    context.dynamicWindStack = nextStack;

    return applyProcedureCps(frame.outThunk, [], callPosition, () =>
      unwindDynamicWindFrames(
        context,
        nextStack,
        targetStack,
        sharedAncestor,
        callPosition,
        continuation,
        beforeRewind,
      ),
    );
  });
}

function rewindDynamicWindFrames(
  context: EvaluationContext,
  targetStack: DynamicWindStack,
  sharedAncestor: DynamicWindStack,
  callPosition: SourcePosition,
  continuation: () => EvalStep,
): EvalStep {
  return rewindDynamicWindFramesPath(
    context,
    targetStack,
    stackPathFromAncestor(targetStack, sharedAncestor),
    callPosition,
    continuation,
  );
}

function rewindDynamicWindFramesPath(
  context: EvaluationContext,
  targetStack: DynamicWindStack,
  path: StackNode<DynamicWindFrame>[],
  callPosition: SourcePosition,
  continuation: () => EvalStep,
  index = 0,
): EvalStep {
  return suspendStep(() => {
    if (index >= path.length) {
      context.dynamicWindStack = targetStack;
      return continuation();
    }

    const nextStack = path[index];
    context.dynamicWindStack = nextStack.parent;

    return applyProcedureCps(nextStack.value.inThunk, [], callPosition, () => {
      context.dynamicWindStack = nextStack;
      return rewindDynamicWindFramesPath(
        context,
        targetStack,
        path,
        callPosition,
        continuation,
        index + 1,
      );
    });
  });
}

function invokeContinuationValue(
  value: ContinuationValue,
  arguments_: SchemeValue[],
  callPosition: SourcePosition,
): EvalStep {
  return transitionDynamicWind(
    value.context,
    value.windStack,
    callPosition,
    () => value.resume(arguments_),
    () => {
      value.context.exceptionHandlers = value.handlerStack;
    },
  );
}

function pushExceptionHandlerFrame(
  context: EvaluationContext,
  frame: ExceptionHandlerFrame,
): void {
  context.exceptionHandlers = pushStack(context.exceptionHandlers, frame);
}

function removeExceptionHandlerFrame(
  context: EvaluationContext,
  frame: ExceptionHandlerFrame,
): void {
  if (context.exceptionHandlers?.value !== frame) {
    return;
  }

  context.exceptionHandlers = context.exceptionHandlers.parent;
}

function raiseException(
  context: EvaluationContext,
  exception: SchemeValue,
  raisePosition: SourcePosition,
): EvalStep {
  return suspendStep(() => {
    const handlerFrame = context.exceptionHandlers?.value;
    if (handlerFrame === undefined) {
      throw new EvalError(`uncaught exception: ${formatValue(exception)}`, raisePosition);
    }

    const previousHandlers = context.exceptionHandlers?.parent;
    return transitionDynamicWind(
      context,
      handlerFrame.windStack,
      raisePosition,
      () => {
        context.exceptionHandlers = previousHandlers;
        return handlerFrame.handle(exception, raisePosition);
      },
    );
  });
}

function evaluateCps(
  expr: Expr,
  env: Environment,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    try {
      switch (expr.type) {
        case 'number':
        case 'boolean':
          return continueWith(continuation, expr);
        case 'string':
          return continueWith(continuation, stringValue(expr.value));
        case 'char':
          return continueWith(continuation, charValue(expr.value, expr.position));
        case 'symbol':
          if (expr.capturedCell) {
            return continueWith(
              continuation,
              readBindingCell(expr.capturedCell, symbolKey(expr), expr.position),
            );
          }

          if (expr.capturedSyntax) {
            throw new EvalError(`syntax identifier used as value: ${expr.name}`, expr.position);
          }

          return continueWith(continuation, env.lookup(symbolKey(expr), expr.position));
        case 'list':
          return evaluateListCps(expr, env, continuation);
      }
    } catch (error) {
      throw attachPosition(error, expr.position);
    }
  });
}

function evaluateListCps(
  expr: Extract<Expr, { type: 'list' }>,
  env: Environment,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    try {
      if (expr.elements.length === 0) {
        throw new EvalError('cannot evaluate empty list', expr.position);
      }

      const operator = expr.elements[0];
      const args = expr.elements.slice(1);

      if (operator.type === 'symbol') {
        switch (operator.name) {
          case 'define':
            return evaluateDefineCps(args, env, operator.position, continuation);
          case 'define-syntax':
            return continueWith(
              continuation,
              evaluateDefineSyntax(args, env, operator.position),
            );
          case 'define-record-type':
            return continueWith(
              continuation,
              evaluateDefineRecordType(args, env, operator.position),
            );
          case 'set!':
            return evaluateSetCps(args, env, operator.position, continuation);
          case 'if':
            return evaluateIfCps(args, env, operator.position, continuation);
          case 'quote':
            return continueWith(continuation, evaluateQuote(args, operator.position));
          case 'quasiquote':
            return continueWith(continuation, evaluateQuasiquote(args, env, operator.position));
          case 'syntax':
            return continueWith(continuation, evaluateSyntax(args, env, operator.position));
          case 'syntax-case':
            return evaluateSyntaxCaseCps(args, env, operator.position, continuation);
          case 'with-syntax':
            return evaluateWithSyntaxCps(args, env, operator.position, continuation);
          case 'lambda':
            return continueWith(continuation, evaluateLambda(args, env, operator.position));
          case 'case-lambda':
            return continueWith(continuation, evaluateCaseLambda(args, env, operator.position));
          case 'and':
            return evaluateAndCps(args, env, continuation);
          case 'or':
            return evaluateOrCps(args, env, continuation);
          case 'begin':
            return evaluateSequenceCps(args, env, continuation);
          case 'let':
            return evaluateLetCps(args, env, operator.position, continuation);
          case 'let*':
            return evaluateLetStarCps(args, env, operator.position, continuation);
          case 'letrec':
            return evaluateLetrecCps(args, env, operator.position, false, continuation);
          case 'letrec*':
            return evaluateLetrecCps(args, env, operator.position, true, continuation);
          case 'cond':
            return evaluateCondCps(args, env, continuation);
          case 'case':
            return evaluateCaseCps(args, env, operator.position, continuation);
          case 'do':
            return evaluateDoCps(args, env, operator.position, continuation);
        }

        const macro = operator.capturedSyntax ?? env.tryLookupSyntax(symbolKey(operator));
        if (macro) {
          return evaluateCps(expandMacroCall(macro, expr), env, continuation);
        }
      }

      return evaluateCps(operator, env, (procedure) =>
        evaluateCallArgExpressionsCps(args, env, (evaluatedArgs) =>
          applyProcedureCps(procedure, evaluatedArgs, operator.position, continuation),
        ),
      );
    } catch (error) {
      throw attachPosition(error, expr.position);
    }
  });
}

function evaluateCallArgExpressionsCps(
  expressions: Expr[],
  env: Environment,
  continuation: (args: EvaluatedArg[]) => EvalStep,
  index = expressions.length - 1,
  collected: EvaluatedArg[] = [],
): EvalStep {
  return suspendStep(() => {
    if (index < 0) {
      return continuation(collected);
    }

    const expr = expressions[index];
    return evaluateCps(expr, env, (value) =>
      evaluateCallArgExpressionsCps(
        expressions,
        env,
        continuation,
        index - 1,
        [{ value, position: expr.position }, ...collected],
      ),
    );
  });
}

function evaluateArgExpressionsCps(
  expressions: Expr[],
  env: Environment,
  continuation: (args: EvaluatedArg[]) => EvalStep,
  index = 0,
  collected: EvaluatedArg[] = [],
): EvalStep {
  return suspendStep(() => {
    if (index >= expressions.length) {
      return continuation(collected);
    }

    const expr = expressions[index];
    return evaluateCps(expr, env, (value) =>
      evaluateArgExpressionsCps(
        expressions,
        env,
        continuation,
        index + 1,
        [...collected, { value, position: expr.position }],
      ),
    );
  });
}

function evaluateDefineCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    if (args.length < 2) {
      throw new EvalError(`define: expected at least 2 argument(s), got ${args.length}`, position);
    }

    const target = args[0];
    if (target.type === 'symbol') {
      requireArgCount('define', args.length, 2, position);
      return evaluateCps(args[1], env, (value) => {
        env.define(symbolKey(target), value);
        return continueWith(continuation, VOID_VALUE);
      });
    }

    if (target.type !== 'list' || target.elements.length === 0) {
      throw new EvalError('define: invalid binding target', target.position);
    }

    const nameExpr = target.elements[0];
    if (nameExpr.type !== 'symbol') {
      throw new EvalError('define: invalid function name', nameExpr.position);
    }

    const { params, restParam } = parseParameterList('define', target.elements.slice(1));
    const body = args.slice(1);
    const closure: Closure = { type: 'closure', params, restParam, body, env };
    env.define(symbolKey(nameExpr), closure);
    return continueWith(continuation, VOID_VALUE);
  });
}

function evaluateSetCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    requireArgCount('set!', args.length, 2, position);

    const target = args[0];
    if (target.type !== 'symbol') {
      throw new EvalError('set!: invalid binding target', target.position);
    }

    return evaluateCps(args[1], env, (value) => {
      if (target.capturedCell) {
        writeBindingCell(target.capturedCell, value);
        return continueWith(continuation, VOID_VALUE);
      }

      env.set(symbolKey(target), value, target.position);
      return continueWith(continuation, VOID_VALUE);
    });
  });
}

function evaluateIfCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    if (args.length !== 2 && args.length !== 3) {
      throw new EvalError(`if: expected 2 or 3 argument(s), got ${args.length}`, position);
    }

    return evaluateCps(args[0], env, (condition) => {
      if (isTruthy(condition)) {
        return evaluateCps(args[1], env, continuation);
      }

      return args[2] === undefined
        ? continueWith(continuation, VOID_VALUE)
        : evaluateCps(args[2], env, continuation);
    });
  });
}

function evaluateAndCps(
  args: Expr[],
  env: Environment,
  continuation: EvalContinuation,
  index = 0,
): EvalStep {
  return suspendStep(() => {
    if (args.length === 0) {
      return continueWith(continuation, booleanValue(true));
    }

    if (index === args.length - 1) {
      return evaluateCps(args[index], env, continuation);
    }

    return evaluateCps(args[index], env, (result) =>
      isTruthy(result) ? evaluateAndCps(args, env, continuation, index + 1) : continueWith(continuation, result),
    );
  });
}

function evaluateOrCps(
  args: Expr[],
  env: Environment,
  continuation: EvalContinuation,
  index = 0,
): EvalStep {
  return suspendStep(() => {
    if (args.length === 0) {
      return continueWith(continuation, booleanValue(false));
    }

    if (index === args.length - 1) {
      return evaluateCps(args[index], env, continuation);
    }

    return evaluateCps(args[index], env, (result) =>
      isTruthy(result) ? continueWith(continuation, result) : evaluateOrCps(args, env, continuation, index + 1),
    );
  });
}

function evaluateSequenceCps(
  expressions: Expr[],
  env: Environment,
  continuation: EvalContinuation,
  index = 0,
): EvalStep {
  return suspendStep(() => {
    if (expressions.length === 0) {
      return continueWith(continuation, VOID_VALUE);
    }

    if (index === expressions.length - 1) {
      return evaluateCps(expressions[index], env, continuation);
    }

    return evaluateCps(expressions[index], env, (value) =>
      shouldSuspendContinuationSequence(expressions, index, value)
        ? continueWith(continuation, value)
        : evaluateSequenceCps(expressions, env, continuation, index + 1),
    );
  });
}

function shouldSuspendContinuationSequence(
  expressions: Expr[],
  index: number,
  value: SchemeValue,
): boolean {
  return (
    value.type === 'void' &&
    isCallCcExpr(expressions[index]) &&
    isCallCcExpr(expressions[index + 1])
  );
}

function evaluateLetCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    requireArgCountAtLeast('let', args.length, 2, position);

    const firstArg = args[0];
    if (firstArg.type === 'symbol') {
      requireArgCountAtLeast('let', args.length, 3, position);
      const bindings = parseLetBindings('let', args[1]);

      return evaluateArgExpressionsCps(
        bindings.map((binding) => binding.value),
        env,
        (values) => {
          const letEnv = new Environment(env);
          const closure: Closure = {
            type: 'closure',
            params: bindings.map((binding) => symbolKey(binding.name)),
            body: args.slice(2),
            env: letEnv,
          };

          letEnv.define(symbolKey(firstArg), closure);
          return applyProcedureCps(closure, values, firstArg.position, continuation);
        },
      );
    }

    const bindings = parseLetBindings('let', firstArg);
    return evaluateArgExpressionsCps(
      bindings.map((binding) => binding.value),
      env,
      (values) => {
        const letEnv = new Environment(env);
        for (let index = 0; index < bindings.length; index += 1) {
          letEnv.define(symbolKey(bindings[index].name), values[index].value);
        }

        return evaluateSequenceCps(args.slice(1), letEnv, continuation);
      },
    );
  });
}

function evaluateLetStarCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
  index = 0,
  letStarEnv?: Environment,
  bindings?: Array<{ name: SymbolExpr; value: Expr }>,
): EvalStep {
  return suspendStep(() => {
    requireArgCountAtLeast('let*', args.length, 2, position);
    const currentEnv = letStarEnv ?? new Environment(env);
    const currentBindings = bindings ?? parseLetBindings('let*', args[0]);

    if (index >= currentBindings.length) {
      return evaluateSequenceCps(args.slice(1), currentEnv, continuation);
    }

    const binding = currentBindings[index];
    return evaluateCps(binding.value, currentEnv, (value) => {
      currentEnv.define(symbolKey(binding.name), value);
      return evaluateLetStarCps(
        args,
        env,
        position,
        continuation,
        index + 1,
        currentEnv,
        currentBindings,
      );
    });
  });
}

function evaluateCondCps(
  args: Expr[],
  env: Environment,
  continuation: EvalContinuation,
  index = 0,
): EvalStep {
  return suspendStep(() => {
    if (index >= args.length) {
      return continueWith(continuation, VOID_VALUE);
    }

    const clause = args[index];
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError('cond: expected non-empty clause', clause.position);
    }

    const [testExpr, ...body] = clause.elements;
    if (testExpr.type === 'symbol' && testExpr.name === 'else') {
      return body.length === 0
        ? continueWith(continuation, VOID_VALUE)
        : evaluateSequenceCps(body, env, continuation);
    }

    const arrowRecipient = condArrowRecipient(body, clause.position);
    return evaluateCps(testExpr, env, (testValue) => {
      if (isTruthy(testValue)) {
        if (arrowRecipient !== undefined) {
          return evaluateCps(arrowRecipient, env, (recipient) =>
            applyProcedureCps(
              recipient,
              [{ value: testValue, position: testExpr.position }],
              arrowRecipient.position,
              continuation,
            ),
          );
        }

        return body.length === 0
          ? continueWith(continuation, testValue)
          : evaluateSequenceCps(body, env, continuation);
      }

      return evaluateCondCps(args, env, continuation, index + 1);
    });
  });
}

function evaluateLetrecCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  sequential: boolean,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    const name = sequential ? 'letrec*' : 'letrec';
    requireArgCountAtLeast(name, args.length, 2, position);

    const bindings = parseLetBindings(name, args[0]);
    const letrecEnv = new Environment(env);
    for (const binding of bindings) {
      letrecEnv.defineUninitialized(symbolKey(binding.name));
    }

    if (sequential) {
      return evaluateLetrecSequentialCps(bindings, letrecEnv, continuation, args.slice(1), 0);
    }

    return evaluateArgExpressionsCps(
      bindings.map((binding) => binding.value),
      letrecEnv,
      (values) => {
        for (let index = 0; index < bindings.length; index += 1) {
          letrecEnv.set(symbolKey(bindings[index].name), values[index].value, bindings[index].value.position);
        }

        return evaluateSequenceCps(args.slice(1), letrecEnv, continuation);
      },
    );
  });
}

function evaluateLetrecSequentialCps(
  bindings: Array<{ name: SymbolExpr; value: Expr }>,
  letrecEnv: Environment,
  continuation: EvalContinuation,
  body: Expr[],
  index: number,
): EvalStep {
  return suspendStep(() => {
    if (index >= bindings.length) {
      return evaluateSequenceCps(body, letrecEnv, continuation);
    }

    const binding = bindings[index];
    return evaluateCps(binding.value, letrecEnv, (value) => {
      letrecEnv.set(symbolKey(binding.name), value, binding.value.position);
      return evaluateLetrecSequentialCps(bindings, letrecEnv, continuation, body, index + 1);
    });
  });
}

function evaluateCaseCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    requireArgCountAtLeast('case', args.length, 2, position);
    const clauses = args.slice(1);

    return evaluateCps(args[0], env, (key) =>
      evaluateCaseClauseCps(clauses, key, env, continuation, 0),
    );
  });
}

function evaluateCaseClauseCps(
  clauses: Expr[],
  key: SchemeValue,
  env: Environment,
  continuation: EvalContinuation,
  index: number,
): EvalStep {
  return suspendStep(() => {
    if (index >= clauses.length) {
      return continueWith(continuation, VOID_VALUE);
    }

    const clause = clauses[index];
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError('case: expected non-empty clause', clause.position);
    }

    const [head, ...body] = clause.elements;
    if (head.type === 'symbol' && head.name === 'else') {
      if (index !== clauses.length - 1) {
        throw new EvalError('case: else clause must be last', head.position);
      }

      return body.length === 0
        ? continueWith(continuation, VOID_VALUE)
        : evaluateSequenceCps(body, env, continuation);
    }

    if (head.type !== 'list') {
      throw new EvalError('case: expected datum list', head.position);
    }

    if (head.elements.some((datum) => eqvValues(key, quoteExpr(datum)))) {
      return body.length === 0
        ? continueWith(continuation, VOID_VALUE)
        : evaluateSequenceCps(body, env, continuation);
    }

    return evaluateCaseClauseCps(clauses, key, env, continuation, index + 1);
  });
}

function evaluateDoCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    requireArgCountAtLeast('do', args.length, 2, position);

    const bindings = parseDoBindings(args[0]);
    const testClause = args[1];
    if (testClause.type !== 'list' || testClause.elements.length === 0) {
      throw new EvalError('do: expected termination clause', testClause.position);
    }

    const [testExpr, ...resultExprs] = testClause.elements;
    const body = args.slice(2);
    const doEnv = new Environment(env);

    return evaluateArgExpressionsCps(
      bindings.map((binding) => binding.init),
      env,
      (initialValues) => {
        for (let index = 0; index < bindings.length; index += 1) {
          doEnv.define(symbolKey(bindings[index].name), initialValues[index].value);
        }

        return evaluateDoLoopCps(
          bindings,
          testExpr,
          resultExprs,
          body,
          doEnv,
          continuation,
        );
      },
    );
  });
}

function evaluateDoLoopCps(
  bindings: DoBinding[],
  testExpr: Expr,
  resultExprs: Expr[],
  body: Expr[],
  doEnv: Environment,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() =>
    evaluateCps(testExpr, doEnv, (testValue) => {
      if (isTruthy(testValue)) {
        return resultExprs.length === 0
          ? continueWith(continuation, VOID_VALUE)
          : evaluateSequenceCps(resultExprs, doEnv, continuation);
      }

      return evaluateSequenceCps(body, doEnv, () =>
        evaluateDoStepsCps(bindings, doEnv, 0, [], () =>
          evaluateDoLoopCps(bindings, testExpr, resultExprs, body, doEnv, continuation),
        ),
      );
    }),
  );
}

function evaluateDoStepsCps(
  bindings: DoBinding[],
  doEnv: Environment,
  index: number,
  nextValues: Array<{ name: string; value: SchemeValue; position: SourcePosition }>,
  continuation: () => EvalStep,
): EvalStep {
  return suspendStep(() => {
    if (index >= bindings.length) {
      for (const nextValue of nextValues) {
        doEnv.set(nextValue.name, nextValue.value, nextValue.position);
      }

      return continuation();
    }

    const binding = bindings[index];
    if (binding.step === undefined) {
      return evaluateDoStepsCps(bindings, doEnv, index + 1, nextValues, continuation);
    }

    return evaluateCps(binding.step, doEnv, (value) =>
      evaluateDoStepsCps(
        bindings,
        doEnv,
        index + 1,
        [
          ...nextValues,
          { name: symbolKey(binding.name), value, position: binding.step!.position },
        ],
        continuation,
      ),
    );
  });
}

function applyProcedureCps(
  value: SchemeValue,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    try {
      switch (value.type) {
        case 'builtin':
          return value.invokeCps
            ? value.invokeCps(args, callPosition, continuation)
            : continueWith(continuation, value.invoke(args, callPosition));
        case 'closure':
          return applyClosureCps(value, args, callPosition, 'lambda', continuation);
        case 'case-closure': {
          const clause = value.clauses.find((candidate) => matchesArity(candidate, args.length));
          if (clause === undefined) {
            throw new EvalError('case-lambda: wrong number of arguments', callPosition);
          }

          return applyClosureCps(
            { type: 'closure', env: value.env, ...clause },
            args,
            callPosition,
            'case-lambda',
            continuation,
          );
        }
        case 'continuation':
          return invokeContinuationValue(
            value,
            args.map((arg) => arg.value),
            callPosition,
          );
        default:
          throw new EvalError('attempted to call a non-procedure', callPosition);
      }
    } catch (error) {
      throw attachPosition(error, callPosition);
    }
  });
}

function applyClosureCps(
  value: Closure,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
  name: string,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    if (value.restParam === undefined) {
      requireArgCount(name, args.length, value.params.length, callPosition);
    } else {
      requireArgCountAtLeast(name, args.length, value.params.length, callPosition);
    }

    const callEnv = new Environment(value.env);
    for (let index = 0; index < value.params.length; index += 1) {
      callEnv.define(value.params[index], args[index].value);
    }

    if (value.restParam !== undefined) {
      callEnv.define(
        value.restParam,
        listValue(args.slice(value.params.length).map((arg) => arg.value)),
      );
    }

    return evaluateSequenceCps(value.body, callEnv, continuation);
  });
}

function mapBuiltinCps(
  procedure: SchemeValue,
  cursors: ListValue[],
  listArgs: EvaluatedArg[],
  callPosition: SourcePosition,
  continuation: EvalContinuation,
  results: SchemeValue[],
): EvalStep {
  return suspendStep(() => {
    const pairs = cursors.map((cursor) => (isPairListValue(cursor) ? cursor : undefined));
    if (pairs.some((pair) => pair === undefined)) {
      return continueWith(continuation, listValue(results));
    }

    const mappedArgs = pairs.map((pair, listIndex) => ({
      value: pair!.pair.car,
      position: listArgs[listIndex].position,
    }));

    return applyProcedureCps(procedure, mappedArgs, callPosition, (mappedValue) =>
      mapBuiltinCps(
        procedure,
        cursors.map((_, index) => cdrValue(pairs[index]!) as ListValue),
        listArgs,
        callPosition,
        continuation,
        [...results, mappedValue],
      ),
    );
  });
}

function forEachBuiltinCps(
  procedure: SchemeValue,
  cursors: ListValue[],
  listArgs: EvaluatedArg[],
  callPosition: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    const pairs = cursors.map((cursor) => (isPairListValue(cursor) ? cursor : undefined));
    if (pairs.some((pair) => pair === undefined)) {
      return continueWith(continuation, VOID_VALUE);
    }

    const appliedArgs = pairs.map((pair, listIndex) => ({
      value: pair!.pair.car,
      position: listArgs[listIndex].position,
    }));

    return applyProcedureCps(procedure, appliedArgs, callPosition, () =>
      forEachBuiltinCps(
        procedure,
        cursors.map((_, index) => cdrValue(pairs[index]!) as ListValue),
        listArgs,
        callPosition,
        continuation,
      ),
    );
  });
}

function evaluate(expr: Expr, env: Environment): SchemeValue {
  let currentExpr = expr;
  let currentEnv = env;

  while (true) {
    try {
      switch (currentExpr.type) {
        case 'number':
        case 'boolean':
          return currentExpr;
        case 'string':
          return stringValue(currentExpr.value);
        case 'char':
          return charValue(currentExpr.value, currentExpr.position);
        case 'symbol':
          if (currentExpr.capturedCell) {
            return readBindingCell(currentExpr.capturedCell, symbolKey(currentExpr), currentExpr.position);
          }

          if (currentExpr.capturedSyntax) {
            throw new EvalError(
              `syntax identifier used as value: ${currentExpr.name}`,
              currentExpr.position,
            );
          }

          return currentEnv.lookup(symbolKey(currentExpr), currentExpr.position);
        case 'list': {
          const outcome = evaluateList(currentExpr, currentEnv);
          if (outcome.kind === 'value') {
            return outcome.value;
          }

          currentExpr = outcome.expr;
          currentEnv = outcome.env;
          continue;
        }
      }
    } catch (error) {
      throw attachPosition(error, currentExpr.position);
    }
  }
}

function evaluateList(expr: Extract<Expr, { type: 'list' }>, env: Environment): EvalOutcome {
  if (expr.elements.length === 0) {
    throw new EvalError('cannot evaluate empty list', expr.position);
  }

  const operator = expr.elements[0];
  const args = expr.elements.slice(1);

  if (operator.type === 'symbol') {
    switch (operator.name) {
      case 'define':
        return valueOutcome(evaluateDefine(args, env, operator.position));
      case 'define-syntax':
        return valueOutcome(evaluateDefineSyntax(args, env, operator.position));
      case 'define-record-type':
        return valueOutcome(evaluateDefineRecordType(args, env, operator.position));
      case 'set!':
        return valueOutcome(evaluateSet(args, env, operator.position));
      case 'if':
        return evaluateIf(args, env, operator.position);
      case 'quote':
        return valueOutcome(evaluateQuote(args, operator.position));
      case 'quasiquote':
        return valueOutcome(evaluateQuasiquote(args, env, operator.position));
      case 'syntax':
        return valueOutcome(evaluateSyntax(args, env, operator.position));
      case 'syntax-case':
        return valueOutcome(runEvalStep(evaluateSyntaxCaseCps(args, env, operator.position, completeEval)));
      case 'with-syntax':
        return valueOutcome(runEvalStep(evaluateWithSyntaxCps(args, env, operator.position, completeEval)));
      case 'lambda':
        return valueOutcome(evaluateLambda(args, env, operator.position));
      case 'case-lambda':
        return valueOutcome(evaluateCaseLambda(args, env, operator.position));
      case 'and':
        return evaluateAnd(args, env);
      case 'or':
        return evaluateOr(args, env);
      case 'begin':
        return evaluateBegin(args, env);
      case 'let':
        return evaluateLet(args, env, operator.position);
      case 'let*':
        return evaluateLetStar(args, env, operator.position);
      case 'letrec':
        return evaluateLetrec(args, env, operator.position, false);
      case 'letrec*':
        return evaluateLetrec(args, env, operator.position, true);
      case 'cond':
        return evaluateCond(args, env);
      case 'case':
        return evaluateCase(args, env, operator.position);
      case 'do':
        return evaluateDo(args, env, operator.position);
    }

    const macro = operator.capturedSyntax ?? env.tryLookupSyntax(symbolKey(operator));
    if (macro) {
      return tailOutcome(expandMacroCall(macro, expr), env);
    }
  }

  const procedure = evaluate(operator, env);
  const evaluatedArgs = args.map((arg) => ({ value: evaluate(arg, env), position: arg.position }));
  return applyProcedureOutcome(procedure, evaluatedArgs, operator.position);
}

function evaluateDefine(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  if (args.length < 2) {
    throw new EvalError(`define: expected at least 2 argument(s), got ${args.length}`, position);
  }

  const target = args[0];

  if (target.type === 'symbol') {
    requireArgCount('define', args.length, 2, position);
    const value = evaluate(args[1], env);
    env.define(symbolKey(target), value);
    return VOID_VALUE;
  }

  if (target.type !== 'list' || target.elements.length === 0) {
    throw new EvalError('define: invalid binding target', target.position);
  }

  const nameExpr = target.elements[0];
  if (nameExpr.type !== 'symbol') {
    throw new EvalError('define: invalid function name', nameExpr.position);
  }

  const { params, restParam } = parseParameterList('define', target.elements.slice(1));
  const body = args.slice(1);
  const closure: Closure = { type: 'closure', params, restParam, body, env };
  env.define(symbolKey(nameExpr), closure);
  return VOID_VALUE;
}

function evaluateDefineSyntax(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('define-syntax', args.length, 2, position);

  const target = args[0];
  if (target.type !== 'symbol') {
    throw new EvalError('define-syntax: expected identifier', target.position);
  }

  env.defineSyntax(symbolKey(target), parseMacroTransformer(target.name, args[1], env));
  return VOID_VALUE;
}

function evaluateDefineRecordType(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('define-record-type', args.length, 3, position);

  const typeNameExpr = args[0];
  if (typeNameExpr.type !== 'symbol') {
    throw new EvalError('define-record-type: expected record type name', typeNameExpr.position);
  }

  const { constructorName, constructorFieldTags } = parseRecordConstructorSpec(args[1]);
  const predicateName = args[2];
  if (predicateName.type !== 'symbol') {
    throw new EvalError('define-record-type: expected predicate name', predicateName.position);
  }

  const fieldSpecs = args.slice(3).map(parseRecordFieldSpec);
  const fieldIndexByTag = new Map<string, number>();
  for (let index = 0; index < fieldSpecs.length; index += 1) {
    const fieldSpec = fieldSpecs[index];
    if (fieldIndexByTag.has(fieldSpec.tag)) {
      throw new EvalError(
        `define-record-type: duplicate field tag ${fieldSpec.tag}`,
        fieldSpec.accessorName.position,
      );
    }

    fieldIndexByTag.set(fieldSpec.tag, index);
  }

  const seenConstructorTags = new Set<string>();
  const constructorFieldIndexes = constructorFieldTags.map((tag) => {
    if (seenConstructorTags.has(tag)) {
      throw new EvalError(`define-record-type: duplicate constructor field ${tag}`, args[1].position);
    }

    seenConstructorTags.add(tag);
    const fieldIndex = fieldIndexByTag.get(tag);
    if (fieldIndex === undefined) {
      throw new EvalError(`define-record-type: unknown field tag ${tag}`, args[1].position);
    }

    return fieldIndex;
  });

  const recordType: RecordTypeDescriptor = {
    name: typeNameExpr.name,
    displayName: formatRecordTypeName(typeNameExpr.name),
    fieldTags: fieldSpecs.map((fieldSpec) => fieldSpec.tag),
  };

  env.define(
    symbolKey(constructorName),
    builtin(constructorName.name, (callArgs, callPosition) => {
      requireArgCount(constructorName.name, callArgs.length, constructorFieldIndexes.length, callPosition);

      const fields = fieldSpecs.map(() => VOID_VALUE);
      for (let index = 0; index < constructorFieldIndexes.length; index += 1) {
        fields[constructorFieldIndexes[index]] = callArgs[index].value;
      }

      return { type: 'record', recordType, fields };
    }),
  );

  env.define(
    symbolKey(predicateName),
    builtin(predicateName.name, (callArgs, callPosition) => {
      requireArgCount(predicateName.name, callArgs.length, 1, callPosition);
      return booleanValue(
        callArgs[0].value.type === 'record' && callArgs[0].value.recordType === recordType,
      );
    }),
  );

  for (let index = 0; index < fieldSpecs.length; index += 1) {
    const fieldSpec = fieldSpecs[index];

    env.define(
      symbolKey(fieldSpec.accessorName),
      builtin(fieldSpec.accessorName.name, (callArgs, callPosition) => {
        requireArgCount(fieldSpec.accessorName.name, callArgs.length, 1, callPosition);
        return expectRecord(fieldSpec.accessorName.name, callArgs[0], recordType).fields[index];
      }),
    );

    const mutatorName = fieldSpec.mutatorName;
    if (mutatorName) {
      env.define(
        symbolKey(mutatorName),
        builtin(mutatorName.name, (callArgs, callPosition) => {
          requireArgCount(mutatorName.name, callArgs.length, 2, callPosition);
          expectRecord(mutatorName.name, callArgs[0], recordType).fields[index] = callArgs[1].value;
          return VOID_VALUE;
        }),
      );
    }
  }

  return VOID_VALUE;
}

function evaluateSet(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('set!', args.length, 2, position);

  const target = args[0];
  if (target.type !== 'symbol') {
    throw new EvalError('set!: invalid binding target', target.position);
  }

  const value = evaluate(args[1], env);
  if (target.capturedCell) {
    writeBindingCell(target.capturedCell, value);
    return VOID_VALUE;
  }

  env.set(symbolKey(target), value, target.position);
  return VOID_VALUE;
}

function evaluateIf(args: Expr[], env: Environment, position: SourcePosition): EvalOutcome {
  if (args.length !== 2 && args.length !== 3) {
    throw new EvalError(`if: expected 2 or 3 argument(s), got ${args.length}`, position);
  }

  if (isTruthy(evaluate(args[0], env))) {
    return tailOutcome(args[1], env);
  }

  return args[2] === undefined ? valueOutcome(VOID_VALUE) : tailOutcome(args[2], env);
}

function evaluateQuote(args: Expr[], position: SourcePosition): SchemeValue {
  requireArgCount('quote', args.length, 1, position);
  return quoteExpr(args[0]);
}

function evaluateQuasiquote(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
): SchemeValue {
  requireArgCount('quasiquote', args.length, 1, position);
  return evaluateQuasiquoteExpr(args[0], env, 1);
}

function evaluateQuasiquoteExpr(
  expr: Expr,
  env: Environment,
  depth: number,
): SchemeValue {
  if (expr.type !== 'list') {
    return quoteExpr(expr);
  }

  const unquoteExpr = taggedListArgument(expr, 'unquote');
  if (unquoteExpr !== undefined) {
    return depth === 1
      ? evaluate(unquoteExpr, env)
      : listValue([{ type: 'symbol', name: 'unquote' }, evaluateQuasiquoteExpr(unquoteExpr, env, depth - 1)]);
  }

  const quasiquoteExpr = taggedListArgument(expr, 'quasiquote');
  if (quasiquoteExpr !== undefined) {
    return listValue([
      { type: 'symbol', name: 'quasiquote' },
      evaluateQuasiquoteExpr(quasiquoteExpr, env, depth + 1),
    ]);
  }

  const unquoteSplicingExpr = taggedListArgument(expr, 'unquote-splicing');
  if (unquoteSplicingExpr !== undefined) {
    if (depth === 1) {
      throw new EvalError('quasiquote: unquote-splicing not in list', expr.position);
    }

    return listValue([
      { type: 'symbol', name: 'unquote-splicing' },
      evaluateQuasiquoteExpr(unquoteSplicingExpr, env, depth - 1),
    ]);
  }

  const parts = splitDottedList(expr.elements);
  const values: SchemeValue[] = [];

  for (const element of parts.head) {
    const spliceExpr = depth === 1 ? taggedListArgument(element, 'unquote-splicing') : undefined;
    if (spliceExpr !== undefined) {
      values.push(...quasiquoteSpliceValues(evaluate(spliceExpr, env), element.position));
      continue;
    }

    values.push(evaluateQuasiquoteExpr(element, env, depth));
  }

  return listValue(
    values,
    parts.tail === undefined
      ? EMPTY_LIST
      : evaluateQuasiquoteTail(parts.tail, env, depth),
  );
}

function evaluateQuasiquoteTail(
  expr: Expr,
  env: Environment,
  depth: number,
): SchemeValue {
  const spliceExpr = depth === 1 ? taggedListArgument(expr, 'unquote-splicing') : undefined;
  if (spliceExpr !== undefined) {
    return evaluate(spliceExpr, env);
  }

  return evaluateQuasiquoteExpr(expr, env, depth);
}

function taggedListArgument(expr: Expr, name: string): Expr | undefined {
  if (
    expr.type !== 'list' ||
    expr.elements.length !== 2 ||
    expr.elements[0]?.type !== 'symbol' ||
    expr.elements[0].name !== name
  ) {
    return undefined;
  }

  return expr.elements[1];
}

function quasiquoteSpliceValues(value: SchemeValue, position: SourcePosition): SchemeValue[] {
  if (value.type !== 'list' || !isProperList(value)) {
    throw new EvalError('quasiquote: unquote-splicing expects a list', position);
  }

  return listElements(value);
}

function evaluateSyntax(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCount('syntax', args.length, 1, position);

  const expanded = expandTemplate(
    args[0],
    env.collectTemplateBindings(),
    { ellipsis: '...', name: 'syntax' },
    [],
  );

  return syntaxValue(
    cloneExpr(hygienizeExpr(expanded, env.currentMacroDefinitionEnv() ?? env, new Map())),
  );
}

function evaluateSyntaxCaseCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    requireArgCountAtLeast('syntax-case', args.length, 3, position);
    const literals = parseLiteralIdentifierList('syntax-case', args[1]);
    const matcher: PatternMatcher = { ellipsis: '...', literals };

    return evaluateCps(args[0], env, (value) =>
      evaluateSyntaxCaseClausesCps(
        expectSyntax('syntax-case', { value, position: args[0].position }).expr,
        args.slice(2),
        matcher,
        env,
        continuation,
      ),
    );
  });
}

function evaluateSyntaxCaseClausesCps(
  input: Expr,
  clauses: Expr[],
  matcher: PatternMatcher,
  env: Environment,
  continuation: EvalContinuation,
  index = 0,
): EvalStep {
  return suspendStep(() => {
    if (index >= clauses.length) {
      throw new EvalError('syntax-case: no matching clause', input.position);
    }

    const clause = clauses[index];
    if (clause.type !== 'list' || clause.elements.length < 2) {
      throw new EvalError('syntax-case: expected clause', clause.position);
    }

    const [pattern, ...rest] = clause.elements;
    const bindings = matchPattern(pattern, input, matcher, new Map(), []);
    if (bindings === null) {
      return evaluateSyntaxCaseClausesCps(
        input,
        clauses,
        matcher,
        env,
        continuation,
        index + 1,
      );
    }

    const clauseEnv = new Environment(env, {
      templateBindings: bindings,
      macroDefinitionEnv: env.currentMacroDefinitionEnv(),
    });

    if (rest.length === 1) {
      return evaluateCps(rest[0], clauseEnv, continuation);
    }

    const [fender, ...body] = rest;
    if (body.length === 0) {
      throw new EvalError('syntax-case: expected clause body', clause.position);
    }

    return evaluateCps(fender, clauseEnv, (guardValue) =>
      isTruthy(guardValue)
        ? evaluateSequenceCps(body, clauseEnv, continuation)
        : evaluateSyntaxCaseClausesCps(input, clauses, matcher, env, continuation, index + 1),
    );
  });
}

function evaluateWithSyntaxCps(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  continuation: EvalContinuation,
): EvalStep {
  return suspendStep(() => {
    requireArgCountAtLeast('with-syntax', args.length, 2, position);
    const bindingsExpr = args[0];
    if (bindingsExpr.type !== 'list') {
      throw new EvalError('with-syntax: expected binding list', bindingsExpr.position);
    }

    return evaluateWithSyntaxBindingsCps(bindingsExpr.elements, env, 0, new Map(), (bindings) =>
      evaluateSequenceCps(
        args.slice(1),
        new Environment(env, {
          templateBindings: bindings,
          macroDefinitionEnv: env.currentMacroDefinitionEnv(),
        }),
        continuation,
      ),
    );
  });
}

function evaluateWithSyntaxBindingsCps(
  bindings: Expr[],
  env: Environment,
  index: number,
  collected: MatchBindings,
  continuation: (bindings: MatchBindings) => EvalStep,
): EvalStep {
  return suspendStep(() => {
    if (index >= bindings.length) {
      return continuation(collected);
    }

    const bindingExpr = bindings[index];
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError('with-syntax: expected (pattern expr) binding', bindingExpr.position);
    }

    const [pattern, valueExpr] = bindingExpr.elements;
    return evaluateCps(valueExpr, env, (value) => {
      const syntaxObject = expectSyntax('with-syntax', { value, position: valueExpr.position });
      const matched = matchPattern(
        pattern,
        syntaxObject.expr,
        { ellipsis: '...', literals: new Set<string>() },
        new Map(),
        [],
      );
      if (matched === null) {
        throw new EvalError('with-syntax: pattern did not match syntax object', pattern.position);
      }

      return evaluateWithSyntaxBindingsCps(
        bindings,
        env,
        index + 1,
        mergeTemplateBindings(collected, matched, pattern.position),
        continuation,
      );
    });
  });
}

function evaluateLambda(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('lambda', args.length, 2, position);
  const { params, restParam } = parseFormalParameters('lambda', args[0]);

  return {
    type: 'closure',
    params,
    restParam,
    body: args.slice(1),
    env,
  };
}

function evaluateCaseLambda(args: Expr[], env: Environment, position: SourcePosition): SchemeValue {
  requireArgCountAtLeast('case-lambda', args.length, 1, position);
  return {
    type: 'case-closure',
    clauses: args.map(parseCaseLambdaClause),
    env,
  };
}

function evaluateAnd(args: Expr[], env: Environment): EvalOutcome {
  if (args.length === 0) {
    return valueOutcome(booleanValue(true));
  }

  for (let index = 0; index < args.length - 1; index += 1) {
    const result = evaluate(args[index], env);
    if (!isTruthy(result)) {
      return valueOutcome(result);
    }
  }

  return tailOutcome(args[args.length - 1], env);
}

function evaluateOr(args: Expr[], env: Environment): EvalOutcome {
  if (args.length === 0) {
    return valueOutcome(booleanValue(false));
  }

  for (let index = 0; index < args.length - 1; index += 1) {
    const result = evaluate(args[index], env);
    if (isTruthy(result)) {
      return valueOutcome(result);
    }
  }

  return tailOutcome(args[args.length - 1], env);
}

function evaluateBegin(args: Expr[], env: Environment): EvalOutcome {
  return evaluateSequenceOutcome(args, env);
}

function evaluateLet(args: Expr[], env: Environment, position: SourcePosition): EvalOutcome {
  requireArgCountAtLeast('let', args.length, 2, position);

  const firstArg = args[0];
  if (firstArg.type === 'symbol') {
    requireArgCountAtLeast('let', args.length, 3, position);

    const bindings = parseLetBindings('let', args[1]);
    const values = bindings.map((binding) => ({
      value: evaluate(binding.value, env),
      position: binding.value.position,
    }));
    const letEnv = new Environment(env);
    const closure: Closure = {
      type: 'closure',
      params: bindings.map((binding) => symbolKey(binding.name)),
      body: args.slice(2),
      env: letEnv,
    };

    letEnv.define(symbolKey(firstArg), closure);
    return applyProcedureOutcome(closure, values, firstArg.position);
  }

  const bindings = parseLetBindings('let', firstArg);
  const letEnv = new Environment(env);

  for (const binding of bindings) {
    letEnv.define(symbolKey(binding.name), evaluate(binding.value, env));
  }

  return evaluateSequenceOutcome(args.slice(1), letEnv);
}

function evaluateLetStar(args: Expr[], env: Environment, position: SourcePosition): EvalOutcome {
  requireArgCountAtLeast('let*', args.length, 2, position);

  const bindings = parseLetBindings('let*', args[0]);
  const letStarEnv = new Environment(env);

  for (const binding of bindings) {
    letStarEnv.define(symbolKey(binding.name), evaluate(binding.value, letStarEnv));
  }

  return evaluateSequenceOutcome(args.slice(1), letStarEnv);
}

function evaluateCond(args: Expr[], env: Environment): EvalOutcome {
  for (const clause of args) {
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError('cond: expected non-empty clause', clause.position);
    }

    const [testExpr, ...body] = clause.elements;

    if (testExpr.type === 'symbol' && testExpr.name === 'else') {
      return body.length === 0 ? valueOutcome(VOID_VALUE) : evaluateSequenceOutcome(body, env);
    }

    const arrowRecipient = condArrowRecipient(body, clause.position);
    const testValue = evaluate(testExpr, env);
    if (isTruthy(testValue)) {
      if (arrowRecipient !== undefined) {
        return applyProcedureOutcome(
          evaluate(arrowRecipient, env),
          [{ value: testValue, position: testExpr.position }],
          arrowRecipient.position,
        );
      }

      return body.length === 0 ? valueOutcome(testValue) : evaluateSequenceOutcome(body, env);
    }
  }

  return valueOutcome(VOID_VALUE);
}

function condArrowRecipient(body: Expr[], clausePosition: SourcePosition): Expr | undefined {
  if (body[0]?.type !== 'symbol' || body[0].name !== '=>') {
    return undefined;
  }

  if (body.length !== 2) {
    throw new EvalError('cond: => clause expects exactly one recipient', clausePosition);
  }

  return body[1];
}

function evaluateLetrec(
  args: Expr[],
  env: Environment,
  position: SourcePosition,
  sequential: boolean,
): EvalOutcome {
  const name = sequential ? 'letrec*' : 'letrec';
  requireArgCountAtLeast(name, args.length, 2, position);

  const bindings = parseLetBindings(name, args[0]);
  const letrecEnv = new Environment(env);

  for (const binding of bindings) {
    letrecEnv.defineUninitialized(symbolKey(binding.name));
  }

  if (sequential) {
    for (const binding of bindings) {
      letrecEnv.set(
        symbolKey(binding.name),
        evaluate(binding.value, letrecEnv),
        binding.value.position,
      );
    }
  } else {
    const values = bindings.map((binding) => evaluate(binding.value, letrecEnv));
    for (let index = 0; index < bindings.length; index += 1) {
      letrecEnv.set(symbolKey(bindings[index].name), values[index], bindings[index].value.position);
    }
  }

  return evaluateSequenceOutcome(args.slice(1), letrecEnv);
}

function evaluateCase(args: Expr[], env: Environment, position: SourcePosition): EvalOutcome {
  requireArgCountAtLeast('case', args.length, 2, position);

  const key = evaluate(args[0], env);
  const clauses = args.slice(1);

  for (let index = 0; index < clauses.length; index += 1) {
    const clause = clauses[index];
    if (clause.type !== 'list' || clause.elements.length === 0) {
      throw new EvalError('case: expected non-empty clause', clause.position);
    }

    const [head, ...body] = clause.elements;
    if (head.type === 'symbol' && head.name === 'else') {
      if (index !== clauses.length - 1) {
        throw new EvalError('case: else clause must be last', head.position);
      }

      return body.length === 0 ? valueOutcome(VOID_VALUE) : evaluateSequenceOutcome(body, env);
    }

    if (head.type !== 'list') {
      throw new EvalError('case: expected datum list', head.position);
    }

    if (head.elements.some((datum) => eqvValues(key, quoteExpr(datum)))) {
      return body.length === 0 ? valueOutcome(VOID_VALUE) : evaluateSequenceOutcome(body, env);
    }
  }

  return valueOutcome(VOID_VALUE);
}

function evaluateDo(args: Expr[], env: Environment, position: SourcePosition): EvalOutcome {
  requireArgCountAtLeast('do', args.length, 2, position);

  const bindings = parseDoBindings(args[0]);
  const testClause = args[1];
  if (testClause.type !== 'list' || testClause.elements.length === 0) {
    throw new EvalError('do: expected termination clause', testClause.position);
  }

  const [testExpr, ...resultExprs] = testClause.elements;
  const body = args.slice(2);
  const doEnv = new Environment(env);

  for (const binding of bindings) {
    doEnv.define(symbolKey(binding.name), evaluate(binding.init, env));
  }

  while (true) {
    if (isTruthy(evaluate(testExpr, doEnv))) {
      return resultExprs.length === 0 ? valueOutcome(VOID_VALUE) : evaluateSequenceOutcome(resultExprs, doEnv);
    }

    evaluateSequence(body, doEnv);

    const nextValues = bindings.map((binding) =>
      binding.step === undefined
        ? undefined
        : {
            name: symbolKey(binding.name),
            value: evaluate(binding.step, doEnv),
            position: binding.step.position,
          },
    );

    for (const nextValue of nextValues) {
      if (nextValue !== undefined) {
        doEnv.set(nextValue.name, nextValue.value, nextValue.position);
      }
    }
  }
}

function quoteExpr(expr: Expr): SchemeValue {
  switch (expr.type) {
    case 'number':
      return expr;
    case 'boolean':
      return booleanValue(expr.value);
    case 'string':
      return stringValue(expr.value);
    case 'char':
      return charValue(expr.value, expr.position);
    case 'symbol':
      return { type: 'symbol', name: expr.name };
    case 'list': {
      const parts = splitDottedList(expr.elements);
      return listValue(
        parts.head.map(quoteExpr),
        parts.tail === undefined ? EMPTY_LIST : quoteExpr(parts.tail),
      );
    }
  }
}

function datumToExpr(value: SchemeValue, position: SourcePosition): Expr {
  switch (value.type) {
    case 'number':
      return { type: 'number', value: value.value, position };
    case 'boolean':
      return { type: 'boolean', value: value.value, position };
    case 'string':
      return { type: 'string', value: value.value, position };
    case 'char':
      return { type: 'char', value: value.value, position };
    case 'symbol':
      return { type: 'symbol', name: value.name, position };
    case 'syntax':
      return cloneExpr(value.expr);
    case 'list': {
      const elements: Expr[] = [];
      let cursor: SchemeValue = value;

      while (cursor.type === 'list' && cursor.pair !== undefined) {
        elements.push(datumToExpr(cursor.pair.car, position));
        cursor = cursor.pair.cdr;
      }

      return buildListExprFromParts(
        elements,
        cursor.type === 'list' && cursor.pair === undefined
          ? undefined
          : datumToExpr(cursor, position),
        position,
      );
    }
    default:
      throw new EvalError('datum->syntax: unsupported datum', position);
  }
}

function parseFormalParameters(name: string, paramsExpr: Expr): { params: string[]; restParam?: string } {
  if (paramsExpr.type === 'symbol') {
    return { params: [], restParam: symbolKey(paramsExpr) };
  }

  if (paramsExpr.type !== 'list') {
    throw new EvalError(`${name}: parameter list must be a list`, paramsExpr.position);
  }

  return parseParameterList(name, paramsExpr.elements);
}

function parseParameterList(name: string, params: Expr[]): { params: string[]; restParam?: string } {
  const names: string[] = [];

  for (let index = 0; index < params.length; index += 1) {
    const param = params[index];
    if (param.type !== 'symbol') {
      throw new EvalError(`${name}: parameter names must be symbols`, param.position);
    }

    if (param.name !== '.') {
      names.push(symbolKey(param));
      continue;
    }

    const restParam = params[index + 1];
    if (
      restParam === undefined ||
      restParam.type !== 'symbol' ||
      restParam.name === '.' ||
      index + 2 !== params.length
    ) {
      throw new EvalError(`${name}: invalid rest parameter list`, param.position);
    }

    return { params: names, restParam: symbolKey(restParam) };
  }

  return { params: names };
}

function parseCaseLambdaClause(clauseExpr: Expr): CaseClosureClause {
  if (clauseExpr.type !== 'list' || clauseExpr.elements.length < 2) {
    throw new EvalError('case-lambda: expected clause', clauseExpr.position);
  }

  const [paramsExpr, ...body] = clauseExpr.elements;
  const { params, restParam } = parseFormalParameters('case-lambda', paramsExpr);
  return { params, restParam, body };
}

function parseLetBindings(name: string, bindingsExpr: Expr): Array<{ name: SymbolExpr; value: Expr }> {
  if (bindingsExpr.type !== 'list') {
    throw new EvalError(`${name}: expected binding list`, bindingsExpr.position);
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      throw new EvalError(`${name}: expected binding pair`, bindingExpr.position);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    if (nameExpr.type !== 'symbol') {
      throw new EvalError(`${name}: binding name must be a symbol`, nameExpr.position);
    }

    return { name: nameExpr, value: valueExpr };
  });
}

function parseDoBindings(bindingsExpr: Expr): DoBinding[] {
  if (bindingsExpr.type !== 'list') {
    throw new EvalError('do: expected binding list', bindingsExpr.position);
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (
      bindingExpr.type !== 'list' ||
      (bindingExpr.elements.length !== 2 && bindingExpr.elements.length !== 3)
    ) {
      throw new EvalError('do: expected binding of form (name init step?)', bindingExpr.position);
    }

    const [nameExpr, initExpr, stepExpr] = bindingExpr.elements;
    if (nameExpr.type !== 'symbol') {
      throw new EvalError('do: binding name must be a symbol', nameExpr.position);
    }

    return { name: nameExpr, init: initExpr, step: stepExpr };
  });
}

function parseRecordConstructorSpec(bindingsExpr: Expr): {
  constructorName: SymbolExpr;
  constructorFieldTags: string[];
} {
  if (bindingsExpr.type !== 'list' || bindingsExpr.elements.length === 0) {
    throw new EvalError(
      'define-record-type: expected constructor specification',
      bindingsExpr.position,
    );
  }

  const constructorName = bindingsExpr.elements[0];
  if (constructorName.type !== 'symbol') {
    throw new EvalError(
      'define-record-type: expected constructor name',
      constructorName.position,
    );
  }

  const constructorFieldTags = bindingsExpr.elements.slice(1).map((fieldExpr) => {
    if (fieldExpr.type !== 'symbol') {
      throw new EvalError(
        'define-record-type: constructor field tags must be symbols',
        fieldExpr.position,
      );
    }

    return fieldExpr.name;
  });

  return { constructorName, constructorFieldTags };
}

function parseRecordFieldSpec(fieldExpr: Expr): RecordFieldSpec {
  if (
    fieldExpr.type !== 'list' ||
    (fieldExpr.elements.length !== 2 && fieldExpr.elements.length !== 3)
  ) {
    throw new EvalError('define-record-type: expected field specification', fieldExpr.position);
  }

  const [tagExpr, accessorExpr, mutatorExpr] = fieldExpr.elements;
  if (tagExpr.type !== 'symbol') {
    throw new EvalError('define-record-type: field tag must be a symbol', tagExpr.position);
  }

  if (accessorExpr.type !== 'symbol') {
    throw new EvalError('define-record-type: accessor name must be a symbol', accessorExpr.position);
  }

  if (mutatorExpr !== undefined && mutatorExpr.type !== 'symbol') {
    throw new EvalError('define-record-type: mutator name must be a symbol', mutatorExpr.position);
  }

  return {
    tag: tagExpr.name,
    accessorName: accessorExpr,
    mutatorName: mutatorExpr,
  };
}

function symbolKey(symbol: SymbolExpr): string {
  return symbol.resolvedName ?? symbol.name;
}

function parseLiteralIdentifierList(name: string, expr: Expr): Set<string> {
  if (expr.type !== 'list') {
    throw new EvalError(`${name}: expected literal identifier list`, expr.position);
  }

  const literals = new Set<string>();
  for (const literal of expr.elements) {
    if (literal.type !== 'symbol') {
      throw new EvalError(`${name}: literal identifiers must be symbols`, literal.position);
    }

    literals.add(literal.name);
  }

  return literals;
}

function parseMacroTransformer(
  keywordName: string,
  transformerExpr: Expr,
  env: Environment,
): MacroBinding {
  if (
    transformerExpr.type === 'list' &&
    transformerExpr.elements.length >= 1 &&
    transformerExpr.elements[0].type === 'symbol' &&
    transformerExpr.elements[0].name === 'syntax-rules'
  ) {
    return parseSyntaxRules(keywordName, transformerExpr, env);
  }

  const transformer = evaluate(transformerExpr, env);
  if (!isProcedureValue(transformer)) {
    throw new EvalError('define-syntax: expected transformer procedure', transformerExpr.position);
  }

  return {
    type: 'procedure-macro',
    name: keywordName,
    transformer,
    env,
  };
}

function parseSyntaxRules(
  keywordName: string,
  transformerExpr: Expr,
  env: Environment,
): SyntaxRulesMacro {
  if (transformerExpr.type !== 'list' || transformerExpr.elements.length < 2) {
    throw new EvalError('define-syntax: expected syntax-rules transformer', transformerExpr.position);
  }

  const [head, ...rest] = transformerExpr.elements;
  if (head.type !== 'symbol' || head.name !== 'syntax-rules') {
    throw new EvalError('define-syntax: expected syntax-rules transformer', transformerExpr.position);
  }

  let ellipsis = '...';
  let literalsExpr: Expr | undefined;
  let ruleExprs: Expr[] = [];

  if (rest[0]?.type === 'symbol') {
    ellipsis = rest[0].name;
    literalsExpr = rest[1];
    ruleExprs = rest.slice(2);
  } else {
    literalsExpr = rest[0];
    ruleExprs = rest.slice(1);
  }

  if (literalsExpr === undefined || literalsExpr.type !== 'list') {
    throw new EvalError('syntax-rules: expected literal identifier list', transformerExpr.position);
  }

  if (ruleExprs.length === 0) {
    throw new EvalError('syntax-rules: expected at least one rule', transformerExpr.position);
  }

  const literals = parseLiteralIdentifierList('syntax-rules', literalsExpr);
  literals.add(keywordName);

  const rules = ruleExprs.map((ruleExpr) => {
    if (ruleExpr.type !== 'list' || ruleExpr.elements.length !== 2) {
      throw new EvalError('syntax-rules: expected (pattern template) rule', ruleExpr.position);
    }

    const [pattern, template] = ruleExpr.elements;
    return { pattern, template };
  });

  return {
    type: 'syntax-rules',
    name: keywordName,
    ellipsis,
    literals,
    rules,
    env,
  };
}

function expandMacroCall(macro: MacroBinding, expr: Extract<Expr, { type: 'list' }>): Expr {
  switch (macro.type) {
    case 'syntax-rules':
      return expandSyntaxRulesMacroCall(macro, expr);
    case 'procedure-macro':
      return expandProcedureMacroCall(macro, expr);
  }
}

function expandSyntaxRulesMacroCall(
  macro: SyntaxRulesMacro,
  expr: Extract<Expr, { type: 'list' }>,
): Expr {
  for (const rule of macro.rules) {
    const bindings = matchPattern(rule.pattern, expr, macro, new Map(), []);
    if (bindings === null) {
      continue;
    }

    const expanded = expandTemplate(
      rule.template,
      bindings,
      { ellipsis: macro.ellipsis, name: 'syntax-rules' },
      [],
    );
    return cloneExpr(hygienizeExpr(expanded, macro.env, new Map()));
  }

  throw new EvalError(`${macro.name}: no matching syntax-rules pattern`, expr.position);
}

function expandProcedureMacroCall(
  macro: ProcedureMacro,
  expr: Extract<Expr, { type: 'list' }>,
): Expr {
  const result = runEvalStep(
    applyProcedureCps(
      wrapMacroTransformer(macro.transformer, macro.env),
      [{ value: syntaxValue(cloneExpr(expr)), position: expr.position }],
      expr.position,
      completeEval,
    ),
  );

  if (result.type !== 'syntax') {
    throw new EvalError(`${macro.name}: transformer must return a syntax object`, expr.position);
  }

  return cloneExpr(result.expr);
}

function matchPattern(
  pattern: Expr,
  input: Expr,
  matcher: PatternMatcher,
  bindings: MatchBindings,
  path: number[],
): MatchBindings | null {
  switch (pattern.type) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return exprSyntaxEqual(pattern, input) ? bindings : null;
    case 'symbol':
      if (matcher.literals.has(pattern.name) || pattern.name === matcher.ellipsis) {
        return input.type === 'symbol' && input.name === pattern.name ? bindings : null;
      }

      return bindPatternVariable(bindings, pattern.name, path, input);
    case 'list':
      if (input.type !== 'list') {
        return null;
      }

      return matchPatternList(pattern.elements, input.elements, matcher, bindings, path);
  }
}

function matchPatternList(
  patternElements: Expr[],
  inputElements: Expr[],
  matcher: PatternMatcher,
  bindings: MatchBindings,
  path: number[],
): MatchBindings | null {
  const patternParts = splitDottedList(patternElements);
  const inputParts = splitDottedList(inputElements);
  return matchPatternSequence(
    patternParts.head,
    inputParts.head,
    inputParts.tail,
    patternParts.tail,
    matcher,
    bindings,
    path,
    0,
    0,
  );
}

function matchPatternSequence(
  patterns: Expr[],
  inputs: Expr[],
  inputTail: Expr | undefined,
  tailPattern: Expr | undefined,
  matcher: PatternMatcher,
  bindings: MatchBindings,
  path: number[],
  patternIndex: number,
  inputIndex: number,
): MatchBindings | null {
  if (patternIndex === patterns.length) {
    if (tailPattern === undefined) {
      return inputIndex === inputs.length && inputTail === undefined ? bindings : null;
    }

    return matchPattern(
      tailPattern,
      buildPatternRemainderExpr(inputs.slice(inputIndex), inputTail),
      matcher,
      bindings,
      path,
    );
  }

  const pattern = patterns[patternIndex];
  if (patternIndex + 1 < patterns.length && isEllipsisExpr(patterns[patternIndex + 1], matcher.ellipsis)) {
    const seededBindings = seedRepeatedPatternBindings(bindings, pattern, matcher, path);
    const minimumRemainingLength = minimumPatternLength(patterns.slice(patternIndex + 2), matcher);
    const maxRepeatCount = inputs.length - inputIndex - minimumRemainingLength;
    if (maxRepeatCount < 0) {
      return null;
    }

    for (let repeatCount = 0; repeatCount <= maxRepeatCount; repeatCount += 1) {
      let repeatedBindings = seededBindings;
      let matched = true;

      for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
        const nextBindings = matchPattern(
          pattern,
          inputs[inputIndex + repeatIndex],
          matcher,
          repeatedBindings,
          [...path, repeatIndex],
        );
        if (nextBindings === null) {
          matched = false;
          break;
        }

        repeatedBindings = nextBindings;
      }

      if (!matched) {
        continue;
      }

      const remainingBindings = matchPatternSequence(
        patterns,
        inputs,
        inputTail,
        tailPattern,
        matcher,
        repeatedBindings,
        path,
        patternIndex + 2,
        inputIndex + repeatCount,
      );
      if (remainingBindings !== null) {
        return remainingBindings;
      }
    }

    return null;
  }

  if (inputIndex >= inputs.length) {
    return null;
  }

  const nextBindings = matchPattern(pattern, inputs[inputIndex], matcher, bindings, path);
  if (nextBindings === null) {
    return null;
  }

  return matchPatternSequence(
    patterns,
    inputs,
    inputTail,
    tailPattern,
    matcher,
    nextBindings,
    path,
    patternIndex + 1,
    inputIndex + 1,
  );
}

function minimumPatternLength(patterns: Expr[], matcher: PatternMatcher): number {
  let length = 0;

  for (let index = 0; index < patterns.length; index += 1) {
    if (index + 1 < patterns.length && isEllipsisExpr(patterns[index + 1], matcher.ellipsis)) {
      index += 1;
      continue;
    }

    length += 1;
  }

  return length;
}

function seedRepeatedPatternBindings(
  bindings: MatchBindings,
  pattern: Expr,
  matcher: PatternMatcher,
  path: number[],
): MatchBindings {
  const variableNames = collectPatternVariables(pattern, matcher, new Set<string>());
  if (variableNames.size === 0) {
    return bindings;
  }

  const nextBindings = new Map(bindings);
  for (const name of variableNames) {
    nextBindings.set(name, ensureArrayBinding(nextBindings.get(name), path));
  }

  return nextBindings;
}

function collectPatternVariables(
  pattern: Expr,
  matcher: PatternMatcher,
  names: Set<string>,
): Set<string> {
  switch (pattern.type) {
    case 'symbol':
      if (!matcher.literals.has(pattern.name) && pattern.name !== matcher.ellipsis) {
        names.add(pattern.name);
      }
      return names;
    case 'list':
      for (const element of splitDottedList(pattern.elements).head) {
        collectPatternVariables(element, matcher, names);
      }

      if (splitDottedList(pattern.elements).tail !== undefined) {
        collectPatternVariables(splitDottedList(pattern.elements).tail!, matcher, names);
      }

      return names;
    default:
      return names;
  }
}

function ensureArrayBinding(binding: MatchBinding | undefined, path: number[]): MatchBinding {
  if (path.length === 0) {
    return binding ?? [];
  }

  if (binding !== undefined && !Array.isArray(binding)) {
    return binding;
  }

  const [index, ...rest] = path;
  const values = binding === undefined ? [] : [...binding];
  values[index] = ensureArrayBinding(values[index], rest);
  return values;
}

function bindPatternVariable(
  bindings: MatchBindings,
  name: string,
  path: number[],
  input: Expr,
): MatchBindings | null {
  const nextBinding = setMatchBinding(bindings.get(name), path, input);
  if (nextBinding === null) {
    return null;
  }

  const nextBindings = new Map(bindings);
  nextBindings.set(name, nextBinding);
  return nextBindings;
}

function setMatchBinding(
  binding: MatchBinding | undefined,
  path: number[],
  input: Expr,
): MatchBinding | null {
  if (path.length === 0) {
    if (binding === undefined) {
      return input;
    }

    if (Array.isArray(binding)) {
      return null;
    }

    return exprSyntaxEqual(binding, input) ? binding : null;
  }

  if (binding !== undefined && !Array.isArray(binding)) {
    return null;
  }

  const [index, ...rest] = path;
  const values = binding === undefined ? [] : [...binding];
  const nextBinding = setMatchBinding(values[index], rest, input);
  if (nextBinding === null) {
    return null;
  }

  values[index] = nextBinding;
  return values;
}

function getMatchBinding(binding: MatchBinding | undefined, path: number[]): MatchBinding | undefined {
  let current = binding;

  for (const index of path) {
    if (!Array.isArray(current)) {
      return undefined;
    }

    current = current[index];
  }

  return current;
}

function mergeTemplateBindings(
  left: MatchBindings,
  right: MatchBindings,
  position: SourcePosition,
): MatchBindings {
  const merged = new Map(left);
  for (const [name, binding] of right) {
    if (merged.has(name)) {
      throw new EvalError(`with-syntax: duplicate template binding ${name}`, position);
    }

    merged.set(name, binding);
  }

  return merged;
}

function expandTemplate(
  template: Expr,
  bindings: MatchBindings,
  templateContext: TemplateContext,
  path: number[],
): Expr {
  switch (template.type) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return { ...cloneExpr(template), introduced: true };
    case 'symbol': {
      if (template.name === templateContext.ellipsis) {
        throw new EvalError(`${templateContext.name}: invalid ellipsis in template`, template.position);
      }

      const binding = bindings.get(template.name);
      if (binding === undefined) {
        return { ...cloneExpr(template), introduced: true };
      }

      const value = getMatchBinding(binding, path);
      if (value === undefined || Array.isArray(value)) {
        throw new EvalError(`${templateContext.name}: invalid template ellipsis usage`, template.position);
      }

      return cloneExpr(value);
    }
    case 'list': {
      const parts = splitDottedList(template.elements);
      const elements: Expr[] = [];

      for (let index = 0; index < parts.head.length; index += 1) {
        const element = parts.head[index];
        if (
          index + 1 < parts.head.length &&
          isEllipsisExpr(parts.head[index + 1], templateContext.ellipsis)
        ) {
          const repeatCount = findTemplateRepeatCount(element, bindings, path);
          if (repeatCount === null) {
            throw new EvalError(
              `${templateContext.name}: template ellipsis has no repeated variable`,
              element.position,
            );
          }

          for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
            elements.push(expandTemplate(element, bindings, templateContext, [...path, repeatIndex]));
          }

          index += 1;
          continue;
        }

        elements.push(expandTemplate(element, bindings, templateContext, path));
      }

      return buildListExprFromParts(
        elements,
        parts.tail === undefined
          ? undefined
          : expandTemplate(parts.tail, bindings, templateContext, path),
        template.position,
        true,
        parts.dotExpr,
      );
    }
  }
}

function findTemplateRepeatCount(
  template: Expr,
  bindings: MatchBindings,
  path: number[],
): number | null {
  switch (template.type) {
    case 'symbol': {
      const binding = bindings.get(template.name);
      if (binding === undefined) {
        return null;
      }

      const value = getMatchBinding(binding, path);
      return Array.isArray(value) ? value.length : null;
    }
    case 'list':
      for (const element of splitDottedList(template.elements).head) {
        const repeatCount = findTemplateRepeatCount(element, bindings, path);
        if (repeatCount !== null) {
          return repeatCount;
        }
      }

      if (splitDottedList(template.elements).tail !== undefined) {
        return findTemplateRepeatCount(splitDottedList(template.elements).tail!, bindings, path);
      }

      return null;
    default:
      return null;
  }
}

function hygienizeExpr(
  expr: Expr,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.type === 'symbol') {
    return hygienizeSymbol(expr, definitionEnv, scope);
  }

  if (expr.type !== 'list' || !expr.introduced) {
    return expr;
  }

  const operator = expr.elements[0];
  if (operator?.type === 'symbol') {
    switch (operator.name) {
      case 'quote':
        return expr;
      case 'lambda':
        return hygienizeLambdaExpr(expr, definitionEnv, scope);
      case 'case-lambda':
        return hygienizeCaseLambdaExpr(expr, definitionEnv, scope);
      case 'let':
        return hygienizeLetExpr(expr, definitionEnv, scope);
      case 'define':
        return hygienizeDefineExpr(expr, definitionEnv, scope);
    }
  }

  return {
    ...expr,
    elements: expr.elements.map((element) => hygienizeExpr(element, definitionEnv, scope)),
  };
}

function hygienizeSymbol(
  expr: SymbolExpr,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (!expr.introduced) {
    return expr;
  }

  const renamed = scope.get(expr.name);
  if (renamed !== undefined) {
    return { ...expr, resolvedName: renamed };
  }

  if (SPECIAL_FORM_NAMES.has(expr.name)) {
    return expr;
  }

  const syntax = definitionEnv.tryLookupSyntax(expr.name);
  if (syntax !== undefined) {
    return { ...expr, capturedSyntax: syntax };
  }

  const cell = definitionEnv.tryLookupCell(expr.name);
  if (cell !== undefined) {
    return { ...expr, capturedCell: cell };
  }

  return expr;
}

function hygienizeLambdaExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.elements.length < 2) {
    return expr;
  }

  const transformedParams = hygienizeParameterSpec(expr.elements[1], scope);
  const transformedBody = hygienizeBodyExpressions(
    expr.elements.slice(2),
    definitionEnv,
    transformedParams.scope,
  );
  return {
    ...expr,
    elements: [
      expr.elements[0],
      transformedParams.paramsExpr,
      ...transformedBody.expressions,
    ],
  };
}

function hygienizeCaseLambdaExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.elements.length < 2) {
    return expr;
  }

  return {
    ...expr,
    elements: [
      expr.elements[0],
      ...expr.elements.slice(1).map((clause) => hygienizeCaseLambdaClause(clause, definitionEnv, scope)),
    ],
  };
}

function hygienizeCaseLambdaClause(
  clauseExpr: Expr,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (clauseExpr.type !== 'list' || clauseExpr.elements.length === 0) {
    return hygienizeExpr(clauseExpr, definitionEnv, scope);
  }

  const [paramsExpr, ...body] = clauseExpr.elements;
  const transformedParams = hygienizeParameterSpec(paramsExpr, scope);
  const transformedBody = hygienizeBodyExpressions(
    body,
    definitionEnv,
    transformedParams.scope,
  );

  return {
    ...clauseExpr,
    elements: [
      transformedParams.paramsExpr,
      ...transformedBody.expressions,
    ],
  };
}

function hygienizeLetExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  if (expr.elements.length < 3) {
    return expr;
  }

  const [operator, firstArg] = expr.elements;
  if (firstArg.type === 'symbol') {
    let bodyScope = new Map(scope);
    const renamedLet = freshenBinder(firstArg, bodyScope);
    bodyScope = renamedLet.scope;

    const transformedBindings = hygienizeLetBindings(expr.elements[2], definitionEnv, scope, bodyScope);
    const transformedBody = hygienizeBodyExpressions(
      expr.elements.slice(3),
      definitionEnv,
      transformedBindings.scope,
    );
    return {
      ...expr,
      elements: [
        operator,
        renamedLet.symbol,
        transformedBindings.bindingsExpr,
        ...transformedBody.expressions,
      ],
    };
  }

  const transformedBindings = hygienizeLetBindings(firstArg, definitionEnv, scope, new Map(scope));
  const transformedBody = hygienizeBodyExpressions(
    expr.elements.slice(2),
    definitionEnv,
    transformedBindings.scope,
  );
  return {
    ...expr,
    elements: [
      operator,
      transformedBindings.bindingsExpr,
      ...transformedBody.expressions,
    ],
  };
}

function hygienizeDefineExpr(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): Expr {
  return hygienizeDefineExprWithScope(expr, definitionEnv, scope).expr;
}

function hygienizeDefineExprWithScope(
  expr: Extract<Expr, { type: 'list' }>,
  definitionEnv: Environment,
  scope: Map<string, string>,
): { expr: Expr; scope: Map<string, string> } {
  if (expr.elements.length < 3) {
    return { expr, scope };
  }

  const [operator, target, ...rest] = expr.elements;

  if (target.type === 'symbol') {
    const renamedTarget = freshenBinder(target, new Map(scope));
    return {
      expr: {
        ...expr,
        elements: [
          operator,
          renamedTarget.symbol,
          ...rest.map((element) => hygienizeExpr(element, definitionEnv, scope)),
        ],
      },
      scope: renamedTarget.scope,
    };
  }

  if (target.type !== 'list' || target.elements.length === 0 || target.elements[0].type !== 'symbol') {
    return {
      expr: {
        ...expr,
        elements: expr.elements.map((element) => hygienizeExpr(element, definitionEnv, scope)),
      },
      scope,
    };
  }

  let bodyScope = new Map(scope);
  const renamedTarget = freshenBinder(target.elements[0], bodyScope);
  bodyScope = renamedTarget.scope;

  const transformedParams = hygienizeParameterSpec(
    { type: 'list', elements: target.elements.slice(1), position: target.position, introduced: target.introduced },
    bodyScope,
  );

  return {
    expr: {
      ...expr,
      elements: [
        operator,
        {
          type: 'list',
          position: target.position,
          introduced: target.introduced,
          elements: [renamedTarget.symbol, ...(transformedParams.paramsExpr as Extract<Expr, { type: 'list' }>).elements],
        },
        ...rest.map((element) => hygienizeExpr(element, definitionEnv, transformedParams.scope)),
      ],
    },
    scope: renamedTarget.scope,
  };
}

function hygienizeBodyExpressions(
  expressions: Expr[],
  definitionEnv: Environment,
  scope: Map<string, string>,
): { expressions: Expr[]; scope: Map<string, string> } {
  let currentScope = new Map(scope);
  const transformedExpressions: Expr[] = [];

  for (const expression of expressions) {
    const transformed = hygienizeBodyExpression(expression, definitionEnv, currentScope);
    transformedExpressions.push(transformed.expr);
    currentScope = transformed.scope;
  }

  return { expressions: transformedExpressions, scope: currentScope };
}

function hygienizeBodyExpression(
  expr: Expr,
  definitionEnv: Environment,
  scope: Map<string, string>,
): { expr: Expr; scope: Map<string, string> } {
  if (expr.type === 'list' && expr.introduced && expr.elements[0]?.type === 'symbol') {
    switch (expr.elements[0].name) {
      case 'define':
        return hygienizeDefineExprWithScope(expr, definitionEnv, scope);
      case 'begin': {
        const transformedBody = hygienizeBodyExpressions(
          expr.elements.slice(1),
          definitionEnv,
          scope,
        );

        return {
          expr: {
            ...expr,
            elements: [expr.elements[0], ...transformedBody.expressions],
          },
          scope: transformedBody.scope,
        };
      }
    }
  }

  return { expr: hygienizeExpr(expr, definitionEnv, scope), scope };
}

function hygienizeParameterSpec(
  paramsExpr: Expr,
  scope: Map<string, string>,
): { paramsExpr: Expr; scope: Map<string, string> } {
  let nextScope = new Map(scope);

  if (paramsExpr.type === 'symbol') {
    const renamed = freshenBinder(paramsExpr, nextScope);
    return { paramsExpr: renamed.symbol, scope: renamed.scope };
  }

  if (paramsExpr.type !== 'list') {
    return { paramsExpr, scope: nextScope };
  }

  const params: Expr[] = [];
  for (const param of paramsExpr.elements) {
    if (param.type !== 'symbol' || param.name === '.') {
      params.push(param);
      continue;
    }

    const renamed = freshenBinder(param, nextScope);
    params.push(renamed.symbol);
    nextScope = renamed.scope;
  }

  return {
    paramsExpr: { ...paramsExpr, elements: params },
    scope: nextScope,
  };
}

function hygienizeLetBindings(
  bindingsExpr: Expr,
  definitionEnv: Environment,
  valueScope: Map<string, string>,
  bodyScope: Map<string, string>,
): { bindingsExpr: Expr; scope: Map<string, string> } {
  if (bindingsExpr.type !== 'list') {
    return { bindingsExpr, scope: bodyScope };
  }

  let nextScope = new Map(bodyScope);
  const bindings = bindingsExpr.elements.map((bindingExpr) => {
    if (bindingExpr.type !== 'list' || bindingExpr.elements.length !== 2) {
      return hygienizeExpr(bindingExpr, definitionEnv, valueScope);
    }

    const [nameExpr, valueExpr] = bindingExpr.elements;
    const transformedValue = hygienizeExpr(valueExpr, definitionEnv, valueScope);

    if (nameExpr.type !== 'symbol' || nameExpr.name === '.') {
      return {
        ...bindingExpr,
        elements: [nameExpr, transformedValue],
      };
    }

    const renamed = freshenBinder(nameExpr, nextScope);
    nextScope = renamed.scope;

    return {
      ...bindingExpr,
      elements: [renamed.symbol, transformedValue],
    };
  });

  return {
    bindingsExpr: { ...bindingsExpr, elements: bindings },
    scope: nextScope,
  };
}

function freshenBinder(
  symbol: SymbolExpr,
  scope: Map<string, string>,
): { symbol: SymbolExpr; scope: Map<string, string> } {
  if (!symbol.introduced || symbol.name === '.') {
    return { symbol, scope };
  }

  const freshName = freshResolvedName(symbol.name);
  const nextScope = new Map(scope);
  nextScope.set(symbol.name, freshName);
  return {
    symbol: { ...symbol, resolvedName: freshName },
    scope: nextScope,
  };
}

function freshResolvedName(name: string): string {
  freshIdentifierCounter += 1;
  return `__macro_${freshIdentifierCounter}_${name}`;
}

function isCallCcExpr(expr: Expr | undefined): boolean {
  return expr?.type === 'list' && expr.elements[0]?.type === 'symbol' && expr.elements[0].name === 'call/cc';
}

function isEllipsisExpr(expr: Expr, ellipsis: string): boolean {
  return expr.type === 'symbol' && expr.name === ellipsis;
}

function splitDottedList(elements: Expr[]): DottedListParts {
  const dotExpr = elements[elements.length - 2];
  if (elements.length >= 2 && dotExpr.type === 'symbol' && dotExpr.name === '.') {
    return {
      head: elements.slice(0, -2),
      tail: elements[elements.length - 1],
      dotExpr,
    };
  }

  return { head: elements };
}

function buildPatternRemainderExpr(head: Expr[], tail: Expr | undefined): Expr {
  if (head.length === 0) {
    return tail ?? { type: 'list', elements: [], position: START_POSITION };
  }

  return buildListExprFromParts(
    head,
    tail,
    head[0].position,
  );
}

function buildListExprFromParts(
  head: Expr[],
  tail: Expr | undefined,
  position: SourcePosition,
  introduced?: boolean,
  dotExpr?: SymbolExpr,
): Extract<Expr, { type: 'list' }> {
  if (tail === undefined) {
    return { type: 'list', elements: head, position, introduced };
  }

  if (tail.type === 'list') {
    const tailParts = splitDottedList(tail.elements);
    return {
      type: 'list',
      position,
      introduced,
      elements: [
        ...head,
        ...tailParts.head,
        ...(tailParts.tail === undefined
          ? []
          : [
              makeDotExpr((tailParts.dotExpr ?? dotExpr)?.position ?? position, introduced),
              tailParts.tail,
            ]),
      ],
    };
  }

  return {
    type: 'list',
    position,
    introduced,
    elements: [
      ...head,
      makeDotExpr(dotExpr?.position ?? position, introduced),
      tail,
    ],
  };
}

function makeDotExpr(position: SourcePosition, introduced?: boolean): SymbolExpr {
  return {
    type: 'symbol',
    name: '.',
    position,
    introduced,
  };
}

function exprSyntaxEqual(left: Expr, right: Expr): boolean {
  if (left.type !== right.type) {
    return false;
  }

  switch (left.type) {
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'symbol':
      return left.name === (right as typeof left).name;
    case 'list': {
      const rightList = right as Extract<Expr, { type: 'list' }>;
      if (left.elements.length !== rightList.elements.length) {
        return false;
      }

      for (let index = 0; index < left.elements.length; index += 1) {
        if (!exprSyntaxEqual(left.elements[index], rightList.elements[index])) {
          return false;
        }
      }

      return true;
    }
  }
}

function cloneExpr<T extends Expr>(expr: T): T {
  switch (expr.type) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return { ...expr, introduced: undefined };
    case 'symbol':
      return { ...expr, introduced: undefined };
    case 'list':
      return {
        ...expr,
        introduced: undefined,
        elements: expr.elements.map((element) => cloneExpr(element)),
      } as T;
  }
}

function wrapMacroTransformer(value: SchemeValue, definitionEnv: Environment): SchemeValue {
  switch (value.type) {
    case 'closure':
      return {
        ...value,
        env: new Environment(value.env, { macroDefinitionEnv: definitionEnv }),
      };
    case 'case-closure':
      return {
        ...value,
        env: new Environment(value.env, { macroDefinitionEnv: definitionEnv }),
      };
    default:
      return value;
  }
}

function applyProcedureOutcome(
  value: SchemeValue,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
): EvalOutcome {
  switch (value.type) {
    case 'builtin':
      return valueOutcome(value.invoke(args, callPosition));
    case 'closure':
      return applyClosureOutcome(value, args, callPosition, 'lambda');
    case 'case-closure': {
      const clause = value.clauses.find((candidate) => matchesArity(candidate, args.length));
      if (clause === undefined) {
        throw new EvalError('case-lambda: wrong number of arguments', callPosition);
      }

      return applyClosureOutcome(
        { type: 'closure', env: value.env, ...clause },
        args,
        callPosition,
        'case-lambda',
      );
    }
    default:
      throw new EvalError('attempted to call a non-procedure', callPosition);
  }
}

function applyProcedure(
  value: SchemeValue,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
): SchemeValue {
  return resolveEvalOutcome(applyProcedureOutcome(value, args, callPosition));
}

function applyClosureOutcome(
  value: Closure,
  args: EvaluatedArg[],
  callPosition: SourcePosition,
  name: string,
): EvalOutcome {
  if (value.restParam === undefined) {
    requireArgCount(name, args.length, value.params.length, callPosition);
  } else {
    requireArgCountAtLeast(name, args.length, value.params.length, callPosition);
  }

  const callEnv = new Environment(value.env);
  for (let index = 0; index < value.params.length; index += 1) {
    callEnv.define(value.params[index], args[index].value);
  }

  if (value.restParam !== undefined) {
    callEnv.define(value.restParam, listValue(args.slice(value.params.length).map((arg) => arg.value)));
  }

  return evaluateSequenceOutcome(value.body, callEnv);
}

function matchesArity(
  clause: Pick<Closure, 'params' | 'restParam'>,
  argCount: number,
): boolean {
  return clause.restParam === undefined ? argCount === clause.params.length : argCount >= clause.params.length;
}

function evaluateSequence(expressions: Expr[], env: Environment): SchemeValue {
  return resolveEvalOutcome(evaluateSequenceOutcome(expressions, env));
}

function evaluateSequenceOutcome(expressions: Expr[], env: Environment): EvalOutcome {
  if (expressions.length === 0) {
    return valueOutcome(VOID_VALUE);
  }

  for (let index = 0; index < expressions.length - 1; index += 1) {
    evaluate(expressions[index], env);
  }

  return tailOutcome(expressions[expressions.length - 1], env);
}

function resolveEvalOutcome(outcome: EvalOutcome): SchemeValue {
  return outcome.kind === 'value' ? outcome.value : evaluate(outcome.expr, outcome.env);
}

function valueOutcome(value: SchemeValue): EvalOutcome {
  return { kind: 'value', value };
}

function tailOutcome(expr: Expr, env: Environment): EvalOutcome {
  return { kind: 'tail', expr, env };
}

function defineCoreSyntax(env: Environment): void {
  const [guardTransformer] = parseProgram(`
    (syntax-rules (else)
      ((guard (var clause ...) body ...)
       (call/cc
         (lambda (guard-k)
           (with-exception-handler
             (lambda (var)
               (guard-k
                 (cond clause ...
                       (else (raise var)))))
             (lambda () body ...))))))
  `);

  env.defineSyntax('guard', parseSyntaxRules('guard', guardTransformer, env));
}

function createGlobalEnv(context: EvaluationContext): Environment {
  const env = new Environment();
  const callWithCurrentContinuationBuiltin = (name: string): BuiltinProcedure =>
    builtin(
      name,
      (_args, callPosition) => {
        throw new EvalError(`${name}: internal error`, callPosition);
      },
      (args, callPosition, continuation) => {
        requireArgCount(name, args.length, 1, callPosition);
        return applyProcedureCps(
          args[0].value,
          [
            {
              value: continuationValue(
                context,
                context.dynamicWindStack,
                context.exceptionHandlers,
                (values) => continueWith(continuation, multiValueResult(values)),
              ),
              position: callPosition,
            },
          ],
          callPosition,
          continuation,
        );
      },
    );
  const dynamicWindBuiltin: BuiltinProcedure = builtin(
    'dynamic-wind',
    (_args, callPosition) => {
      throw new EvalError('dynamic-wind: internal error', callPosition);
    },
    (args, callPosition, continuation) => {
      requireArgCount('dynamic-wind', args.length, 3, callPosition);

      const baseStack = context.dynamicWindStack;
      const frame: DynamicWindFrame = {
        inThunk: args[0].value,
        outThunk: args[2].value,
      };

      return transitionDynamicWind(
        context,
        pushStack(baseStack, frame),
        callPosition,
        () =>
          applyProcedureCps(args[1].value, [], callPosition, (bodyValue) =>
            transitionDynamicWind(context, baseStack, callPosition, () =>
              continueWith(continuation, bodyValue),
            ),
          ),
      );
    },
  );
  const raiseBuiltin: BuiltinProcedure = builtin(
    'raise',
    (_args, callPosition) => {
      throw new EvalError('raise: internal error', callPosition);
    },
    (args, callPosition) => {
      requireArgCount('raise', args.length, 1, callPosition);
      return raiseException(context, args[0].value, callPosition);
    },
  );
  const errorBuiltin: BuiltinProcedure = builtin(
    'error',
    (args, callPosition) => {
      requireArgCountAtLeast('error', args.length, 1, callPosition);
      throw new EvalError(buildErrorMessage(args), callPosition);
    },
    (args, callPosition) => {
      requireArgCountAtLeast('error', args.length, 1, callPosition);
      return raiseException(context, stringValue(buildErrorMessage(args)), callPosition);
    },
  );
  const withExceptionHandlerBuiltin: BuiltinProcedure = builtin(
    'with-exception-handler',
    (_args, callPosition) => {
      throw new EvalError('with-exception-handler: internal error', callPosition);
    },
    (args, callPosition, continuation) => {
      requireArgCount('with-exception-handler', args.length, 2, callPosition);

      const frame: ExceptionHandlerFrame = {
        windStack: context.dynamicWindStack,
        handle: (exception, raisePosition) =>
          applyProcedureCps(
            args[0].value,
            [{ value: exception, position: raisePosition }],
            raisePosition,
            () => {
              throw new EvalError('raise: handler returned', raisePosition);
            },
          ),
      };

      pushExceptionHandlerFrame(context, frame);
      return applyProcedureCps(args[1].value, [], callPosition, (value) => {
        removeExceptionHandlerFrame(context, frame);
        return continueWith(continuation, value);
      });
    },
  );
  const valuesBuiltin: BuiltinProcedure = builtin(
    'values',
    (args) => multiValueResult(args.map((arg) => arg.value)),
    (args, _callPosition, continuation) =>
      continueWith(continuation, multiValueResult(args.map((arg) => arg.value))),
  );
  const callWithValuesBuiltin: BuiltinProcedure = builtin(
    'call-with-values',
    (args, callPosition) => {
      requireArgCount('call-with-values', args.length, 2, callPosition);
      const produced = applyProcedure(args[0].value, [], callPosition);
      return applyProcedure(args[1].value, multiValueArgs(produced, callPosition), callPosition);
    },
    (args, callPosition, continuation) => {
      requireArgCount('call-with-values', args.length, 2, callPosition);
      return applyProcedureCps(args[0].value, [], callPosition, (produced) =>
        applyProcedureCps(
          args[1].value,
          multiValueArgs(produced, callPosition),
          callPosition,
          continuation,
        ),
      );
    },
  );

  env.define(
    '+',
    builtin('+', (args, callPosition) =>
      numberValue(addNumbers(evaluateNumberArgs('+', args), callPosition), callPosition),
    ),
  );

  env.define(
    '*',
    builtin('*', (args, callPosition) =>
      numberValue(multiplyNumbers(evaluateNumberArgs('*', args), callPosition), callPosition),
    ),
  );

  env.define(
    '-',
    builtin('-', (args, callPosition) => {
      const values = evaluateNumberArgs('-', args);
      requireArgCountAtLeast('-', values.length, 1, callPosition);
      return numberValue(subtractNumbers(values, callPosition), callPosition);
    }),
  );

  env.define(
    '/',
    builtin('/', (args, callPosition) => {
      const values = evaluateNumberArgs('/', args);
      requireArgCountAtLeast('/', values.length, 1, callPosition);

      if (values.length === 1) {
        if (isZeroNumber(values[0])) {
          throw new EvalError('division by zero', args[0].position);
        }
      }

      for (let index = 1; index < values.length; index += 1) {
        if (isZeroNumber(values[index])) {
          throw new EvalError('division by zero', args[index].position);
        }
      }

      return numberValue(divideNumbers(values, callPosition), callPosition);
    }),
  );

  env.define(
    '<',
    builtin('<', (args, callPosition) =>
      booleanValue(compareNumberArgs('<', args, (comparison) => comparison < 0, callPosition)),
    ),
  );
  env.define(
    '>',
    builtin('>', (args, callPosition) =>
      booleanValue(compareNumberArgs('>', args, (comparison) => comparison > 0, callPosition)),
    ),
  );
  env.define(
    '=',
    builtin('=', (args, callPosition) =>
      booleanValue(compareNumberArgs('=', args, (comparison) => comparison === 0, callPosition)),
    ),
  );
  env.define(
    '<=',
    builtin('<=', (args, callPosition) =>
      booleanValue(compareNumberArgs('<=', args, (comparison) => comparison <= 0, callPosition)),
    ),
  );
  env.define(
    '>=',
    builtin('>=', (args, callPosition) =>
      booleanValue(compareNumberArgs('>=', args, (comparison) => comparison >= 0, callPosition)),
    ),
  );

  env.define(
    'abs',
    builtin('abs', (args, callPosition) => {
      requireArgCount('abs', args.length, 1, callPosition);
      return numberValue(absNumber(expectNumber('abs', args[0]), callPosition), callPosition);
    }),
  );

  env.define(
    'gcd',
    builtin('gcd', (args, callPosition) => {
      if (args.length === 0) {
        return numberValue(exactInteger(0n), callPosition);
      }

      let result = 0n;
      for (const arg of args) {
        result = greatestCommonDivisorBigInt(result, absBigInt(expectIntegerBigInt('gcd', arg)));
      }

      return integerResultValue(result, hasAnyInexactArgs(args), callPosition);
    }),
  );

  env.define(
    'lcm',
    builtin('lcm', (args, callPosition) => {
      if (args.length === 0) {
        return numberValue(exactInteger(1n), callPosition);
      }

      let result = 1n;
      for (const arg of args) {
        const value = absBigInt(expectIntegerBigInt('lcm', arg));
        result = leastCommonMultipleBigInt(result, value);
      }

      return integerResultValue(result, hasAnyInexactArgs(args), callPosition);
    }),
  );

  env.define(
    'quotient',
    builtin('quotient', (args, callPosition) => {
      requireArgCount('quotient', args.length, 2, callPosition);
      const dividend = expectInteger('quotient', args[0]);
      const divisor = expectInteger('quotient', args[1]);
      if (divisor === 0) {
        throw new EvalError('division by zero', args[1].position);
      }

      return numberValue(Math.trunc(dividend / divisor), callPosition);
    }),
  );

  env.define(
    'remainder',
    builtin('remainder', (args, callPosition) => {
      requireArgCount('remainder', args.length, 2, callPosition);
      const dividend = expectInteger('remainder', args[0]);
      const divisor = expectInteger('remainder', args[1]);
      if (divisor === 0) {
        throw new EvalError('division by zero', args[1].position);
      }

      return numberValue(dividend % divisor, callPosition);
    }),
  );

  env.define(
    'modulo',
    builtin('modulo', (args, callPosition) => {
      requireArgCount('modulo', args.length, 2, callPosition);
      const dividend = expectInteger('modulo', args[0]);
      const divisor = expectInteger('modulo', args[1]);
      if (divisor === 0) {
        throw new EvalError('division by zero', args[1].position);
      }

      let result = dividend % divisor;
      if (result !== 0 && Math.sign(result) !== Math.sign(divisor)) {
        result += divisor;
      }

      return numberValue(result, callPosition);
    }),
  );

  env.define(
    'min',
    builtin('min', (args, callPosition) => {
      const values = evaluateNumberArgs('min', args);
      requireArgCountAtLeast('min', values.length, 1, callPosition);
      return numberValue(minNumber(values), callPosition);
    }),
  );

  env.define(
    'max',
    builtin('max', (args, callPosition) => {
      const values = evaluateNumberArgs('max', args);
      requireArgCountAtLeast('max', values.length, 1, callPosition);
      return numberValue(maxNumber(values), callPosition);
    }),
  );

  env.define(
    'expt',
    builtin('expt', (args, callPosition) => {
      requireArgCount('expt', args.length, 2, callPosition);
      const base = expectNumber('expt', args[0]);
      const exponent = expectInteger('expt', args[1]);
      return numberValue(exptNumber(base, exponent, args[1].position), callPosition);
    }),
  );

  env.define(
    'truncate',
    builtin('truncate', (args, callPosition) => {
      requireArgCount('truncate', args.length, 1, callPosition);
      return numberValue(truncateSchemeNumber(expectNumber('truncate', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'round',
    builtin('round', (args, callPosition) => {
      requireArgCount('round', args.length, 1, callPosition);
      return numberValue(roundSchemeNumber(expectNumber('round', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'zero?',
    builtin('zero?', (args, callPosition) => {
      requireArgCount('zero?', args.length, 1, callPosition);
      return booleanValue(isZeroNumber(expectNumber('zero?', args[0])));
    }),
  );

  env.define(
    'positive?',
    builtin('positive?', (args, callPosition) => {
      requireArgCount('positive?', args.length, 1, callPosition);
      return booleanValue(isPositiveNumber(expectNumber('positive?', args[0])));
    }),
  );

  env.define(
    'negative?',
    builtin('negative?', (args, callPosition) => {
      requireArgCount('negative?', args.length, 1, callPosition);
      return booleanValue(isNegativeNumber(expectNumber('negative?', args[0])));
    }),
  );

  env.define(
    'odd?',
    builtin('odd?', (args, callPosition) => {
      requireArgCount('odd?', args.length, 1, callPosition);
      return booleanValue(Math.abs(expectInteger('odd?', args[0]) % 2) === 1);
    }),
  );

  env.define(
    'even?',
    builtin('even?', (args, callPosition) => {
      requireArgCount('even?', args.length, 1, callPosition);
      return booleanValue(expectInteger('even?', args[0]) % 2 === 0);
    }),
  );

  env.define(
    'not',
    builtin('not', (args, callPosition) => {
      requireArgCount('not', args.length, 1, callPosition);
      return booleanValue(!isTruthy(args[0].value));
    }),
  );

  env.define(
    'procedure?',
    builtin('procedure?', (args, callPosition) => {
      requireArgCount('procedure?', args.length, 1, callPosition);
      return booleanValue(isProcedureValue(args[0].value));
    }),
  );

  env.define(
    'eq?',
    builtin('eq?', (args, callPosition) => {
      requireArgCount('eq?', args.length, 2, callPosition);
      return booleanValue(eqvValues(args[0].value, args[1].value));
    }),
  );

  env.define(
    'eqv?',
    builtin('eqv?', (args, callPosition) => {
      requireArgCount('eqv?', args.length, 2, callPosition);
      return booleanValue(eqvValues(args[0].value, args[1].value));
    }),
  );

  env.define(
    'equal?',
    builtin('equal?', (args, callPosition) => {
      requireArgCount('equal?', args.length, 2, callPosition);
      return booleanValue(equalValues(args[0].value, args[1].value));
    }),
  );

  env.define(
    'cons',
    builtin('cons', (args, callPosition) => {
      requireArgCount('cons', args.length, 2, callPosition);
      return consValue(args[0].value, args[1].value);
    }),
  );

  env.define(
    'car',
    builtin('car', (args, callPosition) => {
      requireArgCount('car', args.length, 1, callPosition);
      return expectPair('car', args[0]).pair.car;
    }),
  );

  env.define(
    'cdr',
    builtin('cdr', (args, callPosition) => {
      requireArgCount('cdr', args.length, 1, callPosition);
      return cdrValue(expectPair('cdr', args[0]));
    }),
  );

  env.define(
    'cddr',
    builtin('cddr', (args, callPosition) => {
      requireArgCount('cddr', args.length, 1, callPosition);
      const first = cdrValue(expectPair('cddr', args[0]));
      return cdrValue(expectPair('cddr', { value: first, position: args[0].position }));
    }),
  );

  env.define(
    'set-car!',
    builtin('set-car!', (args, callPosition) => {
      requireArgCount('set-car!', args.length, 2, callPosition);
      expectPair('set-car!', args[0]).pair.car = args[1].value;
      return VOID_VALUE;
    }),
  );

  env.define(
    'set-cdr!',
    builtin('set-cdr!', (args, callPosition) => {
      requireArgCount('set-cdr!', args.length, 2, callPosition);
      expectPair('set-cdr!', args[0]).pair.cdr = args[1].value;
      return VOID_VALUE;
    }),
  );

  env.define('list', builtin('list', (args) => listValue(args.map((arg) => arg.value))));

  env.define('vector', builtin('vector', (args) => vectorValue(args.map((arg) => arg.value))));

  env.define(
    'make-vector',
    builtin('make-vector', (args, callPosition) => {
      if (args.length !== 1 && args.length !== 2) {
        throw new EvalError(`make-vector: expected 1 or 2 argument(s), got ${args.length}`, callPosition);
      }

      const length = expectNonNegativeInteger('make-vector', args[0]);
      const fill = args[1]?.value ?? VOID_VALUE;
      return vectorValue(Array(length).fill(fill));
    }),
  );

  env.define(
    'vector?',
    builtin('vector?', (args, callPosition) => {
      requireArgCount('vector?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'vector');
    }),
  );

  env.define(
    'vector-length',
    builtin('vector-length', (args, callPosition) => {
      requireArgCount('vector-length', args.length, 1, callPosition);
      return numberValue(expectVector('vector-length', args[0]).elements.length, callPosition);
    }),
  );

  env.define(
    'vector-ref',
    builtin('vector-ref', (args, callPosition) => {
      requireArgCount('vector-ref', args.length, 2, callPosition);
      const vector = expectVector('vector-ref', args[0]);
      const index = expectNonNegativeInteger('vector-ref', args[1]);
      if (index >= vector.elements.length) {
        throw new EvalError('vector-ref: index out of range', args[1].position);
      }

      return vector.elements[index];
    }),
  );

  env.define(
    'vector-set!',
    builtin('vector-set!', (args, callPosition) => {
      requireArgCount('vector-set!', args.length, 3, callPosition);
      const vector = expectVector('vector-set!', args[0]);
      const index = expectNonNegativeInteger('vector-set!', args[1]);
      if (index >= vector.elements.length) {
        throw new EvalError('vector-set!: index out of range', args[1].position);
      }

      vector.elements[index] = args[2].value;
      return VOID_VALUE;
    }),
  );

  env.define(
    'vector->list',
    builtin('vector->list', (args, callPosition) => {
      requireArgCount('vector->list', args.length, 1, callPosition);
      return listValue([...expectVector('vector->list', args[0]).elements]);
    }),
  );

  env.define(
    'list->vector',
    builtin('list->vector', (args, callPosition) => {
      requireArgCount('list->vector', args.length, 1, callPosition);
      return vectorValue(listElements(expectList('list->vector', args[0])));
    }),
  );

  env.define(
    'length',
    builtin('length', (args, callPosition) => {
      requireArgCount('length', args.length, 1, callPosition);
      return numberValue(listLength(expectList('length', args[0])), callPosition);
    }),
  );

  env.define(
    'reverse',
    builtin('reverse', (args, callPosition) => {
      requireArgCount('reverse', args.length, 1, callPosition);

      let result: ListValue = EMPTY_LIST;
      let cursor: ListValue = expectList('reverse', args[0]);
      while (isPairListValue(cursor)) {
        result = consValue(cursor.pair.car, result);
        cursor = cdrValue(cursor) as ListValue;
      }

      return result;
    }),
  );

  env.define(
    'append',
    builtin('append', (args) => {
      if (args.length === 0) {
        return EMPTY_LIST;
      }

      const prefixElements: SchemeValue[] = [];
      for (const arg of args.slice(0, -1)) {
        prefixElements.push(...listElements(expectList('append', arg)));
      }

      const tail = args[args.length - 1].value;
      if (prefixElements.length === 0) {
        return tail;
      }

      return listValue(prefixElements, tail);
    }),
  );

  env.define(
    'apply',
    builtin('apply', (args, callPosition) => {
      requireArgCountAtLeast('apply', args.length, 2, callPosition);

      const procedure = args[0].value;
      const finalListArg = args[args.length - 1];
      const trailingArgs = listElements(expectList('apply', finalListArg)).map((value) => ({
        value,
        position: finalListArg.position,
      }));

      return applyProcedure(
        procedure,
        [...args.slice(1, -1), ...trailingArgs],
        callPosition,
      );
    }, (args, callPosition, continuation) => {
      requireArgCountAtLeast('apply', args.length, 2, callPosition);

      const procedure = args[0].value;
      const finalListArg = args[args.length - 1];
      const trailingArgs = listElements(expectList('apply', finalListArg)).map((value) => ({
        value,
        position: finalListArg.position,
      }));

      return applyProcedureCps(
        procedure,
        [...args.slice(1, -1), ...trailingArgs],
        callPosition,
        continuation,
      );
    }),
  );

  env.define('call/cc', callWithCurrentContinuationBuiltin('call/cc'));
  env.define(
    'call-with-current-continuation',
    callWithCurrentContinuationBuiltin('call-with-current-continuation'),
  );
  env.define('values', valuesBuiltin);
  env.define('call-with-values', callWithValuesBuiltin);
  env.define('raise', raiseBuiltin);
  env.define('error', errorBuiltin);
  env.define('with-exception-handler', withExceptionHandlerBuiltin);
  env.define('dynamic-wind', dynamicWindBuiltin);
  defineCoreSyntax(env);

  env.define(
    'map',
    builtin('map', (args, callPosition) => {
      requireArgCountAtLeast('map', args.length, 2, callPosition);

      const procedure = args[0].value;
      const results: SchemeValue[] = [];
      const cursors = args.slice(1).map((arg) => expectList('map', arg));

      while (true) {
        const pairs = cursors.map((cursor) => (isPairListValue(cursor) ? cursor : undefined));
        if (pairs.some((pair) => pair === undefined)) {
          break;
        }

        const mappedArgs = pairs.map((pair, listIndex) => ({
          value: pair!.pair.car,
          position: args[listIndex + 1].position,
        }));
        results.push(applyProcedure(procedure, mappedArgs, callPosition));

        for (let index = 0; index < cursors.length; index += 1) {
          cursors[index] = cdrValue(pairs[index]!) as ListValue;
        }
      }

      return listValue(results);
    }, (args, callPosition, continuation) => {
      requireArgCountAtLeast('map', args.length, 2, callPosition);

      const procedure = args[0].value;
      const cursors = args.slice(1).map((arg) => expectList('map', arg));

      return mapBuiltinCps(procedure, cursors, args.slice(1), callPosition, continuation, []);
    }),
  );

  env.define(
    'for-each',
    builtin('for-each', (args, callPosition) => {
      requireArgCountAtLeast('for-each', args.length, 2, callPosition);

      const procedure = args[0].value;
      const cursors = args.slice(1).map((arg) => expectList('for-each', arg));

      while (true) {
        const pairs = cursors.map((cursor) => (isPairListValue(cursor) ? cursor : undefined));
        if (pairs.some((pair) => pair === undefined)) {
          break;
        }

        const appliedArgs = pairs.map((pair, listIndex) => ({
          value: pair!.pair.car,
          position: args[listIndex + 1].position,
        }));
        applyProcedure(procedure, appliedArgs, callPosition);

        for (let index = 0; index < cursors.length; index += 1) {
          cursors[index] = cdrValue(pairs[index]!) as ListValue;
        }
      }

      return VOID_VALUE;
    }, (args, callPosition, continuation) => {
      requireArgCountAtLeast('for-each', args.length, 2, callPosition);

      const procedure = args[0].value;
      const cursors = args.slice(1).map((arg) => expectList('for-each', arg));

      return forEachBuiltinCps(procedure, cursors, args.slice(1), callPosition, continuation);
    }),
  );

  env.define(
    'display',
    builtin('display', (args, callPosition) => {
      requireArgCount('display', args.length, 1, callPosition);
      context.output.push(formatDisplayValue(args[0].value));
      return VOID_VALUE;
    }),
  );

  env.define(
    'write',
    builtin('write', (args, callPosition) => {
      requireArgCount('write', args.length, 1, callPosition);
      context.output.push(formatValue(args[0].value));
      return VOID_VALUE;
    }),
  );

  env.define(
    'newline',
    builtin('newline', (args, callPosition) => {
      requireArgCount('newline', args.length, 0, callPosition);
      context.output.push('\n');
      return VOID_VALUE;
    }),
  );

  env.define(
    'string-append',
    builtin('string-append', (args) =>
      stringValue(args.map((arg) => expectString('string-append', arg)).join('')),
    ),
  );

  env.define(
    'make-string',
    builtin('make-string', (args, callPosition) => {
      if (args.length !== 1 && args.length !== 2) {
        throw new EvalError(`make-string: expected 1 or 2 argument(s), got ${args.length}`, callPosition);
      }

      const length = expectNonNegativeInteger('make-string', args[0]);
      const fill = args[1] === undefined ? '\0' : expectChar('make-string', args[1]).value;
      return stringValue(fill.repeat(length));
    }),
  );

  env.define(
    'string',
    builtin('string', (args) =>
      stringValue(args.map((arg) => expectChar('string', arg).value).join('')),
    ),
  );

  env.define(
    'string-length',
    builtin('string-length', (args, callPosition) => {
      requireArgCount('string-length', args.length, 1, callPosition);
      return numberValue(codePoints(expectString('string-length', args[0])).length, callPosition);
    }),
  );

  env.define(
    'substring',
    builtin('substring', (args, callPosition) => {
      requireArgCount('substring', args.length, 3, callPosition);
      const value = expectString('substring', args[0]);
      const start = expectNonNegativeInteger('substring', args[1]);
      const end = expectNonNegativeInteger('substring', args[2]);
      const characters = codePoints(value);

      if (start > end) {
        throw new EvalError('substring: start index exceeds end index', args[1].position);
      }

      if (end > characters.length) {
        throw new EvalError('substring: index out of range', args[2].position);
      }

      return stringValue(characters.slice(start, end).join(''));
    }),
  );

  env.define(
    'string->number',
    builtin('string->number', (args, callPosition) => {
      requireArgCount('string->number', args.length, 1, callPosition);
      const value = expectString('string->number', args[0]);
      const parsed = parseNumberLiteral(value, args[0].position);
      if (parsed === null) {
        return booleanValue(false);
      }

      return numberValue(parsed, args[0].position);
    }),
  );

  env.define(
    'number->string',
    builtin('number->string', (args, callPosition) => {
      requireArgCount('number->string', args.length, 1, callPosition);
      return stringValue(formatNumber(expectNumber('number->string', args[0])));
    }),
  );

  env.define(
    'symbol->string',
    builtin('symbol->string', (args, callPosition) => {
      requireArgCount('symbol->string', args.length, 1, callPosition);
      return stringValue(expectSymbol('symbol->string', args[0]).name);
    }),
  );

  env.define(
    'string->symbol',
    builtin('string->symbol', (args, callPosition) => {
      requireArgCount('string->symbol', args.length, 1, callPosition);
      return { type: 'symbol', name: expectString('string->symbol', args[0]) };
    }),
  );

  env.define(
    'syntax->datum',
    builtin('syntax->datum', (args, callPosition) => {
      requireArgCount('syntax->datum', args.length, 1, callPosition);
      return quoteExpr(expectSyntax('syntax->datum', args[0]).expr);
    }),
  );

  env.define(
    'datum->syntax',
    builtin('datum->syntax', (args, callPosition) => {
      requireArgCount('datum->syntax', args.length, 2, callPosition);
      expectSyntax('datum->syntax', args[0]);
      return syntaxValue(datumToExpr(args[1].value, args[1].position));
    }),
  );

  env.define(
    'string-ref',
    builtin('string-ref', (args, callPosition) => {
      requireArgCount('string-ref', args.length, 2, callPosition);
      const value = expectString('string-ref', args[0]);
      const index = expectNonNegativeInteger('string-ref', args[1]);
      const characters = codePoints(value);

      if (index >= characters.length) {
        throw new EvalError('string-ref: index out of range', args[1].position);
      }

      return charValue(characters[index], args[1].position);
    }),
  );

  env.define(
    'string-copy',
    builtin('string-copy', (args, callPosition) => {
      requireArgCount('string-copy', args.length, 1, callPosition);
      return stringValue(expectString('string-copy', args[0]));
    }),
  );

  env.define(
    'string->list',
    builtin('string->list', (args, callPosition) => {
      requireArgCount('string->list', args.length, 1, callPosition);
      return listValue(
        codePoints(expectString('string->list', args[0])).map((character) => charValue(character)),
      );
    }),
  );

  env.define(
    'list->string',
    builtin('list->string', (args, callPosition) => {
      requireArgCount('list->string', args.length, 1, callPosition);
      const characters = listElements(expectList('list->string', args[0])).map((element) => {
        if (element.type !== 'char') {
          throw new EvalError('list->string: expected list of characters', args[0].position);
        }

        return element.value;
      });

      return stringValue(characters.join(''));
    }),
  );

  env.define(
    'string-set!',
    builtin('string-set!', (args, callPosition) => {
      requireArgCount('string-set!', args.length, 3, callPosition);
      const target = expectStringValue('string-set!', args[0]);
      const index = expectNonNegativeInteger('string-set!', args[1]);
      const character = expectChar('string-set!', args[2]).value;
      const characters = codePoints(target.value);

      if (index >= characters.length) {
        throw new EvalError('string-set!: index out of range', args[1].position);
      }

      if (!target.mutable) {
        throw new EvalError('string-set!: immutable strings', callPosition);
      }

      characters[index] = character;
      target.value = characters.join('');
      return VOID_VALUE;
    }),
  );

  env.define(
    'string=?',
    builtin('string=?', (args, callPosition) =>
      booleanValue(compareStringArgs('string=?', args, (a, b) => a === b, callPosition)),
    ),
  );

  env.define(
    'string<?',
    builtin('string<?', (args, callPosition) =>
      booleanValue(compareStringArgs('string<?', args, (a, b) => a < b, callPosition)),
    ),
  );

  env.define(
    'string>?',
    builtin('string>?', (args, callPosition) =>
      booleanValue(compareStringArgs('string>?', args, (a, b) => a > b, callPosition)),
    ),
  );

  env.define(
    'string<=?',
    builtin('string<=?', (args, callPosition) =>
      booleanValue(compareStringArgs('string<=?', args, (a, b) => a <= b, callPosition)),
    ),
  );

  env.define(
    'string>=?',
    builtin('string>=?', (args, callPosition) =>
      booleanValue(compareStringArgs('string>=?', args, (a, b) => a >= b, callPosition)),
    ),
  );

  env.define(
    'string-ci=?',
    builtin('string-ci=?', (args, callPosition) =>
      booleanValue(
        compareStringArgs(
          'string-ci=?',
          args,
          (a, b) => a.toLowerCase() === b.toLowerCase(),
          callPosition,
        ),
      ),
    ),
  );

  env.define(
    'string-upcase',
    builtin('string-upcase', (args, callPosition) => {
      requireArgCount('string-upcase', args.length, 1, callPosition);
      return stringValue(expectString('string-upcase', args[0]).toUpperCase());
    }),
  );

  env.define(
    'string-downcase',
    builtin('string-downcase', (args, callPosition) => {
      requireArgCount('string-downcase', args.length, 1, callPosition);
      return stringValue(expectString('string-downcase', args[0]).toLowerCase());
    }),
  );

  env.define(
    'null?',
    builtin('null?', (args, callPosition) => {
      requireArgCount('null?', args.length, 1, callPosition);
      return booleanValue(isNullListValue(args[0].value));
    }),
  );

  env.define(
    'pair?',
    builtin('pair?', (args, callPosition) => {
      requireArgCount('pair?', args.length, 1, callPosition);
      return booleanValue(isPairListValue(args[0].value));
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
    'integer?',
    builtin('integer?', (args, callPosition) => {
      requireArgCount('integer?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isIntegerNumber(args[0].value.value));
    }),
  );

  env.define(
    'rational?',
    builtin('rational?', (args, callPosition) => {
      requireArgCount('rational?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isRationalNumber(args[0].value.value));
    }),
  );

  env.define(
    'exact?',
    builtin('exact?', (args, callPosition) => {
      requireArgCount('exact?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isExactNumber(args[0].value.value));
    }),
  );

  env.define(
    'inexact?',
    builtin('inexact?', (args, callPosition) => {
      requireArgCount('inexact?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'number' && isInexactNumber(args[0].value.value));
    }),
  );

  env.define(
    'exact->inexact',
    builtin('exact->inexact', (args, callPosition) => {
      requireArgCount('exact->inexact', args.length, 1, callPosition);
      return numberValue(exactToInexact(expectNumber('exact->inexact', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'inexact->exact',
    builtin('inexact->exact', (args, callPosition) => {
      requireArgCount('inexact->exact', args.length, 1, callPosition);
      return numberValue(inexactToExact(expectNumber('inexact->exact', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'numerator',
    builtin('numerator', (args, callPosition) => {
      requireArgCount('numerator', args.length, 1, callPosition);
      return numberValue(numeratorPart(expectNumber('numerator', args[0]), args[0].position), callPosition);
    }),
  );

  env.define(
    'denominator',
    builtin('denominator', (args, callPosition) => {
      requireArgCount('denominator', args.length, 1, callPosition);
      return numberValue(denominatorPart(expectNumber('denominator', args[0]), args[0].position), callPosition);
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

  env.define(
    'identifier?',
    builtin('identifier?', (args, callPosition) => {
      requireArgCount('identifier?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'syntax' && args[0].value.expr.type === 'symbol');
    }),
  );

  env.define(
    'char?',
    builtin('char?', (args, callPosition) => {
      requireArgCount('char?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'char');
    }),
  );

  env.define(
    'list?',
    builtin('list?', (args, callPosition) => {
      requireArgCount('list?', args.length, 1, callPosition);
      return booleanValue(args[0].value.type === 'list' && isProperList(args[0].value));
    }),
  );

  env.define(
    'list-ref',
    builtin('list-ref', (args, callPosition) => {
      requireArgCount('list-ref', args.length, 2, callPosition);
      const list = expectList('list-ref', args[0]);
      const index = expectNonNegativeInteger('list-ref', args[1]);
      if (index >= listLength(list)) {
        throw new EvalError('list-ref: index out of range', args[1].position);
      }

      return listRefValue(list, index);
    }),
  );

  env.define(
    'list-tail',
    builtin('list-tail', (args, callPosition) => {
      requireArgCount('list-tail', args.length, 2, callPosition);
      const list = expectList('list-tail', args[0]);
      const index = expectNonNegativeInteger('list-tail', args[1]);
      if (index > listLength(list)) {
        throw new EvalError('list-tail: index out of range', args[1].position);
      }

      return listTailValue(list, index);
    }),
  );

  env.define(
    'assq',
    builtin('assq', (args, callPosition) => {
      requireArgCount('assq', args.length, 2, callPosition);
      const key = args[0].value;
      let cursor: ListValue = expectList('assq', args[1]);

      while (isPairListValue(cursor)) {
        const entry = cursor.pair.car;
        if (isPairListValue(entry) && eqvValues(entry.pair.car, key)) {
          return entry;
        }

        cursor = cdrValue(cursor) as ListValue;
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'assoc',
    builtin('assoc', (args, callPosition) => {
      requireArgCount('assoc', args.length, 2, callPosition);
      const key = args[0].value;
      const alist = expectList('assoc', args[1]);

      let cursor: ListValue = alist;
      while (isPairListValue(cursor)) {
        const entry = cursor.pair.car;
        if (isPairListValue(entry) && equalValues(entry.pair.car, key)) {
          return entry;
        }

        cursor = cdrValue(cursor) as ListValue;
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'assv',
    builtin('assv', (args, callPosition) => {
      requireArgCount('assv', args.length, 2, callPosition);
      const key = args[0].value;
      let cursor: ListValue = expectList('assv', args[1]);

      while (isPairListValue(cursor)) {
        const entry = cursor.pair.car;
        if (isPairListValue(entry) && eqvValues(entry.pair.car, key)) {
          return entry;
        }

        cursor = cdrValue(cursor) as ListValue;
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'memq',
    builtin('memq', (args, callPosition) => {
      requireArgCount('memq', args.length, 2, callPosition);
      const target = args[0].value;
      let cursor: ListValue = expectList('memq', args[1]);

      while (isPairListValue(cursor)) {
        if (eqvValues(cursor.pair.car, target)) {
          return cursor;
        }

        cursor = cdrValue(cursor) as ListValue;
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'memv',
    builtin('memv', (args, callPosition) => {
      requireArgCount('memv', args.length, 2, callPosition);
      const target = args[0].value;
      let cursor: ListValue = expectList('memv', args[1]);

      while (isPairListValue(cursor)) {
        if (eqvValues(cursor.pair.car, target)) {
          return cursor;
        }

        cursor = cdrValue(cursor) as ListValue;
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'member',
    builtin('member', (args, callPosition) => {
      requireArgCount('member', args.length, 2, callPosition);
      const target = args[0].value;
      let cursor: ListValue = expectList('member', args[1]);

      while (isPairListValue(cursor)) {
        if (equalValues(cursor.pair.car, target)) {
          return cursor;
        }

        cursor = cdrValue(cursor) as ListValue;
      }

      return booleanValue(false);
    }),
  );

  env.define(
    'char-alphabetic?',
    builtin('char-alphabetic?', (args, callPosition) => {
      requireArgCount('char-alphabetic?', args.length, 1, callPosition);
      return booleanValue(/^[A-Za-z]$/u.test(expectChar('char-alphabetic?', args[0]).value));
    }),
  );

  env.define(
    'char-numeric?',
    builtin('char-numeric?', (args, callPosition) => {
      requireArgCount('char-numeric?', args.length, 1, callPosition);
      return booleanValue(/^[0-9]$/u.test(expectChar('char-numeric?', args[0]).value));
    }),
  );

  env.define(
    'char-upcase',
    builtin('char-upcase', (args, callPosition) => {
      requireArgCount('char-upcase', args.length, 1, callPosition);
      return charValue(expectChar('char-upcase', args[0]).value.toUpperCase(), args[0].position);
    }),
  );

  env.define(
    'char-downcase',
    builtin('char-downcase', (args, callPosition) => {
      requireArgCount('char-downcase', args.length, 1, callPosition);
      return charValue(expectChar('char-downcase', args[0]).value.toLowerCase(), args[0].position);
    }),
  );

  env.define(
    'char=?',
    builtin('char=?', (args, callPosition) =>
      booleanValue(compareCharArgs('char=?', args, (a, b) => a === b, callPosition)),
    ),
  );

  env.define(
    'char<?',
    builtin('char<?', (args, callPosition) =>
      booleanValue(
        compareCharArgs(
          'char<?',
          args,
          (a, b) => a.codePointAt(0)! < b.codePointAt(0)!,
          callPosition,
        ),
      ),
    ),
  );

  env.define(
    'char->integer',
    builtin('char->integer', (args, callPosition) => {
      requireArgCount('char->integer', args.length, 1, callPosition);
      return numberValue(expectChar('char->integer', args[0]).value.codePointAt(0)!, callPosition);
    }),
  );

  env.define(
    'integer->char',
    builtin('integer->char', (args, callPosition) => {
      requireArgCount('integer->char', args.length, 1, callPosition);
      const codePoint = expectNonNegativeInteger('integer->char', args[0]);
      if (!isValidUnicodeScalar(codePoint)) {
        throw new EvalError('integer->char: invalid code point', args[0].position);
      }

      return charValue(String.fromCodePoint(codePoint), callPosition);
    }),
  );

  return env;
}

function builtin(
  name: string,
  invoke: (args: EvaluatedArg[], callPosition: SourcePosition) => SchemeValue,
  invokeCps?: (
    args: EvaluatedArg[],
    callPosition: SourcePosition,
    continuation: EvalContinuation,
  ) => EvalStep,
): BuiltinProcedure {
  return { type: 'builtin', name, invoke, invokeCps };
}

function evaluateNumberArgs(name: string, args: EvaluatedArg[]): SchemeNumber[] {
  return args.map((arg) => expectNumber(name, arg));
}

function compareNumberArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (comparison: number) => boolean,
  position: SourcePosition,
): boolean {
  const values = evaluateNumberArgs(name, args);
  requireArgCountAtLeast(name, values.length, 1, position);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(compareNumbers(values[index], values[index + 1]))) {
      return false;
    }
  }

  return true;
}

function compareStringArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (left: string, right: string) => boolean,
  position: SourcePosition,
): boolean {
  const values = args.map((arg) => expectString(name, arg));
  requireArgCountAtLeast(name, values.length, 1, position);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function compareCharArgs(
  name: string,
  args: EvaluatedArg[],
  predicate: (left: string, right: string) => boolean,
  position: SourcePosition,
): boolean {
  const values = args.map((arg) => expectChar(name, arg).value);
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

function readBindingCell(
  cell: BindingCell,
  name: string,
  position: SourcePosition,
): SchemeValue {
  if (!cell.initialized) {
    throw new EvalError(`uninitialized variable: ${name}`, position);
  }

  return cell.value;
}

function writeBindingCell(cell: BindingCell, value: SchemeValue): void {
  cell.value = value;
  cell.initialized = true;
}

function expectNumber(name: string, arg: EvaluatedArg): SchemeNumber {
  if (arg.value.type !== 'number') {
    throw new EvalError(`${name}: expected number`, arg.position);
  }

  return arg.value.value;
}

function expectInteger(name: string, arg: EvaluatedArg): number {
  const value = expectNumber(name, arg);
  if (!isIntegerNumber(value)) {
    throw new EvalError(`${name}: expected integer`, arg.position);
  }

  return integerToJs(value, arg.position);
}

function expectIntegerBigInt(name: string, arg: EvaluatedArg): bigint {
  const value = expectNumber(name, arg);
  if (!isIntegerNumber(value)) {
    throw new EvalError(`${name}: expected integer`, arg.position);
  }

  return value.kind === 'exact' ? value.numerator : BigInt(integerToJs(value, arg.position));
}

function expectString(name: string, arg: EvaluatedArg): string {
  return expectStringValue(name, arg).value;
}

function expectStringValue(name: string, arg: EvaluatedArg): StringValue {
  if (arg.value.type !== 'string') {
    throw new EvalError(`${name}: expected string`, arg.position);
  }

  return arg.value;
}

function expectSymbol(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'symbol' }> {
  if (arg.value.type !== 'symbol') {
    throw new EvalError(`${name}: expected symbol`, arg.position);
  }

  return arg.value;
}

function expectSyntax(name: string, arg: EvaluatedArg): SyntaxValue {
  if (arg.value.type !== 'syntax') {
    throw new EvalError(`${name}: expected syntax object`, arg.position);
  }

  return arg.value;
}

function expectNonNegativeInteger(name: string, arg: EvaluatedArg): number {
  const value = expectInteger(name, arg);
  if (value < 0) {
    throw new EvalError(`${name}: expected non-negative integer`, arg.position);
  }

  return value;
}

function hasAnyInexactArgs(args: EvaluatedArg[]): boolean {
  return args.some((arg) => arg.value.type === 'number' && arg.value.value.kind === 'inexact');
}

function integerResultValue(
  value: bigint,
  inexact: boolean,
  position?: SourcePosition,
): SchemeValue {
  return inexact ? numberValue(inexactNumber(Number(value), position), position) : numberValue(exactInteger(value), position);
}

function expectList(name: string, arg: EvaluatedArg): ListValue {
  const list = expectListValue(name, arg);
  if (!isProperList(list)) {
    throw new EvalError(`${name}: expected list`, arg.position);
  }

  return list;
}

function expectListValue(name: string, arg: EvaluatedArg): ListValue {
  if (arg.value.type !== 'list') {
    throw new EvalError(`${name}: expected list`, arg.position);
  }

  return arg.value;
}

function expectVector(name: string, arg: EvaluatedArg): VectorValue {
  if (arg.value.type !== 'vector') {
    throw new EvalError(`${name}: expected vector`, arg.position);
  }

  return arg.value;
}

function expectPair(name: string, arg: EvaluatedArg): ListValue & { pair: PairCell } {
  const list = expectListValue(name, arg);
  if (!isPairListValue(list)) {
    throw new EvalError(`${name}: expected pair`, arg.position);
  }

  return list;
}

function expectChar(name: string, arg: EvaluatedArg): Extract<SchemeValue, { type: 'char' }> {
  if (arg.value.type !== 'char') {
    throw new EvalError(`${name}: expected character`, arg.position);
  }

  return arg.value;
}

function expectRecord(
  name: string,
  arg: EvaluatedArg,
  recordType: RecordTypeDescriptor,
): Extract<SchemeValue, { type: 'record' }> {
  if (arg.value.type !== 'record' || arg.value.recordType !== recordType) {
    throw new EvalError(`${name}: expected ${recordType.name}`, arg.position);
  }

  return arg.value;
}

function pairValue(car: SchemeValue, cdr: SchemeValue): ListValue {
  return { type: 'list', pair: { car, cdr } };
}

function listValue(elements: SchemeValue[], tail: SchemeValue = EMPTY_LIST): ListValue {
  let result: SchemeValue = tail;

  for (let index = elements.length - 1; index >= 0; index -= 1) {
    result = pairValue(elements[index], result);
  }

  if (result.type !== 'list') {
    throw new Error('internal error: list tail is not a list');
  }

  return result;
}

function vectorValue(elements: SchemeValue[]): VectorValue {
  return { type: 'vector', elements };
}

function isNullListValue(value: SchemeValue): value is ListValue & { pair?: undefined } {
  return value.type === 'list' && value.pair === undefined;
}

function isPairListValue(value: SchemeValue): value is ListValue & { pair: PairCell } {
  return value.type === 'list' && value.pair !== undefined;
}

function stepList(
  value: SchemeValue,
): { kind: 'pair'; next: SchemeValue } | { kind: 'null' } | { kind: 'improper' } {
  if (value.type !== 'list') {
    return { kind: 'improper' };
  }

  if (value.pair === undefined) {
    return { kind: 'null' };
  }

  return { kind: 'pair', next: value.pair.cdr };
}

function isProperList(value: ListValue): boolean {
  let slow: SchemeValue = value;
  let fast: SchemeValue = value;

  while (true) {
    const fastStep1 = stepList(fast);
    if (fastStep1.kind === 'null') {
      return true;
    }
    if (fastStep1.kind === 'improper') {
      return false;
    }
    fast = fastStep1.next;

    const fastStep2 = stepList(fast);
    if (fastStep2.kind === 'null') {
      return true;
    }
    if (fastStep2.kind === 'improper') {
      return false;
    }
    fast = fastStep2.next;

    const slowStep = stepList(slow);
    if (slowStep.kind !== 'pair') {
      return slowStep.kind === 'null';
    }
    slow = slowStep.next;

    if (fast === slow) {
      return false;
    }
  }
}

function consValue(head: SchemeValue, tail: SchemeValue): ListValue {
  return pairValue(head, tail);
}

function cdrValue(list: ListValue & { pair: PairCell }): SchemeValue {
  return list.pair.cdr;
}

function listLength(list: ListValue): number {
  let length = 0;
  let cursor: SchemeValue = list;

  while (isPairListValue(cursor)) {
    length += 1;
    cursor = cursor.pair.cdr;
  }

  return length;
}

function listElements(list: ListValue): SchemeValue[] {
  const elements: SchemeValue[] = [];
  let cursor: SchemeValue = list;

  while (isPairListValue(cursor)) {
    elements.push(cursor.pair.car);
    cursor = cursor.pair.cdr;
  }

  return elements;
}

function listRefValue(list: ListValue, index: number): SchemeValue {
  let cursor: SchemeValue = list;
  let remaining = index;

  while (isPairListValue(cursor)) {
    if (remaining === 0) {
      return cursor.pair.car;
    }

    remaining -= 1;
    cursor = cursor.pair.cdr;
  }

  throw new Error('internal error: list-ref on exhausted list');
}

function listTailValue(list: ListValue, index: number): ListValue {
  let cursor: SchemeValue = list;

  for (let remaining = index; remaining > 0; remaining -= 1) {
    if (!isPairListValue(cursor)) {
      throw new Error('internal error: list-tail on exhausted list');
    }

    cursor = cursor.pair.cdr;
  }

  if (cursor.type !== 'list') {
    throw new Error('internal error: list-tail returned a non-list');
  }

  return cursor;
}

function eqvValues(left: SchemeValue, right: SchemeValue): boolean {
  if (left === right) {
    return true;
  }

  if (left.type !== right.type) {
    return false;
  }

  switch (left.type) {
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'symbol':
      return left.name === (right as typeof left).name;
    case 'syntax':
      return exprSyntaxEqual(left.expr, (right as typeof left).expr);
    case 'list':
      return isNullListValue(left) && isNullListValue(right as ListValue);
    case 'vector':
    case 'record':
    case 'builtin':
    case 'closure':
    case 'case-closure':
    case 'continuation':
    case 'multiple-values':
      return false;
    case 'void':
      return true;
  }
}

function equalValues(left: SchemeValue, right: SchemeValue): boolean {
  return equalValuesInternal(left, right, new Map());
}

function equalValuesInternal(
  left: SchemeValue,
  right: SchemeValue,
  memo: Map<object, Set<object>>,
): boolean {
  if (left === right) {
    return true;
  }

  if (left.type !== right.type) {
    return false;
  }

  switch (left.type) {
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'symbol':
      return left.name === (right as typeof left).name;
    case 'syntax':
      return exprSyntaxEqual(left.expr, (right as typeof left).expr);
    case 'list': {
      const rightList = right as ListValue;
      if (isNullListValue(left) || isNullListValue(rightList)) {
        return isNullListValue(left) && isNullListValue(rightList);
      }

      if (rememberComparison(memo, left, rightList)) {
        return true;
      }

      const leftPair = left as ListValue & { pair: PairCell };
      const rightPair = rightList as ListValue & { pair: PairCell };
      return (
        equalValuesInternal(leftPair.pair.car, rightPair.pair.car, memo) &&
        equalValuesInternal(leftPair.pair.cdr, rightPair.pair.cdr, memo)
      );
    }
    case 'vector': {
      const rightVector = right as VectorValue;
      if (left.elements.length !== rightVector.elements.length) {
        return false;
      }

      if (rememberComparison(memo, left, rightVector)) {
        return true;
      }

      for (let index = 0; index < left.elements.length; index += 1) {
        if (!equalValuesInternal(left.elements[index], rightVector.elements[index], memo)) {
          return false;
        }
      }

      return true;
    }
    case 'record':
      return false;
    case 'builtin':
    case 'closure':
    case 'case-closure':
    case 'continuation':
      return left === right;
    case 'multiple-values': {
      const rightValues = (right as MultipleValues).values;
      if (left.values.length !== rightValues.length) {
        return false;
      }

      for (let index = 0; index < left.values.length; index += 1) {
        if (!equalValuesInternal(left.values[index], rightValues[index], memo)) {
          return false;
        }
      }

      return true;
    }
    case 'void':
      return true;
  }
}

function rememberComparison(memo: Map<object, Set<object>>, left: object, right: object): boolean {
  const seenLeft = memo.get(left);
  if (seenLeft?.has(right) ?? false) {
    return true;
  }

  if (seenLeft === undefined) {
    memo.set(left, new Set([right]));
  } else {
    seenLeft.add(right);
  }

  const seenRight = memo.get(right);
  if (seenRight === undefined) {
    memo.set(right, new Set([left]));
  } else {
    seenRight.add(left);
  }

  return false;
}

function isTruthy(value: SchemeValue): boolean {
  return value.type !== 'boolean' || value.value;
}

function parseCharLiteral(value: string, position: SourcePosition): string | null {
  if (!value.startsWith('#\\')) {
    return null;
  }

  const literal = value.slice(2);
  switch (literal) {
    case 'space':
      return ' ';
    case 'newline':
      return '\n';
  }

  const characters = codePoints(literal);
  if (characters.length === 1) {
    return characters[0];
  }

  throw new EvalError('invalid character literal', position);
}

function codePoints(value: string): string[] {
  return Array.from(value);
}

function isValidUnicodeScalar(value: number): boolean {
  return (
    Number.isInteger(value) &&
    value >= 0 &&
    value <= 0x10ffff &&
    (value < 0xd800 || value > 0xdfff)
  );
}

function absBigInt(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function greatestCommonDivisorBigInt(left: bigint, right: bigint): bigint {
  let a = absBigInt(left);
  let b = absBigInt(right);

  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }

  return a;
}

function leastCommonMultipleBigInt(left: bigint, right: bigint): bigint {
  if (left === 0n || right === 0n) {
    return 0n;
  }

  return absBigInt((left / greatestCommonDivisorBigInt(left, right)) * right);
}

function truncateSchemeNumber(value: SchemeNumber, position?: SourcePosition): SchemeNumber {
  if (value.kind === 'exact') {
    return exactInteger(value.numerator / value.denominator);
  }

  return inexactNumber(Math.trunc(value.value), position);
}

function roundSchemeNumber(value: SchemeNumber, position?: SourcePosition): SchemeNumber {
  if (value.kind === 'exact') {
    const quotient = value.numerator / value.denominator;
    const remainder = absBigInt(value.numerator % value.denominator);
    const doubledRemainder = remainder * 2n;

    if (doubledRemainder < value.denominator) {
      return exactInteger(quotient);
    }

    if (doubledRemainder > value.denominator) {
      return exactInteger(quotient + (value.numerator < 0n ? -1n : 1n));
    }

    return exactInteger(quotient % 2n === 0n ? quotient : quotient + (value.numerator < 0n ? -1n : 1n));
  }

  const truncated = Math.trunc(value.value);
  const difference = Math.abs(value.value - truncated);
  if (difference < 0.5) {
    return inexactNumber(truncated, position);
  }

  if (difference > 0.5) {
    return inexactNumber(truncated + Math.sign(value.value), position);
  }

  const rounded = truncated % 2 === 0 ? truncated : truncated + Math.sign(value.value);
  return inexactNumber(rounded, position);
}

function numberValue(value: number | SchemeNumber, position?: SourcePosition): SchemeValue {
  if (typeof value === 'number') {
    return {
      type: 'number',
      value: Number.isInteger(value) ? exactInteger(value) : inexactNumber(value, position),
    };
  }

  return { type: 'number', value };
}

function booleanValue(value: boolean): SchemeValue {
  return { type: 'boolean', value };
}

function syntaxValue(expr: Expr): SyntaxValue {
  return { type: 'syntax', expr };
}

function stringValue(value: string, mutable = !stringsAreImmutable()): StringValue {
  return { type: 'string', value, mutable };
}

function charValue(value: string, position?: SourcePosition): SchemeValue {
  if (codePoints(value).length !== 1) {
    throw new EvalError('invalid character', position);
  }

  return { type: 'char', value };
}

function multiValueResult(values: SchemeValue[]): SchemeValue {
  return values.length === 1 ? values[0] : { type: 'multiple-values', values };
}

function multiValueArgs(value: SchemeValue, position: SourcePosition): EvaluatedArg[] {
  if (value.type === 'multiple-values') {
    return value.values.map((entry) => ({ value: entry, position }));
  }

  return [{ value, position }];
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

function buildErrorMessage(args: EvaluatedArg[]): string {
  return args
    .map(({ value }) => (value.type === 'string' ? value.value : formatValue(value)))
    .join(' ');
}

function formatDisplayValue(value: SchemeValue): string {
  return formatValueInternal(value, 'display', new Set());
}

function formatValue(value: SchemeValue): string {
  return formatValueInternal(value, 'write', new Set());
}

function formatValueInternal(
  value: SchemeValue,
  mode: 'display' | 'write',
  active: Set<object>,
): string {
  switch (value.type) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return mode === 'display' ? value.value : JSON.stringify(value.value);
    case 'char':
      return mode === 'display' ? value.value : formatChar(value.value);
    case 'symbol':
      return value.name;
    case 'syntax':
      return '#<syntax>';
    case 'list':
      return formatListValue(value, mode, active);
    case 'vector':
      return formatVectorValue(value, mode, active);
    case 'record':
      return `#<${value.recordType.displayName}>`;
    case 'builtin':
    case 'closure':
    case 'case-closure':
    case 'continuation':
      return '#<procedure>';
    case 'multiple-values':
      return '#<values>';
    case 'void':
      return '#<void>';
  }
}

function isProcedureValue(value: SchemeValue): boolean {
  return (
    value.type === 'builtin' ||
    value.type === 'closure' ||
    value.type === 'case-closure' ||
    value.type === 'continuation'
  );
}

function formatListValue(
  value: ListValue,
  mode: 'display' | 'write',
  active: Set<object>,
): string {
  if (isNullListValue(value)) {
    return '()';
  }

  if (active.has(value)) {
    return '#<circular>';
  }

  const entered: ListValue[] = [value];
  const parts: string[] = [];
  let cursor = value as ListValue & { pair: PairCell };
  active.add(value);

  try {
    while (true) {
      parts.push(formatValueInternal(cursor.pair.car, mode, active));

      const next = cursor.pair.cdr;
      if (isNullListValue(next)) {
        return `(${parts.join(' ')})`;
      }

      if (next.type !== 'list') {
        return `(${parts.join(' ')} . ${formatValueInternal(next, mode, active)})`;
      }

      if (active.has(next)) {
        return `(${parts.join(' ')} . #<circular>)`;
      }

      active.add(next);
      entered.push(next);
      cursor = next as ListValue & { pair: PairCell };
    }
  } finally {
    for (const list of entered) {
      active.delete(list);
    }
  }
}

function formatVectorValue(
  value: VectorValue,
  mode: 'display' | 'write',
  active: Set<object>,
): string {
  if (active.has(value)) {
    return '#<circular>';
  }

  active.add(value);
  try {
    return `#(${value.elements.map((element) => formatValueInternal(element, mode, active)).join(' ')})`;
  } finally {
    active.delete(value);
  }
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

function formatRecordTypeName(name: string): string {
  if (name.startsWith('<') && name.endsWith('>') && name.length > 2) {
    return name.slice(1, -1);
  }

  return name;
}
