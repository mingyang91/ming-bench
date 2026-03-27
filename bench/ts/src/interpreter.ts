import { EvalError, attachPosition, type SourcePosition } from './evalError.js';
import * as num from './numbers.js';

type ExprBase = { position: SourcePosition };

type Expr =
  | (ExprBase & { kind: 'number'; value: num.NumericValue })
  | (ExprBase & { kind: 'boolean'; value: boolean })
  | (ExprBase & { kind: 'char'; value: string })
  | (ExprBase & { kind: 'string'; value: string })
  | (ExprBase & { kind: 'symbol'; name: string })
  | (ExprBase & { kind: 'list'; items: Expr[] });

type CharValue = { kind: 'char'; value: string };
type MutableStringValue = { kind: 'mutable-string'; chars: string[] };
type SchemeSymbol = { kind: 'symbol-value'; name: string };
type EmptyList = { kind: 'empty-list' };
type PairValue = { kind: 'pair'; car: Value; cdr: Value };
type VectorValue = { kind: 'vector'; items: Value[] };
type VoidValue = { kind: 'void' };
type TailStep = { kind: 'tail-step'; expr: Expr; env: Env };
type BuiltinProc = {
  kind: 'builtin';
  name: string;
  apply: (args: Value[], position: SourcePosition) => EvalResult;
};
type PatternCapture =
  | { kind: 'single'; expr: Expr }
  | { kind: 'repeat'; exprs: Expr[] };
type MacroRule = { pattern: Expr; template: Expr };
type MacroTransformer = {
  name: string;
  literals: Set<string>;
  rules: MacroRule[];
  definitionEnv: Env;
};
type EqualityState = {
  seenPairs: Map<PairValue, Set<PairValue>>;
  seenVectors: Map<VectorValue, Set<VectorValue>>;
};
type FormatState = {
  activePairs: Set<PairValue>;
  activeVectors: Set<VectorValue>;
};
type TemplateContext = {
  macro: MacroTransformer;
  captures: Map<string, PatternCapture>;
  renameMap: Map<string, string>;
  capturedFreeNames: Map<string, string>;
};
type ParamSpec = {
  required: string[];
  rest?: string;
};
type ProcedureClause = {
  params: ParamSpec;
  body: Expr[];
};
type UserProc = {
  kind: 'lambda';
  name?: string;
  params: ParamSpec;
  body: Expr[];
  env: Env;
};
type CaseLambdaProc = {
  kind: 'case-lambda';
  name?: string;
  clauses: ProcedureClause[];
  env: Env;
};
type RecordFieldSpec = {
  name: string;
  accessor: string;
  mutator?: string;
};
type RecordTypeDescriptor = {
  name: string;
  fieldNames: string[];
};
type RecordValue = {
  kind: 'record';
  recordType: RecordTypeDescriptor;
  fields: Value[];
};

type Value =
  | num.NumericValue
  | boolean
  | string
  | CharValue
  | MutableStringValue
  | SchemeSymbol
  | EmptyList
  | PairValue
  | VectorValue
  | RecordValue
  | VoidValue
  | BuiltinProc
  | UserProc
  | CaseLambdaProc;
type EvalResult = Value | TailStep;

type BindingSpec = { name: string; init: Expr };
type DoBindingSpec = { name: string; init: Expr; step?: Expr };
type Cell = { value: Value | typeof UNINITIALIZED };

const EMPTY_LIST: EmptyList = { kind: 'empty-list' };
const VOID: VoidValue = { kind: 'void' };
const UNINITIALIZED = Symbol('uninitialized');
const STRING_IMMUTABILITY_LEVEL = 15;
const CORE_SYNTAX = new Set([
  'and',
  'begin',
  'case',
  'case-lambda',
  'cond',
  'define',
  'define-record-type',
  'define-syntax',
  'do',
  'if',
  'lambda',
  'let',
  'let*',
  'letrec',
  'letrec*',
  'or',
  'quote',
  'set!',
]);

let macroIdentifierCounter = 0;

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
  private readonly bindings = new Map<string, Cell>();

  constructor(private readonly parent?: Env) {}

  define(name: string, value: Value): void {
    this.bindings.set(name, { value });
  }

  defineUninitialized(name: string): void {
    this.bindings.set(name, { value: UNINITIALIZED });
  }

  defineAlias(name: string, cell: Cell): void {
    this.bindings.set(name, cell);
  }

  lookupCell(name: string): Cell | undefined {
    const local = this.bindings.get(name);
    if (local !== undefined) {
      return local;
    }

    return this.parent?.lookupCell(name);
  }

  lookup(name: string): Value {
    const cell = this.lookupCell(name);
    if (cell !== undefined) {
      if (cell.value === UNINITIALIZED) {
        throw new EvalError(`uninitialized binding: ${name}`);
      }
      return cell.value;
    }

    throw new EvalError(`unbound symbol: ${name}`);
  }

  set(name: string, value: Value): void {
    const cell = this.lookupCell(name);
    if (cell !== undefined) {
      cell.value = value;
      return;
    }

    throw new EvalError(`unbound symbol: ${name}`);
  }
}

class MacroEnv {
  private readonly bindings = new Map<string, MacroTransformer>();

  constructor(private readonly parent?: MacroEnv) {}

  define(name: string, transformer: MacroTransformer): void {
    this.bindings.set(name, transformer);
  }

  lookup(name: string): MacroTransformer | undefined {
    const local = this.bindings.get(name);
    if (local !== undefined) {
      return local;
    }

    return this.parent?.lookup(name);
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

    if (token.startsWith('#\\')) {
      return parseCharToken(token, position);
    }

    const numericValue = num.parseNumberToken(token);
    if (numericValue !== undefined) {
      return { kind: 'number', value: numericValue, position };
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

function freshMacroIdentifier(name: string): string {
  macroIdentifierCounter += 1;
  return `__macro_${macroIdentifierCounter}_${name}`;
}

function cloneExpr(expr: Expr): Expr {
  switch (expr.kind) {
    case 'number':
      return { kind: 'number', value: expr.value, position: expr.position };
    case 'boolean':
      return { kind: 'boolean', value: expr.value, position: expr.position };
    case 'char':
      return { kind: 'char', value: expr.value, position: expr.position };
    case 'string':
      return { kind: 'string', value: expr.value, position: expr.position };
    case 'symbol':
      return { kind: 'symbol', name: expr.name, position: expr.position };
    case 'list':
      return { kind: 'list', items: expr.items.map((item) => cloneExpr(item)), position: expr.position };
  }
}

function isEllipsisSymbol(expr: Expr | undefined): expr is ExprBase & { kind: 'symbol'; name: '...' } {
  return expr?.kind === 'symbol' && expr.name === '...';
}

function parseSyntaxRules(name: string, expr: Expr, env: Env): MacroTransformer {
  if (expr.kind !== 'list' || expr.items.length < 2) {
    throw new EvalError('define-syntax expects a syntax-rules transformer');
  }

  const [headExpr, literalsExpr, ...ruleExprs] = expr.items;
  if (headExpr!.kind !== 'symbol' || headExpr.name !== 'syntax-rules') {
    throw new EvalError('define-syntax expects a syntax-rules transformer');
  }

  if (literalsExpr!.kind !== 'list') {
    throw new EvalError('syntax-rules expects a literal identifier list');
  }

  const literals = new Set<string>();
  for (const literalExpr of literalsExpr.items) {
    if (literalExpr.kind !== 'symbol') {
      throw new EvalError('syntax-rules literals must be identifiers');
    }
    literals.add(literalExpr.name);
  }

  if (ruleExprs.length === 0) {
    throw new EvalError('syntax-rules expects at least one rule');
  }

  const rules = ruleExprs.map((ruleExpr) => {
    if (ruleExpr.kind !== 'list' || ruleExpr.items.length !== 2) {
      throw new EvalError('syntax-rules expects (pattern template) rules');
    }

    return {
      pattern: ruleExpr.items[0]!,
      template: ruleExpr.items[1]!,
    };
  });

  return {
    name,
    literals,
    rules,
    definitionEnv: env,
  };
}

function expandMacroInvocation(
  transformer: MacroTransformer,
  invocation: ExprBase & { kind: 'list'; items: Expr[] },
): Expr {
  const literals = new Set(transformer.literals);
  literals.add(transformer.name);

  for (const rule of transformer.rules) {
    const captures = new Map<string, PatternCapture>();
    if (!matchPattern(rule.pattern, invocation, literals, captures)) {
      continue;
    }

    return expandTemplate(rule.template, {
      macro: transformer,
      captures,
      renameMap: new Map<string, string>(),
      capturedFreeNames: new Map<string, string>(),
    });
  }

  throw new EvalError(`no matching syntax-rules pattern for ${transformer.name}`);
}

function matchPattern(
  pattern: Expr,
  expr: Expr,
  literals: Set<string>,
  captures: Map<string, PatternCapture>,
  repeatedContext = false,
): boolean {
  switch (pattern.kind) {
    case 'number':
    case 'boolean':
    case 'char':
    case 'string':
      return pattern.kind === expr.kind && pattern.value === expr.value;

    case 'symbol':
      if (pattern.name === '_') {
        return true;
      }

      if (pattern.name === '...') {
        return false;
      }

      if (literals.has(pattern.name)) {
        return expr.kind === 'symbol' && expr.name === pattern.name;
      }

      if (repeatedContext) {
        return addRepeatedCapture(pattern.name, expr, captures);
      }

      return addSingleCapture(pattern.name, expr, captures);

    case 'list':
      return expr.kind === 'list'
        ? matchListPattern(pattern.items, expr.items, literals, captures, repeatedContext)
        : false;
  }
}

function matchListPattern(
  patternItems: Expr[],
  exprItems: Expr[],
  literals: Set<string>,
  captures: Map<string, PatternCapture>,
  repeatedContext: boolean,
): boolean {
  let patternIndex = 0;
  let exprIndex = 0;

  while (patternIndex < patternItems.length) {
    const currentPattern = patternItems[patternIndex]!;
    if (isEllipsisSymbol(currentPattern)) {
      return false;
    }

    if (isEllipsisSymbol(patternItems[patternIndex + 1])) {
      if (patternIndex + 2 !== patternItems.length) {
        throw new EvalError('syntax-rules only supports trailing ellipsis patterns');
      }

      ensureRepeatedCaptureSlots(currentPattern, literals, captures);
      while (exprIndex < exprItems.length) {
        if (!matchPattern(currentPattern, exprItems[exprIndex]!, literals, captures, true)) {
          return false;
        }
        exprIndex += 1;
      }

      return true;
    }

    if (exprIndex >= exprItems.length) {
      return false;
    }

    if (!matchPattern(currentPattern, exprItems[exprIndex]!, literals, captures, repeatedContext)) {
      return false;
    }

    patternIndex += 1;
    exprIndex += 1;
  }

  return exprIndex === exprItems.length;
}

function addSingleCapture(name: string, expr: Expr, captures: Map<string, PatternCapture>): boolean {
  const existing = captures.get(name);
  if (existing === undefined) {
    captures.set(name, { kind: 'single', expr });
    return true;
  }

  return existing.kind === 'single' && sameExpr(existing.expr, expr);
}

function addRepeatedCapture(name: string, expr: Expr, captures: Map<string, PatternCapture>): boolean {
  const existing = captures.get(name);
  if (existing === undefined) {
    captures.set(name, { kind: 'repeat', exprs: [expr] });
    return true;
  }

  if (existing.kind !== 'repeat') {
    return false;
  }

  existing.exprs.push(expr);
  return true;
}

function ensureRepeatedCaptureSlots(pattern: Expr, literals: Set<string>, captures: Map<string, PatternCapture>): void {
  switch (pattern.kind) {
    case 'symbol':
      if (pattern.name === '_' || pattern.name === '...' || literals.has(pattern.name)) {
        return;
      }

      if (!captures.has(pattern.name)) {
        captures.set(pattern.name, { kind: 'repeat', exprs: [] });
        return;
      }

      if (captures.get(pattern.name)?.kind !== 'repeat') {
        throw new EvalError('syntax-rules pattern variable used inconsistently with ellipsis');
      }
      return;

    case 'list':
      pattern.items.forEach((item) => {
        if (!isEllipsisSymbol(item)) {
          ensureRepeatedCaptureSlots(item, literals, captures);
        }
      });
      return;

    default:
      return;
  }
}

function sameExpr(left: Expr, right: Expr): boolean {
  if (left.kind !== right.kind) {
    return false;
  }

  switch (left.kind) {
    case 'number':
      return right.kind === 'number' && num.numericEqual(left.value, right.value);
    case 'boolean':
      return right.kind === 'boolean' && left.value === right.value;
    case 'char':
      return right.kind === 'char' && left.value === right.value;
    case 'string':
      return right.kind === 'string' && left.value === right.value;
    case 'symbol':
      return right.kind === 'symbol' && left.name === right.name;
    case 'list':
      return (
        right.kind === 'list' &&
        left.items.length === right.items.length &&
        left.items.every((item, index) => sameExpr(item, right.items[index]!))
      );
  }
}

function expandTemplate(expr: Expr, ctx: TemplateContext, repetitionIndex?: number): Expr {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
    case 'char':
    case 'string':
      return cloneExpr(expr);

    case 'symbol':
      return expandTemplateSymbol(expr, ctx, repetitionIndex);

    case 'list':
      return expandTemplateList(expr, ctx, repetitionIndex);
  }
}

function expandTemplateSymbol(expr: ExprBase & { kind: 'symbol'; name: string }, ctx: TemplateContext, repetitionIndex?: number): Expr {
  const capture = ctx.captures.get(expr.name);
  if (capture !== undefined) {
    return cloneExpr(resolveCapture(capture, repetitionIndex));
  }

  const renamed = ctx.renameMap.get(expr.name);
  if (renamed !== undefined) {
    return { kind: 'symbol', name: renamed, position: expr.position };
  }

  if (expr.name === '...' || expr.name === ctx.macro.name || ctx.macro.literals.has(expr.name) || CORE_SYNTAX.has(expr.name)) {
    return cloneExpr(expr);
  }

  const capturedName = captureDefinitionIdentifier(expr.name, ctx);
  if (capturedName !== undefined) {
    return { kind: 'symbol', name: capturedName, position: expr.position };
  }

  return cloneExpr(expr);
}

function expandTemplateList(expr: ExprBase & { kind: 'list'; items: Expr[] }, ctx: TemplateContext, repetitionIndex?: number): Expr {
  const [headExpr] = expr.items;
  if (headExpr?.kind === 'symbol' && headExpr.name === 'quote') {
    return cloneExpr(expr);
  }

  if (headExpr?.kind === 'symbol' && headExpr.name === 'let') {
    return expandLetTemplate(expr, ctx, repetitionIndex);
  }

  return {
    kind: 'list',
    items: expandTemplateSequence(expr.items, ctx, repetitionIndex),
    position: expr.position,
  };
}

function expandTemplateSequence(items: Expr[], ctx: TemplateContext, repetitionIndex?: number): Expr[] {
  const expanded: Expr[] = [];

  for (let index = 0; index < items.length; index += 1) {
    const item = items[index]!;

    if (isEllipsisSymbol(item)) {
      throw new EvalError('unexpected ellipsis in syntax-rules template');
    }

    if (isEllipsisSymbol(items[index + 1])) {
      const repeatCount = templateRepetitionCount(item, ctx.captures);
      for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
        expanded.push(expandTemplate(item, ctx, repeatIndex));
      }
      index += 1;
      continue;
    }

    expanded.push(expandTemplate(item, ctx, repetitionIndex));
  }

  return expanded;
}

function expandLetTemplate(
  expr: ExprBase & { kind: 'list'; items: Expr[] },
  ctx: TemplateContext,
  repetitionIndex?: number,
): Expr {
  if (expr.items.length < 3) {
    return {
      kind: 'list',
      items: expandTemplateSequence(expr.items, ctx, repetitionIndex),
      position: expr.position,
    };
  }

  const bindingsExpr = expr.items[1]!;
  if (bindingsExpr.kind !== 'list') {
    return {
      kind: 'list',
      items: expandTemplateSequence(expr.items, ctx, repetitionIndex),
      position: expr.position,
    };
  }

  const scopedRenameMap = new Map(ctx.renameMap);
  const expandedBindings = bindingsExpr.items.map((bindingExpr) => {
    if (bindingExpr.kind !== 'list' || bindingExpr.items.length !== 2) {
      throw new EvalError('syntax-rules let template expects binding pairs');
    }

    const [nameExpr, initExpr] = bindingExpr.items;
    if (nameExpr!.kind !== 'symbol') {
      throw new EvalError('syntax-rules let template expects symbol bindings');
    }

    let expandedName: Expr;
    const capture = ctx.captures.get(nameExpr.name);
    if (capture !== undefined) {
      expandedName = cloneExpr(resolveCapture(capture, repetitionIndex));
    } else {
      const renamedName = freshMacroIdentifier(nameExpr.name);
      scopedRenameMap.set(nameExpr.name, renamedName);
      expandedName = { kind: 'symbol', name: renamedName, position: nameExpr.position };
    }

    return {
      kind: 'list' as const,
      items: [expandedName, expandTemplate(initExpr!, ctx, repetitionIndex)],
      position: bindingExpr.position,
    };
  });

  const bodyContext: TemplateContext = {
    ...ctx,
    renameMap: scopedRenameMap,
  };

  return {
    kind: 'list',
    items: [
      cloneExpr(expr.items[0]!),
      { kind: 'list', items: expandedBindings, position: bindingsExpr.position },
      ...expandTemplateSequence(expr.items.slice(2), bodyContext, repetitionIndex),
    ],
    position: expr.position,
  };
}

function resolveCapture(capture: PatternCapture, repetitionIndex?: number): Expr {
  if (capture.kind === 'single') {
    return capture.expr;
  }

  if (repetitionIndex === undefined) {
    throw new EvalError('syntax-rules template expected ellipsis for repeated pattern variable');
  }

  const value = capture.exprs[repetitionIndex];
  if (value === undefined) {
    throw new EvalError('syntax-rules ellipsis repetition mismatch');
  }

  return value;
}

function templateRepetitionCount(template: Expr, captures: Map<string, PatternCapture>): number {
  const repeatedNames = [...collectRepeatedCaptureNames(template, captures)];
  if (repeatedNames.length === 0) {
    throw new EvalError('syntax-rules ellipsis requires a repeated pattern variable');
  }

  const count = repeatedCaptureLength(captures.get(repeatedNames[0]!)!);
  for (const name of repeatedNames.slice(1)) {
    if (repeatedCaptureLength(captures.get(name)!) !== count) {
      throw new EvalError('syntax-rules ellipsis groups must repeat in lockstep');
    }
  }

  return count;
}

function collectRepeatedCaptureNames(template: Expr, captures: Map<string, PatternCapture>, names = new Set<string>()): Set<string> {
  switch (template.kind) {
    case 'symbol': {
      const capture = captures.get(template.name);
      if (capture?.kind === 'repeat') {
        names.add(template.name);
      }
      return names;
    }

    case 'list':
      if (template.items[0]?.kind === 'symbol' && template.items[0].name === 'quote') {
        return names;
      }
      template.items.forEach((item) => {
        collectRepeatedCaptureNames(item, captures, names);
      });
      return names;

    default:
      return names;
  }
}

function repeatedCaptureLength(capture: PatternCapture): number {
  return capture.kind === 'repeat' ? capture.exprs.length : 0;
}

function captureDefinitionIdentifier(name: string, ctx: TemplateContext): string | undefined {
  const cached = ctx.capturedFreeNames.get(name);
  if (cached !== undefined) {
    return cached;
  }

  const cell = ctx.macro.definitionEnv.lookupCell(name);
  if (cell === undefined) {
    return undefined;
  }

  const alias = freshMacroIdentifier(name);
  ctx.macro.definitionEnv.defineAlias(alias, cell);
  ctx.capturedFreeNames.set(name, alias);
  return alias;
}

function createBuiltins(output: OutputBuffer, macroEnv: MacroEnv): Map<string, BuiltinProc> {
  return new Map<string, BuiltinProc>([
    builtin('+', (args) => sumNumbers('+', args)),
    builtin('*', (args) => productNumbers('*', args)),
    builtin('-', (args) => subtractNumbers(args)),
    builtin('/', (args) => divideNumbers(args)),
    builtin('<', (args) => compareNumbers('<', args, (left, right) => num.numericCompare(left, right) < 0)),
    builtin('>', (args) => compareNumbers('>', args, (left, right) => num.numericCompare(left, right) > 0)),
    builtin('=', (args) => compareNumbers('=', args, (left, right) => num.numericEqual(left, right))),
    builtin('<=', (args) => compareNumbers('<=', args, (left, right) => num.numericCompare(left, right) <= 0)),
    builtin('>=', (args) => compareNumbers('>=', args, (left, right) => num.numericCompare(left, right) >= 0)),
    builtin('abs', (args) => absoluteValue(args)),
    builtin('apply', (args, position) => applyBuiltin(args, position, macroEnv)),
    builtin('append', (args) => appendValues(args)),
    builtin('assoc', (args) => assocBuiltin(args)),
    builtin('assv', (args) => assvBuiltin(args)),
    builtin('boolean?', (args) => unaryPredicate('boolean?', args, (value) => typeof value === 'boolean')),
    builtin('car', (args) => {
      assertExactArity('car', args, 1);
      return expectPair('car', args[0]!).car;
    }),
    builtin('cdr', (args) => {
      assertExactArity('cdr', args, 1);
      return expectPair('cdr', args[0]!).cdr;
    }),
    builtin('cddr', (args) => {
      assertExactArity('cddr', args, 1);
      return expectPair('cddr', expectPair('cddr', args[0]!).cdr).cdr;
    }),
    builtin('char-alphabetic?', (args) => {
      assertExactArity('char-alphabetic?', args, 1);
      return isAlphabeticChar(expectCharValue('char-alphabetic?', args[0]!).value);
    }),
    builtin('char-downcase', (args) => {
      assertExactArity('char-downcase', args, 1);
      return { kind: 'char', value: expectCharValue('char-downcase', args[0]!).value.toLowerCase() };
    }),
    builtin('char->integer', (args) => {
      assertExactArity('char->integer', args, 1);
      return num.exactIntegerFromNumber(expectCharValue('char->integer', args[0]!).value.codePointAt(0)!);
    }),
    builtin('char-numeric?', (args) => {
      assertExactArity('char-numeric?', args, 1);
      return isNumericChar(expectCharValue('char-numeric?', args[0]!).value);
    }),
    builtin('char<?', (args) => compareChars('char<?', args, (left, right) => left < right)),
    builtin('char=?', (args) => compareChars('char=?', args, (left, right) => left === right)),
    builtin('char-upcase', (args) => {
      assertExactArity('char-upcase', args, 1);
      return { kind: 'char', value: expectCharValue('char-upcase', args[0]!).value.toUpperCase() };
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
    builtin('eq?', (args) => {
      assertExactArity('eq?', args, 2);
      return eqValues(args[0]!, args[1]!);
    }),
    builtin('eqv?', (args) => {
      assertExactArity('eqv?', args, 2);
      return eqvValues(args[0]!, args[1]!);
    }),
    builtin('equal?', (args) => {
      assertExactArity('equal?', args, 2);
      return equalValues(args[0]!, args[1]!);
    }),
    builtin('denominator', (args) => {
      assertExactArity('denominator', args, 1);
      return num.denominatorOf(expectNumberValue('denominator', args[0]!));
    }),
    builtin('even?', (args) => integerPredicate('even?', args, (value) => num.numericIsEven(value))),
    builtin('exact->inexact', (args) => {
      assertExactArity('exact->inexact', args, 1);
      return num.exactToInexact(expectNumberValue('exact->inexact', args[0]!));
    }),
    builtin('exact?', (args) =>
      unaryPredicate('exact?', args, (value) => num.isNumericValue(value) && num.isExactNumeric(value)),
    ),
    builtin('expt', (args) => exptNumbers(args)),
    builtin('for-each', (args, position) => forEachBuiltin(args, position, macroEnv)),
    builtin('gcd', (args) => gcdBuiltin(args)),
    builtin('inexact->exact', (args) => {
      assertExactArity('inexact->exact', args, 1);
      return num.inexactToExact(expectNumberValue('inexact->exact', args[0]!));
    }),
    builtin('inexact?', (args) =>
      unaryPredicate('inexact?', args, (value) => num.isNumericValue(value) && num.isInexactNumeric(value)),
    ),
    builtin('integer->char', (args) => {
      assertExactArity('integer->char', args, 1);
      return { kind: 'char', value: String.fromCodePoint(expectCodePoint('integer->char', args[0]!)) };
    }),
    builtin('integer?', (args) =>
      unaryPredicate('integer?', args, (value) => num.isNumericValue(value) && num.numericIsInteger(value)),
    ),
    builtin('length', (args) => {
      assertExactArity('length', args, 1);
      return num.exactIntegerFromNumber(expectProperList('length', args[0]!).length);
    }),
    builtin('list', (args) => makeList(args)),
    builtin('list-ref', (args) => listRefBuiltin(args)),
    builtin('list-tail', (args) => listTailBuiltin(args)),
    builtin('list->string', (args) => {
      assertExactArity('list->string', args, 1);
      return makeRuntimeString(
        expectProperList('list->string', args[0]!)
          .map((item) => expectCharValue('list->string', item).value)
          .join(''),
      );
    }),
    builtin('list->vector', (args) => {
      assertExactArity('list->vector', args, 1);
      return { kind: 'vector', items: expectProperList('list->vector', args[0]!) };
    }),
    builtin('list?', (args) => {
      assertExactArity('list?', args, 1);
      return isProperListValue(args[0]!);
    }),
    builtin('lcm', (args) => lcmBuiltin(args)),
    builtin('make-string', (args) => makeStringBuiltin(args)),
    builtin('make-vector', (args) => makeVectorBuiltin(args)),
    builtin('map', (args, position) => mapBuiltin(args, position, macroEnv)),
    builtin('max', (args) =>
      extremum('max', args, (left, right) => (num.numericCompare(left, right) >= 0 ? left : right)),
    ),
    builtin('member', (args) => memberBuiltin(args)),
    builtin('min', (args) =>
      extremum('min', args, (left, right) => (num.numericCompare(left, right) <= 0 ? left : right)),
    ),
    builtin('modulo', (args) => moduloNumbers(args)),
    builtin('newline', (args) => {
      assertExactArity('newline', args, 0);
      output.write('\n');
      return VOID;
    }),
    builtin('negative?', (args) => numberPredicate('negative?', args, (value) => num.numericIsNegative(value))),
    builtin('not', (args) => {
      assertExactArity('not', args, 1);
      return !isTruthy(args[0]!);
    }),
    builtin('null?', (args) => unaryPredicate('null?', args, isEmptyList)),
    builtin('number->string', (args) => {
      assertExactArity('number->string', args, 1);
      return makeRuntimeString(num.formatNumber(expectNumberValue('number->string', args[0]!)));
    }),
    builtin('number?', (args) => unaryPredicate('number?', args, (value) => num.isNumericValue(value))),
    builtin('numerator', (args) => {
      assertExactArity('numerator', args, 1);
      return num.numeratorOf(expectNumberValue('numerator', args[0]!));
    }),
    builtin('odd?', (args) => integerPredicate('odd?', args, (value) => num.numericIsOdd(value))),
    builtin('pair?', (args) => unaryPredicate('pair?', args, isPair)),
    builtin('positive?', (args) => numberPredicate('positive?', args, (value) => num.numericIsPositive(value))),
    builtin('procedure?', (args) => unaryPredicate('procedure?', args, isProcedure)),
    builtin('quotient', (args) => quotientNumbers(args)),
    builtin('rational?', (args) => unaryPredicate('rational?', args, (value) => num.isNumericValue(value))),
    builtin('remainder', (args) => remainderNumbers(args)),
    builtin('round', (args) => roundBuiltin(args)),
    builtin('reverse', (args) => {
      assertExactArity('reverse', args, 1);
      return makeList(expectProperList('reverse', args[0]!).slice().reverse());
    }),
    builtin('string', (args) => stringBuiltin(args)),
    builtin('string->number', (args) => {
      assertExactArity('string->number', args, 1);
      return num.parseStringNumber(expectStringValue('string->number', args[0]!));
    }),
    builtin('string->list', (args) => {
      assertExactArity('string->list', args, 1);
      return makeList(stringChars(expectStringValue('string->list', args[0]!)).map((char) => ({ kind: 'char', value: char })));
    }),
    builtin('string->symbol', (args) => {
      assertExactArity('string->symbol', args, 1);
      return { kind: 'symbol-value', name: expectStringValue('string->symbol', args[0]!) };
    }),
    builtin('string-append', (args) => makeRuntimeString(args.map((arg) => expectStringValue('string-append', arg)).join(''))),
    builtin('string-copy', (args) => {
      assertExactArity('string-copy', args, 1);
      return makeRuntimeString(expectStringValue('string-copy', args[0]!));
    }),
    builtin('string-ci=?', (args) =>
      compareStrings('string-ci=?', args, (value) => value.toLowerCase(), (left, right) => left === right),
    ),
    builtin('string-downcase', (args) => {
      assertExactArity('string-downcase', args, 1);
      return makeRuntimeString(expectStringValue('string-downcase', args[0]!).toLowerCase());
    }),
    builtin('string>?', (args) => compareStrings('string>?', args, (value) => value, (left, right) => left > right)),
    builtin('string<=?', (args) => compareStrings('string<=?', args, (value) => value, (left, right) => left <= right)),
    builtin('string>=?', (args) => compareStrings('string>=?', args, (value) => value, (left, right) => left >= right)),
    builtin('string<?', (args) => compareStrings('string<?', args, (value) => value, (left, right) => left < right)),
    builtin('string=?', (args) => compareStrings('string=?', args, (value) => value, (left, right) => left === right)),
    builtin('string-length', (args) => {
      assertExactArity('string-length', args, 1);
      return num.exactIntegerFromNumber(stringChars(expectStringValue('string-length', args[0]!)).length);
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
    builtin('string-set!', (args) => {
      assertExactArity('string-set!', args, 3);
      if (stringsAreImmutable()) {
        throw new EvalError('string-set! cannot mutate immutable strings');
      }

      const stringValue = expectMutableStringValue('string-set!', args[0]!);
      const index = expectIndex('string-set!', args[1]!);
      const charValue = expectCharValue('string-set!', args[2]!);
      if (index >= stringValue.chars.length) {
        throw new EvalError('string-set! index out of bounds');
      }
      stringValue.chars[index] = charValue.value;
      return VOID;
    }),
    builtin('string?', (args) => unaryPredicate('string?', args, isStringValue)),
    builtin('string-upcase', (args) => {
      assertExactArity('string-upcase', args, 1);
      return makeRuntimeString(expectStringValue('string-upcase', args[0]!).toUpperCase());
    }),
    builtin('substring', (args) => {
      assertExactArity('substring', args, 3);
      const chars = stringChars(expectStringValue('substring', args[0]!));
      const start = expectIndex('substring', args[1]!);
      const end = expectIndex('substring', args[2]!);
      if (start > end || end > chars.length) {
        throw new EvalError('substring expects valid start/end indices');
      }
      return makeRuntimeString(chars.slice(start, end).join(''));
    }),
    builtin('symbol->string', (args) => {
      assertExactArity('symbol->string', args, 1);
      return makeRuntimeString(expectSymbolValue('symbol->string', args[0]!).name);
    }),
    builtin('symbol?', (args) => unaryPredicate('symbol?', args, isSymbolValue)),
    builtin('truncate', (args) => truncateBuiltin(args)),
    builtin('set-car!', (args) => {
      assertExactArity('set-car!', args, 2);
      expectPair('set-car!', args[0]!).car = args[1]!;
      return VOID;
    }),
    builtin('set-cdr!', (args) => {
      assertExactArity('set-cdr!', args, 2);
      expectPair('set-cdr!', args[0]!).cdr = args[1]!;
      return VOID;
    }),
    builtin('vector', (args) => ({ kind: 'vector', items: [...args] })),
    builtin('vector->list', (args) => {
      assertExactArity('vector->list', args, 1);
      return makeList(expectVectorValue('vector->list', args[0]!).items);
    }),
    builtin('vector-length', (args) => {
      assertExactArity('vector-length', args, 1);
      return num.exactIntegerFromNumber(expectVectorValue('vector-length', args[0]!).items.length);
    }),
    builtin('vector-ref', (args) => {
      assertExactArity('vector-ref', args, 2);
      const vector = expectVectorValue('vector-ref', args[0]!);
      const index = expectIndex('vector-ref', args[1]!);
      if (index >= vector.items.length) {
        throw new EvalError('vector-ref index out of bounds');
      }
      return vector.items[index]!;
    }),
    builtin('vector-set!', (args) => {
      assertExactArity('vector-set!', args, 3);
      const vector = expectVectorValue('vector-set!', args[0]!);
      const index = expectIndex('vector-set!', args[1]!);
      if (index >= vector.items.length) {
        throw new EvalError('vector-set! index out of bounds');
      }
      vector.items[index] = args[2]!;
      return VOID;
    }),
    builtin('vector?', (args) => unaryPredicate('vector?', args, isVectorValue)),
    builtin('write', (args) => {
      assertExactArity('write', args, 1);
      output.write(formatValue(args[0]!));
      return VOID;
    }),
    builtin('zero?', (args) => numberPredicate('zero?', args, (value) => num.numericIsZero(value))),
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
  const macroEnv = new MacroEnv();
  const env = createGlobalEnv(output, macroEnv);
  const lastValue = evalSequence(program, env, macroEnv);

  return {
    result: formatValue(lastValue),
    output: output.toString(),
  };
}

function createGlobalEnv(output: OutputBuffer, macroEnv: MacroEnv): Env {
  const env = new Env();
  for (const [name, value] of createBuiltins(output, macroEnv)) {
    env.define(name, value);
  }
  return env;
}

function evalSequence(exprs: Expr[], env: Env, macroEnv: MacroEnv): Value {
  if (exprs.length === 0) {
    return VOID;
  }

  for (const expr of exprs.slice(0, -1)) {
    evalExpr(expr, env, macroEnv);
  }

  return evalExpr(exprs[exprs.length - 1]!, env, macroEnv);
}

function evalSequenceTail(exprs: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  if (exprs.length === 0) {
    return VOID;
  }

  for (const expr of exprs.slice(0, -1)) {
    evalExpr(expr, env, macroEnv);
  }

  return { kind: 'tail-step', expr: exprs[exprs.length - 1]!, env };
}

function isTailStep(result: EvalResult): result is TailStep {
  return typeof result === 'object' && result !== null && result.kind === 'tail-step';
}

function resolveEvalResult(result: EvalResult, macroEnv: MacroEnv): Value {
  return isTailStep(result) ? evalExpr(result.expr, result.env, macroEnv) : result;
}

function evalExpr(expr: Expr, env: Env, macroEnv: MacroEnv): Value {
  let currentExpr = expr;
  let currentEnv = env;

  while (true) {
    try {
      const result = evalExprOnce(currentExpr, currentEnv, macroEnv);
      if (isTailStep(result)) {
        currentExpr = result.expr;
        currentEnv = result.env;
        continue;
      }

      return result;
    } catch (error) {
      throw attachPosition(error, currentExpr.position);
    }
  }
}

function evalExprOnce(expr: Expr, env: Env, macroEnv: MacroEnv): EvalResult {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
      return expr.value;

    case 'string':
      return makeRuntimeString(expr.value);

    case 'char':
      return { kind: 'char', value: expr.value };

    case 'symbol':
      return env.lookup(expr.name);

    case 'list':
      return evalList(expr, env, macroEnv);
  }
}

function evalList(expr: ExprBase & { kind: 'list'; items: Expr[] }, env: Env, macroEnv: MacroEnv): EvalResult {
  const { items } = expr;
  if (items.length === 0) {
    throw new EvalError('cannot evaluate empty list');
  }

  const first = items[0]!;
  if (first.kind === 'symbol') {
    switch (first.name) {
      case 'and':
        return evalAnd(items.slice(1), env, macroEnv);
      case 'begin':
        return evalSequenceTail(items.slice(1), env, macroEnv);
      case 'case':
        return evalCase(items.slice(1), env, macroEnv);
      case 'case-lambda':
        return evalCaseLambda(items.slice(1), env);
      case 'cond':
        return evalCond(items.slice(1), env, macroEnv);
      case 'define':
        return evalDefine(items.slice(1), env, macroEnv);
      case 'define-record-type':
        return evalDefineRecordType(items.slice(1), env);
      case 'define-syntax':
        return evalDefineSyntax(items.slice(1), env, macroEnv);
      case 'do':
        return evalDo(items.slice(1), env, macroEnv);
      case 'if':
        return evalIf(items.slice(1), env, macroEnv);
      case 'lambda':
        return evalLambda(items.slice(1), env);
      case 'let':
        return evalLet(items.slice(1), env, macroEnv);
      case 'let*':
        return evalLetStar(items.slice(1), env, macroEnv);
      case 'letrec':
        return evalLetRec(items.slice(1), env, macroEnv, false);
      case 'letrec*':
        return evalLetRec(items.slice(1), env, macroEnv, true);
      case 'or':
        return evalOr(items.slice(1), env, macroEnv);
      case 'quote':
        return evalQuote(items.slice(1));
      case 'set!':
        return evalSet(items.slice(1), env, macroEnv);
    }

    const macro = macroEnv.lookup(first.name);
    if (macro !== undefined) {
      return { kind: 'tail-step', expr: expandMacroInvocation(macro, expr), env };
    }
  }

  const proc = evalExpr(first, env, macroEnv);
  if (!isProcedure(proc)) {
    throw new EvalError('attempted to call a non-procedure');
  }

  const args = items.slice(1).map((item) => evalExpr(item, env, macroEnv));
  return applyProcedure(proc, args, first.position, macroEnv);
}

function evalAnd(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  if (args.length === 0) {
    return true;
  }

  for (const arg of args.slice(0, -1)) {
    const value = evalExpr(arg, env, macroEnv);
    if (!isTruthy(value)) {
      return value;
    }
  }

  return { kind: 'tail-step', expr: args[args.length - 1]!, env };
}

function evalCase(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  assertAtLeastArity('case', args, 1);

  const key = evalExpr(args[0]!, env, macroEnv);
  const clauses = args.slice(1);

  for (let index = 0; index < clauses.length; index += 1) {
    const clauseExpr = clauses[index]!;
    if (clauseExpr.kind !== 'list' || clauseExpr.items.length === 0) {
      throw new EvalError('case expects non-empty clauses');
    }

    const [headExpr, ...body] = clauseExpr.items;
    if (headExpr!.kind === 'symbol' && headExpr.name === 'else') {
      if (index !== clauses.length - 1) {
        throw new EvalError('case else clause must be last');
      }
      return evalSequenceTail(body, env, macroEnv);
    }

    if (headExpr!.kind !== 'list') {
      throw new EvalError('case expects each clause datum list to be a list');
    }

    if (headExpr.items.some((datumExpr) => eqvValues(key, quoteExpr(datumExpr)))) {
      return evalSequenceTail(body, env, macroEnv);
    }
  }

  return VOID;
}

function evalCond(clauses: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
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
      return evalSequenceTail(body, env, macroEnv);
    }

    const testValue = evalExpr(testExpr!, env, macroEnv);
    if (isTruthy(testValue)) {
      return body.length === 0 ? testValue : evalSequenceTail(body, env, macroEnv);
    }
  }

  return VOID;
}

function evalOr(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  if (args.length === 0) {
    return false;
  }

  for (const arg of args.slice(0, -1)) {
    const value = evalExpr(arg, env, macroEnv);
    if (isTruthy(value)) {
      return value;
    }
  }

  return { kind: 'tail-step', expr: args[args.length - 1]!, env };
}

function evalDefine(args: Expr[], env: Env, macroEnv: MacroEnv): Value {
  assertAtLeastArity('define', args, 2);

  const target = args[0]!;
  const body = args.slice(1);

  if (target.kind === 'symbol') {
    assertExactArity('define', body, 1);
    env.define(target.name, evalExpr(body[0]!, env, macroEnv));
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
      params: parseParamItems(target.items.slice(1)),
      body,
      env,
    });
    return VOID;
  }

  throw new EvalError('define expects a symbol name');
}

function evalDefineSyntax(args: Expr[], env: Env, macroEnv: MacroEnv): Value {
  assertExactArity('define-syntax', args, 2);

  const nameExpr = args[0]!;
  if (nameExpr.kind !== 'symbol') {
    throw new EvalError('define-syntax expects a symbol name');
  }

  macroEnv.define(nameExpr.name, parseSyntaxRules(nameExpr.name, args[1]!, env));
  return VOID;
}

function evalDefineRecordType(args: Expr[], env: Env): Value {
  assertAtLeastArity('define-record-type', args, 3);

  const recordTypeName = expectSymbolExprName('define-record-type', args[0]!, 'type name');
  const constructorSpec = parseRecordConstructorSpec(args[1]!);
  const predicateName = expectSymbolExprName('define-record-type', args[2]!, 'predicate name');
  const fieldSpecs = args.slice(3).map((fieldExpr) => parseRecordFieldSpec(fieldExpr));
  const fieldIndices = new Map<string, number>();

  fieldSpecs.forEach((fieldSpec, index) => {
    if (fieldIndices.has(fieldSpec.name)) {
      throw new EvalError('define-record-type field names must be unique');
    }
    fieldIndices.set(fieldSpec.name, index);
  });

  const constructorFieldIndices = constructorSpec.fields.map((fieldName) => {
    const fieldIndex = fieldIndices.get(fieldName);
    if (fieldIndex === undefined) {
      throw new EvalError(`define-record-type constructor field not found: ${fieldName}`);
    }
    return fieldIndex;
  });

  const recordType: RecordTypeDescriptor = {
    name: recordTypeName,
    fieldNames: fieldSpecs.map((fieldSpec) => fieldSpec.name),
  };

  env.define(constructorSpec.name, {
    kind: 'builtin',
    name: constructorSpec.name,
    apply: (constructorArgs) => {
      assertExactArity(constructorSpec.name, constructorArgs, constructorSpec.fields.length);
      const fields = fieldSpecs.map(() => VOID as Value);
      constructorFieldIndices.forEach((fieldIndex, argIndex) => {
        fields[fieldIndex] = constructorArgs[argIndex]!;
      });
      return { kind: 'record', recordType, fields };
    },
  });

  env.define(predicateName, {
    kind: 'builtin',
    name: predicateName,
    apply: (predicateArgs) => {
      assertExactArity(predicateName, predicateArgs, 1);
      return isRecordValue(predicateArgs[0]!) && predicateArgs[0]!.recordType === recordType;
    },
  });

  fieldSpecs.forEach((fieldSpec, index) => {
    env.define(fieldSpec.accessor, {
      kind: 'builtin',
      name: fieldSpec.accessor,
      apply: (accessorArgs) => {
        assertExactArity(fieldSpec.accessor, accessorArgs, 1);
        return expectRecordValue(fieldSpec.accessor, accessorArgs[0]!, recordType).fields[index]!;
      },
    });

    if (fieldSpec.mutator !== undefined) {
      env.define(fieldSpec.mutator, {
        kind: 'builtin',
        name: fieldSpec.mutator,
        apply: (mutatorArgs) => {
          assertExactArity(fieldSpec.mutator!, mutatorArgs, 2);
          expectRecordValue(fieldSpec.mutator!, mutatorArgs[0]!, recordType).fields[index] = mutatorArgs[1]!;
          return VOID;
        },
      });
    }
  });

  return VOID;
}

function evalIf(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  if (args.length !== 2 && args.length !== 3) {
    throw new EvalError('if expects 2 or 3 argument(s)');
  }

  const [conditionExpr, thenExpr, elseExpr] = args;
  return isTruthy(evalExpr(conditionExpr!, env, macroEnv))
    ? { kind: 'tail-step', expr: thenExpr!, env }
    : elseExpr === undefined
      ? VOID
      : { kind: 'tail-step', expr: elseExpr, env };
}

function evalCaseLambda(args: Expr[], env: Env): Value {
  assertAtLeastArity('case-lambda', args, 1);

  return {
    kind: 'case-lambda',
    clauses: args.map((clauseExpr) => parseCaseLambdaClause(clauseExpr)),
    env,
  };
}

function evalLambda(args: Expr[], env: Env): Value {
  assertAtLeastArity('lambda', args, 2);

  const paramsExpr = args[0]!;

  return {
    kind: 'lambda',
    params: parseFormals(paramsExpr),
    body: args.slice(1),
    env,
  };
}

function evalLet(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  assertAtLeastArity('let', args, 2);

  const firstArg = args[0]!;
  if (firstArg.kind === 'symbol') {
    assertAtLeastArity('let', args, 3);
    return evalNamedLet(firstArg.name, args[1]!, args.slice(2), env, macroEnv);
  }

  const bindings = parseBindings(firstArg);
  const body = args.slice(1);
  const values = bindings.map((binding) => evalExpr(binding.init, env, macroEnv));
  const letEnv = new Env(env);

  bindings.forEach((binding, index) => {
    letEnv.define(binding.name, values[index]!);
  });

  return evalSequenceTail(body, letEnv, macroEnv);
}

function evalLetStar(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  assertAtLeastArity('let*', args, 2);

  const bindings = parseBindings(args[0]!, 'let*');
  const body = args.slice(1);
  const letStarEnv = new Env(env);

  for (const binding of bindings) {
    letStarEnv.define(binding.name, evalExpr(binding.init, letStarEnv, macroEnv));
  }

  return evalSequenceTail(body, letStarEnv, macroEnv);
}

function evalLetRec(args: Expr[], env: Env, macroEnv: MacroEnv, sequential: boolean): EvalResult {
  const name = sequential ? 'letrec*' : 'letrec';
  assertAtLeastArity(name, args, 2);

  const bindings = parseBindings(args[0]!, name);
  const body = args.slice(1);
  const letEnv = new Env(env);

  bindings.forEach((binding) => {
    letEnv.defineUninitialized(binding.name);
  });

  if (sequential) {
    for (const binding of bindings) {
      letEnv.set(binding.name, evalExpr(binding.init, letEnv, macroEnv));
    }
  } else {
    const values = bindings.map((binding) => evalExpr(binding.init, letEnv, macroEnv));
    bindings.forEach((binding, index) => {
      letEnv.set(binding.name, values[index]!);
    });
  }

  return evalSequenceTail(body, letEnv, macroEnv);
}

function evalNamedLet(name: string, bindingsExpr: Expr, body: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  const bindings = parseBindings(bindingsExpr);
  const values = bindings.map((binding) => evalExpr(binding.init, env, macroEnv));
  const letEnv = new Env(env);
  const proc: UserProc = {
    kind: 'lambda',
    name,
    params: { required: bindings.map((binding) => binding.name) },
    body,
    env: letEnv,
  };

  letEnv.define(name, proc);
  return applyProcedure(proc, values, bindingsExpr.position, macroEnv);
}

function evalDo(args: Expr[], env: Env, macroEnv: MacroEnv): EvalResult {
  assertAtLeastArity('do', args, 2);

  const bindings = parseDoBindings(args[0]!);
  const testClause = parseDoTestClause(args[1]!);
  const body = args.slice(2);
  const initialValues = bindings.map((binding) => evalExpr(binding.init, env, macroEnv));
  const doEnv = new Env(env);

  bindings.forEach((binding, index) => {
    doEnv.define(binding.name, initialValues[index]!);
  });

  while (true) {
    if (isTruthy(evalExpr(testClause.test, doEnv, macroEnv))) {
      return evalSequenceTail(testClause.results, doEnv, macroEnv);
    }

    evalSequence(body, doEnv, macroEnv);

    const nextValues = bindings.map((binding) =>
      binding.step === undefined ? doEnv.lookup(binding.name) : evalExpr(binding.step, doEnv, macroEnv),
    );

    bindings.forEach((binding, index) => {
      doEnv.set(binding.name, nextValues[index]!);
    });
  }
}

function evalQuote(args: Expr[]): Value {
  assertExactArity('quote', args, 1);
  return quoteExpr(args[0]!);
}

function evalSet(args: Expr[], env: Env, macroEnv: MacroEnv): Value {
  assertExactArity('set!', args, 2);

  const target = args[0]!;
  if (target.kind !== 'symbol') {
    throw new EvalError('set! expects a symbol name');
  }

  env.set(target.name, evalExpr(args[1]!, env, macroEnv));
  return VOID;
}

function parseFormals(expr: Expr): ParamSpec {
  if (expr.kind === 'symbol') {
    return { required: [], rest: expr.name };
  }

  if (expr.kind !== 'list') {
    throw new EvalError('lambda expects a parameter list');
  }

  return parseParamItems(expr.items);
}

function parseCaseLambdaClause(expr: Expr): ProcedureClause {
  if (expr.kind !== 'list' || expr.items.length < 2) {
    throw new EvalError('case-lambda clauses must be of the form (formals body ...)');
  }

  const [paramsExpr, ...body] = expr.items;
  return {
    params: parseFormals(paramsExpr!),
    body,
  };
}

function parseParamItems(items: Expr[]): ParamSpec {
  const required: string[] = [];

  for (let index = 0; index < items.length; index += 1) {
    const item = items[index]!;
    if (item.kind !== 'symbol') {
      throw new EvalError('lambda parameters must be symbols');
    }

    if (item.name === '.') {
      if (index !== items.length - 2) {
        throw new EvalError('lambda expects a valid dotted parameter list');
      }

      const restExpr = items[index + 1]!;
      if (restExpr.kind !== 'symbol' || restExpr.name === '.') {
        throw new EvalError('lambda parameters must be symbols');
      }

      return {
        required,
        rest: restExpr.name,
      };
    }

    required.push(item.name);
  }

  return { required };
}

function parseBindings(expr: Expr, formName = 'let'): BindingSpec[] {
  if (expr.kind !== 'list') {
    throw new EvalError(`${formName} expects a binding list`);
  }

  return expr.items.map((bindingExpr) => {
    if (bindingExpr.kind !== 'list' || bindingExpr.items.length !== 2) {
      throw new EvalError(`${formName} bindings must be pairs`);
    }

    const nameExpr = bindingExpr.items[0]!;
    if (nameExpr.kind !== 'symbol') {
      throw new EvalError(`${formName} bindings must start with a symbol`);
    }

    return {
      name: nameExpr.name,
      init: bindingExpr.items[1]!,
    };
  });
}

function parseDoBindings(expr: Expr): DoBindingSpec[] {
  if (expr.kind !== 'list') {
    throw new EvalError('do expects a binding list');
  }

  return expr.items.map((bindingExpr) => {
    if (bindingExpr.kind !== 'list' || (bindingExpr.items.length !== 2 && bindingExpr.items.length !== 3)) {
      throw new EvalError('do bindings must be of the form (name init) or (name init step)');
    }

    const [nameExpr, initExpr, stepExpr] = bindingExpr.items;
    if (nameExpr!.kind !== 'symbol') {
      throw new EvalError('do bindings must start with a symbol');
    }

    return {
      name: nameExpr.name,
      init: initExpr!,
      step: stepExpr,
    };
  });
}

function parseDoTestClause(expr: Expr): { test: Expr; results: Expr[] } {
  if (expr.kind !== 'list' || expr.items.length === 0) {
    throw new EvalError('do expects a termination clause');
  }

  const [test, ...results] = expr.items;
  return { test: test!, results };
}

function parseRecordConstructorSpec(expr: Expr): { name: string; fields: string[] } {
  if (expr.kind !== 'list' || expr.items.length === 0) {
    throw new EvalError('define-record-type expects a constructor specification');
  }

  const name = expectSymbolExprName('define-record-type', expr.items[0]!, 'constructor name');
  const fields = expr.items.slice(1).map((fieldExpr) => expectSymbolExprName('define-record-type', fieldExpr, 'constructor field'));

  assertUniqueNames('define-record-type constructor fields', fields);
  return { name, fields };
}

function parseRecordFieldSpec(expr: Expr): RecordFieldSpec {
  if (expr.kind !== 'list' || (expr.items.length !== 2 && expr.items.length !== 3)) {
    throw new EvalError('define-record-type expects field clauses of the form (field accessor) or (field accessor mutator)');
  }

  const [fieldExpr, accessorExpr, mutatorExpr] = expr.items;
  return {
    name: expectSymbolExprName('define-record-type', fieldExpr!, 'field name'),
    accessor: expectSymbolExprName('define-record-type', accessorExpr!, 'field accessor'),
    mutator:
      mutatorExpr === undefined
        ? undefined
        : expectSymbolExprName('define-record-type', mutatorExpr, 'field mutator'),
  };
}

function expectSymbolExprName(formName: string, expr: Expr, role: string): string {
  if (expr.kind !== 'symbol') {
    throw new EvalError(`${formName} expects ${role} to be a symbol`);
  }

  return expr.name;
}

function assertUniqueNames(context: string, names: string[]): void {
  const seen = new Set<string>();
  for (const name of names) {
    if (seen.has(name)) {
      throw new EvalError(`${context} must be unique`);
    }
    seen.add(name);
  }
}

function quoteExpr(expr: Expr): Value {
  switch (expr.kind) {
    case 'number':
    case 'boolean':
      return expr.value;

    case 'string':
      return makeRuntimeString(expr.value);

    case 'char':
      return { kind: 'char', value: expr.value };

    case 'symbol':
      return { kind: 'symbol-value', name: expr.name };

    case 'list':
      return makeList(expr.items.map((item) => quoteExpr(item)));
  }
}

function applyProcedure(
  proc: BuiltinProc | UserProc | CaseLambdaProc,
  args: Value[],
  position: SourcePosition,
  macroEnv: MacroEnv,
): EvalResult {
  try {
    if (proc.kind === 'builtin') {
      return proc.apply(args, position);
    }

    if (proc.kind === 'lambda') {
      return applyProcedureClause(proc.name ?? 'lambda', proc.env, { params: proc.params, body: proc.body }, args, macroEnv);
    }

    const clause = proc.clauses.find((candidate) => procedureArityMatches(args, candidate.params));
    if (clause === undefined) {
      throw new EvalError(`${proc.name ?? 'case-lambda'} has no matching clause for ${args.length} argument(s)`);
    }

    return applyProcedureClause(proc.name ?? 'case-lambda', proc.env, clause, args, macroEnv);
  } catch (error) {
    throw attachPosition(error, position);
  }
}

function applyProcedureClause(
  name: string,
  env: Env,
  clause: ProcedureClause,
  args: Value[],
  macroEnv: MacroEnv,
): EvalResult {
  assertProcedureArity(name, args, clause.params);

  const callEnv = new Env(env);
  clause.params.required.forEach((param, index) => {
    callEnv.define(param, args[index]!);
  });
  if (clause.params.rest !== undefined) {
    callEnv.define(clause.params.rest, makeList(args.slice(clause.params.required.length)));
  }

  return evalSequenceTail(clause.body, callEnv, macroEnv);
}

function builtin(name: string, apply: (args: Value[], position: SourcePosition) => EvalResult): [string, BuiltinProc] {
  return [name, { kind: 'builtin', name, apply }];
}

function applyBuiltin(args: Value[], position: SourcePosition, macroEnv: MacroEnv): EvalResult {
  assertAtLeastArity('apply', args, 2);

  const proc = args[0]!;
  if (!isProcedure(proc)) {
    throw new EvalError('apply expects a procedure');
  }

  const prefixArgs = args.slice(1, -1);
  const listArgs = expectProperList('apply', args[args.length - 1]!);
  return applyProcedure(proc, [...prefixArgs, ...listArgs], position, macroEnv);
}

function absoluteValue(args: Value[]): num.NumericValue {
  assertExactArity('abs', args, 1);
  return num.absNumeric(expectNumberValue('abs', args[0]!));
}

function unaryPredicate(name: string, args: Value[], predicate: (value: Value) => boolean): boolean {
  assertExactArity(name, args, 1);
  return predicate(args[0]!);
}

function numberPredicate(name: string, args: Value[], predicate: (value: num.NumericValue) => boolean): boolean {
  assertExactArity(name, args, 1);
  return predicate(expectNumberValue(name, args[0]!));
}

function integerPredicate(name: string, args: Value[], predicate: (value: num.NumericValue) => boolean): boolean {
  assertExactArity(name, args, 1);
  return predicate(expectIntegerValue(name, args[0]!));
}

function compareChars(
  name: string,
  args: Value[],
  compare: (left: number, right: number) => boolean,
): boolean {
  assertAtLeastArity(name, args, 2);
  const codePoints = args.map((arg) => {
    const value = expectCharValue(name, arg).value.codePointAt(0);
    if (value === undefined) {
      throw new EvalError(`${name} expects valid characters`);
    }
    return value;
  });

  for (let index = 1; index < codePoints.length; index += 1) {
    if (!compare(codePoints[index - 1]!, codePoints[index]!)) {
      return false;
    }
  }

  return true;
}

function compareStrings(
  name: string,
  args: Value[],
  normalize: (value: string) => string,
  compare: (left: string, right: string) => boolean,
): boolean {
  assertAtLeastArity(name, args, 2);
  const values = args.map((arg) => normalize(expectStringValue(name, arg)));

  for (let index = 1; index < values.length; index += 1) {
    if (!compare(values[index - 1]!, values[index]!)) {
      return false;
    }
  }

  return true;
}

function sumNumbers(name: string, args: Value[]): num.NumericValue {
  return num.sumNumeric(expectNumbers(name, args));
}

function productNumbers(name: string, args: Value[]): num.NumericValue {
  return num.productNumeric(expectNumbers(name, args));
}

function subtractNumbers(args: Value[]): num.NumericValue {
  const numbers = expectNumbers('-', args);
  assertAtLeastArity('-', numbers, 1);
  return num.subtractNumeric(numbers);
}

function divideNumbers(args: Value[]): num.NumericValue {
  const numbers = expectNumbers('/', args);
  assertAtLeastArity('/', numbers, 1);
  return num.divideNumeric(numbers);
}

function compareNumbers(
  name: string,
  args: Value[],
  compare: (left: num.NumericValue, right: num.NumericValue) => boolean,
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

function quotientNumbers(args: Value[]): num.NumericValue {
  assertExactArity('quotient', args, 2);
  return num.quotientNumeric(
    expectIntegerValue('quotient', args[0]!),
    expectIntegerValue('quotient', args[1]!),
  );
}

function remainderNumbers(args: Value[]): num.NumericValue {
  assertExactArity('remainder', args, 2);
  return num.remainderNumeric(
    expectIntegerValue('remainder', args[0]!),
    expectIntegerValue('remainder', args[1]!),
  );
}

function moduloNumbers(args: Value[]): num.NumericValue {
  assertExactArity('modulo', args, 2);
  return num.moduloNumeric(
    expectIntegerValue('modulo', args[0]!),
    expectIntegerValue('modulo', args[1]!),
  );
}

function extremum(
  name: string,
  args: Value[],
  select: (left: num.NumericValue, right: num.NumericValue) => num.NumericValue,
): num.NumericValue {
  const numbers = expectNumbers(name, args);
  assertAtLeastArity(name, numbers, 1);

  let result = numbers[0]!;
  for (const value of numbers.slice(1)) {
    result = select(result, value);
  }

  return result;
}

function exptNumbers(args: Value[]): num.NumericValue {
  assertExactArity('expt', args, 2);
  return num.exptNumeric(expectNumberValue('expt', args[0]!), expectIntegerValue('expt', args[1]!));
}

function makeVectorBuiltin(args: Value[]): VectorValue {
  if (args.length !== 1 && args.length !== 2) {
    throw new EvalError('make-vector expects 1 or 2 argument(s)');
  }

  const length = expectIndex('make-vector', args[0]!);
  const fill = args.length === 2 ? args[1]! : VOID;
  return { kind: 'vector', items: Array.from({ length }, () => fill) };
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

function expectVectorValue(name: string, value: Value): VectorValue {
  if (!isVectorValue(value)) {
    throw new EvalError(`${name} expects a vector`);
  }

  return value;
}

function expectRecordValue(name: string, value: Value, recordType?: RecordTypeDescriptor): RecordValue {
  if (!isRecordValue(value)) {
    throw new EvalError(`${name} expects a record`);
  }

  if (recordType !== undefined && value.recordType !== recordType) {
    throw new EvalError(`${name} expects a ${recordType.name} record`);
  }

  return value;
}

function expectProperList(name: string, value: Value): Value[] {
  const items: Value[] = [];
  let current = value;
  const seen = new Set<PairValue>();

  while (isPair(current)) {
    if (seen.has(current)) {
      throw new EvalError(`${name} expects a proper list`);
    }
    seen.add(current);
    items.push(current.car);
    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${name} expects a proper list`);
  }

  return items;
}

function expectNumbers(name: string, args: Value[]): num.NumericValue[] {
  return args.map((arg) => {
    if (!num.isNumericValue(arg)) {
      throw new EvalError(`${name} expects number arguments`);
    }
    return arg;
  });
}

function expectNumberValue(name: string, value: Value): num.NumericValue {
  if (!num.isNumericValue(value)) {
    throw new EvalError(`${name} expects a number`);
  }

  return value;
}

function expectIntegerValue(name: string, value: Value): num.NumericValue {
  const numericValue = expectNumberValue(name, value);
  if (!num.numericIsInteger(numericValue)) {
    throw new EvalError(`${name} expects an integer`);
  }

  return numericValue;
}

function expectStringValue(name: string, value: Value): string {
  if (typeof value === 'string') {
    return value;
  }

  if (isMutableStringValue(value)) {
    return value.chars.join('');
  }

  throw new EvalError(`${name} expects a string`);
}

function expectMutableStringValue(name: string, value: Value): MutableStringValue {
  if (!isMutableStringValue(value)) {
    throw new EvalError(`${name} expects a mutable string`);
  }

  return value;
}

function expectCharValue(name: string, value: Value): CharValue {
  if (!isCharValue(value)) {
    throw new EvalError(`${name} expects a character`);
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
  const index = expectIntegerValue(name, value);
  const indexValue = num.numericToNumber(index);
  if (indexValue < 0) {
    throw new EvalError(`${name} expects a non-negative integer index`);
  }

  return indexValue;
}

function expectCodePoint(name: string, value: Value): number {
  const numericValue = expectIntegerValue(name, value);
  const codePoint = num.numericToNumber(numericValue);

  if (!Number.isSafeInteger(codePoint) || codePoint < 0 || codePoint > 0x10ffff) {
    throw new EvalError(`${name} expects a valid character code`);
  }

  return codePoint;
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

function assertProcedureArity(name: string, args: readonly unknown[], params: ParamSpec): void {
  if (params.rest === undefined) {
    assertExactArity(name, args, params.required.length);
    return;
  }

  assertAtLeastArity(name, args, params.required.length);
}

function procedureArityMatches(args: readonly unknown[], params: ParamSpec): boolean {
  if (params.rest === undefined) {
    return args.length === params.required.length;
  }

  return args.length >= params.required.length;
}

function listRefBuiltin(args: Value[]): Value {
  assertExactArity('list-ref', args, 2);
  const tail = listTailValue('list-ref', args[0]!, expectIndex('list-ref', args[1]!));
  if (!isPair(tail)) {
    throw new EvalError('list-ref index out of bounds');
  }

  return tail.car;
}

function listTailBuiltin(args: Value[]): Value {
  assertExactArity('list-tail', args, 2);
  return listTailValue('list-tail', args[0]!, expectIndex('list-tail', args[1]!));
}

function listTailValue(name: string, list: Value, index: number): Value {
  let current = list;

  for (let offset = 0; offset < index; offset += 1) {
    if (isEmptyList(current)) {
      throw new EvalError(`${name} index out of bounds`);
    }
    if (!isPair(current)) {
      throw new EvalError(`${name} expects a proper list`);
    }
    current = current.cdr;
  }

  ensureProperList(name, current);
  return current;
}

function ensureProperList(name: string, value: Value): void {
  let current = value;
  const seen = new Set<PairValue>();

  while (isPair(current)) {
    if (seen.has(current)) {
      throw new EvalError(`${name} expects a proper list`);
    }
    seen.add(current);
    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${name} expects a proper list`);
  }
}

function isProperListValue(value: Value): boolean {
  let current = value;
  const seen = new Set<PairValue>();

  while (isPair(current)) {
    if (seen.has(current)) {
      return false;
    }
    seen.add(current);
    current = current.cdr;
  }

  return isEmptyList(current);
}

function mapBuiltin(args: Value[], position: SourcePosition, macroEnv: MacroEnv): Value {
  assertAtLeastArity('map', args, 2);

  const proc = args[0]!;
  if (!isProcedure(proc)) {
    throw new EvalError('map expects a procedure');
  }

  const currentLists = args.slice(1);
  const seenLists = currentLists.map(() => new Set<PairValue>());
  const results: Value[] = [];

  while (true) {
    let sawEmpty = false;
    let sawPair = false;

    for (const current of currentLists) {
      if (isEmptyList(current)) {
        sawEmpty = true;
        continue;
      }
      if (!isPair(current)) {
        throw new EvalError('map expects proper list arguments');
      }
      sawPair = true;
    }

    if (sawEmpty) {
      if (sawPair) {
        throw new EvalError('map expects lists of equal length');
      }
      return makeList(results);
    }

    const elementArgs: Value[] = [];
    for (let index = 0; index < currentLists.length; index += 1) {
      const current = currentLists[index]!;
      if (!isPair(current)) {
        throw new EvalError('map expects proper list arguments');
      }
      if (seenLists[index]!.has(current)) {
        throw new EvalError('map expects proper list arguments');
      }
      seenLists[index]!.add(current);
      elementArgs.push(current.car);
      currentLists[index] = current.cdr;
    }

    results.push(resolveEvalResult(applyProcedure(proc, elementArgs, position, macroEnv), macroEnv));
  }
}

function forEachBuiltin(args: Value[], position: SourcePosition, macroEnv: MacroEnv): Value {
  assertAtLeastArity('for-each', args, 2);

  const proc = args[0]!;
  if (!isProcedure(proc)) {
    throw new EvalError('for-each expects a procedure');
  }

  const currentLists = args.slice(1);
  const seenLists = currentLists.map(() => new Set<PairValue>());

  while (true) {
    let sawEmpty = false;
    let sawPair = false;

    for (const current of currentLists) {
      if (isEmptyList(current)) {
        sawEmpty = true;
        continue;
      }
      if (!isPair(current)) {
        throw new EvalError('for-each expects proper list arguments');
      }
      sawPair = true;
    }

    if (sawEmpty) {
      if (sawPair) {
        throw new EvalError('for-each expects lists of equal length');
      }
      return VOID;
    }

    const elementArgs: Value[] = [];
    for (let index = 0; index < currentLists.length; index += 1) {
      const current = currentLists[index]!;
      if (!isPair(current)) {
        throw new EvalError('for-each expects proper list arguments');
      }
      if (seenLists[index]!.has(current)) {
        throw new EvalError('for-each expects proper list arguments');
      }
      seenLists[index]!.add(current);
      elementArgs.push(current.car);
      currentLists[index] = current.cdr;
    }

    resolveEvalResult(applyProcedure(proc, elementArgs, position, macroEnv), macroEnv);
  }
}

function memberBuiltin(args: Value[]): Value {
  assertExactArity('member', args, 2);

  const key = args[0]!;
  let current = args[1]!;
  const seen = new Set<PairValue>();

  while (isPair(current)) {
    if (seen.has(current)) {
      throw new EvalError('member expects a proper list');
    }
    seen.add(current);

    if (equalValues(key, current.car)) {
      return current;
    }

    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError('member expects a proper list');
  }

  return false;
}

function assvBuiltin(args: Value[]): Value {
  assertExactArity('assv', args, 2);

  const key = args[0]!;
  let current = args[1]!;
  const seen = new Set<PairValue>();

  while (isPair(current)) {
    if (seen.has(current)) {
      throw new EvalError('assv expects an association list');
    }
    seen.add(current);

    const entry = current.car;
    if (!isPair(entry)) {
      throw new EvalError('assv expects an association list');
    }
    if (eqvValues(key, entry.car)) {
      return entry;
    }

    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError('assv expects an association list');
  }

  return false;
}

function assocBuiltin(args: Value[]): Value {
  assertExactArity('assoc', args, 2);

  const key = args[0]!;
  let current = args[1]!;

  while (isPair(current)) {
    const entry = current.car;
    if (!isPair(entry)) {
      throw new EvalError('assoc expects an association list');
    }
    if (equalValues(key, entry.car)) {
      return entry;
    }
    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError('assoc expects an association list');
  }

  return false;
}

function gcdBuiltin(args: Value[]): num.NumericValue {
  const integers = args.map((arg) => expectIntegerValue('gcd', arg));
  if (integers.length === 0) {
    return makeExactIntegerValue(0n);
  }

  if (integers.every((value) => num.isExactNumeric(value))) {
    let result = 0n;
    for (const value of integers) {
      result = gcdBigInt(result, absBigInt(integerNumericToBigInt(value)));
    }
    return makeExactIntegerValue(result);
  }

  let result = 0;
  for (const value of integers) {
    result = gcdNumber(result, Math.abs(Math.trunc(num.numericToNumber(value))));
  }
  return makeInexactNumberValue(result);
}

function lcmBuiltin(args: Value[]): num.NumericValue {
  const integers = args.map((arg) => expectIntegerValue('lcm', arg));
  if (integers.length === 0) {
    return makeExactIntegerValue(1n);
  }

  if (integers.every((value) => num.isExactNumeric(value))) {
    let result = 1n;
    for (const value of integers) {
      result = lcmBigInt(result, integerNumericToBigInt(value));
    }
    return makeExactIntegerValue(result);
  }

  let result = 1;
  for (const value of integers) {
    result = lcmNumber(result, Math.trunc(num.numericToNumber(value)));
  }
  return makeInexactNumberValue(result);
}

function truncateBuiltin(args: Value[]): num.NumericValue {
  assertExactArity('truncate', args, 1);
  return integerizingNumericResult(expectNumberValue('truncate', args[0]!), Math.trunc);
}

function roundBuiltin(args: Value[]): num.NumericValue {
  assertExactArity('round', args, 1);
  return integerizingNumericResult(expectNumberValue('round', args[0]!), Math.round);
}

function makeStringBuiltin(args: Value[]): string | MutableStringValue {
  if (args.length !== 1 && args.length !== 2) {
    throw new EvalError('make-string expects 1 or 2 argument(s)');
  }

  const length = expectIndex('make-string', args[0]!);
  const fill = args[1] === undefined ? '\0' : expectCharValue('make-string', args[1]).value;
  return makeRuntimeString(fill.repeat(length));
}

function stringBuiltin(args: Value[]): string | MutableStringValue {
  return makeRuntimeString(args.map((arg) => expectCharValue('string', arg).value).join(''));
}

function integerizingNumericResult(
  value: num.NumericValue,
  transform: (value: number) => number,
): num.NumericValue {
  if (value.kind === 'exact-integer') {
    return value;
  }

  const transformed = transform(num.numericToNumber(value));
  return value.kind === 'inexact-number'
    ? makeInexactNumberValue(transformed)
    : makeExactIntegerValue(BigInt(transformed));
}

function integerNumericToBigInt(value: num.NumericValue): bigint {
  return value.kind === 'exact-integer' ? value.value : BigInt(Math.trunc(num.numericToNumber(value)));
}

function makeExactIntegerValue(value: bigint): num.NumericValue {
  return { kind: 'exact-integer', value };
}

function makeInexactNumberValue(value: number): num.NumericValue {
  return { kind: 'inexact-number', value: Object.is(value, -0) ? 0 : value };
}

function absBigInt(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function gcdBigInt(left: bigint, right: bigint): bigint {
  let a = absBigInt(left);
  let b = absBigInt(right);

  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }

  return a;
}

function lcmBigInt(left: bigint, right: bigint): bigint {
  if (left === 0n || right === 0n) {
    return 0n;
  }

  return absBigInt((left / gcdBigInt(left, right)) * right);
}

function gcdNumber(left: number, right: number): number {
  let a = Math.abs(left);
  let b = Math.abs(right);

  while (b !== 0) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }

  return a;
}

function lcmNumber(left: number, right: number): number {
  if (left === 0 || right === 0) {
    return 0;
  }

  return Math.abs((left / gcdNumber(left, right)) * right);
}

function formatValue(value: Value): string {
  return formatValueWithMode(value, 'write', { activePairs: new Set(), activeVectors: new Set() });
}

function formatDisplayValue(value: Value): string {
  return formatValueWithMode(value, 'display', { activePairs: new Set(), activeVectors: new Set() });
}

function formatValueWithMode(value: Value, mode: 'display' | 'write', state: FormatState): string {
  if (num.isNumericValue(value)) {
    return num.formatNumber(value);
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  if (typeof value === 'string') {
    return mode === 'display' ? value : JSON.stringify(value);
  }

  if (isMutableStringValue(value)) {
    const contents = value.chars.join('');
    return mode === 'display' ? contents : JSON.stringify(contents);
  }

  switch (value.kind) {
    case 'char':
      return mode === 'display' ? value.value : formatCharLiteral(value.value);
    case 'symbol-value':
      return value.name;
    case 'empty-list':
      return '()';
    case 'pair':
      return `(${formatPairContents(value, mode, state)})`;
    case 'vector':
      return formatVectorValue(value, mode, state);
    case 'record':
      return `#<record:${value.recordType.name}>`;
    case 'void':
      return '';
    case 'builtin':
      return `#<procedure:${value.name}>`;
    case 'lambda':
      return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
    case 'case-lambda':
      return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
  }
}

function formatPairContents(pair: PairValue, mode: 'display' | 'write', state: FormatState): string {
  if (state.activePairs.has(pair)) {
    return '#<circular>';
  }

  state.activePairs.add(pair);
  try {
    const head = formatValueWithMode(pair.car, mode, state);

    if (isEmptyList(pair.cdr)) {
      return head;
    }

    if (isPair(pair.cdr)) {
      return `${head} ${formatPairContents(pair.cdr, mode, state)}`;
    }

    return `${head} . ${formatValueWithMode(pair.cdr, mode, state)}`;
  } finally {
    state.activePairs.delete(pair);
  }
}

function formatVectorValue(vector: VectorValue, mode: 'display' | 'write', state: FormatState): string {
  if (state.activeVectors.has(vector)) {
    return '#<circular>';
  }

  state.activeVectors.add(vector);
  try {
    return `#(${vector.items.map((item) => formatValueWithMode(item, mode, state)).join(' ')})`;
  } finally {
    state.activeVectors.delete(vector);
  }
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

function stringChars(value: string): string[] {
  return Array.from(value);
}

function currentBenchLevel(): number | undefined {
  const rawLevel = (globalThis as { process?: { env?: Record<string, string | undefined> } }).process?.env?.BENCH_LEVEL;
  if (rawLevel === undefined) {
    return undefined;
  }

  const level = Number.parseInt(rawLevel, 10);
  return Number.isNaN(level) ? undefined : level;
}

function stringsAreImmutable(): boolean {
  const level = currentBenchLevel();
  return level === undefined || level >= STRING_IMMUTABILITY_LEVEL;
}

function makeMutableString(value: string): MutableStringValue {
  return { kind: 'mutable-string', chars: stringChars(value) };
}

function makeRuntimeString(value: string): string | MutableStringValue {
  return stringsAreImmutable() ? value : makeMutableString(value);
}

function stringValueText(value: string | MutableStringValue): string {
  return typeof value === 'string' ? value : value.chars.join('');
}

function parseCharToken(token: string, position: SourcePosition): Expr {
  const rawValue = token.slice(2);
  if (rawValue.length === 0) {
    throw new EvalError('invalid character literal', position);
  }

  switch (rawValue.toLowerCase()) {
    case 'space':
      return { kind: 'char', value: ' ', position };
    case 'newline':
      return { kind: 'char', value: '\n', position };
  }

  const chars = Array.from(rawValue);
  if (chars.length !== 1) {
    throw new EvalError('invalid character literal', position);
  }

  return { kind: 'char', value: chars[0]!, position };
}

function eqValues(left: Value, right: Value): boolean {
  if (num.isNumericValue(left) && num.isNumericValue(right)) {
    return num.numericEqual(left, right);
  }

  if (typeof left === 'boolean') {
    return typeof right === 'boolean' && left === right;
  }

  if (isStringValue(left) && isStringValue(right)) {
    return stringValueText(left) === stringValueText(right);
  }

  if (isCharValue(left) && isCharValue(right)) {
    return left.value === right.value;
  }

  if (isSymbolValue(left) && isSymbolValue(right)) {
    return left.name === right.name;
  }

  if (isEmptyList(left) || isEmptyList(right)) {
    return isEmptyList(left) && isEmptyList(right);
  }

  return left === right;
}

function eqvValues(left: Value, right: Value): boolean {
  return eqValues(left, right);
}

function equalValues(left: Value, right: Value): boolean {
  return equalValuesWithState(left, right, { seenPairs: new Map(), seenVectors: new Map() });
}

function equalValuesWithState(left: Value, right: Value, state: EqualityState): boolean {
  if (eqvValues(left, right)) {
    return true;
  }

  if (isStringValue(left) && isStringValue(right)) {
    return stringValueText(left) === stringValueText(right);
  }

  if (isPair(left) && isPair(right)) {
    if (rememberComparison(state.seenPairs, left, right)) {
      return true;
    }

    return equalValuesWithState(left.car, right.car, state) && equalValuesWithState(left.cdr, right.cdr, state);
  }

  if (isVectorValue(left) && isVectorValue(right)) {
    if (left.items.length !== right.items.length) {
      return false;
    }

    if (rememberComparison(state.seenVectors, left, right)) {
      return true;
    }

    for (let index = 0; index < left.items.length; index += 1) {
      if (!equalValuesWithState(left.items[index]!, right.items[index]!, state)) {
        return false;
      }
    }

    return true;
  }

  return false;
}

function rememberComparison<T extends object>(seen: Map<T, Set<T>>, left: T, right: T): boolean {
  let rights = seen.get(left);
  if (rights === undefined) {
    rights = new Set<T>();
    seen.set(left, rights);
  } else if (rights.has(right)) {
    return true;
  }

  rights.add(right);
  return false;
}

function isAlphabeticChar(value: string): boolean {
  return value.toLowerCase() !== value.toUpperCase();
}

function isNumericChar(value: string): boolean {
  return /^[0-9]$/u.test(value);
}

function isTruthy(value: Value): boolean {
  return value !== false;
}

function isProcedure(value: Value): value is BuiltinProc | UserProc | CaseLambdaProc {
  return (
    typeof value === 'object' &&
    value !== null &&
    (value.kind === 'builtin' || value.kind === 'lambda' || value.kind === 'case-lambda')
  );
}

function isPair(value: Value): value is PairValue {
  return typeof value === 'object' && value !== null && value.kind === 'pair';
}

function isRecordValue(value: Value): value is RecordValue {
  return typeof value === 'object' && value !== null && value.kind === 'record';
}

function isVectorValue(value: Value): value is VectorValue {
  return typeof value === 'object' && value !== null && value.kind === 'vector';
}

function isCharValue(value: Value): value is CharValue {
  return typeof value === 'object' && value !== null && value.kind === 'char';
}

function isMutableStringValue(value: Value): value is MutableStringValue {
  return typeof value === 'object' && value !== null && value.kind === 'mutable-string';
}

function isStringValue(value: Value): value is string | MutableStringValue {
  return typeof value === 'string' || isMutableStringValue(value);
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
