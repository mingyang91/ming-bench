import { EvalError, type SourceLocation } from './evalError.js';

type Located<T> = T & { location: SourceLocation };

interface ExactSchemeNumber {
  exactness: 'exact';
  numerator: number;
  denominator: number;
}

interface InexactSchemeNumber {
  exactness: 'inexact';
  value: number;
}

type SchemeNumber = ExactSchemeNumber | InexactSchemeNumber;

type SymbolExpr = Located<{
  kind: 'symbol';
  value: string;
  displayName?: string;
  capturedBinding?: Binding;
  capturedMacro?: MacroTransformer;
  templateId?: number;
}>;

type ListExpr = Located<{ kind: 'list'; items: Expr[] }>;

type Expr =
  | Located<{ kind: 'number'; value: SchemeNumber }>
  | Located<{ kind: 'boolean'; value: boolean }>
  | Located<{ kind: 'string'; value: string }>
  | Located<{ kind: 'char'; value: string }>
  | SymbolExpr
  | ListExpr;

type Token =
  | Located<{ kind: 'number'; value: SchemeNumber }>
  | Located<{ kind: 'boolean'; value: boolean }>
  | Located<{ kind: 'string'; value: string }>
  | Located<{ kind: 'char'; value: string }>
  | Located<{ kind: 'symbol'; value: string }>
  | Located<{ kind: 'paren'; value: '(' | ')' }>
  | Located<{ kind: 'quote' }>;

interface NumberValue {
  kind: 'number';
  value: SchemeNumber;
}

interface BooleanValue {
  kind: 'boolean';
  value: boolean;
}

interface StringValue {
  kind: 'string';
  value: string;
  mutable: boolean;
}

interface CharacterValue {
  kind: 'char';
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

interface VectorValue {
  kind: 'vector';
  elements: SchemeValue[];
}

interface RecordTypeDefinition {
  id: number;
  name: string;
  fieldNames: string[];
}

interface RecordValue {
  kind: 'record';
  recordType: RecordTypeDefinition;
  fields: SchemeValue[];
}

interface VoidValue {
  kind: 'void';
}

type SchemeValue =
  | NumberValue
  | BooleanValue
  | StringValue
  | CharacterValue
  | SymbolValue
  | EmptyListValue
  | PairValue
  | VectorValue
  | RecordValue
  | VoidValue
  | ProcedureValue;

type ProcedureValue =
  | BuiltinProcedureValue
  | ClosureProcedureValue
  | CaseLambdaProcedureValue;

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
  params: SymbolExpr[];
  restParam?: SymbolExpr;
  body: Expr[];
  env: Environment;
}

interface CaseLambdaClause {
  params: SymbolExpr[];
  restParam?: SymbolExpr;
  body: Expr[];
}

interface CaseLambdaProcedureValue {
  kind: 'procedure';
  procedureKind: 'case-lambda';
  name: string;
  clauses: CaseLambdaClause[];
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
  name: SymbolExpr;
  valueExpression: Expr;
}

interface DoBindingSpec {
  name: SymbolExpr;
  initExpression: Expr;
  stepExpression?: Expr;
}

interface RecordConstructorSpec {
  name: SymbolExpr;
  fields: SymbolExpr[];
}

interface RecordFieldSpec {
  name: SymbolExpr;
  accessor: SymbolExpr;
  mutator?: SymbolExpr;
}

interface TokenizationResult {
  tokens: Token[];
  eofLocation: SourceLocation;
}

interface SyntaxRule {
  pattern: ListExpr;
  template: Expr;
  patternVariables: Set<string>;
}

interface MacroTransformer {
  name: string;
  literals: Set<string>;
  rules: SyntaxRule[];
  env: Environment;
}

interface PatternMatch {
  singles: Map<string, Expr>;
  repeats: Map<string, Expr[]>;
}

class Environment {
  private readonly bindings = new Map<string, Binding>();
  private readonly macros = new Map<string, MacroTransformer>();

  constructor(private readonly parent?: Environment) {}

  define(name: string, value: SchemeValue): Binding {
    const binding = { value };
    this.bindings.set(name, binding);
    return binding;
  }

  defineBinding(name: string, binding: Binding): void {
    this.bindings.set(name, binding);
  }

  defineMacro(name: string, transformer: MacroTransformer): void {
    this.macros.set(name, transformer);
  }

  lookup(name: string): SchemeValue | undefined {
    return this.lookupBinding(name)?.value;
  }

  lookupMacro(name: string): MacroTransformer | undefined {
    return this.macros.get(name) ?? this.parent?.lookupMacro(name);
  }

  set(name: string, value: SchemeValue): boolean {
    const binding = this.lookupBinding(name);
    if (binding === undefined) {
      return false;
    }

    binding.value = value;
    return true;
  }

  child(): Environment {
    return new Environment(this);
  }

  lookupBinding(name: string): Binding | undefined {
    return this.bindings.get(name) ?? this.parent?.lookupBinding(name);
  }
}

const TRUE_VALUE: BooleanValue = { kind: 'boolean', value: true };
const FALSE_VALUE: BooleanValue = { kind: 'boolean', value: false };
const EMPTY_LIST_VALUE: EmptyListValue = { kind: 'empty-list' };
const VOID_VALUE: VoidValue = { kind: 'void' };
const SPECIAL_FORM_NAMES = new Set([
  'and',
  'case',
  'or',
  'begin',
  'case-lambda',
  'cond',
  'define',
  'define-record-type',
  'define-syntax',
  'do',
  'if',
  'lambda',
  'let',
  'letrec',
  'letrec*',
  'quote',
  'set!',
]);

let nextTemplateId = 1;
let nextIntroducedIdentifierId = 1;
let nextRecordTypeId = 1;

/**
 * Evaluate one or more Scheme expressions and return the string
 * representation of the last result.
 */
export function evalStr(input: string): string {
  return evaluateProgram(input).result;
}

/**
 * Evaluate Scheme expressions and return both the result string
 * and any captured output from display/write/newline.
 */
export function evalStrWithOutput(input: string): { result: string; output: string } {
  return evaluateProgram(input);
}

function evaluateProgram(input: string): { result: string; output: string } {
  const expressions = parseProgram(input);
  if (expressions.length === 0) {
    throw new EvalError('empty input', { line: 1, column: 1 });
  }

  const output: string[] = [];
  const env = createGlobalEnvironment(output);
  const result = evaluateSequence(expressions, env);
  return { result: formatValue(result), output: output.join('') };
}

function createGlobalEnvironment(output: string[]): Environment {
  const env = new Environment();

  env.define('+', makeBuiltinProcedure('+', (args) => numberValue(sum(asNumbers(args, '+')))));
  env.define('-', makeBuiltinProcedure('-', (args) => numberValue(subtract(asNumbers(args, '-')))));
  env.define('*', makeBuiltinProcedure('*', (args) => numberValue(product(asNumbers(args, '*')))));
  env.define('/', makeBuiltinProcedure('/', (args) => numberValue(divide(asNumbers(args, '/')))));
  env.define(
    '<',
    makeBuiltinProcedure('<', (args) =>
      booleanValue(compareChain(asNumbers(args, '<'), (comparison) => comparison < 0, '<')),
    ),
  );
  env.define(
    '>',
    makeBuiltinProcedure('>', (args) =>
      booleanValue(compareChain(asNumbers(args, '>'), (comparison) => comparison > 0, '>')),
    ),
  );
  env.define(
    '=',
    makeBuiltinProcedure('=', (args) =>
      booleanValue(compareChain(asNumbers(args, '='), (comparison) => comparison === 0, '=')),
    ),
  );
  env.define(
    '<=',
    makeBuiltinProcedure('<=', (args) =>
      booleanValue(compareChain(asNumbers(args, '<='), (comparison) => comparison <= 0, '<=')),
    ),
  );
  env.define('not', makeBuiltinProcedure('not', (args) => applyNot(args)));
  env.define('apply', makeBuiltinProcedure('apply', (args) => applyApply(args)));
  env.define('eq?', makeBuiltinProcedure('eq?', (args) => applyEq(args)));
  env.define('eqv?', makeBuiltinProcedure('eqv?', (args) => applyEqv(args)));
  env.define('equal?', makeBuiltinProcedure('equal?', (args) => applyEqual(args)));
  env.define(
    'procedure?',
    makePredicateProcedure('procedure?', (value) => value.kind === 'procedure'),
  );

  env.define('abs', makeBuiltinProcedure('abs', (args) => applyAbs(args)));
  env.define('quotient', makeBuiltinProcedure('quotient', (args) => applyQuotient(args)));
  env.define('remainder', makeBuiltinProcedure('remainder', (args) => applyRemainder(args)));
  env.define('modulo', makeBuiltinProcedure('modulo', (args) => applyModulo(args)));
  env.define('min', makeBuiltinProcedure('min', (args) => applyMin(args)));
  env.define('max', makeBuiltinProcedure('max', (args) => applyMax(args)));
  env.define('expt', makeBuiltinProcedure('expt', (args) => applyExpt(args)));

  env.define('zero?', makePredicateProcedure('zero?', (value) => compareNumbers(expectNumber(value, 'zero?'), exactNumber(0)) === 0));
  env.define(
    'positive?',
    makePredicateProcedure('positive?', (value) => compareNumbers(expectNumber(value, 'positive?'), exactNumber(0)) > 0),
  );
  env.define(
    'negative?',
    makePredicateProcedure('negative?', (value) => compareNumbers(expectNumber(value, 'negative?'), exactNumber(0)) < 0),
  );
  env.define('odd?', makeBuiltinProcedure('odd?', (args) => applyOddPredicate(args)));
  env.define('even?', makeBuiltinProcedure('even?', (args) => applyEvenPredicate(args)));

  env.define('cons', makeBuiltinProcedure('cons', (args) => applyCons(args)));
  env.define('car', makeBuiltinProcedure('car', (args) => applyCar(args)));
  env.define('cdr', makeBuiltinProcedure('cdr', (args) => applyCdr(args)));
  env.define('null?', makeBuiltinProcedure('null?', (args) => applyNullPredicate(args)));
  env.define('list', makeBuiltinProcedure('list', (args) => arrayToList(args)));
  env.define('length', makeBuiltinProcedure('length', (args) => applyLength(args)));
  env.define('list-ref', makeBuiltinProcedure('list-ref', (args) => applyListRef(args)));
  env.define('list-tail', makeBuiltinProcedure('list-tail', (args) => applyListTail(args)));
  env.define('append', makeBuiltinProcedure('append', (args) => applyAppend(args)));
  env.define('assoc', makeBuiltinProcedure('assoc', (args) => applyAssoc(args)));
  env.define('map', makeBuiltinProcedure('map', (args) => applyMap(args)));
  env.define('vector', makeBuiltinProcedure('vector', (args) => applyVector(args)));
  env.define('make-vector', makeBuiltinProcedure('make-vector', (args) => applyMakeVector(args)));
  env.define('vector-ref', makeBuiltinProcedure('vector-ref', (args) => applyVectorRef(args)));
  env.define('vector-set!', makeBuiltinProcedure('vector-set!', (args) => applyVectorSet(args)));
  env.define('vector-length', makeBuiltinProcedure('vector-length', (args) => applyVectorLength(args)));
  env.define('vector->list', makeBuiltinProcedure('vector->list', (args) => applyVectorToList(args)));
  env.define('list->vector', makeBuiltinProcedure('list->vector', (args) => applyListToVector(args)));
  env.define('display', makeBuiltinProcedure('display', (args) => applyDisplay(args, output)));
  env.define('write', makeBuiltinProcedure('write', (args) => applyWrite(args, output)));
  env.define('newline', makeBuiltinProcedure('newline', (args) => applyNewline(args, output)));
  env.define('string-append', makeBuiltinProcedure('string-append', (args) => applyStringAppend(args)));
  env.define('string-length', makeBuiltinProcedure('string-length', (args) => applyStringLength(args)));
  env.define('string-copy', makeBuiltinProcedure('string-copy', (args) => applyStringCopy(args)));
  env.define('substring', makeBuiltinProcedure('substring', (args) => applySubstring(args)));
  env.define('string-set!', makeBuiltinProcedure('string-set!', (args) => applyStringSet(args)));
  env.define('string->number', makeBuiltinProcedure('string->number', (args) => applyStringToNumber(args)));
  env.define('number->string', makeBuiltinProcedure('number->string', (args) => applyNumberToString(args)));
  env.define('symbol->string', makeBuiltinProcedure('symbol->string', (args) => applySymbolToString(args)));
  env.define('string->symbol', makeBuiltinProcedure('string->symbol', (args) => applyStringToSymbol(args)));
  env.define('string-ref', makeBuiltinProcedure('string-ref', (args) => applyStringRef(args)));
  env.define('string=?', makeBuiltinProcedure('string=?', (args) => applyStringEqual(args)));
  env.define('string<?', makeBuiltinProcedure('string<?', (args) => applyStringLess(args)));
  env.define('string-ci=?', makeBuiltinProcedure('string-ci=?', (args) => applyStringCiEqual(args)));
  env.define('string-upcase', makeBuiltinProcedure('string-upcase', (args) => applyStringUpcase(args)));
  env.define('string-downcase', makeBuiltinProcedure('string-downcase', (args) => applyStringDowncase(args)));

  env.define(
    'char-alphabetic?',
    makeBuiltinProcedure('char-alphabetic?', (args) => applyCharAlphabeticPredicate(args)),
  );
  env.define(
    'char-numeric?',
    makeBuiltinProcedure('char-numeric?', (args) => applyCharNumericPredicate(args)),
  );
  env.define('char-upcase', makeBuiltinProcedure('char-upcase', (args) => applyCharUpcase(args)));
  env.define('char-downcase', makeBuiltinProcedure('char-downcase', (args) => applyCharDowncase(args)));
  env.define('char=?', makeBuiltinProcedure('char=?', (args) => applyCharEqual(args)));
  env.define('char<?', makeBuiltinProcedure('char<?', (args) => applyCharLess(args)));

  env.define('string?', makePredicateProcedure('string?', (value) => value.kind === 'string'));
  env.define('number?', makePredicateProcedure('number?', (value) => value.kind === 'number'));
  env.define(
    'integer?',
    makePredicateProcedure('integer?', (value) => value.kind === 'number' && isIntegerNumber(value.value)),
  );
  env.define(
    'rational?',
    makePredicateProcedure('rational?', (value) => value.kind === 'number' && isRationalNumber(value.value)),
  );
  env.define(
    'exact?',
    makePredicateProcedure('exact?', (value) => value.kind === 'number' && value.value.exactness === 'exact'),
  );
  env.define(
    'inexact?',
    makePredicateProcedure('inexact?', (value) => value.kind === 'number' && value.value.exactness === 'inexact'),
  );
  env.define('boolean?', makePredicateProcedure('boolean?', (value) => value.kind === 'boolean'));
  env.define('pair?', makePredicateProcedure('pair?', (value) => value.kind === 'pair'));
  env.define('symbol?', makePredicateProcedure('symbol?', (value) => value.kind === 'symbol'));
  env.define('list?', makePredicateProcedure('list?', (value) => isProperList(value)));
  env.define('vector?', makePredicateProcedure('vector?', (value) => value.kind === 'vector'));
  env.define('char?', makePredicateProcedure('char?', (value) => value.kind === 'char'));
  env.define(
    'exact->inexact',
    makeBuiltinProcedure('exact->inexact', (args) => applyExactToInexact(args)),
  );
  env.define(
    'inexact->exact',
    makeBuiltinProcedure('inexact->exact', (args) => applyInexactToExact(args)),
  );
  env.define('numerator', makeBuiltinProcedure('numerator', (args) => applyNumerator(args)));
  env.define('denominator', makeBuiltinProcedure('denominator', (args) => applyDenominator(args)));

  return env;
}

function displayIdentifierName(identifier: SymbolExpr): string {
  return identifier.displayName ?? identifier.value;
}

function defineIdentifier(identifier: SymbolExpr, env: Environment, value: SchemeValue): Binding {
  return env.define(identifier.value, value);
}

function defineIdentifierBinding(identifier: SymbolExpr, env: Environment, binding: Binding): void {
  env.defineBinding(identifier.value, binding);
}

function lookupIdentifierValue(identifier: SymbolExpr, env: Environment): SchemeValue | undefined {
  return identifier.capturedBinding?.value ?? env.lookup(identifier.value);
}

function setIdentifierValue(identifier: SymbolExpr, env: Environment, value: SchemeValue): boolean {
  if (identifier.capturedBinding !== undefined) {
    identifier.capturedBinding.value = value;
    return true;
  }

  return env.set(identifier.value, value);
}

function lookupMacroTransformer(
  identifier: SymbolExpr,
  env: Environment,
): MacroTransformer | undefined {
  return identifier.capturedMacro ?? env.lookupMacro(identifier.value);
}

function evaluateDefineSyntax(items: Expr[], env: Environment): SchemeValue {
  expectExactExprCount('define-syntax', items, 2);

  const name = items[0];
  if (name.kind !== 'symbol') {
    throw new EvalError('define-syntax requires a symbol name');
  }

  const transformer = parseSyntaxRules(name, items[1], env);
  env.defineMacro(name.value, transformer);
  return VOID_VALUE;
}

function evaluateDefineRecordType(items: Expr[], env: Environment): SchemeValue {
  expectAtLeastArgCount('define-record-type', items, 3);

  const typeName = expectSymbolExpression(
    items[0],
    'define-record-type requires a symbol record name',
  );
  const constructor = parseRecordConstructorSpec(items[1]);
  const predicate = expectSymbolExpression(
    items[2],
    'define-record-type requires a predicate name',
  );
  const fieldSpecs = items.slice(3).map((item) => parseRecordFieldSpec(item));
  const fieldIndexes = buildRecordFieldIndexMap(fieldSpecs);
  const constructorFieldIndexes = constructor.fields.map((field) => {
    const fieldIndex = fieldIndexes.get(displayIdentifierName(field));
    if (fieldIndex === undefined) {
      throw new EvalError(
        `constructor references unknown record field: ${displayIdentifierName(field)}`,
        field.location,
      );
    }
    return fieldIndex;
  });

  const recordType: RecordTypeDefinition = {
    id: nextRecordTypeId,
    name: displayIdentifierName(typeName),
    fieldNames: fieldSpecs.map((field) => displayIdentifierName(field.name)),
  };
  nextRecordTypeId += 1;

  defineIdentifier(
    constructor.name,
    env,
    makeRecordConstructorProcedure(constructor, fieldSpecs.length, constructorFieldIndexes, recordType),
  );
  defineIdentifier(
    predicate,
    env,
    makeRecordPredicateProcedure(predicate, recordType),
  );

  for (let index = 0; index < fieldSpecs.length; index += 1) {
    const field = fieldSpecs[index];
    defineIdentifier(field.accessor, env, makeRecordAccessorProcedure(field.accessor, recordType, index));

    if (field.mutator !== undefined) {
      defineIdentifier(field.mutator, env, makeRecordMutatorProcedure(field.mutator, recordType, index));
    }
  }

  return VOID_VALUE;
}

function parseRecordConstructorSpec(expression: Expr): RecordConstructorSpec {
  if (expression.kind !== 'list' || expression.items.length === 0) {
    throw new EvalError('define-record-type constructor spec must be a non-empty list', expression.location);
  }

  return {
    name: expectSymbolExpression(
      expression.items[0],
      'define-record-type constructor name must be a symbol',
    ),
    fields: expression.items
      .slice(1)
      .map((item) =>
        expectSymbolExpression(item, 'define-record-type constructor fields must be symbols'),
      ),
  };
}

function parseRecordFieldSpec(expression: Expr): RecordFieldSpec {
  if (
    expression.kind !== 'list' ||
    (expression.items.length !== 2 && expression.items.length !== 3)
  ) {
    throw new EvalError(
      'define-record-type field specs must contain a field name, accessor, and optional mutator',
      expression.location,
    );
  }

  return {
    name: expectSymbolExpression(
      expression.items[0],
      'define-record-type field names must be symbols',
    ),
    accessor: expectSymbolExpression(
      expression.items[1],
      'define-record-type accessor names must be symbols',
    ),
    mutator:
      expression.items[2] === undefined
        ? undefined
        : expectSymbolExpression(
            expression.items[2],
            'define-record-type mutator names must be symbols',
          ),
  };
}

function buildRecordFieldIndexMap(fieldSpecs: RecordFieldSpec[]): Map<string, number> {
  const fieldIndexes = new Map<string, number>();

  for (let index = 0; index < fieldSpecs.length; index += 1) {
    const fieldName = displayIdentifierName(fieldSpecs[index].name);
    if (fieldIndexes.has(fieldName)) {
      throw new EvalError(
        `duplicate record field: ${fieldName}`,
        fieldSpecs[index].name.location,
      );
    }
    fieldIndexes.set(fieldName, index);
  }

  return fieldIndexes;
}

function makeRecordConstructorProcedure(
  constructor: RecordConstructorSpec,
  fieldCount: number,
  constructorFieldIndexes: number[],
  recordType: RecordTypeDefinition,
): BuiltinProcedureValue {
  const name = displayIdentifierName(constructor.name);
  return makeBuiltinProcedure(name, (args) => {
    expectExactArgCount(name, args, constructorFieldIndexes.length);

    const fields = Array.from({ length: fieldCount }, () => VOID_VALUE as SchemeValue);
    for (let index = 0; index < constructorFieldIndexes.length; index += 1) {
      fields[constructorFieldIndexes[index]] = args[index];
    }

    return recordValue(recordType, fields);
  });
}

function makeRecordPredicateProcedure(
  predicate: SymbolExpr,
  recordType: RecordTypeDefinition,
): BuiltinProcedureValue {
  const name = displayIdentifierName(predicate);
  return makeBuiltinProcedure(name, (args) => {
    expectExactArgCount(name, args, 1);
    return booleanValue(args[0].kind === 'record' && args[0].recordType.id === recordType.id);
  });
}

function makeRecordAccessorProcedure(
  accessor: SymbolExpr,
  recordType: RecordTypeDefinition,
  fieldIndex: number,
): BuiltinProcedureValue {
  const name = displayIdentifierName(accessor);
  return makeBuiltinProcedure(name, (args) => {
    expectExactArgCount(name, args, 1);
    return expectRecord(args[0], name, recordType).fields[fieldIndex];
  });
}

function makeRecordMutatorProcedure(
  mutator: SymbolExpr,
  recordType: RecordTypeDefinition,
  fieldIndex: number,
): BuiltinProcedureValue {
  const name = displayIdentifierName(mutator);
  return makeBuiltinProcedure(name, (args) => {
    expectExactArgCount(name, args, 2);
    expectRecord(args[0], name, recordType).fields[fieldIndex] = args[1];
    return VOID_VALUE;
  });
}

function parseSyntaxRules(
  name: SymbolExpr,
  expression: Expr,
  env: Environment,
): MacroTransformer {
  if (expression.kind !== 'list' || expression.items.length < 2) {
    throw new EvalError('define-syntax requires a syntax-rules transformer', expression.location);
  }

  const [head, literalList, ...ruleExpressions] = expression.items;
  if (head.kind !== 'symbol' || head.value !== 'syntax-rules') {
    throw new EvalError('define-syntax requires a syntax-rules transformer', expression.location);
  }

  if (literalList.kind !== 'list') {
    throw new EvalError('syntax-rules literals must be a list', literalList.location);
  }

  const literals = new Set<string>();
  for (const literal of literalList.items) {
    if (literal.kind !== 'symbol') {
      throw new EvalError('syntax-rules literals must be symbols', literal.location);
    }
    literals.add(displayIdentifierName(literal));
  }

  if (ruleExpressions.length === 0) {
    throw new EvalError('syntax-rules requires at least one rule', expression.location);
  }

  return {
    name: displayIdentifierName(name),
    literals,
    rules: ruleExpressions.map((ruleExpression) =>
      parseSyntaxRule(displayIdentifierName(name), ruleExpression, literals),
    ),
    env,
  };
}

function parseSyntaxRule(
  macroName: string,
  expression: Expr,
  literals: Set<string>,
): SyntaxRule {
  if (expression.kind !== 'list' || expression.items.length !== 2) {
    throw new EvalError('syntax-rules clauses must contain a pattern and template', expression.location);
  }

  const pattern = expression.items[0];
  if (pattern.kind !== 'list' || pattern.items.length === 0) {
    throw new EvalError('syntax-rules patterns must be non-empty lists', pattern.location);
  }

  const head = pattern.items[0];
  if (head.kind !== 'symbol' || displayIdentifierName(head) !== macroName) {
    throw new EvalError('syntax-rules pattern must start with the macro name', pattern.location);
  }

  const patternVariables = new Set<string>();
  for (const item of pattern.items.slice(1)) {
    collectPatternVariables(item, literals, patternVariables);
  }

  return {
    pattern,
    template: expression.items[1],
    patternVariables,
  };
}

function collectPatternVariables(
  expression: Expr,
  literals: Set<string>,
  patternVariables: Set<string>,
): void {
  if (expression.kind === 'symbol') {
    const name = displayIdentifierName(expression);
    if (name !== '...' && !literals.has(name)) {
      patternVariables.add(name);
    }
    return;
  }

  if (expression.kind === 'list') {
    for (const item of expression.items) {
      collectPatternVariables(item, literals, patternVariables);
    }
  }
}

function collectRepeatedPatternVariablesFromPattern(
  expression: Expr,
  rule: SyntaxRule,
  names = new Set<string>(),
): Set<string> {
  if (expression.kind === 'symbol') {
    const name = displayIdentifierName(expression);
    if (rule.patternVariables.has(name)) {
      names.add(name);
    }
    return names;
  }

  if (expression.kind === 'list') {
    for (const item of expression.items) {
      if (!isEllipsisMarker(item)) {
        collectRepeatedPatternVariablesFromPattern(item, rule, names);
      }
    }
  }

  return names;
}

function expandMacroCall(
  items: Expr[],
  location: SourceLocation,
  transformer: MacroTransformer,
): Expr {
  const invocation: ListExpr = { kind: 'list', items, location };

  for (const rule of transformer.rules) {
    const match = matchSyntaxRule(rule, invocation);
    if (match === undefined) {
      continue;
    }

    const templateId = nextTemplateId;
    nextTemplateId += 1;

    const expanded = expandTemplate(rule.template, rule, match, templateId);
    resolveTemplateIdentifiers(expanded, transformer.env, templateId);
    return expanded;
  }

  throw new EvalError(`no matching syntax-rules clause for ${transformer.name}`, location);
}

function matchSyntaxRule(rule: SyntaxRule, invocation: ListExpr): PatternMatch | undefined {
  const match: PatternMatch = { singles: new Map(), repeats: new Map() };
  return matchListPattern(rule.pattern.items, invocation.items, rule, match, 0) ? match : undefined;
}

function matchListPattern(
  patternItems: Expr[],
  inputItems: Expr[],
  rule: SyntaxRule,
  match: PatternMatch,
  repeatDepth: number,
  patternIndex = 0,
  inputIndex = 0,
): boolean {
  if (patternIndex === patternItems.length) {
    return inputIndex === inputItems.length;
  }

  const pattern = patternItems[patternIndex];
  if (pattern === undefined) {
    return inputIndex === inputItems.length;
  }

  if (isEllipsisMarker(patternItems[patternIndex + 1])) {
    const repeatedPatternVariables = collectRepeatedPatternVariablesFromPattern(pattern, rule);

    for (let count = inputItems.length - inputIndex; count >= 0; count -= 1) {
      const snapshot = clonePatternMatch(match);
      let matched = true;

      for (let offset = 0; offset < count; offset += 1) {
        if (!matchPattern(pattern, inputItems[inputIndex + offset], rule, snapshot, repeatDepth + 1)) {
          matched = false;
          break;
        }
      }

      if (matched) {
        for (const name of repeatedPatternVariables) {
          if (!snapshot.singles.has(name) && !snapshot.repeats.has(name)) {
            snapshot.repeats.set(name, []);
          }
        }
      }

      if (
        matched &&
        matchListPattern(
          patternItems,
          inputItems,
          rule,
          snapshot,
          repeatDepth,
          patternIndex + 2,
          inputIndex + count,
        )
      ) {
        match.singles = snapshot.singles;
        match.repeats = snapshot.repeats;
        return true;
      }
    }

    return false;
  }

  if (inputIndex >= inputItems.length) {
    return false;
  }

  if (!matchPattern(pattern, inputItems[inputIndex], rule, match, repeatDepth)) {
    return false;
  }

  return matchListPattern(
    patternItems,
    inputItems,
    rule,
    match,
    repeatDepth,
    patternIndex + 1,
    inputIndex + 1,
  );
}

function matchPattern(
  pattern: Expr,
  input: Expr | undefined,
  rule: SyntaxRule,
  match: PatternMatch,
  repeatDepth: number,
): boolean {
  if (input === undefined) {
    return false;
  }

  switch (pattern.kind) {
    case 'number':
      return input.kind === 'number' && sameNumberRepresentation(input.value, pattern.value);
    case 'boolean':
    case 'string':
    case 'char':
      return input.kind === pattern.kind && input.value === pattern.value;
    case 'symbol': {
      const name = displayIdentifierName(pattern);
      if (name === '...') {
        return false;
      }

      if (rule.patternVariables.has(name)) {
        return bindPatternVariable(name, input, match, repeatDepth);
      }

      return input.kind === 'symbol' && displayIdentifierName(input) === name;
    }
    case 'list':
      return input.kind === 'list' && matchListPattern(pattern.items, input.items, rule, match, repeatDepth);
  }
}

function bindPatternVariable(
  name: string,
  value: Expr,
  match: PatternMatch,
  repeatDepth: number,
): boolean {
  if (repeatDepth === 0) {
    if (match.repeats.has(name)) {
      return false;
    }

    const existing = match.singles.get(name);
    if (existing === undefined) {
      match.singles.set(name, value);
      return true;
    }

    return syntaxEqual(existing, value);
  }

  if (match.singles.has(name)) {
    return false;
  }

  const repeatedValues = match.repeats.get(name);
  if (repeatedValues === undefined) {
    match.repeats.set(name, [value]);
    return true;
  }

  repeatedValues.push(value);
  return true;
}

function clonePatternMatch(match: PatternMatch): PatternMatch {
  return {
    singles: new Map(match.singles),
    repeats: new Map(
      Array.from(match.repeats, ([name, values]) => [name, values.slice()] as const),
    ),
  };
}

function expandTemplate(
  template: Expr,
  rule: SyntaxRule,
  match: PatternMatch,
  templateId: number,
  repeatIndex?: number,
): Expr {
  switch (template.kind) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return cloneExpr(template);
    case 'symbol': {
      const name = displayIdentifierName(template);
      if (rule.patternVariables.has(name)) {
        return expandPatternVariable(name, match, repeatIndex, template.location);
      }

      return {
        kind: 'symbol',
        value: template.value,
        displayName: template.displayName,
        location: template.location,
        templateId,
      };
    }
    case 'list':
      return {
        kind: 'list',
        items: expandTemplateItems(template.items, rule, match, templateId, repeatIndex),
        location: template.location,
      };
  }
}

function expandTemplateItems(
  items: Expr[],
  rule: SyntaxRule,
  match: PatternMatch,
  templateId: number,
  repeatIndex?: number,
): Expr[] {
  const expanded: Expr[] = [];

  for (let index = 0; index < items.length; index += 1) {
    const item = items[index];
    if (item === undefined) {
      continue;
    }

    if (isEllipsisMarker(items[index + 1])) {
      const repeatCount = determineTemplateRepeatCount(item, rule, match);
      for (let nestedIndex = 0; nestedIndex < repeatCount; nestedIndex += 1) {
        expanded.push(expandTemplate(item, rule, match, templateId, nestedIndex));
      }
      index += 1;
      continue;
    }

    expanded.push(expandTemplate(item, rule, match, templateId, repeatIndex));
  }

  return expanded;
}

function determineTemplateRepeatCount(
  expression: Expr,
  rule: SyntaxRule,
  match: PatternMatch,
): number {
  const repeatedVariables = Array.from(collectRepeatedTemplateVariables(expression, rule, match));
  if (repeatedVariables.length === 0) {
    throw new EvalError('template ellipsis requires a repeated pattern variable', expression.location);
  }

  const counts = repeatedVariables.map((name) => match.repeats.get(name)?.length ?? 0);
  const expected = counts[0];
  if (counts.some((count) => count !== expected)) {
    throw new EvalError('template ellipsis length mismatch', expression.location);
  }

  return expected;
}

function collectRepeatedTemplateVariables(
  expression: Expr,
  rule: SyntaxRule,
  match: PatternMatch,
  names = new Set<string>(),
): Set<string> {
  if (expression.kind === 'symbol') {
    const name = displayIdentifierName(expression);
    if (rule.patternVariables.has(name) && match.repeats.has(name)) {
      names.add(name);
    }
    return names;
  }

  if (expression.kind === 'list') {
    for (const item of expression.items) {
      if (!isEllipsisMarker(item)) {
        collectRepeatedTemplateVariables(item, rule, match, names);
      }
    }
  }

  return names;
}

function expandPatternVariable(
  name: string,
  match: PatternMatch,
  repeatIndex: number | undefined,
  location: SourceLocation,
): Expr {
  const repeated = match.repeats.get(name);
  if (repeated !== undefined) {
    if (repeatIndex === undefined) {
      throw new EvalError(`pattern variable ${name} requires ellipsis in the template`, location);
    }

    const value = repeated[repeatIndex];
    if (value === undefined) {
      throw new EvalError(`pattern variable ${name} expanded with mismatched ellipsis`, location);
    }
    return cloneExpr(value);
  }

  const single = match.singles.get(name);
  if (single === undefined) {
    throw new EvalError(`unbound pattern variable: ${name}`, location);
  }

  return cloneExpr(single);
}

interface TemplateScope {
  parent?: TemplateScope;
  renamed: Map<string, string>;
}

function resolveTemplateIdentifiers(
  expression: Expr,
  env: Environment,
  templateId: number,
  scope?: TemplateScope,
): void {
  if (expression.kind === 'symbol') {
    resolveTemplateSymbol(expression, env, templateId, scope);
    return;
  }

  if (expression.kind !== 'list' || expression.items.length === 0) {
    return;
  }

  const head = expression.items[0];
  if (head?.kind === 'symbol' && head.value === 'quote') {
    resolveTemplateSymbol(head, env, templateId, scope);
    return;
  }

  if (head?.kind === 'symbol' && head.value === 'lambda') {
    resolveLambdaTemplateIdentifiers(expression, env, templateId, scope);
    return;
  }

  if (head?.kind === 'symbol' && head.value === 'case-lambda') {
    resolveCaseLambdaTemplateIdentifiers(expression, env, templateId, scope);
    return;
  }

  if (head?.kind === 'symbol' && head.value === 'let') {
    resolveLetTemplateIdentifiers(expression, env, templateId, scope);
    return;
  }

  for (const item of expression.items) {
    resolveTemplateIdentifiers(item, env, templateId, scope);
  }
}

function resolveTemplateSymbol(
  identifier: SymbolExpr,
  env: Environment,
  templateId: number,
  scope?: TemplateScope,
): void {
  if (
    identifier.templateId !== templateId ||
    identifier.capturedBinding !== undefined ||
    identifier.capturedMacro !== undefined
  ) {
    return;
  }

  const visibleName = displayIdentifierName(identifier);
  const renamed = lookupRenamedIdentifier(scope, visibleName);
  if (renamed !== undefined) {
    if (identifier.displayName === undefined) {
      identifier.displayName = visibleName;
    }
    identifier.value = renamed;
    return;
  }

  if (SPECIAL_FORM_NAMES.has(visibleName)) {
    return;
  }

  const macro = env.lookupMacro(visibleName);
  if (macro !== undefined) {
    identifier.capturedMacro = macro;
    return;
  }

  const binding = env.lookupBinding(visibleName);
  if (binding !== undefined) {
    identifier.capturedBinding = binding;
  }
}

function resolveLambdaTemplateIdentifiers(
  expression: ListExpr,
  env: Environment,
  templateId: number,
  scope?: TemplateScope,
): void {
  const childScope = createTemplateScope(scope);
  const parameterList = expression.items[1];
  if (parameterList !== undefined) {
    renameLambdaParameters(parameterList, templateId, childScope);
  }

  for (const bodyExpression of expression.items.slice(2)) {
    resolveTemplateIdentifiers(bodyExpression, env, templateId, childScope);
  }
}

function resolveCaseLambdaTemplateIdentifiers(
  expression: ListExpr,
  env: Environment,
  templateId: number,
  scope?: TemplateScope,
): void {
  for (const clause of expression.items.slice(1)) {
    if (clause.kind !== 'list' || clause.items.length === 0) {
      resolveTemplateIdentifiers(clause, env, templateId, scope);
      continue;
    }

    const childScope = createTemplateScope(scope);
    renameLambdaParameters(clause.items[0], templateId, childScope);

    for (const bodyExpression of clause.items.slice(1)) {
      resolveTemplateIdentifiers(bodyExpression, env, templateId, childScope);
    }
  }
}

function renameLambdaParameters(
  expression: Expr,
  templateId: number,
  scope: TemplateScope,
): void {
  if (expression.kind === 'symbol') {
    renameIntroducedBinding(expression, templateId, scope);
    return;
  }

  if (expression.kind !== 'list') {
    return;
  }

  for (const item of expression.items) {
    if (item.kind === 'symbol' && item.value !== '.') {
      renameIntroducedBinding(item, templateId, scope);
    }
  }
}

function resolveLetTemplateIdentifiers(
  expression: ListExpr,
  env: Environment,
  templateId: number,
  scope?: TemplateScope,
): void {
  const bodyScope = createTemplateScope(scope);
  let bindingListIndex = 1;
  let bodyIndex = 2;

  const maybeName = expression.items[1];
  if (maybeName?.kind === 'symbol') {
    renameIntroducedBinding(maybeName, templateId, bodyScope);
    bindingListIndex = 2;
    bodyIndex = 3;
  }

  const bindingList = expression.items[bindingListIndex];
  if (bindingList?.kind === 'list') {
    for (const bindingExpression of bindingList.items) {
      if (
        bindingExpression.kind !== 'list' ||
        bindingExpression.items.length !== 2 ||
        bindingExpression.items[0]?.kind !== 'symbol'
      ) {
        resolveTemplateIdentifiers(bindingExpression, env, templateId, scope);
        continue;
      }

      resolveTemplateIdentifiers(bindingExpression.items[1], env, templateId, scope);
      renameIntroducedBinding(bindingExpression.items[0], templateId, bodyScope);
    }
  } else if (bindingList !== undefined) {
    resolveTemplateIdentifiers(bindingList, env, templateId, scope);
  }

  for (const bodyExpression of expression.items.slice(bodyIndex)) {
    resolveTemplateIdentifiers(bodyExpression, env, templateId, bodyScope);
  }
}

function createTemplateScope(parent?: TemplateScope): TemplateScope {
  return { parent, renamed: new Map() };
}

function renameIntroducedBinding(
  identifier: SymbolExpr,
  templateId: number,
  scope: TemplateScope,
): void {
  if (identifier.templateId !== templateId) {
    return;
  }

  const visibleName = displayIdentifierName(identifier);
  const renamed = createFreshIdentifierName(visibleName);
  if (identifier.displayName === undefined) {
    identifier.displayName = visibleName;
  }
  identifier.value = renamed;
  scope.renamed.set(visibleName, renamed);
}

function lookupRenamedIdentifier(
  scope: TemplateScope | undefined,
  name: string,
): string | undefined {
  let current = scope;
  while (current !== undefined) {
    const renamed = current.renamed.get(name);
    if (renamed !== undefined) {
      return renamed;
    }

    current = current.parent;
  }

  return undefined;
}

function createFreshIdentifierName(name: string): string {
  const id = nextIntroducedIdentifierId;
  nextIntroducedIdentifierId += 1;
  return `\u0001macro:${id}:${name}`;
}

function cloneExpr(expression: Expr): Expr {
  switch (expression.kind) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return { ...expression };
    case 'symbol':
      return { ...expression };
    case 'list':
      return {
        kind: 'list',
        items: expression.items.map((item) => cloneExpr(item)),
        location: expression.location,
      };
  }
}

function syntaxEqual(left: Expr, right: Expr): boolean {
  if (left.kind !== right.kind) {
    return false;
  }

  switch (left.kind) {
    case 'number':
      return sameNumberRepresentation(left.value, (right as typeof left).value);
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'symbol':
      return displayIdentifierName(left) === displayIdentifierName(right as SymbolExpr);
    case 'list': {
      const other = right as ListExpr;
      return (
        left.items.length === other.items.length &&
        left.items.every((item, index) => syntaxEqual(item, other.items[index]))
      );
    }
  }
}

function isEllipsisMarker(expression: Expr | undefined): boolean {
  return expression?.kind === 'symbol' && displayIdentifierName(expression) === '...';
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
    case 'char':
      return { kind: 'char', value: token.value, location: token.location };
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
      case 'char':
        return charValue(expression.value);
      case 'symbol': {
        const value = lookupIdentifierValue(expression, env);
        if (value === undefined) {
          throw new EvalError(`unbound variable: ${displayIdentifierName(expression)}`);
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
    const transformer = lookupMacroTransformer(head, env);
    if (transformer !== undefined) {
      return evaluate(expandMacroCall(items, head.location, transformer), env);
    }

    switch (head.value) {
      case 'and':
        return evaluateAnd(items.slice(1), env);
      case 'case':
        return evaluateCase(items.slice(1), env);
      case 'or':
        return evaluateOr(items.slice(1), env);
      case 'begin':
        return evaluateBegin(items.slice(1), env);
      case 'cond':
        return evaluateCond(items.slice(1), env);
      case 'define':
        return evaluateDefine(items.slice(1), env);
      case 'define-record-type':
        return evaluateDefineRecordType(items.slice(1), env);
      case 'define-syntax':
        return evaluateDefineSyntax(items.slice(1), env);
      case 'do':
        return evaluateDo(items.slice(1), env);
      case 'if':
        return evaluateIf(items.slice(1), env);
      case 'case-lambda':
        return evaluateCaseLambda(items.slice(1), env);
      case 'lambda':
        return evaluateLambda(items.slice(1), env);
      case 'let':
        return evaluateLet(items.slice(1), env);
      case 'letrec':
        return evaluateLetrec(items.slice(1), env);
      case 'letrec*':
        return evaluateLetrecStar(items.slice(1), env);
      case 'quote':
        return evaluateQuote(items.slice(1));
      case 'set!':
        return evaluateSet(items.slice(1), env);
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

function evaluateCase(items: Expr[], env: Environment): SchemeValue {
  if (items.length < 1) {
    throw new EvalError('case requires a key and at least one clause');
  }

  const key = evaluate(items[0], env);
  const clauses = items.slice(1);

  for (let index = 0; index < clauses.length; index += 1) {
    const clause = clauses[index];
    if (clause.kind !== 'list' || clause.items.length === 0) {
      throw new EvalError('case clauses must be non-empty lists');
    }

    const [datumsExpression, ...body] = clause.items;
    if (datumsExpression.kind === 'symbol' && datumsExpression.value === 'else') {
      if (index !== clauses.length - 1) {
        throw new EvalError('case else clause must be last');
      }
      return evaluateCaseClauseBody(body, env);
    }

    if (datumsExpression.kind !== 'list') {
      throw new EvalError('case clauses must start with a datum list');
    }

    for (const datum of datumsExpression.items) {
      if (schemeEqv(key, quoteExpression(datum))) {
        return evaluateCaseClauseBody(body, env);
      }
    }
  }

  return VOID_VALUE;
}

function evaluateCaseClauseBody(body: Expr[], env: Environment): SchemeValue {
  if (body.length === 0) {
    return VOID_VALUE;
  }

  return evaluateSequence(body, env);
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

    return defineVariable(target, items[1], env);
  }

  if (target.kind !== 'list' || target.items.length === 0) {
    throw new EvalError('define requires a symbol or parameter list');
  }

  const name = target.items[0];
  if (name.kind !== 'symbol') {
    throw new EvalError('function name must be a symbol');
  }

  const { params, restParam } = readParameterListItems(target.items.slice(1));
  const body = items.slice(1);
  if (body.length === 0) {
    throw new EvalError('function definition requires a body');
  }

  const binding: Binding = { value: VOID_VALUE };
  defineIdentifierBinding(name, env, binding);
  binding.value = makeClosure(displayIdentifierName(name), params, body, env, restParam);
  return VOID_VALUE;
}

function defineVariable(name: SymbolExpr, valueExpression: Expr, env: Environment): SchemeValue {
  if (isLambdaExpression(valueExpression)) {
    const binding: Binding = { value: VOID_VALUE };
    defineIdentifierBinding(name, env, binding);
    binding.value = evaluateNamedLambda(valueExpression, env, displayIdentifierName(name));
    return VOID_VALUE;
  }

  if (isCaseLambdaExpression(valueExpression)) {
    const binding: Binding = { value: VOID_VALUE };
    defineIdentifierBinding(name, env, binding);
    binding.value = evaluateNamedCaseLambda(valueExpression, env, displayIdentifierName(name));
    return VOID_VALUE;
  }

  defineIdentifier(name, env, evaluate(valueExpression, env));
  return VOID_VALUE;
}

function isLambdaExpression(expression: Expr): expression is ListExpr {
  return (
    expression.kind === 'list' &&
    expression.items.length > 0 &&
    expression.items[0].kind === 'symbol' &&
    expression.items[0].value === 'lambda'
  );
}

function isCaseLambdaExpression(expression: Expr): expression is ListExpr {
  return (
    expression.kind === 'list' &&
    expression.items.length > 0 &&
    expression.items[0].kind === 'symbol' &&
    expression.items[0].value === 'case-lambda'
  );
}

function evaluateNamedLambda(
  expression: ListExpr,
  env: Environment,
  name: string,
): ClosureProcedureValue {
  const { params, restParam, body } = parseLambdaParts(expression.items.slice(1));
  return makeClosure(name, params, body, env, restParam);
}

function evaluateNamedCaseLambda(
  expression: ListExpr,
  env: Environment,
  name: string,
): CaseLambdaProcedureValue {
  return makeCaseLambda(name, parseCaseLambdaClauses(expression.items.slice(1)), env);
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
  const { params, restParam, body } = parseLambdaParts(items);
  return makeClosure('lambda', params, body, env, restParam);
}

function evaluateCaseLambda(items: Expr[], env: Environment): SchemeValue {
  return makeCaseLambda('case-lambda', parseCaseLambdaClauses(items), env);
}

function evaluateSet(items: Expr[], env: Environment): SchemeValue {
  if (items.length !== 2) {
    throw new EvalError(`set! expected 2 argument(s), got ${items.length}`);
  }

  const target = items[0];
  if (target.kind !== 'symbol') {
    throw new EvalError('set! requires a symbol');
  }

  const value = evaluate(items[1], env);
  if (!setIdentifierValue(target, env, value)) {
    throw new EvalError(`unbound variable: ${displayIdentifierName(target)}`);
  }

  return VOID_VALUE;
}

function evaluateDo(items: Expr[], env: Environment): SchemeValue {
  if (items.length < 2) {
    throw new EvalError('do requires bindings and a termination clause');
  }

  const bindings = parseDoBindings(items[0]);
  const { testExpression, resultExpressions } = parseDoTerminationClause(items[1]);
  const body = items.slice(2);

  const loopEnv = env.child();
  const runtimeBindings = bindings.map((bindingSpec) => {
    const binding: Binding = { value: VOID_VALUE };
    defineIdentifierBinding(bindingSpec.name, loopEnv, binding);
    return binding;
  });

  const initValues = bindings.map((bindingSpec) => evaluate(bindingSpec.initExpression, env));
  for (let index = 0; index < bindings.length; index += 1) {
    runtimeBindings[index].value = initValues[index];
  }

  while (true) {
    if (isTruthy(evaluate(testExpression, loopEnv))) {
      return resultExpressions.length === 0 ? VOID_VALUE : evaluateSequence(resultExpressions, loopEnv);
    }

    for (const expression of body) {
      evaluate(expression, loopEnv);
    }

    const nextValues = bindings.map((bindingSpec, index) =>
      bindingSpec.stepExpression === undefined
        ? runtimeBindings[index].value
        : evaluate(bindingSpec.stepExpression, loopEnv),
    );

    for (let index = 0; index < bindings.length; index += 1) {
      runtimeBindings[index].value = nextValues[index];
    }
  }
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
    defineIdentifier(bindings[index].name, letEnv, values[index]);
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
  defineIdentifierBinding(nameExpression, letEnv, binding);

  const closure = makeClosure(
    displayIdentifierName(nameExpression),
    bindings.map((bindingSpec) => bindingSpec.name),
    items.slice(2),
    letEnv,
  );
  binding.value = closure;

  return applyProcedure(closure, args);
}

function evaluateLetrec(items: Expr[], env: Environment): SchemeValue {
  return evaluateRecursiveLet('letrec', items, env, false);
}

function evaluateLetrecStar(items: Expr[], env: Environment): SchemeValue {
  return evaluateRecursiveLet('letrec*', items, env, true);
}

function evaluateRecursiveLet(
  name: 'letrec' | 'letrec*',
  items: Expr[],
  env: Environment,
  sequential: boolean,
): SchemeValue {
  if (items.length < 2) {
    throw new EvalError(`${name} requires bindings and a body`);
  }

  const bindings = parseLetBindings(items[0]);
  const letrecEnv = env.child();

  if (sequential) {
    for (const bindingSpec of bindings) {
      const binding: Binding = { value: VOID_VALUE };
      defineIdentifierBinding(bindingSpec.name, letrecEnv, binding);
      binding.value = evaluate(bindingSpec.valueExpression, letrecEnv);
    }
  } else {
    const runtimeBindings = bindings.map((bindingSpec) => {
      const binding: Binding = { value: VOID_VALUE };
      defineIdentifierBinding(bindingSpec.name, letrecEnv, binding);
      return binding;
    });
    const values = bindings.map((bindingSpec) => evaluate(bindingSpec.valueExpression, letrecEnv));
    for (let index = 0; index < runtimeBindings.length; index += 1) {
      runtimeBindings[index].value = values[index];
    }
  }

  return evaluateSequence(items.slice(1), letrecEnv);
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
      name: nameExpression,
      valueExpression: bindingExpression.items[1],
    };
  });
}

function parseDoBindings(expression: Expr): DoBindingSpec[] {
  if (expression.kind !== 'list') {
    throw new EvalError('do bindings must be a list');
  }

  return expression.items.map((bindingExpression) => {
    if (
      bindingExpression.kind !== 'list' ||
      (bindingExpression.items.length !== 2 && bindingExpression.items.length !== 3)
    ) {
      throw new EvalError('do bindings must contain a name, init, and optional step');
    }

    const nameExpression = bindingExpression.items[0];
    if (nameExpression.kind !== 'symbol') {
      throw new EvalError('do binding names must be symbols');
    }

    return {
      name: nameExpression,
      initExpression: bindingExpression.items[1],
      stepExpression: bindingExpression.items[2],
    };
  });
}

function parseDoTerminationClause(
  expression: Expr,
): { testExpression: Expr; resultExpressions: Expr[] } {
  if (expression.kind !== 'list' || expression.items.length === 0) {
    throw new EvalError('do termination clause must be a non-empty list');
  }

  return {
    testExpression: expression.items[0],
    resultExpressions: expression.items.slice(1),
  };
}

function parseLambdaParts(items: Expr[]): { params: SymbolExpr[]; restParam?: SymbolExpr; body: Expr[] } {
  if (items.length < 2) {
    throw new EvalError('lambda requires parameters and a body');
  }

  const { params, restParam } = readParameterList(items[0]);
  const body = items.slice(1);
  return { params, restParam, body };
}

function parseCaseLambdaClauses(items: Expr[]): CaseLambdaClause[] {
  if (items.length === 0) {
    throw new EvalError('case-lambda requires at least one clause');
  }

  return items.map((item) => parseCaseLambdaClause(item));
}

function parseCaseLambdaClause(expression: Expr): CaseLambdaClause {
  if (expression.kind !== 'list' || expression.items.length < 2) {
    throw new EvalError('case-lambda clauses require parameters and a body', expression.location);
  }

  const { params, restParam } = readParameterList(expression.items[0]);
  return { params, restParam, body: expression.items.slice(1) };
}

function readParameterList(expression: Expr): { params: SymbolExpr[]; restParam?: SymbolExpr } {
  if (expression.kind === 'symbol') {
    return { params: [], restParam: readParameterName(expression) };
  }

  if (expression.kind !== 'list') {
    throw new EvalError('lambda parameters must be a list or symbol');
  }

  return readParameterListItems(expression.items);
}

function readParameterListItems(items: Expr[]): { params: SymbolExpr[]; restParam?: SymbolExpr } {
  const dotIndex = items.findIndex((item) => item.kind === 'symbol' && item.value === '.');
  if (dotIndex === -1) {
    return { params: items.map((item) => readParameterName(item)) };
  }

  if (dotIndex === items.length - 1 || items.length !== dotIndex + 2) {
    throw new EvalError('lambda rest parameter must be the final name');
  }

  return {
    params: items.slice(0, dotIndex).map((item) => readParameterName(item)),
    restParam: readParameterName(items[dotIndex + 1]),
  };
}

function readParameterName(item: Expr): SymbolExpr {
  if (item.kind !== 'symbol' || item.value === '.') {
    throw new EvalError('lambda parameters must be symbols');
  }

  return item;
}

function makeClosure(
  name: string,
  params: SymbolExpr[],
  body: Expr[],
  env: Environment,
  restParam?: SymbolExpr,
): ClosureProcedureValue {
  return { kind: 'procedure', procedureKind: 'closure', name, params, restParam, body, env };
}

function makeCaseLambda(
  name: string,
  clauses: CaseLambdaClause[],
  env: Environment,
): CaseLambdaProcedureValue {
  return { kind: 'procedure', procedureKind: 'case-lambda', name, clauses, env };
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
    case 'char':
      return charValue(expression.value);
    case 'symbol':
      return symbolValue(displayIdentifierName(expression));
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

  if (value.procedureKind === 'case-lambda') {
    const clause = selectCaseLambdaClause(value, args.length);
    return applyUserProcedure(
      value.name,
      clause.params,
      clause.body,
      value.env,
      args,
      clause.restParam,
    );
  }

  return applyUserProcedure(value.name, value.params, value.body, value.env, args, value.restParam);
}

function applyUserProcedure(
  name: string,
  params: SymbolExpr[],
  body: Expr[],
  env: Environment,
  args: SchemeValue[],
  restParam?: SymbolExpr,
): SchemeValue {
  if (restParam === undefined) {
    expectExactArgCount(name, args, params.length);
  } else {
    expectAtLeastArgCount(name, args, params.length);
  }

  const callEnv = env.child();
  for (let index = 0; index < params.length; index += 1) {
    defineIdentifier(params[index], callEnv, args[index]);
  }

  if (restParam !== undefined) {
    defineIdentifier(restParam, callEnv, arrayToList(args.slice(params.length)));
  }

  return evaluateSequence(body, callEnv);
}

function selectCaseLambdaClause(
  procedure: CaseLambdaProcedureValue,
  argCount: number,
): CaseLambdaClause {
  for (const clause of procedure.clauses) {
    if (
      clause.restParam === undefined
        ? argCount === clause.params.length
        : argCount >= clause.params.length
    ) {
      return clause;
    }
  }

  throw new EvalError(formatCaseLambdaArityError(procedure.name, procedure.clauses, argCount));
}

function formatCaseLambdaArityError(
  name: string,
  clauses: CaseLambdaClause[],
  actual: number,
): string {
  const expectations = clauses.map((clause) =>
    clause.restParam === undefined ? `${clause.params.length}` : `at least ${clause.params.length}`,
  );
  return `${name} expected ${expectations.join(' or ')} argument(s), got ${actual}`;
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

function applyApply(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('apply', args, 2);

  const procedure = args[0];
  const prefixArgs = args.slice(1, -1);
  const finalArgs = expectList(args[args.length - 1], 'apply');
  return applyProcedure(procedure, [...prefixArgs, ...finalArgs]);
}

function applyEq(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('eq?', args, 2);
  return booleanValue(schemeEq(args[0], args[1]));
}

function applyEqv(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('eqv?', args, 2);
  return booleanValue(schemeEqv(args[0], args[1]));
}

function applyEqual(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('equal?', args, 2);
  return booleanValue(schemeEqual(args[0], args[1]));
}

function applyAbs(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('abs', args, 1);
  return numberValue(absNumber(expectNumber(args[0], 'abs')));
}

function applyQuotient(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('quotient', args, 2);
  const [dividend, divisor] = args.map((arg) => expectInteger(arg, 'quotient'));
  ensureNonZeroDivisor(divisor);
  return exactIntegerValue(Math.trunc(dividend / divisor));
}

function applyRemainder(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('remainder', args, 2);
  const [dividend, divisor] = args.map((arg) => expectInteger(arg, 'remainder'));
  ensureNonZeroDivisor(divisor);
  return exactIntegerValue(dividend % divisor);
}

function applyModulo(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('modulo', args, 2);
  const [dividend, divisor] = args.map((arg) => expectInteger(arg, 'modulo'));
  ensureNonZeroDivisor(divisor);

  const remainder = dividend % divisor;
  if (remainder === 0) {
    return exactIntegerValue(0);
  }

  return exactIntegerValue(
    Math.sign(remainder) === Math.sign(divisor) ? remainder : remainder + divisor,
  );
}

function applyMin(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('min', args, 1);
  return numberValue(minNumber(asNumbers(args, 'min')));
}

function applyMax(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('max', args, 1);
  return numberValue(maxNumber(asNumbers(args, 'max')));
}

function applyExpt(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('expt', args, 2);
  const base = expectNumber(args[0], 'expt');
  const exponent = expectInteger(args[1], 'expt');
  return numberValue(exptNumber(base, exponent));
}

function applyOddPredicate(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('odd?', args, 1);
  return booleanValue(Math.abs(expectInteger(args[0], 'odd?') % 2) === 1);
}

function applyEvenPredicate(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('even?', args, 1);
  return booleanValue(expectInteger(args[0], 'even?') % 2 === 0);
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
  return exactIntegerValue(expectList(args[0], 'length').length);
}

function applyListRef(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('list-ref', args, 2);

  const elements = expectList(args[0], 'list-ref');
  const index = expectIndex(args[1], 'list-ref');
  if (index >= elements.length) {
    throw new EvalError('list-ref index out of range');
  }

  return elements[index];
}

function applyListTail(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('list-tail', args, 2);

  const index = expectIndex(args[1], 'list-tail');
  let current = args[0];
  for (let remaining = index; remaining > 0; remaining -= 1) {
    if (current.kind === 'pair') {
      current = current.cdr;
      continue;
    }

    if (current.kind === 'empty-list') {
      throw new EvalError('list-tail index out of range');
    }

    throw new EvalError('list-tail expected a proper list');
  }

  if (current.kind !== 'pair' && current.kind !== 'empty-list') {
    throw new EvalError('list-tail expected a proper list');
  }

  return current;
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

function applyAssoc(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('assoc', args, 2);

  const target = args[0];
  let current = args[1];
  const visited = new Set<PairValue>();
  while (current.kind === 'pair') {
    if (visited.has(current)) {
      throw new EvalError('assoc expected a proper list');
    }
    visited.add(current);

    const entry = current.car;
    if (entry.kind !== 'pair') {
      throw new EvalError('assoc expected an association list');
    }

    if (schemeEqual(target, entry.car)) {
      return entry;
    }

    current = current.cdr;
  }

  if (current.kind !== 'empty-list') {
    throw new EvalError('assoc expected a proper list');
  }

  return FALSE_VALUE;
}

function applyMap(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('map', args, 2);

  const procedure = args[0];
  const lists = args.slice(1).map((arg) => expectList(arg, 'map'));
  const resultLength = lists.reduce(
    (shortest, list) => Math.min(shortest, list.length),
    Number.POSITIVE_INFINITY,
  );

  const results: SchemeValue[] = [];
  for (let index = 0; index < resultLength; index += 1) {
    results.push(applyProcedure(procedure, lists.map((list) => list[index])));
  }

  return arrayToList(results);
}

function applyVector(args: SchemeValue[]): SchemeValue {
  return vectorValue([...args]);
}

function applyMakeVector(args: SchemeValue[]): SchemeValue {
  if (args.length !== 1 && args.length !== 2) {
    throw new EvalError(`make-vector expected 1 or 2 argument(s), got ${args.length}`);
  }

  const length = expectIndex(args[0], 'make-vector');
  const fill = args[1] ?? VOID_VALUE;
  return vectorValue(Array.from({ length }, () => fill));
}

function applyVectorRef(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('vector-ref', args, 2);

  const vector = expectVector(args[0], 'vector-ref');
  const index = expectIndex(args[1], 'vector-ref');
  if (index >= vector.elements.length) {
    throw new EvalError('vector-ref index out of range');
  }

  return vector.elements[index];
}

function applyVectorSet(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('vector-set!', args, 3);

  const vector = expectVector(args[0], 'vector-set!');
  const index = expectIndex(args[1], 'vector-set!');
  if (index >= vector.elements.length) {
    throw new EvalError('vector-set! index out of range');
  }

  vector.elements[index] = args[2];
  return VOID_VALUE;
}

function applyVectorLength(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('vector-length', args, 1);
  return exactIntegerValue(expectVector(args[0], 'vector-length').elements.length);
}

function applyVectorToList(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('vector->list', args, 1);
  return arrayToList(expectVector(args[0], 'vector->list').elements);
}

function applyListToVector(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('list->vector', args, 1);
  return vectorValue(expectList(args[0], 'list->vector'));
}

function applyDisplay(args: SchemeValue[], output: string[]): SchemeValue {
  expectExactArgCount('display', args, 1);
  output.push(formatDisplayValue(args[0]));
  return VOID_VALUE;
}

function applyWrite(args: SchemeValue[], output: string[]): SchemeValue {
  expectExactArgCount('write', args, 1);
  output.push(formatValue(args[0]));
  return VOID_VALUE;
}

function applyNewline(args: SchemeValue[], output: string[]): SchemeValue {
  expectExactArgCount('newline', args, 0);
  output.push('\n');
  return VOID_VALUE;
}

function applyStringAppend(args: SchemeValue[]): SchemeValue {
  return stringValue(args.map((arg) => expectString(arg, 'string-append')).join(''));
}

function applyStringLength(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string-length', args, 1);
  return exactIntegerValue(readStringChars(expectString(args[0], 'string-length')).length);
}

function applyStringCopy(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string-copy', args, 1);
  return stringValue(expectString(args[0], 'string-copy'), true);
}

function applySubstring(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('substring', args, 3);

  const chars = readStringChars(expectString(args[0], 'substring'));
  const start = expectIndex(args[1], 'substring');
  const end = expectIndex(args[2], 'substring');
  if (start > end || end > chars.length) {
    throw new EvalError('substring index out of range');
  }

  return stringValue(chars.slice(start, end).join(''));
}

function applyStringSet(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string-set!', args, 3);

  const target = expectMutableString(args[0], 'string-set!');
  const index = expectIndex(args[1], 'string-set!');
  const value = expectChar(args[2], 'string-set!');
  const chars = readStringChars(target.value);
  if (index >= chars.length) {
    throw new EvalError('string-set! index out of range');
  }

  chars[index] = value;
  target.value = chars.join('');
  return VOID_VALUE;
}

function applyStringToNumber(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string->number', args, 1);

  const parsed = parseStringToNumber(expectString(args[0], 'string->number'));
  if (parsed === undefined) {
    return FALSE_VALUE;
  }

  return numberValue(parsed);
}

function applyNumberToString(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('number->string', args, 1);
  return stringValue(formatNumber(expectNumber(args[0], 'number->string')));
}

function applyExactToInexact(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('exact->inexact', args, 1);
  return numberValue(exactToInexact(expectNumber(args[0], 'exact->inexact')));
}

function applyInexactToExact(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('inexact->exact', args, 1);
  return numberValue(inexactToExact(expectNumber(args[0], 'inexact->exact')));
}

function applyNumerator(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('numerator', args, 1);
  const exact = toExactNumber(expectNumber(args[0], 'numerator'));
  return exactIntegerValue(exact.numerator);
}

function applyDenominator(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('denominator', args, 1);
  const exact = toExactNumber(expectNumber(args[0], 'denominator'));
  return exactIntegerValue(exact.denominator);
}

function applySymbolToString(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('symbol->string', args, 1);
  return stringValue(expectSymbol(args[0], 'symbol->string'));
}

function applyStringToSymbol(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string->symbol', args, 1);
  return symbolValue(expectString(args[0], 'string->symbol'));
}

function applyStringRef(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string-ref', args, 2);

  const chars = readStringChars(expectString(args[0], 'string-ref'));
  const index = expectIndex(args[1], 'string-ref');
  if (index >= chars.length) {
    throw new EvalError('string-ref index out of range');
  }

  return charValue(chars[index]);
}

function applyStringEqual(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('string=?', args, 2);
  return booleanValue(compareStringChain(args, (left, right) => compareStrings(left, right) === 0, 'string=?'));
}

function applyStringLess(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('string<?', args, 2);
  return booleanValue(compareStringChain(args, (left, right) => compareStrings(left, right) < 0, 'string<?'));
}

function applyStringCiEqual(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('string-ci=?', args, 2);
  return booleanValue(
    compareStringChain(
      args,
      (left, right) => compareStrings(left.toLowerCase(), right.toLowerCase()) === 0,
      'string-ci=?',
    ),
  );
}

function applyStringUpcase(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string-upcase', args, 1);
  return stringValue(expectString(args[0], 'string-upcase').toUpperCase());
}

function applyStringDowncase(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('string-downcase', args, 1);
  return stringValue(expectString(args[0], 'string-downcase').toLowerCase());
}

function applyCharAlphabeticPredicate(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('char-alphabetic?', args, 1);
  return booleanValue(/^\p{L}$/u.test(expectChar(args[0], 'char-alphabetic?')));
}

function applyCharNumericPredicate(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('char-numeric?', args, 1);
  return booleanValue(/^\p{Nd}$/u.test(expectChar(args[0], 'char-numeric?')));
}

function applyCharUpcase(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('char-upcase', args, 1);
  return charValue(expectChar(args[0], 'char-upcase').toUpperCase());
}

function applyCharDowncase(args: SchemeValue[]): SchemeValue {
  expectExactArgCount('char-downcase', args, 1);
  return charValue(expectChar(args[0], 'char-downcase').toLowerCase());
}

function applyCharEqual(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('char=?', args, 2);
  return booleanValue(compareCharChain(args, (left, right) => left === right, 'char=?'));
}

function applyCharLess(args: SchemeValue[]): SchemeValue {
  expectAtLeastArgCount('char<?', args, 2);
  return booleanValue(compareCharChain(args, (left, right) => left < right, 'char<?'));
}

function asNumbers(args: SchemeValue[], name: string): SchemeNumber[] {
  return args.map((arg) => expectNumber(arg, name));
}

function expectNumber(value: SchemeValue, name: string): SchemeNumber {
  if (value.kind !== 'number') {
    throw new EvalError(`${name} expected numbers`);
  }

  return value.value;
}

function expectInteger(value: SchemeValue, name: string): number {
  const number = expectNumber(value, name);
  if (!isIntegerNumber(number)) {
    throw new EvalError(`${name} expected an integer`);
  }

  return numberToJs(number);
}

function expectString(value: SchemeValue, name: string): string {
  if (value.kind !== 'string') {
    throw new EvalError(`${name} expected a string`);
  }

  return value.value;
}

function expectMutableString(value: SchemeValue, name: string): StringValue {
  if (value.kind !== 'string') {
    throw new EvalError(`${name} expected a string`);
  }

  if (!value.mutable) {
    throw new EvalError(`${name} expected a mutable string`);
  }

  return value;
}

function expectChar(value: SchemeValue, name: string): string {
  if (value.kind !== 'char') {
    throw new EvalError(`${name} expected a character`);
  }

  return value.value;
}

function expectRecord(
  value: SchemeValue,
  name: string,
  recordType: RecordTypeDefinition,
): RecordValue {
  if (value.kind !== 'record' || value.recordType.id !== recordType.id) {
    throw new EvalError(`${name} expected a ${recordType.name} record`);
  }

  return value;
}

function expectSymbol(value: SchemeValue, name: string): string {
  if (value.kind !== 'symbol') {
    throw new EvalError(`${name} expected a symbol`);
  }

  return value.value;
}

function expectPair(value: SchemeValue, name: string): PairValue {
  if (value.kind !== 'pair') {
    throw new EvalError(`${name} expected a pair`);
  }

  return value;
}

function expectVector(value: SchemeValue, name: string): VectorValue {
  if (value.kind !== 'vector') {
    throw new EvalError(`${name} expected a vector`);
  }

  return value;
}

function expectIndex(value: SchemeValue, name: string): number {
  const number = expectNumber(value, name);
  if (!isIntegerNumber(number) || compareNumbers(number, exactNumber(0)) < 0) {
    throw new EvalError(`${name} expected a non-negative integer`);
  }

  return numberToJs(number);
}

function expectList(value: SchemeValue, name: string): SchemeValue[] {
  const elements: SchemeValue[] = [];
  let current = value;
  const visited = new Set<PairValue>();

  while (current.kind === 'pair') {
    if (visited.has(current)) {
      throw new EvalError(`${name} expected a proper list`);
    }
    visited.add(current);

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

function sum(values: SchemeNumber[]): SchemeNumber {
  return values.reduce((total, value) => addNumbers(total, value), exactNumber(0));
}

function ensureNonZeroDivisor(value: number): void {
  if (value === 0) {
    throw new EvalError('division by zero');
  }
}

function subtract(values: SchemeNumber[]): SchemeNumber {
  expectAtLeastArgCount('-', values, 1);
  if (values.length === 1) {
    return negateNumber(values[0]);
  }

  return values.slice(1).reduce((total, value) => subtractNumbers(total, value), values[0]);
}

function product(values: SchemeNumber[]): SchemeNumber {
  return values.reduce((total, value) => multiplyNumbers(total, value), exactNumber(1));
}

function divide(values: SchemeNumber[]): SchemeNumber {
  expectAtLeastArgCount('/', values, 1);

  if (values.length === 1) {
    return divideNumbers(exactNumber(1), values[0]);
  }

  let result = values[0];
  for (const value of values.slice(1)) {
    result = divideNumbers(result, value);
  }

  return result;
}

function compareChain(
  values: SchemeNumber[],
  predicate: (comparison: number) => boolean,
  name: string,
): boolean {
  expectAtLeastArgCount(name, values, 2);

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(compareNumbers(values[index], values[index + 1]))) {
      return false;
    }
  }

  return true;
}

function compareCharChain(
  args: SchemeValue[],
  predicate: (left: number, right: number) => boolean,
  name: string,
): boolean {
  const values = args.map((arg) => charCodePoint(expectChar(arg, name)));
  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function compareStringChain(
  args: SchemeValue[],
  predicate: (left: string, right: string) => boolean,
  name: string,
): boolean {
  const values = args.map((arg) => expectString(arg, name));
  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
      return false;
    }
  }

  return true;
}

function compareStrings(left: string, right: string): number {
  const leftChars = readStringChars(left);
  const rightChars = readStringChars(right);
  const sharedLength = Math.min(leftChars.length, rightChars.length);
  for (let index = 0; index < sharedLength; index += 1) {
    const difference = charCodePoint(leftChars[index]) - charCodePoint(rightChars[index]);
    if (difference !== 0) {
      return difference;
    }
  }

  return leftChars.length - rightChars.length;
}

function formatValue(value: SchemeValue): string {
  switch (value.kind) {
    case 'number':
      return formatNumber(value.value);
    case 'boolean':
      return value.value ? '#t' : '#f';
    case 'string':
      return JSON.stringify(value.value);
    case 'char':
      return formatChar(value.value);
    case 'symbol':
      return value.value;
    case 'empty-list':
      return '()';
    case 'pair':
      return formatPair(value);
    case 'vector':
      return formatVector(value);
    case 'record':
      return `#<record:${value.recordType.name}>`;
    case 'void':
      return '#<void>';
    case 'procedure':
      return `#<procedure:${value.name}>`;
  }
}

function formatDisplayValue(value: SchemeValue): string {
  switch (value.kind) {
    case 'string':
      return value.value;
    case 'char':
      return value.value;
    case 'pair':
      return formatDisplayPair(value);
    case 'vector':
      return formatVector(value);
    default:
      return formatValue(value);
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

function formatDisplayPair(pair: PairValue): string {
  const parts: string[] = [];
  let current: SchemeValue = pair;

  while (current.kind === 'pair') {
    parts.push(formatDisplayValue(current.car));
    current = current.cdr;
  }

  if (current.kind === 'empty-list') {
    return `(${parts.join(' ')})`;
  }

  return `(${parts.join(' ')} . ${formatDisplayValue(current)})`;
}

function formatVector(vector: VectorValue): string {
  return `#(${vector.elements.map((element) => formatValue(element)).join(' ')})`;
}

function formatNumber(value: SchemeNumber): string {
  if (value.exactness === 'exact') {
    if (value.denominator === 1) {
      return String(value.numerator);
    }

    return `${value.numerator}/${value.denominator}`;
  }

  if (Object.is(value.value, -0)) {
    return '0.0';
  }

  return Number.isInteger(value.value) ? `${Math.trunc(value.value)}.0` : String(value.value);
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

function charCodePoint(value: string): number {
  return value.codePointAt(0) ?? 0;
}

function isTruthy(value: SchemeValue): boolean {
  return value.kind !== 'boolean' || value.value;
}

function isProperList(value: SchemeValue): boolean {
  let current = value;
  const visited = new Set<PairValue>();
  while (current.kind === 'pair') {
    if (visited.has(current)) {
      return false;
    }
    visited.add(current);
    current = current.cdr;
  }

  return current.kind === 'empty-list';
}

function schemeEq(left: SchemeValue, right: SchemeValue): boolean {
  return schemeEqv(left, right);
}

function schemeEqv(left: SchemeValue, right: SchemeValue): boolean {
  if (left.kind !== right.kind) {
    return false;
  }

  switch (left.kind) {
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'boolean':
    case 'char':
    case 'symbol':
      return left.value === (right as typeof left).value;
    case 'empty-list':
    case 'void':
      return true;
    case 'string':
    case 'pair':
    case 'vector':
    case 'record':
    case 'procedure':
      return left === right;
  }
}

function schemeEqual(left: SchemeValue, right: SchemeValue): boolean {
  if (left.kind !== right.kind) {
    return false;
  }

  switch (left.kind) {
    case 'number':
      return compareNumbers(left.value, (right as typeof left).value) === 0;
    case 'boolean':
    case 'char':
    case 'symbol':
      return left.value === (right as typeof left).value;
    case 'string':
      return left.value === (right as StringValue).value;
    case 'empty-list':
    case 'void':
      return true;
    case 'pair':
      return (
        schemeEqual(left.car, (right as PairValue).car) &&
        schemeEqual(left.cdr, (right as PairValue).cdr)
      );
    case 'vector': {
      const rightVector = right as VectorValue;
      return (
        left.elements.length === rightVector.elements.length &&
        left.elements.every((element, index) => schemeEqual(element, rightVector.elements[index]))
      );
    }
    case 'record':
    case 'procedure':
      return left === right;
  }
}

function numberValue(value: SchemeNumber): NumberValue {
  return { kind: 'number', value };
}

function exactIntegerValue(value: number): NumberValue {
  return numberValue(exactNumber(value));
}

function booleanValue(value: boolean): BooleanValue {
  return value ? TRUE_VALUE : FALSE_VALUE;
}

function stringValue(value: string, mutable = false): StringValue {
  return { kind: 'string', value, mutable };
}

function charValue(value: string): CharacterValue {
  return { kind: 'char', value };
}

function symbolValue(value: string): SymbolValue {
  return { kind: 'symbol', value };
}

function pairValue(car: SchemeValue, cdr: SchemeValue): PairValue {
  return { kind: 'pair', car, cdr };
}

function vectorValue(elements: SchemeValue[]): VectorValue {
  return { kind: 'vector', elements };
}

function recordValue(recordType: RecordTypeDefinition, fields: SchemeValue[]): RecordValue {
  return { kind: 'record', recordType, fields };
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

function readStringChars(value: string): string[] {
  return Array.from(value);
}

function parseStringToNumber(value: string): SchemeNumber | undefined {
  return parseNumericLiteral(value);
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

  if (raw.startsWith('#\\')) {
    return { kind: 'char', value: parseCharLiteral(raw, location), location };
  }

  const number = parseNumericLiteral(raw);
  if (number !== undefined) {
    return { kind: 'number', value: number, location };
  }

  return { kind: 'symbol', value: raw, location };
}

function parseCharLiteral(raw: string, location: SourceLocation): string {
  const literal = raw.slice(2);
  if (literal === 'space') {
    return ' ';
  }

  if (literal === 'newline') {
    return '\n';
  }

  const chars = Array.from(literal);
  if (chars.length === 1) {
    return chars[0];
  }

  throw new EvalError('invalid character literal', location);
}

function parseNumericLiteral(raw: string): SchemeNumber | undefined {
  if (raw === '+' || raw === '-' || raw === '.') {
    return undefined;
  }

  if (/^[+-]?\d+$/u.test(raw)) {
    return exactNumber(Number(raw));
  }

  const rationalMatch = raw.match(/^([+-]?\d+)\/(\d+)$/u);
  if (rationalMatch !== null) {
    return exactNumber(Number(rationalMatch[1]), Number(rationalMatch[2]));
  }

  if (/^[+-]?(?:\d+\.\d*|\d*\.\d+)$/u.test(raw)) {
    return inexactNumber(Number(raw));
  }

  return undefined;
}

function exactNumber(numerator: number, denominator = 1): ExactSchemeNumber {
  if (!Number.isInteger(numerator) || !Number.isInteger(denominator)) {
    throw new EvalError('exact numbers require integer components');
  }

  if (denominator === 0) {
    throw new EvalError('division by zero');
  }

  if (numerator === 0) {
    return { exactness: 'exact', numerator: 0, denominator: 1 };
  }

  let normalizedNumerator = numerator;
  let normalizedDenominator = denominator;
  if (normalizedDenominator < 0) {
    normalizedNumerator = -normalizedNumerator;
    normalizedDenominator = -normalizedDenominator;
  }

  const divisor = greatestCommonDivisor(Math.abs(normalizedNumerator), normalizedDenominator);
  return {
    exactness: 'exact',
    numerator: normalizedNumerator / divisor,
    denominator: normalizedDenominator / divisor,
  };
}

function inexactNumber(value: number): InexactSchemeNumber {
  return { exactness: 'inexact', value };
}

function greatestCommonDivisor(left: number, right: number): number {
  let a = Math.abs(left);
  let b = Math.abs(right);

  while (b !== 0) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }

  return a === 0 ? 1 : a;
}

function numberToJs(value: SchemeNumber): number {
  return value.exactness === 'exact' ? value.numerator / value.denominator : value.value;
}

function isIntegerNumber(value: SchemeNumber): boolean {
  return value.exactness === 'exact' ? value.denominator === 1 : Number.isInteger(value.value);
}

function isRationalNumber(value: SchemeNumber): boolean {
  return value.exactness === 'exact' || Number.isFinite(value.value);
}

function toExactNumber(value: SchemeNumber): ExactSchemeNumber {
  if (value.exactness === 'exact') {
    return value;
  }

  if (!Number.isFinite(value.value)) {
    throw new EvalError('expected a finite number');
  }

  const raw = value.value.toString();
  const match = raw.match(/^([+-]?)(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/u);
  if (match === null) {
    throw new EvalError('unable to convert inexact number to exact');
  }

  const sign = match[1] === '-' ? -1 : 1;
  const integerPart = match[2] ?? '0';
  const fractionalPart = match[3] ?? '';
  const exponent = Number(match[4] ?? '0');
  const digits = `${integerPart}${fractionalPart}`.replace(/^0+(?=\d)/u, '');
  let numerator = sign * Number(digits === '' ? '0' : digits);
  let denominator = 10 ** fractionalPart.length;

  if (exponent > 0) {
    numerator *= 10 ** exponent;
  } else if (exponent < 0) {
    denominator *= 10 ** -exponent;
  }

  return exactNumber(numerator, denominator);
}

function exactToInexact(value: SchemeNumber): SchemeNumber {
  if (value.exactness === 'inexact') {
    return value;
  }

  return inexactNumber(value.numerator / value.denominator);
}

function inexactToExact(value: SchemeNumber): SchemeNumber {
  return toExactNumber(value);
}

function addNumbers(left: SchemeNumber, right: SchemeNumber): SchemeNumber {
  if (left.exactness === 'exact' && right.exactness === 'exact') {
    return exactNumber(
      left.numerator * right.denominator + right.numerator * left.denominator,
      left.denominator * right.denominator,
    );
  }

  return inexactNumber(numberToJs(left) + numberToJs(right));
}

function subtractNumbers(left: SchemeNumber, right: SchemeNumber): SchemeNumber {
  return addNumbers(left, negateNumber(right));
}

function negateNumber(value: SchemeNumber): SchemeNumber {
  return value.exactness === 'exact'
    ? exactNumber(-value.numerator, value.denominator)
    : inexactNumber(-value.value);
}

function absNumber(value: SchemeNumber): SchemeNumber {
  return value.exactness === 'exact'
    ? exactNumber(Math.abs(value.numerator), value.denominator)
    : inexactNumber(Math.abs(value.value));
}

function multiplyNumbers(left: SchemeNumber, right: SchemeNumber): SchemeNumber {
  if (left.exactness === 'exact' && right.exactness === 'exact') {
    return exactNumber(left.numerator * right.numerator, left.denominator * right.denominator);
  }

  return inexactNumber(numberToJs(left) * numberToJs(right));
}

function divideNumbers(left: SchemeNumber, right: SchemeNumber): SchemeNumber {
  if (compareNumbers(right, exactNumber(0)) === 0) {
    throw new EvalError('division by zero');
  }

  if (left.exactness === 'exact' && right.exactness === 'exact') {
    return exactNumber(left.numerator * right.denominator, left.denominator * right.numerator);
  }

  return inexactNumber(numberToJs(left) / numberToJs(right));
}

function compareNumbers(left: SchemeNumber, right: SchemeNumber): number {
  if (left.exactness === 'exact' && right.exactness === 'exact') {
    const leftScaled = left.numerator * right.denominator;
    const rightScaled = right.numerator * left.denominator;
    if (leftScaled < rightScaled) {
      return -1;
    }

    if (leftScaled > rightScaled) {
      return 1;
    }

    return 0;
  }

  const leftValue = numberToJs(left);
  const rightValue = numberToJs(right);
  if (leftValue < rightValue) {
    return -1;
  }

  if (leftValue > rightValue) {
    return 1;
  }

  return 0;
}

function sameNumberRepresentation(left: SchemeNumber, right: SchemeNumber): boolean {
  if (left.exactness !== right.exactness) {
    return false;
  }

  if (left.exactness === 'exact') {
    const exactRight = right as ExactSchemeNumber;
    return left.numerator === exactRight.numerator && left.denominator === exactRight.denominator;
  }

  const inexactRight = right as InexactSchemeNumber;
  return Object.is(left.value, inexactRight.value);
}

function minNumber(values: SchemeNumber[]): SchemeNumber {
  let result = values[0];
  for (const value of values.slice(1)) {
    if (compareNumbers(value, result) < 0) {
      result = value;
    }
  }
  return result;
}

function maxNumber(values: SchemeNumber[]): SchemeNumber {
  let result = values[0];
  for (const value of values.slice(1)) {
    if (compareNumbers(value, result) > 0) {
      result = value;
    }
  }
  return result;
}

function exptNumber(base: SchemeNumber, exponent: number): SchemeNumber {
  if (base.exactness === 'exact') {
    if (exponent === 0) {
      return exactNumber(1);
    }

    const magnitude = Math.abs(exponent);
    const numerator = base.numerator ** magnitude;
    const denominator = base.denominator ** magnitude;
    return exponent > 0 ? exactNumber(numerator, denominator) : exactNumber(denominator, numerator);
  }

  return inexactNumber(numberToJs(base) ** exponent);
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

function expectSymbolExpression(expression: Expr, message: string): SymbolExpr {
  if (expression.kind !== 'symbol') {
    throw new EvalError(message, expression.location);
  }

  return expression;
}
