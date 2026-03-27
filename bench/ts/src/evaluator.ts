import { EvalError, type SourcePosition } from './evalError.js';

type Expr =
  | { kind: 'number'; value: NumberValue; pos: SourcePosition }
  | { kind: 'boolean'; value: boolean; pos: SourcePosition }
  | { kind: 'string'; value: string; pos: SourcePosition }
  | { kind: 'char'; value: string; pos: SourcePosition }
  | { kind: 'symbol'; name: string; pos: SourcePosition }
  | { kind: 'resolved-symbol'; name: string; pos: SourcePosition; binding: SymbolBinding }
  | { kind: 'list'; elements: Expr[]; pos: SourcePosition };

type SymbolBinding =
  | { kind: 'lexical'; key: string }
  | { kind: 'captured'; env: Environment };

type BuiltinValue = {
  kind: 'builtin';
  name: string;
  apply: (args: SchemeValue[], pos: SourcePosition) => SchemeValue;
};

type ClosureValue = {
  kind: 'closure';
  params: string[];
  restParam?: string;
  body: Expr[];
  env: Environment;
  name?: string;
};

type ProcedureClause = {
  params: string[];
  restParam?: string;
  body: Expr[];
};

type CaseLambdaValue = {
  kind: 'case-lambda';
  clauses: ProcedureClause[];
  env: Environment;
  name?: string;
};

type SymbolValue = {
  kind: 'symbol';
  name: string;
};

type StringValue = {
  kind: 'string';
  chars: string[];
  mutable: boolean;
};

type PairValue = {
  kind: 'pair';
  car: SchemeValue;
  cdr: SchemeValue;
};

type VectorValue = {
  kind: 'vector';
  elements: SchemeValue[];
};

type RecordFieldDescriptor = {
  name: string;
  accessorName?: string;
  mutatorName?: string;
};

type RecordTypeDescriptor = {
  name: string;
  fields: RecordFieldDescriptor[];
};

type RecordValue = {
  kind: 'record';
  type: RecordTypeDescriptor;
  fields: SchemeValue[];
};

type CharValue = {
  kind: 'char';
  value: string;
};

type ExactNumberValue = {
  kind: 'number';
  exact: true;
  numerator: bigint;
  denominator: bigint;
};

type InexactNumberValue = {
  kind: 'number';
  exact: false;
  value: number;
};

type NumberValue = ExactNumberValue | InexactNumberValue;

type EmptyListValue = {
  kind: 'empty-list';
};

type VoidValue = {
  kind: 'void';
};

type UninitializedValue = {
  kind: 'uninitialized';
  name: string;
};

type EvaluationContext = {
  output: string[];
};

type ProcedureValue = BuiltinValue | ClosureValue | CaseLambdaValue;

type SchemeValue =
  | NumberValue
  | boolean
  | StringValue
  | SymbolValue
  | PairValue
  | VectorValue
  | RecordValue
  | CharValue
  | EmptyListValue
  | VoidValue
  | UninitializedValue
  | ProcedureValue;

type Token =
  | { kind: 'lparen'; pos: SourcePosition }
  | { kind: 'rparen'; pos: SourcePosition }
  | { kind: 'quote'; pos: SourcePosition }
  | { kind: 'number'; value: NumberValue; pos: SourcePosition }
  | { kind: 'boolean'; value: boolean; pos: SourcePosition }
  | { kind: 'string'; value: string; pos: SourcePosition }
  | { kind: 'char'; value: string; pos: SourcePosition }
  | { kind: 'symbol'; value: string; pos: SourcePosition };

type TokenStream = {
  tokens: Token[];
  eofPosition: SourcePosition;
};

type SyntaxRule = {
  pattern: Expr;
  template: Expr;
};

type SyntaxTransformer = {
  keyword: string;
  literals: Set<string>;
  rules: SyntaxRule[];
  definitionEnv: Environment;
};

type PatternBindingValue =
  | { kind: 'expr'; value: Expr }
  | { kind: 'repeated'; value: PatternBindingValue[] };

type PatternBindings = Map<string, PatternBindingValue>;

type RuntimeState = {
  nextHygieneId: number;
};

type DoBinding = {
  name: string;
  initExpr: Expr;
  stepExpr?: Expr;
  pos: SourcePosition;
};

type EvaluationStep =
  | { kind: 'value'; value: SchemeValue }
  | { kind: 'expr'; expression: Expr; env: Environment }
  | { kind: 'apply'; procedure: SchemeValue; args: SchemeValue[]; pos: SourcePosition };

const EMPTY_LIST: EmptyListValue = { kind: 'empty-list' };
const VOID: VoidValue = { kind: 'void' };
const EXACT_ZERO: ExactNumberValue = { kind: 'number', exact: true, numerator: 0n, denominator: 1n };
const EXACT_ONE: ExactNumberValue = { kind: 'number', exact: true, numerator: 1n, denominator: 1n };
const STRING_IMMUTABILITY_LEVEL = 15;
const CURRENT_BENCH_LEVEL = parseBenchLevel();

function createBuiltins(context: EvaluationContext): Map<string, BuiltinValue> {
  return new Map<string, BuiltinValue>([
    ['+', builtin('+', (args, pos) => sum(args, EXACT_ZERO, pos))],
    ['-', builtin('-', (args, pos) => subtract(args, pos))],
    ['*', builtin('*', (args, pos) => product(args, EXACT_ONE, pos))],
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
      '>=',
      builtin('>=', (args, pos) => compareChain('>=', args, (left, right) => left >= right, pos)),
    ],
    [
      'not',
      builtin('not', (args, pos) => {
        expectArity('not', args, 1, pos);
        return isFalse(args[0]);
      }),
    ],
    [
      'eq?',
      builtin('eq?', (args, pos) => {
        expectArity('eq?', args, 2, pos);
        return isEqValue(args[0], args[1]);
      }),
    ],
    [
      'eqv?',
      builtin('eqv?', (args, pos) => {
        expectArity('eqv?', args, 2, pos);
        return isEqValue(args[0], args[1]);
      }),
    ],
    [
      'equal?',
      builtin('equal?', (args, pos) => {
        expectArity('equal?', args, 2, pos);
        return isEqualValue(args[0], args[1]);
      }),
    ],
    [
      'abs',
      builtin('abs', (args, pos) => {
        expectArity('abs', args, 1, pos);
        return absNumber(expectNumber(args[0], 'abs', pos));
      }),
    ],
    ['gcd', builtin('gcd', (args, pos) => gcdNumbers(args, pos))],
    ['lcm', builtin('lcm', (args, pos) => lcmNumbers(args, pos))],
    [
      'modulo',
      builtin('modulo', (args, pos) => {
        expectArity('modulo', args, 2, pos);
        const dividend = expectInteger(args[0], 'modulo', pos);
        const divisor = expectInteger(args[1], 'modulo', pos);
        return modulo(dividend, divisor, pos);
      }),
    ],
    [
      'remainder',
      builtin('remainder', (args, pos) => {
        expectArity('remainder', args, 2, pos);
        const dividend = expectInteger(args[0], 'remainder', pos);
        const divisor = expectInteger(args[1], 'remainder', pos);
        return remainder(dividend, divisor, pos);
      }),
    ],
    [
      'quotient',
      builtin('quotient', (args, pos) => {
        expectArity('quotient', args, 2, pos);
        const dividend = expectInteger(args[0], 'quotient', pos);
        const divisor = expectInteger(args[1], 'quotient', pos);
        return quotient(dividend, divisor, pos);
      }),
    ],
    [
      'min',
      builtin('min', (args, pos) => {
        expectAtLeastArity('min', args, 1, pos);
        const numbers = args.map((arg) => expectNumber(arg, 'min', pos));
        return minOrMax(numbers, 'min');
      }),
    ],
    [
      'max',
      builtin('max', (args, pos) => {
        expectAtLeastArity('max', args, 1, pos);
        const numbers = args.map((arg) => expectNumber(arg, 'max', pos));
        return minOrMax(numbers, 'max');
      }),
    ],
    [
      'expt',
      builtin('expt', (args, pos) => {
        expectArity('expt', args, 2, pos);
        const base = expectNumber(args[0], 'expt', pos);
        const exponent = expectInteger(args[1], 'expt', pos);
        return exptNumber(base, exponent, pos);
      }),
    ],
    [
      'truncate',
      builtin('truncate', (args, pos) => {
        expectArity('truncate', args, 1, pos);
        return truncateNumber(expectNumber(args[0], 'truncate', pos));
      }),
    ],
    [
      'round',
      builtin('round', (args, pos) => {
        expectArity('round', args, 1, pos);
        return roundNumber(expectNumber(args[0], 'round', pos));
      }),
    ],
    [
      'zero?',
      builtin('zero?', (args, pos) => {
        expectArity('zero?', args, 1, pos);
        return isZeroNumber(expectNumber(args[0], 'zero?', pos));
      }),
    ],
    [
      'positive?',
      builtin('positive?', (args, pos) => {
        expectArity('positive?', args, 1, pos);
        return compareNumberValues(expectNumber(args[0], 'positive?', pos), EXACT_ZERO) > 0;
      }),
    ],
    [
      'negative?',
      builtin('negative?', (args, pos) => {
        expectArity('negative?', args, 1, pos);
        return compareNumberValues(expectNumber(args[0], 'negative?', pos), EXACT_ZERO) < 0;
      }),
    ],
    [
      'odd?',
      builtin('odd?', (args, pos) => {
        expectArity('odd?', args, 1, pos);
        return isOddNumber(expectInteger(args[0], 'odd?', pos));
      }),
    ],
    [
      'even?',
      builtin('even?', (args, pos) => {
        expectArity('even?', args, 1, pos);
        return isEvenNumber(expectInteger(args[0], 'even?', pos));
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
    ...createPairAccessors(),
    [
      'set-car!',
      builtin('set-car!', (args, pos) => {
        expectArity('set-car!', args, 2, pos);
        expectPair(args[0], 'set-car!', pos).car = args[1];
        return VOID;
      }),
    ],
    [
      'set-cdr!',
      builtin('set-cdr!', (args, pos) => {
        expectArity('set-cdr!', args, 2, pos);
        expectPair(args[0], 'set-cdr!', pos).cdr = args[1];
        return VOID;
      }),
    ],
    [
      'null?',
      builtin('null?', (args, pos) => {
        expectArity('null?', args, 1, pos);
        return isEmptyList(args[0]);
      }),
    ],
    [
      'list?',
      builtin('list?', (args, pos) => {
        expectArity('list?', args, 1, pos);
        return isProperList(args[0]);
      }),
    ],
    ['list', builtin('list', (args) => listToPairs(args))],
    [
      'list-ref',
      builtin('list-ref', (args, pos) => {
        expectArity('list-ref', args, 2, pos);
        return listRef(args[0], expectIndex(args[1], 'list-ref', pos), pos);
      }),
    ],
    [
      'list-tail',
      builtin('list-tail', (args, pos) => {
        expectArity('list-tail', args, 2, pos);
        return listTail(args[0], expectIndex(args[1], 'list-tail', pos), pos);
      }),
    ],
    [
      'length',
      builtin('length', (args, pos) => {
        expectArity('length', args, 1, pos);
        return makeExactInteger(BigInt(listToArray(args[0], 'length', pos).length));
      }),
    ],
    ['vector', builtin('vector', (args) => makeVector(args))],
    [
      'make-vector',
      builtin('make-vector', (args, pos) => {
        if (args.length !== 1 && args.length !== 2) {
          throw new EvalError(`make-vector expected 1 or 2 argument(s), got ${args.length}`, pos);
        }

        const length = expectIndex(args[0], 'make-vector', pos);
        const fill = args[1] ?? VOID;
        return makeVector(Array.from({ length }, () => fill));
      }),
    ],
    [
      'vector-ref',
      builtin('vector-ref', (args, pos) => {
        expectArity('vector-ref', args, 2, pos);
        const vector = expectVector(args[0], 'vector-ref', pos);
        const index = expectIndex(args[1], 'vector-ref', pos);

        if (index >= vector.elements.length) {
          throw new EvalError('vector-ref index out of bounds', pos);
        }

        return vector.elements[index];
      }),
    ],
    [
      'vector-set!',
      builtin('vector-set!', (args, pos) => {
        expectArity('vector-set!', args, 3, pos);
        const vector = expectVector(args[0], 'vector-set!', pos);
        const index = expectIndex(args[1], 'vector-set!', pos);

        if (index >= vector.elements.length) {
          throw new EvalError('vector-set! index out of bounds', pos);
        }

        vector.elements[index] = args[2];
        return VOID;
      }),
    ],
    [
      'vector-length',
      builtin('vector-length', (args, pos) => {
        expectArity('vector-length', args, 1, pos);
        return makeExactInteger(BigInt(expectVector(args[0], 'vector-length', pos).elements.length));
      }),
    ],
    [
      'vector?',
      builtin('vector?', (args, pos) => {
        expectArity('vector?', args, 1, pos);
        return isVectorValue(args[0]);
      }),
    ],
    [
      'vector->list',
      builtin('vector->list', (args, pos) => {
        expectArity('vector->list', args, 1, pos);
        return listToPairs(expectVector(args[0], 'vector->list', pos).elements);
      }),
    ],
    [
      'list->vector',
      builtin('list->vector', (args, pos) => {
        expectArity('list->vector', args, 1, pos);
        return makeVector(listToArray(args[0], 'list->vector', pos));
      }),
    ],
    ['append', builtin('append', (args, pos) => appendLists(args, pos))],
    [
      'reverse',
      builtin('reverse', (args, pos) => {
        expectArity('reverse', args, 1, pos);
        return listToPairs(listToArray(args[0], 'reverse', pos).slice().reverse());
      }),
    ],
    [
      'assoc',
      builtin('assoc', (args, pos) => {
        expectArity('assoc', args, 2, pos);
        return assoc(args[0], args[1], pos);
      }),
    ],
    [
      'assq',
      builtin('assq', (args, pos) => {
        expectArity('assq', args, 2, pos);
        return assocWithComparator(args[0], args[1], 'assq', pos, isEqValue);
      }),
    ],
    [
      'assv',
      builtin('assv', (args, pos) => {
        expectArity('assv', args, 2, pos);
        return assocWithComparator(args[0], args[1], 'assv', pos, isEqValue);
      }),
    ],
    [
      'memq',
      builtin('memq', (args, pos) => {
        expectArity('memq', args, 2, pos);
        return memberWithComparator(args[0], args[1], 'memq', pos, isEqValue);
      }),
    ],
    [
      'memv',
      builtin('memv', (args, pos) => {
        expectArity('memv', args, 2, pos);
        return memberWithComparator(args[0], args[1], 'memv', pos, isEqValue);
      }),
    ],
    [
      'member',
      builtin('member', (args, pos) => {
        expectArity('member', args, 2, pos);
        return memberWithComparator(args[0], args[1], 'member', pos, isEqualValue);
      }),
    ],
    [
      'map',
      builtin('map', (args, pos) => {
        expectAtLeastArity('map', args, 2, pos);
        return mapLists(args[0], args.slice(1), pos);
      }),
    ],
    [
      'for-each',
      builtin('for-each', (args, pos) => {
        expectAtLeastArity('for-each', args, 2, pos);
        return forEachLists(args[0], args.slice(1), pos);
      }),
    ],
    [
      'string?',
      builtin('string?', (args, pos) => {
        expectArity('string?', args, 1, pos);
        return isStringValue(args[0]);
      }),
    ],
    [
      'number?',
      builtin('number?', (args, pos) => {
        expectArity('number?', args, 1, pos);
        return isNumberValue(args[0]);
      }),
    ],
    [
      'integer?',
      builtin('integer?', (args, pos) => {
        expectArity('integer?', args, 1, pos);
        return isNumberValue(args[0]) && isIntegerNumber(args[0]);
      }),
    ],
    [
      'rational?',
      builtin('rational?', (args, pos) => {
        expectArity('rational?', args, 1, pos);
        return isExactNumberValue(args[0]);
      }),
    ],
    [
      'exact?',
      builtin('exact?', (args, pos) => {
        expectArity('exact?', args, 1, pos);
        return isExactNumberValue(args[0]);
      }),
    ],
    [
      'inexact?',
      builtin('inexact?', (args, pos) => {
        expectArity('inexact?', args, 1, pos);
        return isNumberValue(args[0]) && !args[0].exact;
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
      'procedure?',
      builtin('procedure?', (args, pos) => {
        expectArity('procedure?', args, 1, pos);
        return isProcedureValue(args[0]);
      }),
    ],
    [
      'symbol?',
      builtin('symbol?', (args, pos) => {
        expectArity('symbol?', args, 1, pos);
        return isSymbolValue(args[0]);
      }),
    ],
    [
      'display',
      builtin('display', (args, pos) => {
        expectArity('display', args, 1, pos);
        context.output.push(formatDisplayValue(args[0]));
        return VOID;
      }),
    ],
    [
      'write',
      builtin('write', (args, pos) => {
        expectArity('write', args, 1, pos);
        context.output.push(formatValue(args[0]));
        return VOID;
      }),
    ],
    [
      'newline',
      builtin('newline', (args, pos) => {
        expectArity('newline', args, 0, pos);
        context.output.push('\n');
        return VOID;
      }),
    ],
    [
      'error',
      builtin('error', (args, pos) => {
        expectAtLeastArity('error', args, 1, pos);
        throw new EvalError(args.map((arg) => formatDisplayValue(arg)).join(' '), pos);
      }),
    ],
    [
      'apply',
      builtin('apply', (args, pos) => {
        expectAtLeastArity('apply', args, 2, pos);

        const [procedure, ...rest] = args;
        const listArg = rest[rest.length - 1];
        const prefixArgs = rest.slice(0, -1);
        const listArgs = listToArray(listArg, 'apply', pos);

        return applyProcedure(procedure, [...prefixArgs, ...listArgs], pos);
      }),
    ],
    [
      'string-append',
      builtin('string-append', (args, pos) =>
        makeString(args.map((arg) => expectStringContent(arg, 'string-append', pos)).join('')),
      ),
    ],
    [
      'string-length',
      builtin('string-length', (args, pos) => {
        expectArity('string-length', args, 1, pos);
        return makeExactInteger(BigInt(expectStringValue(args[0], 'string-length', pos).chars.length));
      }),
    ],
    [
      'make-string',
      builtin('make-string', (args, pos) => {
        if (args.length !== 1 && args.length !== 2) {
          throw new EvalError(`make-string expected 1 or 2 argument(s), got ${args.length}`, pos);
        }

        const length = expectIndex(args[0], 'make-string', pos);
        const fill = args[1] === undefined ? ' ' : expectChar(args[1], 'make-string', pos).value;
        return makeString(Array.from({ length }, () => fill));
      }),
    ],
    [
      'string',
      builtin('string', (args, pos) =>
        makeString(args.map((arg) => expectChar(arg, 'string', pos).value)),
      ),
    ],
    [
      'substring',
      builtin('substring', (args, pos) => {
        expectArity('substring', args, 3, pos);
        const chars = expectStringValue(args[0], 'substring', pos).chars;
        const start = expectIndex(args[1], 'substring', pos);
        const end = expectIndex(args[2], 'substring', pos);

        if (start > end || end > chars.length) {
          throw new EvalError('substring index out of bounds', pos);
        }

        return makeString(chars.slice(start, end));
      }),
    ],
    [
      'string->number',
      builtin('string->number', (args, pos) => {
        expectArity('string->number', args, 1, pos);
        const value = expectStringContent(args[0], 'string->number', pos);
        return parseNumberLiteral(value) ?? false;
      }),
    ],
    [
      'number->string',
      builtin('number->string', (args, pos) => {
        expectArity('number->string', args, 1, pos);
        return makeString(formatNumberValue(expectNumber(args[0], 'number->string', pos)));
      }),
    ],
    [
      'exact->inexact',
      builtin('exact->inexact', (args, pos) => {
        expectArity('exact->inexact', args, 1, pos);
        return exactToInexact(expectNumber(args[0], 'exact->inexact', pos));
      }),
    ],
    [
      'inexact->exact',
      builtin('inexact->exact', (args, pos) => {
        expectArity('inexact->exact', args, 1, pos);
        return inexactToExact(expectNumber(args[0], 'inexact->exact', pos), 'inexact->exact', pos);
      }),
    ],
    [
      'numerator',
      builtin('numerator', (args, pos) => {
        expectArity('numerator', args, 1, pos);
        return makeExactInteger(exactNumberParts(expectNumber(args[0], 'numerator', pos), 'numerator', pos).numerator);
      }),
    ],
    [
      'denominator',
      builtin('denominator', (args, pos) => {
        expectArity('denominator', args, 1, pos);
        return makeExactInteger(
          exactNumberParts(expectNumber(args[0], 'denominator', pos), 'denominator', pos).denominator,
        );
      }),
    ],
    [
      'symbol->string',
      builtin('symbol->string', (args, pos) => {
        expectArity('symbol->string', args, 1, pos);
        return makeString(expectSymbolValue(args[0], 'symbol->string', pos).name);
      }),
    ],
    [
      'string->symbol',
      builtin('string->symbol', (args, pos) => {
        expectArity('string->symbol', args, 1, pos);
        return {
          kind: 'symbol',
          name: expectStringContent(args[0], 'string->symbol', pos),
        };
      }),
    ],
    [
      'string-ref',
      builtin('string-ref', (args, pos) => {
        expectArity('string-ref', args, 2, pos);
        const chars = expectStringValue(args[0], 'string-ref', pos).chars;
        const index = expectIndex(args[1], 'string-ref', pos);

        if (index >= chars.length) {
          throw new EvalError('string-ref index out of bounds', pos);
        }

        return { kind: 'char', value: chars[index] };
      }),
    ],
    [
      'string-copy',
      builtin('string-copy', (args, pos) => {
        expectArity('string-copy', args, 1, pos);
        return makeString(expectStringValue(args[0], 'string-copy', pos).chars);
      }),
    ],
    [
      'string->list',
      builtin('string->list', (args, pos) => {
        expectArity('string->list', args, 1, pos);
        return listToPairs(
          expectStringValue(args[0], 'string->list', pos).chars.map((char): CharValue => ({
            kind: 'char',
            value: char,
          })),
        );
      }),
    ],
    [
      'list->string',
      builtin('list->string', (args, pos) => {
        expectArity('list->string', args, 1, pos);
        return makeString(
          listToArray(args[0], 'list->string', pos).map((value) => expectChar(value, 'list->string', pos).value),
        );
      }),
    ],
    [
      'string-set!',
      builtin('string-set!', (args, pos) => {
        expectArity('string-set!', args, 3, pos);
        const target = expectMutableString(args[0], 'string-set!', pos);
        const index = expectIndex(args[1], 'string-set!', pos);
        const char = expectChar(args[2], 'string-set!', pos);

        if (index >= target.chars.length) {
          throw new EvalError('string-set! index out of bounds', pos);
        }

        target.chars[index] = char.value;
        return VOID;
      }),
    ],
    [
      'char?',
      builtin('char?', (args, pos) => {
        expectArity('char?', args, 1, pos);
        return isChar(args[0]);
      }),
    ],
    [
      'char->integer',
      builtin('char->integer', (args, pos) => {
        expectArity('char->integer', args, 1, pos);
        return makeExactInteger(BigInt(charCode(expectChar(args[0], 'char->integer', pos).value)));
      }),
    ],
    [
      'integer->char',
      builtin('integer->char', (args, pos) => {
        expectArity('integer->char', args, 1, pos);
        const codePoint = integerToSafeNumber(expectInteger(args[0], 'integer->char', pos), 'integer->char', pos);

        if (
          codePoint < 0 ||
          codePoint > 0x10ffff ||
          (codePoint >= 0xd800 && codePoint <= 0xdfff)
        ) {
          throw new EvalError('integer->char expected a valid Unicode scalar value', pos);
        }

        return { kind: 'char', value: String.fromCodePoint(codePoint) };
      }),
    ],
    [
      'char-alphabetic?',
      builtin('char-alphabetic?', (args, pos) => {
        expectArity('char-alphabetic?', args, 1, pos);
        return /^[A-Za-z]$/.test(expectChar(args[0], 'char-alphabetic?', pos).value);
      }),
    ],
    [
      'char-numeric?',
      builtin('char-numeric?', (args, pos) => {
        expectArity('char-numeric?', args, 1, pos);
        return /^[0-9]$/.test(expectChar(args[0], 'char-numeric?', pos).value);
      }),
    ],
    [
      'char-upcase',
      builtin('char-upcase', (args, pos) => {
        expectArity('char-upcase', args, 1, pos);
        return { kind: 'char', value: expectChar(args[0], 'char-upcase', pos).value.toUpperCase() };
      }),
    ],
    [
      'char-downcase',
      builtin('char-downcase', (args, pos) => {
        expectArity('char-downcase', args, 1, pos);
        return { kind: 'char', value: expectChar(args[0], 'char-downcase', pos).value.toLowerCase() };
      }),
    ],
    [
      'char=?',
      builtin('char=?', (args, pos) =>
        compareCharChain('char=?', args, (left, right) => left === right, pos),
      ),
    ],
    [
      'char<?',
      builtin('char<?', (args, pos) =>
        compareCharChain('char<?', args, (left, right) => left < right, pos),
      ),
    ],
    [
      'string=?',
      builtin('string=?', (args, pos) =>
        compareStringChain('string=?', args, (left, right) => left === right, pos),
      ),
    ],
    [
      'string<?',
      builtin('string<?', (args, pos) =>
        compareStringChain('string<?', args, (left, right) => left < right, pos),
      ),
    ],
    [
      'string>?',
      builtin('string>?', (args, pos) =>
        compareStringChain('string>?', args, (left, right) => left > right, pos),
      ),
    ],
    [
      'string<=?',
      builtin('string<=?', (args, pos) =>
        compareStringChain('string<=?', args, (left, right) => left <= right, pos),
      ),
    ],
    [
      'string>=?',
      builtin('string>=?', (args, pos) =>
        compareStringChain('string>=?', args, (left, right) => left >= right, pos),
      ),
    ],
    [
      'string-ci=?',
      builtin('string-ci=?', (args, pos) =>
        compareStringChain(
          'string-ci=?',
          args,
          (left, right) => left.toLowerCase() === right.toLowerCase(),
          pos,
        ),
      ),
    ],
    [
      'string-upcase',
      builtin('string-upcase', (args, pos) => {
        expectArity('string-upcase', args, 1, pos);
        return makeString(expectStringContent(args[0], 'string-upcase', pos).toUpperCase());
      }),
    ],
    [
      'string-downcase',
      builtin('string-downcase', (args, pos) => {
        expectArity('string-downcase', args, 1, pos);
        return makeString(expectStringContent(args[0], 'string-downcase', pos).toLowerCase());
      }),
    ],
  ]);
}

export function evalStr(input: string): string {
  return evaluateInput(input).result;
}

export function evalStrWithOutput(input: string): { result: string; output: string } {
  return evaluateInput(input);
}

function evaluateInput(input: string): { result: string; output: string } {
  const parser = new Parser(tokenize(input));
  const expressions = parser.parseProgram();

  if (expressions.length === 0) {
    throw new EvalError('expected at least one expression', { line: 1, col: 1 });
  }

  const context: EvaluationContext = { output: [] };
  const env = createGlobalEnvironment(context);
  let result: SchemeValue = VOID;

  for (const expression of expressions) {
    result = evaluate(expression, env);
  }

  return {
    result: formatValue(result),
    output: context.output.join(''),
  };
}

class Environment {
  private readonly bindings = new Map<string, SchemeValue>();
  private readonly syntaxBindings = new Map<string, SyntaxTransformer>();
  private readonly parent?: Environment;
  private readonly runtime: RuntimeState;

  constructor(parent?: Environment) {
    this.parent = parent;
    this.runtime = parent?.runtime ?? { nextHygieneId: 0 };
  }

  define(name: string, value: SchemeValue): void {
    this.bindings.set(name, value);
  }

  defineSyntax(name: string, transformer: SyntaxTransformer): void {
    this.syntaxBindings.set(name, transformer);
  }

  lookup(name: string, pos: SourcePosition): SchemeValue {
    if (this.bindings.has(name)) {
      const value = this.bindings.get(name) as SchemeValue;
      if (typeof value === 'object' && value !== null && value.kind === 'uninitialized') {
        throw new EvalError(`uninitialized binding: ${value.name}`, pos);
      }

      return value;
    }

    if (this.parent !== undefined) {
      return this.parent.lookup(name, pos);
    }

    throw new EvalError(`unbound symbol: ${name}`, pos);
  }

  assign(name: string, value: SchemeValue, pos: SourcePosition): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, value);
      return;
    }

    if (this.parent !== undefined) {
      this.parent.assign(name, value, pos);
      return;
    }

    throw new EvalError(`unbound symbol: ${name}`, pos);
  }

  lookupSyntax(name: string): SyntaxTransformer | undefined {
    if (this.syntaxBindings.has(name)) {
      return this.syntaxBindings.get(name);
    }

    return this.parent?.lookupSyntax(name);
  }

  freshBindingKey(name: string): string {
    this.runtime.nextHygieneId += 1;
    return `${name}#${this.runtime.nextHygieneId}`;
  }
}

function createGlobalEnvironment(context: EvaluationContext): Environment {
  const env = new Environment();

  for (const [name, value] of createBuiltins(context)) {
    env.define(name, value);
  }

  return env;
}

function symbolName(expression: Expr): string | undefined {
  if (expression.kind === 'symbol' || expression.kind === 'resolved-symbol') {
    return expression.name;
  }

  return undefined;
}

function bindingName(expression: Expr, context: string): string {
  if (expression.kind === 'symbol') {
    return expression.name;
  }

  if (expression.kind === 'resolved-symbol') {
    return expression.binding.kind === 'lexical' ? expression.binding.key : expression.name;
  }

  throw new EvalError(`${context} expected a symbol`, expression.pos);
}

function evaluateSymbol(expression: Extract<Expr, { kind: 'resolved-symbol' }>, env: Environment): SchemeValue {
  if (expression.binding.kind === 'lexical') {
    return env.lookup(expression.binding.key, expression.pos);
  }

  return expression.binding.env.lookup(expression.name, expression.pos);
}

function assignSymbol(expression: Expr, value: SchemeValue, env: Environment, context: string): void {
  if (expression.kind === 'symbol') {
    env.assign(expression.name, value, expression.pos);
    return;
  }

  if (expression.kind === 'resolved-symbol') {
    if (expression.binding.kind === 'lexical') {
      env.assign(expression.binding.key, value, expression.pos);
    } else {
      expression.binding.env.assign(expression.name, value, expression.pos);
    }
    return;
  }

  throw new EvalError(`${context} expected a symbol`, expression.pos);
}

function lookupSyntax(expression: Expr, env: Environment): SyntaxTransformer | undefined {
  if (expression.kind === 'symbol') {
    return env.lookupSyntax(expression.name);
  }

  if (expression.kind === 'resolved-symbol') {
    if (expression.binding.kind === 'lexical') {
      return env.lookupSyntax(expression.binding.key);
    }

    return expression.binding.env.lookupSyntax(expression.name);
  }

  return undefined;
}

function valueStep(value: SchemeValue): EvaluationStep {
  return { kind: 'value', value };
}

function exprStep(expression: Expr, env: Environment): EvaluationStep {
  return { kind: 'expr', expression, env };
}

function applyStep(procedure: SchemeValue, args: SchemeValue[], pos: SourcePosition): EvaluationStep {
  return { kind: 'apply', procedure, args, pos };
}

function evaluate(expression: Expr, env: Environment): SchemeValue {
  return runEvaluation(exprStep(expression, env));
}

function runEvaluation(initialStep: EvaluationStep): SchemeValue {
  let step = initialStep;

  while (true) {
    switch (step.kind) {
      case 'value':
        return step.value;
      case 'expr':
        const expression = step.expression;
        try {
          step = evaluateExpressionStep(expression, step.env);
        } catch (error) {
          throw errorWithPosition(error, expression.pos);
        }
        break;
      case 'apply':
        step = applyProcedureStep(step.procedure, step.args, step.pos);
        break;
    }
  }
}

function evaluateExpressionStep(expression: Expr, env: Environment): EvaluationStep {
  switch (expression.kind) {
    case 'number':
    case 'boolean':
      return valueStep(expression.value);
    case 'string':
      return valueStep(makeString(expression.value));
    case 'char':
      return valueStep({ kind: 'char', value: expression.value });
    case 'symbol':
      return valueStep(env.lookup(expression.name, expression.pos));
    case 'resolved-symbol':
      return valueStep(evaluateSymbol(expression, env));
    case 'list':
      return evaluateListStep(expression.elements, env, expression.pos);
  }
}

function evaluateListStep(elements: Expr[], env: Environment, pos: SourcePosition): EvaluationStep {
  if (elements.length === 0) {
    throw new EvalError('cannot evaluate empty list', pos);
  }

  const [operatorExpr, ...argumentExprs] = elements;

  const operatorName = symbolName(operatorExpr);

  if (operatorName !== undefined) {
    switch (operatorName) {
      case 'and':
        return evaluateAndStep(argumentExprs, env);
      case 'or':
        return evaluateOrStep(argumentExprs, env);
      case 'if':
        return evaluateIfStep(argumentExprs, env, operatorExpr.pos);
      case 'define':
        return valueStep(evaluateDefine(argumentExprs, env, operatorExpr.pos));
      case 'define-syntax':
        return valueStep(evaluateDefineSyntax(argumentExprs, env, operatorExpr.pos));
      case 'define-record-type':
        return valueStep(evaluateDefineRecordType(argumentExprs, env, operatorExpr.pos));
      case 'quote':
        return valueStep(evaluateQuote(argumentExprs, operatorExpr.pos));
      case 'lambda':
        return valueStep(evaluateLambda(argumentExprs, env, operatorExpr.pos));
      case 'case-lambda':
        return valueStep(evaluateCaseLambda(argumentExprs, env, operatorExpr.pos));
      case 'begin':
        return evaluateBeginStep(argumentExprs, env);
      case 'cond':
        return evaluateCondStep(argumentExprs, env, operatorExpr.pos);
      case 'case':
        return evaluateCaseStep(argumentExprs, env, operatorExpr.pos);
      case 'let':
        return evaluateLetStep(argumentExprs, env, operatorExpr.pos);
      case 'let*':
        return evaluateLetStarStep(argumentExprs, env, operatorExpr.pos);
      case 'letrec':
        return evaluateLetrecStep(argumentExprs, env, operatorExpr.pos, false);
      case 'letrec*':
        return evaluateLetrecStep(argumentExprs, env, operatorExpr.pos, true);
      case 'do':
        return valueStep(evaluateDo(argumentExprs, env, operatorExpr.pos));
      case 'set!':
        return valueStep(evaluateSet(argumentExprs, env, operatorExpr.pos));
    }

    const transformer = lookupSyntax(operatorExpr, env);
    if (transformer !== undefined) {
      return exprStep(expandMacro(transformer, { kind: 'list', elements, pos }), env);
    }
  }

  const operator = evaluate(operatorExpr, env);
  const args = argumentExprs.map((argument) => evaluate(argument, env));
  return applyStep(operator, args, operatorExpr.pos);
}

function evaluateAndStep(expressions: Expr[], env: Environment): EvaluationStep {
  if (expressions.length === 0) {
    return valueStep(true);
  }

  for (let index = 0; index < expressions.length - 1; index += 1) {
    const result = evaluate(expressions[index], env);
    if (isFalse(result)) {
      return valueStep(result);
    }
  }

  return exprStep(expressions[expressions.length - 1], env);
}

function evaluateOrStep(expressions: Expr[], env: Environment): EvaluationStep {
  if (expressions.length === 0) {
    return valueStep(false);
  }

  for (let index = 0; index < expressions.length - 1; index += 1) {
    const result = evaluate(expressions[index], env);
    if (!isFalse(result)) {
      return valueStep(result);
    }
  }

  return exprStep(expressions[expressions.length - 1], env);
}

function evaluateIfStep(expressions: Expr[], env: Environment, pos: SourcePosition): EvaluationStep {
  if (expressions.length !== 2 && expressions.length !== 3) {
    throw new EvalError(`if expected 2 or 3 argument(s), got ${expressions.length}`, pos);
  }

  const [conditionExpr, thenExpr, elseExpr] = expressions;
  const condition = evaluate(conditionExpr, env);

  if (isFalse(condition)) {
    return elseExpr === undefined ? valueStep(VOID) : exprStep(elseExpr, env);
  }

  return exprStep(thenExpr, env);
}

function evaluateDefine(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length < 2) {
    throw new EvalError(`define expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  const [targetExpr, ...valueExprs] = expressions;

  if (targetExpr.kind === 'symbol' || targetExpr.kind === 'resolved-symbol') {
    if (valueExprs.length !== 1) {
      throw new EvalError(`define expected 1 value expression, got ${valueExprs.length}`, pos);
    }

    const value = evaluate(valueExprs[0], env);
    env.define(bindingName(targetExpr, 'define'), value);
    return VOID;
  }

  if (targetExpr.kind !== 'list' || targetExpr.elements.length === 0) {
    throw new EvalError('define expected a symbol or function signature', targetExpr.pos);
  }

  const [nameExpr, ...paramExprs] = targetExpr.elements;
  const name = expectSymbolExpr(nameExpr, 'define');
  const { params, restParam } = parseFormalParameters(paramExprs, 'define');

  if (valueExprs.length === 0) {
    throw new EvalError('define expected at least one function body expression', pos);
  }

  const closure: ClosureValue = {
    kind: 'closure',
    name,
    params,
    restParam,
    body: valueExprs,
    env,
  };

  env.define(bindingName(nameExpr, 'define'), closure);
  return VOID;
}

function evaluateSet(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length !== 2) {
    throw new EvalError(`set! expected 2 argument(s), got ${expressions.length}`, pos);
  }

  const [targetExpr, valueExpr] = expressions;
  const value = evaluate(valueExpr, env);
  assignSymbol(targetExpr, value, env, 'set!');
  return VOID;
}

function evaluateDefineSyntax(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length !== 2) {
    throw new EvalError(`define-syntax expected 2 argument(s), got ${expressions.length}`, pos);
  }

  const [keywordExpr, transformerExpr] = expressions;
  const keyword = expectSymbolExpr(keywordExpr, 'define-syntax');
  env.defineSyntax(bindingName(keywordExpr, 'define-syntax'), parseSyntaxRules(keyword, transformerExpr, env));
  return VOID;
}

function evaluateDefineRecordType(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length < 3) {
    throw new EvalError(
      `define-record-type expected at least 3 argument(s), got ${expressions.length}`,
      pos,
    );
  }

  const [typeNameExpr, constructorExpr, predicateExpr, ...fieldExprs] = expressions;
  const typeName = expectSymbolExpr(typeNameExpr, 'define-record-type');

  if (constructorExpr.kind !== 'list' || constructorExpr.elements.length === 0) {
    throw new EvalError('define-record-type expected a constructor specification', constructorExpr.pos);
  }

  const [constructorNameExpr, ...constructorFieldExprs] = constructorExpr.elements;
  const constructorName = expectSymbolExpr(constructorNameExpr, 'define-record-type');
  const constructorBinding = bindingName(constructorNameExpr, 'define-record-type');
  const predicateName = expectSymbolExpr(predicateExpr, 'define-record-type');
  const predicateBinding = bindingName(predicateExpr, 'define-record-type');
  const constructorFields = constructorFieldExprs.map((fieldExpr) =>
    expectSymbolExpr(fieldExpr, 'define-record-type'),
  );
  const fields = fieldExprs.map((fieldExpr) => parseRecordField(fieldExpr));
  const recordType: RecordTypeDescriptor = {
    name: typeName,
    fields,
  };
  const fieldIndexes = new Map<string, number>();

  for (let index = 0; index < fields.length; index += 1) {
    if (fieldIndexes.has(fields[index].name)) {
      throw new EvalError(`define-record-type duplicate field: ${fields[index].name}`, fieldExprs[index].pos);
    }

    fieldIndexes.set(fields[index].name, index);
  }

  for (const fieldName of constructorFields) {
    if (!fieldIndexes.has(fieldName)) {
      throw new EvalError(`define-record-type unknown constructor field: ${fieldName}`, constructorExpr.pos);
    }
  }

  env.define(
    constructorBinding,
    builtin(constructorName, (args, callPos) => {
      expectArity(constructorName, args, constructorFields.length, callPos);
      const fieldValues = new Array<SchemeValue>(fields.length).fill(VOID);

      for (let index = 0; index < constructorFields.length; index += 1) {
        const fieldIndex = fieldIndexes.get(constructorFields[index]);
        if (fieldIndex === undefined) {
          throw new EvalError(`define-record-type unknown constructor field: ${constructorFields[index]}`, callPos);
        }

        fieldValues[fieldIndex] = args[index];
      }

      return {
        kind: 'record',
        type: recordType,
        fields: fieldValues,
      };
    }),
  );

  env.define(
    predicateBinding,
    builtin(predicateName, (args, callPos) => {
      expectArity(predicateName, args, 1, callPos);
      return isRecordValue(args[0]) && args[0].type === recordType;
    }),
  );

  for (let index = 0; index < fields.length; index += 1) {
    const field = fields[index];
    const fieldExpr = fieldExprs[index];

    if (field.accessorName !== undefined) {
      const accessorName = field.accessorName;
      const accessorExpr = fieldExpr.kind === 'list' ? fieldExpr.elements[1] : undefined;
      if (accessorExpr === undefined) {
        throw new EvalError('define-record-type expected an accessor name', fieldExpr.pos);
      }

      env.define(
        bindingName(accessorExpr, 'define-record-type'),
        builtin(accessorName, (args, callPos) => {
          expectArity(accessorName, args, 1, callPos);
          return expectRecordOfType(args[0], recordType, accessorName, callPos).fields[index];
        }),
      );
    }

    if (field.mutatorName !== undefined) {
      const mutatorName = field.mutatorName;
      const mutatorExpr = fieldExpr.kind === 'list' ? fieldExpr.elements[2] : undefined;
      if (mutatorExpr === undefined) {
        throw new EvalError('define-record-type expected a mutator name', fieldExpr.pos);
      }

      env.define(
        bindingName(mutatorExpr, 'define-record-type'),
        builtin(mutatorName, (args, callPos) => {
          expectArity(mutatorName, args, 2, callPos);
          expectRecordOfType(args[0], recordType, mutatorName, callPos).fields[index] = args[1];
          return VOID;
        }),
      );
    }
  }

  return VOID;
}

function parseRecordField(expression: Expr): RecordFieldDescriptor {
  if (expression.kind === 'symbol' || expression.kind === 'resolved-symbol') {
    return {
      name: expectSymbolExpr(expression, 'define-record-type'),
    };
  }

  if (expression.kind !== 'list' || expression.elements.length === 0 || expression.elements.length > 3) {
    throw new EvalError('define-record-type expected field specs of the form name or (name accessor)', expression.pos);
  }

  const [nameExpr, accessorExpr, mutatorExpr] = expression.elements;
  return {
    name: expectSymbolExpr(nameExpr, 'define-record-type'),
    accessorName:
      accessorExpr === undefined ? undefined : expectSymbolExpr(accessorExpr, 'define-record-type'),
    mutatorName:
      mutatorExpr === undefined ? undefined : expectSymbolExpr(mutatorExpr, 'define-record-type'),
  };
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
  const { params, restParam } = parseFormals(paramsExpr, 'lambda');

  return {
    kind: 'closure',
    params,
    restParam,
    body,
    env,
  };
}

function evaluateCaseLambda(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length === 0) {
    throw new EvalError('case-lambda expected at least one clause', pos);
  }

  return {
    kind: 'case-lambda',
    clauses: expressions.map((expression) => parseCaseLambdaClause(expression)),
    env,
  };
}

function evaluateBeginStep(expressions: Expr[], env: Environment): EvaluationStep {
  return evaluateSequenceStep(expressions, env);
}

function evaluateCondStep(clauses: Expr[], env: Environment, pos: SourcePosition): EvaluationStep {
  for (let index = 0; index < clauses.length; index += 1) {
    const clause = clauses[index];

    if (clause.kind !== 'list' || clause.elements.length === 0) {
      throw new EvalError('cond expected a non-empty clause', clause.pos);
    }

    const [testExpr, ...bodyExprs] = clause.elements;
    const isElseClause = symbolName(testExpr) === 'else';

    if (isElseClause) {
      if (index !== clauses.length - 1) {
        throw new EvalError('cond else clause must be last', clause.pos);
      }

      return evaluateSequenceStep(bodyExprs, env);
    }

    const testValue = evaluate(testExpr, env);
    if (!isFalse(testValue)) {
      if (bodyExprs.length === 0) {
        return valueStep(testValue);
      }

      return evaluateSequenceStep(bodyExprs, env);
    }
  }

  return valueStep(VOID);
}

function evaluateCaseStep(expressions: Expr[], env: Environment, pos: SourcePosition): EvaluationStep {
  if (expressions.length < 1) {
    throw new EvalError('case expected at least 1 argument', pos);
  }

  const [keyExpr, ...clauses] = expressions;
  const key = evaluate(keyExpr, env);

  for (let index = 0; index < clauses.length; index += 1) {
    const clause = clauses[index];

    if (clause.kind !== 'list' || clause.elements.length === 0) {
      throw new EvalError('case expected a non-empty clause', clause.pos);
    }

    const [datumExpr, ...bodyExprs] = clause.elements;
    const isElseClause = symbolName(datumExpr) === 'else';

    if (isElseClause) {
      if (index !== clauses.length - 1) {
        throw new EvalError('case else clause must be last', clause.pos);
      }

      return evaluateSequenceStep(bodyExprs, env);
    }

    if (datumExpr.kind !== 'list') {
      throw new EvalError('case expected a datum list or else clause', datumExpr.pos);
    }

    for (const datum of datumExpr.elements) {
      if (isEqValue(key, quoteExpr(datum))) {
        return evaluateSequenceStep(bodyExprs, env);
      }
    }
  }

  return valueStep(VOID);
}

function evaluateLetStep(expressions: Expr[], env: Environment, pos: SourcePosition): EvaluationStep {
  if (expressions.length < 2) {
    throw new EvalError(`let expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  if (expressions[0].kind === 'symbol' || expressions[0].kind === 'resolved-symbol') {
    const [nameExpr, bindingsExpr, ...bodyExprs] = expressions;

    if (bindingsExpr === undefined || bodyExprs.length === 0) {
      throw new EvalError('let expected bindings and a body', pos);
    }

    const bindings = parseBindings(bindingsExpr, 'let');
    const values = bindings.map((binding) => evaluate(binding.valueExpr, env));
    const letEnv = new Environment(env);
    const closure: ClosureValue = {
      kind: 'closure',
      name: expectSymbolExpr(nameExpr, 'let'),
      params: bindings.map((binding) => binding.name),
      body: bodyExprs,
      env: letEnv,
    };

    letEnv.define(bindingName(nameExpr, 'let'), closure);
    return applyStep(closure, values, nameExpr.pos);
  }

  const [bindingsExpr, ...bodyExprs] = expressions;
  const bindings = parseBindings(bindingsExpr, 'let');
  const values = bindings.map((binding) => evaluate(binding.valueExpr, env));
  const letEnv = new Environment(env);

  for (let index = 0; index < bindings.length; index += 1) {
    letEnv.define(bindings[index].name, values[index]);
  }

  return evaluateSequenceStep(bodyExprs, letEnv);
}

function evaluateLetStarStep(expressions: Expr[], env: Environment, pos: SourcePosition): EvaluationStep {
  if (expressions.length < 2) {
    throw new EvalError(`let* expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  const [bindingsExpr, ...bodyExprs] = expressions;
  const bindings = parseBindings(bindingsExpr, 'let*');
  const letStarEnv = new Environment(env);

  for (const binding of bindings) {
    letStarEnv.define(binding.name, evaluate(binding.valueExpr, letStarEnv));
  }

  return evaluateSequenceStep(bodyExprs, letStarEnv);
}

function evaluateLetrecStep(
  expressions: Expr[],
  env: Environment,
  pos: SourcePosition,
  sequential: boolean,
): EvaluationStep {
  if (expressions.length < 2) {
    const name = sequential ? 'letrec*' : 'letrec';
    throw new EvalError(`${name} expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  const [bindingsExpr, ...bodyExprs] = expressions;
  const name = sequential ? 'letrec*' : 'letrec';
  const bindings = parseBindings(bindingsExpr, name);
  const letrecEnv = new Environment(env);

  for (const binding of bindings) {
    letrecEnv.define(binding.name, { kind: 'uninitialized', name: binding.name });
  }

  if (sequential) {
    for (const binding of bindings) {
      letrecEnv.define(binding.name, evaluate(binding.valueExpr, letrecEnv));
    }
  } else {
    const values = bindings.map((binding) => evaluate(binding.valueExpr, letrecEnv));

    for (let index = 0; index < bindings.length; index += 1) {
      letrecEnv.define(bindings[index].name, values[index]);
    }
  }

  return evaluateSequenceStep(bodyExprs, letrecEnv);
}

function evaluateDo(expressions: Expr[], env: Environment, pos: SourcePosition): SchemeValue {
  if (expressions.length < 2) {
    throw new EvalError(`do expected at least 2 argument(s), got ${expressions.length}`, pos);
  }

  const [bindingsExpr, testClauseExpr, ...bodyExprs] = expressions;
  const bindings = parseDoBindings(bindingsExpr);

  if (testClauseExpr.kind !== 'list' || testClauseExpr.elements.length === 0) {
    throw new EvalError('do expected a termination clause', testClauseExpr.pos);
  }

  const [testExpr, ...resultExprs] = testClauseExpr.elements;
  const initValues = bindings.map((binding) => evaluate(binding.initExpr, env));
  const doEnv = new Environment(env);

  for (let index = 0; index < bindings.length; index += 1) {
    doEnv.define(bindings[index].name, initValues[index]);
  }

  while (true) {
    if (!isFalse(evaluate(testExpr, doEnv))) {
      return resultExprs.length === 0 ? VOID : evaluateSequence(resultExprs, doEnv);
    }

    evaluateSequence(bodyExprs, doEnv);

    const nextValues = bindings.map((binding) =>
      binding.stepExpr === undefined ? doEnv.lookup(binding.name, binding.pos) : evaluate(binding.stepExpr, doEnv),
    );

    for (let index = 0; index < bindings.length; index += 1) {
      doEnv.assign(bindings[index].name, nextValues[index], bindings[index].pos);
    }
  }
}

function applyProcedure(value: SchemeValue, args: SchemeValue[], pos: SourcePosition): SchemeValue {
  return runEvaluation(applyStep(value, args, pos));
}

function applyProcedureStep(value: SchemeValue, args: SchemeValue[], pos: SourcePosition): EvaluationStep {
  if (isBuiltin(value)) {
    return valueStep(value.apply(args, pos));
  }

  if (isClosure(value)) {
    return applyProcedureClauseStep(value, value.env, args, pos, value.name ?? 'lambda');
  }

  if (isCaseLambda(value)) {
    const clause = value.clauses.find((candidate) => matchesClauseArity(candidate, args.length));

    if (clause === undefined) {
      throw new EvalError(`case-lambda has no matching clause for ${args.length} argument(s)`, pos);
    }

    return applyProcedureClauseStep(clause, value.env, args, pos, value.name ?? 'case-lambda');
  }

  throw new EvalError('attempted to call a non-procedure value', pos);
}

function applyProcedureClauseStep(
  clause: ProcedureClause,
  env: Environment,
  args: SchemeValue[],
  pos: SourcePosition,
  name: string,
): EvaluationStep {
  if (clause.restParam === undefined && args.length !== clause.params.length) {
    throw new EvalError(
      `${name} expected ${clause.params.length} argument(s), got ${args.length}`,
      pos,
    );
  }

  if (clause.restParam !== undefined && args.length < clause.params.length) {
    throw new EvalError(
      `${name} expected at least ${clause.params.length} argument(s), got ${args.length}`,
      pos,
    );
  }

  const callEnv = new Environment(env);

  for (let index = 0; index < clause.params.length; index += 1) {
    callEnv.define(clause.params[index], args[index]);
  }

  if (clause.restParam !== undefined) {
    callEnv.define(clause.restParam, listToPairs(args.slice(clause.params.length)));
  }

  return evaluateSequenceStep(clause.body, callEnv);
}

function matchesClauseArity(clause: ProcedureClause, argCount: number): boolean {
  return clause.restParam === undefined ? argCount === clause.params.length : argCount >= clause.params.length;
}

function parseSyntaxRules(keyword: string, expression: Expr, env: Environment): SyntaxTransformer {
  if (expression.kind !== 'list' || expression.elements.length < 2) {
    throw new EvalError('define-syntax expected a syntax-rules transformer', expression.pos);
  }

  const [headExpr, literalsExpr, ...ruleExprs] = expression.elements;
  if (expectSymbolExpr(headExpr, 'define-syntax') !== 'syntax-rules') {
    throw new EvalError('define-syntax expected a syntax-rules transformer', headExpr.pos);
  }

  if (literalsExpr.kind !== 'list') {
    throw new EvalError('syntax-rules expected a literal identifier list', literalsExpr.pos);
  }

  const literals = new Set<string>();
  for (const literalExpr of literalsExpr.elements) {
    literals.add(expectSymbolExpr(literalExpr, 'syntax-rules'));
  }

  if (ruleExprs.length === 0) {
    throw new EvalError('syntax-rules expected at least one rule', expression.pos);
  }

  const rules = ruleExprs.map((ruleExpr): SyntaxRule => {
    if (ruleExpr.kind !== 'list' || ruleExpr.elements.length !== 2) {
      throw new EvalError('syntax-rules expected rules of the form (pattern template)', ruleExpr.pos);
    }

    return {
      pattern: ruleExpr.elements[0],
      template: ruleExpr.elements[1],
    };
  });

  return {
    keyword,
    literals,
    rules,
    definitionEnv: env,
  };
}

function expandMacro(transformer: SyntaxTransformer, expression: Expr): Expr {
  for (const rule of transformer.rules) {
    const bindings = matchMacroRule(rule.pattern, expression, transformer.literals);
    if (bindings !== undefined) {
      return expandTemplate(rule.template, bindings, transformer.definitionEnv, new Map(), []);
    }
  }

  throw new EvalError(`no matching syntax-rules pattern for ${transformer.keyword}`, expression.pos);
}

function matchMacroRule(
  pattern: Expr,
  expression: Expr,
  literals: Set<string>,
): PatternBindings | undefined {
  if (pattern.kind !== 'list') {
    return undefined;
  }

  if (expression.kind !== 'list') {
    return undefined;
  }

  return matchPatternSequence(pattern.elements.slice(1), expression.elements.slice(1), literals);
}

function matchPattern(
  pattern: Expr,
  expression: Expr,
  literals: Set<string>,
): PatternBindings | undefined {
  switch (pattern.kind) {
    case 'number':
      return expression.kind === 'number' && sameNumberLiteral(expression.value, pattern.value)
        ? new Map()
        : undefined;
    case 'boolean':
      return expression.kind === 'boolean' && expression.value === pattern.value ? new Map() : undefined;
    case 'string':
      return expression.kind === 'string' && expression.value === pattern.value ? new Map() : undefined;
    case 'char':
      return expression.kind === 'char' && expression.value === pattern.value ? new Map() : undefined;
    case 'symbol': {
      if (pattern.name === '...') {
        return undefined;
      }

      if (literals.has(pattern.name)) {
        return symbolName(expression) === pattern.name ? new Map() : undefined;
      }

      return new Map([[pattern.name, { kind: 'expr', value: expression }]]);
    }
    case 'resolved-symbol':
      return undefined;
    case 'list':
      return expression.kind === 'list'
        ? matchPatternSequence(pattern.elements, expression.elements, literals)
        : undefined;
  }
}

function matchPatternSequence(
  patternElements: Expr[],
  expressionElements: Expr[],
  literals: Set<string>,
): PatternBindings | undefined {
  const matchFrom = (patternIndex: number, expressionIndex: number): PatternBindings | undefined => {
    if (patternIndex >= patternElements.length) {
      return expressionIndex === expressionElements.length ? new Map() : undefined;
    }

    const currentPattern = patternElements[patternIndex];
    const nextPattern = patternElements[patternIndex + 1];

    if (nextPattern !== undefined && isEllipsisExpr(nextPattern)) {
      const remainingMinimum = minimumSequenceLength(patternElements.slice(patternIndex + 2));
      const maxCount = expressionElements.length - expressionIndex - remainingMinimum;

      if (maxCount < 0) {
        return undefined;
      }

      const repeatedVariables = [...collectPatternVariables(currentPattern, literals)];

      for (let count = maxCount; count >= 0; count -= 1) {
        const grouped: PatternBindings = new Map();
        const collected = new Map<string, PatternBindingValue[]>(
          repeatedVariables.map((name) => [name, []]),
        );
        let valid = true;

        for (let offset = 0; offset < count; offset += 1) {
          const matched = matchPattern(currentPattern, expressionElements[expressionIndex + offset], literals);
          if (matched === undefined) {
            valid = false;
            break;
          }

          for (const variable of repeatedVariables) {
            const value = matched.get(variable);
            if (value !== undefined) {
              collected.get(variable)?.push(value);
            }
          }
        }

        if (!valid) {
          continue;
        }

        for (const variable of repeatedVariables) {
          grouped.set(variable, {
            kind: 'repeated',
            value: collected.get(variable) ?? [],
          });
        }

        const rest = matchFrom(patternIndex + 2, expressionIndex + count);
        if (rest === undefined) {
          continue;
        }

        const merged = mergePatternBindings(grouped, rest);
        if (merged !== undefined) {
          return merged;
        }
      }

      return undefined;
    }

    const currentExpression = expressionElements[expressionIndex];
    if (currentExpression === undefined) {
      return undefined;
    }

    const current = matchPattern(currentPattern, currentExpression, literals);
    if (current === undefined) {
      return undefined;
    }

    const rest = matchFrom(patternIndex + 1, expressionIndex + 1);
    if (rest === undefined) {
      return undefined;
    }

    return mergePatternBindings(current, rest);
  };

  return matchFrom(0, 0);
}

function minimumSequenceLength(patternElements: Expr[]): number {
  let total = 0;

  for (let index = 0; index < patternElements.length; ) {
    if (patternElements[index + 1] !== undefined && isEllipsisExpr(patternElements[index + 1])) {
      index += 2;
    } else {
      total += 1;
      index += 1;
    }
  }

  return total;
}

function collectPatternVariables(pattern: Expr, literals: Set<string>): Set<string> {
  const variables = new Set<string>();

  const visit = (expression: Expr): void => {
    switch (expression.kind) {
      case 'symbol':
        if (expression.name !== '...' && !literals.has(expression.name)) {
          variables.add(expression.name);
        }
        break;
      case 'resolved-symbol':
        break;
      case 'list':
        for (const element of expression.elements) {
          visit(element);
        }
        break;
      default:
        break;
    }
  };

  visit(pattern);
  return variables;
}

function mergePatternBindings(
  left: PatternBindings,
  right: PatternBindings,
): PatternBindings | undefined {
  const merged: PatternBindings = new Map(left);

  for (const [name, value] of right) {
    const existing = merged.get(name);
    if (existing !== undefined) {
      if (!patternBindingEquals(existing, value)) {
        return undefined;
      }
    } else {
      merged.set(name, value);
    }
  }

  return merged;
}

function patternBindingEquals(left: PatternBindingValue, right: PatternBindingValue): boolean {
  if (left.kind === 'expr' && right.kind === 'expr') {
    return syntaxEquals(left.value, right.value);
  }

  if (left.kind === 'repeated' && right.kind === 'repeated') {
    return (
      left.value.length === right.value.length &&
      left.value.every((value, index) => patternBindingEquals(value, right.value[index]))
    );
  }

  return false;
}

function syntaxEquals(left: Expr, right: Expr): boolean {
  if (left.kind !== right.kind) {
    return false;
  }

  switch (left.kind) {
    case 'number':
      return right.kind === 'number' && sameNumberLiteral(left.value, right.value);
    case 'boolean':
    case 'string':
    case 'char':
      return left.value === (right as typeof left).value;
    case 'symbol':
      return left.name === (right as typeof left).name;
    case 'resolved-symbol':
      return (
        right.kind === 'resolved-symbol' &&
        left.name === right.name &&
        ((left.binding.kind === 'lexical' &&
          right.binding.kind === 'lexical' &&
          left.binding.key === right.binding.key) ||
          (left.binding.kind === 'captured' &&
            right.binding.kind === 'captured' &&
            left.binding.env === right.binding.env))
      );
    case 'list':
      return (
        right.kind === 'list' &&
        left.elements.length === right.elements.length &&
        left.elements.every((element, index) => syntaxEquals(element, right.elements[index]))
      );
  }
}

function expandTemplate(
  template: Expr,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): Expr {
  switch (template.kind) {
    case 'number':
    case 'boolean':
    case 'string':
    case 'char':
      return template;
    case 'symbol': {
      if (template.name === '...') {
        throw new EvalError('unexpected ellipsis in macro template', template.pos);
      }

      const binding = bindings.get(template.name);
      if (binding !== undefined) {
        return resolveTemplateBinding(binding, path, template.pos);
      }

      const localBinding = scope.get(template.name);
      if (localBinding !== undefined) {
        return makeResolvedSymbol(template.name, template.pos, { kind: 'lexical', key: localBinding });
      }

      return makeResolvedSymbol(template.name, template.pos, { kind: 'captured', env: definitionEnv });
    }
    case 'resolved-symbol':
      return template;
    case 'list':
      return expandTemplateList(template, bindings, definitionEnv, scope, path);
  }
}

function expandTemplateList(
  template: Extract<Expr, { kind: 'list' }>,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): Expr {
  const { elements } = template;

  if (elements.length === 0) {
    return template;
  }

  const headName = symbolName(elements[0]);
  if (headName !== undefined && !bindings.has(headName)) {
    switch (headName) {
      case 'lambda':
        return expandLambdaTemplate(template, bindings, definitionEnv, scope, path);
      case 'let':
        return expandLetTemplate(template, bindings, definitionEnv, scope, path);
      case 'define':
        return expandDefineTemplate(template, bindings, definitionEnv, scope, path);
    }
  }

  return {
    kind: 'list',
    elements: expandTemplateSequence(elements, bindings, definitionEnv, scope, path),
    pos: template.pos,
  };
}

function expandLambdaTemplate(
  template: Extract<Expr, { kind: 'list' }>,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): Expr {
  const { elements } = template;

  if (elements.length < 3) {
    return {
      kind: 'list',
      elements: expandTemplateSequence(elements, bindings, definitionEnv, scope, path),
      pos: template.pos,
    };
  }

  const head = expandTemplate(elements[0], bindings, definitionEnv, scope, path);
  const [paramsExpr, parameterScope] = expandParameterBindings(
    elements[1],
    bindings,
    definitionEnv,
    scope,
    path,
  );
  const bodyScope = extendScope(scope, parameterScope);
  const expanded = [head, paramsExpr];

  for (const bodyTemplate of elements.slice(2)) {
    expanded.push(expandTemplate(bodyTemplate, bindings, definitionEnv, bodyScope, path));
  }

  return {
    kind: 'list',
    elements: expanded,
    pos: template.pos,
  };
}

function expandDefineTemplate(
  template: Extract<Expr, { kind: 'list' }>,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): Expr {
  const { elements } = template;

  if (elements.length < 3) {
    return {
      kind: 'list',
      elements: expandTemplateSequence(elements, bindings, definitionEnv, scope, path),
      pos: template.pos,
    };
  }

  const head = expandTemplate(elements[0], bindings, definitionEnv, scope, path);
  const targetName = symbolName(elements[1]);

  if (targetName !== undefined && !bindings.has(targetName)) {
    const bindingKey = definitionEnv.freshBindingKey(targetName);
    const definitionScope = extendScope(scope, new Map([[targetName, bindingKey]]));
    const expanded = [
      head,
      makeResolvedSymbol(targetName, elements[1].pos, { kind: 'lexical', key: bindingKey }),
    ];

    for (const valueTemplate of elements.slice(2)) {
      expanded.push(expandTemplate(valueTemplate, bindings, definitionEnv, definitionScope, path));
    }

    return {
      kind: 'list',
      elements: expanded,
      pos: template.pos,
    };
  }

  return {
    kind: 'list',
    elements: [
      head,
      expandTemplate(elements[1], bindings, definitionEnv, scope, path),
      ...elements.slice(2).map((valueTemplate) =>
        expandTemplate(valueTemplate, bindings, definitionEnv, scope, path),
      ),
    ],
    pos: template.pos,
  };
}

function expandParameterBindings(
  template: Expr,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): [Expr, Map<string, string>] {
  const parameterName = symbolName(template);
  if (template.kind !== 'list') {
    if (parameterName !== undefined && parameterName !== '.' && !bindings.has(parameterName)) {
      const bindingKey = definitionEnv.freshBindingKey(parameterName);
      return [
        makeResolvedSymbol(parameterName, template.pos, { kind: 'lexical', key: bindingKey }),
        new Map([[parameterName, bindingKey]]),
      ];
    }

    return [expandTemplate(template, bindings, definitionEnv, scope, path), new Map()];
  }

  const parameterScope = new Map<string, string>();
  const expanded: Expr[] = [];

  for (const parameterTemplate of template.elements) {
    const currentName = symbolName(parameterTemplate);
    if (currentName !== undefined && currentName !== '.' && !bindings.has(currentName)) {
      const bindingKey = definitionEnv.freshBindingKey(currentName);
      parameterScope.set(currentName, bindingKey);
      expanded.push(makeResolvedSymbol(currentName, parameterTemplate.pos, { kind: 'lexical', key: bindingKey }));
      continue;
    }

    expanded.push(expandTemplate(parameterTemplate, bindings, definitionEnv, scope, path));
  }

  return [
    {
      kind: 'list',
      elements: expanded,
      pos: template.pos,
    },
    parameterScope,
  ];
}

function expandLetTemplate(
  template: Extract<Expr, { kind: 'list' }>,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): Expr {
  const { elements } = template;

  if (elements.length < 3 || elements[1].kind !== 'list') {
    return {
      kind: 'list',
      elements: expandTemplateSequence(elements, bindings, definitionEnv, scope, path),
      pos: template.pos,
    };
  }

  const head = expandTemplate(elements[0], bindings, definitionEnv, scope, path);
  const [bindingExpr, bindingScope] = expandLetBindings(
    elements[1],
    bindings,
    definitionEnv,
    scope,
    path,
  );
  const bodyScope = extendScope(scope, bindingScope);
  const expanded = [head, bindingExpr];

  for (const bodyTemplate of elements.slice(2)) {
    expanded.push(expandTemplate(bodyTemplate, bindings, definitionEnv, bodyScope, path));
  }

  return {
    kind: 'list',
    elements: expanded,
    pos: template.pos,
  };
}

function expandLetBindings(
  template: Extract<Expr, { kind: 'list' }>,
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): [Expr, Map<string, string>] {
  const bindingScope = new Map<string, string>();
  const expanded: Expr[] = [];

  for (const bindingTemplate of template.elements) {
    if (bindingTemplate.kind !== 'list' || bindingTemplate.elements.length !== 2) {
      return [
        {
          kind: 'list',
          elements: expandTemplateSequence(template.elements, bindings, definitionEnv, scope, path),
          pos: template.pos,
        },
        new Map(),
      ];
    }

    const [bindingNameExpr, valueTemplate] = bindingTemplate.elements;
    const valueExpr = expandTemplate(valueTemplate, bindings, definitionEnv, scope, path);
    const currentName = symbolName(bindingNameExpr);

    const nameExpr =
      currentName !== undefined && !bindings.has(currentName)
        ? (() => {
            const bindingKey = definitionEnv.freshBindingKey(currentName);
            bindingScope.set(currentName, bindingKey);
            return makeResolvedSymbol(currentName, bindingNameExpr.pos, {
              kind: 'lexical',
              key: bindingKey,
            });
          })()
        : expandTemplate(bindingNameExpr, bindings, definitionEnv, scope, path);

    expanded.push({
      kind: 'list',
      elements: [nameExpr, valueExpr],
      pos: bindingTemplate.pos,
    });
  }

  return [
    {
      kind: 'list',
      elements: expanded,
      pos: template.pos,
    },
    bindingScope,
  ];
}

function expandTemplateSequence(
  templates: Expr[],
  bindings: PatternBindings,
  definitionEnv: Environment,
  scope: Map<string, string>,
  path: number[],
): Expr[] {
  const expanded: Expr[] = [];

  for (let index = 0; index < templates.length; ) {
    const template = templates[index];

    if (templates[index + 1] !== undefined && isEllipsisExpr(templates[index + 1])) {
      const repeatCount = determineTemplateRepeatCount(template, bindings, path);
      if (repeatCount === undefined) {
        throw new EvalError(
          'macro template ellipsis must follow a pattern variable',
          templates[index + 1].pos,
        );
      }

      for (let repeatIndex = 0; repeatIndex < repeatCount; repeatIndex += 1) {
        expanded.push(
          expandTemplate(template, bindings, definitionEnv, scope, [...path, repeatIndex]),
        );
      }

      index += 2;
      continue;
    }

    expanded.push(expandTemplate(template, bindings, definitionEnv, scope, path));
    index += 1;
  }

  return expanded;
}

function determineTemplateRepeatCount(
  template: Expr,
  bindings: PatternBindings,
  path: number[],
): number | undefined {
  const counts: number[] = [];

  const visit = (expression: Expr): void => {
    switch (expression.kind) {
      case 'symbol': {
        const binding = bindings.get(expression.name);
        if (binding !== undefined) {
          const resolved = bindingAtPath(binding, path, expression.pos);
          if (resolved.kind === 'repeated') {
            counts.push(resolved.value.length);
          }
        }
        return;
      }
      case 'resolved-symbol':
        return;
      case 'list':
        for (const element of expression.elements) {
          if (!isEllipsisExpr(element)) {
            visit(element);
          }
        }
        return;
      default:
        return;
    }
  };

  visit(template);

  if (counts.length === 0) {
    return undefined;
  }

  if (counts.slice(1).some((count) => count !== counts[0])) {
    throw new EvalError('macro template ellipsis matched inconsistent lengths', template.pos);
  }

  return counts[0];
}

function bindingAtPath(
  value: PatternBindingValue,
  path: number[],
  pos: SourcePosition,
): PatternBindingValue {
  if (path.length === 0) {
    return value;
  }

  if (value.kind !== 'repeated') {
    throw new EvalError('invalid macro ellipsis expansion', pos);
  }

  const next = value.value[path[0]];
  if (next === undefined) {
    throw new EvalError('invalid macro ellipsis expansion', pos);
  }

  return bindingAtPath(next, path.slice(1), pos);
}

function resolveTemplateBinding(
  value: PatternBindingValue,
  path: number[],
  pos: SourcePosition,
): Expr {
  const resolved = bindingAtPath(value, path, pos);

  if (resolved.kind === 'expr') {
    return resolved.value;
  }

  throw new EvalError('macro template is missing an ellipsis', pos);
}

function extendScope(scope: Map<string, string>, additions: Map<string, string>): Map<string, string> {
  const extended = new Map(scope);
  for (const [name, bindingKey] of additions) {
    extended.set(name, bindingKey);
  }
  return extended;
}

function makeResolvedSymbol(name: string, pos: SourcePosition, binding: SymbolBinding): Expr {
  return {
    kind: 'resolved-symbol',
    name,
    pos,
    binding,
  };
}

function isEllipsisExpr(expression: Expr): boolean {
  return symbolName(expression) === '...';
}

function quoteExpr(expression: Expr): SchemeValue {
  switch (expression.kind) {
    case 'number':
    case 'boolean':
      return expression.value;
    case 'string':
      return makeString(expression.value);
    case 'char':
      return { kind: 'char', value: expression.value };
    case 'symbol':
    case 'resolved-symbol':
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
    } else if (rawToken.startsWith('#\\')) {
      tokens.push({ kind: 'char', value: parseCharLiteral(rawToken, pos), pos });
    } else {
      const numberValue = parseNumberLiteral(rawToken, pos);
      if (numberValue !== undefined) {
        tokens.push({ kind: 'number', value: numberValue, pos });
      } else {
        tokens.push({ kind: 'symbol', value: rawToken, pos });
      }
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
      case 'char':
        return { kind: 'char', value: token.value, pos: token.pos };
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

function createPairAccessors(): Array<[string, BuiltinValue]> {
  const accessors: Array<[string, BuiltinValue]> = [];

  for (let length = 2; length <= 4; length += 1) {
    const combinations = 1 << length;

    for (let mask = 0; mask < combinations; mask += 1) {
      const operations: Array<'a' | 'd'> = [];

      for (let bit = length - 1; bit >= 0; bit -= 1) {
        operations.push(((mask >> bit) & 1) === 0 ? 'a' : 'd');
      }

      const name = `c${operations.join('')}r`;
      accessors.push([
        name,
        builtin(name, (args, pos) => {
          expectArity(name, args, 1, pos);
          let current = args[0];

          for (let index = operations.length - 1; index >= 0; index -= 1) {
            const pair = expectPair(current, name, pos);
            current = operations[index] === 'a' ? pair.car : pair.cdr;
          }

          return current;
        }),
      ]);
    }
  }

  return accessors;
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

function isNumberValue(value: SchemeValue): value is NumberValue {
  return typeof value === 'object' && value !== null && value.kind === 'number';
}

function isExactNumberValue(value: SchemeValue): value is ExactNumberValue {
  return isNumberValue(value) && value.exact;
}

function isBuiltin(value: SchemeValue): value is BuiltinValue {
  return typeof value === 'object' && value !== null && value.kind === 'builtin';
}

function isClosure(value: SchemeValue): value is ClosureValue {
  return typeof value === 'object' && value !== null && value.kind === 'closure';
}

function isCaseLambda(value: SchemeValue): value is CaseLambdaValue {
  return typeof value === 'object' && value !== null && value.kind === 'case-lambda';
}

function isProcedureValue(value: SchemeValue): value is ProcedureValue {
  return isBuiltin(value) || isClosure(value) || isCaseLambda(value);
}

function isSymbolValue(value: SchemeValue): value is SymbolValue {
  return typeof value === 'object' && value !== null && value.kind === 'symbol';
}

function isStringValue(value: SchemeValue): value is StringValue {
  return typeof value === 'object' && value !== null && value.kind === 'string';
}

function isPair(value: SchemeValue): value is PairValue {
  return typeof value === 'object' && value !== null && value.kind === 'pair';
}

function isVectorValue(value: SchemeValue): value is VectorValue {
  return typeof value === 'object' && value !== null && value.kind === 'vector';
}

function isChar(value: SchemeValue): value is CharValue {
  return typeof value === 'object' && value !== null && value.kind === 'char';
}

function isEmptyList(value: SchemeValue): value is EmptyListValue {
  return typeof value === 'object' && value !== null && value.kind === 'empty-list';
}

function isRecordValue(value: SchemeValue): value is RecordValue {
  return typeof value === 'object' && value !== null && value.kind === 'record';
}

function sameNumberLiteral(left: NumberValue, right: NumberValue): boolean {
  if (left.exact && right.exact) {
    return left.numerator === right.numerator && left.denominator === right.denominator;
  }

  if (!left.exact && !right.exact) {
    return Object.is(normalizeNumber(left.value), normalizeNumber(right.value));
  }

  return false;
}

function bigintAbs(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function bigintGcd(left: bigint, right: bigint): bigint {
  let a = bigintAbs(left);
  let b = bigintAbs(right);

  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }

  return a === 0n ? 1n : a;
}

function makeExactInteger(value: bigint): ExactNumberValue {
  return makeExactNumber(value, 1n);
}

function makeExactNumber(numerator: bigint, denominator: bigint): ExactNumberValue {
  if (denominator === 0n) {
    throw new Error('denominator must not be zero');
  }

  if (numerator === 0n) {
    return EXACT_ZERO;
  }

  let normalizedNumerator = numerator;
  let normalizedDenominator = denominator;

  if (normalizedDenominator < 0n) {
    normalizedNumerator = -normalizedNumerator;
    normalizedDenominator = -normalizedDenominator;
  }

  const divisor = bigintGcd(normalizedNumerator, normalizedDenominator);

  return {
    kind: 'number',
    exact: true,
    numerator: normalizedNumerator / divisor,
    denominator: normalizedDenominator / divisor,
  };
}

function makeInexactNumber(value: number): InexactNumberValue {
  return {
    kind: 'number',
    exact: false,
    value: normalizeNumber(value),
  };
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

function expectNumber(value: SchemeValue, name: string, pos: SourcePosition): NumberValue {
  if (!isNumberValue(value)) {
    throw new EvalError(`${name} expected a number`, pos);
  }

  return value;
}

function expectStringValue(value: SchemeValue, name: string, pos: SourcePosition): StringValue {
  if (!isStringValue(value)) {
    throw new EvalError(`${name} expected a string`, pos);
  }

  return value;
}

function expectStringContent(value: SchemeValue, name: string, pos: SourcePosition): string {
  return expectStringValue(value, name, pos).chars.join('');
}

function expectMutableString(value: SchemeValue, name: string, pos: SourcePosition): StringValue {
  const stringValue = expectStringValue(value, name, pos);

  if (!stringValue.mutable) {
    throw new EvalError(`${name} expected a mutable string`, pos);
  }

  return stringValue;
}

function expectPair(value: SchemeValue, name: string, pos: SourcePosition): PairValue {
  if (!isPair(value)) {
    throw new EvalError(`${name} expected a pair`, pos);
  }

  return value;
}

function expectVector(value: SchemeValue, name: string, pos: SourcePosition): VectorValue {
  if (!isVectorValue(value)) {
    throw new EvalError(`${name} expected a vector`, pos);
  }

  return value;
}

function expectChar(value: SchemeValue, name: string, pos: SourcePosition): CharValue {
  if (!isChar(value)) {
    throw new EvalError(`${name} expected a character`, pos);
  }

  return value;
}

function expectSymbolValue(value: SchemeValue, name: string, pos: SourcePosition): SymbolValue {
  if (!isSymbolValue(value)) {
    throw new EvalError(`${name} expected a symbol`, pos);
  }

  return value;
}

function expectRecordOfType(
  value: SchemeValue,
  recordType: RecordTypeDescriptor,
  name: string,
  pos: SourcePosition,
): RecordValue {
  if (!isRecordValue(value) || value.type !== recordType) {
    throw new EvalError(`${name} expected a ${recordType.name} record`, pos);
  }

  return value;
}

function expectIndex(value: SchemeValue, name: string, pos: SourcePosition): number {
  const numericValue = expectInteger(value, name, pos);
  const index = integerToSafeNumber(numericValue, name, pos);

  if (index < 0) {
    throw new EvalError(`${name} expected a non-negative integer`, pos);
  }

  return index;
}

function expectInteger(value: SchemeValue, name: string, pos: SourcePosition): NumberValue {
  const numericValue = expectNumber(value, name, pos);

  if (!isIntegerNumber(numericValue)) {
    throw new EvalError(`${name} expected an integer`, pos);
  }

  return numericValue;
}

function expectSymbolExpr(expression: Expr, name: string): string {
  if (expression.kind === 'symbol' || expression.kind === 'resolved-symbol') {
    return expression.name;
  }

  throw new EvalError(`${name} expected a symbol`, expression.pos);
}

function expectParameterName(expression: Expr, name: string): string {
  const parameterName = bindingName(expression, name);
  if (symbolName(expression) === '.') {
    throw new EvalError(`${name} expected a symbol`, expression.pos);
  }

  return parameterName;
}

function parseFormals(paramsExpr: Expr, name: string): { params: string[]; restParam?: string } {
  if (paramsExpr.kind === 'symbol' || paramsExpr.kind === 'resolved-symbol') {
    return {
      params: [],
      restParam: expectParameterName(paramsExpr, name),
    };
  }

  if (paramsExpr.kind !== 'list') {
    throw new EvalError(`${name} expected a parameter list`, paramsExpr.pos);
  }

  return parseFormalParameters(paramsExpr.elements, name);
}

function parseFormalParameters(
  parameterExprs: Expr[],
  name: string,
): { params: string[]; restParam?: string } {
  const params: string[] = [];

  for (let index = 0; index < parameterExprs.length; index += 1) {
    const parameterExpr = parameterExprs[index];

    if (symbolName(parameterExpr) === '.') {
      if (index === parameterExprs.length - 1) {
        throw new EvalError(`${name} expected a symbol after .`, parameterExpr.pos);
      }

      if (index !== parameterExprs.length - 2) {
        throw new EvalError(`${name} expected exactly one rest parameter after .`, parameterExpr.pos);
      }

      return {
        params,
        restParam: expectParameterName(parameterExprs[index + 1], name),
      };
    }

    params.push(expectParameterName(parameterExpr, name));
  }

  return { params };
}

function parseCaseLambdaClause(expression: Expr): ProcedureClause {
  if (expression.kind !== 'list' || expression.elements.length < 2) {
    throw new EvalError('case-lambda expected clauses of the form (formals body ...)', expression.pos);
  }

  const [paramsExpr, ...body] = expression.elements;
  const { params, restParam } = parseFormals(paramsExpr, 'case-lambda');
  return {
    params,
    restParam,
    body,
  };
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
      name: bindingName(nameExpr, name),
      valueExpr,
    };
  });
}

function parseDoBindings(bindingsExpr: Expr): DoBinding[] {
  if (bindingsExpr.kind !== 'list') {
    throw new EvalError('do expected a bindings list', bindingsExpr.pos);
  }

  return bindingsExpr.elements.map((bindingExpr) => {
    if (
      bindingExpr.kind !== 'list' ||
      bindingExpr.elements.length < 2 ||
      bindingExpr.elements.length > 3
    ) {
      throw new EvalError('do expected bindings of the form (name init [step])', bindingExpr.pos);
    }

    const [nameExpr, initExpr, stepExpr] = bindingExpr.elements;
    return {
      name: bindingName(nameExpr, 'do'),
      initExpr,
      stepExpr,
      pos: nameExpr.pos,
    };
  });
}

function evaluateSequence(expressions: Expr[], env: Environment): SchemeValue {
  return runEvaluation(evaluateSequenceStep(expressions, env));
}

function evaluateSequenceStep(expressions: Expr[], env: Environment): EvaluationStep {
  if (expressions.length === 0) {
    return valueStep(VOID);
  }

  for (let index = 0; index < expressions.length - 1; index += 1) {
    evaluate(expressions[index], env);
  }

  return exprStep(expressions[expressions.length - 1], env);
}

function listRef(value: SchemeValue, index: number, pos: SourcePosition): SchemeValue {
  let current = value;
  let remaining = index;

  while (remaining > 0) {
    if (!isPair(current)) {
      throw new EvalError('list-ref index out of bounds', pos);
    }

    current = current.cdr;
    remaining -= 1;
  }

  if (!isPair(current)) {
    throw new EvalError('list-ref index out of bounds', pos);
  }

  return current.car;
}

function listTail(value: SchemeValue, index: number, pos: SourcePosition): SchemeValue {
  let current = value;
  let remaining = index;

  while (remaining > 0) {
    if (!isPair(current)) {
      throw new EvalError('list-tail index out of bounds', pos);
    }

    current = current.cdr;
    remaining -= 1;
  }

  if (!isPair(current) && !isEmptyList(current)) {
    throw new EvalError('list-tail index out of bounds', pos);
  }

  return current;
}

function listToArray(value: SchemeValue, name: string, pos: SourcePosition): SchemeValue[] {
  const elements: SchemeValue[] = [];
  let current = value;
  const seenPairs = new Set<PairValue>();

  while (isPair(current)) {
    if (seenPairs.has(current)) {
      throw new EvalError(`${name} expected a list`, pos);
    }

    seenPairs.add(current);
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

function isProperList(value: SchemeValue): boolean {
  let current = value;
  const seenPairs = new Set<PairValue>();

  while (isPair(current)) {
    if (seenPairs.has(current)) {
      return false;
    }

    seenPairs.add(current);
    current = current.cdr;
  }

  return isEmptyList(current);
}

function assoc(key: SchemeValue, value: SchemeValue, pos: SourcePosition): SchemeValue {
  return assocWithComparator(key, value, 'assoc', pos, isEqualValue);
}

function assocWithComparator(
  key: SchemeValue,
  value: SchemeValue,
  name: string,
  pos: SourcePosition,
  comparator: (left: SchemeValue, right: SchemeValue) => boolean,
): SchemeValue {
  let current = value;
  const seenPairs = new Set<PairValue>();

  while (isPair(current)) {
    if (seenPairs.has(current)) {
      throw new EvalError(`${name} expected a list`, pos);
    }

    seenPairs.add(current);
    const entry = expectPair(current.car, name, pos);

    if (comparator(key, entry.car)) {
      return entry;
    }

    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${name} expected a list`, pos);
  }

  return false;
}

function memberWithComparator(
  key: SchemeValue,
  value: SchemeValue,
  name: string,
  pos: SourcePosition,
  comparator: (left: SchemeValue, right: SchemeValue) => boolean,
): SchemeValue {
  let current = value;
  const seenPairs = new Set<PairValue>();

  while (isPair(current)) {
    if (seenPairs.has(current)) {
      throw new EvalError(`${name} expected a list`, pos);
    }

    seenPairs.add(current);

    if (comparator(key, current.car)) {
      return current;
    }

    current = current.cdr;
  }

  if (!isEmptyList(current)) {
    throw new EvalError(`${name} expected a list`, pos);
  }

  return false;
}

function listArgumentsToArrays(lists: SchemeValue[], name: string, pos: SourcePosition): SchemeValue[][] {
  const arrays = lists.map((list) => listToArray(list, name, pos));
  const length = arrays[0].length;

  for (const array of arrays) {
    if (array.length !== length) {
      throw new EvalError(`${name} expected lists of equal length`, pos);
    }
  }

  return arrays;
}

function mapLists(procedure: SchemeValue, lists: SchemeValue[], pos: SourcePosition): SchemeValue {
  const arrays = listArgumentsToArrays(lists, 'map', pos);
  const length = arrays[0].length;

  const results: SchemeValue[] = [];

  for (let index = 0; index < length; index += 1) {
    results.push(applyProcedure(procedure, arrays.map((array) => array[index]), pos));
  }

  return listToPairs(results);
}

function forEachLists(procedure: SchemeValue, lists: SchemeValue[], pos: SourcePosition): SchemeValue {
  const arrays = listArgumentsToArrays(lists, 'for-each', pos);
  const length = arrays[0].length;

  for (let index = 0; index < length; index += 1) {
    applyProcedure(procedure, arrays.map((array) => array[index]), pos);
  }

  return VOID;
}

function numericToInexact(value: NumberValue): number {
  if (value.exact) {
    return normalizeNumber(Number(value.numerator) / Number(value.denominator));
  }

  return normalizeNumber(value.value);
}

function exactToInexact(value: NumberValue): NumberValue {
  return value.exact ? makeInexactNumber(numericToInexact(value)) : value;
}

function integerToBigInt(value: NumberValue, name: string, pos: SourcePosition): bigint {
  return exactNumberParts(expectInteger(value, name, pos), name, pos).numerator;
}

function gcdNumbers(args: SchemeValue[], pos: SourcePosition): NumberValue {
  if (args.length === 0) {
    return EXACT_ZERO;
  }

  let result = 0n;

  for (const arg of args) {
    result = bigintGcd(result, integerToBigInt(expectNumber(arg, 'gcd', pos), 'gcd', pos));
  }

  return makeExactInteger(result);
}

function lcmNumbers(args: SchemeValue[], pos: SourcePosition): NumberValue {
  if (args.length === 0) {
    return EXACT_ONE;
  }

  let result = 1n;

  for (const arg of args) {
    const value = integerToBigInt(expectNumber(arg, 'lcm', pos), 'lcm', pos);
    if (value === 0n || result === 0n) {
      result = 0n;
      continue;
    }

    result = bigintAbs((result / bigintGcd(result, value)) * value);
  }

  return makeExactInteger(result);
}

function truncateNumber(value: NumberValue): NumberValue {
  if (value.exact) {
    return makeExactInteger(value.numerator / value.denominator);
  }

  return makeInexactNumber(Math.trunc(value.value));
}

function roundNumber(value: NumberValue): NumberValue {
  if (!value.exact) {
    return makeInexactNumber(Math.round(value.value));
  }

  if (value.denominator === 1n) {
    return value;
  }

  let rounded = value.numerator / value.denominator;
  const remainder = bigintAbs(value.numerator % value.denominator);

  if (remainder * 2n >= value.denominator) {
    rounded += value.numerator >= 0n ? 1n : -1n;
  }

  return makeExactInteger(rounded);
}

function decimalStringToExact(raw: string): ExactNumberValue {
  let text = raw;
  let sign = 1n;

  if (text.startsWith('+')) {
    text = text.slice(1);
  } else if (text.startsWith('-')) {
    text = text.slice(1);
    sign = -1n;
  }

  let exponent = 0;
  const exponentMatch = text.match(/^(.*?)[eE]([+-]?\d+)$/);
  if (exponentMatch !== null) {
    text = exponentMatch[1];
    exponent = Number(exponentMatch[2]);
  }

  let integerPart = text;
  let fractionalPart = '';
  const dotIndex = text.indexOf('.');

  if (dotIndex >= 0) {
    integerPart = text.slice(0, dotIndex);
    fractionalPart = text.slice(dotIndex + 1);
  }

  if (integerPart === '') {
    integerPart = '0';
  }

  const digits = `${integerPart}${fractionalPart}`.replace(/^0+(?=\d)/, '') || '0';
  let numerator = BigInt(digits);
  let denominator = 10n ** BigInt(fractionalPart.length);

  if (exponent > 0) {
    numerator *= 10n ** BigInt(exponent);
  } else if (exponent < 0) {
    denominator *= 10n ** BigInt(-exponent);
  }

  return makeExactNumber(sign * numerator, denominator);
}

function inexactToExact(value: NumberValue, name: string, pos: SourcePosition): ExactNumberValue {
  if (value.exact) {
    return value;
  }

  if (!Number.isFinite(value.value)) {
    throw new EvalError(`${name} expected a finite number`, pos);
  }

  return decimalStringToExact(normalizeNumber(value.value).toString());
}

function exactNumberParts(value: NumberValue, name: string, pos: SourcePosition): ExactNumberValue {
  return value.exact ? value : inexactToExact(value, name, pos);
}

function isZeroNumber(value: NumberValue): boolean {
  return value.exact ? value.numerator === 0n : normalizeNumber(value.value) === 0;
}

function isIntegerNumber(value: NumberValue): boolean {
  return value.exact ? value.denominator === 1n : Number.isInteger(value.value);
}

function integerToSafeNumber(value: NumberValue, name: string, pos: SourcePosition): number {
  if (value.exact) {
    if (value.denominator !== 1n) {
      throw new EvalError(`${name} expected an integer`, pos);
    }

    if (
      value.numerator < BigInt(Number.MIN_SAFE_INTEGER) ||
      value.numerator > BigInt(Number.MAX_SAFE_INTEGER)
    ) {
      throw new EvalError(`${name} expected a safe integer`, pos);
    }

    return Number(value.numerator);
  }

  if (!Number.isSafeInteger(value.value)) {
    throw new EvalError(`${name} expected a safe integer`, pos);
  }

  return normalizeNumber(value.value);
}

function compareNumberValues(left: NumberValue, right: NumberValue): number {
  if (left.exact && right.exact) {
    const delta = left.numerator * right.denominator - right.numerator * left.denominator;
    return delta < 0n ? -1 : delta > 0n ? 1 : 0;
  }

  const leftValue = numericToInexact(left);
  const rightValue = numericToInexact(right);

  if (leftValue < rightValue) {
    return -1;
  }

  if (leftValue > rightValue) {
    return 1;
  }

  return 0;
}

function numberValuesEqual(left: NumberValue, right: NumberValue): boolean {
  return compareNumberValues(left, right) === 0;
}

function addNumberValues(left: NumberValue, right: NumberValue): NumberValue {
  if (left.exact && right.exact) {
    return makeExactNumber(
      left.numerator * right.denominator + right.numerator * left.denominator,
      left.denominator * right.denominator,
    );
  }

  return makeInexactNumber(numericToInexact(left) + numericToInexact(right));
}

function subtractNumberValues(left: NumberValue, right: NumberValue): NumberValue {
  if (left.exact && right.exact) {
    return makeExactNumber(
      left.numerator * right.denominator - right.numerator * left.denominator,
      left.denominator * right.denominator,
    );
  }

  return makeInexactNumber(numericToInexact(left) - numericToInexact(right));
}

function multiplyNumberValues(left: NumberValue, right: NumberValue): NumberValue {
  if (left.exact && right.exact) {
    return makeExactNumber(left.numerator * right.numerator, left.denominator * right.denominator);
  }

  return makeInexactNumber(numericToInexact(left) * numericToInexact(right));
}

function divideNumberValues(left: NumberValue, right: NumberValue, pos: SourcePosition): NumberValue {
  if (isZeroNumber(right)) {
    throw new EvalError('division by zero', pos);
  }

  if (left.exact && right.exact) {
    return makeExactNumber(left.numerator * right.denominator, left.denominator * right.numerator);
  }

  return makeInexactNumber(numericToInexact(left) / numericToInexact(right));
}

function absNumber(value: NumberValue): NumberValue {
  if (value.exact) {
    return makeExactNumber(bigintAbs(value.numerator), value.denominator);
  }

  return makeInexactNumber(Math.abs(value.value));
}

function minOrMax(numbers: NumberValue[], kind: 'min' | 'max'): NumberValue {
  let result = numbers[0];
  let sawInexact = !result.exact;

  for (const value of numbers.slice(1)) {
    sawInexact ||= !value.exact;
    const comparison = compareNumberValues(value, result);

    if ((kind === 'min' && comparison < 0) || (kind === 'max' && comparison > 0)) {
      result = value;
    }
  }

  return sawInexact ? makeInexactNumber(numericToInexact(result)) : result;
}

function bigintPow(base: bigint, exponent: bigint): bigint {
  let result = 1n;
  let factor = base;
  let power = exponent;

  while (power > 0n) {
    if (power % 2n === 1n) {
      result *= factor;
    }

    factor *= factor;
    power /= 2n;
  }

  return result;
}

function exptNumber(base: NumberValue, exponent: NumberValue, pos: SourcePosition): NumberValue {
  if (base.exact && exponent.exact) {
    const power = exponent.numerator;

    if (power >= 0n) {
      return makeExactNumber(bigintPow(base.numerator, power), bigintPow(base.denominator, power));
    }

    if (base.numerator === 0n) {
      throw new EvalError('division by zero', pos);
    }

    const positivePower = -power;
    return makeExactNumber(
      bigintPow(base.denominator, positivePower),
      bigintPow(base.numerator, positivePower),
    );
  }

  return makeInexactNumber(numericToInexact(base) ** integerToSafeNumber(exponent, 'expt', pos));
}

function isOddNumber(value: NumberValue): boolean {
  if (value.exact) {
    return bigintAbs(value.numerator % 2n) === 1n;
  }

  return Math.abs(value.value % 2) === 1;
}

function isEvenNumber(value: NumberValue): boolean {
  if (value.exact) {
    return value.numerator % 2n === 0n;
  }

  return value.value % 2 === 0;
}

function numberSign(value: NumberValue): number {
  return compareNumberValues(value, EXACT_ZERO);
}

function sum(args: SchemeValue[], identity: NumberValue, pos: SourcePosition): NumberValue {
  let total = identity;

  for (const arg of args) {
    total = addNumberValues(total, expectNumber(arg, '+', pos));
  }

  return total;
}

function subtract(args: SchemeValue[], pos: SourcePosition): NumberValue {
  expectAtLeastArity('-', args, 1, pos);

  if (args.length === 1) {
    return subtractNumberValues(EXACT_ZERO, expectNumber(args[0], '-', pos));
  }

  let total = expectNumber(args[0], '-', pos);

  for (const arg of args.slice(1)) {
    total = subtractNumberValues(total, expectNumber(arg, '-', pos));
  }

  return total;
}

function product(args: SchemeValue[], identity: NumberValue, pos: SourcePosition): NumberValue {
  let total = identity;

  for (const arg of args) {
    total = multiplyNumberValues(total, expectNumber(arg, '*', pos));
  }

  return total;
}

function divide(args: SchemeValue[], pos: SourcePosition): NumberValue {
  expectAtLeastArity('/', args, 2, pos);

  let total = expectNumber(args[0], '/', pos);

  for (const arg of args.slice(1)) {
    total = divideNumberValues(total, expectNumber(arg, '/', pos), pos);
  }

  return total;
}

function quotient(dividend: NumberValue, divisor: NumberValue, pos: SourcePosition): NumberValue {
  if (isZeroNumber(divisor)) {
    throw new EvalError('division by zero', pos);
  }

  if (dividend.exact && divisor.exact) {
    return makeExactInteger(dividend.numerator / divisor.numerator);
  }

  return makeInexactNumber(Math.trunc(numericToInexact(dividend) / numericToInexact(divisor)));
}

function remainder(dividend: NumberValue, divisor: NumberValue, pos: SourcePosition): NumberValue {
  if (isZeroNumber(divisor)) {
    throw new EvalError('division by zero', pos);
  }

  if (dividend.exact && divisor.exact) {
    return makeExactInteger(dividend.numerator % divisor.numerator);
  }

  return makeInexactNumber(numericToInexact(dividend) % numericToInexact(divisor));
}

function modulo(dividend: NumberValue, divisor: NumberValue, pos: SourcePosition): NumberValue {
  const result = remainder(dividend, divisor, pos);

  if (!isZeroNumber(result) && numberSign(result) !== numberSign(divisor)) {
    return addNumberValues(result, divisor);
  }

  return result;
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
    if (!predicate(numericToInexact(numbers[index]), numericToInexact(numbers[index + 1]))) {
      return false;
    }
  }

  return true;
}

function compareCharChain(
  name: string,
  args: SchemeValue[],
  predicate: (left: number, right: number) => boolean,
  pos: SourcePosition,
): boolean {
  expectAtLeastArity(name, args, 2, pos);
  const codes = args.map((arg) => charCode(expectChar(arg, name, pos).value));

  for (let index = 0; index < codes.length - 1; index += 1) {
    if (!predicate(codes[index], codes[index + 1])) {
      return false;
    }
  }

  return true;
}

function compareStringChain(
  name: string,
  args: SchemeValue[],
  predicate: (left: string, right: string) => boolean,
  pos: SourcePosition,
): boolean {
  expectAtLeastArity(name, args, 2, pos);
  const values = args.map((arg) => expectStringContent(arg, name, pos));

  for (let index = 0; index < values.length - 1; index += 1) {
    if (!predicate(values[index], values[index + 1])) {
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

function parseBenchLevel(): number | undefined {
  const rawBenchLevel = (
    globalThis as typeof globalThis & {
      process?: { env?: Record<string, string | undefined> };
    }
  ).process?.env?.BENCH_LEVEL;

  if (rawBenchLevel === undefined) {
    return undefined;
  }

  const parsedLevel = Number.parseInt(rawBenchLevel, 10);
  return Number.isNaN(parsedLevel) ? undefined : parsedLevel;
}

function stringsAreImmutable(): boolean {
  return CURRENT_BENCH_LEVEL === undefined || CURRENT_BENCH_LEVEL >= STRING_IMMUTABILITY_LEVEL;
}

function stringToChars(value: string): string[] {
  return Array.from(value);
}

function makeVector(elements: SchemeValue[]): VectorValue {
  return {
    kind: 'vector',
    elements: [...elements],
  };
}

function makeString(value: string | string[], mutable = !stringsAreImmutable()): StringValue {
  return {
    kind: 'string',
    chars: typeof value === 'string' ? stringToChars(value) : [...value],
    mutable,
  };
}

function parseCharLiteral(rawToken: string, pos: SourcePosition): string {
  const literal = rawToken.slice(2);

  if (literal === 'space') {
    return ' ';
  }

  if (literal === 'newline') {
    return '\n';
  }

  if (stringToChars(literal).length === 1) {
    return literal;
  }

  throw new EvalError('invalid character literal', pos);
}

function parseNumberLiteral(rawToken: string, pos?: SourcePosition): NumberValue | undefined {
  if (/^[+-]?\d+$/.test(rawToken)) {
    return makeExactInteger(BigInt(rawToken));
  }

  const rationalMatch = rawToken.match(/^([+-]?\d+)\/(\d+)$/);
  if (rationalMatch !== null) {
    const denominator = BigInt(rationalMatch[2]);

    if (denominator === 0n) {
      if (pos !== undefined) {
        throw new EvalError('invalid rational literal', pos);
      }

      return undefined;
    }

    return makeExactNumber(BigInt(rationalMatch[1]), denominator);
  }

  if (/^[+-]?(?:\d+\.\d*|\d*\.\d+)$/.test(rawToken)) {
    return makeInexactNumber(Number(rawToken));
  }

  return undefined;
}

function isEqValue(left: SchemeValue, right: SchemeValue): boolean {
  if (isNumberValue(left) && isNumberValue(right)) {
    return numberValuesEqual(left, right);
  }

  if (typeof left === 'boolean' && typeof right === 'boolean') {
    return left === right;
  }

  if (isEmptyList(left) && isEmptyList(right)) {
    return true;
  }

  if (isSymbolValue(left) && isSymbolValue(right)) {
    return left.name === right.name;
  }

  if (isChar(left) && isChar(right)) {
    return left.value === right.value;
  }

  return left === right;
}

function isEqualValue(left: SchemeValue, right: SchemeValue): boolean {
  return isEqualValueInternal(left, right, new WeakMap<object, WeakSet<object>>());
}

function isEqualValueInternal(
  left: SchemeValue,
  right: SchemeValue,
  seenComparisons: WeakMap<object, WeakSet<object>>,
): boolean {
  if (isEqValue(left, right)) {
    return true;
  }

  if (isStringValue(left) && isStringValue(right)) {
    return left.chars.join('') === right.chars.join('');
  }

  if (isPair(left) && isPair(right)) {
    if (hasSeenComparison(seenComparisons, left, right)) {
      return true;
    }

    markSeenComparison(seenComparisons, left, right);
    return (
      isEqualValueInternal(left.car, right.car, seenComparisons) &&
      isEqualValueInternal(left.cdr, right.cdr, seenComparisons)
    );
  }

  if (isVectorValue(left) && isVectorValue(right)) {
    if (hasSeenComparison(seenComparisons, left, right)) {
      return true;
    }

    markSeenComparison(seenComparisons, left, right);
    return (
      left.elements.length === right.elements.length &&
      left.elements.every((value, index) =>
        isEqualValueInternal(value, right.elements[index], seenComparisons),
      )
    );
  }

  return false;
}

function hasSeenComparison(
  seenComparisons: WeakMap<object, WeakSet<object>>,
  left: object,
  right: object,
): boolean {
  return seenComparisons.get(left)?.has(right) ?? false;
}

function markSeenComparison(
  seenComparisons: WeakMap<object, WeakSet<object>>,
  left: object,
  right: object,
): void {
  addSeenComparison(seenComparisons, left, right);
  addSeenComparison(seenComparisons, right, left);
}

function addSeenComparison(
  seenComparisons: WeakMap<object, WeakSet<object>>,
  left: object,
  right: object,
): void {
  let rightValues = seenComparisons.get(left);
  if (rightValues === undefined) {
    rightValues = new WeakSet<object>();
    seenComparisons.set(left, rightValues);
  }

  rightValues.add(right);
}

function charCode(value: string): number {
  return value.codePointAt(0) as number;
}

function formatNumberValue(value: NumberValue): string {
  if (value.exact) {
    return value.denominator === 1n
      ? value.numerator.toString()
      : `${value.numerator}/${value.denominator}`;
  }

  const normalized = normalizeNumber(value.value);
  const rendered = String(normalized);
  return Number.isInteger(normalized) ? `${rendered}.0` : rendered;
}

function formatValue(value: SchemeValue): string {
  return formatValueInternal(value, false, new Set<object>());
}

function formatDisplayValue(value: SchemeValue): string {
  return formatValueInternal(value, true, new Set<object>());
}

function formatValueInternal(value: SchemeValue, display: boolean, path: Set<object>): string {
  if (isNumberValue(value)) {
    return formatNumberValue(value);
  }

  if (typeof value === 'boolean') {
    return value ? '#t' : '#f';
  }

  if (isStringValue(value)) {
    return display ? value.chars.join('') : JSON.stringify(value.chars.join(''));
  }

  if (isChar(value)) {
    return display ? value.value : formatCharLiteral(value.value);
  }

  if (isBuiltin(value)) {
    return `#<procedure:${value.name}>`;
  }

  if (isClosure(value)) {
    return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
  }

  if (isCaseLambda(value)) {
    return value.name === undefined ? '#<procedure>' : `#<procedure:${value.name}>`;
  }

  if (isRecordValue(value)) {
    return `#<record:${value.type.name}>`;
  }

  if (isVectorValue(value)) {
    return formatVectorInternal(value, display, path);
  }

  if (isPair(value)) {
    return formatPairInternal(value, display, path);
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
  return formatPairInternal(pair, false, new Set<object>());
}

function formatDisplayPair(pair: PairValue): string {
  return formatPairInternal(pair, true, new Set<object>());
}

function formatPairInternal(pair: PairValue, display: boolean, path: Set<object>): string {
  if (path.has(pair)) {
    return '#<cycle>';
  }

  const parts: string[] = [];
  let current = pair;
  const pathEntries: object[] = [];

  path.add(pair);
  pathEntries.push(pair);

  try {
    while (true) {
      parts.push(formatValueInternal(current.car, display, path));
      const next = current.cdr;

      if (isPair(next)) {
        if (path.has(next)) {
          return `(${parts.join(' ')} . #<cycle>)`;
        }

        path.add(next);
        pathEntries.push(next);
        current = next;
        continue;
      }

      if (isEmptyList(next)) {
        return `(${parts.join(' ')})`;
      }

      return `(${parts.join(' ')} . ${formatValueInternal(next, display, path)})`;
    }
  } finally {
    for (const entry of pathEntries) {
      path.delete(entry);
    }
  }
}

function formatVectorInternal(vector: VectorValue, display: boolean, path: Set<object>): string {
  if (path.has(vector)) {
    return '#<cycle>';
  }

  path.add(vector);

  try {
    return formatVector(vector, (value) => formatValueInternal(value, display, path));
  } finally {
    path.delete(vector);
  }
}

function formatVector(vector: VectorValue, formatter: (value: SchemeValue) => string): string {
  return `#(${vector.elements.map((element) => formatter(element)).join(' ')})`;
}

function formatCharLiteral(value: string): string {
  if (value === ' ') {
    return '#\\space';
  }

  if (value === '\n') {
    return '#\\newline';
  }

  return `#\\${value}`;
}
