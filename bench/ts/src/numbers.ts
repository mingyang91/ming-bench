import { EvalError } from './evalError.js';

export type ExactIntegerValue = { kind: 'exact-integer'; value: bigint };
export type ExactRationalValue = { kind: 'exact-rational'; numerator: bigint; denominator: bigint };
export type InexactNumberValue = { kind: 'inexact-number'; value: number };
export type NumericValue = ExactIntegerValue | ExactRationalValue | InexactNumberValue;

type ExactNumericValue = ExactIntegerValue | ExactRationalValue;
type ExactFraction = { numerator: bigint; denominator: bigint };

const INTEGER_PATTERN = /^[+-]?\d+$/;
const RATIONAL_PATTERN = /^([+-]?\d+)\/([+-]?\d+)$/;
const INEXACT_PATTERN = /^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$/;

export function isNumericValue(value: unknown): value is NumericValue {
  if (typeof value !== 'object' || value === null) {
    return false;
  }

  const candidate = value as Record<string, unknown>;
  return (
    (candidate.kind === 'exact-integer' && typeof candidate.value === 'bigint') ||
    (candidate.kind === 'exact-rational' &&
      typeof candidate.numerator === 'bigint' &&
      typeof candidate.denominator === 'bigint') ||
    (candidate.kind === 'inexact-number' && typeof candidate.value === 'number')
  );
}

export function isExactNumeric(value: NumericValue): value is ExactNumericValue {
  return value.kind !== 'inexact-number';
}

export function isInexactNumeric(value: NumericValue): value is InexactNumberValue {
  return value.kind === 'inexact-number';
}

export function parseNumberToken(token: string): NumericValue | undefined {
  if (token === '+' || token === '-') {
    return undefined;
  }

  if (INTEGER_PATTERN.test(token)) {
    return makeExactInteger(BigInt(token));
  }

  const rationalMatch = token.match(RATIONAL_PATTERN);
  if (rationalMatch !== null) {
    const numerator = BigInt(rationalMatch[1]!);
    const denominator = BigInt(rationalMatch[2]!);
    if (denominator === 0n) {
      throw new EvalError('invalid rational literal');
    }
    return makeExactRational(numerator, denominator);
  }

  if (INEXACT_PATTERN.test(token)) {
    return makeInexactNumber(Number(token));
  }

  return undefined;
}

export function parseStringNumber(value: string): NumericValue | boolean {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    return false;
  }

  return parseNumberToken(trimmed) ?? false;
}

export function formatNumber(value: NumericValue): string {
  switch (value.kind) {
    case 'exact-integer':
      return value.value.toString();

    case 'exact-rational':
      return `${value.numerator.toString()}/${value.denominator.toString()}`;

    case 'inexact-number': {
      const normalized = normalizeInexact(value.value);
      const text = String(normalized);
      if (Number.isInteger(normalized) && !text.includes('e') && !text.includes('E')) {
        return `${text}.0`;
      }
      return text;
    }
  }
}

export function numericEqual(left: NumericValue, right: NumericValue): boolean {
  return numericCompare(left, right) === 0;
}

export function numericCompare(left: NumericValue, right: NumericValue): number {
  if (isExactNumeric(left) && isExactNumeric(right)) {
    const leftFraction = asExactFraction(left);
    const rightFraction = asExactFraction(right);
    const leftScaled = leftFraction.numerator * rightFraction.denominator;
    const rightScaled = rightFraction.numerator * leftFraction.denominator;
    if (leftScaled < rightScaled) {
      return -1;
    }
    if (leftScaled > rightScaled) {
      return 1;
    }
    return 0;
  }

  const leftValue = numericToNumber(left);
  const rightValue = numericToNumber(right);
  if (leftValue < rightValue) {
    return -1;
  }
  if (leftValue > rightValue) {
    return 1;
  }
  return 0;
}

export function numericToNumber(value: NumericValue): number {
  switch (value.kind) {
    case 'exact-integer':
      return Number(value.value);

    case 'exact-rational':
      return Number(value.numerator) / Number(value.denominator);

    case 'inexact-number':
      return normalizeInexact(value.value);
  }
}

export function numericIsInteger(value: NumericValue): boolean {
  switch (value.kind) {
    case 'exact-integer':
      return true;

    case 'exact-rational':
      return false;

    case 'inexact-number':
      return Number.isInteger(normalizeInexact(value.value));
  }
}

export function exactIntegerFromNumber(value: number): NumericValue {
  if (!Number.isInteger(value)) {
    throw new EvalError('expected exact integer');
  }

  return makeExactInteger(BigInt(value));
}

export function numericIsOdd(value: NumericValue): boolean {
  switch (value.kind) {
    case 'exact-integer':
      return absBigInt(value.value % 2n) === 1n;

    case 'exact-rational':
      return false;

    case 'inexact-number': {
      const normalized = normalizeInexact(value.value);
      return Number.isInteger(normalized) && Math.abs(normalized % 2) === 1;
    }
  }
}

export function numericIsEven(value: NumericValue): boolean {
  switch (value.kind) {
    case 'exact-integer':
      return value.value % 2n === 0n;

    case 'exact-rational':
      return false;

    case 'inexact-number': {
      const normalized = normalizeInexact(value.value);
      return Number.isInteger(normalized) && normalized % 2 === 0;
    }
  }
}

export function numericIsZero(value: NumericValue): boolean {
  switch (value.kind) {
    case 'exact-integer':
      return value.value === 0n;

    case 'exact-rational':
      return value.numerator === 0n;

    case 'inexact-number':
      return normalizeInexact(value.value) === 0;
  }
}

export function numericIsPositive(value: NumericValue): boolean {
  switch (value.kind) {
    case 'exact-integer':
      return value.value > 0n;

    case 'exact-rational':
      return value.numerator > 0n;

    case 'inexact-number':
      return normalizeInexact(value.value) > 0;
  }
}

export function numericIsNegative(value: NumericValue): boolean {
  switch (value.kind) {
    case 'exact-integer':
      return value.value < 0n;

    case 'exact-rational':
      return value.numerator < 0n;

    case 'inexact-number':
      return normalizeInexact(value.value) < 0;
  }
}

export function absNumeric(value: NumericValue): NumericValue {
  switch (value.kind) {
    case 'exact-integer':
      return makeExactInteger(absBigInt(value.value));

    case 'exact-rational':
      return makeExactRational(absBigInt(value.numerator), value.denominator);

    case 'inexact-number':
      return makeInexactNumber(Math.abs(value.value));
  }
}

export function sumNumeric(values: NumericValue[]): NumericValue {
  if (values.some(isInexactNumeric)) {
    return makeInexactNumber(values.reduce((sum, value) => sum + numericToNumber(value), 0));
  }

  let numerator = 0n;
  let denominator = 1n;
  for (const value of values) {
    const fraction = asExactFraction(value as ExactNumericValue);
    numerator = numerator * fraction.denominator + fraction.numerator * denominator;
    denominator *= fraction.denominator;
    const reduced = reduceFraction(numerator, denominator);
    numerator = reduced.numerator;
    denominator = reduced.denominator;
  }

  return makeExactRational(numerator, denominator);
}

export function productNumeric(values: NumericValue[]): NumericValue {
  if (values.some(isInexactNumeric)) {
    return makeInexactNumber(values.reduce((product, value) => product * numericToNumber(value), 1));
  }

  let numerator = 1n;
  let denominator = 1n;
  for (const value of values) {
    const fraction = asExactFraction(value as ExactNumericValue);
    numerator *= fraction.numerator;
    denominator *= fraction.denominator;
    const reduced = reduceFraction(numerator, denominator);
    numerator = reduced.numerator;
    denominator = reduced.denominator;
  }

  return makeExactRational(numerator, denominator);
}

export function subtractNumeric(values: NumericValue[]): NumericValue {
  if (values.some(isInexactNumeric)) {
    if (values.length === 1) {
      return makeInexactNumber(-numericToNumber(values[0]!));
    }

    let result = numericToNumber(values[0]!);
    for (const value of values.slice(1)) {
      result -= numericToNumber(value);
    }
    return makeInexactNumber(result);
  }

  if (values.length === 1) {
    const fraction = asExactFraction(values[0]! as ExactNumericValue);
    return makeExactRational(-fraction.numerator, fraction.denominator);
  }

  let result = asExactFraction(values[0]! as ExactNumericValue);
  for (const value of values.slice(1)) {
    const fraction = asExactFraction(value as ExactNumericValue);
    result = reduceFraction(
      result.numerator * fraction.denominator - fraction.numerator * result.denominator,
      result.denominator * fraction.denominator,
    );
  }

  return makeExactRational(result.numerator, result.denominator);
}

export function divideNumeric(values: NumericValue[]): NumericValue {
  if (values.some(isInexactNumeric)) {
    if (values.length === 1) {
      if (numericIsZero(values[0]!)) {
        throw new EvalError('division by zero');
      }
      return makeInexactNumber(1 / numericToNumber(values[0]!));
    }

    let result = numericToNumber(values[0]!);
    for (const value of values.slice(1)) {
      if (numericIsZero(value)) {
        throw new EvalError('division by zero');
      }
      result /= numericToNumber(value);
    }
    return makeInexactNumber(result);
  }

  if (values.length === 1) {
    const fraction = asExactFraction(values[0]! as ExactNumericValue);
    if (fraction.numerator === 0n) {
      throw new EvalError('division by zero');
    }
    return makeExactRational(fraction.denominator, fraction.numerator);
  }

  let result = asExactFraction(values[0]! as ExactNumericValue);
  for (const value of values.slice(1)) {
    const fraction = asExactFraction(value as ExactNumericValue);
    if (fraction.numerator === 0n) {
      throw new EvalError('division by zero');
    }
    result = reduceFraction(
      result.numerator * fraction.denominator,
      result.denominator * fraction.numerator,
    );
  }

  return makeExactRational(result.numerator, result.denominator);
}

export function quotientNumeric(dividend: NumericValue, divisor: NumericValue): NumericValue {
  if (!numericIsInteger(dividend) || !numericIsInteger(divisor)) {
    throw new EvalError('quotient expects an integer');
  }

  if (numericIsZero(divisor)) {
    throw new EvalError('division by zero');
  }

  if (isExactNumeric(dividend) && isExactNumeric(divisor)) {
    return makeExactInteger(exactIntegerValue(dividend) / exactIntegerValue(divisor));
  }

  return makeInexactNumber(Math.trunc(numericToNumber(dividend) / numericToNumber(divisor)));
}

export function remainderNumeric(dividend: NumericValue, divisor: NumericValue): NumericValue {
  if (!numericIsInteger(dividend) || !numericIsInteger(divisor)) {
    throw new EvalError('remainder expects an integer');
  }

  if (numericIsZero(divisor)) {
    throw new EvalError('division by zero');
  }

  if (isExactNumeric(dividend) && isExactNumeric(divisor)) {
    return makeExactInteger(exactIntegerValue(dividend) % exactIntegerValue(divisor));
  }

  return makeInexactNumber(numericToNumber(dividend) % numericToNumber(divisor));
}

export function moduloNumeric(dividend: NumericValue, divisor: NumericValue): NumericValue {
  if (!numericIsInteger(dividend) || !numericIsInteger(divisor)) {
    throw new EvalError('modulo expects an integer');
  }

  if (numericIsZero(divisor)) {
    throw new EvalError('division by zero');
  }

  if (isExactNumeric(dividend) && isExactNumeric(divisor)) {
    const dividendValue = exactIntegerValue(dividend);
    const divisorValue = exactIntegerValue(divisor);
    const remainder = dividendValue % divisorValue;
    if (remainder === 0n) {
      return makeExactInteger(0n);
    }
    return makeExactInteger(
      signBigInt(remainder) === signBigInt(divisorValue) ? remainder : remainder + divisorValue,
    );
  }

  const dividendValue = numericToNumber(dividend);
  const divisorValue = numericToNumber(divisor);
  const remainder = dividendValue % divisorValue;
  if (remainder === 0) {
    return makeInexactNumber(0);
  }
  return makeInexactNumber(
    Math.sign(remainder) === Math.sign(divisorValue) ? remainder : remainder + divisorValue,
  );
}

export function exptNumeric(base: NumericValue, exponent: NumericValue): NumericValue {
  if (!numericIsInteger(exponent)) {
    throw new EvalError('expt expects an integer');
  }

  if (isExactNumeric(base) && isExactNumeric(exponent)) {
    const exponentValue = exactIntegerValue(exponent);
    if (exponentValue === 0n) {
      return makeExactInteger(1n);
    }

    const baseFraction = asExactFraction(base);
    if (exponentValue < 0n) {
      if (baseFraction.numerator === 0n) {
        throw new EvalError('division by zero');
      }
      const power = powExactFraction(baseFraction, -exponentValue);
      return makeExactRational(power.denominator, power.numerator);
    }

    const power = powExactFraction(baseFraction, exponentValue);
    return makeExactRational(power.numerator, power.denominator);
  }

  return makeInexactNumber(Math.pow(numericToNumber(base), numericToNumber(exponent)));
}

export function exactToInexact(value: NumericValue): NumericValue {
  return makeInexactNumber(numericToNumber(value));
}

export function inexactToExact(value: NumericValue): NumericValue {
  if (isExactNumeric(value)) {
    return value;
  }

  const text = String(normalizeInexact(value.value));
  const parsed = exactFromInexactText(text);
  return parsed;
}

export function numeratorOf(value: NumericValue): NumericValue {
  switch (value.kind) {
    case 'exact-integer':
      return value;

    case 'exact-rational':
      return makeExactInteger(value.numerator);

    case 'inexact-number':
      return numeratorOf(inexactToExact(value));
  }
}

export function denominatorOf(value: NumericValue): NumericValue {
  switch (value.kind) {
    case 'exact-integer':
      return makeExactInteger(1n);

    case 'exact-rational':
      return makeExactInteger(value.denominator);

    case 'inexact-number':
      return denominatorOf(inexactToExact(value));
  }
}

function makeExactInteger(value: bigint): ExactIntegerValue {
  return { kind: 'exact-integer', value };
}

function makeInexactNumber(value: number): InexactNumberValue {
  return { kind: 'inexact-number', value: normalizeInexact(value) };
}

function makeExactRational(numerator: bigint, denominator: bigint): NumericValue {
  if (denominator === 0n) {
    throw new EvalError('division by zero');
  }

  let reduced = reduceFraction(numerator, denominator);
  if (reduced.denominator < 0n) {
    reduced = {
      numerator: -reduced.numerator,
      denominator: -reduced.denominator,
    };
  }

  if (reduced.denominator === 1n) {
    return makeExactInteger(reduced.numerator);
  }

  return {
    kind: 'exact-rational',
    numerator: reduced.numerator,
    denominator: reduced.denominator,
  };
}

function reduceFraction(numerator: bigint, denominator: bigint): ExactFraction {
  if (numerator === 0n) {
    return { numerator: 0n, denominator: 1n };
  }

  const divisor = gcd(absBigInt(numerator), absBigInt(denominator));
  return {
    numerator: numerator / divisor,
    denominator: denominator / divisor,
  };
}

function gcd(left: bigint, right: bigint): bigint {
  let a = left;
  let b = right;
  while (b !== 0n) {
    const next = a % b;
    a = b;
    b = next;
  }
  return a;
}

function absBigInt(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function signBigInt(value: bigint): number {
  if (value < 0n) {
    return -1;
  }
  if (value > 0n) {
    return 1;
  }
  return 0;
}

function asExactFraction(value: ExactNumericValue): ExactFraction {
  switch (value.kind) {
    case 'exact-integer':
      return { numerator: value.value, denominator: 1n };

    case 'exact-rational':
      return { numerator: value.numerator, denominator: value.denominator };
  }
}

function exactIntegerValue(value: NumericValue): bigint {
  if (value.kind === 'exact-integer') {
    return value.value;
  }

  throw new EvalError('expected exact integer');
}

function powExactFraction(value: ExactFraction, exponent: bigint): ExactFraction {
  let remaining = exponent;
  let base = value;
  let result: ExactFraction = { numerator: 1n, denominator: 1n };

  while (remaining > 0n) {
    if (remaining % 2n === 1n) {
      result = reduceFraction(
        result.numerator * base.numerator,
        result.denominator * base.denominator,
      );
    }

    remaining /= 2n;
    if (remaining > 0n) {
      base = reduceFraction(base.numerator * base.numerator, base.denominator * base.denominator);
    }
  }

  return result;
}

function exactFromInexactText(text: string): NumericValue {
  const match = text.match(/^([+-]?)(?:(\d+)(?:\.(\d*))?|\.(\d+))(?:[eE]([+-]?\d+))?$/);
  if (match === null) {
    throw new EvalError('inexact->exact expects a finite number');
  }

  const sign = match[1] === '-' ? -1n : 1n;
  const intPart = match[2] ?? '0';
  const fractionPart = match[3] ?? match[4] ?? '';
  const exponent = match[5] === undefined ? 0 : Number.parseInt(match[5], 10);
  const digits = `${intPart}${fractionPart}`.replace(/^0+(?=\d)/, '') || '0';

  let numerator = sign * BigInt(digits);
  let denominator = 1n;

  const scale = fractionPart.length - exponent;
  if (scale > 0) {
    denominator = 10n ** BigInt(scale);
  } else if (scale < 0) {
    numerator *= 10n ** BigInt(-scale);
  }

  return makeExactRational(numerator, denominator);
}

function normalizeInexact(value: number): number {
  return Object.is(value, -0) ? 0 : value;
}
