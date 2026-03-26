import { EvalError } from './evalError.js';

export type ExactNumber = {
  exact: true;
  numerator: bigint;
  denominator: bigint;
};

export type InexactNumber = {
  exact: false;
  value: number;
};

export type SchemeNumber = ExactNumber | InexactNumber;

const INTEGER_RE = /^[+-]?\d+$/;
const RATIONAL_RE = /^([+-]?\d+)\/(\d+)$/;
const DECIMAL_RE =
  /^[+-]?(?:(?:\d+\.\d*)|(?:\.\d+)|(?:\d+(?:\.\d*)?[eE][+-]?\d+)|(?:\.\d+[eE][+-]?\d+))$/;
const DECIMAL_OR_SCIENTIFIC_RE =
  /^([+-]?)(?:(\d+)(?:\.(\d*))?|\.(\d+))(?:[eE]([+-]?\d+))?$/;

export function parseNumberLiteral(text: string): SchemeNumber | undefined {
  if (INTEGER_RE.test(text)) {
    return makeExactInteger(BigInt(text));
  }

  const rationalMatch = text.match(RATIONAL_RE);
  if (rationalMatch !== null) {
    return makeExactRational(BigInt(rationalMatch[1]), BigInt(rationalMatch[2]));
  }

  if (DECIMAL_RE.test(text)) {
    return makeInexact(Number(text));
  }

  return undefined;
}

export function parseNumberStringValue(text: string): SchemeNumber | undefined {
  return parseNumberLiteral(text.trim());
}

export function makeExactInteger(value: bigint | number): SchemeNumber {
  return makeExactRational(typeof value === 'bigint' ? value : BigInt(value), 1n);
}

export function makeExactRational(numerator: bigint, denominator: bigint): SchemeNumber {
  if (denominator === 0n) {
    throw new EvalError('division by zero');
  }

  if (numerator === 0n) {
    return {
      exact: true,
      numerator: 0n,
      denominator: 1n,
    };
  }

  let normalizedNumerator = numerator;
  let normalizedDenominator = denominator;

  if (normalizedDenominator < 0n) {
    normalizedNumerator = -normalizedNumerator;
    normalizedDenominator = -normalizedDenominator;
  }

  const divisor = gcd(absBigInt(normalizedNumerator), normalizedDenominator);
  return {
    exact: true,
    numerator: normalizedNumerator / divisor,
    denominator: normalizedDenominator / divisor,
  };
}

export function makeInexact(value: number): SchemeNumber {
  return {
    exact: false,
    value: Object.is(value, -0) ? 0 : value,
  };
}

export function isExactNumber(value: SchemeNumber): value is ExactNumber {
  return value.exact;
}

export function isInexactNumber(value: SchemeNumber): value is InexactNumber {
  return !value.exact;
}

export function isIntegerNumber(value: SchemeNumber): boolean {
  return value.exact ? value.denominator === 1n : Number.isInteger(value.value);
}

export function isRationalNumber(value: SchemeNumber): boolean {
  return value.exact;
}

export function addNumbers(values: SchemeNumber[]): SchemeNumber {
  if (values.some(isInexactNumber)) {
    return makeInexact(values.reduce((sum, value) => sum + numberToJsNumber(value), 0));
  }

  const exactValues = values as ExactNumber[];
  return exactValues.reduce(
    (sum, value) =>
      makeExactRational(
        sum.numerator * value.denominator + value.numerator * sum.denominator,
        sum.denominator * value.denominator,
      ) as ExactNumber,
    makeExactInteger(0) as ExactNumber,
  );
}

export function subtractNumbers(values: SchemeNumber[]): SchemeNumber {
  if (values.length === 0) {
    throw new EvalError('- expects at least 1 argument');
  }

  if (values.some(isInexactNumber)) {
    const inexactValues = values.map((value) => numberToJsNumber(value));
    if (inexactValues.length === 1) {
      return makeInexact(-inexactValues[0]);
    }

    const [first, ...rest] = inexactValues;
    return makeInexact(rest.reduce((result, value) => result - value, first));
  }

  const [first, ...rest] = values as ExactNumber[];
  if (rest.length === 0) {
    return makeExactRational(-first.numerator, first.denominator);
  }

  return rest.reduce(
    (result, value) =>
      makeExactRational(
        result.numerator * value.denominator - value.numerator * result.denominator,
        result.denominator * value.denominator,
      ) as ExactNumber,
    first,
  );
}

export function multiplyNumbers(values: SchemeNumber[]): SchemeNumber {
  if (values.some(isInexactNumber)) {
    return makeInexact(values.reduce((product, value) => product * numberToJsNumber(value), 1));
  }

  const exactValues = values as ExactNumber[];
  return exactValues.reduce(
    (product, value) =>
      makeExactRational(
        product.numerator * value.numerator,
        product.denominator * value.denominator,
      ) as ExactNumber,
    makeExactInteger(1) as ExactNumber,
  );
}

export function divideNumbers(values: SchemeNumber[]): SchemeNumber {
  if (values.length === 0) {
    throw new EvalError('/ expects at least 1 argument');
  }

  if (values.some(isInexactNumber)) {
    const inexactValues = values.map((value) => numberToJsNumber(value));
    if (inexactValues.length === 1) {
      if (inexactValues[0] === 0) {
        throw new EvalError('division by zero');
      }

      return makeInexact(1 / inexactValues[0]);
    }

    const [first, ...rest] = inexactValues;
    let result = first;

    for (const value of rest) {
      if (value === 0) {
        throw new EvalError('division by zero');
      }
      result /= value;
    }

    return makeInexact(result);
  }

  const exactValues = values as ExactNumber[];
  if (exactValues.length === 1) {
    if (exactValues[0].numerator === 0n) {
      throw new EvalError('division by zero');
    }

    return makeExactRational(exactValues[0].denominator, exactValues[0].numerator);
  }

  const [first, ...rest] = exactValues;
  let result = first;

  for (const value of rest) {
    if (value.numerator === 0n) {
      throw new EvalError('division by zero');
    }

    result = makeExactRational(
      result.numerator * value.denominator,
      result.denominator * value.numerator,
    ) as ExactNumber;
  }

  return result;
}

export function absNumber(value: SchemeNumber): SchemeNumber {
  return value.exact
    ? makeExactRational(absBigInt(value.numerator), value.denominator)
    : makeInexact(Math.abs(value.value));
}

export function integerDivision(
  left: SchemeNumber,
  right: SchemeNumber,
  operation: 'modulo' | 'remainder' | 'quotient',
): SchemeNumber {
  if (!isIntegerNumber(left) || !isIntegerNumber(right)) {
    throw new EvalError(`${operation} expects integer arguments`);
  }

  if (left.exact && right.exact) {
    const dividend = left.numerator;
    const divisor = right.numerator;

    if (divisor === 0n) {
      throw new EvalError('division by zero');
    }

    switch (operation) {
      case 'quotient':
        return makeExactInteger(dividend / divisor);
      case 'remainder':
        return makeExactInteger(dividend % divisor);
      case 'modulo': {
        let result = dividend % divisor;
        if (result !== 0n && bigintSign(result) !== bigintSign(divisor)) {
          result += divisor;
        }
        return makeExactInteger(result);
      }
    }
  }

  const dividend = numberToJsNumber(left);
  const divisor = numberToJsNumber(right);

  if (divisor === 0) {
    throw new EvalError('division by zero');
  }

  switch (operation) {
    case 'quotient':
      return makeInexact(Math.trunc(dividend / divisor));
    case 'remainder':
      return makeInexact(dividend % divisor);
    case 'modulo': {
      let result = dividend % divisor;
      if (result !== 0 && Math.sign(result) !== Math.sign(divisor)) {
        result += divisor;
      }
      return makeInexact(result);
    }
  }
}

export function minNumbers(values: SchemeNumber[]): SchemeNumber {
  return selectExtremum(values, (candidate, current) => compareNumbers(candidate, current) < 0);
}

export function maxNumbers(values: SchemeNumber[]): SchemeNumber {
  return selectExtremum(values, (candidate, current) => compareNumbers(candidate, current) > 0);
}

export function exptNumber(base: SchemeNumber, exponent: SchemeNumber): SchemeNumber {
  if (!isIntegerNumber(exponent)) {
    throw new EvalError('expt expects integer arguments');
  }

  if (base.exact && exponent.exact) {
    const power = exponent.numerator;
    if (power === 0n) {
      return makeExactInteger(1);
    }

    if (power > 0n) {
      return makeExactRational(
        powBigInt(base.numerator, power),
        powBigInt(base.denominator, power),
      );
    }

    if (base.numerator === 0n) {
      throw new EvalError('division by zero');
    }

    const absolutePower = -power;
    return makeExactRational(
      powBigInt(base.denominator, absolutePower),
      powBigInt(base.numerator, absolutePower),
    );
  }

  return makeInexact(numberToJsNumber(base) ** numberToJsNumber(exponent));
}

export function compareNumbers(left: SchemeNumber, right: SchemeNumber): -1 | 0 | 1 {
  if (left.exact && right.exact) {
    const comparison = left.numerator * right.denominator - right.numerator * left.denominator;
    if (comparison < 0n) {
      return -1;
    }
    if (comparison > 0n) {
      return 1;
    }
    return 0;
  }

  const difference = numberToJsNumber(left) - numberToJsNumber(right);
  if (difference < 0) {
    return -1;
  }
  if (difference > 0) {
    return 1;
  }
  return 0;
}

export function equalNumbers(left: SchemeNumber, right: SchemeNumber): boolean {
  return compareNumbers(left, right) === 0;
}

export function sameNumericSyntax(left: SchemeNumber, right: SchemeNumber): boolean {
  if (left.exact !== right.exact) {
    return false;
  }

  if (left.exact) {
    const exactRight = right as ExactNumber;
    return left.numerator === exactRight.numerator && left.denominator === exactRight.denominator;
  }

  return left.value === (right as InexactNumber).value;
}

export function numberToJsNumber(value: SchemeNumber): number {
  return value.exact ? Number(value.numerator) / Number(value.denominator) : value.value;
}

export function exactToInexact(value: SchemeNumber): SchemeNumber {
  return value.exact ? makeInexact(numberToJsNumber(value)) : value;
}

export function inexactToExact(value: SchemeNumber): SchemeNumber {
  if (value.exact) {
    return value;
  }

  if (!Number.isFinite(value.value)) {
    throw new EvalError('inexact->exact expects a finite number');
  }

  return decimalTextToExact(value.value.toString());
}

export function numeratorValue(value: SchemeNumber): SchemeNumber {
  if (!value.exact) {
    throw new EvalError('numerator expects an exact number');
  }

  return makeExactInteger(value.numerator);
}

export function denominatorValue(value: SchemeNumber): SchemeNumber {
  if (!value.exact) {
    throw new EvalError('denominator expects an exact number');
  }

  return makeExactInteger(value.denominator);
}

export function formatNumber(value: SchemeNumber): string {
  if (value.exact) {
    return value.denominator === 1n
      ? value.numerator.toString(10)
      : `${value.numerator.toString(10)}/${value.denominator.toString(10)}`;
  }

  return Number.isInteger(value.value) ? `${value.value.toString(10)}.0` : String(value.value);
}

function selectExtremum(
  values: SchemeNumber[],
  shouldReplace: (candidate: SchemeNumber, current: SchemeNumber) => boolean,
): SchemeNumber {
  if (values.length === 0) {
    throw new EvalError('expected at least 1 numeric argument');
  }

  let result = values[0];
  for (const value of values.slice(1)) {
    if (shouldReplace(value, result)) {
      result = value;
    }
  }

  return result;
}

function decimalTextToExact(text: string): SchemeNumber {
  const match = text.match(DECIMAL_OR_SCIENTIFIC_RE);
  if (match === null) {
    throw new EvalError('cannot convert inexact number to exact');
  }

  const sign = match[1] === '-' ? -1n : 1n;
  const integerDigits = match[2] ?? '0';
  const fractionDigits = match[3] ?? match[4] ?? '';
  const exponent = Number.parseInt(match[5] ?? '0', 10);
  const significantDigits = trimLeadingZeros(`${integerDigits}${fractionDigits}`) || '0';

  let numerator = BigInt(significantDigits);
  let scale = fractionDigits.length;

  if (exponent >= 0) {
    if (exponent >= scale) {
      numerator *= 10n ** BigInt(exponent - scale);
      scale = 0;
    } else {
      scale -= exponent;
    }
  } else {
    scale += -exponent;
  }

  if (sign < 0n) {
    numerator = -numerator;
  }

  return makeExactRational(numerator, 10n ** BigInt(scale));
}

function gcd(left: bigint, right: bigint): bigint {
  let a = left;
  let b = right;

  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }

  return a;
}

function absBigInt(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function bigintSign(value: bigint): -1 | 0 | 1 {
  if (value < 0n) {
    return -1;
  }
  if (value > 0n) {
    return 1;
  }
  return 0;
}

function powBigInt(base: bigint, exponent: bigint): bigint {
  let result = 1n;
  let currentBase = base;
  let currentExponent = exponent;

  while (currentExponent > 0n) {
    if (currentExponent % 2n === 1n) {
      result *= currentBase;
    }

    currentExponent /= 2n;
    if (currentExponent > 0n) {
      currentBase *= currentBase;
    }
  }

  return result;
}

function trimLeadingZeros(value: string): string {
  return value.replace(/^0+(?=\d)/, '');
}
