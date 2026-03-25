import { EvalError } from './evalError.js';
export function parseNumberLiteral(source, position) {
    if (/^[+-]?\d+$/.test(source)) {
        return exactInteger(BigInt(source));
    }
    const rationalMatch = source.match(/^([+-]?\d+)\/([+-]?\d+)$/);
    if (rationalMatch !== null) {
        return exactRational(BigInt(rationalMatch[1]), BigInt(rationalMatch[2]), position);
    }
    if (/^[+-]?(?:\d+\.\d+|\.\d+)$/.test(source)) {
        return inexactNumber(Number(source), position);
    }
    return null;
}
export function exactInteger(value) {
    const integerValue = typeof value === 'bigint' ? value : BigInt(Object.is(value, -0) ? 0 : value);
    return { kind: 'exact', numerator: integerValue, denominator: 1n };
}
export function exactRational(numerator, denominator, position) {
    if (denominator === 0n) {
        throw new EvalError('invalid number', position);
    }
    if (numerator === 0n) {
        return exactInteger(0n);
    }
    let normalizedNumerator = numerator;
    let normalizedDenominator = denominator;
    if (normalizedDenominator < 0n) {
        normalizedNumerator = -normalizedNumerator;
        normalizedDenominator = -normalizedDenominator;
    }
    const divisor = greatestCommonDivisor(absBigInt(normalizedNumerator), normalizedDenominator);
    return {
        kind: 'exact',
        numerator: normalizedNumerator / divisor,
        denominator: normalizedDenominator / divisor,
    };
}
export function inexactNumber(value, position) {
    if (!Number.isFinite(value)) {
        throw new EvalError('invalid number', position);
    }
    return { kind: 'inexact', value };
}
export function addNumbers(values, position) {
    if (values.some(isInexactNumber)) {
        return inexactNumber(values.reduce((sum, value) => sum + numberToJs(value), 0), position);
    }
    let result = exactInteger(0n);
    for (const value of values) {
        result = exactAdd(result, value);
    }
    return result;
}
export function subtractNumbers(values, position) {
    if (values.length === 1) {
        return negateNumber(values[0], position);
    }
    if (values.some(isInexactNumber)) {
        let result = numberToJs(values[0]);
        for (const value of values.slice(1)) {
            result -= numberToJs(value);
        }
        return inexactNumber(result, position);
    }
    let result = values[0];
    for (const value of values.slice(1)) {
        result = exactSubtract(result, value);
    }
    return result;
}
export function multiplyNumbers(values, position) {
    if (values.some(isInexactNumber)) {
        return inexactNumber(values.reduce((product, value) => product * numberToJs(value), 1), position);
    }
    let result = exactInteger(1n);
    for (const value of values) {
        result = exactMultiply(result, value);
    }
    return result;
}
export function divideNumbers(values, position) {
    if (values.length === 1) {
        const value = values[0];
        if (value.kind === 'inexact') {
            return inexactNumber(1 / value.value, position);
        }
        return exactDivide(exactInteger(1n), value);
    }
    if (values.some(isInexactNumber)) {
        let result = numberToJs(values[0]);
        for (const value of values.slice(1)) {
            result /= numberToJs(value);
        }
        return inexactNumber(result, position);
    }
    let result = values[0];
    for (const value of values.slice(1)) {
        result = exactDivide(result, value);
    }
    return result;
}
export function compareNumbers(left, right) {
    const exactLeft = toComparableExact(left);
    const exactRight = toComparableExact(right);
    const difference = exactLeft.numerator * exactRight.denominator -
        exactRight.numerator * exactLeft.denominator;
    if (difference < 0n) {
        return -1;
    }
    if (difference > 0n) {
        return 1;
    }
    return 0;
}
export function numberToJs(value) {
    if (value.kind === 'inexact') {
        return value.value;
    }
    return Number(value.numerator) / Number(value.denominator);
}
export function exactToInexact(value, position) {
    return value.kind === 'inexact' ? value : inexactNumber(numberToJs(value), position);
}
export function inexactToExact(value, position) {
    return value.kind === 'exact' ? value : exactFromFiniteNumber(value.value, position);
}
export function numeratorPart(value, position) {
    return exactInteger(toComparableExact(value, position).numerator);
}
export function denominatorPart(value, position) {
    return exactInteger(toComparableExact(value, position).denominator);
}
export function isExactNumber(value) {
    return value.kind === 'exact';
}
export function isInexactNumber(value) {
    return value.kind === 'inexact';
}
export function isIntegerNumber(value) {
    return value.kind === 'exact' ? value.denominator === 1n : Number.isInteger(value.value);
}
export function isRationalNumber(_value) {
    return true;
}
export function isZeroNumber(value) {
    return value.kind === 'exact' ? value.numerator === 0n : value.value === 0;
}
export function isPositiveNumber(value) {
    return compareNumbers(value, exactInteger(0n)) > 0;
}
export function isNegativeNumber(value) {
    return compareNumbers(value, exactInteger(0n)) < 0;
}
export function absNumber(value, position) {
    if (value.kind === 'inexact') {
        return inexactNumber(Math.abs(value.value), position);
    }
    return exactRational(absBigInt(value.numerator), value.denominator, position);
}
export function minNumber(values) {
    let result = values[0];
    for (const value of values.slice(1)) {
        if (compareNumbers(value, result) < 0) {
            result = value;
        }
    }
    return result;
}
export function maxNumber(values) {
    let result = values[0];
    for (const value of values.slice(1)) {
        if (compareNumbers(value, result) > 0) {
            result = value;
        }
    }
    return result;
}
export function exptNumber(base, exponent, position) {
    if (!Number.isInteger(exponent)) {
        throw new EvalError('expt: expected integer exponent', position);
    }
    if (base.kind === 'inexact') {
        return inexactNumber(base.value ** exponent, position);
    }
    const exponentBigInt = BigInt(exponent);
    if (exponentBigInt === 0n) {
        return exactInteger(1n);
    }
    if (exponentBigInt < 0n) {
        if (base.numerator === 0n) {
            throw new EvalError('division by zero', position);
        }
        return exactPower(exactRational(base.denominator, base.numerator, position), -exponentBigInt);
    }
    return exactPower(base, exponentBigInt);
}
export function formatNumber(value) {
    if (value.kind === 'inexact') {
        const normalized = Object.is(value.value, -0) ? 0 : value.value;
        return Number.isInteger(normalized) ? normalized.toFixed(1) : String(normalized);
    }
    if (value.denominator === 1n) {
        return value.numerator.toString();
    }
    return `${value.numerator}/${value.denominator}`;
}
export function integerToJs(value, position) {
    if (!isIntegerNumber(value)) {
        throw new EvalError('expected integer', position);
    }
    const numberValue = numberToJs(value);
    if (!Number.isFinite(numberValue)) {
        throw new EvalError('invalid number', position);
    }
    return numberValue;
}
function negateNumber(value, position) {
    if (value.kind === 'inexact') {
        return inexactNumber(-value.value, position);
    }
    return exactRational(-value.numerator, value.denominator, position);
}
function exactAdd(left, right) {
    return exactRational(left.numerator * right.denominator + right.numerator * left.denominator, left.denominator * right.denominator);
}
function exactSubtract(left, right) {
    return exactRational(left.numerator * right.denominator - right.numerator * left.denominator, left.denominator * right.denominator);
}
function exactMultiply(left, right) {
    return exactRational(left.numerator * right.numerator, left.denominator * right.denominator);
}
function exactDivide(left, right, position) {
    if (right.numerator === 0n) {
        throw new EvalError('division by zero', position);
    }
    return exactRational(left.numerator * right.denominator, left.denominator * right.numerator, position);
}
function exactPower(base, exponent) {
    return exactRational(base.numerator ** exponent, base.denominator ** exponent);
}
function toComparableExact(value, position) {
    return value.kind === 'exact' ? value : exactFromFiniteNumber(value.value, position);
}
function exactFromFiniteNumber(value, position) {
    if (!Number.isFinite(value)) {
        throw new EvalError('invalid number', position);
    }
    if (Object.is(value, -0)) {
        return exactInteger(0n);
    }
    return exactFromDecimalText(value.toString(), position);
}
function exactFromDecimalText(text, position) {
    const match = text.match(/^([+-]?)(?:(\d+)(?:\.(\d*))?|\.(\d+))(?:e([+-]?\d+))?$/i);
    if (match === null) {
        throw new EvalError('invalid number', position);
    }
    const sign = match[1] === '-' ? -1n : 1n;
    const integerPart = match[2] ?? '0';
    const fractionalPart = match[3] ?? match[4] ?? '';
    const exponent = match[5] === undefined ? 0 : Number.parseInt(match[5], 10);
    const digits = `${integerPart}${fractionalPart}`.replace(/^0+(?=\d)/, '');
    let numerator = BigInt(digits === '' ? '0' : digits);
    let denominator = 1n;
    const scale = fractionalPart.length - exponent;
    if (scale > 0) {
        denominator = 10n ** BigInt(scale);
    }
    else if (scale < 0) {
        numerator *= 10n ** BigInt(-scale);
    }
    return exactRational(sign * numerator, denominator, position);
}
function absBigInt(value) {
    return value < 0n ? -value : value;
}
function greatestCommonDivisor(left, right) {
    let a = left;
    let b = right;
    while (b !== 0n) {
        const next = a % b;
        a = b;
        b = next;
    }
    return a;
}
