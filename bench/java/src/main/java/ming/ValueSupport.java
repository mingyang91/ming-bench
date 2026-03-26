package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

final class ValueSupport {
    private ValueSupport() {
    }

    static long requireInt(Value value, String operator) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError(operator + " expects numeric arguments");
    }

    static Value requireNumericValue(Value value, String operator) throws EvalError {
        if (isNumber(value)) {
            return value;
        }
        throw new EvalError(operator + " expects numeric arguments");
    }

    static double requireNumberAsDouble(Value value, String operator) throws EvalError {
        return switch (requireNumericValue(value, operator)) {
            case IntValue intValue -> (double) intValue.value();
            case RationalValue rationalValue ->
                    (double) rationalValue.numerator() / (double) rationalValue.denominator();
            case InexactValue inexactValue -> inexactValue.value();
            default -> throw new IllegalStateException("non-numeric value");
        };
    }

    static ExactRational requireExactRational(Value value, String operator) throws EvalError {
        return switch (value) {
            case IntValue intValue -> new ExactRational(intValue.value(), 1L);
            case RationalValue rationalValue ->
                    new ExactRational(rationalValue.numerator(), rationalValue.denominator());
            case InexactValue ignored -> throw new EvalError(operator + " expects numeric arguments");
            default -> throw new EvalError(operator + " expects numeric arguments");
        };
    }

    static ExactRational requireRationalParts(Value value, String operator) throws EvalError {
        return switch (value) {
            case IntValue intValue -> new ExactRational(intValue.value(), 1L);
            case RationalValue rationalValue ->
                    new ExactRational(rationalValue.numerator(), rationalValue.denominator());
            default -> throw new EvalError(operator + " expects an exact rational");
        };
    }

    static boolean isNumber(Value value) {
        return switch (value) {
            case IntValue intValue -> true;
            case RationalValue rationalValue -> true;
            case InexactValue inexactValue -> true;
            default -> false;
        };
    }

    static boolean isExactNumber(Value value) {
        return switch (value) {
            case IntValue intValue -> true;
            case RationalValue rationalValue -> true;
            default -> false;
        };
    }

    static boolean isInteger(Value value) {
        return switch (value) {
            case IntValue ignored -> true;
            case RationalValue rationalValue -> rationalValue.denominator() == 1L;
            case InexactValue inexactValue -> Double.isFinite(inexactValue.value())
                    && Math.rint(inexactValue.value()) == inexactValue.value();
            default -> false;
        };
    }

    static boolean containsInexact(List<Value> values) {
        for (Value value : values) {
            if (value instanceof InexactValue) {
                return true;
            }
        }
        return false;
    }

    static boolean eqValues(Value left, Value right) {
        if (left == right) {
            return true;
        }

        return switch (left) {
            case IntValue leftInt -> right instanceof IntValue rightInt
                    && leftInt.value() == rightInt.value();
            case RationalValue leftRational -> right instanceof RationalValue rightRational
                    && leftRational.numerator() == rightRational.numerator()
                    && leftRational.denominator() == rightRational.denominator();
            case InexactValue leftInexact -> right instanceof InexactValue rightInexact
                    && Double.compare(leftInexact.value(), rightInexact.value()) == 0;
            case BoolValue leftBool -> right instanceof BoolValue rightBool
                    && leftBool.value() == rightBool.value();
            case CharValue leftChar -> right instanceof CharValue rightChar
                    && leftChar.value() == rightChar.value();
            case SymbolValue leftSymbol -> right instanceof SymbolValue rightSymbol
                    && leftSymbol.name().equals(rightSymbol.name());
            case EmptyListValue ignored -> right instanceof EmptyListValue;
            default -> false;
        };
    }

    static boolean equalValues(Value left, Value right) {
        if (eqValues(left, right)) {
            return true;
        }

        return switch (left) {
            case StringValue leftString -> right instanceof StringValue rightString
                    && leftString.text().equals(rightString.text());
            case PairValue leftPair -> right instanceof PairValue rightPair
                    && equalValues(leftPair.car(), rightPair.car())
                    && equalValues(leftPair.cdr(), rightPair.cdr());
            case VectorValue leftVector -> right instanceof VectorValue rightVector
                    && vectorsEqual(leftVector, rightVector);
            default -> false;
        };
    }

    private static boolean vectorsEqual(VectorValue left, VectorValue right) {
        if (left.length() != right.length()) {
            return false;
        }
        for (int index = 0; index < left.length(); index++) {
            if (!equalValues(left.ref(index), right.ref(index))) {
                return false;
            }
        }
        return true;
    }

    static int numberSign(Value value, String operator) throws EvalError {
        if (value instanceof InexactValue inexactValue) {
            return Double.compare(inexactValue.value(), 0.0);
        }
        return Long.compare(requireExactRational(value, operator).numerator(), 0L);
    }

    static Value exactValue(ExactRational rational) {
        return exactValue(rational.numerator(), rational.denominator());
    }

    static Value exactValue(long numerator, long denominator) {
        ExactRational normalized = normalizeExactRational(numerator, denominator);
        if (normalized.denominator() == 1L) {
            return new IntValue(normalized.numerator());
        }
        return new RationalValue(normalized.numerator(), normalized.denominator());
    }

    static ExactRational addExact(ExactRational left, ExactRational right) {
        return normalizeExactRational(
                left.numerator() * right.denominator() + right.numerator() * left.denominator(),
                left.denominator() * right.denominator());
    }

    static ExactRational subtractExact(ExactRational left, ExactRational right) {
        return normalizeExactRational(
                left.numerator() * right.denominator() - right.numerator() * left.denominator(),
                left.denominator() * right.denominator());
    }

    static ExactRational multiplyExact(ExactRational left, ExactRational right) {
        return normalizeExactRational(
                left.numerator() * right.numerator(),
                left.denominator() * right.denominator());
    }

    static ExactRational divideExact(ExactRational left, ExactRational right) {
        return normalizeExactRational(
                left.numerator() * right.denominator(),
                left.denominator() * right.numerator());
    }

    static int compareExact(ExactRational left, ExactRational right) {
        return Long.compare(
                left.numerator() * right.denominator(),
                right.numerator() * left.denominator());
    }

    static Value inexactToExactValue(double value) throws EvalError {
        if (!Double.isFinite(value)) {
            throw new EvalError("inexact->exact expects a finite inexact number");
        }

        BigDecimal decimal = BigDecimal.valueOf(value);
        BigInteger numerator = decimal.unscaledValue();
        BigInteger denominator = BigInteger.ONE;
        if (decimal.scale() >= 0) {
            denominator = BigInteger.TEN.pow(decimal.scale());
        } else {
            numerator = numerator.multiply(BigInteger.TEN.pow(-decimal.scale()));
        }

        try {
            return exactValue(numerator.longValueExact(), denominator.longValueExact());
        } catch (ArithmeticException error) {
            throw new EvalError("inexact->exact overflow");
        }
    }

    static StringValue immutableString(String value) {
        return new StringValue(new StringBuilder(value), false);
    }

    private static ExactRational normalizeExactRational(long numerator, long denominator) {
        if (denominator == 0L) {
            throw new IllegalArgumentException("exact rationals cannot have zero denominator");
        }
        if (denominator < 0L) {
            numerator = -numerator;
            denominator = -denominator;
        }

        long divisor = gcd(numerator, denominator);
        return new ExactRational(numerator / divisor, denominator / divisor);
    }

    private static long gcd(long left, long right) {
        long a = Math.abs(left);
        long b = Math.abs(right);
        if (a == 0L) {
            return b == 0L ? 1L : b;
        }

        while (b != 0L) {
            long next = a % b;
            a = b;
            b = next;
        }
        return a;
    }
}
