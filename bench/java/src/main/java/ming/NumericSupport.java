package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.regex.Pattern;

final class NumericSupport {
    private static final Pattern INTEGER_PATTERN = Pattern.compile("[+-]?\\d+");
    private static final Pattern RATIONAL_PATTERN = Pattern.compile("[+-]?\\d+/[+-]?\\d+");
    private static final Pattern INEXACT_PATTERN =
            Pattern.compile("[+-]?(?:\\d+\\.\\d*|\\d*\\.\\d+)");

    private NumericSupport() {
    }

    static ParsedNumber parseLiteral(String token) {
        if (INTEGER_PATTERN.matcher(token).matches()) {
            try {
                return new ParsedInteger(Integer.parseInt(token));
            } catch (NumberFormatException error) {
                return new ParsedRational(new BigInteger(token), BigInteger.ONE);
            }
        }

        if (RATIONAL_PATTERN.matcher(token).matches()) {
            int slashIndex = token.indexOf('/');
            BigInteger numerator = new BigInteger(token.substring(0, slashIndex));
            BigInteger denominator = new BigInteger(token.substring(slashIndex + 1));
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("invalid rational literal");
            }

            ExactFraction fraction = new ExactFraction(numerator, denominator);
            return new ParsedRational(fraction.numerator(), fraction.denominator());
        }

        if (INEXACT_PATTERN.matcher(token).matches()) {
            return new ParsedInexact(Double.parseDouble(token));
        }

        return null;
    }

    static boolean isNumber(Value value) {
        return value instanceof IntValue
                || value instanceof RationalValue
                || value instanceof InexactValue;
    }

    static boolean isExact(Value value) {
        return value instanceof IntValue || value instanceof RationalValue;
    }

    static boolean isInexact(Value value) {
        return value instanceof InexactValue;
    }

    static boolean isInteger(Value value) {
        if (value instanceof IntValue) {
            return true;
        }
        if (value instanceof RationalValue rationalValue) {
            return rationalValue.denominator().equals(BigInteger.ONE);
        }
        if (value instanceof InexactValue inexactValue) {
            return Double.isFinite(inexactValue.value())
                    && Math.rint(inexactValue.value()) == inexactValue.value();
        }
        return false;
    }

    static boolean isRational(Value value) {
        if (value instanceof InexactValue inexactValue) {
            return Double.isFinite(inexactValue.value());
        }
        return value instanceof IntValue || value instanceof RationalValue;
    }

    static ExactFraction toExactFraction(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return ExactFraction.of(intValue.value());
        }
        if (value instanceof RationalValue rationalValue) {
            return new ExactFraction(rationalValue.numerator(), rationalValue.denominator());
        }
        if (value instanceof InexactValue) {
            throw new EvalError("expected exact number");
        }
        throw new EvalError("expected number");
    }

    static double toDouble(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        if (value instanceof RationalValue rationalValue) {
            return new ExactFraction(rationalValue.numerator(), rationalValue.denominator())
                    .toDouble();
        }
        if (value instanceof InexactValue inexactValue) {
            return inexactValue.value();
        }
        throw new EvalError("expected number");
    }

    static BigInteger expectExactInteger(Value value) throws EvalError {
        if (value instanceof IntValue intValue) {
            return BigInteger.valueOf(intValue.value());
        }
        if (value instanceof RationalValue rationalValue
                && rationalValue.denominator().equals(BigInteger.ONE)) {
            return rationalValue.numerator();
        }
        if (isNumber(value)) {
            throw new EvalError("expected integer");
        }
        throw new EvalError("expected number");
    }

    static Value exactToValue(ExactFraction fraction) {
        if (fraction.denominator().equals(BigInteger.ONE)) {
            return integerToValue(fraction.numerator());
        }
        return new RationalValue(fraction.numerator(), fraction.denominator());
    }

    static Value integerToValue(BigInteger integer) {
        try {
            return new IntValue(integer.intValueExact());
        } catch (ArithmeticException error) {
            return new RationalValue(integer, BigInteger.ONE);
        }
    }

    static Value inexactToExact(Value value) throws EvalError {
        if (!(value instanceof InexactValue inexactValue)) {
            if (isNumber(value)) {
                return value;
            }
            throw new EvalError("expected number");
        }

        BigDecimal decimal = BigDecimal.valueOf(inexactValue.value());
        BigInteger numerator = decimal.unscaledValue();
        BigInteger denominator = BigInteger.ONE;

        if (decimal.scale() > 0) {
            denominator = BigInteger.TEN.pow(decimal.scale());
        } else if (decimal.scale() < 0) {
            numerator = numerator.multiply(BigInteger.TEN.pow(-decimal.scale()));
        }

        return exactToValue(new ExactFraction(numerator, denominator));
    }

    static int compare(Value left, Value right) throws EvalError {
        if (isExact(left) && isExact(right)) {
            return toExactFraction(left).compareTo(toExactFraction(right));
        }
        return Double.compare(toDouble(left), toDouble(right));
    }
}

sealed interface ParsedNumber permits ParsedInteger, ParsedRational, ParsedInexact {
}

record ParsedInteger(int value) implements ParsedNumber {
}

record ParsedRational(BigInteger numerator, BigInteger denominator) implements ParsedNumber {
}

record ParsedInexact(double value) implements ParsedNumber {
}

record ExactFraction(BigInteger numerator, BigInteger denominator) {
    ExactFraction {
        if (denominator.signum() == 0) {
            throw new IllegalArgumentException("division by zero");
        }

        if (numerator.signum() == 0) {
            numerator = BigInteger.ZERO;
            denominator = BigInteger.ONE;
        } else {
            if (denominator.signum() < 0) {
                numerator = numerator.negate();
                denominator = denominator.negate();
            }

            BigInteger gcd = numerator.gcd(denominator);
            numerator = numerator.divide(gcd);
            denominator = denominator.divide(gcd);
        }
    }

    static ExactFraction of(int value) {
        return new ExactFraction(BigInteger.valueOf(value), BigInteger.ONE);
    }

    ExactFraction add(ExactFraction other) {
        return new ExactFraction(
                numerator.multiply(other.denominator()).add(other.numerator().multiply(denominator)),
                denominator.multiply(other.denominator())
        );
    }

    ExactFraction subtract(ExactFraction other) {
        return new ExactFraction(
                numerator.multiply(other.denominator())
                        .subtract(other.numerator().multiply(denominator)),
                denominator.multiply(other.denominator())
        );
    }

    ExactFraction multiply(ExactFraction other) {
        return new ExactFraction(numerator.multiply(other.numerator()),
                denominator.multiply(other.denominator()));
    }

    ExactFraction divide(ExactFraction other) throws EvalError {
        if (other.numerator().signum() == 0) {
            throw new EvalError("division by zero");
        }
        return new ExactFraction(numerator.multiply(other.denominator()),
                denominator.multiply(other.numerator()));
    }

    ExactFraction negate() {
        return new ExactFraction(numerator.negate(), denominator);
    }

    int compareTo(ExactFraction other) {
        return numerator.multiply(other.denominator())
                .compareTo(other.numerator().multiply(denominator));
    }

    int signum() {
        return numerator.signum();
    }

    double toDouble() {
        return numerator.doubleValue() / denominator.doubleValue();
    }
}
