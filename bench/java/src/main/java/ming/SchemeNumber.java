package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

final class SchemeNumber {
    private static final BigInteger ONE = BigInteger.ONE;

    private final BigInteger numerator;
    private final BigInteger denominator;
    private final Double inexactValue;

    private SchemeNumber(BigInteger numerator, BigInteger denominator, Double inexactValue) {
        this.numerator = numerator;
        this.denominator = denominator;
        this.inexactValue = inexactValue;
    }

    static SchemeNumber exact(BigInteger value) {
        return exact(value, ONE);
    }

    static SchemeNumber exact(BigInteger numerator, BigInteger denominator) {
        if (denominator.signum() == 0) {
            throw new IllegalArgumentException("denominator cannot be zero");
        }
        if (numerator.signum() == 0) {
            return new SchemeNumber(BigInteger.ZERO, ONE, null);
        }
        if (denominator.signum() < 0) {
            numerator = numerator.negate();
            denominator = denominator.negate();
        }
        BigInteger gcd = numerator.gcd(denominator);
        return new SchemeNumber(numerator.divide(gcd), denominator.divide(gcd), null);
    }

    static SchemeNumber inexact(double value) {
        return new SchemeNumber(null, null, value);
    }

    static SchemeNumber fromValue(NumericValue value) {
        if (value instanceof IntValue intValue) {
            return exact(intValue.value());
        }
        if (value instanceof RationalValue rationalValue) {
            return exact(rationalValue.numerator(), rationalValue.denominator());
        }
        if (value instanceof InexactValue inexactValue) {
            return inexact(inexactValue.value());
        }
        throw new IllegalStateException("unsupported numeric value");
    }

    static SchemeNumber parseLiteral(String token) {
        try {
            if (token.contains("/")) {
                if (token.indexOf('/') != token.lastIndexOf('/') || token.contains(".")) {
                    throw new NumberFormatException("invalid rational literal");
                }
                int slash = token.indexOf('/');
                BigInteger numerator = new BigInteger(token.substring(0, slash));
                BigInteger denominator = new BigInteger(token.substring(slash + 1));
                return exact(numerator, denominator);
            }
            if (token.contains(".") || token.contains("e") || token.contains("E")) {
                return inexact(Double.parseDouble(token));
            }
            return exact(new BigInteger(token));
        } catch (IllegalArgumentException error) {
            NumberFormatException wrapped = new NumberFormatException(token);
            wrapped.initCause(error);
            throw wrapped;
        }
    }

    boolean isExact() {
        return inexactValue == null;
    }

    boolean isInteger() {
        if (isExact()) {
            return denominator.equals(ONE);
        }
        double value = inexactValue;
        return Double.isFinite(value) && Math.rint(value) == value;
    }

    boolean isZero() {
        return signum() == 0;
    }

    int signum() {
        if (isExact()) {
            return numerator.signum();
        }
        if (inexactValue > 0) {
            return 1;
        }
        if (inexactValue < 0) {
            return -1;
        }
        return 0;
    }

    BigInteger numeratorExact() {
        if (!isExact()) {
            throw new IllegalStateException("not an exact number");
        }
        return numerator;
    }

    BigInteger denominatorExact() {
        if (!isExact()) {
            throw new IllegalStateException("not an exact number");
        }
        return denominator;
    }

    BigInteger integerExact() {
        if (!isInteger() || !isExact()) {
            throw new IllegalStateException("not an exact integer");
        }
        return numerator;
    }

    double toDouble() {
        if (isExact()) {
            return numerator.doubleValue() / denominator.doubleValue();
        }
        return inexactValue;
    }

    SchemeNumber add(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exact(
                    numerator.multiply(other.denominator).add(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator));
        }
        return inexact(toDouble() + other.toDouble());
    }

    SchemeNumber subtract(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exact(
                    numerator.multiply(other.denominator).subtract(
                            other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator));
        }
        return inexact(toDouble() - other.toDouble());
    }

    SchemeNumber multiply(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exact(numerator.multiply(other.numerator),
                    denominator.multiply(other.denominator));
        }
        return inexact(toDouble() * other.toDouble());
    }

    SchemeNumber divide(SchemeNumber other) {
        if (other.isZero()) {
            throw new ArithmeticException("division by zero");
        }
        if (isExact() && other.isExact()) {
            return exact(numerator.multiply(other.denominator),
                    denominator.multiply(other.numerator));
        }
        return inexact(toDouble() / other.toDouble());
    }

    SchemeNumber negate() {
        if (isExact()) {
            return exact(numerator.negate(), denominator);
        }
        return inexact(-inexactValue);
    }

    SchemeNumber abs() {
        if (isExact()) {
            return exact(numerator.abs(), denominator);
        }
        return inexact(Math.abs(inexactValue));
    }

    SchemeNumber pow(int exponent) {
        if (exponent < 0) {
            throw new IllegalArgumentException("negative exponent");
        }
        if (isExact()) {
            return exact(numerator.pow(exponent), denominator.pow(exponent));
        }
        return inexact(Math.pow(inexactValue, exponent));
    }

    int compareTo(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return numerator.multiply(other.denominator)
                    .compareTo(other.numerator.multiply(denominator));
        }
        return Double.compare(toDouble(), other.toDouble());
    }

    boolean numericallyEquals(SchemeNumber other) {
        return compareTo(other) == 0;
    }

    SchemeNumber exactToInexact() {
        return inexact(toDouble());
    }

    SchemeNumber inexactToExact() {
        if (isExact()) {
            return this;
        }

        BigDecimal decimal = BigDecimal.valueOf(inexactValue);
        BigInteger unscaled = decimal.unscaledValue();
        BigInteger denominator = ONE;
        if (decimal.scale() >= 0) {
            denominator = BigInteger.TEN.pow(decimal.scale());
        } else {
            unscaled = unscaled.multiply(BigInteger.TEN.pow(-decimal.scale()));
        }
        return exact(unscaled, denominator);
    }

    NumericValue toValue() {
        if (isExact()) {
            if (denominator.equals(ONE)) {
                return new IntValue(numerator);
            }
            return new RationalValue(numerator, denominator);
        }
        return new InexactValue(inexactValue);
    }

    String format() {
        if (isExact()) {
            if (denominator.equals(ONE)) {
                return numerator.toString();
            }
            return numerator + "/" + denominator;
        }
        return Double.toString(inexactValue);
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof SchemeNumber schemeNumber)) {
            return false;
        }
        if (isExact() != schemeNumber.isExact()) {
            return false;
        }
        if (isExact()) {
            return numerator.equals(schemeNumber.numerator)
                    && denominator.equals(schemeNumber.denominator);
        }
        return Double.doubleToLongBits(inexactValue)
                == Double.doubleToLongBits(schemeNumber.inexactValue);
    }

    @Override
    public int hashCode() {
        if (isExact()) {
            return Objects.hash(numerator, denominator);
        }
        return Double.hashCode(inexactValue);
    }
}
