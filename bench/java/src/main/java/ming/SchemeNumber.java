package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

final class SchemeNumber implements Comparable<SchemeNumber> {
    static final SchemeNumber EXACT_ZERO = exact(Rational.ZERO);
    static final SchemeNumber EXACT_ONE = exact(Rational.ONE);

    private final Rational exactValue;
    private final Double inexactValue;

    private SchemeNumber(Rational exactValue, Double inexactValue) {
        if ((exactValue == null) == (inexactValue == null)) {
            throw new IllegalArgumentException("number must be exact or inexact");
        }
        this.exactValue = exactValue;
        this.inexactValue = inexactValue;
    }

    static SchemeNumber exact(Rational value) {
        return new SchemeNumber(Objects.requireNonNull(value), null);
    }

    static SchemeNumber inexact(double value) {
        return new SchemeNumber(null, value);
    }

    static SchemeNumber parse(String token) {
        Rational exact = Rational.parse(token);
        if (exact != null) {
            return exact(exact);
        }
        if (!isDecimalToken(token)) {
            return null;
        }

        try {
            double parsed = Double.parseDouble(token);
            if (!Double.isFinite(parsed)) {
                return null;
            }
            return inexact(parsed);
        } catch (NumberFormatException ignored) {
            return null;
        }
    }

    private static boolean isDecimalToken(String token) {
        if (token.isEmpty() || !token.contains(".")) {
            return false;
        }

        int start = 0;
        char first = token.charAt(0);
        if (first == '+' || first == '-') {
            if (token.length() == 1) {
                return false;
            }
            start = 1;
        }

        boolean sawDot = false;
        boolean sawDigit = false;
        for (int index = start; index < token.length(); index++) {
            char ch = token.charAt(index);
            if (Character.isDigit(ch)) {
                sawDigit = true;
                continue;
            }
            if (ch == '.' && !sawDot) {
                sawDot = true;
                continue;
            }
            return false;
        }
        return sawDot && sawDigit;
    }

    boolean isExact() {
        return exactValue != null;
    }

    boolean isInexact() {
        return !isExact();
    }

    Rational toExact() {
        if (exactValue != null) {
            return exactValue;
        }
        return decimalToRational(BigDecimal.valueOf(inexactValue));
    }

    double toDouble() {
        if (exactValue != null) {
            return exactValue.numerator().doubleValue() / exactValue.denominator().doubleValue();
        }
        return inexactValue;
    }

    SchemeNumber add(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exact(exactValue.add(other.exactValue));
        }
        return inexact(toDouble() + other.toDouble());
    }

    SchemeNumber subtract(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exact(exactValue.subtract(other.exactValue));
        }
        return inexact(toDouble() - other.toDouble());
    }

    SchemeNumber multiply(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exact(exactValue.multiply(other.exactValue));
        }
        return inexact(toDouble() * other.toDouble());
    }

    SchemeNumber divide(SchemeNumber other, SourceLoc callLoc) throws EvalError {
        if (other.signum() == 0) {
            throw SchemeErrors.at(callLoc, "division by zero");
        }
        if (isExact() && other.isExact()) {
            return exact(exactValue.divide(other.exactValue, callLoc));
        }
        return inexact(toDouble() / other.toDouble());
    }

    SchemeNumber negate() {
        if (isExact()) {
            return exact(exactValue.negate());
        }
        return inexact(-inexactValue);
    }

    boolean isInteger() {
        return toExact().denominator().equals(BigInteger.ONE);
    }

    boolean isRational() {
        return true;
    }

    int signum() {
        if (exactValue != null) {
            return exactValue.numerator().signum();
        }
        if (inexactValue == 0.0d) {
            return 0;
        }
        return inexactValue < 0.0d ? -1 : 1;
    }

    boolean numericallyEquals(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exactValue.equals(other.exactValue);
        }
        return toDouble() == other.toDouble();
    }

    String render() {
        if (exactValue != null) {
            return exactValue.render();
        }
        return Double.toString(inexactValue);
    }

    @Override
    public int compareTo(SchemeNumber other) {
        if (isExact() && other.isExact()) {
            return exactValue.compareTo(other.exactValue);
        }

        double left = toDouble();
        double right = other.toDouble();
        if (left == right) {
            return 0;
        }
        return left < right ? -1 : 1;
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
        if (exactValue != null) {
            return exactValue.equals(schemeNumber.exactValue);
        }
        return Double.doubleToLongBits(inexactValue)
                == Double.doubleToLongBits(schemeNumber.inexactValue);
    }

    @Override
    public int hashCode() {
        if (exactValue != null) {
            return exactValue.hashCode();
        }
        return Long.hashCode(Double.doubleToLongBits(inexactValue));
    }

    private static Rational decimalToRational(BigDecimal value) {
        int scale = value.scale();
        BigInteger numerator = value.unscaledValue();
        BigInteger denominator = BigInteger.ONE;
        if (scale > 0) {
            denominator = BigInteger.TEN.pow(scale);
        } else if (scale < 0) {
            numerator = numerator.multiply(BigInteger.TEN.pow(-scale));
        }
        return Rational.of(numerator, denominator);
    }
}
