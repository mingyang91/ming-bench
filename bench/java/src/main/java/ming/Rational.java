package ming;

import java.math.BigInteger;

record Rational(BigInteger numerator, BigInteger denominator) implements Comparable<Rational> {
    static final Rational ZERO = integer(BigInteger.ZERO);
    static final Rational ONE = integer(BigInteger.ONE);

    Rational {
        if (denominator.signum() == 0) {
            throw new IllegalArgumentException("denominator cannot be zero");
        }
    }

    static Rational integer(BigInteger value) {
        return new Rational(value, BigInteger.ONE);
    }

    static Rational of(BigInteger numerator, BigInteger denominator) {
        if (denominator.signum() == 0) {
            throw new IllegalArgumentException("denominator cannot be zero");
        }

        BigInteger normalizedNumerator = numerator;
        BigInteger normalizedDenominator = denominator;
        if (normalizedDenominator.signum() < 0) {
            normalizedNumerator = normalizedNumerator.negate();
            normalizedDenominator = normalizedDenominator.negate();
        }

        BigInteger gcd = normalizedNumerator.gcd(normalizedDenominator);
        return new Rational(
                normalizedNumerator.divide(gcd),
                normalizedDenominator.divide(gcd)
        );
    }

    static Rational parse(String token) {
        if (isIntegerToken(token)) {
            return Rational.integer(new BigInteger(token));
        }

        int slashIndex = token.indexOf('/');
        if (slashIndex <= 0 || slashIndex != token.lastIndexOf('/')) {
            return null;
        }

        String numeratorToken = token.substring(0, slashIndex);
        String denominatorToken = token.substring(slashIndex + 1);
        if (!isIntegerToken(numeratorToken) || !isIntegerToken(denominatorToken)) {
            return null;
        }

        BigInteger denominatorValue = new BigInteger(denominatorToken);
        if (denominatorValue.signum() == 0) {
            return null;
        }
        return Rational.of(new BigInteger(numeratorToken), denominatorValue);
    }

    static boolean isIntegerToken(String token) {
        if (token.isEmpty()) {
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

        for (int index = start; index < token.length(); index++) {
            if (!Character.isDigit(token.charAt(index))) {
                return false;
            }
        }
        return true;
    }

    Rational add(Rational other) {
        return of(
                numerator.multiply(other.denominator).add(other.numerator.multiply(denominator)),
                denominator.multiply(other.denominator)
        );
    }

    Rational subtract(Rational other) {
        return of(
                numerator.multiply(other.denominator).subtract(other.numerator.multiply(denominator)),
                denominator.multiply(other.denominator)
        );
    }

    Rational multiply(Rational other) {
        return of(numerator.multiply(other.numerator), denominator.multiply(other.denominator));
    }

    Rational divide(Rational other, SourceLoc callLoc) throws EvalError {
        if (other.numerator.signum() == 0) {
            throw SchemeErrors.at(callLoc, "division by zero");
        }
        return of(numerator.multiply(other.denominator), denominator.multiply(other.numerator));
    }

    Rational negate() {
        return new Rational(numerator.negate(), denominator);
    }

    String render() {
        if (denominator.equals(BigInteger.ONE)) {
            return numerator.toString();
        }
        return numerator + "/" + denominator;
    }

    @Override
    public int compareTo(Rational other) {
        return numerator.multiply(other.denominator)
                .compareTo(other.numerator.multiply(denominator));
    }
}
