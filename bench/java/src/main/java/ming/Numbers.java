package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

final class Numbers {
    private record Fraction(BigInteger numerator, BigInteger denominator) {
    }

    private static final BigInteger ZERO = BigInteger.ZERO;
    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger TEN = BigInteger.TEN;

    private Numbers() {
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

        for (int i = start; i < token.length(); i++) {
            if (!Character.isDigit(token.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    static boolean looksLikeNumberLiteral(String token) {
        return isIntegerToken(token) || isRationalToken(token) || isDecimalToken(token);
    }

    static Value parseLiteral(String token) throws EvalError {
        if (isIntegerToken(token)) {
            try {
                return new IntValue(Long.parseLong(token));
            } catch (NumberFormatException error) {
                throw new EvalError("invalid integer literal: " + token);
            }
        }

        if (isRationalToken(token)) {
            int slashIndex = token.indexOf('/');
            try {
                long numerator = Long.parseLong(token.substring(0, slashIndex));
                long denominator = Long.parseLong(token.substring(slashIndex + 1));
                if (denominator == 0L) {
                    throw new EvalError("invalid rational literal: " + token);
                }
                return exact(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator));
            } catch (NumberFormatException error) {
                throw new EvalError("invalid rational literal: " + token);
            }
        }

        if (isDecimalToken(token)) {
            try {
                return new InexactValue(Double.parseDouble(token));
            } catch (NumberFormatException error) {
                throw new EvalError("invalid decimal literal: " + token);
            }
        }

        throw new EvalError("invalid number literal: " + token);
    }

    static Value tryParseLiteral(String token) {
        try {
            return parseLiteral(token);
        } catch (EvalError error) {
            return null;
        }
    }

    static NumericValue expectNumber(Value value, String operator) throws EvalError {
        if (value instanceof NumericValue numericValue) {
            return numericValue;
        }
        throw new EvalError(operator + " expects number arguments");
    }

    static boolean isNumber(Value value) {
        return value instanceof NumericValue;
    }

    static boolean isExact(Value value) {
        return value instanceof NumericValue numericValue && numericValue.isExact();
    }

    static boolean isInexact(Value value) {
        return value instanceof NumericValue numericValue && !numericValue.isExact();
    }

    static boolean isInteger(Value value) {
        return switch (value) {
            case IntValue ignored -> true;
            case RationalValue ignored -> false;
            case InexactValue inexactValue ->
                    Double.isFinite(inexactValue.value())
                            && Math.rint(inexactValue.value()) == inexactValue.value();
            default -> false;
        };
    }

    static boolean isRational(Value value) {
        return value instanceof IntValue || value instanceof RationalValue;
    }

    static boolean equals(Value left, Value right) {
        if (!(left instanceof NumericValue leftNumber) || !(right instanceof NumericValue rightNumber)) {
            return false;
        }
        return compare(leftNumber, rightNumber) == 0;
    }

    static boolean compareChain(String operator, List<Value> arguments) throws EvalError {
        NumericValue previous = expectNumber(arguments.getFirst(), operator);
        for (int i = 1; i < arguments.size(); i++) {
            NumericValue current = expectNumber(arguments.get(i), operator);
            int comparison = compare(previous, current);
            boolean matches = switch (operator) {
                case "<" -> comparison < 0;
                case ">" -> comparison > 0;
                case "=" -> comparison == 0;
                case "<=" -> comparison <= 0;
                default -> false;
            };
            if (!matches) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    static Value add(List<Value> arguments, String operator) throws EvalError {
        if (hasInexactOperand(arguments, operator)) {
            double total = 0.0;
            for (Value argument : arguments) {
                total += expectNumber(argument, operator).toDouble();
            }
            return new InexactValue(total);
        }

        BigInteger numerator = ZERO;
        BigInteger denominator = ONE;
        for (Value argument : arguments) {
            Fraction fraction = exactFraction(expectNumber(argument, operator));
            numerator = numerator.multiply(fraction.denominator())
                    .add(fraction.numerator().multiply(denominator));
            denominator = denominator.multiply(fraction.denominator());
        }
        return exact(numerator, denominator);
    }

    static Value subtract(List<Value> arguments, String operator) throws EvalError {
        NumericValue first = expectNumber(arguments.getFirst(), operator);
        if (hasInexactOperand(arguments, operator)) {
            double result = first.toDouble();
            if (arguments.size() == 1) {
                return new InexactValue(-result);
            }
            for (int i = 1; i < arguments.size(); i++) {
                result -= expectNumber(arguments.get(i), operator).toDouble();
            }
            return new InexactValue(result);
        }

        Fraction result = exactFraction(first);
        if (arguments.size() == 1) {
            return exact(result.numerator().negate(), result.denominator());
        }
        for (int i = 1; i < arguments.size(); i++) {
            Fraction next = exactFraction(expectNumber(arguments.get(i), operator));
            result = new Fraction(
                    result.numerator().multiply(next.denominator())
                            .subtract(next.numerator().multiply(result.denominator())),
                    result.denominator().multiply(next.denominator()));
        }
        return exact(result.numerator(), result.denominator());
    }

    static Value multiply(List<Value> arguments, String operator) throws EvalError {
        if (hasInexactOperand(arguments, operator)) {
            double product = 1.0;
            for (Value argument : arguments) {
                product *= expectNumber(argument, operator).toDouble();
            }
            return new InexactValue(product);
        }

        BigInteger numerator = ONE;
        BigInteger denominator = ONE;
        for (Value argument : arguments) {
            Fraction fraction = exactFraction(expectNumber(argument, operator));
            numerator = numerator.multiply(fraction.numerator());
            denominator = denominator.multiply(fraction.denominator());
        }
        return exact(numerator, denominator);
    }

    static Value divide(List<Value> arguments, String operator) throws EvalError {
        NumericValue first = expectNumber(arguments.getFirst(), operator);
        if (hasInexactOperand(arguments, operator)) {
            double result = first.toDouble();
            for (int i = 1; i < arguments.size(); i++) {
                double divisor = expectNumber(arguments.get(i), operator).toDouble();
                if (divisor == 0.0) {
                    throw new EvalError("division by zero");
                }
                result /= divisor;
            }
            return new InexactValue(result);
        }

        Fraction result = exactFraction(first);
        for (int i = 1; i < arguments.size(); i++) {
            Fraction divisor = exactFraction(expectNumber(arguments.get(i), operator));
            if (divisor.numerator().signum() == 0) {
                throw new EvalError("division by zero");
            }
            result = new Fraction(
                    result.numerator().multiply(divisor.denominator()),
                    result.denominator().multiply(divisor.numerator()));
        }
        return exact(result.numerator(), result.denominator());
    }

    static Value abs(Value argument, String operator) throws EvalError {
        NumericValue number = expectNumber(argument, operator);
        if (number instanceof InexactValue inexactValue) {
            return new InexactValue(Math.abs(inexactValue.value()));
        }
        Fraction fraction = exactFraction(number);
        return exact(fraction.numerator().abs(), fraction.denominator());
    }

    static Value min(List<Value> arguments, String operator) throws EvalError {
        NumericValue best = expectNumber(arguments.getFirst(), operator);
        for (int i = 1; i < arguments.size(); i++) {
            NumericValue candidate = expectNumber(arguments.get(i), operator);
            if (compare(candidate, best) < 0) {
                best = candidate;
            }
        }
        return best;
    }

    static Value max(List<Value> arguments, String operator) throws EvalError {
        NumericValue best = expectNumber(arguments.getFirst(), operator);
        for (int i = 1; i < arguments.size(); i++) {
            NumericValue candidate = expectNumber(arguments.get(i), operator);
            if (compare(candidate, best) > 0) {
                best = candidate;
            }
        }
        return best;
    }

    static boolean isZero(Value argument, String operator) throws EvalError {
        return compare(expectNumber(argument, operator), new IntValue(0L)) == 0;
    }

    static boolean isPositive(Value argument, String operator) throws EvalError {
        return compare(expectNumber(argument, operator), new IntValue(0L)) > 0;
    }

    static boolean isNegative(Value argument, String operator) throws EvalError {
        return compare(expectNumber(argument, operator), new IntValue(0L)) < 0;
    }

    static Value exactToInexact(Value argument, String operator) throws EvalError {
        NumericValue number = expectNumber(argument, operator);
        if (number instanceof InexactValue) {
            return number;
        }
        return new InexactValue(number.toDouble());
    }

    static Value inexactToExact(Value argument, String operator) throws EvalError {
        NumericValue number = expectNumber(argument, operator);
        if (number.isExact()) {
            return number;
        }

        InexactValue inexactValue = (InexactValue) number;
        if (!Double.isFinite(inexactValue.value())) {
            throw new EvalError(operator + " expects finite arguments");
        }

        BigDecimal decimal = BigDecimal.valueOf(inexactValue.value()).stripTrailingZeros();
        BigInteger numerator = decimal.unscaledValue();
        BigInteger denominator = ONE;
        int scale = decimal.scale();
        if (scale > 0) {
            denominator = TEN.pow(scale);
        } else if (scale < 0) {
            numerator = numerator.multiply(TEN.pow(-scale));
        }
        return exact(numerator, denominator);
    }

    static Value numerator(Value argument, String operator) throws EvalError {
        NumericValue number = expectNumber(argument, operator);
        return switch (number) {
            case IntValue intValue -> intValue;
            case RationalValue rationalValue -> new IntValue(rationalValue.numerator());
            case InexactValue ignored -> throw new EvalError(operator + " expects exact arguments");
        };
    }

    static Value denominator(Value argument, String operator) throws EvalError {
        NumericValue number = expectNumber(argument, operator);
        return switch (number) {
            case IntValue ignored -> new IntValue(1L);
            case RationalValue rationalValue -> new IntValue(rationalValue.denominator());
            case InexactValue ignored -> throw new EvalError(operator + " expects exact arguments");
        };
    }

    private static int compare(NumericValue left, NumericValue right) {
        if (!left.isExact() || !right.isExact()) {
            double leftDouble = left.toDouble();
            double rightDouble = right.toDouble();
            if (leftDouble < rightDouble) {
                return -1;
            }
            if (leftDouble > rightDouble) {
                return 1;
            }
            return 0;
        }

        Fraction leftFraction = exactFraction(left);
        Fraction rightFraction = exactFraction(right);
        return leftFraction.numerator().multiply(rightFraction.denominator())
                .compareTo(rightFraction.numerator().multiply(leftFraction.denominator()));
    }

    private static boolean hasInexactOperand(List<Value> arguments, String operator) throws EvalError {
        for (Value argument : arguments) {
            if (!expectNumber(argument, operator).isExact()) {
                return true;
            }
        }
        return false;
    }

    private static Fraction exactFraction(NumericValue number) {
        return switch (number) {
            case IntValue intValue -> new Fraction(BigInteger.valueOf(intValue.value()), ONE);
            case RationalValue rationalValue -> new Fraction(
                    BigInteger.valueOf(rationalValue.numerator()),
                    BigInteger.valueOf(rationalValue.denominator()));
            case InexactValue ignored -> throw new IllegalArgumentException("expected exact number");
        };
    }

    private static Value exact(BigInteger numerator, BigInteger denominator) throws EvalError {
        if (denominator.signum() == 0) {
            throw new EvalError("division by zero");
        }
        if (numerator.signum() == 0) {
            return new IntValue(0L);
        }
        if (denominator.signum() < 0) {
            numerator = numerator.negate();
            denominator = denominator.negate();
        }

        BigInteger divisor = numerator.gcd(denominator);
        numerator = numerator.divide(divisor);
        denominator = denominator.divide(divisor);

        try {
            long normalizedNumerator = numerator.longValueExact();
            long normalizedDenominator = denominator.longValueExact();
            if (normalizedDenominator == 1L) {
                return new IntValue(normalizedNumerator);
            }
            return new RationalValue(normalizedNumerator, normalizedDenominator);
        } catch (ArithmeticException error) {
            throw new EvalError("exact arithmetic overflow");
        }
    }

    private static boolean isRationalToken(String token) {
        int slashIndex = token.indexOf('/');
        if (slashIndex <= 0 || slashIndex != token.lastIndexOf('/')) {
            return false;
        }

        String numerator = token.substring(0, slashIndex);
        String denominator = token.substring(slashIndex + 1);
        return isIntegerToken(numerator) && isUnsignedIntegerToken(denominator);
    }

    private static boolean isDecimalToken(String token) {
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

        int dotIndex = token.indexOf('.', start);
        if (dotIndex <= start || dotIndex != token.lastIndexOf('.') || dotIndex == token.length() - 1) {
            return false;
        }

        for (int i = start; i < dotIndex; i++) {
            if (!Character.isDigit(token.charAt(i))) {
                return false;
            }
        }
        for (int i = dotIndex + 1; i < token.length(); i++) {
            if (!Character.isDigit(token.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isUnsignedIntegerToken(String token) {
        if (token.isEmpty()) {
            return false;
        }
        for (int i = 0; i < token.length(); i++) {
            if (!Character.isDigit(token.charAt(i))) {
                return false;
            }
        }
        return true;
    }
}
