package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

sealed interface NumericValue extends SchemeValue permits IntValue, RationalValue, RealValue {
    boolean isExact();

    double doubleValue();
}

record RationalValue(long numerator, long denominator) implements NumericValue {
    RationalValue {
        if (denominator == 0L) {
            throw new IllegalArgumentException("denominator must not be zero");
        }
        if (denominator < 0L) {
            numerator = -numerator;
            denominator = -denominator;
        }

        long divisor = gcd(numerator, denominator);
        numerator /= divisor;
        denominator /= divisor;
    }

    @Override
    public boolean isExact() {
        return true;
    }

    @Override
    public double doubleValue() {
        return (double) numerator / (double) denominator;
    }

    @Override
    public String render() {
        if (denominator == 1L) {
            return Long.toString(numerator);
        }
        return numerator + "/" + denominator;
    }

    private static long gcd(long left, long right) {
        long a = Math.abs(left);
        long b = Math.abs(right);
        while (b != 0L) {
            long remainder = a % b;
            a = b;
            b = remainder;
        }
        return a == 0L ? 1L : a;
    }
}

record RealValue(double value) implements NumericValue {
    @Override
    public boolean isExact() {
        return false;
    }

    @Override
    public double doubleValue() {
        return value;
    }

    @Override
    public String render() {
        return Double.toString(value);
    }
}

final class Numbers {
    private Numbers() {
    }

    static SchemeValue parseNumber(String token, SourcePosition position) throws EvalError {
        if (isIntegerToken(token)) {
            return parseInteger(token, position);
        }
        if (isRationalToken(token)) {
            return parseRational(token, position);
        }
        if (isRealToken(token)) {
            return parseReal(token, position);
        }
        return null;
    }

    static SchemeValue tryParseNumber(String token) {
        try {
            return parseNumber(token, null);
        } catch (EvalError error) {
            return null;
        }
    }

    static SchemeValue add(List<SchemeValue> arguments, String procedure) throws EvalError {
        if (containsInexact(arguments, procedure)) {
            double total = 0.0;
            for (SchemeValue argument : arguments) {
                total += requireNumeric(argument, procedure).doubleValue();
            }
            return new RealValue(total);
        }

        ExactFraction total = new ExactFraction(BigInteger.ZERO, BigInteger.ONE);
        for (SchemeValue argument : arguments) {
            total = total.add(requireExactFraction(argument, procedure));
        }
        return total.toSchemeValue();
    }

    static SchemeValue subtract(List<SchemeValue> arguments, String procedure) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError(procedure + ": expected at least 1 argument");
        }

        if (containsInexact(arguments, procedure)) {
            double result = requireNumeric(arguments.getFirst(), procedure).doubleValue();
            if (arguments.size() == 1) {
                return new RealValue(-result);
            }
            for (int index = 1; index < arguments.size(); index++) {
                result -= requireNumeric(arguments.get(index), procedure).doubleValue();
            }
            return new RealValue(result);
        }

        ExactFraction result = requireExactFraction(arguments.getFirst(), procedure);
        if (arguments.size() == 1) {
            return result.negate().toSchemeValue();
        }
        for (int index = 1; index < arguments.size(); index++) {
            result = result.subtract(requireExactFraction(arguments.get(index), procedure));
        }
        return result.toSchemeValue();
    }

    static SchemeValue multiply(List<SchemeValue> arguments, String procedure) throws EvalError {
        if (containsInexact(arguments, procedure)) {
            double total = 1.0;
            for (SchemeValue argument : arguments) {
                total *= requireNumeric(argument, procedure).doubleValue();
            }
            return new RealValue(total);
        }

        ExactFraction total = new ExactFraction(BigInteger.ONE, BigInteger.ONE);
        for (SchemeValue argument : arguments) {
            total = total.multiply(requireExactFraction(argument, procedure));
        }
        return total.toSchemeValue();
    }

    static SchemeValue divide(List<SchemeValue> arguments, String procedure) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError(procedure + ": expected at least 2 arguments");
        }

        if (containsInexact(arguments, procedure)) {
            double result = requireNumeric(arguments.getFirst(), procedure).doubleValue();
            for (int index = 1; index < arguments.size(); index++) {
                double divisor = requireNumeric(arguments.get(index), procedure).doubleValue();
                if (divisor == 0.0) {
                    throw new EvalError("division by zero");
                }
                result /= divisor;
            }
            return new RealValue(result);
        }

        ExactFraction result = requireExactFraction(arguments.getFirst(), procedure);
        for (int index = 1; index < arguments.size(); index++) {
            ExactFraction divisor = requireExactFraction(arguments.get(index), procedure);
            if (divisor.numerator().signum() == 0) {
                throw new EvalError("division by zero");
            }
            result = result.divide(divisor);
        }
        return result.toSchemeValue();
    }

    static SchemeValue abs(SchemeValue argument, String procedure) throws EvalError {
        NumericValue numeric = requireNumeric(argument, procedure);
        if (!numeric.isExact()) {
            return new RealValue(Math.abs(numeric.doubleValue()));
        }
        return requireExactFraction(argument, procedure).abs().toSchemeValue();
    }

    static int compareValues(SchemeValue left, SchemeValue right, String procedure) throws EvalError {
        NumericValue leftNumber = requireNumeric(left, procedure);
        NumericValue rightNumber = requireNumeric(right, procedure);
        if (leftNumber.isExact() && rightNumber.isExact()) {
            return requireExactFraction(left, procedure).compareTo(requireExactFraction(right, procedure));
        }

        double leftValue = leftNumber.doubleValue();
        double rightValue = rightNumber.doubleValue();
        if (leftValue == rightValue) {
            return 0;
        }
        return Double.compare(leftValue, rightValue);
    }

    static boolean numericEquals(SchemeValue left, SchemeValue right) {
        if (!(left instanceof NumericValue leftNumber) || !(right instanceof NumericValue rightNumber)) {
            return false;
        }
        if (leftNumber.isExact() && rightNumber.isExact()) {
            ExactFraction leftFraction = toExactFraction(leftNumber);
            ExactFraction rightFraction = toExactFraction(rightNumber);
            return leftFraction.numerator().equals(rightFraction.numerator())
                    && leftFraction.denominator().equals(rightFraction.denominator());
        }
        return leftNumber.doubleValue() == rightNumber.doubleValue();
    }

    static NumericValue requireNumeric(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof NumericValue numeric) {
            return numeric;
        }
        throw new EvalError(procedure + ": expected number");
    }

    static long requireInteger(SchemeValue value, String procedure) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        if (value instanceof RationalValue rationalValue && rationalValue.denominator() == 1L) {
            return rationalValue.numerator();
        }
        throw new EvalError(procedure + ": expected integer");
    }

    static boolean isNumber(SchemeValue value) {
        return value instanceof NumericValue;
    }

    static boolean isIntegerValue(SchemeValue value) {
        if (value instanceof IntValue) {
            return true;
        }
        if (value instanceof RationalValue rationalValue) {
            return rationalValue.denominator() == 1L;
        }
        if (value instanceof RealValue realValue) {
            double numeric = realValue.value();
            return Double.isFinite(numeric) && Math.rint(numeric) == numeric;
        }
        return false;
    }

    static boolean isRationalValue(SchemeValue value) {
        if (value instanceof RealValue realValue) {
            return Double.isFinite(realValue.value());
        }
        return value instanceof NumericValue;
    }

    static boolean isExactNumber(SchemeValue value) {
        return value instanceof NumericValue numeric && numeric.isExact();
    }

    static boolean isInexactNumber(SchemeValue value) {
        return value instanceof NumericValue numeric && !numeric.isExact();
    }

    static SchemeValue exactToInexact(SchemeValue value, String procedure) throws EvalError {
        NumericValue numeric = requireNumeric(value, procedure);
        if (!numeric.isExact()) {
            return numeric;
        }
        return new RealValue(numeric.doubleValue());
    }

    static SchemeValue inexactToExact(SchemeValue value, String procedure) throws EvalError {
        NumericValue numeric = requireNumeric(value, procedure);
        if (numeric.isExact()) {
            return (SchemeValue) numeric;
        }

        double raw = numeric.doubleValue();
        if (!Double.isFinite(raw)) {
            throw new EvalError(procedure + ": expected finite number");
        }

        BigDecimal decimal = BigDecimal.valueOf(raw).stripTrailingZeros();
        BigInteger numerator = decimal.unscaledValue();
        int scale = decimal.scale();
        BigInteger denominator = BigInteger.ONE;
        if (scale > 0) {
            denominator = BigInteger.TEN.pow(scale);
        } else if (scale < 0) {
            numerator = numerator.multiply(BigInteger.TEN.pow(-scale));
        }
        return new ExactFraction(numerator, denominator).toSchemeValue();
    }

    static SchemeValue numerator(SchemeValue value, String procedure) throws EvalError {
        ExactFraction fraction = requireExactFraction(value, procedure);
        return new IntValue(fraction.numerator().longValueExact());
    }

    static SchemeValue denominator(SchemeValue value, String procedure) throws EvalError {
        ExactFraction fraction = requireExactFraction(value, procedure);
        return new IntValue(fraction.denominator().longValueExact());
    }

    private static ExactFraction requireExactFraction(SchemeValue value, String procedure) throws EvalError {
        if (!(value instanceof NumericValue numeric)) {
            throw new EvalError(procedure + ": expected number");
        }
        if (!numeric.isExact()) {
            throw new EvalError(procedure + ": expected exact number");
        }
        return toExactFraction(numeric);
    }

    private static ExactFraction toExactFraction(NumericValue value) {
        if (value instanceof IntValue intValue) {
            return new ExactFraction(BigInteger.valueOf(intValue.value()), BigInteger.ONE);
        }
        RationalValue rationalValue = (RationalValue) value;
        return new ExactFraction(
                BigInteger.valueOf(rationalValue.numerator()),
                BigInteger.valueOf(rationalValue.denominator())
        );
    }

    private static boolean containsInexact(List<SchemeValue> arguments, String procedure) throws EvalError {
        boolean hasInexact = false;
        for (SchemeValue argument : arguments) {
            NumericValue numeric = requireNumeric(argument, procedure);
            if (!numeric.isExact()) {
                hasInexact = true;
            }
        }
        return hasInexact;
    }

    private static SchemeValue parseInteger(String token, SourcePosition position) throws EvalError {
        try {
            return new IntValue(Long.parseLong(token));
        } catch (NumberFormatException error) {
            throw numberParseError(position, "invalid integer: " + token);
        }
    }

    private static SchemeValue parseRational(String token, SourcePosition position) throws EvalError {
        int slash = token.indexOf('/');
        String numeratorToken = token.substring(0, slash);
        String denominatorToken = token.substring(slash + 1);
        try {
            BigInteger numerator = new BigInteger(numeratorToken);
            BigInteger denominator = new BigInteger(denominatorToken);
            if (denominator.signum() == 0) {
                throw numberParseError(position, "invalid rational: " + token);
            }
            return new ExactFraction(numerator, denominator).toSchemeValue();
        } catch (NumberFormatException error) {
            throw numberParseError(position, "invalid rational: " + token);
        }
    }

    private static SchemeValue parseReal(String token, SourcePosition position) throws EvalError {
        try {
            return new RealValue(Double.parseDouble(token));
        } catch (NumberFormatException error) {
            throw numberParseError(position, "invalid number: " + token);
        }
    }

    private static boolean isIntegerToken(String token) {
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

        for (int current = start; current < token.length(); current++) {
            if (!Character.isDigit(token.charAt(current))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isRationalToken(String token) {
        int slash = token.indexOf('/');
        if (slash <= 0 || slash != token.lastIndexOf('/') || slash == token.length() - 1) {
            return false;
        }
        return isIntegerToken(token.substring(0, slash))
                && isIntegerToken(token.substring(slash + 1));
    }

    private static boolean isRealToken(String token) {
        if (!token.contains(".")) {
            return false;
        }

        int start = 0;
        if (token.charAt(0) == '+' || token.charAt(0) == '-') {
            if (token.length() == 1) {
                return false;
            }
            start = 1;
        }

        boolean sawDot = false;
        boolean sawDigit = false;
        for (int index = start; index < token.length(); index++) {
            char current = token.charAt(index);
            if (current == '.') {
                if (sawDot) {
                    return false;
                }
                sawDot = true;
                continue;
            }
            if (!Character.isDigit(current)) {
                return false;
            }
            sawDigit = true;
        }
        return sawDot && sawDigit;
    }

    private static EvalError numberParseError(SourcePosition position, String detail) {
        if (position == null) {
            return new EvalError(detail);
        }
        return new EvalError(position, detail);
    }

    private record ExactFraction(BigInteger numerator, BigInteger denominator) {
        private ExactFraction {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("denominator must not be zero");
            }
            if (denominator.signum() < 0) {
                numerator = numerator.negate();
                denominator = denominator.negate();
            }

            BigInteger divisor = numerator.gcd(denominator);
            if (divisor.signum() != 0) {
                numerator = numerator.divide(divisor);
                denominator = denominator.divide(divisor);
            }
        }

        private ExactFraction add(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.denominator).add(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator)
            );
        }

        private ExactFraction subtract(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.denominator).subtract(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator)
            );
        }

        private ExactFraction multiply(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.numerator),
                    denominator.multiply(other.denominator)
            );
        }

        private ExactFraction divide(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.denominator),
                    denominator.multiply(other.numerator)
            );
        }

        private ExactFraction negate() {
            return new ExactFraction(numerator.negate(), denominator);
        }

        private ExactFraction abs() {
            return new ExactFraction(numerator.abs(), denominator);
        }

        private int compareTo(ExactFraction other) {
            return numerator.multiply(other.denominator).compareTo(other.numerator.multiply(denominator));
        }

        private SchemeValue toSchemeValue() throws EvalError {
            try {
                if (denominator.equals(BigInteger.ONE)) {
                    return new IntValue(numerator.longValueExact());
                }
                return new RationalValue(numerator.longValueExact(), denominator.longValueExact());
            } catch (ArithmeticException error) {
                throw new EvalError("numeric overflow");
            }
        }
    }
}
