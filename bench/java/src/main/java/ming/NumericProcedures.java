package ming;

import java.math.BigInteger;
import java.util.List;

final class NumericProcedures {
    private final ValueSupport valueSupport;

    NumericProcedures(ValueSupport valueSupport) {
        this.valueSupport = valueSupport;
    }

    Value addNumbers(List<Value> args) throws EvalError {
        if (containsInexact(args)) {
            double total = 0.0;
            for (Value arg : args) {
                total += NumericSupport.toDouble(arg);
            }
            return new InexactValue(total);
        }

        ExactFraction total = ExactFraction.of(0);
        for (Value arg : args) {
            total = total.add(NumericSupport.toExactFraction(arg));
        }
        return NumericSupport.exactToValue(total);
    }

    Value subtractNumbers(List<Value> args) throws EvalError {
        valueSupport.requireAtLeast("-", args.size(), 1);

        if (containsInexact(args)) {
            double result = NumericSupport.toDouble(args.getFirst());
            if (args.size() == 1) {
                return new InexactValue(-result);
            }

            for (int index = 1; index < args.size(); index++) {
                result -= NumericSupport.toDouble(args.get(index));
            }
            return new InexactValue(result);
        }

        ExactFraction result = NumericSupport.toExactFraction(args.getFirst());
        if (args.size() == 1) {
            return NumericSupport.exactToValue(result.negate());
        }

        for (int index = 1; index < args.size(); index++) {
            result = result.subtract(NumericSupport.toExactFraction(args.get(index)));
        }
        return NumericSupport.exactToValue(result);
    }

    Value multiplyNumbers(List<Value> args) throws EvalError {
        if (containsInexact(args)) {
            double total = 1.0;
            for (Value arg : args) {
                total *= NumericSupport.toDouble(arg);
            }
            return new InexactValue(total);
        }

        ExactFraction total = ExactFraction.of(1);
        for (Value arg : args) {
            total = total.multiply(NumericSupport.toExactFraction(arg));
        }
        return NumericSupport.exactToValue(total);
    }

    Value divideNumbers(List<Value> args) throws EvalError {
        valueSupport.requireAtLeast("/", args.size(), 2);

        if (containsInexact(args)) {
            double result = NumericSupport.toDouble(args.getFirst());
            for (int index = 1; index < args.size(); index++) {
                double divisor = NumericSupport.toDouble(args.get(index));
                if (divisor == 0.0) {
                    throw new EvalError("division by zero");
                }
                result /= divisor;
            }
            return new InexactValue(result);
        }

        ExactFraction result = NumericSupport.toExactFraction(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            result = result.divide(NumericSupport.toExactFraction(args.get(index)));
        }
        return NumericSupport.exactToValue(result);
    }

    Value absBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireArity("abs", args.size(), 1);

        Value value = valueSupport.expectNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return new InexactValue(Math.abs(inexactValue.value()));
        }

        ExactFraction fraction = NumericSupport.toExactFraction(value);
        if (fraction.signum() < 0) {
            fraction = fraction.negate();
        }
        return NumericSupport.exactToValue(fraction);
    }

    int quotient(List<Value> args) throws EvalError {
        valueSupport.requireArity("quotient", args.size(), 2);

        int dividend = valueSupport.expectInt(args.get(0));
        int divisor = valueSupport.expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return dividend / divisor;
    }

    int remainder(List<Value> args) throws EvalError {
        valueSupport.requireArity("remainder", args.size(), 2);

        int dividend = valueSupport.expectInt(args.get(0));
        int divisor = valueSupport.expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return dividend % divisor;
    }

    int modulo(List<Value> args) throws EvalError {
        valueSupport.requireArity("modulo", args.size(), 2);

        int dividend = valueSupport.expectInt(args.get(0));
        int divisor = valueSupport.expectInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return Math.floorMod(dividend, divisor);
    }

    Value minBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireAtLeast("min", args.size(), 1);

        Value result = valueSupport.expectNumber(args.getFirst());
        boolean sawInexact = NumericSupport.isInexact(result);
        for (int index = 1; index < args.size(); index++) {
            Value current = valueSupport.expectNumber(args.get(index));
            if (NumericSupport.compare(current, result) < 0) {
                result = current;
            }
            sawInexact |= NumericSupport.isInexact(current);
        }
        if (sawInexact && NumericSupport.isExact(result)) {
            return exactToInexact(result);
        }
        return result;
    }

    Value maxBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireAtLeast("max", args.size(), 1);

        Value result = valueSupport.expectNumber(args.getFirst());
        boolean sawInexact = NumericSupport.isInexact(result);
        for (int index = 1; index < args.size(); index++) {
            Value current = valueSupport.expectNumber(args.get(index));
            if (NumericSupport.compare(current, result) > 0) {
                result = current;
            }
            sawInexact |= NumericSupport.isInexact(current);
        }
        if (sawInexact && NumericSupport.isExact(result)) {
            return exactToInexact(result);
        }
        return result;
    }

    int expt(List<Value> args) throws EvalError {
        valueSupport.requireArity("expt", args.size(), 2);

        int base = valueSupport.expectInt(args.get(0));
        int exponent = valueSupport.expectInt(args.get(1));
        if (exponent < 0) {
            throw new EvalError("expt exponent must be non-negative");
        }

        int result = 1;
        for (int index = 0; index < exponent; index++) {
            result *= base;
        }
        return result;
    }

    boolean compareIncreasing(List<Value> args, Comparison comparison) throws EvalError {
        valueSupport.requireAtLeast(comparison.symbol(), args.size(), 2);

        Value previous = valueSupport.expectNumber(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            Value current = valueSupport.expectNumber(args.get(index));
            if (!comparison.matches(NumericSupport.compare(previous, current))) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    Value gcdBuiltin(List<Value> args) throws EvalError {
        BigInteger result = BigInteger.ZERO;
        for (Value arg : args) {
            result = result.gcd(NumericSupport.expectExactInteger(arg).abs());
        }
        return NumericSupport.integerToValue(result);
    }

    Value lcmBuiltin(List<Value> args) throws EvalError {
        BigInteger result = BigInteger.ONE;
        boolean sawArgument = false;

        for (Value arg : args) {
            BigInteger value = NumericSupport.expectExactInteger(arg).abs();
            sawArgument = true;
            if (value.signum() == 0) {
                result = BigInteger.ZERO;
                break;
            }
            result = result.divide(result.gcd(value)).multiply(value);
        }

        if (!sawArgument) {
            return new IntValue(1);
        }
        return NumericSupport.integerToValue(result);
    }

    Value truncateBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireArity("truncate", args.size(), 1);

        Value value = valueSupport.expectNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            double raw = inexactValue.value();
            return new InexactValue(raw < 0.0 ? Math.ceil(raw) : Math.floor(raw));
        }

        ExactFraction fraction = NumericSupport.toExactFraction(value);
        return NumericSupport.integerToValue(
                fraction.numerator().divide(fraction.denominator()));
    }

    Value roundBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireArity("round", args.size(), 1);

        Value value = valueSupport.expectNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return new InexactValue(Math.rint(inexactValue.value()));
        }

        ExactFraction fraction = NumericSupport.toExactFraction(value);
        BigInteger[] quotientAndRemainder = fraction.numerator().divideAndRemainder(
                fraction.denominator());
        BigInteger quotient = quotientAndRemainder[0];
        BigInteger doubledRemainder = quotientAndRemainder[1].abs().multiply(BigInteger.TWO);
        int relation = doubledRemainder.compareTo(fraction.denominator());

        if (relation > 0 || (relation == 0 && quotient.testBit(0))) {
            quotient = quotient.add(BigInteger.valueOf(fraction.signum()));
        }
        return NumericSupport.integerToValue(quotient);
    }

    Value stringToNumber(String token) {
        ParsedNumber parsedNumber;
        try {
            parsedNumber = NumericSupport.parseLiteral(token);
        } catch (IllegalArgumentException error) {
            return BoolValue.FALSE;
        }

        if (parsedNumber == null) {
            return BoolValue.FALSE;
        }
        return parsedNumberToValue(parsedNumber);
    }

    Value exactToInexact(Value value) throws EvalError {
        return new InexactValue(NumericSupport.toDouble(value));
    }

    Value numeratorBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireArity("numerator", args.size(), 1);
        ExactFraction fraction = NumericSupport.toExactFraction(args.getFirst());
        return NumericSupport.integerToValue(fraction.numerator());
    }

    Value denominatorBuiltin(List<Value> args) throws EvalError {
        valueSupport.requireArity("denominator", args.size(), 1);
        ExactFraction fraction = NumericSupport.toExactFraction(args.getFirst());
        return NumericSupport.integerToValue(fraction.denominator());
    }

    BoolValue signPredicate(String name, List<Value> args, int expectedSign)
            throws EvalError {
        valueSupport.requireArity(name, args.size(), 1);

        Value value = valueSupport.expectNumber(args.getFirst());
        int sign;
        if (NumericSupport.isExact(value)) {
            sign = NumericSupport.toExactFraction(value).signum();
        } else {
            sign = Double.compare(((InexactValue) value).value(), 0.0);
        }
        return BoolValue.of(sign == expectedSign || (expectedSign == 1 && sign > 0)
                || (expectedSign == -1 && sign < 0));
    }

    private boolean containsInexact(List<Value> args) throws EvalError {
        for (Value arg : args) {
            valueSupport.expectNumber(arg);
            if (NumericSupport.isInexact(arg)) {
                return true;
            }
        }
        return false;
    }

    private Value parsedNumberToValue(ParsedNumber parsedNumber) {
        return switch (parsedNumber) {
            case ParsedInteger parsedInteger -> new IntValue(parsedInteger.value());
            case ParsedRational parsedRational -> NumericSupport.exactToValue(
                    new ExactFraction(parsedRational.numerator(), parsedRational.denominator()));
            case ParsedInexact parsedInexact -> new InexactValue(parsedInexact.value());
        };
    }
}
