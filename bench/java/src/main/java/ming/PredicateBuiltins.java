package ming;

import static ming.EvaluatorSupport.isProcedure;
import static ming.EvaluatorSupport.isTruthy;
import static ming.EvaluatorSupport.requireChar;
import static ming.EvaluatorSupport.requireExactArgs;
import static ming.EvaluatorSupport.requireMinArgs;
import static ming.EvaluatorSupport.requireString;
import static ming.RuntimeConstants.FALSE;
import static ming.RuntimeConstants.TRUE;
import static ming.RuntimeConstants.boolValue;
import static ming.ValueSupport.compareExact;
import static ming.ValueSupport.containsInexact;
import static ming.ValueSupport.eqValues;
import static ming.ValueSupport.equalValues;
import static ming.ValueSupport.exactValue;
import static ming.ValueSupport.inexactToExactValue;
import static ming.ValueSupport.isExactNumber;
import static ming.ValueSupport.isInteger;
import static ming.ValueSupport.isNumber;
import static ming.ValueSupport.numberSign;
import static ming.ValueSupport.requireExactRational;
import static ming.ValueSupport.requireNumberAsDouble;
import static ming.ValueSupport.requireNumericValue;
import static ming.ValueSupport.requireRationalParts;

import java.util.List;
import java.util.function.IntPredicate;
import java.util.function.LongPredicate;
import java.util.function.Predicate;

final class PredicateBuiltins {
    private PredicateBuiltins() {
    }

    static Value compare(List<Value> arguments, String operator) throws EvalError {
        requireMinArgs(operator, arguments, 2);
        if (containsInexact(arguments)) {
            for (int index = 0; index < arguments.size() - 1; index++) {
                double left = requireNumberAsDouble(arguments.get(index), operator);
                double right = requireNumberAsDouble(arguments.get(index + 1), operator);
                if (!comparePair(Double.compare(left, right), operator)) {
                    return FALSE;
                }
            }
            return TRUE;
        }

        for (int index = 0; index < arguments.size() - 1; index++) {
            ExactRational left = requireExactRational(arguments.get(index), operator);
            ExactRational right = requireExactRational(arguments.get(index + 1), operator);
            if (!comparePair(compareExact(left, right), operator)) {
                return FALSE;
            }
        }
        return TRUE;
    }

    static Value compareChars(List<Value> arguments, String operator) throws EvalError {
        requireMinArgs(operator, arguments, 2);
        for (int index = 0; index < arguments.size() - 1; index++) {
            char left = requireChar(arguments.get(index), operator);
            char right = requireChar(arguments.get(index + 1), operator);
            if ("char=?".equals(operator) && left != right) {
                return FALSE;
            }
            if ("char<?".equals(operator) && left >= right) {
                return FALSE;
            }
        }
        return TRUE;
    }

    static Value compareStrings(List<Value> arguments, String operator) throws EvalError {
        requireMinArgs(operator, arguments, 2);
        for (int index = 0; index < arguments.size() - 1; index++) {
            String left = requireString(arguments.get(index), operator);
            String right = requireString(arguments.get(index + 1), operator);
            boolean matches = switch (operator) {
                case "string=?" -> left.equals(right);
                case "string<?" -> left.compareTo(right) < 0;
                case "string-ci=?" -> left.equalsIgnoreCase(right);
                default -> throw new EvalError("unknown string comparison operator: " + operator);
            };
            if (!matches) {
                return FALSE;
            }
        }
        return TRUE;
    }

    static Value not(List<Value> arguments) throws EvalError {
        requireExactArgs("not", arguments, 1);
        return boolValue(!isTruthy(arguments.get(0)));
    }

    static Value signPredicate(String name, List<Value> arguments,
            IntPredicate predicate) throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(predicate.test(numberSign(arguments.get(0), name)));
    }

    static Value integerNumericPredicate(String name, List<Value> arguments,
            LongPredicate predicate) throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(predicate.test(ValueSupport.requireInt(arguments.get(0), name)));
    }

    static Value charAlphabetic(List<Value> arguments) throws EvalError {
        requireExactArgs("char-alphabetic?", arguments, 1);
        return boolValue(Character.isLetter(requireChar(arguments.get(0), "char-alphabetic?")));
    }

    static Value charNumeric(List<Value> arguments) throws EvalError {
        requireExactArgs("char-numeric?", arguments, 1);
        return boolValue(Character.isDigit(requireChar(arguments.get(0), "char-numeric?")));
    }

    static Value charUpcase(List<Value> arguments) throws EvalError {
        requireExactArgs("char-upcase", arguments, 1);
        return new CharValue(Character.toUpperCase(requireChar(arguments.get(0), "char-upcase")));
    }

    static Value charDowncase(List<Value> arguments) throws EvalError {
        requireExactArgs("char-downcase", arguments, 1);
        return new CharValue(Character.toLowerCase(requireChar(arguments.get(0), "char-downcase")));
    }

    static Value eq(List<Value> arguments) throws EvalError {
        requireExactArgs("eq?", arguments, 2);
        return boolValue(eqValues(arguments.get(0), arguments.get(1)));
    }

    static Value eqv(List<Value> arguments) throws EvalError {
        requireExactArgs("eqv?", arguments, 2);
        return boolValue(eqValues(arguments.get(0), arguments.get(1)));
    }

    static Value equal(List<Value> arguments) throws EvalError {
        requireExactArgs("equal?", arguments, 2);
        return boolValue(equalValues(arguments.get(0), arguments.get(1)));
    }

    static Value procedurePredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("procedure?", arguments, 1);
        return boolValue(isProcedure(arguments.get(0)));
    }

    static Value numberPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("number?", arguments, 1);
        return boolValue(isNumber(arguments.get(0)));
    }

    static Value integerPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("integer?", arguments, 1);
        return boolValue(isInteger(arguments.get(0)));
    }

    static Value rationalPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("rational?", arguments, 1);
        return boolValue(isExactNumber(arguments.get(0)));
    }

    static Value exactPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("exact?", arguments, 1);
        return boolValue(isExactNumber(arguments.get(0)));
    }

    static Value inexactPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("inexact?", arguments, 1);
        return boolValue(arguments.get(0) instanceof InexactValue);
    }

    static Value exactToInexact(List<Value> arguments) throws EvalError {
        requireExactArgs("exact->inexact", arguments, 1);
        return new InexactValue(requireNumberAsDouble(arguments.get(0), "exact->inexact"));
    }

    static Value inexactToExact(List<Value> arguments) throws EvalError {
        requireExactArgs("inexact->exact", arguments, 1);
        Value value = requireNumericValue(arguments.get(0), "inexact->exact");
        if (value instanceof InexactValue inexactValue) {
            return inexactToExactValue(inexactValue.value());
        }
        return value;
    }

    static Value numerator(List<Value> arguments) throws EvalError {
        requireExactArgs("numerator", arguments, 1);
        ExactRational rational = requireRationalParts(arguments.get(0), "numerator");
        return exactValue(rational.numerator(), 1L);
    }

    static Value denominator(List<Value> arguments) throws EvalError {
        requireExactArgs("denominator", arguments, 1);
        ExactRational rational = requireRationalParts(arguments.get(0), "denominator");
        return exactValue(rational.denominator(), 1L);
    }

    static Value typePredicate(String name, List<Value> arguments, Predicate<Value> predicate)
            throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(predicate.test(arguments.get(0)));
    }

    private static boolean comparePair(int comparison, String operator)
            throws EvalError {
        return switch (operator) {
            case "<" -> comparison < 0;
            case ">" -> comparison > 0;
            case "=" -> comparison == 0;
            case "<=" -> comparison <= 0;
            default -> throw new EvalError("unknown comparison operator: " + operator);
        };
    }
}
