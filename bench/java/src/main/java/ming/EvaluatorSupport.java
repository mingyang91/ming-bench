package ming;

import static ming.RuntimeConstants.EMPTY_LIST;
import static ming.ValueSupport.requireInt;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;

final class EvaluatorSupport {
    private EvaluatorSupport() {
    }

    static Value listValue(List<Value> values) {
        Value result = EMPTY_LIST;
        for (int index = values.size() - 1; index >= 0; index--) {
            result = new PairValue(values.get(index), result);
        }
        return result;
    }

    static void requireMinArgs(String name, List<?> arguments, int min)
            throws EvalError {
        if (arguments.size() < min) {
            throw new EvalError(name + " expected at least "
                    + min + " argument(s)");
        }
    }

    static void requireExactArgs(String name, List<?> arguments, int exact)
            throws EvalError {
        if (arguments.size() != exact) {
            throw new EvalError(name + " expected exactly "
                    + exact + " argument(s)");
        }
    }

    static String requireString(Value value, String operator) throws EvalError {
        return requireStringValue(value, operator).text();
    }

    static StringValue requireStringValue(Value value, String operator) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError(operator + " expects string arguments");
    }

    static String requireSymbol(Value value, String operator) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError(operator + " expects symbol arguments");
    }

    static char requireChar(Value value, String operator) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError(operator + " expects character arguments");
    }

    static int requireIndex(Value value, String operator) throws EvalError {
        long index = requireInt(value, operator);
        if (index < 0L || index > Integer.MAX_VALUE) {
            throw new EvalError(operator + " expects a valid index");
        }
        return (int) index;
    }

    static PairValue requirePair(Value value, String operator) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError(operator + " expects a pair");
    }

    static VectorValue requireVector(Value value, String operator) throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw new EvalError(operator + " expects a vector");
    }

    static int requireVectorIndex(VectorValue vector, Value value, String operator)
            throws EvalError {
        int index = requireIndex(value, operator);
        if (index >= vector.length()) {
            throw new EvalError(operator + " index out of bounds");
        }
        return index;
    }

    static List<Value> requireProperList(Value value, String operator) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Set<PairValue> seen = Collections.newSetFromMap(new IdentityHashMap<>());
        Value current = value;
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw new EvalError(operator + " expects a proper list");
            }
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw new EvalError(operator + " expects a proper list");
        }
        return elements;
    }

    static boolean isProperListValue(Value value) {
        Value slow = value;
        Value fast = value;
        while (true) {
            if (fast instanceof EmptyListValue) {
                return true;
            }
            if (!(fast instanceof PairValue fastPair2)) {
                return false;
            }
            fast = fastPair2.cdr();

            if (fast instanceof EmptyListValue) {
                return true;
            }
            if (!(fast instanceof PairValue fastPair)) {
                return false;
            }
            fast = fastPair.cdr();

            if (!(slow instanceof PairValue slowPair)) {
                return false;
            }
            slow = slowPair.cdr();

            if (fast == slow) {
                return false;
            }
        }
    }

    static boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    static boolean isProcedure(Value value) {
        return value instanceof BuiltinValue
                || value instanceof ClosureValue
                || value instanceof CaseLambdaValue;
    }
}
