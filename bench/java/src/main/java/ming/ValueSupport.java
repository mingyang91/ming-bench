package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

final class ValueSupport {
    Value quoteToValue(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case RationalExpr rationalExpr -> NumericSupport.exactToValue(
                    new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator()));
            case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> quoteListToValue(listExpr.elements());
        };
    }

    private Value quoteListToValue(List<Expr> elements) throws EvalError {
        Value result = EmptyListValue.INSTANCE;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(quoteToValue(elements.get(index)), result);
        }
        return result;
    }

    Value expectNumber(Value value) throws EvalError {
        if (NumericSupport.isNumber(value)) {
            return value;
        }
        throw new EvalError("expected number");
    }

    int expectInt(Value value) throws EvalError {
        BigInteger integer = NumericSupport.expectExactInteger(value);
        try {
            return integer.intValueExact();
        } catch (ArithmeticException error) {
            throw new EvalError("integer out of range");
        }
    }

    int expectIndex(Value value, String operationName) throws EvalError {
        int index = expectInt(value);
        if (index < 0) {
            throw new EvalError(operationName + " index out of range");
        }
        return index;
    }

    String expectString(Value value) throws EvalError {
        return expectStringValue(value).value();
    }

    StringValue expectStringValue(Value value) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError("expected string");
    }

    char expectChar(Value value) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError("expected character");
    }

    String expectSymbol(Value value) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw new EvalError("expected symbol");
    }

    SyntaxValue expectSyntax(Value value) throws EvalError {
        if (value instanceof SyntaxValue syntaxValue) {
            return syntaxValue;
        }
        throw new EvalError("expected syntax object");
    }

    PairValue expectPair(Value value) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError("expected pair");
    }

    VectorValue expectVectorValue(Value value) throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw new EvalError("expected vector");
    }

    RecordValue expectRecord(Value value, RecordType recordType) throws EvalError {
        if (value instanceof RecordValue recordValue && recordValue.type() == recordType) {
            return recordValue;
        }
        throw new EvalError("expected record of type " + recordType.name());
    }

    String stringAppend(List<Value> args) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value arg : args) {
            builder.append(expectString(arg));
        }
        return builder.toString();
    }

    Value syntaxToDatum(Value value) throws EvalError {
        return quoteToValue(expectSyntax(value).expr());
    }

    Expr datumToExpr(Value value, SourcePos position) throws EvalError {
        return switch (value) {
            case IntValue intValue -> new IntExpr(intValue.value(), position);
            case RationalValue rationalValue ->
                    new RationalExpr(rationalValue.numerator(), rationalValue.denominator(),
                            position);
            case InexactValue inexactValue -> new InexactExpr(inexactValue.value(), position);
            case BoolValue boolValue -> new BoolExpr(boolValue.value(), position);
            case StringValue stringValue -> new StringExpr(stringValue.value(), position);
            case CharValue charValue -> new CharExpr(charValue.value(), position);
            case SymbolValue symbolValue -> new SymbolExpr(symbolValue.name(), position);
            case EmptyListValue ignored -> new ListExpr(List.of(), position);
            case PairValue pairValue -> pairToExpr(pairValue, position);
            case SyntaxValue syntaxValue -> syntaxValue.expr();
            case SyntaxSequenceValue sequenceValue ->
                    throw new EvalError("expected a datum, got syntax sequence");
            case VectorValue vectorValue -> throw new EvalError("cannot convert vector to syntax");
            case RecordValue recordValue -> throw new EvalError("cannot convert record to syntax");
            case VoidValue ignored -> throw new EvalError("cannot convert void to syntax");
            case UninitializedValue ignored ->
                    throw new EvalError("cannot convert uninitialized value to syntax");
            case MultiValueValue multiValue ->
                    throw new EvalError("cannot convert multiple values to syntax");
            case ProcedureValue procedureValue ->
                    throw new EvalError("cannot convert procedure to syntax");
        };
    }

    private Expr pairToExpr(PairValue pairValue, SourcePos position) throws EvalError {
        List<Expr> elements = new ArrayList<>();
        Value current = pairValue;
        while (current instanceof PairValue pair) {
            elements.add(datumToExpr(pair.car(), position));
            current = pair.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw new EvalError("cannot convert dotted pair to syntax");
        }
        return new ListExpr(elements, position);
    }

    boolean compareChars(List<Value> args, String name, CharComparison comparison)
            throws EvalError {
        requireAtLeast(name, args.size(), 2);

        char previous = expectChar(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            char current = expectChar(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    boolean compareStrings(List<Value> args, String name, StringComparison comparison)
            throws EvalError {
        requireAtLeast(name, args.size(), 2);

        String previous = expectString(args.getFirst());
        for (int index = 1; index < args.size(); index++) {
            String current = expectString(args.get(index));
            if (!comparison.matches(previous, current)) {
                return false;
            }
            previous = current;
        }
        return true;
    }

    boolean isEq(Value left, Value right) throws EvalError {
        if (left == right) {
            return true;
        }
        if (NumericSupport.isNumber(left) && NumericSupport.isNumber(right)) {
            return NumericSupport.compare(left, right) == 0;
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return false;
    }

    boolean isEqv(Value left, Value right) throws EvalError {
        return isEq(left, right);
    }

    boolean isEqual(Value left, Value right) throws EvalError {
        return isEqual(left, right, new IdentityHashMap<>());
    }

    private boolean isEqual(Value left, Value right,
                            IdentityHashMap<Value, IdentityHashMap<Value, Boolean>> seenPairs)
            throws EvalError {
        if (left == right) {
            return true;
        }
        if (NumericSupport.isNumber(left) && NumericSupport.isNumber(right)) {
            return NumericSupport.compare(left, right) == 0;
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            if (alreadyCompared(leftPair, rightPair, seenPairs)) {
                return true;
            }
            return isEqual(leftPair.car(), rightPair.car(), seenPairs)
                    && isEqual(leftPair.cdr(), rightPair.cdr(), seenPairs);
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            if (leftVector.length() != rightVector.length()) {
                return false;
            }
            if (alreadyCompared(leftVector, rightVector, seenPairs)) {
                return true;
            }
            for (int index = 0; index < leftVector.length(); index++) {
                if (!isEqual(leftVector.element(index), rightVector.element(index), seenPairs)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    Value errorBuiltin(List<Value> args) throws EvalError {
        requireAtLeast("error", args.size(), 1);

        Value messageValue = args.getFirst();
        StringBuilder builder = new StringBuilder();
        if (messageValue instanceof StringValue stringValue) {
            builder.append(stringValue.value());
        } else {
            builder.append(messageValue.render());
        }

        for (int index = 1; index < args.size(); index++) {
            if (index == 1) {
                builder.append(':');
            }
            builder.append(' ');
            builder.append(args.get(index).render());
        }

        throw new EvalError(builder.toString());
    }

    BoolValue typePredicate(String name, List<Value> args, ValuePredicate predicate)
            throws EvalError {
        requireArity(name, args.size(), 1);
        return BoolValue.of(predicate.matches(args.getFirst()));
    }

    void requireArity(String name, int actual, int expected) throws EvalError {
        if (actual != expected) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected " + expected
                            + ", got " + actual
            );
        }
    }

    void requireAtLeast(String name, int actual, int minimum) throws EvalError {
        if (actual < minimum) {
            throw new EvalError(
                    "wrong number of arguments for " + name + ": expected at least "
                            + minimum + ", got " + actual
            );
        }
    }

    boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private boolean alreadyCompared(Value left, Value right,
                                    IdentityHashMap<Value,
                                            IdentityHashMap<Value, Boolean>> seenPairs) {
        IdentityHashMap<Value, Boolean> rightValues = seenPairs.get(left);
        if (rightValues == null) {
            rightValues = new IdentityHashMap<>();
            seenPairs.put(left, rightValues);
        } else if (rightValues.containsKey(right)) {
            return true;
        }

        rightValues.put(right, Boolean.TRUE);
        return false;
    }
}
