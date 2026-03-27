package ming;

import java.util.HashSet;
import java.util.Set;

final class ValueEquality {
    boolean eqv(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (left instanceof NumberValue leftNumber && right instanceof NumberValue rightNumber) {
            return leftNumber.value().numericallyEquals(rightNumber.value());
        }
        if (left instanceof BooleanValue leftBoolean && right instanceof BooleanValue rightBoolean) {
            return leftBoolean.value() == rightBoolean.value();
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.codePoint() == rightChar.codePoint();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return false;
    }

    boolean equal(Value left, Value right) {
        return equal(left, right, new HashSet<>());
    }

    private boolean equal(Value left, Value right, Set<ComparisonKey> seen) {
        if (eqv(left, right)) {
            return true;
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.text().equals(rightString.text());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            ComparisonKey comparison = new ComparisonKey(leftPair, rightPair);
            if (!seen.add(comparison)) {
                return true;
            }
            return equal(leftPair.car(), rightPair.car(), seen)
                    && equal(leftPair.cdr(), rightPair.cdr(), seen);
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            ComparisonKey comparison = new ComparisonKey(leftVector, rightVector);
            if (!seen.add(comparison)) {
                return true;
            }
            if (leftVector.size() != rightVector.size()) {
                return false;
            }
            for (int index = 0; index < leftVector.size(); index++) {
                if (!equal(leftVector.element(index), rightVector.element(index), seen)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private record ComparisonKey(Value left, Value right) {
    }
}
