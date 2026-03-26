package ming;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

final class CollectionProcedures {
    @FunctionalInterface
    private interface MatchPredicate {
        boolean matches(Value left, Value right) throws EvalError;
    }

    private final Evaluator evaluator;

    CollectionProcedures(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    Value setCarBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("set-car!", args.size(), 2);
        evaluator.expectPair(args.getFirst()).setCar(args.get(1));
        return VoidValue.INSTANCE;
    }

    Value setCdrBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("set-cdr!", args.size(), 2);
        evaluator.expectPair(args.getFirst()).setCdr(args.get(1));
        return VoidValue.INSTANCE;
    }

    int lengthOfList(Value value) throws EvalError {
        int length = 0;
        Value current = value;
        IdentityHashMap<PairValue, Boolean> seenPairs = new IdentityHashMap<>();
        while (current instanceof PairValue pairValue) {
            ensureAcyclicListPair(pairValue, seenPairs);
            length++;
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw new EvalError("expected list");
    }

    List<Value> listElements(Value value) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value;
        IdentityHashMap<PairValue, Boolean> seenPairs = new IdentityHashMap<>();
        while (current instanceof PairValue pairValue) {
            ensureAcyclicListPair(pairValue, seenPairs);
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw new EvalError("expected list");
    }

    Value listRef(List<Value> args) throws EvalError {
        evaluator.requireArity("list-ref", args.size(), 2);

        int index = evaluator.expectIndex(args.get(1), "list-ref");
        Value current = args.getFirst();
        for (int remaining = index; remaining >= 0; remaining--) {
            if (!(current instanceof PairValue pairValue)) {
                throw new EvalError("list-ref index out of range");
            }
            if (remaining == 0) {
                return pairValue.car();
            }
            current = pairValue.cdr();
        }

        throw new EvalError("list-ref index out of range");
    }

    Value listTailBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("list-tail", args.size(), 2);
        return listTail(args.getFirst(), evaluator.expectIndex(args.get(1), "list-tail"));
    }

    Value cxrBuiltin(String name, List<Value> args) throws EvalError {
        evaluator.requireArity(name, args.size(), 1);

        Value current = args.getFirst();
        for (int index = name.length() - 2; index >= 1; index--) {
            PairValue pairValue = evaluator.expectPair(current);
            current = switch (name.charAt(index)) {
                case 'a' -> pairValue.car();
                case 'd' -> pairValue.cdr();
                default -> throw new IllegalArgumentException("invalid cxr builtin: " + name);
            };
        }
        return current;
    }

    boolean isProperList(Value value) {
        Value current = value;
        IdentityHashMap<PairValue, Boolean> seenPairs = new IdentityHashMap<>();
        while (current instanceof PairValue pairValue) {
            if (seenPairs.put(pairValue, Boolean.TRUE) != null) {
                return false;
            }
            current = pairValue.cdr();
        }
        return current instanceof EmptyListValue;
    }

    Value makeVectorBuiltin(List<Value> args) throws EvalError {
        if (args.size() < 1 || args.size() > 2) {
            throw new EvalError("wrong number of arguments for make-vector: expected 1 or 2, got "
                    + args.size());
        }

        int length = evaluator.expectIndex(args.getFirst(), "make-vector");
        Value fill = args.size() == 2 ? args.get(1) : VoidValue.INSTANCE;
        List<Value> elements = new ArrayList<>(length);
        for (int index = 0; index < length; index++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    Value vectorRefBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("vector-ref", args.size(), 2);

        VectorValue vector = evaluator.expectVectorValue(args.getFirst());
        int index = evaluator.expectIndex(args.get(1), "vector-ref");
        if (index >= vector.length()) {
            throw new EvalError("vector-ref index out of range");
        }
        return vector.element(index);
    }

    Value vectorSetBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("vector-set!", args.size(), 3);

        VectorValue vector = evaluator.expectVectorValue(args.getFirst());
        int index = evaluator.expectIndex(args.get(1), "vector-set!");
        if (index >= vector.length()) {
            throw new EvalError("vector-set! index out of range");
        }
        vector.setElement(index, args.get(2));
        return VoidValue.INSTANCE;
    }

    Value appendLists(List<Value> args) throws EvalError {
        if (args.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        Value result = args.get(args.size() - 1);
        for (int argIndex = args.size() - 2; argIndex >= 0; argIndex--) {
            List<Value> elements = listElements(args.get(argIndex));
            for (int elementIndex = elements.size() - 1; elementIndex >= 0; elementIndex--) {
                result = new PairValue(elements.get(elementIndex), result);
            }
        }
        return result;
    }

    Value reverseBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("reverse", args.size(), 1);

        Value result = EmptyListValue.INSTANCE;
        Value current = args.getFirst();
        IdentityHashMap<PairValue, Boolean> seenPairs = new IdentityHashMap<>();
        while (current instanceof PairValue pairValue) {
            ensureAcyclicListPair(pairValue, seenPairs);
            result = new PairValue(pairValue.car(), result);
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return result;
        }
        throw new EvalError("expected list");
    }

    Value applyBuiltin(List<Value> args) throws EvalError {
        evaluator.requireAtLeast("apply", args.size(), 2);

        List<Value> expandedArgs = new ArrayList<>();
        for (int index = 1; index < args.size() - 1; index++) {
            expandedArgs.add(args.get(index));
        }
        expandedArgs.addAll(listElements(args.get(args.size() - 1)));

        return evaluator.applyProcedure(args.getFirst(), expandedArgs);
    }

    Value mapBuiltin(List<Value> args) throws EvalError {
        evaluator.requireAtLeast("map", args.size(), 2);

        Value procedure = args.getFirst();
        ParallelLists parallelLists = collectParallelLists("map", args);
        List<Value> results = new ArrayList<>(parallelLists.length());
        for (int elementIndex = 0; elementIndex < parallelLists.length(); elementIndex++) {
            results.add(evaluator.applyProcedure(
                    procedure, buildInvocationArgs(parallelLists.arguments(), elementIndex)));
        }
        return makeList(results);
    }

    Value assqBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("assq", args.size(), 2);
        return assocBuiltin(args.get(0), args.get(1), evaluator::isEq);
    }

    Value assocBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("assoc", args.size(), 2);
        return assocBuiltin(args.get(0), args.get(1), evaluator::isEqual);
    }

    Value assvBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("assv", args.size(), 2);
        return assocBuiltin(args.get(0), args.get(1), evaluator::isEqv);
    }

    Value memqBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("memq", args.size(), 2);
        return memberBuiltin(args.get(0), args.get(1), evaluator::isEq);
    }

    Value memvBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("memv", args.size(), 2);
        return memberBuiltin(args.get(0), args.get(1), evaluator::isEqv);
    }

    Value memberBuiltin(List<Value> args) throws EvalError {
        evaluator.requireArity("member", args.size(), 2);
        return memberBuiltin(args.get(0), args.get(1), evaluator::isEqual);
    }

    private Value assocBuiltin(Value key, Value list, MatchPredicate predicate)
            throws EvalError {
        Value current = list;
        IdentityHashMap<PairValue, Boolean> seenPairs = new IdentityHashMap<>();
        while (current instanceof PairValue pairValue) {
            ensureAcyclicListPair(pairValue, seenPairs);
            Value entry = pairValue.car();
            PairValue association = evaluator.expectPair(entry);
            if (predicate.matches(key, association.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("expected list");
    }

    private Value memberBuiltin(Value key, Value list, MatchPredicate predicate)
            throws EvalError {
        Value current = list;
        IdentityHashMap<PairValue, Boolean> seenPairs = new IdentityHashMap<>();
        while (current instanceof PairValue pairValue) {
            ensureAcyclicListPair(pairValue, seenPairs);
            if (predicate.matches(key, pairValue.car())) {
                return current;
            }
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw new EvalError("expected list");
    }

    Value forEachBuiltin(List<Value> args) throws EvalError {
        evaluator.requireAtLeast("for-each", args.size(), 2);

        Value procedure = args.getFirst();
        ParallelLists parallelLists = collectParallelLists("for-each", args);
        for (int elementIndex = 0; elementIndex < parallelLists.length(); elementIndex++) {
            evaluator.applyProcedure(
                    procedure, buildInvocationArgs(parallelLists.arguments(), elementIndex));
        }

        return VoidValue.INSTANCE;
    }

    Value makeList(List<Value> args) {
        Value result = EmptyListValue.INSTANCE;
        for (int index = args.size() - 1; index >= 0; index--) {
            result = new PairValue(args.get(index), result);
        }
        return result;
    }

    private List<Value> buildInvocationArgs(List<List<Value>> listArguments, int elementIndex) {
        List<Value> invocationArgs = new ArrayList<>(listArguments.size());
        for (List<Value> listArgument : listArguments) {
            invocationArgs.add(listArgument.get(elementIndex));
        }
        return invocationArgs;
    }

    private ParallelLists collectParallelLists(String name, List<Value> args) throws EvalError {
        List<List<Value>> listArguments = new ArrayList<>(args.size() - 1);
        Integer expectedLength = null;

        for (int index = 1; index < args.size(); index++) {
            List<Value> elements = listElements(args.get(index));
            if (expectedLength == null) {
                expectedLength = elements.size();
            } else if (elements.size() != expectedLength) {
                throw new EvalError(name + " lists must have the same length");
            }
            listArguments.add(elements);
        }

        return new ParallelLists(listArguments, expectedLength == null ? 0 : expectedLength);
    }

    private Value listTail(Value value, int index) throws EvalError {
        Value current = value;
        for (int remaining = index; remaining > 0; remaining--) {
            if (!(current instanceof PairValue pairValue)) {
                throw new EvalError("list-tail index out of range");
            }
            current = pairValue.cdr();
        }
        if (current instanceof PairValue || current instanceof EmptyListValue) {
            return current;
        }
        throw new EvalError("list-tail index out of range");
    }

    private void ensureAcyclicListPair(PairValue pairValue,
                                       IdentityHashMap<PairValue, Boolean> seenPairs)
            throws EvalError {
        if (seenPairs.put(pairValue, Boolean.TRUE) != null) {
            throw new EvalError("expected list");
        }
    }

    private record ParallelLists(List<List<Value>> arguments, int length) {
    }
}
