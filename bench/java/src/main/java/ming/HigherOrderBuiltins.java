package ming;

import java.util.ArrayList;
import java.util.List;

final class HigherOrderBuiltins {
    private static final VoidValue VOID = VoidValue.INSTANCE;

    private final Builtins builtins;

    HigherOrderBuiltins(Builtins builtins) {
        this.builtins = builtins;
    }

    Value applyCallWithCurrentContinuation(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        ensureExactly("call/cc", arguments, 1, callLoc);
        Procedure procedure = requireProcedure(arguments.getFirst(), "call/cc expects a procedure",
                callLoc);
        return procedure.apply(List.of(new ContinuationProcedure(DoneContinuation.INSTANCE)), callLoc);
    }

    MachineState invokeApply(
            Interpreter interpreter,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        ensureAtLeast("apply", arguments, 2, callLoc);
        Procedure procedure = requireProcedure(
                arguments.getFirst(),
                "apply expects a procedure as its first argument",
                callLoc
        );

        List<Value> appliedArguments = new ArrayList<>();
        for (int index = 1; index < arguments.size() - 1; index++) {
            appliedArguments.add(arguments.get(index));
        }
        appliedArguments.addAll(builtins.requireProperListValue(
                arguments.get(arguments.size() - 1),
                "apply",
                callLoc
        ));
        return interpreter.applyProcedure(procedure, List.copyOf(appliedArguments), callLoc, cont);
    }

    MachineState invokeCallWithCurrentContinuation(
            Interpreter interpreter,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        ensureExactly("call/cc", arguments, 1, callLoc);
        Procedure procedure = requireProcedure(arguments.getFirst(), "call/cc expects a procedure",
                callLoc);
        return interpreter.applyProcedure(
                procedure,
                List.of(new ContinuationProcedure(cont)),
                callLoc,
                cont
        );
    }

    MachineState invokeMap(
            Interpreter interpreter,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        ensureAtLeast("map", arguments, 2, callLoc);
        Procedure procedure = requireProcedure(
                arguments.getFirst(),
                "map expects a procedure as its first argument",
                callLoc
        );
        List<List<Value>> lists = requireEqualLengthLists(
                arguments.subList(1, arguments.size()),
                "map",
                callLoc
        );
        return continueMap(interpreter, procedure, lists, 0, List.of(), callLoc, cont);
    }

    MachineState invokeForEach(
            Interpreter interpreter,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        ensureAtLeast("for-each", arguments, 2, callLoc);
        Procedure procedure = requireProcedure(
                arguments.getFirst(),
                "for-each expects a procedure as its first argument",
                callLoc
        );
        List<List<Value>> lists = requireEqualLengthLists(
                arguments.subList(1, arguments.size()),
                "for-each",
                callLoc
        );
        return continueForEach(interpreter, procedure, lists, 0, callLoc, cont);
    }

    private MachineState continueMap(
            Interpreter interpreter,
            Procedure procedure,
            List<List<Value>> lists,
            int elementIndex,
            List<Value> results,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        int expectedLength = lists.getFirst().size();
        if (elementIndex >= expectedLength) {
            return interpreter.deliver(builtins.buildListValue(results), cont);
        }

        List<Value> mappedArguments = new ArrayList<>(lists.size());
        for (List<Value> list : lists) {
            mappedArguments.add(list.get(elementIndex));
        }

        List<Value> resultSnapshot = List.copyOf(results);
        Continuation nextCont = new PendingContinuation((nextInterpreter, value, next) ->
                continueMap(
                        nextInterpreter,
                        procedure,
                        lists,
                        elementIndex + 1,
                        appendValue(resultSnapshot, value),
                        callLoc,
                        next
                ), cont);
        return interpreter.applyProcedure(procedure, List.copyOf(mappedArguments), callLoc, nextCont);
    }

    private MachineState continueForEach(
            Interpreter interpreter,
            Procedure procedure,
            List<List<Value>> lists,
            int elementIndex,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        int expectedLength = lists.getFirst().size();
        if (elementIndex >= expectedLength) {
            return interpreter.deliver(VOID, cont);
        }

        List<Value> appliedArguments = new ArrayList<>(lists.size());
        for (List<Value> list : lists) {
            appliedArguments.add(list.get(elementIndex));
        }

        Continuation nextCont = new PendingContinuation((nextInterpreter, ignored, next) ->
                continueForEach(
                        nextInterpreter,
                        procedure,
                        lists,
                        elementIndex + 1,
                        callLoc,
                        next
                ), cont);
        return interpreter.applyProcedure(procedure, List.copyOf(appliedArguments), callLoc, nextCont);
    }

    private Procedure requireProcedure(Value value, String message, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof Procedure procedure) {
            return procedure;
        }
        throw builtins.errorAt(callLoc, message);
    }

    private List<List<Value>> requireEqualLengthLists(
            List<Value> listArguments,
            String procedureName,
            SourceLoc callLoc
    ) throws EvalError {
        List<List<Value>> lists = new ArrayList<>(listArguments.size());
        int expectedLength = -1;
        for (Value argument : listArguments) {
            List<Value> elements = builtins.requireProperListValue(argument, procedureName, callLoc);
            if (expectedLength == -1) {
                expectedLength = elements.size();
            } else if (elements.size() != expectedLength) {
                throw builtins.errorAt(
                        callLoc,
                        procedureName + " expects lists of equal length"
                );
            }
            lists.add(elements);
        }
        return List.copyOf(lists);
    }

    private List<Value> appendValue(List<Value> values, Value value) {
        List<Value> updated = new ArrayList<>(values.size() + 1);
        updated.addAll(values);
        updated.add(value);
        return List.copyOf(updated);
    }

    private void ensureExactly(
            String procedureName,
            List<Value> arguments,
            int expected,
            SourceLoc callLoc
    ) throws EvalError {
        if (arguments.size() != expected) {
            throw builtins.errorAt(
                    callLoc,
                    procedureName + " expected " + expected + " arguments but got "
                            + arguments.size()
            );
        }
    }

    private void ensureAtLeast(
            String procedureName,
            List<Value> arguments,
            int minimum,
            SourceLoc callLoc
    ) throws EvalError {
        if (arguments.size() < minimum) {
            throw builtins.errorAt(
                    callLoc,
                    procedureName + " expected at least " + minimum + " arguments but got "
                            + arguments.size()
            );
        }
    }
}
