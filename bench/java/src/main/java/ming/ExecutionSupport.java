package ming;

import java.util.List;

@FunctionalInterface
interface ContinuationFrame {
    MachineState resume(Interpreter interpreter, Value value, Continuation next) throws EvalError;
}

sealed interface Continuation permits DoneContinuation, PendingContinuation {
}

enum DoneContinuation implements Continuation {
    INSTANCE
}

record PendingContinuation(ContinuationFrame frame, Continuation next) implements Continuation {
}

sealed interface MachineState permits EvalState, SequenceState, ReturnState, ApplyState {
}

record EvalState(Expr expression, Environment env, Continuation cont) implements MachineState {
}

record SequenceState(List<Expr> expressions, Environment env, Continuation cont)
        implements MachineState {
}

record ReturnState(Value value, Continuation cont) implements MachineState {
}

record ApplyState(Procedure procedure, List<Value> arguments, SourceLoc callLoc, Continuation cont)
        implements MachineState {
}
