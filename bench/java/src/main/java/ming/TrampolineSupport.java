package ming;

import java.util.List;

@FunctionalInterface
interface Bounce {
    Bounce run() throws EvalError;
}

@FunctionalInterface
interface Continuation {
    Bounce resume(List<Value> values) throws EvalError;
}

@FunctionalInterface
interface SingleValueContinuation {
    Bounce resume(Value value) throws EvalError;
}

@FunctionalInterface
interface ValueListContinuation {
    Bounce resume(List<Value> values) throws EvalError;
}

@FunctionalInterface
interface BounceFactory {
    Bounce create(Continuation halt) throws EvalError;
}
