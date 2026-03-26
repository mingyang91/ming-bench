package ming;

import java.util.List;

sealed interface Step permits EvalExprStep, ReturnStep, ApplyStep, ThunkStep, DoneStep {
}

record EvalExprStep(Expr expr, Env env, Kont kont) implements Step {
}

record ReturnStep(Value value, Kont kont) implements Step {
}

record ApplyStep(Value operator, List<Value> arguments, Kont kont, int line, int column)
        implements Step {
}

record ThunkStep(StepSupplier supplier) implements Step {
}

record DoneStep(Value value) implements Step {
}

@FunctionalInterface
interface ContinuationBody {
    Step apply(Value value) throws EvalError;
}

@FunctionalInterface
interface StepSupplier {
    Step get() throws EvalError;
}

record Kont(int line, int column, ContinuationBody body) {
    Step apply(Value value) throws EvalError {
        try {
            return body.apply(value);
        } catch (EvalError error) {
            if (line <= 0 || column <= 0) {
                throw error;
            }
            throw error.withPosition(line, column);
        }
    }
}

final class DynamicWindFrame {
    private final Value inThunk;
    private final Value outThunk;

    DynamicWindFrame(Value inThunk, Value outThunk) {
        this.inThunk = inThunk;
        this.outThunk = outThunk;
    }

    Value inThunk() {
        return inThunk;
    }

    Value outThunk() {
        return outThunk;
    }
}

@FunctionalInterface
interface ExceptionHandlerBody {
    Step apply(RaisedException exception) throws EvalError;
}

record ExceptionHandlerFrame(List<DynamicWindFrame> dynamicStack,
        ExceptionHandlerFrame parent, ExceptionHandlerBody body) {
}

record CapturedContinuation(Kont target, List<DynamicWindFrame> dynamicStack,
        ExceptionHandlerFrame exceptionHandlerStack) {
}

final class ContinuationJump extends RuntimeException {
    private final CapturedContinuation target;
    private final Value value;

    ContinuationJump(CapturedContinuation target, Value value) {
        this.target = target;
        this.value = value;
    }

    CapturedContinuation target() {
        return target;
    }

    Value value() {
        return value;
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        return this;
    }
}

final class RaisedException extends RuntimeException {
    private final Value value;
    private final int line;
    private final int column;

    RaisedException(Value value, int line, int column) {
        this.value = value;
        this.line = line;
        this.column = column;
    }

    Value value() {
        return value;
    }

    int line() {
        return line;
    }

    int column() {
        return column;
    }

    boolean hasPosition() {
        return line > 0 && column > 0;
    }

    @Override
    public synchronized Throwable fillInStackTrace() {
        return this;
    }
}
