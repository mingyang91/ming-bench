package ming;

@FunctionalInterface
interface ExceptionHandlerAction {
    Evaluator.Bounce handle(Value exceptionValue) throws EvalError;
}

final class ExceptionHandlerFrame {
    private final ExceptionHandlerFrame parent;
    private final ExceptionHandlerAction action;
    private final WindFrame windContext;

    ExceptionHandlerFrame(ExceptionHandlerFrame parent, ExceptionHandlerAction action,
                          WindFrame windContext) {
        this.parent = parent;
        this.action = action;
        this.windContext = windContext;
    }

    ExceptionHandlerFrame parent() {
        return parent;
    }

    ExceptionHandlerAction action() {
        return action;
    }

    WindFrame windContext() {
        return windContext;
    }
}
