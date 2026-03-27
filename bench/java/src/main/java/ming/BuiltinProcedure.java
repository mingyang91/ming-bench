package ming;

import java.util.List;

@FunctionalInterface
interface BuiltinInvoker {
    Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
}

final class BuiltinProcedure implements Value, Procedure {
    private final String name;
    private final BuiltinInvoker invoker;

    BuiltinProcedure(String name, BuiltinInvoker invoker) {
        this.name = name;
        this.invoker = invoker;
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return invoker.apply(arguments, callLoc);
    }
}
