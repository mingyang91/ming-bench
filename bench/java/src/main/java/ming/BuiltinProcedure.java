package ming;

import java.util.List;

@FunctionalInterface
interface BuiltinInvoker {
    Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
}

@FunctionalInterface
interface MachineBuiltinInvoker {
    MachineState apply(
            Interpreter interpreter,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError;
}

final class BuiltinProcedure implements Value, Procedure {
    private final String name;
    private final BuiltinInvoker invoker;
    private final MachineBuiltinInvoker machineInvoker;

    BuiltinProcedure(String name, BuiltinInvoker invoker) {
        this(name, invoker, null);
    }

    BuiltinProcedure(String name, BuiltinInvoker invoker, MachineBuiltinInvoker machineInvoker) {
        this.name = name;
        this.invoker = invoker;
        this.machineInvoker = machineInvoker;
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return invoker.apply(arguments, callLoc);
    }

    MachineState invoke(
            Interpreter interpreter,
            List<Value> arguments,
            SourceLoc callLoc,
            Continuation cont
    ) throws EvalError {
        if (machineInvoker != null) {
            return machineInvoker.apply(interpreter, arguments, callLoc, cont);
        }
        return interpreter.deliver(invoker.apply(arguments, callLoc), cont);
    }
}
