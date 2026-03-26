package ming;

import java.util.List;

sealed interface Value permits IntValue, BoolValue, StringValue, SymbolValue, ListValue,
        CharValue, VoidValue, ProcedureValue {
    String render();

    default boolean isTruthy() {
        return true;
    }
}

record IntValue(long value) implements Value {
    @Override
    public String render() {
        return Long.toString(value);
    }
}

record BoolValue(boolean value) implements Value {
    @Override
    public String render() {
        return value ? "#t" : "#f";
    }

    @Override
    public boolean isTruthy() {
        return value;
    }
}

record StringValue(String value) implements Value {
    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\r' -> builder.append("\\r");
                case '\t' -> builder.append("\\t");
                default -> builder.append(ch);
            }
        }
        builder.append('"');
        return builder.toString();
    }
}

record SymbolValue(String name) implements Value {
    @Override
    public String render() {
        return name;
    }
}

record CharValue(char value) implements Value {
    @Override
    public String render() {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }
}

record ListValue(List<Value> elements) implements Value {
    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        for (int i = 0; i < elements.size(); i++) {
            if (i > 0) {
                builder.append(' ');
            }
            builder.append(elements.get(i).render());
        }
        builder.append(')');
        return builder.toString();
    }
}

enum VoidValue implements Value {
    INSTANCE;

    @Override
    public String render() {
        return "#<void>";
    }
}

sealed interface ProcedureValue extends Value permits PrimitiveProcedureValue, LambdaProcedureValue {
    Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError;

    @Override
    default String render() {
        return "#<procedure>";
    }
}

@FunctionalInterface
interface PrimitiveImplementation {
    Value apply(List<Value> arguments) throws EvalError;
}

record PrimitiveProcedureValue(String name, PrimitiveImplementation implementation)
        implements ProcedureValue {
    @Override
    public Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError {
        return implementation.apply(arguments);
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class LambdaProcedureValue implements ProcedureValue {
    private final String name;
    private final List<String> parameters;
    private final List<Expr> body;
    private final Environment definingEnvironment;

    LambdaProcedureValue(String name,
                         List<String> parameters,
                         List<Expr> body,
                         Environment definingEnvironment) {
        this.name = name;
        this.parameters = List.copyOf(parameters);
        this.body = List.copyOf(body);
        this.definingEnvironment = definingEnvironment;
    }

    @Override
    public Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError {
        if (arguments.size() != parameters.size()) {
            String procedureName = name == null ? "lambda" : name;
            throw new EvalError(procedureName + " expected " + parameters.size() + " argument(s)");
        }

        Environment callEnvironment = new Environment(definingEnvironment);
        for (int i = 0; i < parameters.size(); i++) {
            callEnvironment.define(parameters.get(i), arguments.get(i));
        }

        return evaluator.evalSequence(body, callEnvironment);
    }

    @Override
    public String render() {
        if (name == null) {
            return ProcedureValue.super.render();
        }
        return "#<procedure:" + name + ">";
    }
}
