package ming;

import java.util.ArrayList;
import java.util.List;

sealed interface Value permits NumericValue, BoolValue, StringValue, SymbolValue, ListValue,
        PairValue, CharValue, VoidValue, ProcedureValue, RecordValue {
    String render();

    default boolean isTruthy() {
        return true;
    }
}

sealed interface NumericValue extends Value permits IntValue, RationalValue, InexactValue {
    boolean isExact();

    double toDouble();
}

record IntValue(long value) implements NumericValue {
    @Override
    public String render() {
        return Long.toString(value);
    }

    @Override
    public boolean isExact() {
        return true;
    }

    @Override
    public double toDouble() {
        return (double) value;
    }
}

record RationalValue(long numerator, long denominator) implements NumericValue {
    RationalValue {
        if (denominator == 0L) {
            throw new IllegalArgumentException("denominator must be non-zero");
        }
        if (denominator < 0L) {
            numerator = -numerator;
            denominator = -denominator;
        }
    }

    @Override
    public String render() {
        if (denominator == 1L) {
            return Long.toString(numerator);
        }
        return numerator + "/" + denominator;
    }

    @Override
    public boolean isExact() {
        return true;
    }

    @Override
    public double toDouble() {
        return (double) numerator / (double) denominator;
    }
}

record InexactValue(double value) implements NumericValue {
    @Override
    public String render() {
        return Double.toString(value);
    }

    @Override
    public boolean isExact() {
        return false;
    }

    @Override
    public double toDouble() {
        return value;
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

final class StringValue implements Value {
    private final StringBuilder value;

    StringValue(String value) {
        this.value = new StringBuilder(value);
    }

    String value() {
        return value.toString();
    }

    int length() {
        return value.length();
    }

    char charAt(int index) {
        return value.charAt(index);
    }

    void setCharAt(int index, char ch) {
        value.setCharAt(index, ch);
    }

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

record PairValue(Value car, Value cdr) implements Value {
    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        appendRender(builder, this);
        builder.append(')');
        return builder.toString();
    }

    private static void appendRender(StringBuilder builder, Value value) {
        if (value instanceof PairValue pairValue) {
            builder.append(pairValue.car().render());
            if (pairValue.cdr() instanceof ListValue listValue) {
                for (Value element : listValue.elements()) {
                    builder.append(' ');
                    builder.append(element.render());
                }
                return;
            }
            if (pairValue.cdr() instanceof PairValue nextPair) {
                builder.append(' ');
                appendRender(builder, nextPair);
                return;
            }
            builder.append(" . ");
            builder.append(pairValue.cdr().render());
            return;
        }

        builder.append(value.render());
    }
}

record RecordTypeDescriptor(String name, int fieldCount) {
    RecordTypeDescriptor {
        if (fieldCount < 0) {
            throw new IllegalArgumentException("field count must be non-negative");
        }
    }
}

final class RecordValue implements Value {
    private final RecordTypeDescriptor type;
    private final List<Value> fields;

    RecordValue(RecordTypeDescriptor type, List<Value> fields) {
        if (type.fieldCount() != fields.size()) {
            throw new IllegalArgumentException("field count does not match record type");
        }
        this.type = type;
        this.fields = new ArrayList<>(fields);
    }

    RecordTypeDescriptor type() {
        return type;
    }

    Value field(int index) {
        return fields.get(index);
    }

    void setField(int index, Value value) {
        fields.set(index, value);
    }

    @Override
    public String render() {
        return "#<record:" + type.name() + ">";
    }
}

enum VoidValue implements Value {
    INSTANCE;

    @Override
    public String render() {
        return "#<void>";
    }
}

sealed interface ProcedureValue extends Value
        permits PrimitiveProcedureValue, LambdaProcedureValue, CaseLambdaProcedureValue {
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
    private final String restParameter;
    private final List<Expr> body;
    private final Environment definingEnvironment;

    LambdaProcedureValue(String name,
                         List<String> parameters,
                         String restParameter,
                         List<Expr> body,
                         Environment definingEnvironment) {
        this.name = name;
        this.parameters = List.copyOf(parameters);
        this.restParameter = restParameter;
        this.body = List.copyOf(body);
        this.definingEnvironment = definingEnvironment;
    }

    @Override
    public Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError {
        if (restParameter == null && arguments.size() != parameters.size()) {
            String procedureName = name == null ? "lambda" : name;
            throw new EvalError(procedureName + " expected " + parameters.size() + " argument(s)");
        }
        if (restParameter != null && arguments.size() < parameters.size()) {
            String procedureName = name == null ? "lambda" : name;
            throw new EvalError(procedureName + " expected at least " + parameters.size() + " argument(s)");
        }

        Environment callEnvironment = new Environment(definingEnvironment);
        for (int i = 0; i < parameters.size(); i++) {
            callEnvironment.define(parameters.get(i), arguments.get(i));
        }
        if (restParameter != null) {
            callEnvironment.define(
                    restParameter,
                    new ListValue(List.copyOf(arguments.subList(parameters.size(), arguments.size()))));
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

record CaseLambdaClause(ParameterSpec parameters, List<Expr> body) {
    CaseLambdaClause {
        body = List.copyOf(body);
    }
}

final class CaseLambdaProcedureValue implements ProcedureValue {
    private final List<CaseLambdaClause> clauses;
    private final Environment definingEnvironment;

    CaseLambdaProcedureValue(List<CaseLambdaClause> clauses, Environment definingEnvironment) {
        this.clauses = List.copyOf(clauses);
        this.definingEnvironment = definingEnvironment;
    }

    @Override
    public Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError {
        for (CaseLambdaClause clause : clauses) {
            ParameterSpec parameters = clause.parameters();
            if (!parameters.matchesArity(arguments.size())) {
                continue;
            }

            Environment callEnvironment = new Environment(definingEnvironment);
            for (int i = 0; i < parameters.fixedParameters().size(); i++) {
                callEnvironment.define(parameters.fixedParameters().get(i), arguments.get(i));
            }
            if (parameters.restParameter() != null) {
                callEnvironment.define(
                        parameters.restParameter(),
                        new ListValue(List.copyOf(arguments.subList(
                                parameters.fixedParameters().size(),
                                arguments.size()))));
            }
            return evaluator.evalSequence(clause.body(), callEnvironment);
        }

        throw new EvalError(
                "case-lambda expected a matching clause for " + arguments.size() + " argument(s)");
    }
}
