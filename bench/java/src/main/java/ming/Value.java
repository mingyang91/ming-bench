package ming;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

sealed interface Value permits NumericValue, BoolValue, StringValue, SymbolValue, ListValue,
        PairValue, CharValue, VectorValue, VoidValue, ProcedureValue, RecordValue, UninitializedValue,
        CallCcProcedureValue, RaiseProcedureValue, WithExceptionHandlerProcedureValue, ContinuationProcedureValue {
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
    private final boolean mutable;

    StringValue(String value) {
        this(value, true);
    }

    StringValue(String value, boolean mutable) {
        this.value = new StringBuilder(value);
        this.mutable = mutable;
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

    boolean isMutable() {
        return mutable;
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
    ListValue {
        elements = List.copyOf(elements);
    }

    boolean isEmpty() {
        return elements.isEmpty();
    }

    @Override
    public String render() {
        return ValueRenderer.render(this);
    }
}

final class PairValue implements Value {
    private Value car;
    private Value cdr;

    PairValue(Value car, Value cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    Value car() {
        return car;
    }

    Value cdr() {
        return cdr;
    }

    void setCar(Value value) {
        car = value;
    }

    void setCdr(Value value) {
        cdr = value;
    }

    @Override
    public String render() {
        return ValueRenderer.render(this);
    }
}

final class ValueRenderer {
    private static final String CYCLE_MARKER = "#<cycle>";

    private ValueRenderer() {
    }

    static String render(Value value) {
        StringBuilder builder = new StringBuilder();
        appendValue(builder, value, new IdentityHashMap<>());
        return builder.toString();
    }

    private static void appendValue(StringBuilder builder,
                                    Value value,
                                    IdentityHashMap<PairValue, Boolean> activePairs) {
        if (value instanceof PairValue pairValue) {
            appendPair(builder, pairValue, activePairs);
            return;
        }
        if (value instanceof ListValue listValue) {
            appendList(builder, listValue, activePairs);
            return;
        }
        builder.append(value.render());
    }

    private static void appendList(StringBuilder builder,
                                   ListValue listValue,
                                   IdentityHashMap<PairValue, Boolean> activePairs) {
        if (listValue.isEmpty()) {
            builder.append("()");
            return;
        }

        builder.append('(');
        for (int i = 0; i < listValue.elements().size(); i++) {
            if (i > 0) {
                builder.append(' ');
            }
            appendValue(builder, listValue.elements().get(i), activePairs);
        }
        builder.append(')');
    }

    private static void appendPair(StringBuilder builder,
                                   PairValue pairValue,
                                   IdentityHashMap<PairValue, Boolean> activePairs) {
        if (activePairs.put(pairValue, Boolean.TRUE) != null) {
            builder.append(CYCLE_MARKER);
            return;
        }

        try {
            builder.append('(');
            appendPairContents(builder, pairValue, activePairs);
            builder.append(')');
        } finally {
            activePairs.remove(pairValue);
        }
    }

    private static void appendPairContents(StringBuilder builder,
                                           PairValue pairValue,
                                           IdentityHashMap<PairValue, Boolean> activePairs) {
        appendValue(builder, pairValue.car(), activePairs);
        appendPairTail(builder, pairValue.cdr(), activePairs);
    }

    private static void appendPairTail(StringBuilder builder,
                                       Value tail,
                                       IdentityHashMap<PairValue, Boolean> activePairs) {
        if (tail instanceof PairValue nextPair) {
            if (activePairs.containsKey(nextPair)) {
                builder.append(" . ");
                builder.append(CYCLE_MARKER);
                return;
            }

            builder.append(' ');
            activePairs.put(nextPair, Boolean.TRUE);
            try {
                appendPairContents(builder, nextPair, activePairs);
            } finally {
                activePairs.remove(nextPair);
            }
            return;
        }

        if (tail instanceof ListValue listValue) {
            if (listValue.isEmpty()) {
                return;
            }

            for (Value element : listValue.elements()) {
                builder.append(' ');
                appendValue(builder, element, activePairs);
            }
            return;
        }

        builder.append(" . ");
        appendValue(builder, tail, activePairs);
    }
}

final class VectorValue implements Value {
    private final List<Value> elements;

    VectorValue(List<Value> elements) {
        this.elements = new ArrayList<>(elements);
    }

    int length() {
        return elements.size();
    }

    Value element(int index) {
        return elements.get(index);
    }

    void setElement(int index, Value value) {
        elements.set(index, value);
    }

    List<Value> elements() {
        return List.copyOf(elements);
    }

    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append("#(");
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

enum UninitializedValue implements Value {
    INSTANCE;

    @Override
    public String render() {
        return "#<uninitialized>";
    }
}

sealed interface ProcedureValue extends Value
        permits PrimitiveProcedureValue, LambdaProcedureValue, CaseLambdaProcedureValue {
    Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError;

    default TailCall applyTail(List<Value> arguments, Evaluator evaluator) throws EvalError {
        return new TailCallValue(apply(arguments, evaluator));
    }

    @Override
    default String render() {
        return "#<procedure>";
    }
}

sealed interface TailCall permits TailCallValue, TailCallSequence {
}

record TailCallValue(Value value) implements TailCall {
}

record TailCallSequence(List<Expr> expressions, Environment environment) implements TailCall {
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

    Environment createCallEnvironment(List<Value> arguments) throws EvalError {
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
                    SchemeLists.fromElements(arguments.subList(parameters.size(), arguments.size())));
        }

        return callEnvironment;
    }

    @Override
    public Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError {
        return evaluator.evalTailSequence(body, createCallEnvironment(arguments));
    }

    @Override
    public TailCall applyTail(List<Value> arguments, Evaluator evaluator) throws EvalError {
        return new TailCallSequence(body, createCallEnvironment(arguments));
    }

    @Override
    public String render() {
        if (name == null) {
            return ProcedureValue.super.render();
        }
        return "#<procedure:" + name + ">";
    }

    List<Expr> bodyExpressions() {
        return body;
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

    private TailCall matchClause(List<Value> arguments) throws EvalError {
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
                        SchemeLists.fromElements(arguments.subList(
                                parameters.fixedParameters().size(),
                                arguments.size())));
            }
            return new TailCallSequence(clause.body(), callEnvironment);
        }

        throw new EvalError(
                "case-lambda expected a matching clause for " + arguments.size() + " argument(s)");
    }

    @Override
    public Value apply(List<Value> arguments, Evaluator evaluator) throws EvalError {
        TailCall tailCall = matchClause(arguments);
        if (tailCall instanceof TailCallValue tailCallValue) {
            return tailCallValue.value();
        }
        TailCallSequence tailCallSequence = (TailCallSequence) tailCall;
        return evaluator.evalTailSequence(tailCallSequence.expressions(), tailCallSequence.environment());
    }

    @Override
    public TailCall applyTail(List<Value> arguments, Evaluator evaluator) throws EvalError {
        return matchClause(arguments);
    }
}
