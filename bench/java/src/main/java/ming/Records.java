package ming;

import java.util.List;

final class RecordType {
    private final String name;
    private final List<String> fieldNames;

    RecordType(String name, List<String> fieldNames) {
        this.name = name;
        this.fieldNames = List.copyOf(fieldNames);
    }

    String name() {
        return name;
    }

    int fieldCount() {
        return fieldNames.size();
    }
}

final class RecordValue implements Value {
    private final RecordType type;
    private final List<Value> fields;

    RecordValue(RecordType type, List<Value> fields) {
        if (fields.size() != type.fieldCount()) {
            throw new IllegalArgumentException("record field count mismatch");
        }
        this.type = type;
        this.fields = List.copyOf(fields);
    }

    RecordType type() {
        return type;
    }

    Value field(int index) {
        return fields.get(index);
    }

    @Override
    public String render() {
        return "#<record:" + type.name() + ">";
    }
}

abstract class RecordProcedure implements Value, Procedure {
    private final String name;

    RecordProcedure(String name) {
        this.name = name;
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }

    void ensureExactly(List<Value> arguments, int expected, SourceLoc callLoc) throws EvalError {
        if (arguments.size() != expected) {
            throw SchemeErrors.at(callLoc,
                    name + " expected " + expected + " arguments but got " + arguments.size());
        }
    }
}

final class RecordConstructorProcedure extends RecordProcedure {
    private final RecordType type;

    RecordConstructorProcedure(String name, RecordType type) {
        super(name);
        this.type = type;
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly(arguments, type.fieldCount(), callLoc);
        return new RecordValue(type, arguments);
    }
}

final class RecordPredicateProcedure extends RecordProcedure {
    private final RecordType type;

    RecordPredicateProcedure(String name, RecordType type) {
        super(name);
        this.type = type;
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly(arguments, 1, callLoc);
        return new BooleanValue(arguments.getFirst() instanceof RecordValue recordValue
                && recordValue.type() == type);
    }
}

final class RecordAccessorProcedure extends RecordProcedure {
    private final RecordType type;
    private final int fieldIndex;

    RecordAccessorProcedure(String name, RecordType type, int fieldIndex) {
        super(name);
        this.type = type;
        this.fieldIndex = fieldIndex;
    }

    @Override
    public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly(arguments, 1, callLoc);
        if (!(arguments.getFirst() instanceof RecordValue recordValue)
                || recordValue.type() != type) {
            throw SchemeErrors.at(callLoc, "record accessor expects a " + type.name() + " value");
        }
        return recordValue.field(fieldIndex);
    }
}
