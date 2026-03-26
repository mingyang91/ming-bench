package ming;

import java.util.ArrayList;
import java.util.List;

interface RecordProcedureSupport {
    void requireArity(String name, int actual, int expected) throws EvalError;

    RecordValue expectRecord(Value value, RecordType recordType) throws EvalError;
}

record RecordConstructorSpec(String name, List<String> fieldNames) {
    RecordConstructorSpec {
        fieldNames = List.copyOf(fieldNames);
    }
}

record RecordFieldSpec(String fieldName, String accessorName, String mutatorName) {
}

final class RecordConstructorProcedure extends ProcedureValue {
    private final String name;
    private final RecordType recordType;
    private final List<Integer> fieldIndexes;
    private final RecordProcedureSupport support;

    RecordConstructorProcedure(String name, RecordType recordType, List<Integer> fieldIndexes,
                               RecordProcedureSupport support) {
        this.name = name;
        this.recordType = recordType;
        this.fieldIndexes = fieldIndexes;
        this.support = support;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        support.requireArity(name, args.size(), fieldIndexes.size());

        List<Value> fields = new ArrayList<>(recordType.fieldCount());
        for (int index = 0; index < recordType.fieldCount(); index++) {
            fields.add(VoidValue.INSTANCE);
        }
        for (int index = 0; index < fieldIndexes.size(); index++) {
            fields.set(fieldIndexes.get(index), args.get(index));
        }
        return new RecordValue(recordType, fields);
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class RecordPredicateProcedure extends ProcedureValue {
    private final String name;
    private final RecordType recordType;
    private final RecordProcedureSupport support;

    RecordPredicateProcedure(String name, RecordType recordType, RecordProcedureSupport support) {
        this.name = name;
        this.recordType = recordType;
        this.support = support;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        support.requireArity(name, args.size(), 1);
        return BoolValue.of(args.getFirst() instanceof RecordValue recordValue
                && recordValue.type() == recordType);
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class RecordAccessorProcedure extends ProcedureValue {
    private final String name;
    private final RecordType recordType;
    private final int fieldIndex;
    private final RecordProcedureSupport support;

    RecordAccessorProcedure(String name, RecordType recordType, int fieldIndex,
                            RecordProcedureSupport support) {
        this.name = name;
        this.recordType = recordType;
        this.fieldIndex = fieldIndex;
        this.support = support;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        support.requireArity(name, args.size(), 1);
        return support.expectRecord(args.getFirst(), recordType).field(fieldIndex);
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}

final class RecordMutatorProcedure extends ProcedureValue {
    private final String name;
    private final RecordType recordType;
    private final int fieldIndex;
    private final RecordProcedureSupport support;

    RecordMutatorProcedure(String name, RecordType recordType, int fieldIndex,
                           RecordProcedureSupport support) {
        this.name = name;
        this.recordType = recordType;
        this.fieldIndex = fieldIndex;
        this.support = support;
    }

    @Override
    Value apply(List<Value> args) throws EvalError {
        support.requireArity(name, args.size(), 2);
        RecordValue recordValue = support.expectRecord(args.get(0), recordType);
        recordValue.setField(fieldIndex, args.get(1));
        return VoidValue.INSTANCE;
    }

    @Override
    public String render() {
        return "#<procedure:" + name + ">";
    }
}
