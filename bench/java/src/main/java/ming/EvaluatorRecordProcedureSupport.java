package ming;

final class EvaluatorRecordProcedureSupport implements RecordProcedureSupport {
    private final Evaluator evaluator;

    EvaluatorRecordProcedureSupport(Evaluator evaluator) {
        this.evaluator = evaluator;
    }

    @Override
    public void requireArity(String name, int actual, int expected) throws EvalError {
        evaluator.requireArity(name, actual, expected);
    }

    @Override
    public RecordValue expectRecord(Value value, RecordType recordType) throws EvalError {
        return evaluator.expectRecord(value, recordType);
    }
}
