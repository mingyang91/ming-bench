package ming;

class SchemeRecord {
    final RecordType type;
    final Object[] fields;
    SchemeRecord(RecordType type, Object[] fields) {
        this.type = type;
        this.fields = fields;
    }
}
