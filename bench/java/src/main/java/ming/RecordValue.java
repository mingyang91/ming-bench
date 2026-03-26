package ming;

import java.util.List;

final class RecordValue implements SchemeValue {
    private final RecordType type;
    private final List<SchemeValue> fields;

    RecordValue(RecordType type, List<SchemeValue> fields) {
        this.type = type;
        this.fields = List.copyOf(fields);
    }

    boolean hasType(RecordType expectedType) {
        return type == expectedType;
    }

    SchemeValue field(int index) {
        return fields.get(index);
    }

    @Override
    public String render() {
        return "#<record:" + type.name() + ">";
    }
}
