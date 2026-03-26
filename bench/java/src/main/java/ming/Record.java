package ming;

import java.util.List;
import java.util.Map;
import java.util.LinkedHashMap;

public class Record {
    final RecordType type;
    final Object[] fields;

    Record(RecordType type, Object[] fields) {
        this.type = type;
        this.fields = fields;
    }

    static class RecordType {
        final String name;
        final List<String> fieldNames;

        RecordType(String name, List<String> fieldNames) {
            this.name = name;
            this.fieldNames = fieldNames;
        }
    }
}
