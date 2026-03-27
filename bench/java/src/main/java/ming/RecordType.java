package ming;

import java.util.List;

class RecordType {
    final String name;
    final List<String> fieldNames;
    RecordType(String name, List<String> fieldNames) {
        this.name = name;
        this.fieldNames = fieldNames;
    }
}
