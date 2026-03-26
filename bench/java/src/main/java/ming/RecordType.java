package ming;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class RecordType {
    private final String name;
    private final List<String> fieldNames;
    private final Map<String, Integer> fieldIndexes;

    RecordType(String name, List<String> fieldNames) {
        this.name = name;
        this.fieldNames = List.copyOf(fieldNames);
        this.fieldIndexes = new HashMap<>(fieldNames.size());
        for (int index = 0; index < fieldNames.size(); index++) {
            fieldIndexes.put(fieldNames.get(index), index);
        }
    }

    String name() {
        return name;
    }

    int fieldCount() {
        return fieldNames.size();
    }

    int requireFieldIndex(String fieldName) throws EvalError {
        Integer index = fieldIndexes.get(fieldName);
        if (index == null) {
            throw new EvalError("define-record-type: unknown field " + fieldName);
        }
        return index;
    }
}
