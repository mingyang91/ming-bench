package ming;

import java.util.Map;

public class SchemeRecord {
    public final String typeName;
    public final Object[] fields;
    public final String[] fieldNames;

    public SchemeRecord(String typeName, String[] fieldNames, Object[] fields) {
        this.typeName = typeName;
        this.fieldNames = fieldNames;
        this.fields = fields;
    }

    public Object getField(String name) {
        for (int i = 0; i < fieldNames.length; i++) {
            if (fieldNames[i].equals(name)) return fields[i];
        }
        return null;
    }
}
