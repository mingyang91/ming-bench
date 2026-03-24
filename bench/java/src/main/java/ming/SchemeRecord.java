package ming;

import java.util.Map;

public class SchemeRecord {
    private final String typeName;
    private final Map<String, Object> fields;

    public SchemeRecord(String typeName, Map<String, Object> fields) {
        this.typeName = typeName;
        this.fields = fields;
    }

    public String typeName() { return typeName; }

    public Object getField(String name) { return fields.get(name); }

    @Override
    public String toString() { return "#<record:" + typeName + ">"; }
}
