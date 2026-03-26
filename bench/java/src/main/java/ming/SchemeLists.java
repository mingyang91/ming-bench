package ming;

import java.util.List;

final class SchemeLists {
    static final ListValue EMPTY = new ListValue(List.of());

    private SchemeLists() {
    }

    static boolean isEmpty(Value value) {
        return value instanceof ListValue listValue && listValue.isEmpty();
    }

    static Value fromElements(List<Value> elements) {
        Value list = EMPTY;
        for (int i = elements.size() - 1; i >= 0; i--) {
            list = new PairValue(elements.get(i), list);
        }
        return list;
    }
}
