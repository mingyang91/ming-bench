package ming;

import java.util.ArrayList;
import java.util.List;

final class ContinuationValueSupport {
    Value pack(List<Value> values) {
        if (values.size() == 1) {
            return values.getFirst();
        }
        return new MultiValueValue(values);
    }

    Value requireSingle(List<Value> values) throws EvalError {
        if (values.size() != 1) {
            throw new EvalError("expected single value, got " + values.size());
        }
        return values.getFirst();
    }

    List<Value> append(List<Value> values, Value value) {
        List<Value> next = new ArrayList<>(values.size() + 1);
        next.addAll(values);
        next.add(value);
        return List.copyOf(next);
    }

    List<Value> prepend(Value value, List<Value> values) {
        List<Value> next = new ArrayList<>(values.size() + 1);
        next.add(value);
        next.addAll(values);
        return List.copyOf(next);
    }
}
