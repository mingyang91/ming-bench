package ming;

import java.util.Arrays;

public class SchemeVector {
    final Object[] elements;

    public SchemeVector(int size, Object fill) {
        elements = new Object[size];
        Arrays.fill(elements, fill);
    }

    public SchemeVector(Object[] elems) {
        elements = elems;
    }

    public Object ref(int idx) throws EvalError {
        if (idx < 0 || idx >= elements.length) throw new EvalError("vector-ref: index out of range");
        return elements[idx];
    }

    public void set(int idx, Object val) throws EvalError {
        if (idx < 0 || idx >= elements.length) throw new EvalError("vector-set!: index out of range");
        elements[idx] = val;
    }

    public int length() {
        return elements.length;
    }
}
