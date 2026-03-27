package ming;

public class SchemeVector {
    final Object[] elements;

    public SchemeVector(int size, Object fill) {
        elements = new Object[size];
        for (int i = 0; i < size; i++) elements[i] = fill;
    }

    public SchemeVector(Object[] elems) {
        elements = elems;
    }

    public int length() { return elements.length; }
    public Object ref(int i) { return elements[i]; }
    public void set(int i, Object val) { elements[i] = val; }
}
