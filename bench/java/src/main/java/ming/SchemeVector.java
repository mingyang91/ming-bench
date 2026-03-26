package ming;

class SchemeVector {
    final Object[] data;

    SchemeVector(Object[] data) {
        this.data = data;
    }

    SchemeVector(int size, Object fill) {
        this.data = new Object[size];
        for (int i = 0; i < size; i++) data[i] = fill;
    }
}
