package ming;

final class RuntimeConstants {
    static final BoolValue TRUE = new BoolValue(true);
    static final BoolValue FALSE = new BoolValue(false);
    static final EmptyListValue EMPTY_LIST = new EmptyListValue();
    static final VoidValue VOID = new VoidValue();

    private RuntimeConstants() {
    }

    static BoolValue boolValue(boolean value) {
        return value ? TRUE : FALSE;
    }
}
