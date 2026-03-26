package ming;

public final class SchemeNil {
    public static final SchemeNil INSTANCE = new SchemeNil();
    private SchemeNil() {}
    @Override public String toString() { return "()"; }
}
