package ming;

public record SchemeSymbol(String name) {
    @Override
    public String toString() {
        return name;
    }
}
