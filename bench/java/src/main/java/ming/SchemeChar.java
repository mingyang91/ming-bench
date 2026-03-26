package ming;

public record SchemeChar(char value) {
    @Override
    public String toString() {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            case '\t' -> "#\\tab";
            default -> "#\\" + value;
        };
    }
}
