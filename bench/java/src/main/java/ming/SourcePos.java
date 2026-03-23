package ming;

public record SourcePos(int line, int col) {
    public static final SourcePos NONE = new SourcePos(0, 0);

    @Override
    public String toString() {
        return line + ":" + col;
    }
}
