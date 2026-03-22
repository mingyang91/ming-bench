package ming;

public record SourcePos(int line, int col) {
    @Override
    public String toString() {
        return line + ":" + col;
    }
}
