package ming;

public record SourcePos(int line, int column) {
    @Override
    public String toString() {
        return line + ":" + column;
    }
}
