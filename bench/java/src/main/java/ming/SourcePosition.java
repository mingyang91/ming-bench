package ming;

record SourcePosition(int line, int column) {
    @Override
    public String toString() {
        return line + ":" + column;
    }
}
