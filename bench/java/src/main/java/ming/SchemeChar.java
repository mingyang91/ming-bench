package ming;

public record SchemeChar(char value) {
    @Override
    public String toString() {
        return "#\\" + value;
    }
}
