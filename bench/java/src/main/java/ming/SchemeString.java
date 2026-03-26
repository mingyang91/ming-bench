package ming;

/**
 * Mutable string type for Scheme's string-copy / string-set! support.
 */
public class SchemeString {
    private final char[] data;

    public SchemeString(String s) {
        this.data = s.toCharArray();
    }

    public SchemeString(char[] data) {
        this.data = data.clone();
    }

    public int length() {
        return data.length;
    }

    public char charAt(int idx) {
        return data[idx];
    }

    public void setCharAt(int idx, char c) {
        data[idx] = c;
    }

    public String value() {
        return new String(data);
    }

    public String substring(int start, int end) {
        return new String(data, start, end - start);
    }

    @Override
    public String toString() {
        return "\"" + new String(data) + "\"";
    }
}
