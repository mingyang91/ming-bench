package ming;

public class SchemeString {
    private char[] chars;
    private boolean immutable;

    public SchemeString(String value) {
        this.chars = value.toCharArray();
        this.immutable = false;
    }

    public SchemeString(String value, boolean immutable) {
        this.chars = value.toCharArray();
        this.immutable = immutable;
    }

    public String value() {
        return new String(chars);
    }

    public char charAt(int index) {
        return chars[index];
    }

    public boolean isImmutable() {
        return immutable;
    }

    public void setChar(int index, char c) {
        chars[index] = c;
    }

    public int length() {
        return chars.length;
    }
}
