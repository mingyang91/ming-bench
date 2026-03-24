package ming;

public class SchemeString {
    private char[] chars;

    public SchemeString(String value) {
        this.chars = value.toCharArray();
    }

    public String value() {
        return new String(chars);
    }

    public char charAt(int index) {
        return chars[index];
    }

    public void setChar(int index, char c) {
        chars[index] = c;
    }

    public int length() {
        return chars.length;
    }
}
