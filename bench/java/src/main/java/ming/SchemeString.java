package ming;

class SchemeString {
    private char[] chars;
    SchemeString(String value) { this.chars = value.toCharArray(); }
    String value() { return new String(chars); }
    char charAt(int i) { return chars[i]; }
    int length() { return chars.length; }
    void setChar(int i, char c) { chars[i] = c; }
    SchemeString copy() { return new SchemeString(value()); }
    @Override public boolean equals(Object o) {
        return o instanceof SchemeString s && value().equals(s.value());
    }
    @Override public int hashCode() { return value().hashCode(); }
}
