package ming;

class MutableString {
    final char[] chars;

    MutableString(char[] chars) {
        this.chars = chars;
    }

    String toSchemeStr() {
        return "\"" + new String(chars) + "\"";
    }

    String inner() {
        return new String(chars);
    }
}
