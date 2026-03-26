package ming;

sealed interface Value permits IntValue, BoolValue, StringValue {
    String render();

    default boolean isTruthy() {
        return true;
    }
}

record IntValue(long value) implements Value {
    @Override
    public String render() {
        return Long.toString(value);
    }
}

record BoolValue(boolean value) implements Value {
    @Override
    public String render() {
        return value ? "#t" : "#f";
    }

    @Override
    public boolean isTruthy() {
        return value;
    }
}

record StringValue(String value) implements Value {
    @Override
    public String render() {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            switch (ch) {
                case '\\' -> builder.append("\\\\");
                case '"' -> builder.append("\\\"");
                case '\n' -> builder.append("\\n");
                case '\r' -> builder.append("\\r");
                case '\t' -> builder.append("\\t");
                default -> builder.append(ch);
            }
        }
        builder.append('"');
        return builder.toString();
    }
}
