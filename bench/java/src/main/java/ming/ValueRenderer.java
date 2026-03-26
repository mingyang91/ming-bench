package ming;

final class ValueRenderer {
    private ValueRenderer() {
    }

    static String render(Value value) {
        return renderValue(value, false);
    }

    static String renderDisplay(Value value) {
        return renderValue(value, true);
    }

    private static String renderValue(Value value, boolean displayMode) {
        return switch (value) {
            case IntValue intValue -> Long.toString(intValue.value());
            case RationalValue rationalValue ->
                    rationalValue.numerator() + "/" + rationalValue.denominator();
            case InexactValue inexactValue -> Double.toString(inexactValue.value());
            case BoolValue boolValue -> boolValue.value() ? "#t" : "#f";
            case StringValue stringValue ->
                    displayMode ? stringValue.text() : quote(stringValue.text());
            case SymbolValue symbolValue -> symbolValue.name();
            case EmptyListValue ignored -> "()";
            case CharValue charValue -> displayMode
                    ? Character.toString(charValue.value())
                    : renderChar(charValue.value());
            case PairValue pairValue -> renderPair(pairValue, displayMode);
            case BuiltinValue ignored -> "#<procedure>";
            case ClosureValue ignored -> "#<procedure>";
            case CaseLambdaValue ignored -> "#<procedure>";
            case RecordTypeValue recordTypeValue -> "#<record-type " + recordTypeValue.name() + ">";
            case RecordInstanceValue recordInstanceValue ->
                    "#<record " + recordInstanceValue.type().name() + ">";
            case VoidValue ignored -> "#<void>";
            case UninitializedValue ignored -> "#<uninitialized>";
        };
    }

    private static String renderPair(PairValue pairValue, boolean displayMode) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');

        Value current = pairValue;
        boolean first = true;
        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(renderValue(pair.car(), displayMode));
            current = pair.cdr();
            first = false;
        }

        if (!(current instanceof EmptyListValue)) {
            builder.append(" . ");
            builder.append(renderValue(current, displayMode));
        }

        builder.append(')');
        return builder.toString();
    }

    private static String renderChar(char value) {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }

    private static String quote(String value) {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
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
