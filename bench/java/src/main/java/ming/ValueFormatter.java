package ming;

final class ValueFormatter {
    private ValueFormatter() {
    }

    static String format(Value value) {
        return format(value, false);
    }

    static String formatDisplay(Value value) {
        return format(value, true);
    }

    private static String format(Value value, boolean displayMode) {
        if (value instanceof NumericValue numericValue) {
            return SchemeNumber.fromValue(numericValue).format();
        }
        if (value instanceof BoolValue boolValue) {
            return boolValue.value() ? "#t" : "#f";
        }
        if (value instanceof StringValue stringValue) {
            if (displayMode) {
                return stringValue.value();
            }
            return "\"" + escapeString(stringValue.value()) + "\"";
        }
        if (value instanceof CharValue charValue) {
            if (displayMode) {
                return Character.toString(charValue.value());
            }
            return formatCharacter(charValue.value());
        }
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        if (value instanceof EmptyListValue) {
            return "()";
        }
        if (value instanceof PairValue pairValue) {
            return formatPair(pairValue, displayMode);
        }
        if (value instanceof RecordValue recordValue) {
            return "#<record " + recordValue.type().name() + ">";
        }
        if (value instanceof VoidValue) {
            return "#<void>";
        }
        if (value instanceof ProcedureValue) {
            return "#<procedure>";
        }
        throw new IllegalStateException("unsupported runtime value");
    }

    private static String formatPair(PairValue pairValue, boolean displayMode) {
        StringBuilder builder = new StringBuilder("(");
        Value current = pairValue;
        boolean first = true;
        while (current instanceof PairValue pair) {
            if (!first) {
                builder.append(' ');
            }
            builder.append(format(pair.car(), displayMode));
            current = pair.cdr();
            first = false;
        }

        if (current instanceof EmptyListValue) {
            builder.append(')');
        } else {
            builder.append(" . ").append(format(current, displayMode)).append(')');
        }
        return builder.toString();
    }

    private static String formatCharacter(char value) {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }

    private static String escapeString(String value) {
        StringBuilder builder = new StringBuilder(value.length());
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
        return builder.toString();
    }
}
