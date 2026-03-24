package ming;

import java.util.List;

sealed interface Value permits Value.IntegerValue, Value.BooleanValue, Value.StringValue, Value.SymbolValue, Value.ListValue, Value.VoidValue, Value.BuiltinProcedure, Value.ClosureValue {
    String render();

    default boolean isTruthy() {
        return true;
    }

    private static String escapeString(String value) {
        StringBuilder builder = new StringBuilder(value.length() + 2);
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

    record IntegerValue(long value) implements Value {
        @Override
        public String render() {
            return Long.toString(value);
        }
    }

    record BooleanValue(boolean value) implements Value {
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
            return escapeString(value);
        }
    }

    record SymbolValue(String name) implements Value {
        @Override
        public String render() {
            return name;
        }
    }

    record ListValue(List<Value> elements) implements Value {
        public ListValue {
            elements = List.copyOf(elements);
        }

        @Override
        public String render() {
            if (elements.isEmpty()) {
                return "()";
            }

            StringBuilder builder = new StringBuilder();
            builder.append('(');
            for (int i = 0; i < elements.size(); i++) {
                if (i > 0) {
                    builder.append(' ');
                }
                builder.append(elements.get(i).render());
            }
            builder.append(')');
            return builder.toString();
        }
    }

    record VoidValue() implements Value {
        @Override
        public String render() {
            return "#<void>";
        }
    }

    record BuiltinProcedure(String name) implements Value {
        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }
    }

    record ClosureValue(List<String> parameters, List<Expr> body, Environment env) implements Value {
        public ClosureValue {
            parameters = List.copyOf(parameters);
            body = List.copyOf(body);
        }

        @Override
        public String render() {
            return "#<procedure>";
        }
    }
}
