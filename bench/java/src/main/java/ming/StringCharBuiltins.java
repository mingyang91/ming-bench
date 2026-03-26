package ming;

import java.util.List;

import static ming.SchemeFormatter.schemeToString;

/**
 * String and character builtin procedures, extracted from Evaluator.
 */
final class StringCharBuiltins {

    private StringCharBuiltins() {}

    static Object applyStringBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "string-append" -> {
                StringBuilder sb = new StringBuilder();
                for (Object arg : args) sb.append(requireString(arg));
                yield "\"" + sb + "\"";
            }
            case "string-length" -> (long) requireString(args.get(0)).length();
            case "substring" -> {
                String s = requireString(args.get(0));
                int start = (int) requireLong(args.get(1));
                int end = args.size() == 3 ? (int) requireLong(args.get(2)) : s.length();
                yield "\"" + s.substring(start, end) + "\"";
            }
            case "string-ref" -> new SchemeChar(requireString(args.get(0)).charAt((int) requireLong(args.get(1))));
            case "string->number" -> {
                try { yield Long.parseLong(requireString(args.get(0))); }
                catch (NumberFormatException e) { yield Boolean.FALSE; }
            }
            case "number->string" -> "\"" + schemeToString(args.get(0)) + "\"";
            case "symbol->string" -> {
                if (!(args.get(0) instanceof SchemeSymbol sym)) throw new EvalError("symbol->string: not a symbol");
                yield "\"" + sym.name() + "\"";
            }
            case "string->symbol" -> new SchemeSymbol(requireString(args.get(0)));
            case "string-copy" -> new SchemeString(requireString(args.get(0)));
            case "string-set!" -> {
                Object target = args.get(0);
                if (target instanceof SchemeString ss) {
                    int idx = (int) requireLong(args.get(1));
                    if (!(args.get(2) instanceof SchemeChar ch)) throw new EvalError("string-set!: expected char");
                    ss.setCharAt(idx, ch.value());
                    yield Evaluator.VOID;
                }
                throw new EvalError("string-set!: strings are immutable");
            }
            case "string->list" -> {
                String s = requireString(args.get(0));
                Object result = null;
                for (int i = s.length() - 1; i >= 0; i--) {
                    result = new SchemePair(new SchemeChar(s.charAt(i)), result == null ? SchemeNil.INSTANCE : result);
                }
                yield result == null ? SchemeNil.INSTANCE : result;
            }
            case "list->string" -> {
                StringBuilder sb = new StringBuilder();
                Object lst = args.get(0);
                while (lst instanceof SchemePair p) {
                    if (!(p.car instanceof SchemeChar ch)) throw new EvalError("list->string: expected char");
                    sb.append(ch.value());
                    lst = p.cdr;
                }
                yield "\"" + sb.toString() + "\"";
            }
            case "string=?" -> requireString(args.get(0)).equals(requireString(args.get(1)));
            case "string<?" -> requireString(args.get(0)).compareTo(requireString(args.get(1))) < 0;
            case "string-ci=?" -> requireString(args.get(0)).equalsIgnoreCase(requireString(args.get(1)));
            case "string-upcase" -> "\"" + requireString(args.get(0)).toUpperCase() + "\"";
            case "string-downcase" -> "\"" + requireString(args.get(0)).toLowerCase() + "\"";
            case "make-string" -> {
                int len = (int) requireLong(args.get(0));
                char ch = args.size() > 1 && args.get(1) instanceof SchemeChar sc ? sc.value() : ' ';
                StringBuilder sb = new StringBuilder(len);
                for (int i = 0; i < len; i++) sb.append(ch);
                yield new SchemeString(sb.toString());
            }
            case "string" -> {
                StringBuilder sb = new StringBuilder();
                for (Object arg : args) {
                    if (!(arg instanceof SchemeChar ch)) throw new EvalError("string: expected char");
                    sb.append(ch.value());
                }
                yield "\"" + sb + "\"";
            }
            case "string>?" -> requireString(args.get(0)).compareTo(requireString(args.get(1))) > 0;
            case "string<=?" -> requireString(args.get(0)).compareTo(requireString(args.get(1))) <= 0;
            case "string>=?" -> requireString(args.get(0)).compareTo(requireString(args.get(1))) >= 0;
            default -> throw new EvalError("unknown string procedure: " + name);
        };
    }

    static Object applyCharBuiltin(String name, List<Object> args) throws EvalError {
        return switch (name) {
            case "char-alphabetic?" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-alphabetic?: expected char");
                yield Character.isLetter(ch.value());
            }
            case "char-numeric?" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-numeric?: expected char");
                yield Character.isDigit(ch.value());
            }
            case "char=?" -> {
                if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                    throw new EvalError("char=?: expected chars");
                yield a.value() == b.value();
            }
            case "char<?" -> {
                if (!(args.get(0) instanceof SchemeChar a) || !(args.get(1) instanceof SchemeChar b))
                    throw new EvalError("char<?: expected chars");
                yield a.value() < b.value();
            }
            case "char-upcase" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-upcase: expected char");
                yield new SchemeChar(Character.toUpperCase(ch.value()));
            }
            case "char-downcase" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char-downcase: expected char");
                yield new SchemeChar(Character.toLowerCase(ch.value()));
            }
            case "char->integer" -> {
                if (!(args.get(0) instanceof SchemeChar ch)) throw new EvalError("char->integer: expected char");
                yield (long) ch.value();
            }
            case "integer->char" -> {
                long n = requireLong(args.get(0));
                yield new SchemeChar((char) n);
            }
            default -> throw new EvalError("unknown char procedure: " + name);
        };
    }

    static long requireLong(Object val) throws EvalError {
        if (val instanceof Long l) return l;
        if (val instanceof Double d) return d.longValue();
        throw new EvalError("expected integer, got: " + schemeToString(val));
    }

    static String requireString(Object val) throws EvalError {
        if (val instanceof String s) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                return s.substring(1, s.length() - 1);
            }
            return s;
        }
        if (val instanceof SchemeString ss) {
            return ss.value();
        }
        throw new EvalError("expected string, got: " + schemeToString(val));
    }
}
