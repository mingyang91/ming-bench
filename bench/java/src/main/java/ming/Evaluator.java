package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    // ── Value types ──────────────────────────────────────────────
    private sealed interface Val permits Val.Int, Val.Bool, Val.Str, Val.Sym, Val.PairV, Val.Nil, Val.Void, Val.Builtin {
        record Int(long value) implements Val {}
        record Bool(boolean value) implements Val {}
        record Str(String value) implements Val {}
        record Sym(String name) implements Val {}
        record PairV(Val car, Val cdr) implements Val {}
        record Nil() implements Val {}
        record Void() implements Val {}
        record Builtin(String name, java.util.function.Function<List<Val>, Val> fn) implements Val {}
    }

    // ── Display ──────────────────────────────────────────────────
    private static String display(Val v) {
        return switch (v) {
            case Val.Int i -> String.valueOf(i.value());
            case Val.Bool b -> b.value() ? "#t" : "#f";
            case Val.Str s -> "\"" + s.value() + "\"";
            case Val.Sym s -> s.name();
            case Val.Nil ignored -> "()";
            case Val.Void ignored -> "#<void>";
            case Val.PairV p -> displayPair(p);
            case Val.Builtin b -> "#<procedure:" + b.name() + ">";
        };
    }

    private static String displayPair(Val.PairV p) {
        StringBuilder sb = new StringBuilder("(");
        Val cur = p;
        boolean first = true;
        while (cur instanceof Val.PairV pair) {
            if (!first) sb.append(' ');
            first = false;
            sb.append(display(pair.car()));
            cur = pair.cdr();
        }
        if (!(cur instanceof Val.Nil)) {
            sb.append(" . ").append(display(cur));
        }
        sb.append(')');
        return sb.toString();
    }

    private static boolean isTruthy(Val v) {
        return !(v instanceof Val.Bool b && !b.value());
    }

    // ── Tokenizer ────────────────────────────────────────────────
    private static List<String> tokenize(String input) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        int len = input.length();
        while (i < len) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) { i++; continue; }
            if (c == ';') { while (i < len && input.charAt(i) != '\n') i++; continue; }
            if (c == '(') { tokens.add("("); i++; continue; }
            if (c == ')') { tokens.add(")"); i++; continue; }
            if (c == '\'') { tokens.add("'"); i++; continue; }
            if (c == '"') {
                StringBuilder sb = new StringBuilder("\"");
                i++;
                while (i < len && input.charAt(i) != '"') {
                    if (input.charAt(i) == '\\') { sb.append(input.charAt(i++)); }
                    sb.append(input.charAt(i++));
                }
                sb.append('"');
                if (i < len) i++; // skip closing "
                tokens.add(sb.toString());
                continue;
            }
            // atom
            StringBuilder sb = new StringBuilder();
            while (i < len && !Character.isWhitespace(input.charAt(i))
                    && input.charAt(i) != '(' && input.charAt(i) != ')'
                    && input.charAt(i) != '"' && input.charAt(i) != ';') {
                sb.append(input.charAt(i++));
            }
            tokens.add(sb.toString());
        }
        return tokens;
    }

    // ── Parser ───────────────────────────────────────────────────
    private static int[] parseIdx = {0}; // thread-local hack avoided; use instance

    private Val parse(List<String> tokens, int[] idx) throws EvalError {
        if (idx[0] >= tokens.size()) throw new EvalError("unexpected EOF");
        String tok = tokens.get(idx[0]++);
        if (tok.equals("(")) {
            List<Val> elems = new ArrayList<>();
            while (idx[0] < tokens.size() && !tokens.get(idx[0]).equals(")")) {
                elems.add(parse(tokens, idx));
            }
            if (idx[0] >= tokens.size()) throw new EvalError("missing )");
            idx[0]++; // skip )
            // build list
            Val list = new Val.Nil();
            for (int i = elems.size() - 1; i >= 0; i--) {
                list = new Val.PairV(elems.get(i), list);
            }
            return list;
        }
        if (tok.equals(")")) throw new EvalError("unexpected )");
        if (tok.equals("'")) {
            Val quoted = parse(tokens, idx);
            return new Val.PairV(new Val.Sym("quote"), new Val.PairV(quoted, new Val.Nil()));
        }
        return parseAtom(tok);
    }

    private Val parseAtom(String tok) {
        if (tok.equals("#t")) return new Val.Bool(true);
        if (tok.equals("#f")) return new Val.Bool(false);
        if (tok.startsWith("\"")) return new Val.Str(tok.substring(1, tok.length() - 1));
        try {
            return new Val.Int(Long.parseLong(tok));
        } catch (NumberFormatException e) {
            return new Val.Sym(tok);
        }
    }

    // ── Eval ─────────────────────────────────────────────────────
    private Val eval(Val expr) throws EvalError {
        return switch (expr) {
            case Val.Int i -> i;
            case Val.Bool b -> b;
            case Val.Str s -> s;
            case Val.Nil n -> n;
            case Val.Void v -> v;
            case Val.Builtin b -> b;
            case Val.Sym sym -> throw new EvalError("unbound variable: " + sym.name());
            case Val.PairV pair -> evalList(pair);
        };
    }

    private Val evalList(Val.PairV pair) throws EvalError {
        Val head = pair.car();

        // Special forms
        if (head instanceof Val.Sym sym) {
            switch (sym.name()) {
                case "and" -> { return evalAnd(pair.cdr()); }
                case "or" -> { return evalOr(pair.cdr()); }
            }
        }

        // Function call
        Val fn = eval(head);
        if (!(fn instanceof Val.Builtin builtin)) {
            throw new EvalError("not a procedure: " + display(fn));
        }
        List<Val> args = evalArgs(pair.cdr());
        return builtin.fn().apply(args);
    }

    private Val evalAnd(Val args) throws EvalError {
        Val result = new Val.Bool(true);
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = eval(p.car());
            if (!isTruthy(result)) return result;
            cur = p.cdr();
        }
        return result;
    }

    private Val evalOr(Val args) throws EvalError {
        Val result = new Val.Bool(false);
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = eval(p.car());
            if (isTruthy(result)) return result;
            cur = p.cdr();
        }
        return result;
    }

    private List<Val> evalArgs(Val args) throws EvalError {
        List<Val> result = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result.add(eval(p.car()));
            cur = p.cdr();
        }
        return result;
    }

    // ── Environment (just builtins for L1) ───────────────────────
    private Val lookupBuiltin(String name) {
        return switch (name) {
            case "+" -> new Val.Builtin("+", args -> {
                long sum = 0;
                for (Val a : args) sum += asInt(a);
                return new Val.Int(sum);
            });
            case "-" -> new Val.Builtin("-", args -> {
                if (args.isEmpty()) throw new RuntimeException("- requires at least 1 argument");
                if (args.size() == 1) return new Val.Int(-asInt(args.get(0)));
                long result = asInt(args.get(0));
                for (int i = 1; i < args.size(); i++) result -= asInt(args.get(i));
                return new Val.Int(result);
            });
            case "*" -> new Val.Builtin("*", args -> {
                long product = 1;
                for (Val a : args) product *= asInt(a);
                return new Val.Int(product);
            });
            case "/" -> new Val.Builtin("/", args -> {
                if (args.size() < 2) throw new RuntimeException("/ requires at least 2 arguments");
                long result = asInt(args.get(0));
                for (int i = 1; i < args.size(); i++) {
                    long divisor = asInt(args.get(i));
                    if (divisor == 0) throw new RuntimeException("division by zero");
                    result /= divisor;
                }
                return new Val.Int(result);
            });
            case "<" -> new Val.Builtin("<", args -> {
                checkArgCount(args, 2, "<");
                return new Val.Bool(asInt(args.get(0)) < asInt(args.get(1)));
            });
            case ">" -> new Val.Builtin(">", args -> {
                checkArgCount(args, 2, ">");
                return new Val.Bool(asInt(args.get(0)) > asInt(args.get(1)));
            });
            case "=" -> new Val.Builtin("=", args -> {
                checkArgCount(args, 2, "=");
                return new Val.Bool(asInt(args.get(0)) == asInt(args.get(1)));
            });
            case "<=" -> new Val.Builtin("<=", args -> {
                checkArgCount(args, 2, "<=");
                return new Val.Bool(asInt(args.get(0)) <= asInt(args.get(1)));
            });
            case ">=" -> new Val.Builtin(">=", args -> {
                checkArgCount(args, 2, ">=");
                return new Val.Bool(asInt(args.get(0)) >= asInt(args.get(1)));
            });
            case "not" -> new Val.Builtin("not", args -> {
                checkArgCount(args, 1, "not");
                return new Val.Bool(!isTruthy(args.get(0)));
            });
            default -> null;
        };
    }

    private static long asInt(Val v) {
        if (v instanceof Val.Int i) return i.value();
        throw new RuntimeException("expected integer, got: " + display(v));
    }

    private static void checkArgCount(List<Val> args, int expected, String name) {
        if (args.size() != expected)
            throw new RuntimeException(name + " requires " + expected + " arguments, got " + args.size());
    }

    // ── Override eval for symbols to check builtins ──────────────
    private Val evalWithEnv(Val expr) throws EvalError {
        if (expr instanceof Val.Sym sym) {
            Val b = lookupBuiltin(sym.name());
            if (b != null) return b;
            throw new EvalError("unbound variable: " + sym.name());
        }
        if (expr instanceof Val.PairV pair) {
            Val head = pair.car();

            // Special forms
            if (head instanceof Val.Sym sym) {
                switch (sym.name()) {
                    case "and" -> { return evalAndEnv(pair.cdr()); }
                    case "or" -> { return evalOrEnv(pair.cdr()); }
                }
            }

            Val fn = evalWithEnv(head);
            if (!(fn instanceof Val.Builtin builtin)) {
                throw new EvalError("not a procedure: " + display(fn));
            }
            List<Val> args = evalArgsEnv(pair.cdr());
            try {
                return builtin.fn().apply(args);
            } catch (RuntimeException e) {
                throw new EvalError(e.getMessage());
            }
        }
        return eval(expr);
    }

    private Val evalAndEnv(Val args) throws EvalError {
        Val result = new Val.Bool(true);
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = evalWithEnv(p.car());
            if (!isTruthy(result)) return result;
            cur = p.cdr();
        }
        return result;
    }

    private Val evalOrEnv(Val args) throws EvalError {
        Val result = new Val.Bool(false);
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result = evalWithEnv(p.car());
            if (isTruthy(result)) return result;
            cur = p.cdr();
        }
        return result;
    }

    private List<Val> evalArgsEnv(Val args) throws EvalError {
        List<Val> result = new ArrayList<>();
        Val cur = args;
        while (cur instanceof Val.PairV p) {
            result.add(evalWithEnv(p.car()));
            cur = p.cdr();
        }
        return result;
    }

    // ── Public API ───────────────────────────────────────────────
    public String evalStr(String input) throws EvalError {
        List<String> tokens = tokenize(input);
        if (tokens.isEmpty()) throw new EvalError("empty input");
        int[] idx = {0};
        Val result = null;
        while (idx[0] < tokens.size()) {
            result = parse(tokens, idx);
            result = evalWithEnv(result);
        }
        if (result instanceof Val.Void) return "#<void>";
        return display(result);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        String result = evalStr(input);
        return new EvalResult(result, "");
    }
}
