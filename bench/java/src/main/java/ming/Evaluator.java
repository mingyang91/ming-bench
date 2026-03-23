package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

public class Evaluator {
    private final StringBuilder outputBuffer = new StringBuilder();
    private int gensymCounter = 0;

    // exception handler stack
    private final List<SchemeValue> handlerStack = new ArrayList<>();

    // dynamic-wind support
    static class WindRecord {
        final SchemeValue inThunk;
        final SchemeValue outThunk;
        WindRecord(SchemeValue inThunk, SchemeValue outThunk) {
            this.inThunk = inThunk;
            this.outThunk = outThunk;
        }
    }
    private final List<WindRecord> windStack = new ArrayList<>();

    private String gensym(String prefix) {
        return prefix + "__" + (gensymCounter++);
    }

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "quote", "set!", "define", "lambda", "if", "begin", "cond", "and", "or",
        "let", "letrec", "letrec*", "case", "do", "define-syntax", "syntax-rules", "guard"
    );

    public String evalStr(String input) throws EvalError {
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = trampoline(evalProgram(exprs, 0, env));
        if (result instanceof SchemeValue.VoidVal) return "#<void>";
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = trampoline(evalProgram(exprs, 0, env));
        String resultStr = (result instanceof SchemeValue.VoidVal) ? "#<void>" : result.display();
        return new EvalResult(resultStr, outputBuffer.toString());
    }

    private Bounce evalProgram(List<SchemeValue> exprs, int idx, Environment env) {
        if (idx >= exprs.size()) return new Bounce.Done(new SchemeValue.VoidVal());
        if (idx == exprs.size() - 1) {
            return new Bounce.More(() -> eval(exprs.get(idx), env, v -> new Bounce.Done(v)));
        }
        return new Bounce.More(() -> eval(exprs.get(idx), env, ignored ->
            evalProgram(exprs, idx + 1, env)
        ));
    }

    private SchemeValue trampoline(Bounce bounce) throws EvalError {
        while (true) {
            switch (bounce) {
                case Bounce.Done d -> { return d.value(); }
                case Bounce.Err e -> { throw e.error(); }
                case Bounce.More m -> { bounce = m.thunk().get(); }
            }
        }
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment();
        registerBuiltins(env);
        return env;
    }

    private void registerBuiltins(Environment env) {
        // Arithmetic
        env.define("+", new SchemeValue.BuiltinVal("+", args -> {
            long sum = 0;
            for (SchemeValue arg : args) sum += requireInt(arg);
            return new SchemeValue.IntVal(sum);
        }));
        env.define("-", new SchemeValue.BuiltinVal("-", args -> {
            if (args.isEmpty()) throw new EvalError("- requires at least 1 argument");
            if (args.size() == 1) return new SchemeValue.IntVal(-requireInt(args.getFirst()));
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) result -= requireInt(args.get(i));
            return new SchemeValue.IntVal(result);
        }));
        env.define("*", new SchemeValue.BuiltinVal("*", args -> {
            long product = 1;
            for (SchemeValue arg : args) product *= requireInt(arg);
            return new SchemeValue.IntVal(product);
        }));
        env.define("/", new SchemeValue.BuiltinVal("/", args -> {
            if (args.size() < 2) throw new EvalError("/ requires at least 2 arguments");
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) {
                long divisor = requireInt(args.get(i));
                if (divisor == 0) throw new EvalError("Division by zero");
                result /= divisor;
            }
            return new SchemeValue.IntVal(result);
        }));

        env.define("modulo", new SchemeValue.BuiltinVal("modulo", args -> {
            if (args.size() != 2) throw new EvalError("modulo requires 2 arguments");
            long a = requireInt(args.get(0));
            long b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("Division by zero");
            return new SchemeValue.IntVal(Math.floorMod(a, b));
        }));
        env.define("remainder", new SchemeValue.BuiltinVal("remainder", args -> {
            if (args.size() != 2) throw new EvalError("remainder requires 2 arguments");
            long a = requireInt(args.get(0));
            long b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("Division by zero");
            return new SchemeValue.IntVal(a % b);
        }));
        env.define("abs", new SchemeValue.BuiltinVal("abs", args -> {
            if (args.size() != 1) throw new EvalError("abs requires 1 argument");
            return new SchemeValue.IntVal(Math.abs(requireInt(args.getFirst())));
        }));

        // Comparisons
        env.define("<", new SchemeValue.BuiltinVal("<", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) < requireInt(args.get(1)))));
        env.define(">", new SchemeValue.BuiltinVal(">", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) > requireInt(args.get(1)))));
        env.define("=", new SchemeValue.BuiltinVal("=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) == requireInt(args.get(1)))));
        env.define("<=", new SchemeValue.BuiltinVal("<=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) <= requireInt(args.get(1)))));
        env.define(">=", new SchemeValue.BuiltinVal(">=", args ->
            new SchemeValue.BoolVal(requireInt(args.get(0)) >= requireInt(args.get(1)))));
        env.define("not", new SchemeValue.BuiltinVal("not", args ->
            new SchemeValue.BoolVal(!args.getFirst().isTruthy())));

        // Pairs and lists
        env.define("cons", new SchemeValue.BuiltinVal("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons requires exactly 2 arguments");
            return new SchemeValue.PairVal(args.get(0), args.get(1));
        }));
        env.define("car", new SchemeValue.BuiltinVal("car", args -> {
            if (args.size() != 1) throw new EvalError("car requires exactly 1 argument");
            if (args.getFirst() instanceof SchemeValue.PairVal p) return p.car();
            throw new EvalError("car: not a pair: " + args.getFirst().display());
        }));
        env.define("cdr", new SchemeValue.BuiltinVal("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr requires exactly 1 argument");
            if (args.getFirst() instanceof SchemeValue.PairVal p) return p.cdr();
            throw new EvalError("cdr: not a pair: " + args.getFirst().display());
        }));
        env.define("null?", new SchemeValue.BuiltinVal("null?", args -> {
            if (args.size() != 1) throw new EvalError("null? requires exactly 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.NilVal);
        }));
        env.define("list", new SchemeValue.BuiltinVal("list", args -> {
            SchemeValue result = SchemeValue.NIL;
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args.get(i), result);
            }
            return result;
        }));
        env.define("length", new SchemeValue.BuiltinVal("length", args -> {
            if (args.size() != 1) throw new EvalError("length requires exactly 1 argument");
            SchemeValue lst = args.getFirst();
            long count = 0;
            while (lst instanceof SchemeValue.PairVal p) {
                count++;
                lst = p.cdr();
            }
            return new SchemeValue.IntVal(count);
        }));
        env.define("append", new SchemeValue.BuiltinVal("append", args -> {
            if (args.isEmpty()) return SchemeValue.NIL;
            if (args.size() == 1) return args.getFirst();
            SchemeValue result = args.getLast();
            for (int i = args.size() - 2; i >= 0; i--) {
                result = appendTwo(args.get(i), result);
            }
            return result;
        }));

        env.define("reverse", new SchemeValue.BuiltinVal("reverse", args -> {
            if (args.size() != 1) throw new EvalError("reverse requires exactly 1 argument");
            SchemeValue lst = args.getFirst();
            SchemeValue result = SchemeValue.NIL;
            while (lst instanceof SchemeValue.PairVal p) {
                result = new SchemeValue.PairVal(p.car(), result);
                lst = p.cdr();
            }
            return result;
        }));

        // Type predicates
        env.define("string?", new SchemeValue.BuiltinVal("string?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.StringVal)));
        env.define("number?", new SchemeValue.BuiltinVal("number?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.IntVal)));
        env.define("boolean?", new SchemeValue.BuiltinVal("boolean?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.BoolVal)));
        env.define("pair?", new SchemeValue.BuiltinVal("pair?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.PairVal)));
        env.define("symbol?", new SchemeValue.BuiltinVal("symbol?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.SymbolVal)));
        env.define("char?", new SchemeValue.BuiltinVal("char?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.CharVal)));
        env.define("procedure?", new SchemeValue.BuiltinVal("procedure?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.LambdaVal
                || args.getFirst() instanceof SchemeValue.BuiltinVal
                || args.getFirst() instanceof SchemeValue.CpsBuiltinVal
                || args.getFirst() instanceof SchemeValue.ContinuationVal)));

        // I/O
        env.define("display", new SchemeValue.BuiltinVal("display", args -> {
            if (args.size() != 1) throw new EvalError("display requires exactly 1 argument");
            SchemeValue val = args.getFirst();
            if (val instanceof SchemeValue.StringVal s) {
                outputBuffer.append(s.value());
            } else if (val instanceof SchemeValue.CharVal c) {
                outputBuffer.append(c.value());
            } else {
                outputBuffer.append(val.display());
            }
            return new SchemeValue.VoidVal();
        }));
        env.define("write", new SchemeValue.BuiltinVal("write", args -> {
            if (args.size() != 1) throw new EvalError("write requires exactly 1 argument");
            outputBuffer.append(args.getFirst().display());
            return new SchemeValue.VoidVal();
        }));
        env.define("newline", new SchemeValue.BuiltinVal("newline", args -> {
            outputBuffer.append('\n');
            return new SchemeValue.VoidVal();
        }));

        // String operations
        env.define("string-append", new SchemeValue.BuiltinVal("string-append", args -> {
            StringBuilder sb = new StringBuilder();
            for (SchemeValue arg : args) {
                if (!(arg instanceof SchemeValue.StringVal s)) throw new EvalError("string-append: not a string");
                sb.append(s.value());
            }
            return new SchemeValue.StringVal(sb.toString());
        }));
        env.define("string-length", new SchemeValue.BuiltinVal("string-length", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-length: not a string");
            return new SchemeValue.IntVal(s.value().length());
        }));
        env.define("substring", new SchemeValue.BuiltinVal("substring", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("substring: not a string");
            int start = (int) requireInt(args.get(1));
            int end = (int) requireInt(args.get(2));
            return new SchemeValue.StringVal(s.value().substring(start, end));
        }));
        env.define("string->number", new SchemeValue.BuiltinVal("string->number", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string->number: not a string");
            try {
                return new SchemeValue.IntVal(Long.parseLong(s.value()));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        }));
        env.define("number->string", new SchemeValue.BuiltinVal("number->string", args ->
            new SchemeValue.StringVal(String.valueOf(requireInt(args.getFirst())))));
        env.define("symbol->string", new SchemeValue.BuiltinVal("symbol->string", args -> {
            if (!(args.getFirst() instanceof SchemeValue.SymbolVal s)) throw new EvalError("symbol->string: not a symbol");
            return new SchemeValue.StringVal(s.name());
        }));
        env.define("string->symbol", new SchemeValue.BuiltinVal("string->symbol", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string->symbol: not a string");
            return new SchemeValue.SymbolVal(s.value());
        }));
        env.define("string-ref", new SchemeValue.BuiltinVal("string-ref", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("string-ref: not a string");
            int idx = (int) requireInt(args.get(1));
            return new SchemeValue.CharVal(s.charAt(idx));
        }));
        env.define("string-copy", new SchemeValue.BuiltinVal("string-copy", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-copy: not a string");
            return new SchemeValue.StringVal(s.value().toCharArray());
        }));

        // Numeric utilities (L13)
        env.define("quotient", new SchemeValue.BuiltinVal("quotient", args -> {
            if (args.size() != 2) throw new EvalError("quotient requires 2 arguments");
            long a = requireInt(args.get(0));
            long b = requireInt(args.get(1));
            if (b == 0) throw new EvalError("Division by zero");
            // Truncate toward zero (Java default for long division)
            return new SchemeValue.IntVal(a / b);
        }));
        env.define("min", new SchemeValue.BuiltinVal("min", args -> {
            if (args.isEmpty()) throw new EvalError("min requires at least 1 argument");
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) {
                long v = requireInt(args.get(i));
                if (v < result) result = v;
            }
            return new SchemeValue.IntVal(result);
        }));
        env.define("max", new SchemeValue.BuiltinVal("max", args -> {
            if (args.isEmpty()) throw new EvalError("max requires at least 1 argument");
            long result = requireInt(args.getFirst());
            for (int i = 1; i < args.size(); i++) {
                long v = requireInt(args.get(i));
                if (v > result) result = v;
            }
            return new SchemeValue.IntVal(result);
        }));
        env.define("expt", new SchemeValue.BuiltinVal("expt", args -> {
            if (args.size() != 2) throw new EvalError("expt requires 2 arguments");
            long base = requireInt(args.get(0));
            long exp = requireInt(args.get(1));
            long result = 1;
            for (long i = 0; i < exp; i++) result *= base;
            return new SchemeValue.IntVal(result);
        }));
        env.define("zero?", new SchemeValue.BuiltinVal("zero?", args ->
            new SchemeValue.BoolVal(requireInt(args.getFirst()) == 0)));
        env.define("positive?", new SchemeValue.BuiltinVal("positive?", args ->
            new SchemeValue.BoolVal(requireInt(args.getFirst()) > 0)));
        env.define("negative?", new SchemeValue.BuiltinVal("negative?", args ->
            new SchemeValue.BoolVal(requireInt(args.getFirst()) < 0)));
        env.define("odd?", new SchemeValue.BuiltinVal("odd?", args ->
            new SchemeValue.BoolVal(requireInt(args.getFirst()) % 2 != 0)));
        env.define("even?", new SchemeValue.BuiltinVal("even?", args ->
            new SchemeValue.BoolVal(requireInt(args.getFirst()) % 2 == 0)));

        // List utilities (L13)
        env.define("list-ref", new SchemeValue.BuiltinVal("list-ref", args -> {
            if (args.size() != 2) throw new EvalError("list-ref requires 2 arguments");
            SchemeValue lst = args.get(0);
            int idx = (int) requireInt(args.get(1));
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof SchemeValue.PairVal p)) throw new EvalError("list-ref: index out of range");
                lst = p.cdr();
            }
            if (!(lst instanceof SchemeValue.PairVal p)) throw new EvalError("list-ref: index out of range");
            return p.car();
        }));
        env.define("list-tail", new SchemeValue.BuiltinVal("list-tail", args -> {
            if (args.size() != 2) throw new EvalError("list-tail requires 2 arguments");
            SchemeValue lst = args.get(0);
            int idx = (int) requireInt(args.get(1));
            for (int i = 0; i < idx; i++) {
                if (!(lst instanceof SchemeValue.PairVal p)) throw new EvalError("list-tail: index out of range");
                lst = p.cdr();
            }
            return lst;
        }));
        env.define("list?", new SchemeValue.BuiltinVal("list?", args -> {
            SchemeValue v = args.getFirst();
            while (v instanceof SchemeValue.PairVal p) v = p.cdr();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.NilVal);
        }));
        env.define("equal?", new SchemeValue.BuiltinVal("equal?", args -> {
            if (args.size() != 2) throw new EvalError("equal? requires 2 arguments");
            return new SchemeValue.BoolVal(schemeEqual(args.get(0), args.get(1)));
        }));
        env.define("assoc", new SchemeValue.BuiltinVal("assoc", args -> {
            if (args.size() != 2) throw new EvalError("assoc requires 2 arguments");
            SchemeValue key = args.get(0);
            SchemeValue alist = args.get(1);
            while (alist instanceof SchemeValue.PairVal p) {
                if (p.car() instanceof SchemeValue.PairVal entry) {
                    if (schemeEqual(entry.car(), key)) return p.car();
                }
                alist = p.cdr();
            }
            return new SchemeValue.BoolVal(false);
        }));
        env.define("eq?", new SchemeValue.BuiltinVal("eq?", args -> {
            if (args.size() != 2) throw new EvalError("eq? requires 2 arguments");
            SchemeValue a = args.get(0), b = args.get(1);
            if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb)
                return new SchemeValue.BoolVal(sa.name().equals(sb.name()));
            if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib)
                return new SchemeValue.BoolVal(ia.value() == ib.value());
            if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb)
                return new SchemeValue.BoolVal(ba.value() == bb.value());
            if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb)
                return new SchemeValue.BoolVal(ca.value() == cb.value());
            if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal)
                return new SchemeValue.BoolVal(true);
            if (a instanceof SchemeValue.VoidVal && b instanceof SchemeValue.VoidVal)
                return new SchemeValue.BoolVal(true);
            return new SchemeValue.BoolVal(a == b);
        }));

        env.define("eqv?", new SchemeValue.BuiltinVal("eqv?", args -> {
            if (args.size() != 2) throw new EvalError("eqv? requires 2 arguments");
            SchemeValue a = args.get(0), b = args.get(1);
            if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb)
                return new SchemeValue.BoolVal(sa.name().equals(sb.name()));
            if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib)
                return new SchemeValue.BoolVal(ia.value() == ib.value());
            if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb)
                return new SchemeValue.BoolVal(ba.value() == bb.value());
            if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb)
                return new SchemeValue.BoolVal(ca.value() == cb.value());
            if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal)
                return new SchemeValue.BoolVal(true);
            return new SchemeValue.BoolVal(a == b);
        }));

        // Vector operations (L15)
        env.define("vector", new SchemeValue.BuiltinVal("vector", args -> {
            SchemeValue[] elems = args.toArray(new SchemeValue[0]);
            return new SchemeValue.VectorVal(elems);
        }));
        env.define("make-vector", new SchemeValue.BuiltinVal("make-vector", args -> {
            if (args.isEmpty()) throw new EvalError("make-vector requires at least 1 argument");
            int len = (int) requireInt(args.get(0));
            SchemeValue fill = args.size() > 1 ? args.get(1) : new SchemeValue.IntVal(0);
            SchemeValue[] elems = new SchemeValue[len];
            for (int i = 0; i < len; i++) elems[i] = fill;
            return new SchemeValue.VectorVal(elems);
        }));
        env.define("vector-ref", new SchemeValue.BuiltinVal("vector-ref", args -> {
            if (args.size() != 2) throw new EvalError("vector-ref requires 2 arguments");
            if (!(args.get(0) instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-ref: not a vector");
            int idx = (int) requireInt(args.get(1));
            return v.get(idx);
        }));
        env.define("vector-set!", new SchemeValue.BuiltinVal("vector-set!", args -> {
            if (args.size() != 3) throw new EvalError("vector-set! requires 3 arguments");
            if (!(args.get(0) instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-set!: not a vector");
            int idx = (int) requireInt(args.get(1));
            v.set(idx, args.get(2));
            return new SchemeValue.VoidVal();
        }));
        env.define("vector-length", new SchemeValue.BuiltinVal("vector-length", args -> {
            if (args.size() != 1) throw new EvalError("vector-length requires 1 argument");
            if (!(args.get(0) instanceof SchemeValue.VectorVal v)) throw new EvalError("vector-length: not a vector");
            return new SchemeValue.IntVal(v.length());
        }));
        env.define("vector?", new SchemeValue.BuiltinVal("vector?", args ->
            new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.VectorVal)));
        env.define("vector->list", new SchemeValue.BuiltinVal("vector->list", args -> {
            if (args.size() != 1) throw new EvalError("vector->list requires 1 argument");
            if (!(args.get(0) instanceof SchemeValue.VectorVal v)) throw new EvalError("vector->list: not a vector");
            SchemeValue result = SchemeValue.NIL;
            for (int i = v.length() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(v.get(i), result);
            }
            return result;
        }));
        env.define("list->vector", new SchemeValue.BuiltinVal("list->vector", args -> {
            if (args.size() != 1) throw new EvalError("list->vector requires 1 argument");
            List<SchemeValue> elems = new ArrayList<>();
            SchemeValue cur = args.get(0);
            while (cur instanceof SchemeValue.PairVal p) {
                elems.add(p.car());
                cur = p.cdr();
            }
            return new SchemeValue.VectorVal(elems.toArray(new SchemeValue[0]));
        }));

        // error (L15 sboyer needs it)
        env.define("error", new SchemeValue.BuiltinVal("error", args -> {
            StringBuilder msg = new StringBuilder("Error");
            for (int i = 0; i < args.size(); i++) {
                SchemeValue a = args.get(i);
                if (a instanceof SchemeValue.BoolVal b && !b.value()) continue;
                if (a instanceof SchemeValue.StringVal s) msg.append(s.value());
                else msg.append(a.display());
            }
            throw new EvalError(msg.toString());
        }));

        // Character utilities (L13)
        env.define("char-alphabetic?", new SchemeValue.BuiltinVal("char-alphabetic?", args -> {
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-alphabetic?: not a char");
            return new SchemeValue.BoolVal(Character.isLetter(c.value()));
        }));
        env.define("char-numeric?", new SchemeValue.BuiltinVal("char-numeric?", args -> {
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-numeric?: not a char");
            return new SchemeValue.BoolVal(Character.isDigit(c.value()));
        }));
        env.define("char-upcase", new SchemeValue.BuiltinVal("char-upcase", args -> {
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-upcase: not a char");
            return new SchemeValue.CharVal(Character.toUpperCase(c.value()));
        }));
        env.define("char-downcase", new SchemeValue.BuiltinVal("char-downcase", args -> {
            if (!(args.getFirst() instanceof SchemeValue.CharVal c)) throw new EvalError("char-downcase: not a char");
            return new SchemeValue.CharVal(Character.toLowerCase(c.value()));
        }));
        env.define("char=?", new SchemeValue.BuiltinVal("char=?", args -> {
            if (!(args.get(0) instanceof SchemeValue.CharVal a) || !(args.get(1) instanceof SchemeValue.CharVal b))
                throw new EvalError("char=?: not chars");
            return new SchemeValue.BoolVal(a.value() == b.value());
        }));
        env.define("char<?", new SchemeValue.BuiltinVal("char<?", args -> {
            if (!(args.get(0) instanceof SchemeValue.CharVal a) || !(args.get(1) instanceof SchemeValue.CharVal b))
                throw new EvalError("char<?: not chars");
            return new SchemeValue.BoolVal(a.value() < b.value());
        }));

        // String comparison utilities (L13)
        env.define("string=?", new SchemeValue.BuiltinVal("string=?", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal a) || !(args.get(1) instanceof SchemeValue.StringVal b))
                throw new EvalError("string=?: not strings");
            return new SchemeValue.BoolVal(a.value().equals(b.value()));
        }));
        env.define("string<?", new SchemeValue.BuiltinVal("string<?", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal a) || !(args.get(1) instanceof SchemeValue.StringVal b))
                throw new EvalError("string<?: not strings");
            return new SchemeValue.BoolVal(a.value().compareTo(b.value()) < 0);
        }));
        env.define("string-ci=?", new SchemeValue.BuiltinVal("string-ci=?", args -> {
            if (!(args.get(0) instanceof SchemeValue.StringVal a) || !(args.get(1) instanceof SchemeValue.StringVal b))
                throw new EvalError("string-ci=?: not strings");
            return new SchemeValue.BoolVal(a.value().equalsIgnoreCase(b.value()));
        }));
        env.define("string-upcase", new SchemeValue.BuiltinVal("string-upcase", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-upcase: not a string");
            return new SchemeValue.StringVal(s.value().toUpperCase());
        }));
        env.define("string-downcase", new SchemeValue.BuiltinVal("string-downcase", args -> {
            if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw new EvalError("string-downcase: not a string");
            return new SchemeValue.StringVal(s.value().toLowerCase());
        }));

        // map (CPS-aware for multi-list support, L13)
        env.define("map", new SchemeValue.CpsBuiltinVal("map", (args, k) -> {
            if (args.size() < 2) return new Bounce.Err(new EvalError("map requires at least 2 arguments"));
            SchemeValue proc = args.getFirst();
            List<SchemeValue> lists = args.subList(1, args.size());
            return mapLoop(proc, lists, new ArrayList<>(), k);
        }));

        // string-set!
        env.define("string-set!", new SchemeValue.BuiltinVal("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set! requires 3 arguments");
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("string-set!: not a string");
            if (!s.isMutable()) throw new EvalError("string-set!: string is immutable");
            if (!(args.get(1) instanceof SchemeValue.IntVal n)) throw new EvalError("string-set!: not a number");
            if (!(args.get(2) instanceof SchemeValue.CharVal c)) throw new EvalError("string-set!: not a char");
            int idx = (int) n.value();
            if (idx < 0 || idx >= s.length()) throw new EvalError("string-set!: index out of range");
            s.setCharAt(idx, c.value());
            return new SchemeValue.VoidVal();
        }));

        // string->list
        env.define("string->list", new SchemeValue.BuiltinVal("string->list", args -> {
            if (args.size() != 1) throw new EvalError("string->list requires 1 argument");
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("string->list: not a string");
            SchemeValue result = new SchemeValue.NilVal();
            String str = s.value();
            for (int i = str.length() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(new SchemeValue.CharVal(str.charAt(i)), result);
            }
            return result;
        }));

        // list->string
        env.define("list->string", new SchemeValue.BuiltinVal("list->string", args -> {
            if (args.size() != 1) throw new EvalError("list->string requires 1 argument");
            StringBuilder sb = new StringBuilder();
            SchemeValue cur = args.get(0);
            while (cur instanceof SchemeValue.PairVal p) {
                if (!(p.car() instanceof SchemeValue.CharVal c)) throw new EvalError("list->string: not a character");
                sb.append(c.value());
                cur = p.cdr();
            }
            return new SchemeValue.StringVal(sb.toString());
        }));

        // char->integer
        env.define("char->integer", new SchemeValue.BuiltinVal("char->integer", args -> {
            if (args.size() != 1) throw new EvalError("char->integer requires 1 argument");
            if (!(args.get(0) instanceof SchemeValue.CharVal c)) throw new EvalError("char->integer: not a character");
            return new SchemeValue.IntVal((int) c.value());
        }));

        // integer->char
        env.define("integer->char", new SchemeValue.BuiltinVal("integer->char", args -> {
            if (args.size() != 1) throw new EvalError("integer->char requires 1 argument");
            long code = requireInt(args.get(0));
            return new SchemeValue.CharVal((char) code);
        }));

        // apply (CPS-aware: needs to forward continuation)
        env.define("apply", new SchemeValue.CpsBuiltinVal("apply", (args, k) -> {
            if (args.size() < 2) return new Bounce.Err(new EvalError("apply requires at least 2 arguments"));
            SchemeValue proc = args.getFirst();
            List<SchemeValue> callArgs = new ArrayList<>();
            for (int i = 1; i < args.size() - 1; i++) {
                callArgs.add(args.get(i));
            }
            SchemeValue last = args.getLast();
            while (last instanceof SchemeValue.PairVal p) {
                callArgs.add(p.car());
                last = p.cdr();
            }
            if (!(last instanceof SchemeValue.NilVal)) {
                return new Bounce.Err(new EvalError("apply: last argument must be a proper list"));
            }
            return applyProc(proc, callArgs, "apply", k);
        }));

        // call/cc and call-with-current-continuation
        SchemeValue.CpsBuiltinFunc callccFunc = (args, k) -> {
            if (args.size() != 1) return new Bounce.Err(new EvalError("call/cc requires 1 argument"));
            SchemeValue f = args.getFirst();
            SchemeValue cont = new SchemeValue.ContinuationVal(k, new ArrayList<>(windStack));
            return applyProc(f, List.of(cont), "call/cc", k);
        };
        env.define("call/cc", new SchemeValue.CpsBuiltinVal("call/cc", callccFunc));
        env.define("call-with-current-continuation", new SchemeValue.CpsBuiltinVal("call-with-current-continuation", callccFunc));

        // dynamic-wind
        env.define("dynamic-wind", new SchemeValue.CpsBuiltinVal("dynamic-wind", (args, k) -> {
            if (args.size() != 3) return new Bounce.Err(new EvalError("dynamic-wind requires 3 arguments"));
            SchemeValue inThunk = args.get(0);
            SchemeValue bodyThunk = args.get(1);
            SchemeValue outThunk = args.get(2);
            WindRecord wr = new WindRecord(inThunk, outThunk);
            // Call in-thunk
            return applyProc(inThunk, List.of(), "dynamic-wind", inResult -> {
                // Push wind record
                windStack.add(wr);
                // Call body-thunk
                return applyProc(bodyThunk, List.of(), "dynamic-wind", bodyResult -> {
                    // Pop wind record
                    windStack.remove(windStack.size() - 1);
                    // Call out-thunk
                    return applyProc(outThunk, List.of(), "dynamic-wind", outResult ->
                        // Return body's value
                        k.apply(bodyResult)
                    );
                });
            });
        }));

        // raise
        env.define("raise", new SchemeValue.CpsBuiltinVal("raise", (args, k) -> {
            if (args.size() != 1) return new Bounce.Err(new EvalError("raise requires 1 argument"));
            SchemeValue val = args.getFirst();
            if (handlerStack.isEmpty()) {
                return new Bounce.Err(new EvalError("Unhandled exception: " + val.display()));
            }
            SchemeValue handler = handlerStack.remove(handlerStack.size() - 1);
            return applyProc(handler, List.of(val), "raise", result ->
                new Bounce.Err(new EvalError("exception handler returned for non-continuable exception"))
            );
        }));

        // with-exception-handler
        env.define("with-exception-handler", new SchemeValue.CpsBuiltinVal("with-exception-handler", (args, k) -> {
            if (args.size() != 2) return new Bounce.Err(new EvalError("with-exception-handler requires 2 arguments"));
            SchemeValue handler = args.get(0);
            SchemeValue thunk = args.get(1);
            handlerStack.add(handler);
            return applyProc(thunk, List.of(), "with-exception-handler", result -> {
                handlerStack.remove(handlerStack.size() - 1);
                return k.apply(result);
            });
        }));
    }

    // ── CPS eval ──────────────────────────────────────────────────────

    private Bounce eval(SchemeValue expr, Environment env, SchemeValue.Cont k) {
        return switch (expr) {
            case SchemeValue.IntVal v -> k.apply(v);
            case SchemeValue.BoolVal v -> k.apply(v);
            case SchemeValue.StringVal v -> k.apply(v);
            case SchemeValue.CharVal v -> k.apply(v);
            case SchemeValue.VoidVal v -> k.apply(v);
            case SchemeValue.NilVal v -> k.apply(v);
            case SchemeValue.PairVal v -> k.apply(v);
            case SchemeValue.LambdaVal v -> k.apply(v);
            case SchemeValue.BuiltinVal v -> k.apply(v);
            case SchemeValue.CpsBuiltinVal v -> k.apply(v);
            case SchemeValue.ContinuationVal v -> k.apply(v);
            case SchemeValue.SyntaxRulesVal v -> k.apply(v);
            case SchemeValue.VectorVal v -> k.apply(v);
            case SchemeValue.SymbolVal v -> {
                try {
                    yield k.apply(env.get(v.name()));
                } catch (EvalError e) {
                    yield new Bounce.Err(new EvalError("Unbound variable: " + v.name() + " at " + v.line() + ":" + v.col()));
                }
            }
            case SchemeValue.ListVal listVal -> evalList(listVal, env, k);
        };
    }

    private Bounce evalList(SchemeValue.ListVal listVal, Environment env, SchemeValue.Cont k) {
        List<SchemeValue> elems = listVal.elements();
        String pos = listVal.line() + ":" + listVal.col();
        if (elems.isEmpty()) return new Bounce.Err(new EvalError("Empty application at " + pos));
        SchemeValue head = elems.getFirst();

        // Check special forms
        if (head instanceof SchemeValue.SymbolVal sym) {
            Bounce special = evalSpecialForm(sym.name(), elems, env, pos, k);
            if (special != null) return special;

            // Check for macro invocation
            try {
                SchemeValue val = env.get(sym.name());
                if (val instanceof SchemeValue.SyntaxRulesVal macro) {
                    SchemeValue expanded = expandMacro(macro, elems, env);
                    return new Bounce.More(() -> eval(expanded, env, k));
                }
            } catch (EvalError ignored) {}
        }

        // Procedure call: eval head, eval args (right-to-left for Guile compat), apply
        return new Bounce.More(() -> eval(head, env, proc ->
            evalArgsRTL(elems, elems.size() - 1, 1, List.of(), env, args ->
                applyProc(proc, args, pos, k)
            )
        ));
    }

    private Bounce evalSpecialForm(String name, List<SchemeValue> elems, Environment env,
                                    String pos, SchemeValue.Cont k) {
        return switch (name) {
            case "quote" -> {
                if (elems.size() != 2)
                    yield new Bounce.Err(new EvalError("quote requires exactly 1 argument at " + pos));
                yield k.apply(quoteDatum(elems.get(1)));
            }
            case "set!" -> {
                if (elems.size() != 3)
                    yield new Bounce.Err(new EvalError("set! requires exactly 2 arguments at " + pos));
                if (!(elems.get(1) instanceof SchemeValue.SymbolVal sym2))
                    yield new Bounce.Err(new EvalError("set!: expected symbol"));
                yield new Bounce.More(() -> eval(elems.get(2), env, val -> {
                    try {
                        env.set(sym2.name(), val);
                        return k.apply(new SchemeValue.VoidVal());
                    } catch (EvalError e) {
                        return new Bounce.Err(e);
                    }
                }));
            }
            case "define" -> evalDefine(elems, env, pos, k);
            case "lambda" -> {
                try {
                    yield k.apply(buildLambda(elems, env, pos));
                } catch (EvalError e) {
                    yield new Bounce.Err(e);
                }
            }
            case "if" -> {
                if (elems.size() < 3 || elems.size() > 4)
                    yield new Bounce.Err(new EvalError("if requires 2 or 3 arguments at " + pos));
                yield new Bounce.More(() -> eval(elems.get(1), env, condVal -> {
                    if (condVal.isTruthy()) {
                        return new Bounce.More(() -> eval(elems.get(2), env, k));
                    } else if (elems.size() == 4) {
                        return new Bounce.More(() -> eval(elems.get(3), env, k));
                    }
                    return k.apply(new SchemeValue.VoidVal());
                }));
            }
            case "begin" -> evalSequence(elems, 1, env, k);
            case "cond" -> evalCond(elems, 1, env, k);
            case "and" -> {
                if (elems.size() == 1) yield k.apply(new SchemeValue.BoolVal(true));
                yield evalAnd(elems, 1, env, k);
            }
            case "or" -> {
                if (elems.size() == 1) yield k.apply(new SchemeValue.BoolVal(false));
                yield evalOr(elems, 1, env, k);
            }
            case "let" -> evalLet(elems, env, pos, k);
            case "letrec" -> evalLetrec(elems, env, pos, false, k);
            case "letrec*" -> evalLetrec(elems, env, pos, true, k);
            case "case" -> evalCase(elems, env, pos, k);
            case "do" -> evalDo(elems, env, pos, k);
            case "define-syntax" -> evalDefineSyntax(elems, env, pos, k);
            case "guard" -> evalGuard(elems, env, pos, k);
            default -> null; // not a special form
        };
    }

    // ── Sequence / body evaluation ────────────────────────────────────

    private Bounce evalSequence(List<SchemeValue> exprs, int start, Environment env, SchemeValue.Cont k) {
        if (start >= exprs.size()) return k.apply(new SchemeValue.VoidVal());
        return evalSeqFrom(exprs, start, env, k);
    }

    private Bounce evalSeqFrom(List<SchemeValue> exprs, int idx, Environment env, SchemeValue.Cont k) {
        if (idx == exprs.size() - 1) {
            return new Bounce.More(() -> eval(exprs.get(idx), env, k));
        }
        return new Bounce.More(() -> eval(exprs.get(idx), env, ignored ->
            evalSeqFrom(exprs, idx + 1, env, k)
        ));
    }

    private Bounce evalBody(List<SchemeValue> body, Environment env, SchemeValue.Cont k) {
        return evalSeqFrom(body, 0, env, k);
    }

    // ── Argument evaluation ───────────────────────────────────────────

    /** Evaluate args right-to-left (Guile-compatible), returning them in left-to-right order. */
    private Bounce evalArgsRTL(List<SchemeValue> elems, int idx, int start,
                                List<SchemeValue> acc, Environment env,
                                Function<List<SchemeValue>, Bounce> then) {
        if (idx < start) return then.apply(acc);
        return new Bounce.More(() -> eval(elems.get(idx), env, val -> {
            // Prepend val to acc — builds left-to-right order as we iterate right-to-left
            List<SchemeValue> newAcc = new ArrayList<>(acc.size() + 1);
            newAcc.add(val);
            newAcc.addAll(acc);
            return evalArgsRTL(elems, idx - 1, start, newAcc, env, then);
        }));
    }

    private Bounce evalInitExprs(List<SchemeValue> exprs, int idx, List<SchemeValue> acc,
                                  Environment env, Function<List<SchemeValue>, Bounce> then) {
        if (idx >= exprs.size()) return then.apply(acc);
        return new Bounce.More(() -> eval(exprs.get(idx), env, val -> {
            List<SchemeValue> newAcc = new ArrayList<>(acc);
            newAcc.add(val);
            return evalInitExprs(exprs, idx + 1, newAcc, env, then);
        }));
    }

    // ── Procedure application ─────────────────────────────────────────

    private Bounce applyProc(SchemeValue proc, List<SchemeValue> args, String pos, SchemeValue.Cont k) {
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            try {
                SchemeValue result = builtin.func().apply(args);
                return k.apply(result);
            } catch (EvalError e) {
                String msg = e.getMessage();
                if (!msg.matches(".*\\d+:\\d+.*")) {
                    return new Bounce.Err(new EvalError(msg + " at " + pos));
                }
                return new Bounce.Err(e);
            }
        }
        if (proc instanceof SchemeValue.CpsBuiltinVal cps) {
            return cps.func().apply(args, k);
        }
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            try {
                Environment callEnv = applyLambdaEnv(lambda, args, pos);
                return evalBody(lambda.body(), callEnv, k);
            } catch (EvalError e) {
                return new Bounce.Err(e);
            }
        }
        if (proc instanceof SchemeValue.ContinuationVal contVal) {
            if (args.size() != 1) {
                return new Bounce.Err(new EvalError("continuation requires exactly 1 argument at " + pos));
            }
            SchemeValue val = args.getFirst();
            @SuppressWarnings("unchecked")
            List<WindRecord> targetWinds = (List<WindRecord>) contVal.windState();
            if (targetWinds == null) {
                return contVal.k().apply(val);
            }
            // Do wind transition then invoke continuation
            return doWindTransition(targetWinds, val, contVal.k());
        }
        return new Bounce.Err(new EvalError("Not a procedure: " + proc.display() + " at " + pos));
    }

    // ── dynamic-wind transition ─────────────────────────────────────

    private Bounce doWindTransition(List<WindRecord> target, SchemeValue val, SchemeValue.Cont k) {
        // Find common prefix length (by identity)
        int common = 0;
        int minLen = Math.min(windStack.size(), target.size());
        while (common < minLen && windStack.get(common) == target.get(common)) {
            common++;
        }
        // Unwind from current, then rewind to target
        return doUnwind(common, target, val, k);
    }

    private Bounce doUnwind(int commonLen, List<WindRecord> target, SchemeValue val, SchemeValue.Cont k) {
        if (windStack.size() <= commonLen) {
            // Done unwinding, start rewinding
            return doRewind(target, commonLen, val, k);
        }
        WindRecord wr = windStack.remove(windStack.size() - 1);
        return applyProc(wr.outThunk, List.of(), "dynamic-wind", ignored ->
            doUnwind(commonLen, target, val, k)
        );
    }

    private Bounce doRewind(List<WindRecord> target, int idx, SchemeValue val, SchemeValue.Cont k) {
        if (idx >= target.size()) {
            // Done rewinding, invoke continuation
            return k.apply(val);
        }
        WindRecord wr = target.get(idx);
        return applyProc(wr.inThunk, List.of(), "dynamic-wind", ignored -> {
            windStack.add(wr);
            return doRewind(target, idx + 1, val, k);
        });
    }

    // ── guard ─────────────────────────────────────────────────────────

    private Bounce evalGuard(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        // (guard (var clause1 clause2 ...) body ...)
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("guard requires at least 2 arguments at " + pos));
        if (!(elems.get(1) instanceof SchemeValue.ListVal clauseList) || clauseList.elements().size() < 2)
            return new Bounce.Err(new EvalError("guard: invalid clause spec at " + pos));

        String varName = ((SchemeValue.SymbolVal) clauseList.elements().getFirst()).name();
        List<SchemeValue> clauses = clauseList.elements().subList(1, clauseList.elements().size());
        List<SchemeValue> body = elems.subList(2, elems.size());

        // Capture guard-site wind state for proper dynamic-wind interaction
        List<WindRecord> guardWindState = new ArrayList<>(windStack);
        SchemeValue.Cont guardK = k;
        Environment guardEnv = env;

        // Exception handler: on raise, transition back to guard context, then eval clauses
        SchemeValue handler = new SchemeValue.CpsBuiltinVal("guard-handler", (args, raiseK) -> {
            SchemeValue exnVal = args.getFirst();
            // Wind back to guard's dynamic context (runs out-thunks)
            return doWindTransition(guardWindState, exnVal, transitionedVal -> {
                Environment clauseEnv = new Environment(guardEnv);
                clauseEnv.define(varName, transitionedVal);
                return evalGuardClauses(clauses, 0, clauseEnv, transitionedVal, guardK);
            });
        });

        handlerStack.add(handler);
        return evalBody(body, env, result -> {
            handlerStack.remove(handlerStack.size() - 1);
            return k.apply(result);
        });
    }

    private Bounce evalGuardClauses(List<SchemeValue> clauses, int idx, Environment env,
                                     SchemeValue exnVal, SchemeValue.Cont guardK) {
        if (idx >= clauses.size()) {
            // No clause matched, re-raise
            if (handlerStack.isEmpty()) {
                return new Bounce.Err(new EvalError("Unhandled exception: " + exnVal.display()));
            }
            SchemeValue nextHandler = handlerStack.remove(handlerStack.size() - 1);
            return applyProc(nextHandler, List.of(exnVal), "guard-reraise", result ->
                new Bounce.Err(new EvalError("exception handler returned for non-continuable exception"))
            );
        }

        if (!(clauses.get(idx) instanceof SchemeValue.ListVal clauseList) || clauseList.elements().isEmpty())
            return new Bounce.Err(new EvalError("guard: bad clause"));

        List<SchemeValue> parts = clauseList.elements();

        // Check for else clause
        if (parts.getFirst() instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
            return evalSequence(parts, 1, env, guardK);
        }

        // Evaluate test
        return new Bounce.More(() -> eval(parts.getFirst(), env, testResult -> {
            if (testResult.isTruthy()) {
                if (parts.size() == 1) {
                    return guardK.apply(testResult);
                }
                return evalSequence(parts, 1, env, guardK);
            }
            return evalGuardClauses(clauses, idx + 1, env, exnVal, guardK);
        }));
    }

    // ── Special form helpers ──────────────────────────────────────────

    private Bounce evalDefine(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("define requires at least 2 arguments at " + pos));
        SchemeValue target = elems.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            return new Bounce.More(() -> eval(elems.get(2), env, val -> {
                env.define(sym.name(), val);
                return k.apply(new SchemeValue.VoidVal());
            }));
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            try {
                List<SchemeValue> parts = nameAndParams.elements();
                if (parts.isEmpty()) throw new EvalError("define: empty name list");
                if (!(parts.getFirst() instanceof SchemeValue.SymbolVal fnName))
                    throw new EvalError("define: expected symbol as function name");
                List<String> params = new ArrayList<>();
                String restParam = null;
                for (int i = 1; i < parts.size(); i++) {
                    if (parts.get(i) instanceof SchemeValue.SymbolVal p && p.name().equals(".")) {
                        if (i + 1 >= parts.size()) throw new EvalError("define: expected symbol after dot");
                        if (!(parts.get(i + 1) instanceof SchemeValue.SymbolVal rest))
                            throw new EvalError("define: expected symbol after dot");
                        restParam = rest.name();
                        break;
                    }
                    if (!(parts.get(i) instanceof SchemeValue.SymbolVal p))
                        throw new EvalError("define: expected symbol as parameter");
                    params.add(p.name());
                }
                List<SchemeValue> body = elems.subList(2, elems.size());
                SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, restParam, body, env);
                env.define(fnName.name(), lambda);
                return k.apply(new SchemeValue.VoidVal());
            } catch (EvalError e) {
                return new Bounce.Err(e);
            }
        }
        return new Bounce.Err(new EvalError("define: invalid syntax"));
    }

    private Bounce evalCond(List<SchemeValue> elems, int idx, Environment env, SchemeValue.Cont k) {
        if (idx >= elems.size()) return k.apply(new SchemeValue.VoidVal());
        if (!(elems.get(idx) instanceof SchemeValue.ListVal clause))
            return new Bounce.Err(new EvalError("cond: expected clause"));
        List<SchemeValue> parts = clause.elements();
        if (parts.isEmpty()) return new Bounce.Err(new EvalError("cond: empty clause"));
        boolean isElse = parts.getFirst() instanceof SchemeValue.SymbolVal s && s.name().equals("else");
        if (isElse) {
            if (parts.size() > 1) return evalSeqFrom(parts, 1, env, k);
            return k.apply(new SchemeValue.VoidVal());
        }
        return new Bounce.More(() -> eval(parts.getFirst(), env, testVal -> {
            if (testVal.isTruthy()) {
                if (parts.size() > 1) return evalSeqFrom(parts, 1, env, k);
                return k.apply(testVal);
            }
            return new Bounce.More(() -> evalCond(elems, idx + 1, env, k));
        }));
    }

    private Bounce evalAnd(List<SchemeValue> elems, int idx, Environment env, SchemeValue.Cont k) {
        if (idx == elems.size() - 1) {
            return new Bounce.More(() -> eval(elems.get(idx), env, k));
        }
        return new Bounce.More(() -> eval(elems.get(idx), env, val -> {
            if (!val.isTruthy()) return k.apply(val);
            return evalAnd(elems, idx + 1, env, k);
        }));
    }

    private Bounce evalOr(List<SchemeValue> elems, int idx, Environment env, SchemeValue.Cont k) {
        if (idx == elems.size() - 1) {
            return new Bounce.More(() -> eval(elems.get(idx), env, k));
        }
        return new Bounce.More(() -> eval(elems.get(idx), env, val -> {
            if (val.isTruthy()) return k.apply(val);
            return evalOr(elems, idx + 1, env, k);
        }));
    }

    private Bounce evalLet(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("let requires bindings and body at " + pos));

        // Named let: (let name ((var init) ...) body...)
        if (elems.get(1) instanceof SchemeValue.SymbolVal nameSym) {
            if (elems.size() < 4)
                return new Bounce.Err(new EvalError("named let requires bindings and body"));
            String loopName = nameSym.name();
            if (!(elems.get(2) instanceof SchemeValue.ListVal bindingsList))
                return new Bounce.Err(new EvalError("let: expected bindings list"));
            List<String> params = new ArrayList<>();
            List<SchemeValue> initExprs = new ArrayList<>();
            for (SchemeValue b : bindingsList.elements()) {
                if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2)
                    return new Bounce.Err(new EvalError("let: invalid binding"));
                if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s))
                    return new Bounce.Err(new EvalError("let: expected symbol in binding"));
                params.add(s.name());
                initExprs.add(binding.elements().get(1));
            }
            List<SchemeValue> body = elems.subList(3, elems.size());
            return evalInitExprs(initExprs, 0, new ArrayList<>(), env, inits -> {
                Environment letEnv = new Environment(env);
                SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, body, letEnv);
                letEnv.define(loopName, lambda);
                Environment callEnv = new Environment(lambda.env());
                for (int i = 0; i < params.size(); i++) {
                    callEnv.define(params.get(i), inits.get(i));
                }
                return evalBody(body, callEnv, k);
            });
        }

        // Regular let
        if (!(elems.get(1) instanceof SchemeValue.ListVal bindingsList))
            return new Bounce.Err(new EvalError("let: expected bindings list"));
        Environment letEnv = new Environment(env);
        List<SchemeValue> body = elems.subList(2, elems.size());
        return evalLetBindings(bindingsList.elements(), 0, env, letEnv, body, k);
    }

    private Bounce evalLetBindings(List<SchemeValue> bindings, int idx, Environment outerEnv,
                                    Environment letEnv, List<SchemeValue> body, SchemeValue.Cont k) {
        if (idx >= bindings.size()) return evalBody(body, letEnv, k);
        SchemeValue b = bindings.get(idx);
        if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2)
            return new Bounce.Err(new EvalError("let: invalid binding"));
        if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s))
            return new Bounce.Err(new EvalError("let: expected symbol in binding"));
        return new Bounce.More(() -> eval(binding.elements().get(1), outerEnv, val -> {
            letEnv.define(s.name(), val);
            return evalLetBindings(bindings, idx + 1, outerEnv, letEnv, body, k);
        }));
    }

    // ── letrec / letrec* ───────────────────────────────────────────────

    private Bounce evalLetrec(List<SchemeValue> elems, Environment env, String pos,
                               boolean isStar, SchemeValue.Cont k) {
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("letrec requires bindings and body at " + pos));
        if (!(elems.get(1) instanceof SchemeValue.ListVal bindingsList))
            return new Bounce.Err(new EvalError("letrec: expected bindings list"));
        List<String> names = new ArrayList<>();
        List<SchemeValue> initExprs = new ArrayList<>();
        for (SchemeValue b : bindingsList.elements()) {
            if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2)
                return new Bounce.Err(new EvalError("letrec: invalid binding"));
            if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s))
                return new Bounce.Err(new EvalError("letrec: expected symbol in binding"));
            names.add(s.name());
            initExprs.add(binding.elements().get(1));
        }
        List<SchemeValue> body = elems.subList(2, elems.size());
        Environment letrecEnv = new Environment(env);
        for (String name : names) {
            letrecEnv.define(name, new SchemeValue.VoidVal());
        }
        if (isStar) {
            return evalLetrecStarBindings(names, initExprs, 0, letrecEnv, body, k);
        } else {
            return evalInitExprs(initExprs, 0, new ArrayList<>(), letrecEnv, inits -> {
                for (int i = 0; i < names.size(); i++) {
                    letrecEnv.define(names.get(i), inits.get(i));
                }
                return evalBody(body, letrecEnv, k);
            });
        }
    }

    private Bounce evalLetrecStarBindings(List<String> names, List<SchemeValue> initExprs, int idx,
                                           Environment env, List<SchemeValue> body, SchemeValue.Cont k) {
        if (idx >= names.size()) return evalBody(body, env, k);
        return new Bounce.More(() -> eval(initExprs.get(idx), env, val -> {
            env.define(names.get(idx), val);
            return evalLetrecStarBindings(names, initExprs, idx + 1, env, body, k);
        }));
    }

    // ── case ─────────────────────────────────────────────────────────

    private Bounce evalCase(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() < 2)
            return new Bounce.Err(new EvalError("case requires key expression at " + pos));
        return new Bounce.More(() -> eval(elems.get(1), env, keyVal ->
            evalCaseClauses(elems, 2, keyVal, env, k)
        ));
    }

    private Bounce evalCaseClauses(List<SchemeValue> elems, int idx, SchemeValue keyVal,
                                    Environment env, SchemeValue.Cont k) {
        if (idx >= elems.size()) return k.apply(new SchemeValue.VoidVal());
        if (!(elems.get(idx) instanceof SchemeValue.ListVal clause))
            return new Bounce.Err(new EvalError("case: expected clause"));
        List<SchemeValue> parts = clause.elements();
        if (parts.isEmpty()) return new Bounce.Err(new EvalError("case: empty clause"));
        // Check for else clause
        if (parts.getFirst() instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
            if (parts.size() > 1) return evalSeqFrom(parts, 1, env, k);
            return k.apply(new SchemeValue.VoidVal());
        }
        // Datums list
        if (!(parts.getFirst() instanceof SchemeValue.ListVal datums))
            return new Bounce.Err(new EvalError("case: expected datum list"));
        boolean matched = false;
        for (SchemeValue datum : datums.elements()) {
            SchemeValue d = quoteDatum(datum);
            if (schemeEqv(keyVal, d)) { matched = true; break; }
        }
        if (matched) {
            if (parts.size() > 1) return evalSeqFrom(parts, 1, env, k);
            return k.apply(new SchemeValue.VoidVal());
        }
        return evalCaseClauses(elems, idx + 1, keyVal, env, k);
    }

    private boolean schemeEqv(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb)
            return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib)
            return ia.value() == ib.value();
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb)
            return ba.value() == bb.value();
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb)
            return ca.value() == cb.value();
        if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal)
            return true;
        return a == b;
    }

    // ── do ───────────────────────────────────────────────────────────

    private Bounce evalDo(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        // (do ((var init step) ...) (test expr ...) body ...)
        if (elems.size() < 3)
            return new Bounce.Err(new EvalError("do requires bindings and test at " + pos));
        if (!(elems.get(1) instanceof SchemeValue.ListVal bindingsList))
            return new Bounce.Err(new EvalError("do: expected bindings list"));
        if (!(elems.get(2) instanceof SchemeValue.ListVal testClause))
            return new Bounce.Err(new EvalError("do: expected test clause"));

        List<String> varNames = new ArrayList<>();
        List<SchemeValue> initExprs = new ArrayList<>();
        List<SchemeValue> stepExprs = new ArrayList<>(); // null means no step
        for (SchemeValue b : bindingsList.elements()) {
            if (!(b instanceof SchemeValue.ListVal binding))
                return new Bounce.Err(new EvalError("do: invalid binding"));
            List<SchemeValue> bElems = binding.elements();
            if (bElems.size() < 2 || bElems.size() > 3)
                return new Bounce.Err(new EvalError("do: invalid binding"));
            if (!(bElems.get(0) instanceof SchemeValue.SymbolVal s))
                return new Bounce.Err(new EvalError("do: expected symbol in binding"));
            varNames.add(s.name());
            initExprs.add(bElems.get(1));
            stepExprs.add(bElems.size() == 3 ? bElems.get(2) : null);
        }

        List<SchemeValue> testParts = testClause.elements();
        if (testParts.isEmpty())
            return new Bounce.Err(new EvalError("do: test clause must have at least test expression"));
        SchemeValue testExpr = testParts.getFirst();
        List<SchemeValue> resultExprs = testParts.subList(1, testParts.size());
        List<SchemeValue> bodyExprs = elems.subList(3, elems.size());

        // Evaluate init exprs in outer env
        return evalInitExprs(initExprs, 0, new ArrayList<>(), env, inits -> {
            Environment doEnv = new Environment(env);
            for (int i = 0; i < varNames.size(); i++) {
                doEnv.define(varNames.get(i), inits.get(i));
            }
            return doLoop(varNames, stepExprs, testExpr, resultExprs, bodyExprs, doEnv, k);
        });
    }

    private Bounce doLoop(List<String> varNames, List<SchemeValue> stepExprs,
                           SchemeValue testExpr, List<SchemeValue> resultExprs,
                           List<SchemeValue> bodyExprs, Environment doEnv, SchemeValue.Cont k) {
        return new Bounce.More(() -> eval(testExpr, doEnv, testVal -> {
            if (testVal.isTruthy()) {
                if (resultExprs.isEmpty()) return k.apply(new SchemeValue.VoidVal());
                return evalSeqFrom(resultExprs, 0, doEnv, k);
            }
            // Execute body (for side effects)
            Bounce afterBody;
            if (bodyExprs.isEmpty()) {
                afterBody = doStep(varNames, stepExprs, doEnv, testExpr, resultExprs, bodyExprs, k);
            } else {
                afterBody = evalSequence(bodyExprs, 0, doEnv, ignored ->
                    doStep(varNames, stepExprs, doEnv, testExpr, resultExprs, bodyExprs, k)
                );
            }
            return afterBody;
        }));
    }

    private Bounce doStep(List<String> varNames, List<SchemeValue> stepExprs,
                           Environment doEnv, SchemeValue testExpr,
                           List<SchemeValue> resultExprs, List<SchemeValue> bodyExprs,
                           SchemeValue.Cont k) {
        // Evaluate all step exprs using current values (parallel)
        List<SchemeValue> stepsToEval = new ArrayList<>();
        List<Integer> stepIndices = new ArrayList<>();
        for (int i = 0; i < stepExprs.size(); i++) {
            if (stepExprs.get(i) != null) {
                stepsToEval.add(stepExprs.get(i));
                stepIndices.add(i);
            }
        }
        if (stepsToEval.isEmpty()) {
            return doLoop(varNames, stepExprs, testExpr, resultExprs, bodyExprs, doEnv, k);
        }
        return evalInitExprs(stepsToEval, 0, new ArrayList<>(), doEnv, stepVals -> {
            for (int i = 0; i < stepIndices.size(); i++) {
                doEnv.define(varNames.get(stepIndices.get(i)), stepVals.get(i));
            }
            return doLoop(varNames, stepExprs, testExpr, resultExprs, bodyExprs, doEnv, k);
        });
    }

    // ── Lambda construction (no CPS needed — doesn't eval anything) ──

    private SchemeValue buildLambda(List<SchemeValue> elems, Environment env, String pos) throws EvalError {
        if (elems.size() < 3) throw new EvalError("lambda requires parameters and body at " + pos);
        SchemeValue paramSpec = elems.get(1);
        if (!(paramSpec instanceof SchemeValue.ListVal paramList))
            throw new EvalError("lambda: expected parameter list");
        List<String> params = new ArrayList<>();
        String restParam = null;
        List<SchemeValue> pElems = paramList.elements();
        for (int i = 0; i < pElems.size(); i++) {
            if (pElems.get(i) instanceof SchemeValue.SymbolVal sym && sym.name().equals(".")) {
                if (i + 1 >= pElems.size()) throw new EvalError("lambda: expected symbol after dot");
                if (!(pElems.get(i + 1) instanceof SchemeValue.SymbolVal rest))
                    throw new EvalError("lambda: expected symbol after dot");
                restParam = rest.name();
                break;
            }
            if (!(pElems.get(i) instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: expected symbol as parameter");
            params.add(sym.name());
        }
        List<SchemeValue> body = elems.subList(2, elems.size());
        return new SchemeValue.LambdaVal(params, restParam, body, env);
    }

    // ── Helpers ───────────────────────────────────────────────────────

    private SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.NilVal) return b;
        if (a instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        }
        throw new EvalError("append: not a proper list");
    }

    private SchemeValue quoteDatum(SchemeValue v) {
        if (v instanceof SchemeValue.ListVal list) {
            if (list.elements().isEmpty()) return SchemeValue.NIL;
            SchemeValue result = SchemeValue.NIL;
            for (int i = list.elements().size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(quoteDatum(list.elements().get(i)), result);
            }
            return result;
        }
        return v;
    }

    private Environment applyLambdaEnv(SchemeValue.LambdaVal lambda, List<SchemeValue> args, String pos) throws EvalError {
        int required = lambda.params().size();
        if (lambda.restParam() != null) {
            if (args.size() < required)
                throw new EvalError("Expected at least " + required + " arguments, got " + args.size() + " at " + pos);
        } else {
            if (args.size() != required)
                throw new EvalError("Expected " + required + " arguments, got " + args.size() + " at " + pos);
        }
        Environment callEnv = new Environment(lambda.env());
        for (int i = 0; i < required; i++) {
            callEnv.define(lambda.params().get(i), args.get(i));
        }
        if (lambda.restParam() != null) {
            SchemeValue rest = SchemeValue.NIL;
            for (int i = args.size() - 1; i >= required; i--) {
                rest = new SchemeValue.PairVal(args.get(i), rest);
            }
            callEnv.define(lambda.restParam(), rest);
        }
        return callEnv;
    }

    // ── Macro system (define-syntax / syntax-rules) ────────────────

    private Bounce evalDefineSyntax(List<SchemeValue> elems, Environment env, String pos, SchemeValue.Cont k) {
        if (elems.size() != 3)
            return new Bounce.Err(new EvalError("define-syntax requires 2 arguments at " + pos));
        if (!(elems.get(1) instanceof SchemeValue.SymbolVal nameSym))
            return new Bounce.Err(new EvalError("define-syntax: expected symbol"));
        SchemeValue transformer = elems.get(2);
        if (!(transformer instanceof SchemeValue.ListVal tList) || tList.elements().isEmpty())
            return new Bounce.Err(new EvalError("define-syntax: expected syntax-rules"));
        List<SchemeValue> tElems = tList.elements();
        if (!(tElems.getFirst() instanceof SchemeValue.SymbolVal sr) || !sr.name().equals("syntax-rules"))
            return new Bounce.Err(new EvalError("define-syntax: expected syntax-rules"));
        if (tElems.size() < 2)
            return new Bounce.Err(new EvalError("syntax-rules requires literals list"));
        // Parse literals list
        if (!(tElems.get(1) instanceof SchemeValue.ListVal litList))
            return new Bounce.Err(new EvalError("syntax-rules: expected literals list"));
        List<String> literals = new ArrayList<>();
        for (SchemeValue lit : litList.elements()) {
            if (lit instanceof SchemeValue.SymbolVal s) literals.add(s.name());
        }
        // Parse pattern-template pairs
        List<SchemeValue> patterns = new ArrayList<>();
        List<SchemeValue> templates = new ArrayList<>();
        for (int i = 2; i < tElems.size(); i++) {
            if (!(tElems.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().size() != 2)
                return new Bounce.Err(new EvalError("syntax-rules: invalid clause"));
            patterns.add(clause.elements().get(0));
            templates.add(clause.elements().get(1));
        }
        env.define(nameSym.name(), new SchemeValue.SyntaxRulesVal(literals, patterns, templates, env));
        return k.apply(new SchemeValue.VoidVal());
    }

    private SchemeValue expandMacro(SchemeValue.SyntaxRulesVal macro, List<SchemeValue> inputElems,
                                     Environment env) throws EvalError {
        for (int i = 0; i < macro.patterns().size(); i++) {
            SchemeValue pattern = macro.patterns().get(i);
            SchemeValue template = macro.templates().get(i);
            if (!(pattern instanceof SchemeValue.ListVal patList)) continue;

            Map<String, SchemeValue> singles = new HashMap<>();
            Map<String, List<SchemeValue>> lists = new HashMap<>();
            if (matchPattern(patList.elements(), inputElems, 1, 1, macro.literals(), singles, lists)) {
                // Collect pattern variable names
                Set<String> patVars = new HashSet<>(singles.keySet());
                patVars.addAll(lists.keySet());

                // Collect free symbols in template for hygiene
                Set<String> freeSyms = new HashSet<>();
                collectFreeSymbols(template, patVars, freeSyms);

                // Generate renames for hygiene
                Map<String, String> renames = new HashMap<>();
                for (String sym : freeSyms) {
                    renames.put(sym, gensym(sym));
                }

                // Expand template
                SchemeValue expanded = expandTemplate(template, singles, lists, renames);

                // Pre-bind renamed symbols from definition env
                for (var entry : renames.entrySet()) {
                    try {
                        SchemeValue val = macro.defEnv().get(entry.getKey());
                        env.define(entry.getValue(), val);
                    } catch (EvalError ignored) {
                        // Not in definition env — introduced binding, skip
                    }
                }
                return expanded;
            }
        }
        throw new EvalError("No matching pattern for macro");
    }

    private boolean matchPattern(List<SchemeValue> patElems, List<SchemeValue> inputElems,
                                  int patStart, int inputStart, List<String> literals,
                                  Map<String, SchemeValue> singles, Map<String, List<SchemeValue>> lists) {
        int inputIdx = inputStart;
        for (int i = patStart; i < patElems.size(); i++) {
            SchemeValue pat = patElems.get(i);
            // Check if next element is ellipsis
            if (i + 1 < patElems.size() && isEllipsis(patElems.get(i + 1))) {
                if (!(pat instanceof SchemeValue.SymbolVal sym)) return false;
                List<SchemeValue> collected = new ArrayList<>();
                while (inputIdx < inputElems.size()) {
                    collected.add(inputElems.get(inputIdx++));
                }
                lists.put(sym.name(), collected);
                i++; // skip ellipsis
                continue;
            }
            if (inputIdx >= inputElems.size()) return false;
            if (pat instanceof SchemeValue.SymbolVal sym) {
                if (literals.contains(sym.name())) {
                    // Literal: must match exactly
                    if (!(inputElems.get(inputIdx) instanceof SchemeValue.SymbolVal inSym)
                        || !inSym.name().equals(sym.name())) return false;
                    inputIdx++;
                } else {
                    // Pattern variable
                    singles.put(sym.name(), inputElems.get(inputIdx));
                    inputIdx++;
                }
            } else if (pat instanceof SchemeValue.ListVal patNested) {
                if (!(inputElems.get(inputIdx) instanceof SchemeValue.ListVal inNested)) return false;
                if (!matchPattern(patNested.elements(), inNested.elements(), 0, 0, literals, singles, lists))
                    return false;
                inputIdx++;
            } else {
                // Literal value match
                inputIdx++;
            }
        }
        return inputIdx == inputElems.size();
    }

    private boolean isEllipsis(SchemeValue v) {
        return v instanceof SchemeValue.SymbolVal s && s.name().equals("...");
    }

    private SchemeValue expandTemplate(SchemeValue template, Map<String, SchemeValue> singles,
                                        Map<String, List<SchemeValue>> lists, Map<String, String> renames) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (singles.containsKey(name)) return singles.get(name);
            if (renames.containsKey(name)) return new SchemeValue.SymbolVal(renames.get(name));
            return template;
        }
        if (template instanceof SchemeValue.ListVal list) {
            List<SchemeValue> expanded = new ArrayList<>();
            List<SchemeValue> elems = list.elements();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && isEllipsis(elems.get(i + 1))) {
                    SchemeValue elem = elems.get(i);
                    if (elem instanceof SchemeValue.SymbolVal sym && lists.containsKey(sym.name())) {
                        expanded.addAll(lists.get(sym.name()));
                    }
                    i++; // skip ellipsis
                    continue;
                }
                expanded.add(expandTemplate(elems.get(i), singles, lists, renames));
            }
            return new SchemeValue.ListVal(expanded);
        }
        return template;
    }

    private void collectFreeSymbols(SchemeValue template, Set<String> patternVars, Set<String> result) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (!patternVars.contains(name) && !SPECIAL_FORMS.contains(name) && !name.equals("...")) {
                result.add(name);
            }
        } else if (template instanceof SchemeValue.ListVal list) {
            for (SchemeValue elem : list.elements()) {
                collectFreeSymbols(elem, patternVars, result);
            }
        }
    }

    private boolean schemeEqual(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.IntVal ia && b instanceof SchemeValue.IntVal ib)
            return ia.value() == ib.value();
        if (a instanceof SchemeValue.BoolVal ba && b instanceof SchemeValue.BoolVal bb)
            return ba.value() == bb.value();
        if (a instanceof SchemeValue.StringVal sa && b instanceof SchemeValue.StringVal sb)
            return sa.value().equals(sb.value());
        if (a instanceof SchemeValue.SymbolVal sa && b instanceof SchemeValue.SymbolVal sb)
            return sa.name().equals(sb.name());
        if (a instanceof SchemeValue.CharVal ca && b instanceof SchemeValue.CharVal cb)
            return ca.value() == cb.value();
        if (a instanceof SchemeValue.NilVal && b instanceof SchemeValue.NilVal)
            return true;
        if (a instanceof SchemeValue.PairVal pa && b instanceof SchemeValue.PairVal pb)
            return schemeEqual(pa.car(), pb.car()) && schemeEqual(pa.cdr(), pb.cdr());
        if (a instanceof SchemeValue.VectorVal va && b instanceof SchemeValue.VectorVal vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqual(va.get(i), vb.get(i))) return false;
            }
            return true;
        }
        return false;
    }

    private Bounce mapLoop(SchemeValue proc, List<SchemeValue> lists, List<SchemeValue> acc, SchemeValue.Cont k) {
        // Check if any list is empty
        for (SchemeValue lst : lists) {
            if (lst instanceof SchemeValue.NilVal) {
                // Build result list from acc
                SchemeValue result = SchemeValue.NIL;
                for (int i = acc.size() - 1; i >= 0; i--) {
                    result = new SchemeValue.PairVal(acc.get(i), result);
                }
                return k.apply(result);
            }
        }
        // Collect car of each list
        List<SchemeValue> callArgs = new ArrayList<>(lists.size());
        List<SchemeValue> nextLists = new ArrayList<>(lists.size());
        for (SchemeValue lst : lists) {
            if (!(lst instanceof SchemeValue.PairVal p)) {
                return new Bounce.Err(new EvalError("map: not a proper list"));
            }
            callArgs.add(p.car());
            nextLists.add(p.cdr());
        }
        return new Bounce.More(() -> applyProc(proc, callArgs, "map", val -> {
            List<SchemeValue> newAcc = new ArrayList<>(acc);
            newAcc.add(val);
            return mapLoop(proc, nextLists, newAcc, k);
        }));
    }

    private long requireInt(SchemeValue val) throws EvalError {
        if (val instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("Expected integer, got: " + val.display());
    }
}
