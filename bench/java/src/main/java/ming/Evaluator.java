package ming;

import java.util.ArrayList;
import java.util.List;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private final StringBuilder outputBuffer = new StringBuilder();

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
            result = eval(expr, env);
        }
        if (result instanceof SchemeValue.VoidVal) return "#<void>";
        return result.display();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        var tokens = new Tokenizer(input).tokenize();
        var exprs = new Parser(tokens).parseAll();
        if (exprs.isEmpty()) throw new EvalError("No expressions");
        Environment env = createGlobalEnv();
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
            result = eval(expr, env);
        }
        String resultStr = (result instanceof SchemeValue.VoidVal) ? "#<void>" : result.display();
        return new EvalResult(resultStr, outputBuffer.toString());
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
            // Append all lists
            SchemeValue result = args.getLast();
            for (int i = args.size() - 2; i >= 0; i--) {
                result = appendTwo(args.get(i), result);
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
        env.define("number->string", new SchemeValue.BuiltinVal("number->string", args -> {
            return new SchemeValue.StringVal(String.valueOf(requireInt(args.getFirst())));
        }));
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
            return new SchemeValue.StringVal(s.value());
        }));
        env.define("string-set!", new SchemeValue.BuiltinVal("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set! requires 3 arguments");
            if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw new EvalError("string-set!: not a string");
            int idx = (int) requireInt(args.get(1));
            if (!(args.get(2) instanceof SchemeValue.CharVal c)) throw new EvalError("string-set!: not a character");
            s.setCharAt(idx, c.value());
            return new SchemeValue.VoidVal();
        }));
    }

    private SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.NilVal) return b;
        if (a instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        }
        throw new EvalError("append: not a proper list");
    }

    private SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.CharVal v -> v;
            case SchemeValue.VoidVal v -> v;
            case SchemeValue.NilVal v -> v;
            case SchemeValue.PairVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.BuiltinVal v -> v;
            case SchemeValue.SymbolVal v -> {
                try { yield env.get(v.name()); }
                catch (EvalError e) { throw new EvalError("Unbound variable: " + v.name() + " at " + v.line() + ":" + v.col()); }
            }
            case SchemeValue.ListVal v -> evalList(v, env);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal listVal, Environment env) throws EvalError {
        List<SchemeValue> elems = listVal.elements();
        String pos = listVal.line() + ":" + listVal.col();
        if (elems.isEmpty()) throw new EvalError("Empty application at " + pos);
        SchemeValue head = elems.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            switch (name) {
                case "and": return evalAnd(elems, env);
                case "or": return evalOr(elems, env);
                case "if": return evalIf(elems, env, pos);
                case "define": return evalDefine(elems, env, pos);
                case "quote": return evalQuote(elems, pos);
                case "lambda": return evalLambda(elems, env, pos);
                case "let": return evalLet(elems, env, pos);
                case "begin": return evalBegin(elems, env);
                case "cond": return evalCond(elems, env);
                default: break;
            }
        }
        // Procedure call: evaluate all, then apply
        SchemeValue proc = eval(head, env);
        List<SchemeValue> args = new ArrayList<>();
        for (int i = 1; i < elems.size(); i++) {
            args.add(eval(elems.get(i), env));
        }
        return apply(proc, args, pos);
    }

    private SchemeValue apply(SchemeValue proc, List<SchemeValue> args, String pos) throws EvalError {
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            try {
                return builtin.func().apply(args);
            } catch (EvalError e) {
                String msg = e.getMessage();
                if (!msg.matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(msg + " at " + pos);
                }
                throw e;
            }
        }
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (lambda.params().size() != args.size()) {
                throw new EvalError("Expected " + lambda.params().size() + " arguments, got " + args.size() + " at " + pos);
            }
            Environment callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            SchemeValue result = null;
            for (SchemeValue bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }
        throw new EvalError("Not a procedure: " + proc.display() + " at " + pos);
    }

    private SchemeValue evalIf(List<SchemeValue> elems, Environment env, String pos) throws EvalError {
        if (elems.size() < 3 || elems.size() > 4) throw new EvalError("if requires 2 or 3 arguments at " + pos);
        SchemeValue cond = eval(elems.get(1), env);
        if (cond.isTruthy()) {
            return eval(elems.get(2), env);
        } else if (elems.size() == 4) {
            return eval(elems.get(3), env);
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalDefine(List<SchemeValue> elems, Environment env, String pos) throws EvalError {
        if (elems.size() < 3) throw new EvalError("define requires at least 2 arguments at " + pos);
        SchemeValue target = elems.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            SchemeValue val = eval(elems.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            List<SchemeValue> parts = nameAndParams.elements();
            if (parts.isEmpty()) throw new EvalError("define: empty name list");
            if (!(parts.getFirst() instanceof SchemeValue.SymbolVal fnName)) {
                throw new EvalError("define: expected symbol as function name");
            }
            List<String> params = new ArrayList<>();
            for (int i = 1; i < parts.size(); i++) {
                if (!(parts.get(i) instanceof SchemeValue.SymbolVal p)) {
                    throw new EvalError("define: expected symbol as parameter");
                }
                params.add(p.name());
            }
            List<SchemeValue> body = elems.subList(2, elems.size());
            SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(fnName.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError("define: invalid syntax");
    }

    private SchemeValue evalQuote(List<SchemeValue> elems, String pos) throws EvalError {
        if (elems.size() != 2) throw new EvalError("quote requires exactly 1 argument at " + pos);
        return quoteDatum(elems.get(1));
    }

    /** Convert parsed syntax (ListVal) into runtime data (PairVal/NilVal). */
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

    private SchemeValue evalLambda(List<SchemeValue> elems, Environment env, String pos) throws EvalError {
        if (elems.size() < 3) throw new EvalError("lambda requires parameters and body at " + pos);
        SchemeValue paramSpec = elems.get(1);
        if (!(paramSpec instanceof SchemeValue.ListVal paramList)) {
            throw new EvalError("lambda: expected parameter list");
        }
        List<String> params = new ArrayList<>();
        for (SchemeValue p : paramList.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym)) {
                throw new EvalError("lambda: expected symbol as parameter");
            }
            params.add(sym.name());
        }
        List<SchemeValue> body = elems.subList(2, elems.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    private SchemeValue evalLet(List<SchemeValue> elems, Environment env, String pos) throws EvalError {
        if (elems.size() < 3) throw new EvalError("let requires bindings and body at " + pos);
        // Named let: (let name ((var init) ...) body...)
        if (elems.get(1) instanceof SchemeValue.SymbolVal nameSym) {
            if (elems.size() < 4) throw new EvalError("named let requires bindings and body");
            String name = nameSym.name();
            if (!(elems.get(2) instanceof SchemeValue.ListVal bindingsList)) {
                throw new EvalError("let: expected bindings list");
            }
            List<String> params = new ArrayList<>();
            List<SchemeValue> inits = new ArrayList<>();
            for (SchemeValue b : bindingsList.elements()) {
                if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2) {
                    throw new EvalError("let: invalid binding");
                }
                if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s)) {
                    throw new EvalError("let: expected symbol in binding");
                }
                params.add(s.name());
                inits.add(eval(binding.elements().get(1), env));
            }
            List<SchemeValue> body = elems.subList(3, elems.size());
            Environment letEnv = new Environment(env);
            SchemeValue.LambdaVal lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(name, lambda);
            return apply(lambda, inits, pos);
        }
        // Regular let: (let ((var init) ...) body...)
        if (!(elems.get(1) instanceof SchemeValue.ListVal bindingsList)) {
            throw new EvalError("let: expected bindings list");
        }
        Environment letEnv = new Environment(env);
        for (SchemeValue b : bindingsList.elements()) {
            if (!(b instanceof SchemeValue.ListVal binding) || binding.elements().size() != 2) {
                throw new EvalError("let: invalid binding");
            }
            if (!(binding.elements().get(0) instanceof SchemeValue.SymbolVal s)) {
                throw new EvalError("let: expected symbol in binding");
            }
            SchemeValue val = eval(binding.elements().get(1), env);
            letEnv.define(s.name(), val);
        }
        SchemeValue result = new SchemeValue.VoidVal();
        for (int i = 2; i < elems.size(); i++) {
            result = eval(elems.get(i), letEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> elems, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.VoidVal();
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
        }
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> elems, Environment env) throws EvalError {
        for (int i = 1; i < elems.size(); i++) {
            if (!(elems.get(i) instanceof SchemeValue.ListVal clause)) {
                throw new EvalError("cond: expected clause");
            }
            List<SchemeValue> parts = clause.elements();
            if (parts.isEmpty()) throw new EvalError("cond: empty clause");
            // Check for else clause
            if (parts.getFirst() instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
                SchemeValue result = new SchemeValue.VoidVal();
                for (int j = 1; j < parts.size(); j++) {
                    result = eval(parts.get(j), env);
                }
                return result;
            }
            SchemeValue test = eval(parts.getFirst(), env);
            if (test.isTruthy()) {
                SchemeValue result = test;
                for (int j = 1; j < parts.size(); j++) {
                    result = eval(parts.get(j), env);
                }
                return result;
            }
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalAnd(List<SchemeValue> elems, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elems, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false);
        for (int i = 1; i < elems.size(); i++) {
            result = eval(elems.get(i), env);
            if (result.isTruthy()) return result;
        }
        return result;
    }

    private long requireInt(SchemeValue val) throws EvalError {
        if (val instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("Expected integer, got: " + val.display());
    }

    private static String posOf(SchemeValue v) {
        if (v instanceof SchemeValue.ListVal l) return l.line() + ":" + l.col();
        if (v instanceof SchemeValue.SymbolVal s) return s.line() + ":" + s.col();
        return "0:0";
    }
}
