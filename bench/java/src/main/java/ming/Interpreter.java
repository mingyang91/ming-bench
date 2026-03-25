package ming;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;

public class Interpreter {
    private final Environment globals = new Environment();
    private static final SchemeValue NIL = new SchemeValue.ListVal(List.of());
    private IdentityHashMap<SchemeValue, SourcePos> positions;
    private final StringBuilder outputBuffer = new StringBuilder();

    public Interpreter() {
        registerBuiltins();
    }

    public void setPositions(IdentityHashMap<SchemeValue, SourcePos> positions) {
        this.positions = positions;
    }

    public String getOutput() {
        return outputBuffer.toString();
    }

    private void registerBuiltins() {
        globals.define("+", new SchemeValue.BuiltinVal("+", args -> {
            long result = 0;
            for (var arg : args) result += asLong(arg);
            return new SchemeValue.IntVal(result);
        }));
        globals.define("*", new SchemeValue.BuiltinVal("*", args -> {
            long result = 1;
            for (var arg : args) result *= asLong(arg);
            return new SchemeValue.IntVal(result);
        }));
        globals.define("-", new SchemeValue.BuiltinVal("-", args -> {
            if (args.length == 0) throw new EvalError("-: need at least one argument");
            if (args.length == 1) return new SchemeValue.IntVal(-asLong(args[0]));
            long result = asLong(args[0]);
            for (int i = 1; i < args.length; i++) result -= asLong(args[i]);
            return new SchemeValue.IntVal(result);
        }));
        globals.define("/", new SchemeValue.BuiltinVal("/", args -> {
            if (args.length == 0) throw new EvalError("/: need at least one argument");
            long result = asLong(args[0]);
            for (int i = 1; i < args.length; i++) {
                long d = asLong(args[i]);
                if (d == 0) throw new EvalError("division by zero");
                result /= d;
            }
            return new SchemeValue.IntVal(result);
        }));
        globals.define("<", new SchemeValue.BuiltinVal("<", args -> compare(args, (a, b) -> a < b)));
        globals.define(">", new SchemeValue.BuiltinVal(">", args -> compare(args, (a, b) -> a > b)));
        globals.define("=", new SchemeValue.BuiltinVal("=", args -> compare(args, (a, b) -> a == b)));
        globals.define("<=", new SchemeValue.BuiltinVal("<=", args -> compare(args, (a, b) -> a <= b)));
        globals.define(">=", new SchemeValue.BuiltinVal(">=", args -> compare(args, (a, b) -> a >= b)));
        globals.define("not", new SchemeValue.BuiltinVal("not", args -> {
            if (args.length != 1) throw new EvalError("not: expected 1 argument");
            return new SchemeValue.BoolVal(!args[0].isTruthy());
        }));

        // L03 builtins
        globals.define("cons", new SchemeValue.BuiltinVal("cons", args -> {
            if (args.length != 2) throw new EvalError("cons: expected 2 arguments");
            return new SchemeValue.PairVal(args[0], args[1]);
        }));
        globals.define("car", new SchemeValue.BuiltinVal("car", args -> {
            if (args.length != 1) throw new EvalError("car: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p) return p.car();
            throw new EvalError("car: not a pair: " + args[0].display());
        }));
        globals.define("cdr", new SchemeValue.BuiltinVal("cdr", args -> {
            if (args.length != 1) throw new EvalError("cdr: expected 1 argument");
            if (args[0] instanceof SchemeValue.PairVal p) return p.cdr();
            throw new EvalError("cdr: not a pair: " + args[0].display());
        }));
        globals.define("null?", new SchemeValue.BuiltinVal("null?", args -> {
            if (args.length != 1) throw new EvalError("null?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        }));
        globals.define("list", new SchemeValue.BuiltinVal("list", args -> {
            SchemeValue result = NIL;
            for (int i = args.length - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args[i], result);
            }
            return result;
        }));
        globals.define("length", new SchemeValue.BuiltinVal("length", args -> {
            if (args.length != 1) throw new EvalError("length: expected 1 argument");
            long count = 0;
            SchemeValue cur = args[0];
            while (cur instanceof SchemeValue.PairVal p) {
                count++;
                cur = p.cdr();
            }
            if (!(cur instanceof SchemeValue.ListVal l && l.elements().isEmpty())) {
                throw new EvalError("length: not a proper list");
            }
            return new SchemeValue.IntVal(count);
        }));

        globals.define("append", new SchemeValue.BuiltinVal("append", args -> {
            if (args.length == 0) return NIL;
            if (args.length == 1) return args[0];
            // Append all lists
            SchemeValue result = args[args.length - 1];
            for (int i = args.length - 2; i >= 0; i--) {
                result = appendTwo(args[i], result);
            }
            return result;
        }));

        // Type predicates
        globals.define("number?", new SchemeValue.BuiltinVal("number?", args -> {
            if (args.length != 1) throw new EvalError("number?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.IntVal);
        }));
        globals.define("string?", new SchemeValue.BuiltinVal("string?", args -> {
            if (args.length != 1) throw new EvalError("string?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.StringVal);
        }));
        globals.define("boolean?", new SchemeValue.BuiltinVal("boolean?", args -> {
            if (args.length != 1) throw new EvalError("boolean?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.BoolVal);
        }));
        globals.define("pair?", new SchemeValue.BuiltinVal("pair?", args -> {
            if (args.length != 1) throw new EvalError("pair?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.PairVal);
        }));
        globals.define("symbol?", new SchemeValue.BuiltinVal("symbol?", args -> {
            if (args.length != 1) throw new EvalError("symbol?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.SymbolVal);
        }));

        // L05 builtins
        globals.define("display", new SchemeValue.BuiltinVal("display", args -> {
            if (args.length != 1) throw new EvalError("display: expected 1 argument");
            outputBuffer.append(args[0].displayOutput());
            return new SchemeValue.VoidVal();
        }));
        globals.define("write", new SchemeValue.BuiltinVal("write", args -> {
            if (args.length != 1) throw new EvalError("write: expected 1 argument");
            outputBuffer.append(args[0].display());
            return new SchemeValue.VoidVal();
        }));
        globals.define("newline", new SchemeValue.BuiltinVal("newline", args -> {
            if (args.length != 0) throw new EvalError("newline: expected 0 arguments");
            outputBuffer.append("\n");
            return new SchemeValue.VoidVal();
        }));
        globals.define("string-append", new SchemeValue.BuiltinVal("string-append", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                if (!(arg instanceof SchemeValue.StringVal s))
                    throw new EvalError("string-append: expected string");
                sb.append(s.value());
            }
            return new SchemeValue.StringVal(sb.toString());
        }));
        globals.define("string-length", new SchemeValue.BuiltinVal("string-length", args -> {
            if (args.length != 1) throw new EvalError("string-length: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-length: expected string");
            return new SchemeValue.IntVal(s.value().length());
        }));
        globals.define("substring", new SchemeValue.BuiltinVal("substring", args -> {
            if (args.length != 3) throw new EvalError("substring: expected 3 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("substring: expected string");
            int start = (int) asLong(args[1]);
            int end = (int) asLong(args[2]);
            return new SchemeValue.StringVal(s.value().substring(start, end));
        }));
        globals.define("string->number", new SchemeValue.BuiltinVal("string->number", args -> {
            if (args.length != 1) throw new EvalError("string->number: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string->number: expected string");
            try {
                return new SchemeValue.IntVal(Long.parseLong(s.value()));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        }));
        globals.define("number->string", new SchemeValue.BuiltinVal("number->string", args -> {
            if (args.length != 1) throw new EvalError("number->string: expected 1 argument");
            return new SchemeValue.StringVal(Long.toString(asLong(args[0])));
        }));
        globals.define("symbol->string", new SchemeValue.BuiltinVal("symbol->string", args -> {
            if (args.length != 1) throw new EvalError("symbol->string: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.SymbolVal s))
                throw new EvalError("symbol->string: expected symbol");
            return new SchemeValue.StringVal(s.name());
        }));
        globals.define("string->symbol", new SchemeValue.BuiltinVal("string->symbol", args -> {
            if (args.length != 1) throw new EvalError("string->symbol: expected 1 argument");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string->symbol: expected string");
            return new SchemeValue.SymbolVal(s.value());
        }));
        globals.define("string-ref", new SchemeValue.BuiltinVal("string-ref", args -> {
            if (args.length != 2) throw new EvalError("string-ref: expected 2 arguments");
            if (!(args[0] instanceof SchemeValue.StringVal s))
                throw new EvalError("string-ref: expected string");
            int idx = (int) asLong(args[1]);
            return new SchemeValue.CharVal(s.value().charAt(idx));
        }));
        globals.define("char?", new SchemeValue.BuiltinVal("char?", args -> {
            if (args.length != 1) throw new EvalError("char?: expected 1 argument");
            return new SchemeValue.BoolVal(args[0] instanceof SchemeValue.CharVal);
        }));
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globals);
    }

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        try {
            return switch (expr) {
                case SchemeValue.IntVal v -> v;
                case SchemeValue.BoolVal v -> v;
                case SchemeValue.StringVal v -> v;
                case SchemeValue.VoidVal v -> v;
                case SchemeValue.LambdaVal v -> v;
                case SchemeValue.BuiltinVal v -> v;
                case SchemeValue.CharVal v -> v;
                case SchemeValue.PairVal v -> v;
                case SchemeValue.SymbolVal v -> env.get(v.name());
                case SchemeValue.ListVal v -> evalList(v, env);
            };
        } catch (EvalError e) {
            if (positions != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                var pos = positions.get(expr);
                if (pos != null) {
                    throw new EvalError(pos + ": " + e.getMessage());
                }
            }
            throw e;
        }
    }

    private SchemeValue evalList(SchemeValue.ListVal list, Environment env) throws EvalError {
        if (list.elements().isEmpty()) throw new EvalError("empty application");

        var first = list.elements().get(0);
        if (first instanceof SchemeValue.SymbolVal sym) {
            return switch (sym.name()) {
                case "define" -> evalDefine(list.elements(), env);
                case "if" -> evalIf(list.elements(), env);
                case "quote" -> evalQuote(list.elements());
                case "lambda" -> evalLambda(list.elements(), env);
                case "and" -> evalAnd(list.elements(), env);
                case "or" -> evalOr(list.elements(), env);
                case "let" -> evalLet(list.elements(), env);
                case "begin" -> evalBegin(list.elements(), env);
                case "cond" -> evalCond(list.elements(), env);
                default -> evalApplication(list.elements(), env);
            };
        }
        return evalApplication(list.elements(), env);
    }

    private SchemeValue evalDefine(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("define: bad syntax");
        var target = elements.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            var val = eval(elements.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty())
                throw new EvalError("define: bad syntax");
            if (!(nameAndParams.elements().get(0) instanceof SchemeValue.SymbolVal fnName))
                throw new EvalError("define: expected function name");
            var params = new ArrayList<String>();
            for (int i = 1; i < nameAndParams.elements().size(); i++) {
                if (!(nameAndParams.elements().get(i) instanceof SchemeValue.SymbolVal p))
                    throw new EvalError("define: expected parameter name");
                params.add(p.name());
            }
            var body = elements.subList(2, elements.size());
            var lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(fnName.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError("define: bad syntax");
    }

    private SchemeValue evalIf(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("if: bad syntax");
        var cond = eval(elements.get(1), env);
        if (cond.isTruthy()) {
            return eval(elements.get(2), env);
        } else if (elements.size() > 3) {
            return eval(elements.get(3), env);
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalQuote(List<SchemeValue> elements) throws EvalError {
        if (elements.size() != 2) throw new EvalError("quote: expected 1 argument");
        return quoteDatum(elements.get(1));
    }

    /** Convert reader ListVal to pair chains for quoted data. */
    private SchemeValue quoteDatum(SchemeValue datum) {
        if (datum instanceof SchemeValue.ListVal l) {
            if (l.elements().isEmpty()) return NIL;
            SchemeValue result = NIL;
            for (int i = l.elements().size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(quoteDatum(l.elements().get(i)), result);
            }
            return result;
        }
        return datum;
    }

    private SchemeValue evalLambda(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("lambda: bad syntax");
        var paramList = elements.get(1);
        if (!(paramList instanceof SchemeValue.ListVal pl))
            throw new EvalError("lambda: expected parameter list");
        var params = new ArrayList<String>();
        for (var p : pl.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym))
                throw new EvalError("lambda: expected parameter name");
            params.add(sym.name());
        }
        var body = elements.subList(2, elements.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    private SchemeValue evalAnd(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() == 1) return new SchemeValue.BoolVal(true);
        SchemeValue result = new SchemeValue.BoolVal(true);
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue evalOr(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() == 1) return new SchemeValue.BoolVal(false);
        for (int i = 1; i < elements.size(); i++) {
            var result = eval(elements.get(i), env);
            if (result.isTruthy()) return result;
        }
        return new SchemeValue.BoolVal(false);
    }

    private SchemeValue evalLet(List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError("let: bad syntax");

        // Named let: (let name ((var init) ...) body ...)
        if (elements.get(1) instanceof SchemeValue.SymbolVal loopName) {
            if (elements.size() < 4) throw new EvalError("let: bad syntax");
            if (!(elements.get(2) instanceof SchemeValue.ListVal bl))
                throw new EvalError("let: expected bindings list");
            var params = new ArrayList<String>();
            var initVals = new ArrayList<SchemeValue>();
            for (var binding : bl.elements()) {
                if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                    throw new EvalError("let: bad binding");
                if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                    throw new EvalError("let: expected symbol in binding");
                params.add(name.name());
                initVals.add(eval(b.elements().get(1), env));
            }
            var body = elements.subList(3, elements.size());
            var letEnv = new Environment(env);
            var lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(loopName.name(), lambda);
            // Call with initial values
            var callEnv = new Environment(letEnv);
            for (int i = 0; i < params.size(); i++) {
                callEnv.define(params.get(i), initVals.get(i));
            }
            SchemeValue result = new SchemeValue.VoidVal();
            for (var bodyExpr : body) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        // Regular let: (let ((var init) ...) body ...)
        var bindings = elements.get(1);
        if (!(bindings instanceof SchemeValue.ListVal bl))
            throw new EvalError("let: expected bindings list");
        var letEnv = new Environment(env);
        for (var binding : bl.elements()) {
            if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                throw new EvalError("let: bad binding");
            if (!(b.elements().get(0) instanceof SchemeValue.SymbolVal name))
                throw new EvalError("let: expected symbol in binding");
            var val = eval(b.elements().get(1), env);
            letEnv.define(name.name(), val);
        }
        SchemeValue result = new SchemeValue.VoidVal();
        for (int i = 2; i < elements.size(); i++) {
            result = eval(elements.get(i), letEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> elements, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.VoidVal();
        for (int i = 1; i < elements.size(); i++) {
            result = eval(elements.get(i), env);
        }
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> elements, Environment env) throws EvalError {
        for (int i = 1; i < elements.size(); i++) {
            var clause = elements.get(i);
            if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().isEmpty())
                throw new EvalError("cond: bad clause");
            var test = cl.elements().get(0);
            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                SchemeValue result = new SchemeValue.VoidVal();
                for (int j = 1; j < cl.elements().size(); j++) {
                    result = eval(cl.elements().get(j), env);
                }
                return result;
            }
            var testVal = eval(test, env);
            if (testVal.isTruthy()) {
                SchemeValue result = testVal;
                for (int j = 1; j < cl.elements().size(); j++) {
                    result = eval(cl.elements().get(j), env);
                }
                return result;
            }
        }
        return new SchemeValue.VoidVal();
    }

    private SchemeValue evalApplication(List<SchemeValue> elements, Environment env) throws EvalError {
        var proc = eval(elements.get(0), env);
        var args = new SchemeValue[elements.size() - 1];
        for (int i = 1; i < elements.size(); i++) {
            args[i - 1] = eval(elements.get(i), env);
        }

        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (args.length != lambda.params().size())
                throw new EvalError("wrong number of arguments: expected " + lambda.params().size() + ", got " + args.length);
            var callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args[i]);
            }
            SchemeValue result = new SchemeValue.VoidVal();
            for (var bodyExpr : lambda.body()) {
                result = eval(bodyExpr, callEnv);
            }
            return result;
        }

        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            return builtin.proc().apply(args);
        }

        throw new EvalError("not a procedure: " + proc.display());
    }

    @FunctionalInterface
    interface LongCmp { boolean test(long a, long b); }

    private SchemeValue compare(SchemeValue[] args, LongCmp cmp) throws EvalError {
        if (args.length < 2) throw new EvalError("comparison needs at least 2 arguments");
        for (int i = 0; i < args.length - 1; i++) {
            if (!cmp.test(asLong(args[i]), asLong(args[i + 1]))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private static SchemeValue appendTwo(SchemeValue a, SchemeValue b) throws EvalError {
        if (a instanceof SchemeValue.ListVal l && l.elements().isEmpty()) return b;
        if (a instanceof SchemeValue.PairVal p) {
            return new SchemeValue.PairVal(p.car(), appendTwo(p.cdr(), b));
        }
        throw new EvalError("append: not a proper list");
    }

    private static long asLong(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }
}
