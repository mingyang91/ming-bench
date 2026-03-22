package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

public class Interpreter {
    private final Environment globalEnv = new Environment();
    private final StringBuilder outputBuffer = new StringBuilder();

    // Continuation support
    private int nextContId = 0;
    final Map<Integer, ContData> continuationData = new HashMap<>();
    final IdentityHashMap<SchemeValue, SchemeValue> pendingReturns = new IdentityHashMap<>();
    int topLevelIndex = 0;
    // Tracks the outermost let body for continuation restart.
    // Only set once (first let encountered); inner lets don't overwrite it.
    private List<SchemeValue> outerLetBody;
    private int outerLetBodyIndex;
    private Environment outerLetEnv;

    static class ContData {
        final SchemeValue callccExpr;
        final List<SchemeValue> letBody;
        final int letBodyIndex;
        final Environment letBodyEnv;
        final int topLevelIndex;

        ContData(SchemeValue callccExpr, List<SchemeValue> letBody, int letBodyIndex, Environment letBodyEnv, int topLevelIndex) {
            this.callccExpr = callccExpr;
            this.letBody = letBody;
            this.letBodyIndex = letBodyIndex;
            this.letBodyEnv = letBodyEnv;
            this.topLevelIndex = topLevelIndex;
        }
    }

    public Interpreter() {
        registerBuiltins();
    }

    public String getOutput() {
        return outputBuffer.toString();
    }

    private void registerBuiltins() {
        builtin("+", args -> arithPlus(args));
        builtin("-", args -> arithMinus(args));
        builtin("*", args -> arithMul(args));
        builtin("/", args -> arithDiv(args));
        builtin("<", args -> compare(args, (a, b) -> a < b));
        builtin(">", args -> compare(args, (a, b) -> a > b));
        builtin("=", args -> compare(args, (a, b) -> a == b));
        builtin("<=", args -> compare(args, (a, b) -> a <= b));
        builtin(">=", args -> compare(args, (a, b) -> a >= b));
        builtin("not", args -> {
            if (args.size() != 1) throw new EvalError("not: expected 1 argument, got " + args.size());
            return new SchemeValue.BoolVal(!args.getFirst().isTruthy());
        });
        builtin("cons", args -> {
            if (args.size() != 2) throw new EvalError("cons: expected 2 arguments");
            return new SchemeValue.PairVal(args.get(0), args.get(1));
        });
        builtin("car", args -> {
            if (args.size() != 1) throw new EvalError("car: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.PairVal p) return p.car();
            if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) return l.elements().getFirst();
            throw new EvalError("car: expected pair, got: " + v.display());
        });
        builtin("cdr", args -> {
            if (args.size() != 1) throw new EvalError("cdr: expected 1 argument");
            SchemeValue v = args.getFirst();
            if (v instanceof SchemeValue.PairVal p) return p.cdr();
            if (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()) {
                return new SchemeValue.ListVal(l.elements().subList(1, l.elements().size()));
            }
            throw new EvalError("cdr: expected pair, got: " + v.display());
        });
        builtin("null?", args -> {
            if (args.size() != 1) throw new EvalError("null?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.ListVal l && l.elements().isEmpty());
        });
        builtin("list", args -> {
            if (args.isEmpty()) return new SchemeValue.ListVal(List.of());
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = args.size() - 1; i >= 0; i--) {
                result = new SchemeValue.PairVal(args.get(i), result);
            }
            return result;
        });
        builtin("length", args -> {
            if (args.size() != 1) throw new EvalError("length: expected 1 argument");
            SchemeValue v = args.getFirst();
            long count = 0;
            while (true) {
                if (v instanceof SchemeValue.ListVal l) {
                    count += l.elements().size();
                    break;
                } else if (v instanceof SchemeValue.PairVal p) {
                    count++;
                    v = p.cdr();
                } else {
                    throw new EvalError("length: expected list");
                }
            }
            return new SchemeValue.IntVal(count);
        });
        builtin("append", args -> {
            SchemeValue result = new SchemeValue.ListVal(List.of());
            for (int i = args.size() - 1; i >= 0; i--) {
                SchemeValue lst = args.get(i);
                var elems = new ArrayList<SchemeValue>();
                while (true) {
                    if (lst instanceof SchemeValue.PairVal p) {
                        elems.add(p.car());
                        lst = p.cdr();
                    } else if (lst instanceof SchemeValue.ListVal l) {
                        elems.addAll(l.elements());
                        break;
                    } else {
                        break;
                    }
                }
                for (int j = elems.size() - 1; j >= 0; j--) {
                    result = new SchemeValue.PairVal(elems.get(j), result);
                }
            }
            return result;
        });
        builtin("string?", args -> {
            if (args.size() != 1) throw new EvalError("string?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.StringVal || args.getFirst() instanceof SchemeValue.MutableStringVal);
        });
        builtin("number?", args -> {
            if (args.size() != 1) throw new EvalError("number?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.IntVal);
        });
        builtin("boolean?", args -> {
            if (args.size() != 1) throw new EvalError("boolean?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.BoolVal);
        });
        builtin("pair?", args -> {
            if (args.size() != 1) throw new EvalError("pair?: expected 1 argument");
            SchemeValue v = args.getFirst();
            return new SchemeValue.BoolVal(v instanceof SchemeValue.PairVal ||
                (v instanceof SchemeValue.ListVal l && !l.elements().isEmpty()));
        });
        builtin("symbol?", args -> {
            if (args.size() != 1) throw new EvalError("symbol?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.SymbolVal);
        });
        builtin("char?", args -> {
            if (args.size() != 1) throw new EvalError("char?: expected 1 argument");
            return new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.CharVal);
        });
        // Display/Write/Newline
        builtin("display", args -> {
            if (args.size() != 1) throw new EvalError("display: expected 1 argument");
            outputBuffer.append(args.getFirst().displayStr());
            return new SchemeValue.VoidVal();
        });
        builtin("write", args -> {
            if (args.size() != 1) throw new EvalError("write: expected 1 argument");
            outputBuffer.append(args.getFirst().display());
            return new SchemeValue.VoidVal();
        });
        builtin("newline", args -> {
            if (!args.isEmpty()) throw new EvalError("newline: expected 0 arguments");
            outputBuffer.append('\n');
            return new SchemeValue.VoidVal();
        });
        // String operations
        builtin("string-append", args -> {
            var sb = new StringBuilder();
            for (var arg : args) {
                sb.append(requireString(arg, "string-append"));
            }
            return new SchemeValue.StringVal(sb.toString());
        });
        builtin("string-length", args -> {
            if (args.size() != 1) throw new EvalError("string-length: expected 1 argument");
            return new SchemeValue.IntVal(requireString(args.getFirst(), "string-length").length());
        });
        builtin("substring", args -> {
            if (args.size() != 3) throw new EvalError("substring: expected 3 arguments");
            String str = requireString(args.get(0), "substring");
            long start = requireInt(args.get(1));
            long end = requireInt(args.get(2));
            return new SchemeValue.StringVal(str.substring((int) start, (int) end));
        });
        builtin("string->number", args -> {
            if (args.size() != 1) throw new EvalError("string->number: expected 1 argument");
            String str = requireString(args.getFirst(), "string->number");
            try {
                return new SchemeValue.IntVal(Long.parseLong(str));
            } catch (NumberFormatException e) {
                return new SchemeValue.BoolVal(false);
            }
        });
        builtin("number->string", args -> {
            if (args.size() != 1) throw new EvalError("number->string: expected 1 argument");
            return new SchemeValue.StringVal(String.valueOf(requireInt(args.getFirst())));
        });
        builtin("symbol->string", args -> {
            if (args.size() != 1) throw new EvalError("symbol->string: expected 1 argument");
            if (!(args.getFirst() instanceof SchemeValue.SymbolVal s)) throw new EvalError("symbol->string: expected symbol");
            return new SchemeValue.StringVal(s.name());
        });
        builtin("string->symbol", args -> {
            if (args.size() != 1) throw new EvalError("string->symbol: expected 1 argument");
            return new SchemeValue.SymbolVal(requireString(args.getFirst(), "string->symbol"));
        });
        builtin("string-ref", args -> {
            if (args.size() != 2) throw new EvalError("string-ref: expected 2 arguments");
            String str = requireString(args.get(0), "string-ref");
            long idx = requireInt(args.get(1));
            return new SchemeValue.CharVal(str.charAt((int) idx));
        });
        builtin("string-copy", args -> {
            if (args.size() != 1) throw new EvalError("string-copy: expected 1 argument");
            String str = requireString(args.getFirst(), "string-copy");
            return new SchemeValue.MutableStringVal(new StringBuilder(str));
        });
        builtin("apply", args -> {
            if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
            SchemeValue proc = args.getFirst();
            // Last arg must be a list; prefix args are prepended
            List<SchemeValue> lastList = toJavaList(args.getLast());
            var allArgs = new ArrayList<SchemeValue>();
            for (int i = 1; i < args.size() - 1; i++) {
                allArgs.add(args.get(i));
            }
            allArgs.addAll(lastList);
            if (proc instanceof SchemeValue.ContinuationVal cont) {
                if (allArgs.size() != 1) throw new EvalError("continuation: expected 1 argument");
                throw new ContinuationException(cont.id(), allArgs.getFirst());
            }
            if (proc instanceof SchemeValue.LambdaVal lambda) {
                var localEnv = applyLambda(lambda, allArgs, "");
                SchemeValue result = null;
                for (var bodyExpr : lambda.body()) {
                    result = eval(bodyExpr, localEnv);
                }
                return result;
            } else if (proc instanceof SchemeValue.BuiltinVal builtin) {
                return builtin.fn().apply(allArgs);
            }
            throw new EvalError("apply: not a procedure: " + proc.display());
        });
        // call/cc registered as builtins for first-class usage; actual logic handled specially in eval
        globalEnv.define("call/cc", new SchemeValue.BuiltinVal("call/cc", args -> { throw new RuntimeException("call/cc: internal error"); }));
        globalEnv.define("call-with-current-continuation", new SchemeValue.BuiltinVal("call-with-current-continuation", args -> { throw new RuntimeException("internal error"); }));
        builtin("string-set!", args -> {
            if (args.size() != 3) throw new EvalError("string-set!: expected 3 arguments");
            if (!(args.get(0) instanceof SchemeValue.MutableStringVal ms)) {
                throw new EvalError("string-set!: expected mutable string");
            }
            long idx = requireInt(args.get(1));
            if (!(args.get(2) instanceof SchemeValue.CharVal ch)) {
                throw new EvalError("string-set!: expected char as third argument");
            }
            ms.chars().setCharAt((int) idx, ch.value());
            return new SchemeValue.VoidVal();
        });
    }

    @FunctionalInterface
    interface CheckedFunction {
        SchemeValue apply(List<SchemeValue> args) throws EvalError;
    }

    private void builtin(String name, CheckedFunction fn) {
        globalEnv.define(name, new SchemeValue.BuiltinVal(name, args -> {
            try {
                return fn.apply(args);
            } catch (EvalError e) {
                throw new RuntimeException(e);
            }
        }));
    }

    private static String posPrefix(SchemeValue expr) {
        SourcePos p = expr.pos();
        return p != null ? p + ": " : "";
    }

    public SchemeValue eval(SchemeValue expr) throws EvalError {
        return eval(expr, globalEnv);
    }

    public SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        while (true) {
            switch (expr) {
                case SchemeValue.IntVal v -> { return v; }
                case SchemeValue.BoolVal v -> { return v; }
                case SchemeValue.StringVal v -> { return v; }
                case SchemeValue.LambdaVal v -> { return v; }
                case SchemeValue.BuiltinVal v -> { return v; }
                case SchemeValue.PairVal v -> { return v; }
                case SchemeValue.VoidVal v -> { return v; }
                case SchemeValue.CharVal v -> { return v; }
                case SchemeValue.MutableStringVal v -> { return v; }
                case SchemeValue.ContinuationVal v -> { return v; }
                case SchemeValue.SyntaxRulesVal v -> { return v; }
                case SchemeValue.SymbolVal v -> {
                    try {
                        return env.get(v.name());
                    } catch (EvalError e) {
                        throw new EvalError(posPrefix(expr) + e.getMessage());
                    }
                }
                case SchemeValue.ListVal listVal -> {
                    List<SchemeValue> elements = listVal.elements();
                    if (elements.isEmpty()) {
                        throw new EvalError(posPrefix(listVal) + "empty application");
                    }
                    SchemeValue head = elements.getFirst();
                    if (head instanceof SchemeValue.SymbolVal sym) {
                        String name = sym.name();
                        switch (name) {
                            case "and" -> {
                                if (elements.size() == 1) return new SchemeValue.BoolVal(true);
                                for (int i = 1; i < elements.size() - 1; i++) {
                                    SchemeValue result = eval(elements.get(i), env);
                                    if (!result.isTruthy()) return result;
                                }
                                expr = elements.getLast();
                                continue;
                            }
                            case "or" -> {
                                if (elements.size() == 1) return new SchemeValue.BoolVal(false);
                                for (int i = 1; i < elements.size() - 1; i++) {
                                    SchemeValue result = eval(elements.get(i), env);
                                    if (result.isTruthy()) return result;
                                }
                                expr = elements.getLast();
                                continue;
                            }
                            case "define" -> { return evalDefine(listVal, elements, env); }
                            case "set!" -> {
                                if (elements.size() != 3) throw new EvalError(posPrefix(listVal) + "set!: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.SymbolVal sym2))
                                    throw new EvalError(posPrefix(listVal) + "set!: expected symbol");
                                SchemeValue val = eval(elements.get(2), env);
                                env.set(sym2.name(), val);
                                return new SchemeValue.VoidVal();
                            }
                            case "if" -> {
                                if (elements.size() < 3 || elements.size() > 4) throw new EvalError(posPrefix(listVal) + "if: bad syntax");
                                SchemeValue cond = eval(elements.get(1), env);
                                if (cond.isTruthy()) {
                                    expr = elements.get(2);
                                } else if (elements.size() == 4) {
                                    expr = elements.get(3);
                                } else {
                                    return new SchemeValue.BoolVal(false);
                                }
                                continue;
                            }
                            case "quote" -> {
                                if (elements.size() != 2) throw new EvalError(posPrefix(listVal) + "quote: expected 1 argument");
                                return elements.get(1);
                            }
                            case "lambda" -> { return evalLambda(listVal, elements, env); }
                            case "let" -> {
                                if (elements.size() < 3) throw new EvalError("let: bad syntax");
                                if (elements.get(1) instanceof SchemeValue.SymbolVal nameSym) {
                                    // Named let
                                    if (elements.size() < 4) throw new EvalError("let: bad syntax");
                                    if (!(elements.get(2) instanceof SchemeValue.ListVal bl)) throw new EvalError("let: expected bindings list");
                                    var params = new ArrayList<String>();
                                    var inits = new ArrayList<SchemeValue>();
                                    for (var binding : bl.elements()) {
                                        if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                            throw new EvalError("let: bad binding");
                                        if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                            throw new EvalError("let: expected symbol in binding");
                                        params.add(s.name());
                                        inits.add(eval(b.elements().get(1), env));
                                    }
                                    var body = elements.subList(3, elements.size());
                                    var localEnv = new Environment(env);
                                    var lambda = new SchemeValue.LambdaVal(params, null, body, localEnv);
                                    localEnv.define(nameSym.name(), lambda);
                                    // TCO: set up apply inline
                                    for (int i = 0; i < params.size(); i++) {
                                        localEnv.define(params.get(i), inits.get(i));
                                    }
                                    for (int i = 0; i < body.size() - 1; i++) {
                                        eval(body.get(i), localEnv);
                                    }
                                    expr = body.getLast();
                                    env = localEnv;
                                    continue;
                                }
                                // Regular let
                                if (!(elements.get(1) instanceof SchemeValue.ListVal bl)) throw new EvalError("let: expected bindings list");
                                var localEnv = new Environment(env);
                                for (var binding : bl.elements()) {
                                    if (!(binding instanceof SchemeValue.ListVal b) || b.elements().size() != 2)
                                        throw new EvalError("let: bad binding");
                                    if (!(b.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                                        throw new EvalError("let: expected symbol in binding");
                                    localEnv.define(s.name(), eval(b.elements().get(1), env));
                                }
                                var letBody = elements.subList(2, elements.size());
                                boolean isOuterLet = (outerLetBody == null);
                                if (isOuterLet) {
                                    outerLetBody = letBody;
                                    outerLetEnv = localEnv;
                                }
                                for (int i = 0; i < letBody.size() - 1; i++) {
                                    if (isOuterLet) outerLetBodyIndex = i;
                                    eval(letBody.get(i), localEnv);
                                }
                                if (isOuterLet) outerLetBodyIndex = letBody.size() - 1;
                                expr = letBody.getLast();
                                env = localEnv;
                                continue;
                            }
                            case "begin" -> {
                                if (elements.size() < 2) throw new EvalError("begin: empty body");
                                for (int i = 1; i < elements.size() - 1; i++) {
                                    eval(elements.get(i), env);
                                }
                                expr = elements.getLast();
                                continue;
                            }
                            case "call/cc", "call-with-current-continuation" -> {
                                SchemeValue pending = pendingReturns.remove(listVal);
                                if (pending != null) return pending;
                                if (elements.size() != 2) throw new EvalError(posPrefix(listVal) + "call/cc: expected 1 argument");
                                SchemeValue proc = eval(elements.get(1), env);
                                return handleCallCC(proc, listVal);
                            }
                            case "define-syntax" -> {
                                if (elements.size() != 3) throw new EvalError(posPrefix(listVal) + "define-syntax: bad syntax");
                                if (!(elements.get(1) instanceof SchemeValue.SymbolVal nameSym))
                                    throw new EvalError(posPrefix(listVal) + "define-syntax: expected symbol");
                                SchemeValue transformer = evalSyntaxRules(elements.get(2), env);
                                env.define(nameSym.name(), transformer);
                                return new SchemeValue.VoidVal();
                            }
                            case "cond" -> {
                                boolean matched = false;
                                for (int i = 1; i < elements.size(); i++) {
                                    if (!(elements.get(i) instanceof SchemeValue.ListVal clause) || clause.elements().isEmpty())
                                        throw new EvalError("cond: bad clause");
                                    SchemeValue test = clause.elements().getFirst();
                                    if (test instanceof SchemeValue.SymbolVal s && s.name().equals("else")) {
                                        for (int j = 1; j < clause.elements().size() - 1; j++) {
                                            eval(clause.elements().get(j), env);
                                        }
                                        expr = clause.elements().getLast();
                                        matched = true;
                                        break;
                                    }
                                    SchemeValue testResult = eval(test, env);
                                    if (testResult.isTruthy()) {
                                        if (clause.elements().size() == 1) return testResult;
                                        for (int j = 1; j < clause.elements().size() - 1; j++) {
                                            eval(clause.elements().get(j), env);
                                        }
                                        expr = clause.elements().getLast();
                                        matched = true;
                                        break;
                                    }
                                }
                                if (matched) continue;
                                return new SchemeValue.BoolVal(false);
                            }
                            default -> {
                                // fall through to procedure call below
                            }
                        }
                    }
                    // Evaluate head and call as procedure
                    SchemeValue proc = eval(head, env);
                    // Macro expansion
                    if (proc instanceof SchemeValue.SyntaxRulesVal macro) {
                        expr = MacroExpander.expand(macro, listVal);
                        continue;
                    }
                    var args = new ArrayList<SchemeValue>();
                    for (int i = 1; i < elements.size(); i++) {
                        args.add(eval(elements.get(i), env));
                    }
                    // Handle continuation invocation
                    if (proc instanceof SchemeValue.ContinuationVal cont) {
                        if (args.size() != 1) throw new EvalError(posPrefix(listVal) + "continuation: expected 1 argument");
                        throw new ContinuationException(cont.id(), args.getFirst());
                    }
                    // Handle call/cc used as first-class value
                    if (proc instanceof SchemeValue.BuiltinVal bv &&
                            (bv.name().equals("call/cc") || bv.name().equals("call-with-current-continuation"))) {
                        if (args.size() != 1) throw new EvalError(posPrefix(listVal) + "call/cc: expected 1 argument");
                        return handleCallCC(args.getFirst(), listVal);
                    }
                    // TCO for lambda calls
                    if (proc instanceof SchemeValue.LambdaVal lambda) {
                        var localEnv = applyLambda(lambda, args, posPrefix(listVal));
                        for (int i = 0; i < lambda.body().size() - 1; i++) {
                            eval(lambda.body().get(i), localEnv);
                        }
                        expr = lambda.body().getLast();
                        env = localEnv;
                        continue;
                    }
                    if (proc instanceof SchemeValue.BuiltinVal builtin) {
                        try {
                            return builtin.fn().apply(args);
                        } catch (RuntimeException e) {
                            if (e.getCause() instanceof EvalError ee) {
                                String msg = ee.getMessage();
                                if (listVal.pos() != null && !msg.matches(".*\\d+:\\d+.*")) {
                                    throw new EvalError(posPrefix(listVal) + msg);
                                }
                                throw ee;
                            }
                            throw e;
                        }
                    }
                    throw new EvalError(posPrefix(listVal) + "not a procedure: " + proc.display());
                }
            }
        }
    }


    private SchemeValue evalSyntaxRules(SchemeValue srExpr, Environment env) throws EvalError {
        if (!(srExpr instanceof SchemeValue.ListVal list)) throw new EvalError("syntax-rules: bad syntax");
        var elems = list.elements();
        if (elems.isEmpty() || !(elems.getFirst() instanceof SchemeValue.SymbolVal s) || !s.name().equals("syntax-rules"))
            throw new EvalError("syntax-rules: bad syntax");
        if (elems.size() < 2) throw new EvalError("syntax-rules: bad syntax");
        var literals = new ArrayList<String>();
        if (elems.get(1) instanceof SchemeValue.ListVal litList) {
            for (var lit : litList.elements()) {
                if (lit instanceof SchemeValue.SymbolVal ls) literals.add(ls.name());
            }
        }
        var patterns = new ArrayList<SchemeValue>();
        var templates = new ArrayList<SchemeValue>();
        for (int i = 2; i < elems.size(); i++) {
            if (!(elems.get(i) instanceof SchemeValue.ListVal rule) || rule.elements().size() != 2)
                throw new EvalError("syntax-rules: bad rule");
            patterns.add(rule.elements().get(0));
            templates.add(rule.elements().get(1));
        }
        return new SchemeValue.SyntaxRulesVal(literals, patterns, templates, env);
    }

    private SchemeValue evalDefine(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
        SchemeValue target = elements.get(1);
        if (target instanceof SchemeValue.SymbolVal sym) {
            SchemeValue val = eval(elements.get(2), env);
            env.define(sym.name(), val);
            return new SchemeValue.VoidVal();
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            if (nameAndParams.elements().isEmpty()) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
            SchemeValue nameVal = nameAndParams.elements().getFirst();
            if (!(nameVal instanceof SchemeValue.SymbolVal nameSym)) {
                throw new EvalError(posPrefix(listVal) + "define: expected symbol as function name");
            }
            var params = new ArrayList<String>();
            String restParam = null;
            var pElems = nameAndParams.elements();
            for (int i = 1; i < pElems.size(); i++) {
                if (pElems.get(i) instanceof SchemeValue.SymbolVal s && s.name().equals(".")) {
                    if (i + 1 >= pElems.size()) throw new EvalError(posPrefix(listVal) + "define: bad syntax");
                    if (!(pElems.get(i + 1) instanceof SchemeValue.SymbolVal rp))
                        throw new EvalError(posPrefix(listVal) + "define: expected symbol after dot");
                    restParam = rp.name();
                    break;
                }
                if (!(pElems.get(i) instanceof SchemeValue.SymbolVal p)) {
                    throw new EvalError(posPrefix(listVal) + "define: expected symbol as parameter");
                }
                params.add(p.name());
            }
            var body = elements.subList(2, elements.size());
            var lambda = new SchemeValue.LambdaVal(params, restParam, body, env);
            env.define(nameSym.name(), lambda);
            return new SchemeValue.VoidVal();
        }
        throw new EvalError(posPrefix(listVal) + "define: bad syntax");
    }


    private SchemeValue evalLambda(SchemeValue.ListVal listVal, List<SchemeValue> elements, Environment env) throws EvalError {
        if (elements.size() < 3) throw new EvalError(posPrefix(listVal) + "lambda: bad syntax");
        SchemeValue paramList = elements.get(1);
        // Single symbol = all-rest parameter: (lambda args ...)
        if (paramList instanceof SchemeValue.SymbolVal restSym) {
            var body = elements.subList(2, elements.size());
            return new SchemeValue.LambdaVal(List.of(), restSym.name(), body, env);
        }
        if (!(paramList instanceof SchemeValue.ListVal pList)) {
            throw new EvalError(posPrefix(listVal) + "lambda: expected parameter list");
        }
        var params = new ArrayList<String>();
        String restParam = null;
        for (int i = 0; i < pList.elements().size(); i++) {
            var p = pList.elements().get(i);
            if (p instanceof SchemeValue.SymbolVal s && s.name().equals(".")) {
                if (i + 1 >= pList.elements().size()) throw new EvalError(posPrefix(listVal) + "lambda: bad syntax");
                if (!(pList.elements().get(i + 1) instanceof SchemeValue.SymbolVal rp))
                    throw new EvalError(posPrefix(listVal) + "lambda: expected symbol after dot");
                restParam = rp.name();
                break;
            }
            if (!(p instanceof SchemeValue.SymbolVal sym)) {
                throw new EvalError(posPrefix(listVal) + "lambda: expected symbol as parameter");
            }
            params.add(sym.name());
        }
        var body = elements.subList(2, elements.size());
        return new SchemeValue.LambdaVal(params, restParam, body, env);
    }


    private Environment applyLambda(SchemeValue.LambdaVal lambda, List<SchemeValue> args, String errPrefix) throws EvalError {
        int nFixed = lambda.params().size();
        if (lambda.restParam() != null) {
            if (args.size() < nFixed) {
                throw new EvalError(errPrefix + "expected at least " + nFixed + " arguments, got " + args.size());
            }
        } else {
            if (args.size() != nFixed) {
                throw new EvalError(errPrefix + "expected " + nFixed + " arguments, got " + args.size());
            }
        }
        var localEnv = new Environment(lambda.env());
        for (int i = 0; i < nFixed; i++) {
            localEnv.define(lambda.params().get(i), args.get(i));
        }
        if (lambda.restParam() != null) {
            localEnv.define(lambda.restParam(), schemeList(args.subList(nFixed, args.size())));
        }
        return localEnv;
    }

    private SchemeValue schemeList(List<SchemeValue> elems) {
        SchemeValue result = new SchemeValue.ListVal(List.of());
        for (int i = elems.size() - 1; i >= 0; i--) {
            result = new SchemeValue.PairVal(elems.get(i), result);
        }
        return result;
    }

    private List<SchemeValue> toJavaList(SchemeValue v) throws EvalError {
        var result = new ArrayList<SchemeValue>();
        while (true) {
            if (v instanceof SchemeValue.ListVal l) {
                result.addAll(l.elements());
                return result;
            } else if (v instanceof SchemeValue.PairVal p) {
                result.add(p.car());
                v = p.cdr();
            } else {
                throw new EvalError("apply: expected proper list");
            }
        }
    }

    private SchemeValue arithPlus(List<SchemeValue> args) throws EvalError {
        long result = 0;
        for (var arg : args) result += requireInt(arg);
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithMinus(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("-: expected at least 1 argument");
        if (args.size() == 1) return new SchemeValue.IntVal(-requireInt(args.getFirst()));
        long result = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) result -= requireInt(args.get(i));
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithMul(List<SchemeValue> args) throws EvalError {
        long result = 1;
        for (var arg : args) result *= requireInt(arg);
        return new SchemeValue.IntVal(result);
    }

    private SchemeValue arithDiv(List<SchemeValue> args) throws EvalError {
        if (args.isEmpty()) throw new EvalError("/: expected at least 1 argument");
        long result = requireInt(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(args.get(i));
            if (divisor == 0) throw new EvalError("division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result);
    }

    @FunctionalInterface
    interface LongBiPredicate {
        boolean test(long a, long b);
    }

    private SchemeValue compare(List<SchemeValue> args, LongBiPredicate pred) throws EvalError {
        if (args.size() < 2) throw new EvalError("comparison: expected at least 2 arguments");
        for (int i = 0; i < args.size() - 1; i++) {
            if (!pred.test(requireInt(args.get(i)), requireInt(args.get(i + 1)))) {
                return new SchemeValue.BoolVal(false);
            }
        }
        return new SchemeValue.BoolVal(true);
    }

    private long requireInt(SchemeValue v) throws EvalError {
        if (v instanceof SchemeValue.IntVal iv) return iv.value();
        throw new EvalError("expected number, got: " + v.display());
    }

    private String requireString(SchemeValue v, String caller) throws EvalError {
        if (v instanceof SchemeValue.StringVal s) return s.value();
        if (v instanceof SchemeValue.MutableStringVal s) return s.value();
        throw new EvalError(caller + ": expected string");
    }

    private SchemeValue handleCallCC(SchemeValue proc, SchemeValue callccExpr) throws EvalError {
        int contId = nextContId++;
        var contVal = new SchemeValue.ContinuationVal(contId);

        // Store continuation data — capture outermost let body context
        continuationData.put(contId, new ContData(callccExpr, outerLetBody, outerLetBodyIndex, outerLetEnv, topLevelIndex));

        // Call the thunk with the continuation
        try {
            if (proc instanceof SchemeValue.LambdaVal lambda) {
                var localEnv = applyLambda(lambda, List.of(contVal), "");
                SchemeValue result = null;
                for (var bodyExpr : lambda.body()) {
                    result = eval(bodyExpr, localEnv);
                }
                return result;
            } else if (proc instanceof SchemeValue.BuiltinVal builtin) {
                return builtin.fn().apply(List.of(contVal));
            }
            throw new EvalError("call/cc: expected procedure, got: " + proc.display());
        } catch (ContinuationException e) {
            if (e.contId == contId) {
                return e.value; // escape continuation
            }
            throw e;
        }
    }

    /**
     * Restart a let body from a given index (used for reentrant continuations).
     * Called from Evaluator when a ContinuationException targets a let body.
     */
    SchemeValue restartLetBody(List<SchemeValue> body, int startIndex, Environment env) throws EvalError {
        int idx = startIndex;
        while (true) {
            try {
                SchemeValue result = null;
                for (int i = idx; i < body.size(); i++) {
                    outerLetBody = body;
                    outerLetBodyIndex = i;
                    outerLetEnv = env;
                    result = eval(body.get(i), env);
                }
                return result != null ? result : new SchemeValue.VoidVal();
            } catch (ContinuationException e) {
                var cont = continuationData.get(e.contId);
                if (cont != null && cont.letBody == body) {
                    pendingReturns.put(cont.callccExpr, e.value);
                    idx = cont.letBodyIndex;
                } else {
                    throw e;
                }
            }
        }
    }
}
