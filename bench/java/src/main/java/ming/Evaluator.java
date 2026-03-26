package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import static ming.SchemeFormatter.schemeToString;
import static ming.SchemeReader.deepUnwrap;
import static ming.SchemeReader.unwrap;

public class Evaluator {

    private final Environment globalEnv = new Environment(null);
    private final Map<String, String[]> recordTypes = new HashMap<>();
    private StringBuilder outputBuffer = null;

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "not",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
        "integer?", "rational?", "exact?", "inexact?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
        "display", "write", "newline",
        "string-append", "string-length", "substring", "string-ref",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-copy", "string-set!", "string->list", "list->string",
        "apply",
        "eq?", "eqv?", "equal?",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc", "map",
        "char-alphabetic?", "char-numeric?", "char=?", "char<?",
        "char-upcase", "char-downcase", "char->integer", "integer->char",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
        "vector", "make-vector", "vector-ref", "vector-set!", "vector-length",
        "vector?", "vector->list", "list->vector",
        "procedure?",
        "set-car!", "set-cdr!",
        "for-each",
        "caar", "cadr", "cdar", "cddr", "caddr", "cadar", "caddar",
        "caaar", "caadr", "cdaar", "cdadr", "cddar", "cdddr",
        "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "cadddr",
        "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
        "reverse", "memq", "memv", "member", "assq", "assv",
        "gcd", "lcm", "truncate", "round",
        "make-string", "string", "string>?", "string<=?", "string>=?",
        "syntax->datum", "datum->syntax"
    };

    {
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new BuiltinProcedure(name));
        }
        globalEnv.define("call/cc", new BuiltinProcedure("call/cc"));
        globalEnv.define("call-with-current-continuation", new BuiltinProcedure("call/cc"));
        globalEnv.define("raise", new BuiltinProcedure("raise"));
        globalEnv.define("with-exception-handler", new BuiltinProcedure("with-exception-handler"));
        globalEnv.define("values", new BuiltinProcedure("values"));
        globalEnv.define("call-with-values", new BuiltinProcedure("call-with-values"));
    }

    private final SchemeReader reader = new SchemeReader();

    // Top-level expression list and current index, used for continuation capture
    private List<Object> topLevelExprs;
    private int topLevelIndex;

    public String evalStr(String input) throws EvalError {
        var tokens = reader.tokenize(input);
        int[] pos = {0};
        List<Object> exprs = new ArrayList<>();
        while (pos[0] < tokens.size()) {
            exprs.add(reader.parse(tokens, pos));
        }
        if (exprs.isEmpty()) throw new EvalError("no expression");
        Object lastResult = evalTopLevel(exprs, globalEnv);
        return schemeToString(lastResult);
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer = new StringBuilder();
        try {
            var tokens = reader.tokenize(input);
            int[] pos = {0};
            List<Object> exprs = new ArrayList<>();
            while (pos[0] < tokens.size()) {
                exprs.add(reader.parse(tokens, pos));
            }
            if (exprs.isEmpty()) throw new EvalError("no expression");
            Object lastResult = evalTopLevel(exprs, globalEnv);
            return new EvalResult(schemeToString(lastResult), outputBuffer.toString());
        } finally {
            outputBuffer = null;
        }
    }

    // Pending continuation re-entry
    private ContinuationException pendingContinuation = null;

    private Object evalTopLevel(List<Object> exprs, Environment env) throws EvalError {
        topLevelExprs = exprs;
        Object lastResult = null;
        topLevelIndex = 0;
        while (true) {
            try {
                // Check if there's a pending continuation to replay
                if (pendingContinuation != null) {
                    ContinuationException ce = pendingContinuation;
                    pendingContinuation = null;
                    SchemeContinuation cont = continuationRegistry.get(ce.continuationId);
                    if (cont == null) throw new EvalError("invalid continuation");
                    // Clean up any leftover frame stack state
                    bodyFrameStack.clear();

                    boolean sameExpr = (throwTopLevelIndex == cont.captureTopLevelIndex)
                        && cont.frameStack != null && !cont.frameStack.isEmpty()
                        && cont.frameStack.get(cont.frameStack.size() - 1).exprs != null;
                    if (sameExpr) {
                        // Frame-stack replay: process from innermost to outermost
                        List<BodyFrame> frames = cont.frameStack;

                        // Set up bodyFrameStack with all frames except innermost
                        for (int fi = 0; fi < frames.size() - 1; fi++) {
                            BodyFrame f = frames.get(fi);
                            bodyFrameStack.add(new BodyFrame(f.exprs, f.index, f.env));
                        }

                        // Set activeContinuationId for the target call/cc
                        activeContinuationId = ce.continuationId;
                        activeContinuationValue = ce.value;

                        // Process innermost frame (contains the target call/cc)
                        BodyFrame innermost = frames.get(frames.size() - 1);
                        currentBodyExprs = innermost.exprs;
                        currentBodyEnv = innermost.env;
                        Object bodyResult = null;
                        for (int bi = innermost.index; bi < innermost.exprs.size(); bi++) {
                            currentBodyIndex = bi;
                            bodyResult = eval(innermost.exprs.get(bi), innermost.env);
                        }

                        // Process outer frames from innermost to outermost
                        for (int fi = frames.size() - 2; fi >= 0; fi--) {
                            if (!bodyFrameStack.isEmpty()) {
                                bodyFrameStack.remove(bodyFrameStack.size() - 1);
                            }
                            BodyFrame frame = frames.get(fi);
                            if (frame.exprs == null) continue;
                            currentBodyExprs = frame.exprs;
                            currentBodyEnv = frame.env;
                            for (int bi = frame.index + 1; bi < frame.exprs.size(); bi++) {
                                currentBodyIndex = bi;
                                bodyResult = eval(frame.exprs.get(bi), frame.env);
                            }
                        }

                        exprs = cont.remainingTopLevel;
                        topLevelExprs = exprs;
                        topLevelIndex = 0;
                        lastResult = bodyResult;
                    } else {
                        // Top-level replay (saved continuation invoked from outside)
                        activeContinuationValue = ce.value;
                        activeContinuationId = ce.continuationId;
                        List<Object> replay = new ArrayList<>();
                        if (cont.topLevelExpr != null) {
                            replay.add(cont.topLevelExpr);
                        }
                        replay.addAll(cont.remainingTopLevel);
                        exprs = replay;
                        topLevelExprs = exprs;
                        topLevelIndex = 0;
                        lastResult = null;
                    }
                }
                // Normal evaluation loop
                while (topLevelIndex < exprs.size()) {
                    int idx = topLevelIndex;
                    lastResult = eval(exprs.get(idx), env);
                    topLevelIndex = idx + 1;
                }
                return lastResult;
            } catch (ContinuationException ce) {
                bodyFrameStack.clear();
                throwTopLevelIndex = topLevelIndex;
                pendingContinuation = ce;
            } catch (SchemeRaiseException re) {
                throw new EvalError("unhandled exception: " + schemeToString(re.value));
            }
        }
    }

    private int throwTopLevelIndex = -1;

    // Registry of all continuations by ID
    private final Map<Long, SchemeContinuation> continuationRegistry = new HashMap<>();
    // When replaying a continuation, these are set so call/cc knows to return the value
    private Long activeContinuationId = null;
    private Object activeContinuationValue = null;
    // Track which call/cc sites are currently active (within their dynamic extent)
    private final java.util.Set<Long> activeContinuationSites = new java.util.HashSet<>();

    // Current body context tracking (for continuation capture)
    private List<Object> currentBodyExprs = null;
    private int currentBodyIndex = 0;
    private Environment currentBodyEnv = null;

    // Body frame stack: outer body contexts saved when entering nested body forms
    static final class BodyFrame {
        List<Object> exprs;
        int index;
        Environment env;
        BodyFrame(List<Object> exprs, int index, Environment env) {
            this.exprs = exprs; this.index = index; this.env = env;
        }
    }
    private final List<BodyFrame> bodyFrameStack = new ArrayList<>();

    // Macro expansion cache (by list identity) to avoid re-expanding in tight loops
    private final Map<Object, Object> macroExpansionCache = new IdentityHashMap<>();

    // dynamic-wind stack: each entry is {inThunk, outThunk}
    private final List<Object[]> windStack = new ArrayList<>();

    private final SyntaxCaseHandler syntaxCase = new SyntaxCaseHandler(this::eval);

    // --- Evaluator ---

    static final Object VOID = new Object() {
        @Override public String toString() { return "#<void>"; }
    };

    // Trampoline marker for tail call optimization
    private static final class TailCall {
        Object expr;
        Environment env;
        boolean hasBodyFrame; // true if a body form left a frame on bodyFrameStack
        TailCall(Object expr, Environment env) {
            this.expr = expr;
            this.env = env;
        }
        TailCall(Object expr, Environment env, boolean hasBodyFrame) {
            this.expr = expr;
            this.env = env;
            this.hasBodyFrame = hasBodyFrame;
        }
    }

    // Resolve a TailCall chain (trampoline)
    private Object trampoline(Object result) throws EvalError, ContinuationException, SchemeRaiseException {
        int framesToPop = 0;
        try {
            while (result instanceof TailCall tc) {
                if (tc.hasBodyFrame) framesToPop++;
                result = evalStep(tc.expr, tc.env);
                // If continuing the chain, pop frames for TCO (don't restore currentBodyExprs
                // yet — body context should persist through the tail-call chain)
                if (result instanceof TailCall) {
                    while (framesToPop > 0 && !bodyFrameStack.isEmpty()) {
                        bodyFrameStack.remove(bodyFrameStack.size() - 1);
                        framesToPop--;
                    }
                }
            }
            return result;
        } finally {
            // Restore currentBodyExprs when trampoline exits
            while (framesToPop > 0 && !bodyFrameStack.isEmpty()) {
                BodyFrame popped = bodyFrameStack.remove(bodyFrameStack.size() - 1);
                currentBodyExprs = popped.exprs;
                currentBodyIndex = popped.index;
                currentBodyEnv = popped.env;
                framesToPop--;
            }
        }
    }

    // Apply that fully resolves (for non-tail contexts like map, builtin apply)
    private Object applyResolved(Object proc, List<Object> args) throws EvalError, ContinuationException, SchemeRaiseException {
        return trampoline(apply(proc, args));
    }

    // Convert a parsed Java List to a Scheme list (SchemePair chain ending in NIL)
    private Object javaListToSchemeList(List<?> javaList) {
        Object result = SchemeNil.INSTANCE;
        for (int i = javaList.size() - 1; i >= 0; i--) {
            Object elem = unwrap(javaList.get(i));
            if (elem instanceof List<?> subList) {
                elem = javaListToSchemeList(subList);
            }
            result = new SchemePair(elem, result);
        }
        return result;
    }

    // eval() is the public trampoline entry point
    @SuppressWarnings("unchecked")
    private Object eval(Object expr, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        return trampoline(evalStep(expr, env));
    }

    // evalStep does one step of evaluation; returns TailCall for tail positions
    @SuppressWarnings("unchecked")
    private Object evalStep(Object expr, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        // Unwrap Located and add position to any errors
        if (expr instanceof SchemeReader.Located loc) {
            try {
                return evalStep(loc.expr(), env);
            } catch (EvalError e) {
                if (e.getMessage() != null && !e.getMessage().matches(".*\\d+:\\d+.*")) {
                    throw new EvalError(e.getMessage() + " [" + loc.line() + ":" + loc.col() + "]");
                }
                throw e;
            }
        }

        if (expr instanceof Long || expr instanceof Double || expr instanceof SchemeRational || expr instanceof Boolean || expr instanceof SchemeChar) {
            return expr;
        }
        if (expr instanceof String s) {
            return s;
        }
        if (expr instanceof SchemeString ss) {
            return ss;
        }
        if (expr instanceof SchemeSymbol sym) {
            return env.lookup(sym.name());
        }
        if (expr instanceof List<?> rawList) {
            List<Object> list = (List<Object>) rawList;
            if (list.isEmpty()) {
                throw new EvalError("empty application");
            }
            Object first = list.get(0);
            Object rawFirst = unwrap(first);

            // Special forms
            if (rawFirst instanceof SchemeSymbol sym) {
                String name = sym.name();
                List<Object> args = list.subList(1, list.size());

                switch (name) {
                    case "quote" -> {
                        if (args.size() != 1) throw new EvalError("quote: expected 1 argument");
                        Object quoted = unwrap(args.get(0));
                        if (quoted instanceof List<?> ql) {
                            return javaListToSchemeList(ql);
                        }
                        return quoted;
                    }
                    case "if" -> {
                        if (args.size() < 2 || args.size() > 3)
                            throw new EvalError("if: expected 2 or 3 arguments");
                        Object cond = eval(args.get(0), env);
                        if (!cond.equals(Boolean.FALSE)) {
                            return new TailCall(args.get(1), env);
                        } else if (args.size() == 3) {
                            return new TailCall(args.get(2), env);
                        }
                        return VOID;
                    }
                    case "define" -> {
                        return evalDefine(args, env);
                    }
                    case "set!" -> {
                        if (args.size() != 2) throw new EvalError("set!: bad syntax");
                        Object target = unwrap(args.get(0));
                        if (!(target instanceof SchemeSymbol s))
                            throw new EvalError("set!: not a variable");
                        Object val = eval(args.get(1), env);
                        env.set(s.name(), val);
                        return VOID;
                    }
                    case "lambda" -> {
                        if (args.size() < 2) throw new EvalError("lambda: bad syntax");
                        return parseLambda(args, env);
                    }
                    case "case-lambda" -> {
                        if (args.isEmpty()) throw new EvalError("case-lambda: bad syntax");
                        List<SchemeLambda> clauses = new ArrayList<>();
                        for (Object clause : args) {
                            List<?> clauseList = (List<?>) unwrap(clause);
                            if (clauseList.size() < 2) throw new EvalError("case-lambda: bad clause");
                            List<Object> clauseArgs = new ArrayList<>(clauseList);
                            clauses.add(parseLambda(clauseArgs, env));
                        }
                        return new SchemeCaseLambda(clauses);
                    }
                    case "begin" -> {
                        // Push body frame for continuation capture
                        bodyFrameStack.add(new BodyFrame(currentBodyExprs, currentBodyIndex, currentBodyEnv));
                        currentBodyExprs = args;
                        currentBodyEnv = env;
                        boolean tailReturn = false;
                        try {
                            for (int i = 0; i < args.size() - 1; i++) {
                                currentBodyIndex = i;
                                eval(args.get(i), env);
                            }
                            if (!args.isEmpty()) {
                                currentBodyIndex = args.size() - 1;
                                tailReturn = true;
                                return new TailCall(args.get(args.size() - 1), env, true);
                            }
                            return VOID;
                        } finally {
                            if (!tailReturn) {
                                BodyFrame prev = bodyFrameStack.remove(bodyFrameStack.size() - 1);
                                currentBodyExprs = prev.exprs;
                                currentBodyIndex = prev.index;
                                currentBodyEnv = prev.env;
                            }
                        }
                    }
                    case "let" -> {
                        return evalLet(args, env);
                    }
                    case "let*" -> {
                        return evalLetStar(args, env);
                    }
                    case "letrec" -> {
                        return evalLetrec(args, env);
                    }
                    case "letrec*" -> {
                        return evalLetrecStar(args, env);
                    }
                    case "case" -> {
                        return evalCase(args, env);
                    }
                    case "do" -> {
                        return evalDo(args, env);
                    }
                    case "cond" -> {
                        return evalCond(args, env);
                    }
                    // Builtins handled as special forms (unevaluated args for and/or)
                    case "and" -> {
                        if (args.isEmpty()) return Boolean.TRUE;
                        for (int i = 0; i < args.size() - 1; i++) {
                            Object result = eval(args.get(i), env);
                            if (result.equals(Boolean.FALSE)) return Boolean.FALSE;
                        }
                        return new TailCall(args.get(args.size() - 1), env);
                    }
                    case "or" -> {
                        if (args.isEmpty()) return Boolean.FALSE;
                        for (int i = 0; i < args.size() - 1; i++) {
                            Object result = eval(args.get(i), env);
                            if (!result.equals(Boolean.FALSE)) return result;
                        }
                        return new TailCall(args.get(args.size() - 1), env);
                    }
                    case "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
                         "not",
                         "cons", "car", "cdr", "null?", "list", "length", "append",
                         "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
                         "integer?", "rational?", "exact?", "inexact?",
                         "exact->inexact", "inexact->exact",
                         "numerator", "denominator",
                         "display", "write", "newline",
                         "string-append", "string-length", "substring", "string-ref",
                         "string->number", "number->string",
                         "symbol->string", "string->symbol",
                         "string-copy", "string-set!", "string->list", "list->string",
                         "apply",
                         "eq?", "eqv?", "equal?",
                         "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                         "zero?", "positive?", "negative?", "odd?", "even?",
                         "list-ref", "list-tail", "list?", "assoc", "map",
                         "char-alphabetic?", "char-numeric?", "char=?", "char<?",
                         "char-upcase", "char-downcase", "char->integer", "integer->char",
                         "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
                         "vector", "make-vector", "vector-ref", "vector-set!", "vector-length",
        "vector?", "vector->list", "list->vector",
        "procedure?",
        "set-car!", "set-cdr!",
        "for-each",
        "caar", "cadr", "cdar", "cddr", "caddr", "cadar", "caddar",
        "caaar", "caadr", "cdaar", "cdadr", "cddar", "cdddr",
        "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "cadddr",
        "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
        "reverse", "memq", "memv", "member", "assq", "assv",
        "gcd", "lcm", "truncate", "round",
        "make-string", "string", "string>?", "string<=?", "string>=?",
        "syntax->datum", "datum->syntax" -> {
                        return evalBuiltin(name, args, env);
                    }
                    case "define-record-type" -> {
                        return evalDefineRecordType(args, env);
                    }
                    case "call/cc", "call-with-current-continuation" -> {
                        return evalCallCC(args, env);
                    }
                    case "dynamic-wind" -> {
                        if (args.size() != 3) throw new EvalError("dynamic-wind: expected 3 arguments");
                        Object inThunk = eval(args.get(0), env);
                        Object bodyThunk = eval(args.get(1), env);
                        Object outThunk = eval(args.get(2), env);
                        return evalDynamicWind(inThunk, bodyThunk, outThunk);
                    }
                    case "guard" -> {
                        return evalGuard(args, env);
                    }
                    case "define-syntax" -> {
                        if (args.size() != 2) throw new EvalError("define-syntax: bad syntax");
                        String macroName = ((SchemeSymbol) unwrap(args.get(0))).name();
                        Object rhs = unwrap(args.get(1));
                        if (rhs instanceof List<?> srForm) {
                            Object srHead = unwrap(srForm.get(0));
                            if (srHead instanceof SchemeSymbol ss && ss.name().equals("syntax-rules")) {
                                List<?> litList = (List<?>) unwrap(srForm.get(1));
                                List<String> literals = new ArrayList<>();
                                for (Object lit : litList)
                                    literals.add(((SchemeSymbol) unwrap(lit)).name());
                                List<Object[]> rules = new ArrayList<>();
                                for (int i = 2; i < srForm.size(); i++) {
                                    List<?> rule = (List<?>) unwrap(srForm.get(i));
                                    rules.add(new Object[]{deepUnwrap(rule.get(0)), deepUnwrap(rule.get(1))});
                                }
                                env.define(macroName, new SyntaxRules(literals, rules, env));
                                return VOID;
                            }
                        }
                        // Evaluate as expression (lambda transformer)
                        Object transformer = eval(args.get(1), env);
                        env.define(macroName, new SyntaxCaseHandler.MacroTransformer(transformer, env));
                        return VOID;
                    }
                    case "syntax-case" -> {
                        return syntaxCase.evalSyntaxCase(args, env);
                    }
                    case "syntax" -> {
                        return syntaxCase.evalSyntax(args, env);
                    }
                    case "with-syntax" -> {
                        return syntaxCase.evalWithSyntax(args, env);
                    }
                    default -> {
                        // Check if this is a macro call
                        try {
                            Object val = env.lookup(name);
                            if (val instanceof SyntaxRules sr) {
                                Object cached = macroExpansionCache.get(list);
                                if (cached != null) return new TailCall(cached, env);
                                @SuppressWarnings("unchecked")
                                List<Object> deepForm = (List<Object>) deepUnwrap(list);
                                Object expanded = sr.expand(deepForm, env);
                                macroExpansionCache.put(list, expanded);
                                return new TailCall(expanded, env);
                            }
                            if (val instanceof SyntaxCaseHandler.MacroTransformer mt) {
                                @SuppressWarnings("unchecked")
                                List<Object> deepForm = (List<Object>) deepUnwrap(list);
                                Object syntaxObj = new SyntaxObject(deepForm, env);
                                Environment prevDefEnv = syntaxCase.currentMacroDefEnv;
                                syntaxCase.currentMacroDefEnv = mt.defEnv();
                                try {
                                    Object result = applyResolved(mt.transformer(), List.of(syntaxObj));
                                    if (result instanceof SyntaxObject so) result = so.datum();
                                    return new TailCall(result, env);
                                } finally {
                                    syntaxCase.currentMacroDefEnv = prevDefEnv;
                                }
                            }
                        } catch (EvalError ignored) {}
                        // Fall through to procedure call
                    }
                }
            }

            // Procedure call: evaluate all, then apply
            Object proc = eval(first, env);
            List<Object> evaledArgs = new ArrayList<>();
            for (int i = 1; i < list.size(); i++) {
                evaledArgs.add(eval(list.get(i), env));
            }
            return apply(proc, evaledArgs);
        }
        throw new EvalError("cannot evaluate: " + expr);
    }

    @SuppressWarnings("unchecked")
    private Object evalLet(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("let: bad syntax");
        Object first2 = unwrap(args.get(0));
        if (first2 instanceof SchemeSymbol loopName) {
            if (args.size() < 3) throw new EvalError("let: bad syntax");
            Object bindingsObj = unwrap(args.get(1));
            List<?> bindings = (List<?>) bindingsObj;
            List<String> params = new ArrayList<>();
            List<Object> inits = new ArrayList<>();
            for (Object binding : bindings) {
                List<?> b = (List<?>) unwrap(binding);
                params.add(((SchemeSymbol) unwrap(b.get(0))).name());
                inits.add(eval(b.get(1), env));
            }
            Object body = wrapBodyInBegin(args, 2);
            Environment letEnv = new Environment(env);
            SchemeLambda loopLam = new SchemeLambda(params, body, letEnv);
            letEnv.define(loopName.name(), loopLam);
            return apply(loopLam, inits);  // returns TailCall, resolved by trampoline
        }
        List<?> bindings = (List<?>) first2;
        Environment letEnv = new Environment(env);
        for (Object binding : bindings) {
            List<?> b = (List<?>) unwrap(binding);
            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
            Object val = eval(b.get(1), env);
            letEnv.define(varName, val);
        }
        // Track body context for continuation capture via frame stack
        List<Object> bodyExprs = args.subList(1, args.size());
        bodyFrameStack.add(new BodyFrame(currentBodyExprs, currentBodyIndex, currentBodyEnv));
        currentBodyExprs = bodyExprs;
        currentBodyEnv = letEnv;
        boolean tailReturn = false;
        try {
            for (int i = 0; i < bodyExprs.size() - 1; i++) {
                currentBodyIndex = i;
                eval(bodyExprs.get(i), letEnv);
            }
            if (!bodyExprs.isEmpty()) {
                currentBodyIndex = bodyExprs.size() - 1;
                tailReturn = true;
                return new TailCall(bodyExprs.get(bodyExprs.size() - 1), letEnv, true);
            }
            return VOID;
        } finally {
            if (!tailReturn) {
                BodyFrame prev = bodyFrameStack.remove(bodyFrameStack.size() - 1);
                currentBodyExprs = prev.exprs;
                currentBodyIndex = prev.index;
                currentBodyEnv = prev.env;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private Object evalLetStar(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("let*: bad syntax");
        List<?> bindings = (List<?>) unwrap(args.get(0));
        Environment letEnv = new Environment(env);
        for (Object binding : bindings) {
            List<?> b = (List<?>) unwrap(binding);
            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
            Object val = eval(b.get(1), letEnv);
            letEnv.define(varName, val);
        }
        for (int i = 1; i < args.size() - 1; i++) {
            eval(args.get(i), letEnv);
        }
        if (args.size() > 1) {
            return new TailCall(args.get(args.size() - 1), letEnv);
        }
        return VOID;
    }

    @SuppressWarnings("unchecked")
    private Object evalLetrec(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("letrec: bad syntax");
        List<?> bindings = (List<?>) unwrap(args.get(0));
        Environment letEnv = new Environment(env);
        List<String> varNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        for (Object binding : bindings) {
            List<?> b = (List<?>) unwrap(binding);
            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
            varNames.add(varName);
            initExprs.add(b.get(1));
            letEnv.define(varName, VOID);
        }
        List<Object> vals = new ArrayList<>();
        for (Object initExpr : initExprs) {
            vals.add(eval(initExpr, letEnv));
        }
        for (int i = 0; i < varNames.size(); i++) {
            letEnv.set(varNames.get(i), vals.get(i));
        }
        for (int i = 1; i < args.size() - 1; i++) {
            eval(args.get(i), letEnv);
        }
        if (args.size() > 1) {
            return new TailCall(args.get(args.size() - 1), letEnv);
        }
        return VOID;
    }

    @SuppressWarnings("unchecked")
    private Object evalLetrecStar(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("letrec*: bad syntax");
        List<?> bindings = (List<?>) unwrap(args.get(0));
        Environment letEnv = new Environment(env);
        for (Object binding : bindings) {
            List<?> b = (List<?>) unwrap(binding);
            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
            letEnv.define(varName, VOID);
        }
        for (Object binding : bindings) {
            List<?> b = (List<?>) unwrap(binding);
            String varName = ((SchemeSymbol) unwrap(b.get(0))).name();
            Object val = eval(b.get(1), letEnv);
            letEnv.set(varName, val);
        }
        for (int i = 1; i < args.size() - 1; i++) {
            eval(args.get(i), letEnv);
        }
        if (args.size() > 1) {
            return new TailCall(args.get(args.size() - 1), letEnv);
        }
        return VOID;
    }

    private Object evalCase(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.isEmpty()) throw new EvalError("case: bad syntax");
        Object key = eval(args.get(0), env);
        for (int i = 1; i < args.size(); i++) {
            List<?> clause = (List<?>) unwrap(args.get(i));
            if (clause.isEmpty()) throw new EvalError("case: empty clause");
            Object datums = unwrap(clause.get(0));
            if (datums instanceof SchemeSymbol s && s.name().equals("else")) {
                for (int j = 1; j < clause.size() - 1; j++) {
                    eval(clause.get(j), env);
                }
                if (clause.size() > 1) {
                    return new TailCall(clause.get(clause.size() - 1), env);
                }
                return VOID;
            }
            List<?> datumList = (List<?>) datums;
            for (Object datum : datumList) {
                Object d = unwrap(datum);
                if (schemeEqv(key, d)) {
                    for (int j = 1; j < clause.size() - 1; j++) {
                        eval(clause.get(j), env);
                    }
                    if (clause.size() > 1) {
                        return new TailCall(clause.get(clause.size() - 1), env);
                    }
                    return VOID;
                }
            }
        }
        return VOID;
    }

    private Object evalDo(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("do: bad syntax");
        List<?> varSpecs = (List<?>) unwrap(args.get(0));
        List<?> testClause = (List<?>) unwrap(args.get(1));
        List<String> varNames = new ArrayList<>();
        List<Object> initExprs = new ArrayList<>();
        List<Object> stepExprs = new ArrayList<>();
        for (Object spec : varSpecs) {
            List<?> s = (List<?>) unwrap(spec);
            varNames.add(((SchemeSymbol) unwrap(s.get(0))).name());
            initExprs.add(s.get(1));
            stepExprs.add(s.size() > 2 ? s.get(2) : null);
        }
        Environment doEnv = new Environment(env);
        for (int i = 0; i < varNames.size(); i++) {
            doEnv.define(varNames.get(i), eval(initExprs.get(i), env));
        }
        while (true) {
            Object testVal = eval(testClause.get(0), doEnv);
            if (!testVal.equals(Boolean.FALSE)) {
                if (testClause.size() == 1) return VOID;
                for (int j = 1; j < testClause.size() - 1; j++) {
                    eval(testClause.get(j), doEnv);
                }
                return new TailCall(testClause.get(testClause.size() - 1), doEnv);
            }
            for (int j = 2; j < args.size(); j++) {
                eval(args.get(j), doEnv);
            }
            List<Object> newVals = new ArrayList<>();
            for (int i = 0; i < varNames.size(); i++) {
                Object step = stepExprs.get(i);
                if (step != null) {
                    newVals.add(eval(step, doEnv));
                } else {
                    newVals.add(doEnv.lookup(varNames.get(i)));
                }
            }
            for (int i = 0; i < varNames.size(); i++) {
                doEnv.set(varNames.get(i), newVals.get(i));
            }
        }
    }

    private Object evalCond(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        for (Object clause : args) {
            List<?> cl = (List<?>) unwrap(clause);
            if (cl.isEmpty()) throw new EvalError("cond: empty clause");
            Object test = cl.get(0);
            Object rawTest = unwrap(test);
            if (rawTest instanceof SchemeSymbol s && s.name().equals("else")) {
                for (int i = 1; i < cl.size() - 1; i++) {
                    eval(cl.get(i), env);
                }
                if (cl.size() > 1) {
                    return new TailCall(cl.get(cl.size() - 1), env);
                }
                return VOID;
            }
            Object testVal = eval(test, env);
            if (!testVal.equals(Boolean.FALSE)) {
                if (cl.size() == 1) return testVal;
                for (int i = 1; i < cl.size() - 1; i++) {
                    eval(cl.get(i), env);
                }
                return new TailCall(cl.get(cl.size() - 1), env);
            }
        }
        return VOID;
    }

    @SuppressWarnings("unchecked")
    private Object evalDefine(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 2) throw new EvalError("define: bad syntax");
        Object target = unwrap(args.get(0));
        if (target instanceof SchemeSymbol s) {
            Object val = eval(args.get(1), env);
            env.define(s.name(), val);
            return VOID;
        }
        if (target instanceof List<?> sig) {
            if (sig.isEmpty()) throw new EvalError("define: bad syntax");
            String fname = ((SchemeSymbol) unwrap(sig.get(0))).name();
            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int i = 1; i < sig.size(); i++) {
                String pname = ((SchemeSymbol) unwrap(sig.get(i))).name();
                if (pname.equals(".")) {
                    if (i + 1 < sig.size()) {
                        restParam = ((SchemeSymbol) unwrap(sig.get(i + 1))).name();
                    }
                    break;
                }
                params.add(pname);
            }
            Object body = wrapBodyInBegin(args, 1);
            env.define(fname, new SchemeLambda(params, restParam, body, env));
            return VOID;
        }
        throw new EvalError("define: bad syntax");
    }

    private Object evalDefineRecordType(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() < 3) throw new EvalError("define-record-type: bad syntax");
        String typeName = ((SchemeSymbol) unwrap(args.get(0))).name();
        List<?> ctorSpec = (List<?>) unwrap(args.get(1));
        String ctorName = ((SchemeSymbol) unwrap(ctorSpec.get(0))).name();
        List<String> ctorFields = new ArrayList<>();
        for (int i = 1; i < ctorSpec.size(); i++)
            ctorFields.add(((SchemeSymbol) unwrap(ctorSpec.get(i))).name());
        String predName = ((SchemeSymbol) unwrap(args.get(2))).name();
        List<String> fieldNames = new ArrayList<>();
        List<String> accessorNames = new ArrayList<>();
        for (int i = 3; i < args.size(); i++) {
            List<?> fieldSpec = (List<?>) unwrap(args.get(i));
            fieldNames.add(((SchemeSymbol) unwrap(fieldSpec.get(0))).name());
            accessorNames.add(((SchemeSymbol) unwrap(fieldSpec.get(1))).name());
        }
        recordTypes.put(typeName, ctorFields.toArray(new String[0]));
        env.define(ctorName, new BuiltinProcedure("record-ctor:" + typeName));
        env.define(predName, new BuiltinProcedure("record-pred:" + typeName));
        for (int i = 0; i < fieldNames.size(); i++) {
            env.define(accessorNames.get(i), new BuiltinProcedure("record-acc:" + typeName + ":" + fieldNames.get(i)));
        }
        return VOID;
    }

    private Object evalCallCC(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        if (args.size() != 1) throw new EvalError("call/cc: expected 1 argument");
        Object proc = eval(args.get(0), env);
        return doCallCC(proc);
    }

    private Object doCallCC(Object proc) throws EvalError, ContinuationException, SchemeRaiseException {
        // Check if we're replaying a continuation
        if (activeContinuationId != null) {
            Object val = activeContinuationValue;
            activeContinuationId = null;
            activeContinuationValue = null;
            return val;
        }
        // Capture full body frame stack (outer frames + current)
        List<BodyFrame> capturedStack = new ArrayList<>(bodyFrameStack.size() + 1);
        for (BodyFrame f : bodyFrameStack) {
            capturedStack.add(new BodyFrame(f.exprs, f.index, f.env));
        }
        capturedStack.add(new BodyFrame(currentBodyExprs, currentBodyIndex, currentBodyEnv));
        // Capture remaining top-level expressions
        List<Object> remainingTopLevel = new ArrayList<>();
        if (topLevelExprs != null) {
            for (int ri = topLevelIndex + 1; ri < topLevelExprs.size(); ri++) {
                remainingTopLevel.add(topLevelExprs.get(ri));
            }
        }
        Object topLevelExpr = (topLevelExprs != null && topLevelIndex < topLevelExprs.size())
            ? topLevelExprs.get(topLevelIndex) : null;
        SchemeContinuation cont = new SchemeContinuation(capturedStack,
            topLevelIndex, topLevelExpr, remainingTopLevel, globalEnv, this);
        continuationRegistry.put(cont.id, cont);
        // Call the procedure with the continuation, tracking active site
        List<Object> contArgs = new ArrayList<>();
        contArgs.add(cont);
        activeContinuationSites.add(cont.id);
        try {
            return applyResolved(proc, contArgs);
        } catch (ContinuationException ce) {
            if (ce.continuationId == cont.id) {
                return ce.value;
            }
            throw ce;
        } finally {
            activeContinuationSites.remove(cont.id);
        }
    }

    private Object evalDynamicWind(Object inThunk, Object bodyThunk, Object outThunk) throws EvalError, ContinuationException, SchemeRaiseException, SchemeRaiseException {
        // Run in-thunk
        applyResolved(inThunk, List.of());
        // Push wind entry
        Object[] entry = {inThunk, outThunk};
        windStack.add(entry);
        Object result;
        try {
            result = applyResolved(bodyThunk, List.of());
        } catch (ContinuationException ce) {
            // Non-local exit: pop and run out-thunk before re-throwing
            windStack.remove(windStack.size() - 1);
            applyResolved(outThunk, List.of());
            throw ce;
        } catch (SchemeRaiseException re) {
            // Exception: pop and run out-thunk before re-throwing
            windStack.remove(windStack.size() - 1);
            applyResolved(outThunk, List.of());
            throw re;
        } catch (EvalError ee) {
            // EvalError during body: still need to run out-thunk for cleanup
            windStack.remove(windStack.size() - 1);
            applyResolved(outThunk, List.of());
            throw ee;
        }
        // Normal exit: pop and run out-thunk
        windStack.remove(windStack.size() - 1);
        applyResolved(outThunk, List.of());
        return result;
    }

    @SuppressWarnings("unchecked")
    private Object evalGuard(List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        // (guard (var clause1 clause2 ...) body ...)
        if (args.size() < 2) throw new EvalError("guard: bad syntax");
        List<?> clauseSpec = (List<?>) unwrap(args.get(0));
        if (clauseSpec.isEmpty()) throw new EvalError("guard: bad syntax");
        String exnVar = ((SchemeSymbol) unwrap(clauseSpec.get(0))).name();
        List<Object> clauses = new ArrayList<>();
        for (int i = 1; i < clauseSpec.size(); i++) {
            clauses.add(clauseSpec.get(i));
        }
        // Body expressions
        List<Object> bodyExprs = args.subList(1, args.size());
        Object body = wrapBodyInBegin(bodyExprs, 0);

        // Evaluate body, catching SchemeRaiseException
        // Use evalStep (not eval) so tail calls in the guard body are resolved by
        // the outer trampoline, enabling TCO across guard boundaries.
        try {
            Object result = evalStep(body, env);
            if (result instanceof TailCall) return result;
            return result;
        } catch (SchemeRaiseException re) {
            // Test clauses against the raised value
            Environment guardEnv = new Environment(env);
            guardEnv.define(exnVar, re.value);
            for (Object clause : clauses) {
                List<?> cl = (List<?>) unwrap(clause);
                if (cl.isEmpty()) continue;
                Object test = unwrap(cl.get(0));
                if (test instanceof SchemeSymbol sym && sym.name().equals("else")) {
                    // else clause: evaluate body
                    if (cl.size() == 1) return VOID;
                    Object result = null;
                    for (int i = 1; i < cl.size(); i++) {
                        result = eval(cl.get(i), guardEnv);
                    }
                    return result;
                }
                Object testResult = eval(cl.get(0), guardEnv);
                if (!testResult.equals(Boolean.FALSE)) {
                    if (cl.size() == 1) return testResult;
                    Object result = null;
                    for (int i = 1; i < cl.size(); i++) {
                        result = eval(cl.get(i), guardEnv);
                    }
                    return result;
                }
            }
            // No clause matched — re-raise
            throw re;
        }
    }

    private Object evalWithExceptionHandler(Object handler, Object thunk) throws EvalError, ContinuationException, SchemeRaiseException {
        try {
            return applyResolved(thunk, List.of());
        } catch (SchemeRaiseException re) {
            // Call handler with the raised value; handler must escape (via continuation)
            // or it's an error for 'raise'. We call it and if it returns, re-raise.
            return applyResolved(handler, List.of(re.value));
        }
    }

    private Object wrapBodyInBegin(List<Object> args, int bodyStart) {
        if (args.size() == bodyStart + 1) {
            return args.get(bodyStart);
        }
        List<Object> beginList = new ArrayList<>();
        beginList.add(new SchemeSymbol("begin"));
        beginList.addAll(args.subList(bodyStart, args.size()));
        return beginList;
    }

    private SchemeLambda parseLambda(List<?> args, Environment env) throws EvalError {
        Object paramObj = unwrap(args.get(0));
        List<?> paramList = (List<?>) paramObj;
        List<String> params = new ArrayList<>();
        String restParam = null;
        for (int i = 0; i < paramList.size(); i++) {
            String pname = ((SchemeSymbol) unwrap(paramList.get(i))).name();
            if (pname.equals(".")) {
                if (i + 1 < paramList.size()) {
                    restParam = ((SchemeSymbol) unwrap(paramList.get(i + 1))).name();
                }
                break;
            }
            params.add(pname);
        }
        List<Object> bodyArgs = new ArrayList<>();
        for (int i = 1; i < args.size(); i++) bodyArgs.add(args.get(i));
        return new SchemeLambda(params, restParam, wrapBodyInBegin(bodyArgs, 0), env);
    }

    // apply returns TailCall for lambda bodies (for TCO)
    private Object apply(Object proc, List<Object> args) throws EvalError, ContinuationException, SchemeRaiseException {
        if (proc instanceof SchemeContinuation cont) {
            // Continuations accept multiple values: wrap as SchemeValues if > 1 arg
            Object value;
            if (args.size() == 1) {
                value = args.get(0);
            } else {
                value = new SchemeValues(new ArrayList<>(args));
            }
            // Within dynamic extent: escape (caught by doCallCC's try/catch)
            if (activeContinuationSites.contains(cont.id)) {
                throw new ContinuationException(cont.id, value);
            }
            // Different top-level expression: throw for top-level replay
            if (topLevelIndex != cont.captureTopLevelIndex) {
                throw new ContinuationException(cont.id, value);
            }
            // Same top-level: check if we're in the same body context as the call/cc
            if (cont.frameStack != null && !cont.frameStack.isEmpty()) {
                BodyFrame innermost = cont.frameStack.get(cont.frameStack.size() - 1);
                if (innermost.exprs != null) {
                    // Check current body context
                    if (innermost.exprs == currentBodyExprs) {
                        throw new ContinuationException(cont.id, value);
                    }
                    // Check outer frames on the stack
                    for (BodyFrame frame : bodyFrameStack) {
                        if (frame.exprs != null && frame.exprs == innermost.exprs) {
                            throw new ContinuationException(cont.id, value);
                        }
                    }
                }
            }
            // Same top-level, different body context: return value directly (no replay)
            return value;
        }
        if (proc instanceof SchemeCaseLambda cl) {
            for (SchemeLambda clause : cl.clauses) {
                if (clause.restParam != null) {
                    if (args.size() >= clause.params.size()) {
                        return apply(clause, args);
                    }
                } else {
                    if (args.size() == clause.params.size()) {
                        return apply(clause, args);
                    }
                }
            }
            throw new EvalError("no matching clause for " + args.size() + " arguments");
        }
        if (proc instanceof SchemeLambda lam) {
            if (lam.restParam != null) {
                if (args.size() < lam.params.size()) {
                    throw new EvalError("expected at least " + lam.params.size() + " arguments, got " + args.size());
                }
            } else {
                if (args.size() != lam.params.size()) {
                    throw new EvalError("expected " + lam.params.size() + " arguments, got " + args.size());
                }
            }
            Environment callEnv = new Environment(lam.closure);
            for (int i = 0; i < lam.params.size(); i++) {
                callEnv.define(lam.params.get(i), args.get(i));
            }
            if (lam.restParam != null) {
                Object rest = SchemeNil.INSTANCE;
                for (int i = args.size() - 1; i >= lam.params.size(); i--) {
                    rest = new SchemePair(args.get(i), rest);
                }
                callEnv.define(lam.restParam, rest);
            }
            return new TailCall(lam.body, callEnv);
        }
        if (proc instanceof BuiltinProcedure bp) {
            return applyBuiltin(bp.name(), args);
        }
        throw new EvalError("not a procedure");
    }


    private Object applyBuiltin(String name, List<Object> args) throws EvalError, ContinuationException, SchemeRaiseException {
        return switch (name) {
            case "call/cc" -> {
                if (args.size() != 1) throw new EvalError("call/cc: expected 1 argument");
                yield doCallCC(args.get(0));
            }
            case "raise" -> {
                if (args.size() != 1) throw new EvalError("raise: expected 1 argument");
                throw new SchemeRaiseException(args.get(0));
            }
            case "with-exception-handler" -> {
                if (args.size() != 2) throw new EvalError("with-exception-handler: expected 2 arguments");
                yield evalWithExceptionHandler(args.get(0), args.get(1));
            }
            case "values" -> {
                if (args.size() == 1) yield args.get(0);
                yield new SchemeValues(new ArrayList<>(args));
            }
            case "call-with-values" -> {
                if (args.size() != 2) throw new EvalError("call-with-values: expected 2 arguments");
                Object producer = args.get(0);
                Object consumer = args.get(1);
                Object produced = trampoline(apply(producer, List.of()));
                List<Object> consumerArgs;
                if (produced instanceof SchemeValues sv) {
                    consumerArgs = sv.values();
                } else {
                    consumerArgs = List.of(produced);
                }
                yield trampoline(apply(consumer, consumerArgs));
            }
            case "+", "-", "*", "/", "abs", "modulo", "remainder", "quotient",
                 "min", "max", "expt", "zero?", "positive?", "negative?", "odd?", "even?",
                 "gcd", "lcm", "truncate", "round" ->
                ArithmeticOps.apply(name, args);
            case "<" -> ArithmeticOps.toDouble(args.get(0)) < ArithmeticOps.toDouble(args.get(1));
            case ">" -> ArithmeticOps.toDouble(args.get(0)) > ArithmeticOps.toDouble(args.get(1));
            case "=" -> ArithmeticOps.toDouble(args.get(0)) == ArithmeticOps.toDouble(args.get(1));
            case "<=" -> ArithmeticOps.toDouble(args.get(0)) <= ArithmeticOps.toDouble(args.get(1));
            case ">=" -> ArithmeticOps.toDouble(args.get(0)) >= ArithmeticOps.toDouble(args.get(1));
            case "not" -> args.get(0).equals(Boolean.FALSE);
            case "cons", "car", "cdr", "null?", "list", "length", "append",
                 "list-ref", "list-tail", "list?", "assoc", "map",
                 "set-car!", "set-cdr!", "for-each",
                 "caar", "cadr", "cdar", "cddr", "caddr", "cadar", "caddar",
                 "caaar", "caadr", "cdaar", "cdadr", "cddar", "cdddr",
                 "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "cadddr",
                 "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
                 "reverse", "memq", "memv", "member", "assq", "assv" ->
                applyListBuiltin(name, args);
            case "number?", "integer?", "rational?", "exact?", "inexact?",
                 "exact->inexact", "inexact->exact", "numerator", "denominator" ->
                ArithmeticOps.applyNumericType(name, args);
            case "string?", "boolean?", "pair?", "symbol?", "char?", "procedure?" ->
                applyTypePredicateBuiltin(name, args);
            case "display", "write", "newline" ->
                applyIoBuiltin(name, args);
            case "string-append", "string-length", "substring", "string-ref",
                 "string->number", "number->string", "symbol->string", "string->symbol",
                 "string-copy", "string-set!", "string->list", "list->string", "string=?", "string<?", "string-ci=?",
                 "string-upcase", "string-downcase",
                 "make-string", "string", "string>?", "string<=?", "string>=?" ->
                StringCharBuiltins.applyStringBuiltin(name, args);
            case "char-alphabetic?", "char-numeric?", "char=?", "char<?",
                 "char-upcase", "char-downcase", "char->integer", "integer->char" ->
                StringCharBuiltins.applyCharBuiltin(name, args);
            case "syntax->datum" -> {
                if (args.size() != 1) throw new EvalError("syntax->datum: expected 1 argument");
                Object arg = args.get(0);
                yield (arg instanceof SyntaxObject so) ? so.datum() : arg;
            }
            case "datum->syntax" -> {
                if (args.size() != 2) throw new EvalError("datum->syntax: expected 2 arguments");
                Object templateId = args.get(0);
                Object datum = args.get(1);
                Environment ctx = (templateId instanceof SyntaxObject so) ? so.context() : globalEnv;
                yield new SyntaxObject(datum, ctx);
            }
            case "apply" -> {
                if (args.size() < 2) throw new EvalError("apply: expected at least 2 arguments");
                Object applyProc = args.get(0);
                Object lastArg = args.get(args.size() - 1);
                List<Object> allArgs = new ArrayList<>();
                for (int i = 1; i < args.size() - 1; i++) allArgs.add(args.get(i));
                Object cur = lastArg;
                while (cur instanceof SchemePair p) { allArgs.add(p.car); cur = p.cdr; }
                yield applyResolved(applyProc, allArgs);
            }
            case "eq?" -> {
                Object a = args.get(0), b = args.get(1);
                if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) yield sa.name().equals(sb.name());
                if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) yield ca.value() == cb.value();
                yield a == b || a.equals(b);
            }
            case "eqv?" -> schemeEqv(args.get(0), args.get(1));
            case "equal?" -> schemeEqual(args.get(0), args.get(1));
            case "vector" -> new SchemeVector(args.toArray());
            case "make-vector" -> {
                int size = (int) requireLong(args.get(0));
                Object fill = args.size() > 1 ? args.get(1) : 0L;
                yield new SchemeVector(size, fill);
            }
            case "vector-ref" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-ref: not a vector");
                yield v.ref((int) requireLong(args.get(1)));
            }
            case "vector-set!" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-set!: not a vector");
                v.set((int) requireLong(args.get(1)), args.get(2));
                yield VOID;
            }
            case "vector-length" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector-length: not a vector");
                yield (long) v.length();
            }
            case "vector?" -> args.get(0) instanceof SchemeVector;
            case "vector->list" -> {
                if (!(args.get(0) instanceof SchemeVector v)) throw new EvalError("vector->list: not a vector");
                Object result = SchemeNil.INSTANCE;
                for (int i = v.length() - 1; i >= 0; i--) result = new SchemePair(v.elements[i], result);
                yield result;
            }
            case "list->vector" -> {
                List<Object> elems = new ArrayList<>();
                Object cur = args.get(0);
                while (cur instanceof SchemePair p) { elems.add(p.car); cur = p.cdr; }
                yield new SchemeVector(elems.toArray());
            }
            default -> {
                if (name.startsWith("record-ctor:")) {
                    String recType = name.substring("record-ctor:".length());
                    String[] fieldNames = recordTypes.get(recType);
                    if (fieldNames == null) throw new EvalError("unknown record type: " + recType);
                    if (args.size() != fieldNames.length)
                        throw new EvalError(recType + " constructor: expected " + fieldNames.length + " arguments, got " + args.size());
                    yield new SchemeRecord(recType, fieldNames, args.toArray());
                } else if (name.startsWith("record-pred:")) {
                    String recType = name.substring("record-pred:".length());
                    yield args.get(0) instanceof SchemeRecord r && r.typeName.equals(recType);
                } else if (name.startsWith("record-acc:")) {
                    String rest = name.substring("record-acc:".length());
                    int colonIdx = rest.indexOf(':');
                    String recType = rest.substring(0, colonIdx);
                    String fieldName = rest.substring(colonIdx + 1);
                    if (!(args.get(0) instanceof SchemeRecord r) || !r.typeName.equals(recType))
                        throw new EvalError(name + ": not a " + recType);
                    Object val = r.getField(fieldName);
                    if (val == null) throw new EvalError(name + ": no such field " + fieldName);
                    yield val;
                } else {
                    throw new EvalError("unknown procedure: " + name);
                }
            }
        };
    }


    private Object applyListBuiltin(String name, List<Object> args) throws EvalError, ContinuationException, SchemeRaiseException {
        return ListBuiltins.apply(name, args, this::applyResolved);
    }

    private Object applyTypePredicateBuiltin(String name, List<Object> args) {
        return switch (name) {
            case "string?" -> { Object sv = args.get(0); yield sv instanceof String || sv instanceof SchemeString; }
            case "boolean?" -> args.get(0) instanceof Boolean;
            case "pair?" -> args.get(0) instanceof SchemePair;
            case "symbol?" -> args.get(0) instanceof SchemeSymbol;
            case "char?" -> args.get(0) instanceof SchemeChar;
            case "procedure?" -> args.get(0) instanceof SchemeLambda || args.get(0) instanceof SchemeCaseLambda || args.get(0) instanceof BuiltinProcedure || args.get(0) instanceof SchemeContinuation;
            default -> false;
        };
    }

    private Object applyIoBuiltin(String name, List<Object> args) {
        switch (name) {
            case "display" -> { if (outputBuffer != null) outputBuffer.append(displayString(args.get(0))); }
            case "write" -> { if (outputBuffer != null) outputBuffer.append(schemeToString(args.get(0))); }
            case "newline" -> { if (outputBuffer != null) outputBuffer.append("\n"); }
            default -> { }
        }
        return VOID;
    }

    private Object evalBuiltin(String name, List<Object> args, Environment env) throws EvalError, ContinuationException, SchemeRaiseException {
        List<Object> evaluated = new ArrayList<>();
        for (Object arg : args) {
            evaluated.add(eval(arg, env));
        }
        return applyBuiltin(name, evaluated);
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

    /** display format: strings without quotes, vectors/lists display their elements */
    private String displayString(Object val) {
        if (val instanceof String s) {
            if (s.startsWith("\"") && s.endsWith("\"")) {
                return s.substring(1, s.length() - 1);
            }
            return s;
        }
        if (val instanceof SchemeString ss) {
            return ss.value();
        }
        return schemeToString(val);
    }

    static boolean schemeEqv(Object a, Object b) {
        if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (a instanceof Boolean ba && b instanceof Boolean bb) return ba.equals(bb);
        if (ArithmeticOps.isNumber(a) && ArithmeticOps.isNumber(b)) {
            try { return ArithmeticOps.toDouble(a) == ArithmeticOps.toDouble(b); } catch (EvalError e) { return false; }
        }
        return a == b;
    }

    static boolean schemeEqual(Object a, Object b) {
        return schemeEqualRec(a, b, 0);
    }

    static boolean schemeEqualRec(Object a, Object b, int depth) {
        if (a == b) return true;
        if (depth > 100000) return false; // prevent infinite recursion on cycles
        if (a instanceof SchemePair pa && b instanceof SchemePair pb) {
            return schemeEqualRec(pa.car, pb.car, depth + 1) && schemeEqualRec(pa.cdr, pb.cdr, depth + 1);
        }
        if (a instanceof SchemeVector va && b instanceof SchemeVector vb) {
            if (va.length() != vb.length()) return false;
            for (int i = 0; i < va.length(); i++) {
                if (!schemeEqualRec(va.elements[i], vb.elements[i], depth + 1)) return false;
            }
            return true;
        }
        if (a instanceof SchemeNil && b instanceof SchemeNil) return true;
        if (a instanceof SchemeSymbol sa && b instanceof SchemeSymbol sb) return sa.name().equals(sb.name());
        if (a instanceof SchemeChar ca && b instanceof SchemeChar cb) return ca.value() == cb.value();
        if (ArithmeticOps.isNumber(a) && ArithmeticOps.isNumber(b)) {
            try { return ArithmeticOps.toDouble(a) == ArithmeticOps.toDouble(b); } catch (EvalError e) { return false; }
        }
        if ((a instanceof String || a instanceof SchemeString) && (b instanceof String || b instanceof SchemeString)) {
            try { return requireString(a).equals(requireString(b)); } catch (EvalError e) { return false; }
        }
        if (a == null || b == null) return a == b;
        return a.equals(b);
    }

    private void requireArgCount(String name, List<?> args, int expected) throws EvalError {
        if (args.size() != expected) {
            throw new EvalError(name + ": expected " + expected + " arguments, got " + args.size());
        }
    }

}
