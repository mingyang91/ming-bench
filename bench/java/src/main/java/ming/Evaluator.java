package ming;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Evaluator {

    private final Environment globalEnv = new Environment();
    private StringBuilder outputBuffer = new StringBuilder();

    // --- call/cc state ---
    /** Counter incremented each time call/cc is invoked. */
    private int callccCounter = 0;
    /** During replay: the call/cc ID to match. */
    private int replayTargetId = -1;
    /** During replay: the value to return from call/cc. */
    private SchemeValue replayValue = null;
    /** Continuation stack: tracks remaining computation frames for call/cc capture. */
    private final Deque<ContFrame> contStack = new ArrayDeque<>();

    private int gensymCounter = 0;

    private static final Set<String> SPECIAL_FORMS = Set.of(
        "define", "if", "quote", "lambda", "let", "begin", "cond", "set!",
        "and", "or", "define-syntax", "syntax-rules"
    );

    private static final String[] BUILTIN_NAMES = {
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length", "append",
        "string?", "number?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string", "symbol->string", "string->symbol",
        "string-ref", "string-set!", "string-copy",
        "apply",
        "call/cc", "call-with-current-continuation",
        // L12 builtins
        "equal?", "eq?", "map",
        // L13 builtins
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
        // L14 builtins
        "string->list", "list->string", "char->integer", "integer->char"
    };

    {
        for (String name : BUILTIN_NAMES) {
            globalEnv.define(name, new SchemeValue.BuiltinVal(name));
        }
    }

    public String evalStr(String input) throws EvalError {
        outputBuffer.setLength(0);
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        SchemeValue result = evalTopLevel(exprs, 0);
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        SchemeValue result = evalTopLevel(exprs, 0);
        return new EvalResult(result.display(), outputBuffer.toString());
    }

    /**
     * Evaluate top-level expressions with continuation support.
     * When a saved continuation is invoked, ContinuationReturn is caught here
     * and we resume from the captured continuation stack.
     */
    private SchemeValue evalTopLevel(List<SchemeValue> exprs, int startIndex) throws EvalError {
        try {
            SchemeValue result = null;
            for (int i = startIndex; i < exprs.size(); i++) {
                contStack.push(new ContFrame.BodyFrame(exprs.get(i),
                        exprs.subList(i + 1, exprs.size()), globalEnv, callccCounter));
                result = eval(exprs.get(i), globalEnv);
                contStack.pop();
            }
            return result;
        } catch (ContinuationReturn cr) {
            return handleContinuation(cr);
        }
    }

    /**
     * Handle a ContinuationReturn by restoring the continuation stack and resuming.
     * Loops to handle chains of continuation invocations.
     */
    private SchemeValue handleContinuation(ContinuationReturn cr) throws EvalError {
        while (true) {
            try {
                // Restore contStack from snapshot (innermost first in list)
                contStack.clear();
                var snapshot = cr.cont.contStackSnapshot;
                for (int i = snapshot.size() - 1; i >= 0; i--) {
                    contStack.push(snapshot.get(i));
                }
                return resumeFromContStack(cr.value, cr.cont.callccId);
            } catch (ContinuationReturn cr2) {
                cr = cr2;
            }
        }
    }

    /**
     * Resume from a continuation stack after a continuation is invoked.
     * 1. Find and replay the innermost BodyFrame (discarding context frames above it)
     * 2. Evaluate remaining body expressions
     * 3. Process outer frames (DefineFrame/SetFrame apply side effects, BodyFrame evaluate remaining)
     */
    private SchemeValue resumeFromContStack(SchemeValue value, int callccId) throws EvalError {
        // Discard frames above the innermost BodyFrame
        while (!contStack.isEmpty() && !(contStack.peek() instanceof ContFrame.BodyFrame)) {
            contStack.pop();
        }

        if (contStack.isEmpty()) {
            return value;
        }

        // Pop and replay the innermost BodyFrame
        ContFrame.BodyFrame bodyFrame = (ContFrame.BodyFrame) contStack.pop();

        callccCounter = bodyFrame.callccCounterBefore();
        replayTargetId = callccId;
        replayValue = value;

        SchemeValue result = eval(bodyFrame.currentExpr(), bodyFrame.env());

        replayTargetId = -1;
        replayValue = null;

        // Evaluate remaining body expressions from this frame
        if (!bodyFrame.remaining().isEmpty()) {
            result = evalRemainingBody(bodyFrame.remaining(), bodyFrame.env());
        }

        // Process outer frames
        while (!contStack.isEmpty()) {
            ContFrame frame = contStack.pop();
            if (frame instanceof ContFrame.BodyFrame bf) {
                if (!bf.remaining().isEmpty()) {
                    result = evalRemainingBody(bf.remaining(), bf.env());
                }
            } else if (frame instanceof ContFrame.DefineFrame df) {
                df.env().define(df.name(), result);
            } else if (frame instanceof ContFrame.SetFrame sf) {
                sf.env().set(sf.name(), result);
            }
        }

        return result;
    }

    /**
     * Evaluate a sequence of body expressions, pushing contStack frames for each.
     * Returns the result of the last expression.
     */
    private SchemeValue evalRemainingBody(List<SchemeValue> remaining, Environment env) throws EvalError {
        SchemeValue result = null;
        for (int i = 0; i < remaining.size(); i++) {
            contStack.push(new ContFrame.BodyFrame(remaining.get(i),
                    remaining.subList(i + 1, remaining.size()), env, callccCounter));
            result = eval(remaining.get(i), env);
            contStack.pop();
        }
        return result;
    }

    private static EvalError posError(SourcePos pos, String msg) {
        return new EvalError(pos + ": " + msg);
    }

    /** Evaluate with trampoline — resolves Thunks iteratively for TCO. */
    private SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        while (true) {
            SchemeValue result = evalInner(expr, env);
            if (result instanceof SchemeValue.Thunk t) {
                expr = t.expr();
                env = t.env();
            } else {
                return result;
            }
        }
    }

    /** Single-step eval. Returns Thunk for tail positions. */
    private SchemeValue evalInner(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.CharVal v -> v;
            case SchemeValue.BuiltinVal v -> v;
            case SchemeValue.ContinuationVal v -> v;
            case SchemeValue.SyntaxRulesVal v -> v;
            case SchemeValue.PairVal v -> v;
            case SchemeValue.Thunk v -> v; // pass through
            case SchemeValue.SymbolVal v -> {
                try {
                    yield env.lookup(v.name());
                } catch (EvalError e) {
                    throw posError(v.pos(), "undefined variable: " + v.name());
                }
            }
            case SchemeValue.ListVal v -> evalList(v, env);
        };
    }

    private SchemeValue evalList(SchemeValue.ListVal listVal, Environment env) throws EvalError {
        List<SchemeValue> elements = listVal.elements();
        SourcePos pos = listVal.pos();
        if (elements.isEmpty()) throw posError(pos, "empty application");

        SchemeValue head = elements.getFirst();
        if (head instanceof SchemeValue.SymbolVal sym) {
            String op = sym.name();
            List<SchemeValue> args = elements.subList(1, elements.size());

            switch (op) {
                case "define" -> { return evalDefine(args, env, pos); }
                case "if" -> { return evalIfTail(args, env, pos); }
                case "quote" -> {
                    if (args.size() != 1) throw posError(pos, "quote: need exactly one argument");
                    return args.getFirst();
                }
                case "lambda" -> { return evalLambda(args, env, pos); }
                case "let" -> { return evalLetTail(args, env, pos, listVal); }
                case "begin" -> { return evalBeginTail(args, env, pos); }
                case "cond" -> { return evalCondTail(args, env, pos); }
                case "set!" -> { return evalSet(args, env, pos); }
                case "and" -> { return andTail(args, env); }
                case "or" -> { return orTail(args, env); }
                case "define-syntax" -> { return evalDefineSyntax(args, env, pos); }
                default -> {
                    SchemeValue builtinResult = tryBuiltin(op, args, env, pos);
                    if (builtinResult != null) return builtinResult;
                    // Check for macro
                    try {
                        SchemeValue resolved = env.lookup(op);
                        if (resolved instanceof SchemeValue.SyntaxRulesVal macro) {
                            SchemeValue expanded = expandMacro(macro, op, listVal, env);
                            return evalInner(expanded, env);
                        }
                    } catch (EvalError ignored) {}
                }
            }
        }

        // Procedure call
        SchemeValue proc = eval(head, env);
        List<SchemeValue> args = elements.subList(1, elements.size());
        List<SchemeValue> evaledArgs = new ArrayList<>();
        for (SchemeValue arg : args) {
            evaledArgs.add(eval(arg, env));
        }
        return applyTail(proc, evaledArgs, pos);
    }

    private SchemeValue evalDefine(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "define: need at least 2 arguments");
        SchemeValue target = args.getFirst();

        if (target instanceof SchemeValue.SymbolVal sym) {
            contStack.push(new ContFrame.DefineFrame(sym.name(), env));
            SchemeValue val = eval(args.get(1), env);
            contStack.pop();
            env.define(sym.name(), val);
            return val;
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            List<SchemeValue> elems = nameAndParams.elements();
            if (elems.isEmpty()) throw posError(pos, "define: empty name list");
            if (!(elems.getFirst() instanceof SchemeValue.SymbolVal nameSym))
                throw posError(pos, "define: name must be a symbol");

            List<String> params = new ArrayList<>();
            String restParam = null;
            for (int i = 1; i < elems.size(); i++) {
                if (!(elems.get(i) instanceof SchemeValue.SymbolVal p))
                    throw posError(pos, "define: parameter must be a symbol");
                if (p.name().equals(".")) {
                    if (i + 1 >= elems.size())
                        throw posError(pos, "define: missing rest parameter after dot");
                    if (!(elems.get(i + 1) instanceof SchemeValue.SymbolVal restSym))
                        throw posError(pos, "define: rest parameter must be a symbol");
                    restParam = restSym.name();
                    break;
                }
                params.add(p.name());
            }
            List<SchemeValue> body = args.subList(1, args.size());
            SchemeValue lambda = new SchemeValue.LambdaVal(params, restParam, body, env);
            env.define(nameSym.name(), lambda);
            return lambda;
        }
        throw posError(pos, "define: invalid syntax");
    }

    private SchemeValue evalSet(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "set!: need exactly 2 arguments");
        if (!(args.getFirst() instanceof SchemeValue.SymbolVal sym))
            throw posError(pos, "set!: first argument must be a symbol");
        contStack.push(new ContFrame.SetFrame(sym.name(), env));
        SchemeValue val = eval(args.get(1), env);
        contStack.pop();
        try {
            env.set(sym.name(), val);
        } catch (EvalError e) {
            throw posError(pos, "set!: unbound variable: " + sym.name());
        }
        return val;
    }

    /** If with tail-call: returns Thunk for the chosen branch. */
    private SchemeValue evalIfTail(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2 || args.size() > 3) throw posError(pos, "if: need 2 or 3 arguments");
        SchemeValue cond = eval(args.get(0), env);
        if (cond.isTruthy()) {
            return new SchemeValue.Thunk(args.get(1), env);
        } else if (args.size() == 3) {
            return new SchemeValue.Thunk(args.get(2), env);
        }
        return new SchemeValue.BoolVal(false, SourcePos.NONE);
    }

    private SchemeValue evalLambda(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "lambda: need params and body");
        SchemeValue paramSpec = args.getFirst();
        if (!(paramSpec instanceof SchemeValue.ListVal paramList))
            throw posError(pos, "lambda: params must be a list");

        List<String> params = new ArrayList<>();
        String restParam = null;
        List<SchemeValue> elems = paramList.elements();
        for (int i = 0; i < elems.size(); i++) {
            if (!(elems.get(i) instanceof SchemeValue.SymbolVal sym))
                throw posError(pos, "lambda: parameter must be a symbol");
            if (sym.name().equals(".")) {
                if (i + 1 >= elems.size())
                    throw posError(pos, "lambda: missing rest parameter after dot");
                if (!(elems.get(i + 1) instanceof SchemeValue.SymbolVal restSym))
                    throw posError(pos, "lambda: rest parameter must be a symbol");
                restParam = restSym.name();
                break;
            }
            params.add(sym.name());
        }
        List<SchemeValue> body = args.subList(1, args.size());
        return new SchemeValue.LambdaVal(params, restParam, body, env);
    }

    /** Apply with tail-call: returns Thunk for the last body expression. */
    private SchemeValue applyTail(SchemeValue proc, List<SchemeValue> args, SourcePos pos) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (lambda.restParam() != null) {
                if (args.size() < lambda.params().size())
                    throw posError(pos, "wrong number of arguments: expected at least " + lambda.params().size() + ", got " + args.size());
            } else {
                if (args.size() != lambda.params().size())
                    throw posError(pos, "wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size());
            }
            Environment callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            if (lambda.restParam() != null) {
                List<SchemeValue> rest = args.subList(lambda.params().size(), args.size());
                callEnv.define(lambda.restParam(), new SchemeValue.ListVal(new ArrayList<>(rest), SourcePos.NONE));
            }
            // Evaluate all but the last body expression
            for (int i = 0; i < lambda.body().size() - 1; i++) {
                contStack.push(new ContFrame.BodyFrame(lambda.body().get(i),
                        lambda.body().subList(i + 1, lambda.body().size()), callEnv, callccCounter));
                eval(lambda.body().get(i), callEnv);
                contStack.pop();
            }
            // Return Thunk for the last body expression (TCO)
            return new SchemeValue.Thunk(lambda.body().getLast(), callEnv);
        }
        if (proc instanceof SchemeValue.ContinuationVal contVal) {
            if (args.size() != 1) throw posError(pos, "continuation: need exactly 1 argument");
            throw new ContinuationReturn(contVal.cont(), args.getFirst());
        }
        if (proc instanceof SchemeValue.BuiltinVal builtin) {
            return applyBuiltinEvaled(builtin.name(), args, pos);
        }
        throw posError(pos, "not a procedure: " + proc.display());
    }

    // --- Builtins ---

    @FunctionalInterface
    private interface LongBinOp {
        long apply(long a, long b);
    }

    @FunctionalInterface
    private interface LongPred {
        boolean test(long a, long b);
    }

    private SchemeValue tryBuiltin(String op, List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        return switch (op) {
            case "+" -> arith(args, 0, Long::sum, env, pos);
            case "-" -> minus(args, env, pos);
            case "*" -> arith(args, 1, (a, b) -> a * b, env, pos);
            case "/" -> divide(args, env, pos);
            case "<" -> compare(args, (a, b) -> a < b, env, pos);
            case ">" -> compare(args, (a, b) -> a > b, env, pos);
            case "=" -> compare(args, (a, b) -> a == b, env, pos);
            case "<=" -> compare(args, (a, b) -> a <= b, env, pos);
            case ">=" -> compare(args, (a, b) -> a >= b, env, pos);
            case "not" -> not(args, env, pos);
            // L03 builtins
            case "cons" -> builtinCons(args, env, pos);
            case "car" -> builtinCar(args, env, pos);
            case "cdr" -> builtinCdr(args, env, pos);
            case "null?" -> builtinNullQ(args, env, pos);
            case "list" -> builtinList(args, env);
            case "length" -> builtinLength(args, env, pos);
            case "append" -> builtinAppend(args, env, pos);
            case "string?" -> typePred(args, env, SchemeValue.StringVal.class, pos);
            case "number?" -> typePred(args, env, SchemeValue.IntVal.class, pos);
            case "boolean?" -> typePred(args, env, SchemeValue.BoolVal.class, pos);
            case "pair?" -> builtinPairQ(args, env, pos);
            case "symbol?" -> typePred(args, env, SchemeValue.SymbolVal.class, pos);
            case "char?" -> typePred(args, env, SchemeValue.CharVal.class, pos);
            // L05 builtins
            case "display" -> builtinDisplay(args, env, pos);
            case "write" -> builtinWrite(args, env, pos);
            case "newline" -> builtinNewline(args, pos);
            case "string-append" -> builtinStringAppend(args, env, pos);
            case "string-length" -> builtinStringLength(args, env, pos);
            case "substring" -> builtinSubstring(args, env, pos);
            case "string->number" -> builtinStringToNumber(args, env, pos);
            case "number->string" -> builtinNumberToString(args, env, pos);
            case "symbol->string" -> builtinSymbolToString(args, env, pos);
            case "string->symbol" -> builtinStringToSymbol(args, env, pos);
            case "string-ref" -> builtinStringRef(args, env, pos);
            // L06 builtins
            case "string-set!" -> builtinStringSet(args, env, pos);
            case "string-copy" -> builtinStringCopy(args, env, pos);
            // L09 builtins
            case "apply" -> builtinApply(args, env, pos);
            // L10 builtins
            case "call/cc", "call-with-current-continuation" -> builtinCallCC(args, env, pos);
            // L12 builtins
            case "equal?" -> builtinEqualQ(args, env, pos);
            case "eq?" -> builtinEqQ(args, env, pos);
            case "map" -> builtinMap(args, env, pos);
            // L13 builtins
            case "abs" -> builtinAbs(args, env, pos);
            case "modulo" -> builtinModulo(args, env, pos);
            case "remainder" -> builtinRemainder(args, env, pos);
            case "quotient" -> builtinQuotient(args, env, pos);
            case "min" -> builtinMinMax(args, env, pos, true);
            case "max" -> builtinMinMax(args, env, pos, false);
            case "expt" -> builtinExpt(args, env, pos);
            case "zero?" -> builtinSignPred(args, env, pos, "zero");
            case "positive?" -> builtinSignPred(args, env, pos, "positive");
            case "negative?" -> builtinSignPred(args, env, pos, "negative");
            case "odd?" -> builtinParityPred(args, env, pos, true);
            case "even?" -> builtinParityPred(args, env, pos, false);
            case "list-ref" -> builtinListRef(args, env, pos);
            case "list-tail" -> builtinListTail(args, env, pos);
            case "list?" -> builtinListQ(args, env, pos);
            case "assoc" -> builtinAssoc(args, env, pos);
            case "char-alphabetic?" -> builtinCharPred(args, env, pos, Character::isLetter);
            case "char-numeric?" -> builtinCharPred(args, env, pos, Character::isDigit);
            case "char-upcase" -> builtinCharCase(args, env, pos, true);
            case "char-downcase" -> builtinCharCase(args, env, pos, false);
            case "char=?" -> builtinCharCmp(args, env, pos, (a, b) -> a == b);
            case "char<?" -> builtinCharCmp(args, env, pos, (a, b) -> a < b);
            case "string=?" -> builtinStringCmp(args, env, pos, String::equals);
            case "string<?" -> builtinStringCmp(args, env, pos, (a, b) -> a.compareTo(b) < 0);
            case "string-ci=?" -> builtinStringCmp(args, env, pos, (a, b) -> a.equalsIgnoreCase(b));
            case "string-upcase" -> builtinStringCase(args, env, pos, true);
            case "string-downcase" -> builtinStringCase(args, env, pos, false);
            // L14 builtins
            case "string->list" -> builtinStringToList(args, env, pos);
            case "list->string" -> builtinListToString(args, env, pos);
            case "char->integer" -> builtinCharToInteger(args, env, pos);
            case "integer->char" -> builtinIntegerToChar(args, env, pos);
            default -> null;
        };
    }

    private SchemeValue arith(List<SchemeValue> args, long identity, LongBinOp op, Environment env, SourcePos pos) throws EvalError {
        long result = identity;
        for (SchemeValue arg : args) {
            result = op.apply(result, requireInt(eval(arg, env), pos));
        }
        return new SchemeValue.IntVal(result, SourcePos.NONE);
    }

    private SchemeValue minus(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.isEmpty()) throw posError(pos, "-: need at least one argument");
        if (args.size() == 1) {
            return new SchemeValue.IntVal(-requireInt(eval(args.getFirst(), env), pos), SourcePos.NONE);
        }
        long result = requireInt(eval(args.getFirst(), env), pos);
        for (int i = 1; i < args.size(); i++) {
            result -= requireInt(eval(args.get(i), env), pos);
        }
        return new SchemeValue.IntVal(result, SourcePos.NONE);
    }

    private SchemeValue divide(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.isEmpty()) throw posError(pos, "/: need at least one argument");
        long result = requireInt(eval(args.getFirst(), env), pos);
        for (int i = 1; i < args.size(); i++) {
            long divisor = requireInt(eval(args.get(i), env), pos);
            if (divisor == 0) throw posError(pos, "division by zero");
            result /= divisor;
        }
        return new SchemeValue.IntVal(result, SourcePos.NONE);
    }

    private SchemeValue compare(List<SchemeValue> args, LongPred pred, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "comparison needs at least 2 arguments");
        long prev = requireInt(eval(args.getFirst(), env), pos);
        for (int i = 1; i < args.size(); i++) {
            long curr = requireInt(eval(args.get(i), env), pos);
            if (!pred.test(prev, curr)) return new SchemeValue.BoolVal(false, SourcePos.NONE);
            prev = curr;
        }
        return new SchemeValue.BoolVal(true, SourcePos.NONE);
    }

    private SchemeValue not(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "not: need exactly one argument");
        return new SchemeValue.BoolVal(!eval(args.getFirst(), env).isTruthy(), SourcePos.NONE);
    }

    /** and with TCO on last expression. */
    private SchemeValue andTail(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) return new SchemeValue.BoolVal(true, SourcePos.NONE);
        for (int i = 0; i < args.size() - 1; i++) {
            SchemeValue result = eval(args.get(i), env);
            if (!result.isTruthy()) return result;
        }
        // Tail position: return Thunk for last arg
        return new SchemeValue.Thunk(args.getLast(), env);
    }

    /** or with TCO on last expression. */
    private SchemeValue orTail(List<SchemeValue> args, Environment env) throws EvalError {
        if (args.isEmpty()) return new SchemeValue.BoolVal(false, SourcePos.NONE);
        for (int i = 0; i < args.size() - 1; i++) {
            SchemeValue result = eval(args.get(i), env);
            if (result.isTruthy()) return result;
        }
        // Tail position: return Thunk for last arg
        return new SchemeValue.Thunk(args.getLast(), env);
    }

    // --- L03 special forms ---

    /** Let with TCO on last body expression. */
    private SchemeValue evalLetTail(List<SchemeValue> args, Environment env, SourcePos pos, SchemeValue.ListVal letExpr) throws EvalError {
        if (args.size() < 2) throw posError(pos, "let: need bindings and body");

        // Named let: (let name ((var init) ...) body...)
        if (args.getFirst() instanceof SchemeValue.SymbolVal nameSym) {
            if (args.size() < 3) throw posError(pos, "named let: need bindings and body");
            SchemeValue bindingsExpr = args.get(1);
            if (!(bindingsExpr instanceof SchemeValue.ListVal bindingsList))
                throw posError(pos, "let: bindings must be a list");

            List<String> params = new ArrayList<>();
            List<SchemeValue> inits = new ArrayList<>();
            for (SchemeValue b : bindingsList.elements()) {
                if (!(b instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                    throw posError(pos, "let: invalid binding");
                if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                    throw posError(pos, "let: binding name must be a symbol");
                params.add(s.name());
                inits.add(pair.elements().get(1));
            }

            List<SchemeValue> body = args.subList(2, args.size());
            Environment letEnv = new Environment(env);
            SchemeValue lambda = new SchemeValue.LambdaVal(params, null, body, letEnv);
            letEnv.define(nameSym.name(), lambda);

            List<SchemeValue> evaledInits = new ArrayList<>();
            for (SchemeValue init : inits) {
                evaledInits.add(eval(init, env));
            }
            return applyTail(lambda, evaledInits, pos);
        }

        // Regular let: (let ((var init) ...) body...)
        SchemeValue bindingsExpr = args.getFirst();
        if (!(bindingsExpr instanceof SchemeValue.ListVal bindingsList))
            throw posError(pos, "let: bindings must be a list");

        Environment letEnv = new Environment(env);
        for (SchemeValue b : bindingsList.elements()) {
            if (!(b instanceof SchemeValue.ListVal pair) || pair.elements().size() != 2)
                throw posError(pos, "let: invalid binding");
            if (!(pair.elements().getFirst() instanceof SchemeValue.SymbolVal s))
                throw posError(pos, "let: binding name must be a symbol");
            SchemeValue val = eval(pair.elements().get(1), env);
            letEnv.define(s.name(), val);
        }

        // Eval all but last, return Thunk for last (TCO)
        for (int i = 1; i < args.size() - 1; i++) {
            contStack.push(new ContFrame.BodyFrame(args.get(i),
                    args.subList(i + 1, args.size()), letEnv, callccCounter));
            eval(args.get(i), letEnv);
            contStack.pop();
        }
        return new SchemeValue.Thunk(args.getLast(), letEnv);
    }

    /** Begin with TCO on last expression. */
    private SchemeValue evalBeginTail(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.isEmpty()) throw posError(pos, "begin: need at least one expression");
        for (int i = 0; i < args.size() - 1; i++) {
            contStack.push(new ContFrame.BodyFrame(args.get(i),
                    args.subList(i + 1, args.size()), env, callccCounter));
            eval(args.get(i), env);
            contStack.pop();
        }
        return new SchemeValue.Thunk(args.getLast(), env);
    }

    /** Cond with TCO on matching clause body. */
    private SchemeValue evalCondTail(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        for (SchemeValue clause : args) {
            if (!(clause instanceof SchemeValue.ListVal clauseList) || clauseList.elements().isEmpty())
                throw posError(pos, "cond: invalid clause");
            List<SchemeValue> elems = clauseList.elements();
            SchemeValue test = elems.getFirst();

            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                if (elems.size() == 1) return new SchemeValue.BoolVal(false, SourcePos.NONE);
                for (int i = 1; i < elems.size() - 1; i++) {
                    contStack.push(new ContFrame.BodyFrame(elems.get(i),
                            elems.subList(i + 1, elems.size()), env, callccCounter));
                    eval(elems.get(i), env);
                    contStack.pop();
                }
                return new SchemeValue.Thunk(elems.getLast(), env);
            }

            SchemeValue testVal = eval(test, env);
            if (testVal.isTruthy()) {
                if (elems.size() == 1) return testVal;
                for (int i = 1; i < elems.size() - 1; i++) {
                    contStack.push(new ContFrame.BodyFrame(elems.get(i),
                            elems.subList(i + 1, elems.size()), env, callccCounter));
                    eval(elems.get(i), env);
                    contStack.pop();
                }
                return new SchemeValue.Thunk(elems.getLast(), env);
            }
        }
        return new SchemeValue.BoolVal(false, SourcePos.NONE);
    }

    // --- L03 builtins ---

    private SchemeValue builtinCons(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "cons: need exactly 2 arguments");
        SchemeValue a = eval(args.get(0), env);
        SchemeValue b = eval(args.get(1), env);
        if (b instanceof SchemeValue.ListVal lst) {
            var newElems = new ArrayList<SchemeValue>();
            newElems.add(a);
            newElems.addAll(lst.elements());
            return new SchemeValue.ListVal(newElems, SourcePos.NONE);
        }
        return new SchemeValue.PairVal(a, b, SourcePos.NONE);
    }

    private SchemeValue builtinCar(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "car: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) {
            return lst.elements().getFirst();
        }
        if (val instanceof SchemeValue.PairVal pair) return pair.car();
        throw posError(pos, "car: not a pair");
    }

    private SchemeValue builtinCdr(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "cdr: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) {
            return new SchemeValue.ListVal(lst.elements().subList(1, lst.elements().size()), SourcePos.NONE);
        }
        if (val instanceof SchemeValue.PairVal pair) return pair.cdr();
        throw posError(pos, "cdr: not a pair");
    }

    private SchemeValue builtinNullQ(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "null?: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.BoolVal(val instanceof SchemeValue.ListVal lst && lst.elements().isEmpty(), SourcePos.NONE);
    }

    private SchemeValue builtinList(List<SchemeValue> args, Environment env) throws EvalError {
        List<SchemeValue> elems = new ArrayList<>();
        for (SchemeValue arg : args) {
            elems.add(eval(arg, env));
        }
        return new SchemeValue.ListVal(elems, SourcePos.NONE);
    }

    private SchemeValue builtinLength(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "length: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst) {
            return new SchemeValue.IntVal(lst.elements().size(), SourcePos.NONE);
        }
        throw posError(pos, "length: not a list");
    }

    private SchemeValue builtinAppend(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() == 0) return new SchemeValue.ListVal(List.of(), SourcePos.NONE);
        List<SchemeValue> result = new ArrayList<>();
        for (int i = 0; i < args.size() - 1; i++) {
            SchemeValue val = eval(args.get(i), env);
            if (!(val instanceof SchemeValue.ListVal lst))
                throw posError(pos, "append: not a list");
            result.addAll(lst.elements());
        }
        SchemeValue last = eval(args.getLast(), env);
        if (last instanceof SchemeValue.ListVal lst) {
            result.addAll(lst.elements());
            return new SchemeValue.ListVal(result, SourcePos.NONE);
        }
        throw posError(pos, "append: last argument must be a list");
    }

    private SchemeValue builtinPairQ(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "pair?: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.BoolVal(
            (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) || val instanceof SchemeValue.PairVal,
            SourcePos.NONE);
    }

    private SchemeValue typePred(List<SchemeValue> args, Environment env, Class<? extends SchemeValue> type, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "type predicate: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.BoolVal(type.isInstance(val), SourcePos.NONE);
    }

    // --- L05 builtins ---

    private SchemeValue builtinDisplay(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "display: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        outputBuffer.append(val.displayOutput());
        return new SchemeValue.BoolVal(false, SourcePos.NONE); // void
    }

    private SchemeValue builtinWrite(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "write: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        outputBuffer.append(val.display());
        return new SchemeValue.BoolVal(false, SourcePos.NONE); // void
    }

    private SchemeValue builtinNewline(List<SchemeValue> args, SourcePos pos) throws EvalError {
        if (!args.isEmpty()) throw posError(pos, "newline: takes no arguments");
        outputBuffer.append('\n');
        return new SchemeValue.BoolVal(false, SourcePos.NONE); // void
    }

    private SchemeValue builtinStringAppend(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        var sb = new StringBuilder();
        for (SchemeValue arg : args) {
            SchemeValue val = eval(arg, env);
            if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string-append: not a string");
            sb.append(s.value());
        }
        return new SchemeValue.StringVal(sb.toString(), SourcePos.NONE);
    }

    private SchemeValue builtinStringLength(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "string-length: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string-length: not a string");
        return new SchemeValue.IntVal(s.value().length(), SourcePos.NONE);
    }

    private SchemeValue builtinSubstring(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 3) throw posError(pos, "substring: need exactly 3 arguments");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "substring: not a string");
        int start = (int) requireInt(eval(args.get(1), env), pos);
        int end = (int) requireInt(eval(args.get(2), env), pos);
        return new SchemeValue.StringVal(s.value().substring(start, end), SourcePos.NONE);
    }

    private SchemeValue builtinStringToNumber(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "string->number: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string->number: not a string");
        try {
            return new SchemeValue.IntVal(Long.parseLong(s.value()), SourcePos.NONE);
        } catch (NumberFormatException e) {
            return new SchemeValue.BoolVal(false, SourcePos.NONE);
        }
    }

    private SchemeValue builtinNumberToString(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "number->string: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        return new SchemeValue.StringVal(String.valueOf(requireInt(val, pos)), SourcePos.NONE);
    }

    private SchemeValue builtinSymbolToString(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "symbol->string: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.SymbolVal s)) throw posError(pos, "symbol->string: not a symbol");
        return new SchemeValue.StringVal(s.name(), SourcePos.NONE);
    }

    private SchemeValue builtinStringToSymbol(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "string->symbol: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string->symbol: not a string");
        return new SchemeValue.SymbolVal(s.value(), SourcePos.NONE);
    }

    private SchemeValue builtinStringRef(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "string-ref: need exactly 2 arguments");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string-ref: not a string");
        int idx = (int) requireInt(eval(args.get(1), env), pos);
        return new SchemeValue.CharVal(s.value().charAt(idx), SourcePos.NONE);
    }

    // --- L06 builtins ---

    private SchemeValue builtinStringSet(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 3) throw posError(pos, "string-set!: need exactly 3 arguments");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string-set!: not a string");
        if (!s.mutable()) throw posError(pos, "string-set!: strings are immutable");
        int idx = (int) requireInt(eval(args.get(1), env), pos);
        SchemeValue charVal = eval(args.get(2), env);
        if (!(charVal instanceof SchemeValue.CharVal c)) throw posError(pos, "string-set!: not a character");
        if (idx < 0 || idx >= s.chars().length) throw posError(pos, "string-set!: index out of range");
        s.chars()[idx] = c.value();
        return new SchemeValue.BoolVal(false, SourcePos.NONE); // void
    }

    private SchemeValue builtinStringCopy(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "string-copy: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string-copy: not a string");
        return new SchemeValue.StringVal(s.value(), true, SourcePos.NONE);
    }

    // --- L14 builtins ---

    private SchemeValue builtinStringToList(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "string->list: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string->list: not a string");
        List<SchemeValue> chars = new java.util.ArrayList<>();
        for (char c : s.chars()) {
            chars.add(new SchemeValue.CharVal(c, SourcePos.NONE));
        }
        return new SchemeValue.ListVal(chars, SourcePos.NONE);
    }

    private SchemeValue builtinListToString(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "list->string: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.ListVal lst)) throw posError(pos, "list->string: not a list");
        char[] chars = new char[lst.elements().size()];
        for (int i = 0; i < lst.elements().size(); i++) {
            if (!(lst.elements().get(i) instanceof SchemeValue.CharVal c))
                throw posError(pos, "list->string: element is not a character");
            chars[i] = c.value();
        }
        return new SchemeValue.StringVal(chars, false, SourcePos.NONE);
    }

    private SchemeValue builtinCharToInteger(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "char->integer: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.CharVal c)) throw posError(pos, "char->integer: not a character");
        return new SchemeValue.IntVal(c.value(), SourcePos.NONE);
    }

    private SchemeValue builtinIntegerToChar(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "integer->char: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        long n = requireInt(val, pos);
        return new SchemeValue.CharVal((char) n, SourcePos.NONE);
    }

    // --- L09 builtins ---

    private SchemeValue builtinApply(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "apply: need at least 2 arguments");
        SchemeValue proc = eval(args.getFirst(), env);
        // Evaluate all arguments
        List<SchemeValue> evaledMiddle = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) {
            evaledMiddle.add(eval(args.get(i), env));
        }
        SchemeValue lastArg = eval(args.getLast(), env);
        if (!(lastArg instanceof SchemeValue.ListVal lst))
            throw posError(pos, "apply: last argument must be a list");
        // Combine prefix args with the list
        List<SchemeValue> allArgs = new ArrayList<>(evaledMiddle);
        allArgs.addAll(lst.elements());
        return applyTail(proc, allArgs, pos);
    }

    // --- L10 builtins ---

    private SchemeValue builtinCallCC(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "call/cc: need exactly 1 argument");
        SchemeValue proc = eval(args.getFirst(), env);

        int myId = ++callccCounter;

        // Check if we're replaying this call/cc
        if (replayTargetId == myId) {
            SchemeValue val = replayValue;
            replayTargetId = -1;
            replayValue = null;
            return val;
        }

        // Normal execution: capture continuation and call the procedure
        Continuation cont = new Continuation(myId, contStack);
        SchemeValue contVal = new SchemeValue.ContinuationVal(cont);

        try {
            return evalContinuation(applyTail(proc, List.of(contVal), pos));
        } catch (ContinuationReturn cr) {
            if (cr.cont == cont) return cr.value; // escape: continuation invoked from lambda body
            throw cr; // not our continuation, propagate
        }
    }

    /** Resolve a value that may be a Thunk (trampoline). Used after applyTail. */
    private SchemeValue evalContinuation(SchemeValue result) throws EvalError {
        while (result instanceof SchemeValue.Thunk t) {
            result = evalInner(t.expr(), t.env());
        }
        return result;
    }

    /** Call a builtin by name with already-evaluated arguments. */
    private SchemeValue applyBuiltinEvaled(String name, List<SchemeValue> args, SourcePos pos) throws EvalError {
        return switch (name) {
            case "+" -> {
                long result = 0;
                for (SchemeValue a : args) result += requireInt(a, pos);
                yield new SchemeValue.IntVal(result, SourcePos.NONE);
            }
            case "-" -> {
                if (args.isEmpty()) throw posError(pos, "-: need at least one argument");
                if (args.size() == 1) yield new SchemeValue.IntVal(-requireInt(args.getFirst(), pos), SourcePos.NONE);
                long r = requireInt(args.getFirst(), pos);
                for (int i = 1; i < args.size(); i++) r -= requireInt(args.get(i), pos);
                yield new SchemeValue.IntVal(r, SourcePos.NONE);
            }
            case "*" -> {
                long result = 1;
                for (SchemeValue a : args) result *= requireInt(a, pos);
                yield new SchemeValue.IntVal(result, SourcePos.NONE);
            }
            case "/" -> {
                if (args.isEmpty()) throw posError(pos, "/: need at least one argument");
                long r = requireInt(args.getFirst(), pos);
                for (int i = 1; i < args.size(); i++) {
                    long d = requireInt(args.get(i), pos);
                    if (d == 0) throw posError(pos, "division by zero");
                    r /= d;
                }
                yield new SchemeValue.IntVal(r, SourcePos.NONE);
            }
            case "<" -> compareEvaled(args, (a, b) -> a < b, pos);
            case ">" -> compareEvaled(args, (a, b) -> a > b, pos);
            case "=" -> compareEvaled(args, (a, b) -> a == b, pos);
            case "<=" -> compareEvaled(args, (a, b) -> a <= b, pos);
            case ">=" -> compareEvaled(args, (a, b) -> a >= b, pos);
            case "not" -> {
                if (args.size() != 1) throw posError(pos, "not: need exactly one argument");
                yield new SchemeValue.BoolVal(!args.getFirst().isTruthy(), SourcePos.NONE);
            }
            case "cons" -> {
                if (args.size() != 2) throw posError(pos, "cons: need exactly 2 arguments");
                SchemeValue a = args.get(0), b = args.get(1);
                if (b instanceof SchemeValue.ListVal lst) {
                    var ne = new ArrayList<SchemeValue>();
                    ne.add(a); ne.addAll(lst.elements());
                    yield new SchemeValue.ListVal(ne, SourcePos.NONE);
                }
                yield new SchemeValue.PairVal(a, b, SourcePos.NONE);
            }
            case "car" -> {
                if (args.size() != 1) throw posError(pos, "car: need exactly 1 argument");
                if (args.getFirst() instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty())
                    yield lst.elements().getFirst();
                if (args.getFirst() instanceof SchemeValue.PairVal pair) yield pair.car();
                throw posError(pos, "car: not a pair");
            }
            case "cdr" -> {
                if (args.size() != 1) throw posError(pos, "cdr: need exactly 1 argument");
                if (args.getFirst() instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty())
                    yield new SchemeValue.ListVal(lst.elements().subList(1, lst.elements().size()), SourcePos.NONE);
                if (args.getFirst() instanceof SchemeValue.PairVal pair) yield pair.cdr();
                throw posError(pos, "cdr: not a pair");
            }
            case "null?" -> {
                if (args.size() != 1) throw posError(pos, "null?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(args.getFirst() instanceof SchemeValue.ListVal lst && lst.elements().isEmpty(), SourcePos.NONE);
            }
            case "list" -> new SchemeValue.ListVal(new ArrayList<>(args), SourcePos.NONE);
            case "length" -> {
                if (args.size() != 1) throw posError(pos, "length: need exactly 1 argument");
                if (args.getFirst() instanceof SchemeValue.ListVal lst)
                    yield new SchemeValue.IntVal(lst.elements().size(), SourcePos.NONE);
                throw posError(pos, "length: not a list");
            }
            case "append" -> {
                if (args.isEmpty()) yield new SchemeValue.ListVal(List.of(), SourcePos.NONE);
                var result = new ArrayList<SchemeValue>();
                for (int i = 0; i < args.size(); i++) {
                    if (!(args.get(i) instanceof SchemeValue.ListVal lst))
                        throw posError(pos, "append: not a list");
                    result.addAll(lst.elements());
                }
                yield new SchemeValue.ListVal(result, SourcePos.NONE);
            }
            case "display" -> {
                if (args.size() != 1) throw posError(pos, "display: need exactly 1 argument");
                outputBuffer.append(args.getFirst().displayOutput());
                yield new SchemeValue.BoolVal(false, SourcePos.NONE);
            }
            case "write" -> {
                if (args.size() != 1) throw posError(pos, "write: need exactly 1 argument");
                outputBuffer.append(args.getFirst().display());
                yield new SchemeValue.BoolVal(false, SourcePos.NONE);
            }
            case "newline" -> {
                outputBuffer.append('\n');
                yield new SchemeValue.BoolVal(false, SourcePos.NONE);
            }
            case "string-append" -> {
                var sb = new StringBuilder();
                for (SchemeValue a : args) {
                    if (!(a instanceof SchemeValue.StringVal s)) throw posError(pos, "string-append: not a string");
                    sb.append(s.value());
                }
                yield new SchemeValue.StringVal(sb.toString(), SourcePos.NONE);
            }
            case "string-length" -> {
                if (args.size() != 1) throw posError(pos, "string-length: need exactly 1 argument");
                if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw posError(pos, "string-length: not a string");
                yield new SchemeValue.IntVal(s.value().length(), SourcePos.NONE);
            }
            case "substring" -> {
                if (args.size() != 3) throw posError(pos, "substring: need exactly 3 arguments");
                if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw posError(pos, "substring: not a string");
                int start = (int) requireInt(args.get(1), pos);
                int end = (int) requireInt(args.get(2), pos);
                yield new SchemeValue.StringVal(s.value().substring(start, end), SourcePos.NONE);
            }
            case "string->number" -> {
                if (args.size() != 1) throw posError(pos, "string->number: need exactly 1 argument");
                if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw posError(pos, "string->number: not a string");
                try { yield new SchemeValue.IntVal(Long.parseLong(s.value()), SourcePos.NONE); }
                catch (NumberFormatException e) { yield new SchemeValue.BoolVal(false, SourcePos.NONE); }
            }
            case "number->string" -> {
                if (args.size() != 1) throw posError(pos, "number->string: need exactly 1 argument");
                yield new SchemeValue.StringVal(String.valueOf(requireInt(args.getFirst(), pos)), SourcePos.NONE);
            }
            case "symbol->string" -> {
                if (args.size() != 1) throw posError(pos, "symbol->string: need exactly 1 argument");
                if (!(args.getFirst() instanceof SchemeValue.SymbolVal s)) throw posError(pos, "symbol->string: not a symbol");
                yield new SchemeValue.StringVal(s.name(), SourcePos.NONE);
            }
            case "string->symbol" -> {
                if (args.size() != 1) throw posError(pos, "string->symbol: need exactly 1 argument");
                if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw posError(pos, "string->symbol: not a string");
                yield new SchemeValue.SymbolVal(s.value(), SourcePos.NONE);
            }
            case "string-ref" -> {
                if (args.size() != 2) throw posError(pos, "string-ref: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw posError(pos, "string-ref: not a string");
                int idx = (int) requireInt(args.get(1), pos);
                yield new SchemeValue.CharVal(s.value().charAt(idx), SourcePos.NONE);
            }
            case "string-set!" -> {
                if (args.size() != 3) throw posError(pos, "string-set!: need exactly 3 arguments");
                if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw posError(pos, "string-set!: not a string");
                if (!s.mutable()) throw posError(pos, "string-set!: strings are immutable");
                int idx = (int) requireInt(args.get(1), pos);
                if (!(args.get(2) instanceof SchemeValue.CharVal c)) throw posError(pos, "string-set!: not a character");
                if (idx < 0 || idx >= s.chars().length) throw posError(pos, "string-set!: index out of range");
                s.chars()[idx] = c.value();
                yield new SchemeValue.BoolVal(false, SourcePos.NONE); // void
            }
            case "string-copy" -> {
                if (args.size() != 1) throw posError(pos, "string-copy: need exactly 1 argument");
                if (!(args.getFirst() instanceof SchemeValue.StringVal s)) throw posError(pos, "string-copy: not a string");
                yield new SchemeValue.StringVal(s.value(), true, SourcePos.NONE);
            }
            case "string?" -> typePredEvaled(args, SchemeValue.StringVal.class, pos);
            case "number?" -> typePredEvaled(args, SchemeValue.IntVal.class, pos);
            case "boolean?" -> typePredEvaled(args, SchemeValue.BoolVal.class, pos);
            case "pair?" -> {
                if (args.size() != 1) throw posError(pos, "pair?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(
                    (args.getFirst() instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) || args.getFirst() instanceof SchemeValue.PairVal,
                    SourcePos.NONE);
            }
            case "symbol?" -> typePredEvaled(args, SchemeValue.SymbolVal.class, pos);
            case "char?" -> typePredEvaled(args, SchemeValue.CharVal.class, pos);
            case "apply" -> {
                if (args.size() < 2) throw posError(pos, "apply: need at least 2 arguments");
                SchemeValue proc = args.getFirst();
                List<SchemeValue> middle = args.subList(1, args.size() - 1);
                SchemeValue lastArg = args.getLast();
                if (!(lastArg instanceof SchemeValue.ListVal lst))
                    throw posError(pos, "apply: last argument must be a list");
                var allArgs = new ArrayList<SchemeValue>(middle);
                allArgs.addAll(lst.elements());
                yield applyTail(proc, allArgs, pos);
            }
            case "call/cc", "call-with-current-continuation" -> {
                if (args.size() != 1) throw posError(pos, "call/cc: need exactly 1 argument");
                SchemeValue proc = args.getFirst();
                int myId = ++callccCounter;
                // Check replay
                if (replayTargetId == myId) {
                    SchemeValue val = replayValue;
                    replayTargetId = -1;
                    replayValue = null;
                    yield val;
                }
                Continuation cont = new Continuation(myId, contStack);
                SchemeValue contVal = new SchemeValue.ContinuationVal(cont);
                try {
                    yield evalContinuation(applyTail(proc, List.of(contVal), pos));
                } catch (ContinuationReturn cr) {
                    if (cr.cont == cont) yield cr.value;
                    throw cr;
                }
            }
            // L12 builtins
            case "equal?" -> {
                if (args.size() != 2) throw posError(pos, "equal?: need exactly 2 arguments");
                yield new SchemeValue.BoolVal(schemeEqual(args.get(0), args.get(1)), SourcePos.NONE);
            }
            case "eq?" -> {
                if (args.size() != 2) throw posError(pos, "eq?: need exactly 2 arguments");
                yield new SchemeValue.BoolVal(schemeEq(args.get(0), args.get(1)), SourcePos.NONE);
            }
            case "map" -> {
                if (args.size() < 2) throw posError(pos, "map: need procedure and at least one list");
                SchemeValue proc = args.get(0);
                List<List<SchemeValue>> lists = new ArrayList<>();
                for (int i = 1; i < args.size(); i++) {
                    if (!(args.get(i) instanceof SchemeValue.ListVal lst))
                        throw posError(pos, "map: not a list");
                    lists.add(lst.elements());
                }
                int len = lists.get(0).size();
                List<SchemeValue> result = new ArrayList<>();
                for (int i = 0; i < len; i++) {
                    List<SchemeValue> mapArgs = new ArrayList<>();
                    for (List<SchemeValue> lst : lists) mapArgs.add(lst.get(i));
                    result.add(evalContinuation(applyTail(proc, mapArgs, pos)));
                }
                yield new SchemeValue.ListVal(result, SourcePos.NONE);
            }
            // L13 builtins
            case "abs" -> {
                if (args.size() != 1) throw posError(pos, "abs: need exactly 1 argument");
                yield new SchemeValue.IntVal(Math.abs(requireInt(args.get(0), pos)), SourcePos.NONE);
            }
            case "modulo" -> {
                if (args.size() != 2) throw posError(pos, "modulo: need exactly 2 arguments");
                long a = requireInt(args.get(0), pos), b = requireInt(args.get(1), pos);
                yield new SchemeValue.IntVal(Math.floorMod(a, b), SourcePos.NONE);
            }
            case "remainder" -> {
                if (args.size() != 2) throw posError(pos, "remainder: need exactly 2 arguments");
                long a = requireInt(args.get(0), pos), b = requireInt(args.get(1), pos);
                yield new SchemeValue.IntVal(a % b, SourcePos.NONE);
            }
            case "quotient" -> {
                if (args.size() != 2) throw posError(pos, "quotient: need exactly 2 arguments");
                long a = requireInt(args.get(0), pos), b = requireInt(args.get(1), pos);
                yield new SchemeValue.IntVal(a / b, SourcePos.NONE);
            }
            case "min" -> {
                if (args.isEmpty()) throw posError(pos, "min: need at least 1 argument");
                long r = requireInt(args.get(0), pos);
                for (int i = 1; i < args.size(); i++) r = Math.min(r, requireInt(args.get(i), pos));
                yield new SchemeValue.IntVal(r, SourcePos.NONE);
            }
            case "max" -> {
                if (args.isEmpty()) throw posError(pos, "max: need at least 1 argument");
                long r = requireInt(args.get(0), pos);
                for (int i = 1; i < args.size(); i++) r = Math.max(r, requireInt(args.get(i), pos));
                yield new SchemeValue.IntVal(r, SourcePos.NONE);
            }
            case "expt" -> {
                if (args.size() != 2) throw posError(pos, "expt: need exactly 2 arguments");
                long base = requireInt(args.get(0), pos), exp = requireInt(args.get(1), pos);
                long r = 1;
                for (long i = 0; i < exp; i++) r *= base;
                yield new SchemeValue.IntVal(r, SourcePos.NONE);
            }
            case "zero?" -> {
                if (args.size() != 1) throw posError(pos, "zero?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(requireInt(args.get(0), pos) == 0, SourcePos.NONE);
            }
            case "positive?" -> {
                if (args.size() != 1) throw posError(pos, "positive?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(requireInt(args.get(0), pos) > 0, SourcePos.NONE);
            }
            case "negative?" -> {
                if (args.size() != 1) throw posError(pos, "negative?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(requireInt(args.get(0), pos) < 0, SourcePos.NONE);
            }
            case "odd?" -> {
                if (args.size() != 1) throw posError(pos, "odd?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(requireInt(args.get(0), pos) % 2 != 0, SourcePos.NONE);
            }
            case "even?" -> {
                if (args.size() != 1) throw posError(pos, "even?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(requireInt(args.get(0), pos) % 2 == 0, SourcePos.NONE);
            }
            case "list-ref" -> {
                if (args.size() != 2) throw posError(pos, "list-ref: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.ListVal lst)) throw posError(pos, "list-ref: not a list");
                int idx = (int) requireInt(args.get(1), pos);
                yield lst.elements().get(idx);
            }
            case "list-tail" -> {
                if (args.size() != 2) throw posError(pos, "list-tail: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.ListVal lst)) throw posError(pos, "list-tail: not a list");
                int idx = (int) requireInt(args.get(1), pos);
                yield new SchemeValue.ListVal(new ArrayList<>(lst.elements().subList(idx, lst.elements().size())), SourcePos.NONE);
            }
            case "list?" -> {
                if (args.size() != 1) throw posError(pos, "list?: need exactly 1 argument");
                yield new SchemeValue.BoolVal(args.get(0) instanceof SchemeValue.ListVal, SourcePos.NONE);
            }
            case "assoc" -> {
                if (args.size() != 2) throw posError(pos, "assoc: need exactly 2 arguments");
                SchemeValue key = args.get(0);
                if (!(args.get(1) instanceof SchemeValue.ListVal alist)) throw posError(pos, "assoc: not a list");
                for (SchemeValue entry : alist.elements()) {
                    if (entry instanceof SchemeValue.ListVal pair && !pair.elements().isEmpty()) {
                        if (schemeEqual(key, pair.elements().get(0)))
                            yield pair;
                    }
                }
                yield new SchemeValue.BoolVal(false, SourcePos.NONE);
            }
            case "char-alphabetic?" -> {
                if (args.size() != 1) throw posError(pos, "char-alphabetic?: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.CharVal c)) throw posError(pos, "char-alphabetic?: not a character");
                yield new SchemeValue.BoolVal(Character.isLetter(c.value()), SourcePos.NONE);
            }
            case "char-numeric?" -> {
                if (args.size() != 1) throw posError(pos, "char-numeric?: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.CharVal c)) throw posError(pos, "char-numeric?: not a character");
                yield new SchemeValue.BoolVal(Character.isDigit(c.value()), SourcePos.NONE);
            }
            case "char-upcase" -> {
                if (args.size() != 1) throw posError(pos, "char-upcase: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.CharVal c)) throw posError(pos, "char-upcase: not a character");
                yield new SchemeValue.CharVal(Character.toUpperCase(c.value()), SourcePos.NONE);
            }
            case "char-downcase" -> {
                if (args.size() != 1) throw posError(pos, "char-downcase: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.CharVal c)) throw posError(pos, "char-downcase: not a character");
                yield new SchemeValue.CharVal(Character.toLowerCase(c.value()), SourcePos.NONE);
            }
            case "char=?" -> {
                if (args.size() != 2) throw posError(pos, "char=?: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.CharVal a)) throw posError(pos, "char=?: not a character");
                if (!(args.get(1) instanceof SchemeValue.CharVal b)) throw posError(pos, "char=?: not a character");
                yield new SchemeValue.BoolVal(a.value() == b.value(), SourcePos.NONE);
            }
            case "char<?" -> {
                if (args.size() != 2) throw posError(pos, "char<?: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.CharVal a)) throw posError(pos, "char<?: not a character");
                if (!(args.get(1) instanceof SchemeValue.CharVal b)) throw posError(pos, "char<?: not a character");
                yield new SchemeValue.BoolVal(a.value() < b.value(), SourcePos.NONE);
            }
            case "string=?" -> {
                if (args.size() != 2) throw posError(pos, "string=?: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.StringVal a)) throw posError(pos, "string=?: not a string");
                if (!(args.get(1) instanceof SchemeValue.StringVal b)) throw posError(pos, "string=?: not a string");
                yield new SchemeValue.BoolVal(a.value().equals(b.value()), SourcePos.NONE);
            }
            case "string<?" -> {
                if (args.size() != 2) throw posError(pos, "string<?: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.StringVal a)) throw posError(pos, "string<?: not a string");
                if (!(args.get(1) instanceof SchemeValue.StringVal b)) throw posError(pos, "string<?: not a string");
                yield new SchemeValue.BoolVal(a.value().compareTo(b.value()) < 0, SourcePos.NONE);
            }
            case "string-ci=?" -> {
                if (args.size() != 2) throw posError(pos, "string-ci=?: need exactly 2 arguments");
                if (!(args.get(0) instanceof SchemeValue.StringVal a)) throw posError(pos, "string-ci=?: not a string");
                if (!(args.get(1) instanceof SchemeValue.StringVal b)) throw posError(pos, "string-ci=?: not a string");
                yield new SchemeValue.BoolVal(a.value().equalsIgnoreCase(b.value()), SourcePos.NONE);
            }
            case "string-upcase" -> {
                if (args.size() != 1) throw posError(pos, "string-upcase: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw posError(pos, "string-upcase: not a string");
                yield new SchemeValue.StringVal(s.value().toUpperCase(), SourcePos.NONE);
            }
            case "string-downcase" -> {
                if (args.size() != 1) throw posError(pos, "string-downcase: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw posError(pos, "string-downcase: not a string");
                yield new SchemeValue.StringVal(s.value().toLowerCase(), SourcePos.NONE);
            }
            // L14 builtins
            case "string->list" -> {
                if (args.size() != 1) throw posError(pos, "string->list: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.StringVal s)) throw posError(pos, "string->list: not a string");
                List<SchemeValue> chars = new java.util.ArrayList<>();
                for (char c : s.chars()) chars.add(new SchemeValue.CharVal(c, SourcePos.NONE));
                yield new SchemeValue.ListVal(chars, SourcePos.NONE);
            }
            case "list->string" -> {
                if (args.size() != 1) throw posError(pos, "list->string: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.ListVal lst)) throw posError(pos, "list->string: not a list");
                char[] cs = new char[lst.elements().size()];
                for (int i = 0; i < lst.elements().size(); i++) {
                    if (!(lst.elements().get(i) instanceof SchemeValue.CharVal cv))
                        throw posError(pos, "list->string: element is not a character");
                    cs[i] = cv.value();
                }
                yield new SchemeValue.StringVal(cs, false, SourcePos.NONE);
            }
            case "char->integer" -> {
                if (args.size() != 1) throw posError(pos, "char->integer: need exactly 1 argument");
                if (!(args.get(0) instanceof SchemeValue.CharVal c)) throw posError(pos, "char->integer: not a character");
                yield new SchemeValue.IntVal(c.value(), SourcePos.NONE);
            }
            case "integer->char" -> {
                if (args.size() != 1) throw posError(pos, "integer->char: need exactly 1 argument");
                long n = requireInt(args.get(0), pos);
                yield new SchemeValue.CharVal((char) n, SourcePos.NONE);
            }
            default -> throw posError(pos, "unknown builtin: " + name);
        };
    }

    private SchemeValue compareEvaled(List<SchemeValue> args, LongPred pred, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "comparison needs at least 2 arguments");
        long prev = requireInt(args.getFirst(), pos);
        for (int i = 1; i < args.size(); i++) {
            long curr = requireInt(args.get(i), pos);
            if (!pred.test(prev, curr)) return new SchemeValue.BoolVal(false, SourcePos.NONE);
            prev = curr;
        }
        return new SchemeValue.BoolVal(true, SourcePos.NONE);
    }

    private SchemeValue typePredEvaled(List<SchemeValue> args, Class<? extends SchemeValue> type, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "type predicate: need exactly 1 argument");
        return new SchemeValue.BoolVal(type.isInstance(args.getFirst()), SourcePos.NONE);
    }

    private long requireInt(SchemeValue v, SourcePos pos) throws EvalError {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw posError(pos, "expected integer, got: " + v.display());
    }

    // --- L12 builtins ---

    private boolean schemeEqual(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.IntVal ai && b instanceof SchemeValue.IntVal bi) return ai.value() == bi.value();
        if (a instanceof SchemeValue.BoolVal ab && b instanceof SchemeValue.BoolVal bb) return ab.value() == bb.value();
        if (a instanceof SchemeValue.StringVal as && b instanceof SchemeValue.StringVal bs) return as.value().equals(bs.value());
        if (a instanceof SchemeValue.SymbolVal as && b instanceof SchemeValue.SymbolVal bs) return as.name().equals(bs.name());
        if (a instanceof SchemeValue.CharVal ac && b instanceof SchemeValue.CharVal bc) return ac.value() == bc.value();
        if (a instanceof SchemeValue.ListVal al && b instanceof SchemeValue.ListVal bl) {
            if (al.elements().size() != bl.elements().size()) return false;
            for (int i = 0; i < al.elements().size(); i++) {
                if (!schemeEqual(al.elements().get(i), bl.elements().get(i))) return false;
            }
            return true;
        }
        return a == b;
    }

    private boolean schemeEq(SchemeValue a, SchemeValue b) {
        if (a instanceof SchemeValue.SymbolVal as && b instanceof SchemeValue.SymbolVal bs) return as.name().equals(bs.name());
        if (a instanceof SchemeValue.IntVal ai && b instanceof SchemeValue.IntVal bi) return ai.value() == bi.value();
        if (a instanceof SchemeValue.BoolVal ab && b instanceof SchemeValue.BoolVal bb) return ab.value() == bb.value();
        if (a instanceof SchemeValue.CharVal ac && b instanceof SchemeValue.CharVal bc) return ac.value() == bc.value();
        return a == b;
    }

    private SchemeValue builtinEqualQ(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "equal?: need exactly 2 arguments");
        SchemeValue a = eval(args.get(0), env), b = eval(args.get(1), env);
        return new SchemeValue.BoolVal(schemeEqual(a, b), SourcePos.NONE);
    }

    private SchemeValue builtinEqQ(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "eq?: need exactly 2 arguments");
        SchemeValue a = eval(args.get(0), env), b = eval(args.get(1), env);
        return new SchemeValue.BoolVal(schemeEq(a, b), SourcePos.NONE);
    }

    private SchemeValue builtinMap(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "map: need procedure and at least one list");
        SchemeValue proc = eval(args.get(0), env);
        List<List<SchemeValue>> lists = new ArrayList<>();
        for (int i = 1; i < args.size(); i++) {
            SchemeValue val = eval(args.get(i), env);
            if (!(val instanceof SchemeValue.ListVal lst)) throw posError(pos, "map: not a list");
            lists.add(lst.elements());
        }
        int len = lists.get(0).size();
        List<SchemeValue> result = new ArrayList<>();
        for (int i = 0; i < len; i++) {
            List<SchemeValue> mapArgs = new ArrayList<>();
            for (List<SchemeValue> lst : lists) mapArgs.add(lst.get(i));
            result.add(evalContinuation(applyTail(proc, mapArgs, pos)));
        }
        return new SchemeValue.ListVal(result, SourcePos.NONE);
    }

    // --- L13 builtins ---

    private SchemeValue builtinAbs(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "abs: need exactly 1 argument");
        return new SchemeValue.IntVal(Math.abs(requireInt(eval(args.get(0), env), pos)), SourcePos.NONE);
    }

    private SchemeValue builtinModulo(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "modulo: need exactly 2 arguments");
        long a = requireInt(eval(args.get(0), env), pos), b = requireInt(eval(args.get(1), env), pos);
        return new SchemeValue.IntVal(Math.floorMod(a, b), SourcePos.NONE);
    }

    private SchemeValue builtinRemainder(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "remainder: need exactly 2 arguments");
        long a = requireInt(eval(args.get(0), env), pos), b = requireInt(eval(args.get(1), env), pos);
        return new SchemeValue.IntVal(a % b, SourcePos.NONE);
    }

    private SchemeValue builtinQuotient(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "quotient: need exactly 2 arguments");
        long a = requireInt(eval(args.get(0), env), pos), b = requireInt(eval(args.get(1), env), pos);
        return new SchemeValue.IntVal(a / b, SourcePos.NONE);
    }

    private SchemeValue builtinMinMax(List<SchemeValue> args, Environment env, SourcePos pos, boolean isMin) throws EvalError {
        if (args.isEmpty()) throw posError(pos, (isMin ? "min" : "max") + ": need at least 1 argument");
        long result = requireInt(eval(args.get(0), env), pos);
        for (int i = 1; i < args.size(); i++) {
            long v = requireInt(eval(args.get(i), env), pos);
            result = isMin ? Math.min(result, v) : Math.max(result, v);
        }
        return new SchemeValue.IntVal(result, SourcePos.NONE);
    }

    private SchemeValue builtinExpt(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "expt: need exactly 2 arguments");
        long base = requireInt(eval(args.get(0), env), pos), exp = requireInt(eval(args.get(1), env), pos);
        long result = 1;
        for (long i = 0; i < exp; i++) result *= base;
        return new SchemeValue.IntVal(result, SourcePos.NONE);
    }

    private SchemeValue builtinSignPred(List<SchemeValue> args, Environment env, SourcePos pos, String kind) throws EvalError {
        if (args.size() != 1) throw posError(pos, kind + "?: need exactly 1 argument");
        long v = requireInt(eval(args.get(0), env), pos);
        boolean result = switch (kind) { case "zero" -> v == 0; case "positive" -> v > 0; case "negative" -> v < 0; default -> false; };
        return new SchemeValue.BoolVal(result, SourcePos.NONE);
    }

    private SchemeValue builtinParityPred(List<SchemeValue> args, Environment env, SourcePos pos, boolean isOdd) throws EvalError {
        if (args.size() != 1) throw posError(pos, (isOdd ? "odd" : "even") + "?: need exactly 1 argument");
        long v = requireInt(eval(args.get(0), env), pos);
        return new SchemeValue.BoolVal(isOdd ? v % 2 != 0 : v % 2 == 0, SourcePos.NONE);
    }

    private SchemeValue builtinListRef(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "list-ref: need exactly 2 arguments");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.ListVal lst)) throw posError(pos, "list-ref: not a list");
        int idx = (int) requireInt(eval(args.get(1), env), pos);
        return lst.elements().get(idx);
    }

    private SchemeValue builtinListTail(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "list-tail: need exactly 2 arguments");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.ListVal lst)) throw posError(pos, "list-tail: not a list");
        int idx = (int) requireInt(eval(args.get(1), env), pos);
        return new SchemeValue.ListVal(new ArrayList<>(lst.elements().subList(idx, lst.elements().size())), SourcePos.NONE);
    }

    private SchemeValue builtinListQ(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "list?: need exactly 1 argument");
        SchemeValue val = eval(args.get(0), env);
        return new SchemeValue.BoolVal(val instanceof SchemeValue.ListVal, SourcePos.NONE);
    }

    private SchemeValue builtinAssoc(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "assoc: need exactly 2 arguments");
        SchemeValue key = eval(args.get(0), env);
        SchemeValue val = eval(args.get(1), env);
        if (!(val instanceof SchemeValue.ListVal alist)) throw posError(pos, "assoc: not a list");
        for (SchemeValue entry : alist.elements()) {
            if (entry instanceof SchemeValue.ListVal pair && !pair.elements().isEmpty()) {
                if (schemeEqual(key, pair.elements().get(0))) return pair;
            }
        }
        return new SchemeValue.BoolVal(false, SourcePos.NONE);
    }

    @FunctionalInterface
    private interface CharPred { boolean test(char c); }

    private SchemeValue builtinCharPred(List<SchemeValue> args, Environment env, SourcePos pos, CharPred pred) throws EvalError {
        if (args.size() != 1) throw posError(pos, "char predicate: need exactly 1 argument");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.CharVal c)) throw posError(pos, "char predicate: not a character");
        return new SchemeValue.BoolVal(pred.test(c.value()), SourcePos.NONE);
    }

    private SchemeValue builtinCharCase(List<SchemeValue> args, Environment env, SourcePos pos, boolean upper) throws EvalError {
        if (args.size() != 1) throw posError(pos, (upper ? "char-upcase" : "char-downcase") + ": need exactly 1 argument");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.CharVal c)) throw posError(pos, "char case: not a character");
        return new SchemeValue.CharVal(upper ? Character.toUpperCase(c.value()) : Character.toLowerCase(c.value()), SourcePos.NONE);
    }

    @FunctionalInterface
    private interface CharCmp { boolean test(char a, char b); }

    private SchemeValue builtinCharCmp(List<SchemeValue> args, Environment env, SourcePos pos, CharCmp cmp) throws EvalError {
        if (args.size() != 2) throw posError(pos, "char comparison: need exactly 2 arguments");
        SchemeValue a = eval(args.get(0), env), b = eval(args.get(1), env);
        if (!(a instanceof SchemeValue.CharVal ca)) throw posError(pos, "char comparison: not a character");
        if (!(b instanceof SchemeValue.CharVal cb)) throw posError(pos, "char comparison: not a character");
        return new SchemeValue.BoolVal(cmp.test(ca.value(), cb.value()), SourcePos.NONE);
    }

    @FunctionalInterface
    private interface StringCmp { boolean test(String a, String b); }

    private SchemeValue builtinStringCmp(List<SchemeValue> args, Environment env, SourcePos pos, StringCmp cmp) throws EvalError {
        if (args.size() != 2) throw posError(pos, "string comparison: need exactly 2 arguments");
        SchemeValue a = eval(args.get(0), env), b = eval(args.get(1), env);
        if (!(a instanceof SchemeValue.StringVal sa)) throw posError(pos, "string comparison: not a string");
        if (!(b instanceof SchemeValue.StringVal sb)) throw posError(pos, "string comparison: not a string");
        return new SchemeValue.BoolVal(cmp.test(sa.value(), sb.value()), SourcePos.NONE);
    }

    private SchemeValue builtinStringCase(List<SchemeValue> args, Environment env, SourcePos pos, boolean upper) throws EvalError {
        if (args.size() != 1) throw posError(pos, (upper ? "string-upcase" : "string-downcase") + ": need exactly 1 argument");
        SchemeValue val = eval(args.get(0), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string case: not a string");
        return new SchemeValue.StringVal(upper ? s.value().toUpperCase() : s.value().toLowerCase(), SourcePos.NONE);
    }

    // --- L11: Hygienic Macros ---

    private SchemeValue evalDefineSyntax(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 2) throw posError(pos, "define-syntax: need name and transformer");
        if (!(args.get(0) instanceof SchemeValue.SymbolVal nameSym))
            throw posError(pos, "define-syntax: name must be a symbol");

        SchemeValue transformer = args.get(1);
        if (!(transformer instanceof SchemeValue.ListVal tList) || tList.elements().size() < 2)
            throw posError(pos, "define-syntax: transformer must be (syntax-rules ...)");
        if (!(tList.elements().get(0) instanceof SchemeValue.SymbolVal srSym) || !srSym.name().equals("syntax-rules"))
            throw posError(pos, "define-syntax: transformer must be syntax-rules");

        List<String> literals = new ArrayList<>();
        if (tList.elements().get(1) instanceof SchemeValue.ListVal litList) {
            for (SchemeValue lit : litList.elements()) {
                if (lit instanceof SchemeValue.SymbolVal s) literals.add(s.name());
            }
        }

        List<SchemeValue> patterns = new ArrayList<>();
        List<SchemeValue> templates = new ArrayList<>();
        for (int i = 2; i < tList.elements().size(); i++) {
            SchemeValue clause = tList.elements().get(i);
            if (!(clause instanceof SchemeValue.ListVal cl) || cl.elements().size() != 2)
                throw posError(pos, "syntax-rules: each clause must be (pattern template)");
            patterns.add(cl.elements().get(0));
            templates.add(cl.elements().get(1));
        }

        SchemeValue macro = new SchemeValue.SyntaxRulesVal(literals, patterns, templates, env);
        env.define(nameSym.name(), macro);
        return macro;
    }

    private SchemeValue expandMacro(SchemeValue.SyntaxRulesVal macro, String keyword,
                                     SchemeValue.ListVal callForm, Environment callEnv) throws EvalError {
        for (int i = 0; i < macro.patterns().size(); i++) {
            Map<String, Object> bindings = new LinkedHashMap<>();
            if (matchPattern(macro.patterns().get(i), callForm, keyword, macro.literals(), bindings)) {
                Set<String> patternVars = bindings.keySet();

                // Collect template-introduced identifiers that need renaming
                Set<String> toRename = new HashSet<>();
                collectIntroduced(macro.templates().get(i), patternVars, keyword, macro.defEnv(), toRename);

                // Generate gensyms
                Map<String, String> renaming = new LinkedHashMap<>();
                for (String name : toRename) {
                    renaming.put(name, name + "$$" + (++gensymCounter));
                }

                // Inject definition-site bindings for renamed identifiers
                for (Map.Entry<String, String> entry : renaming.entrySet()) {
                    try {
                        SchemeValue val = macro.defEnv().lookup(entry.getKey());
                        callEnv.define(entry.getValue(), val);
                    } catch (EvalError e) {
                        // Not bound in def env - fresh binding introduced by macro (e.g., tmp)
                    }
                }

                return substituteTemplate(macro.templates().get(i), bindings, renaming);
            }
        }
        throw new EvalError("no matching pattern for macro " + keyword);
    }

    private boolean matchPattern(SchemeValue pattern, SchemeValue input, String keyword,
                                  List<String> literals, Map<String, Object> bindings) {
        if (pattern instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (name.equals(keyword)) {
                return input instanceof SchemeValue.SymbolVal inSym && inSym.name().equals(keyword);
            }
            if (literals.contains(name)) {
                return input instanceof SchemeValue.SymbolVal inSym && inSym.name().equals(name);
            }
            if (name.equals("...")) return false;
            bindings.put(name, input);
            return true;
        }

        if (pattern instanceof SchemeValue.ListVal patList && input instanceof SchemeValue.ListVal inList) {
            List<SchemeValue> pats = patList.elements();
            List<SchemeValue> ins = inList.elements();

            // Find ellipsis position
            int ellipsisIdx = -1;
            for (int i = 0; i < pats.size(); i++) {
                if (pats.get(i) instanceof SchemeValue.SymbolVal s && s.name().equals("...")) {
                    ellipsisIdx = i;
                    break;
                }
            }

            if (ellipsisIdx >= 0) {
                int beforeEllipsis = ellipsisIdx - 1;
                int fixedAfter = pats.size() - ellipsisIdx - 1;
                int minRequired = beforeEllipsis + fixedAfter;
                if (ins.size() < minRequired) return false;

                // Match fixed elements before the ellipsis variable
                for (int i = 0; i < beforeEllipsis; i++) {
                    if (!matchPattern(pats.get(i), ins.get(i), keyword, literals, bindings)) return false;
                }

                // Match fixed elements after ellipsis
                for (int i = 0; i < fixedAfter; i++) {
                    int patIdx = ellipsisIdx + 1 + i;
                    int inIdx = ins.size() - fixedAfter + i;
                    if (!matchPattern(pats.get(patIdx), ins.get(inIdx), keyword, literals, bindings)) return false;
                }

                // The repeated pattern
                SchemeValue repeatedPat = pats.get(beforeEllipsis);
                int repeatEnd = ins.size() - fixedAfter;

                if (repeatedPat instanceof SchemeValue.SymbolVal repSym &&
                    !repSym.name().equals(keyword) && !literals.contains(repSym.name())) {
                    List<SchemeValue> matches = new ArrayList<>();
                    for (int i = beforeEllipsis; i < repeatEnd; i++) {
                        matches.add(ins.get(i));
                    }
                    bindings.put(repSym.name(), matches);
                    return true;
                }
                return false;
            }

            // No ellipsis: exact length match
            if (pats.size() != ins.size()) return false;
            for (int i = 0; i < pats.size(); i++) {
                if (!matchPattern(pats.get(i), ins.get(i), keyword, literals, bindings)) return false;
            }
            return true;
        }

        return false;
    }

    private void collectIntroduced(SchemeValue template, Set<String> patternVars,
                                    String keyword, Environment defEnv, Set<String> toRename) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (patternVars.contains(name) || name.equals(keyword) || name.equals("...") ||
                SPECIAL_FORMS.contains(name) || isBuiltinName(name)) {
                return;
            }
            // Don't rename references to other macros in defEnv
            try {
                SchemeValue val = defEnv.lookup(name);
                if (val instanceof SchemeValue.SyntaxRulesVal) return;
            } catch (EvalError ignored) {}
            toRename.add(name);
        } else if (template instanceof SchemeValue.ListVal list) {
            for (SchemeValue elem : list.elements()) {
                collectIntroduced(elem, patternVars, keyword, defEnv, toRename);
            }
        }
    }

    private boolean isBuiltinName(String name) {
        for (String bn : BUILTIN_NAMES) {
            if (bn.equals(name)) return true;
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    private SchemeValue substituteTemplate(SchemeValue template, Map<String, Object> bindings,
                                            Map<String, String> renaming) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (bindings.containsKey(name)) {
                Object val = bindings.get(name);
                if (val instanceof SchemeValue sv) return sv;
            }
            if (renaming.containsKey(name)) {
                return new SchemeValue.SymbolVal(renaming.get(name), sym.pos());
            }
            return template;
        }

        if (template instanceof SchemeValue.ListVal list) {
            List<SchemeValue> result = new ArrayList<>();
            List<SchemeValue> elems = list.elements();
            for (int i = 0; i < elems.size(); i++) {
                if (i + 1 < elems.size() && elems.get(i + 1) instanceof SchemeValue.SymbolVal s && s.name().equals("...")) {
                    // Expand ellipsis
                    SchemeValue subTemplate = elems.get(i);
                    String ellipsisVar = findEllipsisVar(subTemplate, bindings);
                    if (ellipsisVar != null) {
                        List<SchemeValue> values = (List<SchemeValue>) bindings.get(ellipsisVar);
                        for (SchemeValue val : values) {
                            Map<String, Object> newBindings = new LinkedHashMap<>(bindings);
                            newBindings.put(ellipsisVar, val);
                            result.add(substituteTemplate(subTemplate, newBindings, renaming));
                        }
                    }
                    i++; // skip the ...
                } else {
                    result.add(substituteTemplate(elems.get(i), bindings, renaming));
                }
            }
            return new SchemeValue.ListVal(result, list.pos());
        }

        return template;
    }

    private String findEllipsisVar(SchemeValue template, Map<String, Object> bindings) {
        if (template instanceof SchemeValue.SymbolVal sym) {
            String name = sym.name();
            if (bindings.containsKey(name) && bindings.get(name) instanceof List) {
                return name;
            }
        } else if (template instanceof SchemeValue.ListVal list) {
            for (SchemeValue elem : list.elements()) {
                String found = findEllipsisVar(elem, bindings);
                if (found != null) return found;
            }
        }
        return null;
    }
}
