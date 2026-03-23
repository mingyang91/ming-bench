package ming;

import java.util.ArrayList;
import java.util.List;

public class Evaluator {

    private final Environment globalEnv = new Environment();
    private StringBuilder outputBuffer = new StringBuilder();

    public String evalStr(String input) throws EvalError {
        outputBuffer.setLength(0);
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return result.display();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        outputBuffer.setLength(0);
        var parser = new Parser(input);
        List<SchemeValue> exprs = parser.parseAll();
        if (exprs.isEmpty()) throw new EvalError("no expressions");
        SchemeValue result = null;
        for (SchemeValue expr : exprs) {
            result = eval(expr, globalEnv);
        }
        return new EvalResult(result.display(), outputBuffer.toString());
    }

    private static EvalError posError(SourcePos pos, String msg) {
        return new EvalError(pos + ": " + msg);
    }

    private SchemeValue eval(SchemeValue expr, Environment env) throws EvalError {
        return switch (expr) {
            case SchemeValue.IntVal v -> v;
            case SchemeValue.BoolVal v -> v;
            case SchemeValue.StringVal v -> v;
            case SchemeValue.LambdaVal v -> v;
            case SchemeValue.CharVal v -> v;
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
                case "if" -> { return evalIf(args, env, pos); }
                case "quote" -> {
                    if (args.size() != 1) throw posError(pos, "quote: need exactly one argument");
                    return args.getFirst();
                }
                case "lambda" -> { return evalLambda(args, env, pos); }
                case "let" -> { return evalLet(args, env, pos); }
                case "begin" -> { return evalBegin(args, env, pos); }
                case "cond" -> { return evalCond(args, env, pos); }
                default -> {
                    SchemeValue builtinResult = tryBuiltin(op, args, env, pos);
                    if (builtinResult != null) return builtinResult;
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
        return apply(proc, evaledArgs, pos);
    }

    private SchemeValue evalDefine(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "define: need at least 2 arguments");
        SchemeValue target = args.getFirst();

        if (target instanceof SchemeValue.SymbolVal sym) {
            SchemeValue val = eval(args.get(1), env);
            env.define(sym.name(), val);
            return val;
        } else if (target instanceof SchemeValue.ListVal nameAndParams) {
            List<SchemeValue> elems = nameAndParams.elements();
            if (elems.isEmpty()) throw posError(pos, "define: empty name list");
            if (!(elems.getFirst() instanceof SchemeValue.SymbolVal nameSym))
                throw posError(pos, "define: name must be a symbol");

            List<String> params = new ArrayList<>();
            for (int i = 1; i < elems.size(); i++) {
                if (!(elems.get(i) instanceof SchemeValue.SymbolVal p))
                    throw posError(pos, "define: parameter must be a symbol");
                params.add(p.name());
            }
            List<SchemeValue> body = args.subList(1, args.size());
            SchemeValue lambda = new SchemeValue.LambdaVal(params, body, env);
            env.define(nameSym.name(), lambda);
            return lambda;
        }
        throw posError(pos, "define: invalid syntax");
    }

    private SchemeValue evalIf(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2 || args.size() > 3) throw posError(pos, "if: need 2 or 3 arguments");
        SchemeValue cond = eval(args.get(0), env);
        if (cond.isTruthy()) {
            return eval(args.get(1), env);
        } else if (args.size() == 3) {
            return eval(args.get(2), env);
        }
        return new SchemeValue.BoolVal(false, SourcePos.NONE);
    }

    private SchemeValue evalLambda(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() < 2) throw posError(pos, "lambda: need params and body");
        SchemeValue paramSpec = args.getFirst();
        if (!(paramSpec instanceof SchemeValue.ListVal paramList))
            throw posError(pos, "lambda: params must be a list");

        List<String> params = new ArrayList<>();
        for (SchemeValue p : paramList.elements()) {
            if (!(p instanceof SchemeValue.SymbolVal sym))
                throw posError(pos, "lambda: parameter must be a symbol");
            params.add(sym.name());
        }
        List<SchemeValue> body = args.subList(1, args.size());
        return new SchemeValue.LambdaVal(params, body, env);
    }

    private SchemeValue apply(SchemeValue proc, List<SchemeValue> args, SourcePos pos) throws EvalError {
        if (proc instanceof SchemeValue.LambdaVal lambda) {
            if (args.size() != lambda.params().size())
                throw posError(pos, "wrong number of arguments: expected " + lambda.params().size() + ", got " + args.size());
            Environment callEnv = new Environment(lambda.env());
            for (int i = 0; i < lambda.params().size(); i++) {
                callEnv.define(lambda.params().get(i), args.get(i));
            }
            SchemeValue result = null;
            for (SchemeValue expr : lambda.body()) {
                result = eval(expr, callEnv);
            }
            return result;
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
            case "and" -> and(args, env);
            case "or" -> or(args, env);
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

    private SchemeValue and(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(true, SourcePos.NONE);
        for (SchemeValue arg : args) {
            result = eval(arg, env);
            if (!result.isTruthy()) return result;
        }
        return result;
    }

    private SchemeValue or(List<SchemeValue> args, Environment env) throws EvalError {
        SchemeValue result = new SchemeValue.BoolVal(false, SourcePos.NONE);
        for (SchemeValue arg : args) {
            result = eval(arg, env);
            if (result.isTruthy()) return result;
        }
        return result;
    }

    // --- L03 special forms ---

    private SchemeValue evalLet(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
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
            SchemeValue lambda = new SchemeValue.LambdaVal(params, body, letEnv);
            letEnv.define(nameSym.name(), lambda);

            List<SchemeValue> evaledInits = new ArrayList<>();
            for (SchemeValue init : inits) {
                evaledInits.add(eval(init, env));
            }
            return apply(lambda, evaledInits, pos);
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

        SchemeValue result = null;
        for (int i = 1; i < args.size(); i++) {
            result = eval(args.get(i), letEnv);
        }
        return result;
    }

    private SchemeValue evalBegin(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.isEmpty()) throw posError(pos, "begin: need at least one expression");
        SchemeValue result = null;
        for (SchemeValue arg : args) {
            result = eval(arg, env);
        }
        return result;
    }

    private SchemeValue evalCond(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        for (SchemeValue clause : args) {
            if (!(clause instanceof SchemeValue.ListVal clauseList) || clauseList.elements().isEmpty())
                throw posError(pos, "cond: invalid clause");
            List<SchemeValue> elems = clauseList.elements();
            SchemeValue test = elems.getFirst();

            if (test instanceof SchemeValue.SymbolVal sym && sym.name().equals("else")) {
                SchemeValue result = null;
                for (int i = 1; i < elems.size(); i++) {
                    result = eval(elems.get(i), env);
                }
                return result;
            }

            SchemeValue testVal = eval(test, env);
            if (testVal.isTruthy()) {
                SchemeValue result = testVal;
                for (int i = 1; i < elems.size(); i++) {
                    result = eval(elems.get(i), env);
                }
                return result;
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
        throw posError(pos, "cons: second argument must be a proper list (for now)");
    }

    private SchemeValue builtinCar(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "car: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) {
            return lst.elements().getFirst();
        }
        throw posError(pos, "car: not a pair");
    }

    private SchemeValue builtinCdr(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "cdr: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty()) {
            return new SchemeValue.ListVal(lst.elements().subList(1, lst.elements().size()), SourcePos.NONE);
        }
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
        return new SchemeValue.BoolVal(val instanceof SchemeValue.ListVal lst && !lst.elements().isEmpty(), SourcePos.NONE);
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
        int idx = (int) requireInt(eval(args.get(1), env), pos);
        SchemeValue charVal = eval(args.get(2), env);
        if (!(charVal instanceof SchemeValue.CharVal c)) throw posError(pos, "string-set!: not a character");
        s.chars()[idx] = c.value();
        return new SchemeValue.BoolVal(false, SourcePos.NONE); // void
    }

    private SchemeValue builtinStringCopy(List<SchemeValue> args, Environment env, SourcePos pos) throws EvalError {
        if (args.size() != 1) throw posError(pos, "string-copy: need exactly 1 argument");
        SchemeValue val = eval(args.getFirst(), env);
        if (!(val instanceof SchemeValue.StringVal s)) throw posError(pos, "string-copy: not a string");
        return new SchemeValue.StringVal(s.value(), SourcePos.NONE);
    }

    private long requireInt(SchemeValue v, SourcePos pos) throws EvalError {
        if (v instanceof SchemeValue.IntVal i) return i.value();
        throw posError(pos, "expected integer, got: " + v.display());
    }
}
