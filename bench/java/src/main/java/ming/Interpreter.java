package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class Interpreter {
    private static final BooleanValue TRUE = new BooleanValue(true);
    private static final BooleanValue FALSE = new BooleanValue(false);
    private static final EmptyListValue EMPTY_LIST = EmptyListValue.INSTANCE;
    private static final VoidValue VOID = VoidValue.INSTANCE;

    private final Environment globalEnv;
    private final StringBuilder output;

    Interpreter() {
        this.output = new StringBuilder();
        this.globalEnv = createGlobalEnv();
    }

    EvalResult evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input is empty");
        }

        Value lastValue = FALSE;
        for (Expr expression : expressions) {
            lastValue = eval(expression, globalEnv);
        }
        return new EvalResult(lastValue.render(), output.toString());
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment(null);
        env.define("+", new BuiltinProcedure("+", this::applyAdd));
        env.define("-", new BuiltinProcedure("-", this::applySubtract));
        env.define("*", new BuiltinProcedure("*", this::applyMultiply));
        env.define("/", new BuiltinProcedure("/", this::applyDivide));
        env.define("<", new BuiltinProcedure("<", this::applyLessThan));
        env.define(">", new BuiltinProcedure(">", this::applyGreaterThan));
        env.define("=", new BuiltinProcedure("=", this::applyNumericEquals));
        env.define("<=", new BuiltinProcedure("<=", this::applyLessEqual));
        env.define("not", new BuiltinProcedure("not", this::applyNot));
        env.define("display", new BuiltinProcedure("display", this::applyDisplay));
        env.define("write", new BuiltinProcedure("write", this::applyWrite));
        env.define("newline", new BuiltinProcedure("newline", this::applyNewline));
        env.define("cons", new BuiltinProcedure("cons", this::applyCons));
        env.define("car", new BuiltinProcedure("car", this::applyCar));
        env.define("cdr", new BuiltinProcedure("cdr", this::applyCdr));
        env.define("null?", new BuiltinProcedure("null?", this::applyNullPredicate));
        env.define("list", new BuiltinProcedure("list", this::applyList));
        env.define("length", new BuiltinProcedure("length", this::applyLength));
        env.define("append", new BuiltinProcedure("append", this::applyAppend));
        env.define("string-append", new BuiltinProcedure("string-append", this::applyStringAppend));
        env.define("string-length", new BuiltinProcedure("string-length", this::applyStringLength));
        env.define("substring", new BuiltinProcedure("substring", this::applySubstring));
        env.define("string-copy", new BuiltinProcedure("string-copy", this::applyStringCopy));
        env.define("string-set!", new BuiltinProcedure("string-set!", this::applyStringSet));
        env.define("string->number", new BuiltinProcedure("string->number", this::applyStringToNumber));
        env.define("number->string", new BuiltinProcedure("number->string", this::applyNumberToString));
        env.define("symbol->string", new BuiltinProcedure("symbol->string", this::applySymbolToString));
        env.define("string->symbol", new BuiltinProcedure("string->symbol", this::applyStringToSymbol));
        env.define("string-ref", new BuiltinProcedure("string-ref", this::applyStringRef));
        env.define("char?", new BuiltinProcedure("char?", this::applyCharPredicate));
        env.define("string?", new BuiltinProcedure("string?", this::applyStringPredicate));
        env.define("number?", new BuiltinProcedure("number?", this::applyNumberPredicate));
        env.define("boolean?", new BuiltinProcedure("boolean?", this::applyBooleanPredicate));
        env.define("pair?", new BuiltinProcedure("pair?", this::applyPairPredicate));
        env.define("symbol?", new BuiltinProcedure("symbol?", this::applySymbolPredicate));
        return env;
    }

    private Value eval(Expr expression, Environment env) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.codePoint());
            case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name(), symbolExpr.loc());
            case ListExpr listExpr -> evalList(listExpr, env);
        };
    }

    private Value evalList(ListExpr listExpr, Environment env) throws EvalError {
        if (listExpr.elements().isEmpty()) {
            throw error(listExpr.loc(), "cannot evaluate empty list");
        }

        Expr head = listExpr.elements().getFirst();
        if (head instanceof SymbolExpr symbolExpr) {
            String symbolName = symbolExpr.name();
            if ("define".equals(symbolName)) {
                return evalDefine(listExpr, env);
            }
            if ("if".equals(symbolName)) {
                return evalIf(listExpr, env);
            }
            if ("quote".equals(symbolName)) {
                return evalQuote(listExpr);
            }
            if ("lambda".equals(symbolName)) {
                return evalLambda(listExpr, env);
            }
            if ("set!".equals(symbolName)) {
                return evalSet(listExpr, env);
            }
            if ("begin".equals(symbolName)) {
                return evalBegin(listExpr, env);
            }
            if ("let".equals(symbolName)) {
                return evalLet(listExpr, env);
            }
            if ("cond".equals(symbolName)) {
                return evalCond(listExpr, env);
            }
            if ("and".equals(symbolName)) {
                return evalAnd(listExpr.elements().subList(1, listExpr.elements().size()), env);
            }
            if ("or".equals(symbolName)) {
                return evalOr(listExpr.elements().subList(1, listExpr.elements().size()), env);
            }
        }

        Value procedureValue = eval(head, env);
        if (!(procedureValue instanceof Procedure procedure)) {
            throw error(head.loc(), "attempted to call a non-procedure");
        }

        List<Value> arguments = new ArrayList<>();
        for (int index = 1; index < listExpr.elements().size(); index++) {
            arguments.add(eval(listExpr.elements().get(index), env));
        }
        return procedure.apply(arguments, listExpr.loc());
    }

    private Value evalDefine(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("define", listExpr, 3);

        Expr target = listExpr.elements().get(1);
        if (target instanceof SymbolExpr symbolExpr) {
            if (listExpr.elements().size() != 3) {
                throw error(listExpr.loc(),
                        "define expected 2 arguments but got " + (listExpr.elements().size() - 1));
            }
            Value value = eval(listExpr.elements().get(2), env);
            env.define(symbolExpr.name(), value);
            return VOID;
        }

        if (target instanceof ListExpr signature) {
            if (signature.elements().isEmpty()) {
                throw error(target.loc(), "define requires a function name");
            }
            Expr nameExpr = signature.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr nameSymbol)) {
                throw error(nameExpr.loc(), "define requires a function name");
            }

            List<String> parameters = parseParameters(
                    signature.elements().subList(1, signature.elements().size()),
                    "define"
            );
            List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
            UserProcedure procedure = new UserProcedure(nameSymbol.name(), parameters, body, env);
            env.define(nameSymbol.name(), procedure);
            return VOID;
        }

        throw error(target.loc(), "define requires a symbol or parameter list");
    }

    private Value evalIf(ListExpr listExpr, Environment env) throws EvalError {
        ensureExactlyExpressions("if", listExpr, 4);
        Value condition = eval(listExpr.elements().get(1), env);
        Expr branch = condition.isTruthy() ? listExpr.elements().get(2) : listExpr.elements().get(3);
        return eval(branch, env);
    }

    private Value evalQuote(ListExpr listExpr) throws EvalError {
        ensureExactlyExpressions("quote", listExpr, 2);
        return quoteToValue(listExpr.elements().get(1));
    }

    private Value evalLambda(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("lambda", listExpr, 3);
        Expr parametersExpr = listExpr.elements().get(1);
        if (!(parametersExpr instanceof ListExpr parametersList)) {
            throw error(parametersExpr.loc(), "lambda requires a parameter list");
        }

        List<String> parameters = parseParameters(parametersList.elements(), "lambda");
        List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
        return new UserProcedure("lambda", parameters, body, env);
    }

    private Value evalSet(ListExpr listExpr, Environment env) throws EvalError {
        ensureExactlyExpressions("set!", listExpr, 3);

        Expr targetExpr = listExpr.elements().get(1);
        if (!(targetExpr instanceof SymbolExpr symbolExpr)) {
            throw error(targetExpr.loc(), "set! requires a symbol");
        }

        Value value = eval(listExpr.elements().get(2), env);
        env.set(symbolExpr.name(), value, symbolExpr.loc());
        return VOID;
    }

    private Value evalBegin(ListExpr listExpr, Environment env) throws EvalError {
        return evalSequence(listExpr.elements().subList(1, listExpr.elements().size()), env);
    }

    private Value evalLet(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("let", listExpr, 3);

        Expr secondExpr = listExpr.elements().get(1);
        if (secondExpr instanceof SymbolExpr nameSymbol) {
            if (listExpr.elements().size() < 4) {
                throw error(listExpr.loc(),
                        "let expected at least 2 arguments but got "
                                + (listExpr.elements().size() - 1));
            }

            Expr bindingExpr = listExpr.elements().get(2);
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error(bindingExpr.loc(), "let requires a binding list");
            }

            BindingParseResult bindings = parseBindings(bindingList, env);
            List<Expr> body = List.copyOf(listExpr.elements().subList(3, listExpr.elements().size()));

            Environment namedLetEnv = new Environment(env);
            UserProcedure procedure = new UserProcedure(
                    nameSymbol.name(),
                    bindings.names(),
                    body,
                    namedLetEnv
            );
            namedLetEnv.define(nameSymbol.name(), procedure);
            return procedure.apply(bindings.values(), listExpr.loc());
        }

        if (!(secondExpr instanceof ListExpr bindingList)) {
            throw error(secondExpr.loc(), "let requires a binding list");
        }

        BindingParseResult bindings = parseBindings(bindingList, env);
        Environment letEnv = new Environment(env);
        for (int index = 0; index < bindings.names().size(); index++) {
            letEnv.define(bindings.names().get(index), bindings.values().get(index));
        }
        return evalSequence(listExpr.elements().subList(2, listExpr.elements().size()), letEnv);
    }

    private Value evalCond(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("cond", listExpr, 2);

        List<Expr> clauses = listExpr.elements().subList(1, listExpr.elements().size());
        for (int clauseIndex = 0; clauseIndex < clauses.size(); clauseIndex++) {
            Expr clauseExpr = clauses.get(clauseIndex);
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw error(clauseExpr.loc(), "cond clauses must be lists");
            }
            if (clauseList.elements().isEmpty()) {
                throw error(clauseExpr.loc(), "cond clauses cannot be empty");
            }

            Expr testExpr = clauseList.elements().getFirst();
            boolean isElseClause = testExpr instanceof SymbolExpr symbolExpr
                    && "else".equals(symbolExpr.name());
            if (isElseClause) {
                if (clauseIndex != clauses.size() - 1) {
                    throw error(testExpr.loc(), "cond else clause must be last");
                }
                return evalSequence(clauseList.elements().subList(1, clauseList.elements().size()), env);
            }

            Value testValue = eval(testExpr, env);
            if (!testValue.isTruthy()) {
                continue;
            }
            if (clauseList.elements().size() == 1) {
                return testValue;
            }
            return evalSequence(clauseList.elements().subList(1, clauseList.elements().size()), env);
        }

        return VOID;
    }

    private Value evalAnd(List<Expr> expressions, Environment env) throws EvalError {
        Value lastValue = TRUE;
        for (Expr expression : expressions) {
            Value value = eval(expression, env);
            if (!value.isTruthy()) {
                return value;
            }
            lastValue = value;
        }
        return lastValue;
    }

    private Value evalOr(List<Expr> expressions, Environment env) throws EvalError {
        for (Expr expression : expressions) {
            Value value = eval(expression, env);
            if (value.isTruthy()) {
                return value;
            }
        }
        return FALSE;
    }

    private Value applyAdd(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        Rational result = Rational.ZERO;
        for (Value argument : arguments) {
            result = result.add(requireNumber(argument, "+", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applySubtract(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("-", arguments, 1, callLoc);
        Rational result = requireNumber(arguments.getFirst(), "-", callLoc);
        if (arguments.size() == 1) {
            return new NumberValue(result.negate());
        }

        for (int index = 1; index < arguments.size(); index++) {
            result = result.subtract(requireNumber(arguments.get(index), "-", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applyMultiply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        Rational result = Rational.ONE;
        for (Value argument : arguments) {
            result = result.multiply(requireNumber(argument, "*", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applyDivide(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("/", arguments, 1, callLoc);
        Rational result = arguments.size() == 1
                ? Rational.ONE.divide(requireNumber(arguments.getFirst(), "/", callLoc), callLoc)
                : requireNumber(arguments.getFirst(), "/", callLoc);

        int startIndex = arguments.size() == 1 ? 1 : 1;
        for (int index = startIndex; index < arguments.size(); index++) {
            result = result.divide(requireNumber(arguments.get(index), "/", callLoc), callLoc);
        }
        return new NumberValue(result);
    }

    private Value applyLessThan(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, "<", (left, right) -> left.compareTo(right) < 0);
    }

    private Value applyGreaterThan(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, ">", (left, right) -> left.compareTo(right) > 0);
    }

    private Value applyNumericEquals(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, "=", Rational::equals);
    }

    private Value applyLessEqual(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        return applyComparison(arguments, callLoc, "<=", (left, right) -> left.compareTo(right) <= 0);
    }

    private Value applyNot(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("not", arguments, 1, callLoc);
        return arguments.getFirst().isTruthy() ? FALSE : TRUE;
    }

    private Value applyDisplay(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("display", arguments, 1, callLoc);
        output.append(arguments.getFirst().displayRender());
        return VOID;
    }

    private Value applyWrite(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("write", arguments, 1, callLoc);
        output.append(arguments.getFirst().render());
        return VOID;
    }

    private Value applyNewline(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("newline", arguments, 0, callLoc);
        output.append('\n');
        return VOID;
    }

    private Value applyCons(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("cons", arguments, 2, callLoc);
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private Value applyCar(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("car", arguments, 1, callLoc);
        return requirePair(arguments.getFirst(), "car", callLoc).car();
    }

    private Value applyCdr(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("cdr", arguments, 1, callLoc);
        return requirePair(arguments.getFirst(), "cdr", callLoc).cdr();
    }

    private Value applyNullPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("null?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof EmptyListValue ? TRUE : FALSE;
    }

    private Value applyList(List<Value> arguments, SourceLoc callLoc) {
        return buildList(arguments);
    }

    private Value applyLength(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("length", arguments, 1, callLoc);
        List<Value> elements = requireProperList(arguments.getFirst(), "length", callLoc);
        return new NumberValue(Rational.integer(BigInteger.valueOf(elements.size())));
    }

    private Value applyAppend(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        if (arguments.isEmpty()) {
            return EMPTY_LIST;
        }

        Value result = arguments.get(arguments.size() - 1);
        for (int index = arguments.size() - 2; index >= 0; index--) {
            List<Value> elements = requireProperList(arguments.get(index), "append", callLoc);
            for (int elementIndex = elements.size() - 1; elementIndex >= 0; elementIndex--) {
                result = new PairValue(elements.get(elementIndex), result);
            }
        }
        return result;
    }

    private Value applyStringAppend(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(requireString(argument, "string-append", callLoc));
        }
        return new StringValue(builder.toString());
    }

    private Value applyStringLength(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-length", arguments, 1, callLoc);
        int length = requireString(arguments.getFirst(), "string-length", callLoc)
                .codePointCount(0, requireString(arguments.getFirst(), "string-length", callLoc).length());
        return new NumberValue(Rational.integer(BigInteger.valueOf(length)));
    }

    private Value applySubstring(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("substring", arguments, 3, callLoc);
        String value = requireString(arguments.get(0), "substring", callLoc);
        int start = requireIndex(arguments.get(1), "substring", callLoc);
        int end = requireIndex(arguments.get(2), "substring", callLoc);
        int length = value.codePointCount(0, value.length());
        if (start > end || end > length) {
            throw error(callLoc, "substring indices are out of bounds");
        }

        int startOffset = value.offsetByCodePoints(0, start);
        int endOffset = value.offsetByCodePoints(0, end);
        return new StringValue(value.substring(startOffset, endOffset));
    }

    private Value applyStringCopy(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-copy", arguments, 1, callLoc);
        return requireStringValue(arguments.getFirst(), "string-copy", callLoc).copy(true);
    }

    private Value applyStringSet(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-set!", arguments, 3, callLoc);
        StringValue stringValue = requireStringValue(arguments.get(0), "string-set!", callLoc);
        int index = requireIndex(arguments.get(1), "string-set!", callLoc);
        CharValue charValue = requireChar(arguments.get(2), "string-set!", callLoc);
        stringValue.setCodePoint(index, charValue.codePoint(), callLoc);
        return VOID;
    }

    private Value applyStringToNumber(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string->number", arguments, 1, callLoc);
        String value = requireString(arguments.getFirst(), "string->number", callLoc);
        Rational parsed = parseNumberLiteral(value);
        if (parsed == null) {
            return FALSE;
        }
        return new NumberValue(parsed);
    }

    private Value applyNumberToString(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("number->string", arguments, 1, callLoc);
        return new StringValue(requireNumber(arguments.getFirst(), "number->string", callLoc).render());
    }

    private Value applySymbolToString(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("symbol->string", arguments, 1, callLoc);
        return new StringValue(requireSymbol(arguments.getFirst(), "symbol->string", callLoc));
    }

    private Value applyStringToSymbol(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string->symbol", arguments, 1, callLoc);
        return new SymbolValue(requireString(arguments.getFirst(), "string->symbol", callLoc));
    }

    private Value applyStringRef(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-ref", arguments, 2, callLoc);
        String value = requireString(arguments.get(0), "string-ref", callLoc);
        int index = requireIndex(arguments.get(1), "string-ref", callLoc);
        int codePointLength = value.codePointCount(0, value.length());
        if (index >= codePointLength) {
            throw error(callLoc, "string-ref index is out of bounds");
        }
        int charOffset = value.offsetByCodePoints(0, index);
        return new CharValue(value.codePointAt(charOffset));
    }

    private Value applyStringPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof StringValue ? TRUE : FALSE;
    }

    private Value applyNumberPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("number?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof NumberValue ? TRUE : FALSE;
    }

    private Value applyBooleanPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("boolean?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof BooleanValue ? TRUE : FALSE;
    }

    private Value applyPairPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("pair?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof PairValue ? TRUE : FALSE;
    }

    private Value applySymbolPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("symbol?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof SymbolValue ? TRUE : FALSE;
    }

    private Value applyCharPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("char?", arguments, 1, callLoc);
        return arguments.getFirst() instanceof CharValue ? TRUE : FALSE;
    }

    private Value applyComparison(
            List<Value> arguments,
            SourceLoc callLoc,
            String name,
            RationalComparison comparison
    ) throws EvalError {
        ensureAtLeast(name, arguments, 2, callLoc);
        Rational previous = requireNumber(arguments.getFirst(), name, callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            Rational current = requireNumber(arguments.get(index), name, callLoc);
            if (!comparison.test(previous, current)) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        if (expressions.isEmpty()) {
            return VOID;
        }

        Value lastValue = VOID;
        for (Expr expression : expressions) {
            lastValue = eval(expression, env);
        }
        return lastValue;
    }

    private Value quoteToValue(Expr expression) throws EvalError {
        return switch (expression) {
            case NumberExpr numberExpr -> new NumberValue(numberExpr.value());
            case BooleanExpr booleanExpr -> booleanExpr.value() ? TRUE : FALSE;
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.codePoint());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> quoteList(listExpr.elements());
        };
    }

    private Value quoteList(List<Expr> expressions) throws EvalError {
        Value result = EMPTY_LIST;
        for (int index = expressions.size() - 1; index >= 0; index--) {
            result = new PairValue(quoteToValue(expressions.get(index)), result);
        }
        return result;
    }

    private BindingParseResult parseBindings(ListExpr bindingsList, Environment env) throws EvalError {
        List<String> names = new ArrayList<>(bindingsList.elements().size());
        List<Value> values = new ArrayList<>(bindingsList.elements().size());

        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error(bindingExpr.loc(), "let bindings must be lists");
            }
            if (bindingList.elements().size() != 2) {
                throw error(bindingExpr.loc(), "let bindings must contain a name and value");
            }

            Expr nameExpr = bindingList.elements().getFirst();
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw error(nameExpr.loc(), "let binding names must be symbols");
            }

            names.add(symbolExpr.name());
            values.add(eval(bindingList.elements().get(1), env));
        }

        return new BindingParseResult(List.copyOf(names), List.copyOf(values));
    }

    private List<String> parseParameters(List<Expr> parameterExprs, String formName)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        for (Expr parameterExpr : parameterExprs) {
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error(parameterExpr.loc(), formName + " parameters must be symbols");
            }
            parameters.add(symbolExpr.name());
        }
        return List.copyOf(parameters);
    }

    private Rational requireNumber(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof NumberValue numberValue) {
            return numberValue.value();
        }
        throw error(callLoc, procedureName + " expects numeric arguments");
    }

    private String requireString(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        return requireStringValue(value, procedureName, callLoc).text();
    }

    private StringValue requireStringValue(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw error(callLoc, procedureName + " expects string arguments");
    }

    private String requireSymbol(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw error(callLoc, procedureName + " expects symbol arguments");
    }

    private CharValue requireChar(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue;
        }
        throw error(callLoc, procedureName + " expects character arguments");
    }

    private int requireIndex(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        Rational number = requireNumber(value, procedureName, callLoc);
        if (!number.denominator().equals(BigInteger.ONE)) {
            throw error(callLoc, procedureName + " expects a non-negative index");
        }
        BigInteger integer = number.numerator();
        if (integer.signum() < 0) {
            throw error(callLoc, procedureName + " expects a non-negative index");
        }
        if (integer.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            throw error(callLoc, procedureName + " index is too large");
        }
        return integer.intValueExact();
    }

    private PairValue requirePair(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw error(callLoc, procedureName + " expects a pair");
    }

    private List<Value> requireProperList(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        List<Value> elements = new ArrayList<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return elements;
        }
        throw error(callLoc, procedureName + " expects a proper list");
    }

    private Value buildList(List<Value> elements) {
        Value result = EMPTY_LIST;
        for (int index = elements.size() - 1; index >= 0; index--) {
            result = new PairValue(elements.get(index), result);
        }
        return result;
    }

    private void ensureExactly(
            String procedureName,
            List<Value> arguments,
            int expected,
            SourceLoc callLoc
    ) throws EvalError {
        if (arguments.size() != expected) {
            throw error(callLoc,
                    procedureName + " expected " + expected + " arguments but got "
                            + arguments.size());
        }
    }

    private void ensureAtLeast(
            String procedureName,
            List<Value> arguments,
            int minimum,
            SourceLoc callLoc
    ) throws EvalError {
        if (arguments.size() < minimum) {
            throw error(callLoc,
                    procedureName + " expected at least " + minimum + " arguments but got "
                            + arguments.size());
        }
    }

    private void ensureExactlyExpressions(String formName, ListExpr listExpr, int expectedSize)
            throws EvalError {
        if (listExpr.elements().size() != expectedSize) {
            throw error(listExpr.loc(),
                    formName + " expected " + (expectedSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1));
        }
    }

    private void ensureAtLeastExpressions(String formName, ListExpr listExpr, int minimumSize)
            throws EvalError {
        if (listExpr.elements().size() < minimumSize) {
            throw error(listExpr.loc(),
                    formName + " expected at least " + (minimumSize - 1) + " arguments but got "
                            + (listExpr.elements().size() - 1));
        }
    }

    private static EvalError error(SourceLoc loc, String message) {
        return new EvalError(message + " at " + loc.line() + ":" + loc.column());
    }

    @FunctionalInterface
    private interface BuiltinInvoker {
        Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
    }

    @FunctionalInterface
    private interface RationalComparison {
        boolean test(Rational left, Rational right);
    }

    private interface Procedure {
        Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError;
    }

    private sealed interface Expr permits NumberExpr, BooleanExpr, StringExpr, CharExpr, SymbolExpr, ListExpr {
        SourceLoc loc();
    }

    private sealed interface Value permits NumberValue,
            BooleanValue,
            StringValue,
            CharValue,
            SymbolValue,
            PairValue,
            EmptyListValue,
            BuiltinProcedure,
            UserProcedure,
            VoidValue {
        String render();

        default String displayRender() {
            return render();
        }

        default boolean isTruthy() {
            return true;
        }
    }

    private record SourceLoc(int line, int column) {
    }

    private record BindingParseResult(List<String> names, List<Value> values) {
    }

    private record NumberExpr(SourceLoc loc, Rational value) implements Expr {
    }

    private record BooleanExpr(SourceLoc loc, boolean value) implements Expr {
    }

    private record StringExpr(SourceLoc loc, String value) implements Expr {
    }

    private record CharExpr(SourceLoc loc, int codePoint) implements Expr {
    }

    private record SymbolExpr(SourceLoc loc, String name) implements Expr {
    }

    private record ListExpr(SourceLoc loc, List<Expr> elements) implements Expr {
    }

    private record NumberValue(Rational value) implements Value {
        @Override
        public String render() {
            return value.render();
        }
    }

    private record BooleanValue(boolean value) implements Value {
        @Override
        public String render() {
            return value ? "#t" : "#f";
        }

        @Override
        public boolean isTruthy() {
            return value;
        }
    }

    private static final class StringValue implements Value {
        private final StringBuilder contents;
        private final boolean mutable;

        private StringValue(String value) {
            this(value, false);
        }

        private StringValue(String value, boolean mutable) {
            this.contents = new StringBuilder(value);
            this.mutable = mutable;
        }

        @Override
        public String render() {
            return renderString(text());
        }

        @Override
        public String displayRender() {
            return text();
        }

        private String text() {
            return contents.toString();
        }

        private StringValue copy(boolean mutableCopy) {
            return new StringValue(text(), mutableCopy);
        }

        private void setCodePoint(int index, int codePoint, SourceLoc callLoc) throws EvalError {
            if (!mutable) {
                throw error(callLoc, "string-set! expects a mutable string");
            }

            int length = contents.codePointCount(0, contents.length());
            if (index >= length) {
                throw error(callLoc, "string-set! index is out of bounds");
            }

            int startOffset = contents.offsetByCodePoints(0, index);
            int endOffset = contents.offsetByCodePoints(startOffset, 1);
            contents.replace(startOffset, endOffset, new String(Character.toChars(codePoint)));
        }
    }

    private record CharValue(int codePoint) implements Value {
        @Override
        public String render() {
            return renderChar(codePoint);
        }

        @Override
        public String displayRender() {
            return new String(Character.toChars(codePoint));
        }
    }

    private record SymbolValue(String name) implements Value {
        @Override
        public String render() {
            return name;
        }
    }

    private record PairValue(Value car, Value cdr) implements Value {
        @Override
        public String render() {
            return renderContents(false);
        }

        @Override
        public String displayRender() {
            return renderContents(true);
        }

        private String renderContents(boolean displayMode) {
            StringBuilder builder = new StringBuilder();
            builder.append('(');
            appendPairContents(builder, this, displayMode);
            builder.append(')');
            return builder.toString();
        }

        private static void appendPairContents(StringBuilder builder, PairValue pair, boolean displayMode) {
            builder.append(displayMode ? pair.car.displayRender() : pair.car.render());
            if (pair.cdr instanceof EmptyListValue) {
                return;
            }
            if (pair.cdr instanceof PairValue nextPair) {
                builder.append(' ');
                appendPairContents(builder, nextPair, displayMode);
                return;
            }
            builder.append(" . ");
            builder.append(displayMode ? pair.cdr.displayRender() : pair.cdr.render());
        }
    }

    private enum EmptyListValue implements Value {
        INSTANCE;

        @Override
        public String render() {
            return "()";
        }
    }

    private enum VoidValue implements Value {
        INSTANCE;

        @Override
        public String render() {
            return "";
        }
    }

    private static final class BuiltinProcedure implements Value, Procedure {
        private final String name;
        private final BuiltinInvoker invoker;

        private BuiltinProcedure(String name, BuiltinInvoker invoker) {
            this.name = name;
            this.invoker = invoker;
        }

        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }

        @Override
        public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
            return invoker.apply(arguments, callLoc);
        }
    }

    private final class UserProcedure implements Value, Procedure {
        private final String name;
        private final List<String> parameters;
        private final List<Expr> body;
        private final Environment closureEnv;

        private UserProcedure(String name, List<String> parameters, List<Expr> body, Environment closureEnv) {
            this.name = name;
            this.parameters = parameters;
            this.body = body;
            this.closureEnv = closureEnv;
        }

        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }

        @Override
        public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
            if (arguments.size() != parameters.size()) {
                throw error(callLoc,
                        name + " expected " + parameters.size() + " arguments but got "
                                + arguments.size());
            }

            Environment callEnv = new Environment(closureEnv);
            for (int index = 0; index < parameters.size(); index++) {
                callEnv.define(parameters.get(index), arguments.get(index));
            }
            return evalSequence(body, callEnv);
        }
    }

    private static final class Environment {
        private final Environment parent;
        private final Map<String, Value> bindings;

        private Environment(Environment parent) {
            this.parent = parent;
            this.bindings = new HashMap<>();
        }

        private void define(String name, Value value) {
            bindings.put(name, value);
        }

        private Value lookup(String name, SourceLoc loc) throws EvalError {
            Value value = bindings.get(name);
            if (value != null) {
                return value;
            }
            if (parent != null) {
                return parent.lookup(name, loc);
            }
            throw error(loc, "unbound variable: " + name);
        }

        private void set(String name, Value value, SourceLoc loc) throws EvalError {
            if (bindings.containsKey(name)) {
                bindings.put(name, value);
                return;
            }
            if (parent != null) {
                parent.set(name, value, loc);
                return;
            }
            throw error(loc, "unbound variable: " + name);
        }
    }

    private static final class Parser {
        private final String input;
        private int index;
        private int line;
        private int column;

        private Parser(String input) {
            this.input = input;
            this.index = 0;
            this.line = 1;
            this.column = 1;
        }

        private List<Expr> parseProgram() throws EvalError {
            List<Expr> expressions = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd()) {
                expressions.add(parseExpression());
                skipIgnored();
            }
            return List.copyOf(expressions);
        }

        private Expr parseExpression() throws EvalError {
            skipIgnored();
            if (isAtEnd()) {
                throw new EvalError("unexpected end of input");
            }

            SourceLoc loc = currentLoc();
            char ch = peek();
            if (ch == '\'') {
                return parseQuoteShorthand();
            }
            if (ch == '(') {
                return parseList();
            }
            if (ch == ')') {
                throw error(loc, "unexpected ')'");
            }
            if (ch == '"') {
                return parseString();
            }
            return parseAtom();
        }

        private Expr parseList() throws EvalError {
            SourceLoc start = currentLoc();
            advance();

            List<Expr> elements = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd() && peek() != ')') {
                elements.add(parseExpression());
                skipIgnored();
            }

            if (isAtEnd()) {
                throw error(start, "unterminated list");
            }
            advance();
            return new ListExpr(start, List.copyOf(elements));
        }

        private Expr parseQuoteShorthand() throws EvalError {
            SourceLoc start = currentLoc();
            advance();
            Expr quotedExpr = parseExpression();
            return new ListExpr(
                    start,
                    List.of(
                            new SymbolExpr(start, "quote"),
                            quotedExpr
                    )
            );
        }

        private Expr parseString() throws EvalError {
            SourceLoc start = currentLoc();
            advance();

            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char ch = advance();
                if (ch == '"') {
                    return new StringExpr(start, builder.toString());
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw error(start, "unterminated string literal");
                    }
                    builder.append(parseEscape(advance()));
                } else {
                    builder.append(ch);
                }
            }

            throw error(start, "unterminated string literal");
        }

        private char parseEscape(char escaped) {
            return switch (escaped) {
                case 'n' -> '\n';
                case 'r' -> '\r';
                case 't' -> '\t';
                case '"' -> '"';
                case '\\' -> '\\';
                default -> escaped;
            };
        }

        private Expr parseAtom() throws EvalError {
            SourceLoc start = currentLoc();
            StringBuilder builder = new StringBuilder();
            while (!isAtEnd() && !isDelimiter(peek())) {
                builder.append(advance());
            }

            String token = builder.toString();
            if ("#t".equals(token)) {
                return new BooleanExpr(start, true);
            }
            if ("#f".equals(token)) {
                return new BooleanExpr(start, false);
            }
            if (token.startsWith("#\\")) {
                return new CharExpr(start, parseCharacterLiteral(token, start));
            }
            if (isIntegerToken(token)) {
                return new NumberExpr(start, Rational.integer(new BigInteger(token)));
            }
            return new SymbolExpr(start, token);
        }

        private void skipIgnored() {
            while (!isAtEnd()) {
                char ch = peek();
                if (Character.isWhitespace(ch)) {
                    advance();
                    continue;
                }
                if (ch == ';') {
                    skipComment();
                    continue;
                }
                return;
            }
        }

        private void skipComment() {
            while (!isAtEnd() && peek() != '\n') {
                advance();
            }
        }

        private boolean isDelimiter(char ch) {
            return Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ';';
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private char peek() {
            return input.charAt(index);
        }

        private char advance() {
            char ch = input.charAt(index);
            index++;
            if (ch == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            return ch;
        }

        private SourceLoc currentLoc() {
            return new SourceLoc(line, column);
        }

        private boolean isIntegerToken(String token) {
            if (token.isEmpty()) {
                return false;
            }

            int start = 0;
            char first = token.charAt(0);
            if (first == '+' || first == '-') {
                if (token.length() == 1) {
                    return false;
                }
                start = 1;
            }

            for (int index = start; index < token.length(); index++) {
                if (!Character.isDigit(token.charAt(index))) {
                    return false;
                }
            }
            return true;
        }

        private int parseCharacterLiteral(String token, SourceLoc loc) throws EvalError {
            String literal = token.substring(2);
            return switch (literal) {
                case "space" -> ' ';
                case "newline" -> '\n';
                default -> {
                    if (literal.codePointCount(0, literal.length()) != 1) {
                        throw error(loc, "invalid character literal");
                    }
                    yield literal.codePointAt(0);
                }
            };
        }
    }

    private record Rational(BigInteger numerator, BigInteger denominator) implements Comparable<Rational> {
        private static final Rational ZERO = integer(BigInteger.ZERO);
        private static final Rational ONE = integer(BigInteger.ONE);

        private Rational {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("denominator cannot be zero");
            }
        }

        private static Rational integer(BigInteger value) {
            return new Rational(value, BigInteger.ONE);
        }

        private static Rational of(BigInteger numerator, BigInteger denominator) {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("denominator cannot be zero");
            }

            BigInteger normalizedNumerator = numerator;
            BigInteger normalizedDenominator = denominator;
            if (normalizedDenominator.signum() < 0) {
                normalizedNumerator = normalizedNumerator.negate();
                normalizedDenominator = normalizedDenominator.negate();
            }

            BigInteger gcd = normalizedNumerator.gcd(normalizedDenominator);
            return new Rational(
                    normalizedNumerator.divide(gcd),
                    normalizedDenominator.divide(gcd)
            );
        }

        private Rational add(Rational other) {
            return of(
                    numerator.multiply(other.denominator).add(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator)
            );
        }

        private Rational subtract(Rational other) {
            return of(
                    numerator.multiply(other.denominator).subtract(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator)
            );
        }

        private Rational multiply(Rational other) {
            return of(numerator.multiply(other.numerator), denominator.multiply(other.denominator));
        }

        private Rational divide(Rational other, SourceLoc callLoc) throws EvalError {
            if (other.numerator.signum() == 0) {
                throw error(callLoc, "division by zero");
            }
            return of(numerator.multiply(other.denominator), denominator.multiply(other.numerator));
        }

        private Rational negate() {
            return new Rational(numerator.negate(), denominator);
        }

        private String render() {
            if (denominator.equals(BigInteger.ONE)) {
                return numerator.toString();
            }
            return numerator + "/" + denominator;
        }

        @Override
        public int compareTo(Rational other) {
            return numerator.multiply(other.denominator)
                    .compareTo(other.numerator.multiply(denominator));
        }
    }

    private Rational parseNumberLiteral(String token) {
        if (isIntegerToken(token)) {
            return Rational.integer(new BigInteger(token));
        }

        int slashIndex = token.indexOf('/');
        if (slashIndex <= 0 || slashIndex != token.lastIndexOf('/')) {
            return null;
        }

        String numeratorToken = token.substring(0, slashIndex);
        String denominatorToken = token.substring(slashIndex + 1);
        if (!isIntegerToken(numeratorToken) || !isIntegerToken(denominatorToken)) {
            return null;
        }

        BigInteger denominator = new BigInteger(denominatorToken);
        if (denominator.signum() == 0) {
            return null;
        }
        return Rational.of(new BigInteger(numeratorToken), denominator);
    }

    private static String renderString(String value) {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int index = 0; index < value.length(); index++) {
            char ch = value.charAt(index);
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

    private static String renderChar(int codePoint) {
        if (codePoint == ' ') {
            return "#\\space";
        }
        if (codePoint == '\n') {
            return "#\\newline";
        }
        return "#\\" + new String(Character.toChars(codePoint));
    }

    private static boolean isIntegerToken(String token) {
        if (token.isEmpty()) {
            return false;
        }

        int start = 0;
        char first = token.charAt(0);
        if (first == '+' || first == '-') {
            if (token.length() == 1) {
                return false;
            }
            start = 1;
        }

        for (int index = start; index < token.length(); index++) {
            if (!Character.isDigit(token.charAt(index))) {
                return false;
            }
        }
        return true;
    }
}
