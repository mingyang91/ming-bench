package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

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
        env.define("apply", new BuiltinProcedure("apply", this::applyApply));
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
        env.define("exact?", new BuiltinProcedure("exact?", this::applyExactPredicate));
        env.define("inexact?", new BuiltinProcedure("inexact?", this::applyInexactPredicate));
        env.define("exact->inexact", new BuiltinProcedure("exact->inexact", this::applyExactToInexact));
        env.define("inexact->exact", new BuiltinProcedure("inexact->exact", this::applyInexactToExact));
        env.define("integer?", new BuiltinProcedure("integer?", this::applyIntegerPredicate));
        env.define("rational?", new BuiltinProcedure("rational?", this::applyRationalPredicate));
        env.define("numerator", new BuiltinProcedure("numerator", this::applyNumerator));
        env.define("denominator", new BuiltinProcedure("denominator", this::applyDenominator));
        env.define("boolean?", new BuiltinProcedure("boolean?", this::applyBooleanPredicate));
        env.define("pair?", new BuiltinProcedure("pair?", this::applyPairPredicate));
        env.define("symbol?", new BuiltinProcedure("symbol?", this::applySymbolPredicate));
        env.define("eq?", new BuiltinProcedure("eq?", this::applyEq));
        env.define("equal?", new BuiltinProcedure("equal?", this::applyEqual));
        env.define("map", new BuiltinProcedure("map", this::applyMap));
        env.define("abs", new BuiltinProcedure("abs", this::applyAbs));
        env.define("modulo", new BuiltinProcedure("modulo", this::applyModulo));
        env.define("remainder", new BuiltinProcedure("remainder", this::applyRemainder));
        env.define("quotient", new BuiltinProcedure("quotient", this::applyQuotient));
        env.define("min", new BuiltinProcedure("min", this::applyMin));
        env.define("max", new BuiltinProcedure("max", this::applyMax));
        env.define("expt", new BuiltinProcedure("expt", this::applyExpt));
        env.define("zero?", new BuiltinProcedure("zero?", this::applyZeroPredicate));
        env.define("positive?", new BuiltinProcedure("positive?", this::applyPositivePredicate));
        env.define("negative?", new BuiltinProcedure("negative?", this::applyNegativePredicate));
        env.define("odd?", new BuiltinProcedure("odd?", this::applyOddPredicate));
        env.define("even?", new BuiltinProcedure("even?", this::applyEvenPredicate));
        env.define("list-ref", new BuiltinProcedure("list-ref", this::applyListRef));
        env.define("list-tail", new BuiltinProcedure("list-tail", this::applyListTail));
        env.define("list?", new BuiltinProcedure("list?", this::applyListPredicate));
        env.define("assoc", new BuiltinProcedure("assoc", this::applyAssoc));
        env.define("char-alphabetic?", new BuiltinProcedure("char-alphabetic?", this::applyCharAlphabeticPredicate));
        env.define("char-numeric?", new BuiltinProcedure("char-numeric?", this::applyCharNumericPredicate));
        env.define("char-upcase", new BuiltinProcedure("char-upcase", this::applyCharUpcase));
        env.define("char-downcase", new BuiltinProcedure("char-downcase", this::applyCharDowncase));
        env.define("char=?", new BuiltinProcedure("char=?", this::applyCharEquals));
        env.define("char<?", new BuiltinProcedure("char<?", this::applyCharLessThan));
        env.define("string=?", new BuiltinProcedure("string=?", this::applyStringEquals));
        env.define("string<?", new BuiltinProcedure("string<?", this::applyStringLessThan));
        env.define("string-ci=?", new BuiltinProcedure("string-ci=?", this::applyStringCiEquals));
        env.define("string-upcase", new BuiltinProcedure("string-upcase", this::applyStringUpcase));
        env.define("string-downcase", new BuiltinProcedure("string-downcase", this::applyStringDowncase));
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
            if ("define-syntax".equals(symbolName)) {
                return evalDefineSyntax(listExpr, env);
            }
            if ("define-record-type".equals(symbolName)) {
                return evalDefineRecordType(listExpr, env);
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

            SyntaxRulesMacro definition = env.lookupMacro(symbolName);
            if (definition != null) {
                MacroExpansion expansion = SyntaxRulesSupport.expandMacroCall(
                        definition,
                        listExpr.elements().subList(1, listExpr.elements().size()),
                        env,
                        listExpr.loc()
                );
                return eval(expansion.expression(), expansion.environment());
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

            ParameterSpec parameters = parseParameters(
                    signature.elements().subList(1, signature.elements().size()),
                    "define"
            );
            List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
            UserProcedure procedure = new UserProcedure(
                    nameSymbol.name(),
                    parameters.requiredParameters(),
                    parameters.restParameter(),
                    body,
                    env
            );
            env.define(nameSymbol.name(), procedure);
            return VOID;
        }

        throw error(target.loc(), "define requires a symbol or parameter list");
    }

    private Value evalDefineSyntax(ListExpr listExpr, Environment env) throws EvalError {
        ensureExactlyExpressions("define-syntax", listExpr, 3);

        Expr nameExpr = listExpr.elements().get(1);
        if (!(nameExpr instanceof SymbolExpr nameSymbol)) {
            throw error(nameExpr.loc(), "define-syntax name must be a symbol");
        }

        SyntaxRulesMacro definition = SyntaxRulesSupport.parseSyntaxRules(
                nameSymbol.name(),
                listExpr.elements().get(2),
                env
        );
        env.defineMacro(nameSymbol.name(), definition);
        return VOID;
    }

    private Value evalDefineRecordType(ListExpr listExpr, Environment env) throws EvalError {
        ensureAtLeastExpressions("define-record-type", listExpr, 4);

        String typeName = requireSymbolExpr(
                listExpr.elements().get(1),
                "define-record-type type name must be a symbol"
        );

        Expr constructorExpr = listExpr.elements().get(2);
        if (!(constructorExpr instanceof ListExpr constructorList)
                || constructorList.elements().isEmpty()) {
            throw error(constructorExpr.loc(),
                    "define-record-type constructor spec must be a non-empty list");
        }

        String constructorName = requireSymbolExpr(
                constructorList.elements().getFirst(),
                "define-record-type constructor name must be a symbol"
        );
        List<String> constructorFields = new ArrayList<>(Math.max(
                constructorList.elements().size() - 1,
                0
        ));
        for (int index = 1; index < constructorList.elements().size(); index++) {
            constructorFields.add(requireSymbolExpr(
                    constructorList.elements().get(index),
                    "define-record-type constructor fields must be symbols"
            ));
        }

        String predicateName = requireSymbolExpr(
                listExpr.elements().get(3),
                "define-record-type predicate name must be a symbol"
        );

        List<String> fieldNames = new ArrayList<>(Math.max(listExpr.elements().size() - 4, 0));
        List<String> accessorNames = new ArrayList<>(fieldNames.size());
        for (int index = 4; index < listExpr.elements().size(); index++) {
            Expr fieldExpr = listExpr.elements().get(index);
            if (!(fieldExpr instanceof ListExpr fieldList)) {
                throw error(fieldExpr.loc(), "define-record-type field specs must be lists");
            }
            if (fieldList.elements().size() != 2) {
                throw error(fieldExpr.loc(),
                        "define-record-type field specs must contain a field name and accessor");
            }

            fieldNames.add(requireSymbolExpr(
                    fieldList.elements().get(0),
                    "define-record-type field names must be symbols"
            ));
            accessorNames.add(requireSymbolExpr(
                    fieldList.elements().get(1),
                    "define-record-type accessor names must be symbols"
            ));
        }

        if (constructorFields.size() != fieldNames.size()) {
            throw error(constructorExpr.loc(),
                    "define-record-type constructor field count must match record fields");
        }
        if (!constructorFields.equals(fieldNames)) {
            throw error(constructorExpr.loc(),
                    "define-record-type constructor fields must match record fields");
        }

        RecordType recordType = new RecordType(typeName, fieldNames);
        env.define(constructorName, new RecordConstructorProcedure(constructorName, recordType));
        env.define(predicateName, new RecordPredicateProcedure(predicateName, recordType));
        for (int index = 0; index < accessorNames.size(); index++) {
            env.define(
                    accessorNames.get(index),
                    new RecordAccessorProcedure(accessorNames.get(index), recordType, index)
            );
        }

        return VOID;
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

        ParameterSpec parameters = parseParameters(parametersList.elements(), "lambda");
        List<Expr> body = List.copyOf(listExpr.elements().subList(2, listExpr.elements().size()));
        return new UserProcedure(
                "lambda",
                parameters.requiredParameters(),
                parameters.restParameter(),
                body,
                env
        );
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
                    null,
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
        SchemeNumber result = SchemeNumber.EXACT_ZERO;
        for (Value argument : arguments) {
            result = result.add(requireNumber(argument, "+", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applySubtract(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("-", arguments, 1, callLoc);
        SchemeNumber result = requireNumber(arguments.getFirst(), "-", callLoc);
        if (arguments.size() == 1) {
            return new NumberValue(result.negate());
        }

        for (int index = 1; index < arguments.size(); index++) {
            result = result.subtract(requireNumber(arguments.get(index), "-", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applyMultiply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        SchemeNumber result = SchemeNumber.EXACT_ONE;
        for (Value argument : arguments) {
            result = result.multiply(requireNumber(argument, "*", callLoc));
        }
        return new NumberValue(result);
    }

    private Value applyDivide(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("/", arguments, 1, callLoc);
        SchemeNumber result = arguments.size() == 1
                ? SchemeNumber.EXACT_ONE.divide(requireNumber(arguments.getFirst(), "/", callLoc), callLoc)
                : requireNumber(arguments.getFirst(), "/", callLoc);

        for (int index = 1; index < arguments.size(); index++) {
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
        return applyComparison(arguments, callLoc, "=", SchemeNumber::numericallyEquals);
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
        return new NumberValue(SchemeNumber.exact(
                Rational.integer(BigInteger.valueOf(elements.size()))
        ));
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

    private Value applyApply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("apply", arguments, 2, callLoc);

        Value procedureValue = arguments.getFirst();
        if (!(procedureValue instanceof Procedure procedure)) {
            throw error(callLoc, "apply expects a procedure as its first argument");
        }

        List<Value> appliedArguments = new ArrayList<>();
        for (int index = 1; index < arguments.size() - 1; index++) {
            appliedArguments.add(arguments.get(index));
        }
        appliedArguments.addAll(requireProperList(
                arguments.get(arguments.size() - 1),
                "apply",
                callLoc
        ));
        return procedure.apply(appliedArguments, callLoc);
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
        return new NumberValue(SchemeNumber.exact(Rational.integer(BigInteger.valueOf(length))));
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
        SchemeNumber parsed = SchemeNumber.parse(value);
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

    private Value applyExactPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("exact?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "exact?", callLoc).isExact() ? TRUE : FALSE;
    }

    private Value applyInexactPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("inexact?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "inexact?", callLoc).isInexact() ? TRUE : FALSE;
    }

    private Value applyExactToInexact(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("exact->inexact", arguments, 1, callLoc);
        SchemeNumber number = requireNumber(arguments.getFirst(), "exact->inexact", callLoc);
        if (number.isInexact()) {
            return new NumberValue(number);
        }
        return new NumberValue(SchemeNumber.inexact(number.toDouble()));
    }

    private Value applyInexactToExact(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("inexact->exact", arguments, 1, callLoc);
        SchemeNumber number = requireNumber(arguments.getFirst(), "inexact->exact", callLoc);
        if (number.isExact()) {
            return new NumberValue(number);
        }
        return new NumberValue(SchemeNumber.exact(number.toExact()));
    }

    private Value applyIntegerPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("integer?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "integer?", callLoc).isInteger() ? TRUE : FALSE;
    }

    private Value applyRationalPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("rational?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "rational?", callLoc).isRational() ? TRUE : FALSE;
    }

    private Value applyNumerator(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("numerator", arguments, 1, callLoc);
        Rational exact = requireNumber(arguments.getFirst(), "numerator", callLoc).toExact();
        return new NumberValue(SchemeNumber.exact(Rational.integer(exact.numerator())));
    }

    private Value applyDenominator(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("denominator", arguments, 1, callLoc);
        Rational exact = requireNumber(arguments.getFirst(), "denominator", callLoc).toExact();
        return new NumberValue(SchemeNumber.exact(Rational.integer(exact.denominator())));
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

    private Value applyEq(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("eq?", arguments, 2, callLoc);
        return eqValues(arguments.get(0), arguments.get(1)) ? TRUE : FALSE;
    }

    private Value applyEqual(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("equal?", arguments, 2, callLoc);
        return equalValues(arguments.get(0), arguments.get(1)) ? TRUE : FALSE;
    }

    private Value applyMap(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("map", arguments, 2, callLoc);

        Value procedureValue = arguments.getFirst();
        if (!(procedureValue instanceof Procedure procedure)) {
            throw error(callLoc, "map expects a procedure as its first argument");
        }

        List<List<Value>> lists = new ArrayList<>(arguments.size() - 1);
        int expectedLength = -1;
        for (int index = 1; index < arguments.size(); index++) {
            List<Value> elements = requireProperList(arguments.get(index), "map", callLoc);
            if (expectedLength == -1) {
                expectedLength = elements.size();
            } else if (elements.size() != expectedLength) {
                throw error(callLoc, "map expects lists of equal length");
            }
            lists.add(elements);
        }

        List<Value> results = new ArrayList<>(Math.max(expectedLength, 0));
        for (int elementIndex = 0; elementIndex < expectedLength; elementIndex++) {
            List<Value> mappedArguments = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                mappedArguments.add(list.get(elementIndex));
            }
            results.add(procedure.apply(mappedArguments, callLoc));
        }
        return buildList(results);
    }

    private Value applyAbs(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("abs", arguments, 1, callLoc);
        SchemeNumber value = requireNumber(arguments.getFirst(), "abs", callLoc);
        return new NumberValue(value.signum() < 0 ? value.negate() : value);
    }

    private Value applyModulo(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("modulo", arguments, 2, callLoc);
        BigInteger dividend = requireInteger(arguments.get(0), "modulo", callLoc);
        BigInteger divisor = requireInteger(arguments.get(1), "modulo", callLoc);
        if (divisor.signum() == 0) {
            throw error(callLoc, "division by zero");
        }

        BigInteger remainder = dividend.remainder(divisor);
        if (remainder.signum() != 0 && remainder.signum() != divisor.signum()) {
            remainder = remainder.add(divisor);
        }
        return new NumberValue(SchemeNumber.exact(Rational.integer(remainder)));
    }

    private Value applyRemainder(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("remainder", arguments, 2, callLoc);
        BigInteger dividend = requireInteger(arguments.get(0), "remainder", callLoc);
        BigInteger divisor = requireInteger(arguments.get(1), "remainder", callLoc);
        if (divisor.signum() == 0) {
            throw error(callLoc, "division by zero");
        }
        return new NumberValue(SchemeNumber.exact(Rational.integer(dividend.remainder(divisor))));
    }

    private Value applyQuotient(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("quotient", arguments, 2, callLoc);
        BigInteger dividend = requireInteger(arguments.get(0), "quotient", callLoc);
        BigInteger divisor = requireInteger(arguments.get(1), "quotient", callLoc);
        if (divisor.signum() == 0) {
            throw error(callLoc, "division by zero");
        }
        return new NumberValue(SchemeNumber.exact(Rational.integer(dividend.divide(divisor))));
    }

    private Value applyMin(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("min", arguments, 1, callLoc);
        SchemeNumber result = requireNumber(arguments.getFirst(), "min", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            SchemeNumber current = requireNumber(arguments.get(index), "min", callLoc);
            if (current.compareTo(result) < 0) {
                result = current;
            }
        }
        return new NumberValue(result);
    }

    private Value applyMax(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("max", arguments, 1, callLoc);
        SchemeNumber result = requireNumber(arguments.getFirst(), "max", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            SchemeNumber current = requireNumber(arguments.get(index), "max", callLoc);
            if (current.compareTo(result) > 0) {
                result = current;
            }
        }
        return new NumberValue(result);
    }

    private Value applyExpt(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("expt", arguments, 2, callLoc);
        SchemeNumber base = requireNumber(arguments.get(0), "expt", callLoc);
        BigInteger exponent = requireInteger(arguments.get(1), "expt", callLoc);
        return new NumberValue(pow(base, exponent, callLoc));
    }

    private Value applyZeroPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("zero?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "zero?", callLoc).signum() == 0 ? TRUE : FALSE;
    }

    private Value applyPositivePredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("positive?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "positive?", callLoc).signum() > 0 ? TRUE : FALSE;
    }

    private Value applyNegativePredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("negative?", arguments, 1, callLoc);
        return requireNumber(arguments.getFirst(), "negative?", callLoc).signum() < 0 ? TRUE : FALSE;
    }

    private Value applyOddPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("odd?", arguments, 1, callLoc);
        BigInteger value = requireInteger(arguments.getFirst(), "odd?", callLoc).abs();
        return value.remainder(BigInteger.TWO).equals(BigInteger.ONE) ? TRUE : FALSE;
    }

    private Value applyEvenPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("even?", arguments, 1, callLoc);
        BigInteger value = requireInteger(arguments.getFirst(), "even?", callLoc).abs();
        return value.remainder(BigInteger.TWO).equals(BigInteger.ZERO) ? TRUE : FALSE;
    }

    private Value applyListRef(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("list-ref", arguments, 2, callLoc);
        Value current = arguments.getFirst();
        int index = requireIndex(arguments.get(1), "list-ref", callLoc);

        for (int step = 0; step < index; step++) {
            if (!(current instanceof PairValue pairValue)) {
                if (current instanceof EmptyListValue) {
                    throw error(callLoc, "list-ref index is out of bounds");
                }
                throw error(callLoc, "list-ref expects a proper list");
            }
            current = pairValue.cdr();
        }

        if (current instanceof PairValue pairValue) {
            return pairValue.car();
        }
        if (current instanceof EmptyListValue) {
            throw error(callLoc, "list-ref index is out of bounds");
        }
        throw error(callLoc, "list-ref expects a proper list");
    }

    private Value applyListTail(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("list-tail", arguments, 2, callLoc);
        Value current = arguments.getFirst();
        int index = requireIndex(arguments.get(1), "list-tail", callLoc);

        for (int step = 0; step < index; step++) {
            if (!(current instanceof PairValue pairValue)) {
                if (current instanceof EmptyListValue) {
                    throw error(callLoc, "list-tail index is out of bounds");
                }
                throw error(callLoc, "list-tail expects a proper list");
            }
            current = pairValue.cdr();
        }

        if (current instanceof PairValue || current instanceof EmptyListValue) {
            return current;
        }
        throw error(callLoc, "list-tail expects a proper list");
    }

    private Value applyListPredicate(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("list?", arguments, 1, callLoc);
        return isProperList(arguments.getFirst()) ? TRUE : FALSE;
    }

    private Value applyAssoc(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("assoc", arguments, 2, callLoc);
        Value key = arguments.get(0);
        Value current = arguments.get(1);

        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            PairValue association = requirePair(entry, "assoc", callLoc);
            if (equalValues(key, association.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw error(callLoc, "assoc expects a proper list");
        }
        return FALSE;
    }

    private Value applyCharAlphabeticPredicate(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        ensureExactly("char-alphabetic?", arguments, 1, callLoc);
        return Character.isAlphabetic(requireChar(arguments.getFirst(), "char-alphabetic?", callLoc)
                .codePoint())
                ? TRUE
                : FALSE;
    }

    private Value applyCharNumericPredicate(List<Value> arguments, SourceLoc callLoc)
            throws EvalError {
        ensureExactly("char-numeric?", arguments, 1, callLoc);
        return Character.isDigit(requireChar(arguments.getFirst(), "char-numeric?", callLoc)
                .codePoint())
                ? TRUE
                : FALSE;
    }

    private Value applyCharUpcase(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("char-upcase", arguments, 1, callLoc);
        return new CharValue(Character.toUpperCase(
                requireChar(arguments.getFirst(), "char-upcase", callLoc).codePoint()
        ));
    }

    private Value applyCharDowncase(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("char-downcase", arguments, 1, callLoc);
        return new CharValue(Character.toLowerCase(
                requireChar(arguments.getFirst(), "char-downcase", callLoc).codePoint()
        ));
    }

    private Value applyCharEquals(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("char=?", arguments, 2, callLoc);
        int previous = requireChar(arguments.getFirst(), "char=?", callLoc).codePoint();
        for (int index = 1; index < arguments.size(); index++) {
            int current = requireChar(arguments.get(index), "char=?", callLoc).codePoint();
            if (previous != current) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyCharLessThan(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("char<?", arguments, 2, callLoc);
        int previous = requireChar(arguments.getFirst(), "char<?", callLoc).codePoint();
        for (int index = 1; index < arguments.size(); index++) {
            int current = requireChar(arguments.get(index), "char<?", callLoc).codePoint();
            if (previous >= current) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyStringEquals(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("string=?", arguments, 2, callLoc);
        String previous = requireString(arguments.getFirst(), "string=?", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), "string=?", callLoc);
            if (!previous.equals(current)) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyStringLessThan(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("string<?", arguments, 2, callLoc);
        String previous = requireString(arguments.getFirst(), "string<?", callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), "string<?", callLoc);
            if (previous.compareTo(current) >= 0) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyStringCiEquals(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureAtLeast("string-ci=?", arguments, 2, callLoc);
        String previous = requireString(arguments.getFirst(), "string-ci=?", callLoc)
                .toLowerCase(Locale.ROOT);
        for (int index = 1; index < arguments.size(); index++) {
            String current = requireString(arguments.get(index), "string-ci=?", callLoc)
                    .toLowerCase(Locale.ROOT);
            if (!previous.equals(current)) {
                return FALSE;
            }
            previous = current;
        }
        return TRUE;
    }

    private Value applyStringUpcase(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-upcase", arguments, 1, callLoc);
        return new StringValue(
                requireString(arguments.getFirst(), "string-upcase", callLoc).toUpperCase(Locale.ROOT)
        );
    }

    private Value applyStringDowncase(List<Value> arguments, SourceLoc callLoc) throws EvalError {
        ensureExactly("string-downcase", arguments, 1, callLoc);
        return new StringValue(
                requireString(arguments.getFirst(), "string-downcase", callLoc).toLowerCase(Locale.ROOT)
        );
    }

    private Value applyComparison(
            List<Value> arguments,
            SourceLoc callLoc,
            String name,
            NumberComparison comparison
    ) throws EvalError {
        ensureAtLeast(name, arguments, 2, callLoc);
        SchemeNumber previous = requireNumber(arguments.getFirst(), name, callLoc);
        for (int index = 1; index < arguments.size(); index++) {
            SchemeNumber current = requireNumber(arguments.get(index), name, callLoc);
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

    private ParameterSpec parseParameters(List<Expr> parameterExprs, String formName)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;

        for (int index = 0; index < parameterExprs.size(); index++) {
            Expr parameterExpr = parameterExprs.get(index);
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error(parameterExpr.loc(), formName + " parameters must be symbols");
            }

            if (".".equals(symbolExpr.name())) {
                if (restParameter != null || index != parameterExprs.size() - 2) {
                    throw error(symbolExpr.loc(), formName + " has invalid dotted parameter list");
                }

                Expr restExpr = parameterExprs.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol)
                        || ".".equals(restSymbol.name())) {
                    throw error(restExpr.loc(), formName + " has invalid dotted parameter list");
                }
                restParameter = restSymbol.name();
                break;
            }

            parameters.add(symbolExpr.name());
        }
        return new ParameterSpec(List.copyOf(parameters), restParameter);
    }

    private SchemeNumber requireNumber(Value value, String procedureName, SourceLoc callLoc)
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

    private BigInteger requireInteger(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        Rational number = requireNumber(value, procedureName, callLoc).toExact();
        if (!number.denominator().equals(BigInteger.ONE)) {
            throw error(callLoc, procedureName + " expects integer arguments");
        }
        return number.numerator();
    }

    private int requireIndex(Value value, String procedureName, SourceLoc callLoc)
            throws EvalError {
        Rational number = requireNumber(value, procedureName, callLoc).toExact();
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

    private String requireSymbolExpr(Expr expression, String message) throws EvalError {
        if (expression instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        throw error(expression.loc(), message);
    }

    private boolean isProperList(Value value) {
        Value current = value;
        while (current instanceof PairValue pairValue) {
            current = pairValue.cdr();
        }
        return current instanceof EmptyListValue;
    }

    private boolean eqValues(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (left instanceof NumberValue leftNumber && right instanceof NumberValue rightNumber) {
            return leftNumber.value().numericallyEquals(rightNumber.value());
        }
        if (left instanceof BooleanValue leftBoolean && right instanceof BooleanValue rightBoolean) {
            return leftBoolean.value() == rightBoolean.value();
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.codePoint() == rightChar.codePoint();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return false;
    }

    private boolean equalValues(Value left, Value right) {
        if (eqValues(left, right)) {
            return true;
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.text().equals(rightString.text());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            return equalValues(leftPair.car(), rightPair.car())
                    && equalValues(leftPair.cdr(), rightPair.cdr());
        }
        return false;
    }

    private SchemeNumber pow(SchemeNumber base, BigInteger exponent, SourceLoc callLoc)
            throws EvalError {
        if (exponent.signum() == 0) {
            return base.isExact() ? SchemeNumber.EXACT_ONE : SchemeNumber.inexact(1.0d);
        }

        BigInteger magnitude = exponent.signum() < 0 ? exponent.negate() : exponent;
        if (magnitude.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            throw error(callLoc, "expt exponent is too large");
        }

        if (base.isInexact()) {
            return SchemeNumber.inexact(Math.pow(base.toDouble(), exponent.doubleValue()));
        }

        Rational exactBase = base.toExact();
        Rational result = Rational.of(
                exactBase.numerator().pow(magnitude.intValueExact()),
                exactBase.denominator().pow(magnitude.intValueExact())
        );
        if (exponent.signum() < 0) {
            return SchemeNumber.exact(Rational.ONE.divide(result, callLoc));
        }
        return SchemeNumber.exact(result);
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
        return SchemeErrors.at(loc, message);
    }

    @FunctionalInterface
    private interface NumberComparison {
        boolean test(SchemeNumber left, SchemeNumber right);
    }

    private final class UserProcedure implements Value, Procedure {
        private final String name;
        private final List<String> parameters;
        private final String restParameter;
        private final List<Expr> body;
        private final Environment closureEnv;

        private UserProcedure(
                String name,
                List<String> parameters,
                String restParameter,
                List<Expr> body,
                Environment closureEnv
        ) {
            this.name = name;
            this.parameters = parameters;
            this.restParameter = restParameter;
            this.body = body;
            this.closureEnv = closureEnv;
        }

        @Override
        public String render() {
            return "#<procedure:" + name + ">";
        }

        @Override
        public Value apply(List<Value> arguments, SourceLoc callLoc) throws EvalError {
            if (restParameter == null && arguments.size() != parameters.size()) {
                throw error(
                        callLoc,
                        name + " expected " + parameters.size() + " arguments but got "
                                + arguments.size()
                );
            }
            if (restParameter != null && arguments.size() < parameters.size()) {
                throw error(
                        callLoc,
                        name + " expected at least " + parameters.size() + " arguments but got "
                                + arguments.size()
                );
            }

            Environment callEnv = new Environment(closureEnv);
            for (int index = 0; index < parameters.size(); index++) {
                callEnv.define(parameters.get(index), arguments.get(index));
            }
            if (restParameter != null) {
                callEnv.define(
                        restParameter,
                        buildList(arguments.subList(parameters.size(), arguments.size()))
                );
            }
            return evalSequence(body, callEnv);
        }
    }
}
