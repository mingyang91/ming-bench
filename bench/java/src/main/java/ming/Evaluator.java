package ming;

import static ming.EvaluatorSupport.*;
import static ming.RuntimeConstants.*;
import static ming.ValueSupport.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private static final int STRING_IMMUTABILITY_LEVEL = 15;

    private final Env globalEnv;
    private final MacroExpander macroExpander;
    private final boolean immutableStringsEnabled;
    private StringBuilder outputBuffer;

    public Evaluator() {
        this.immutableStringsEnabled = currentBenchLevel() >= STRING_IMMUTABILITY_LEVEL;
        this.globalEnv = createGlobalEnv();
        this.macroExpander = new MacroExpander();
    }

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalProgram(input, false).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return evalProgram(input, true);
    }

    private static int currentBenchLevel() {
        String level = System.getProperty("bench.level", "");
        if (level == null || level.isEmpty()) {
            level = System.getenv("BENCH_LEVEL");
        }
        if (level == null || level.isEmpty()) {
            return Integer.MAX_VALUE;
        }

        try {
            return Integer.parseInt(level);
        } catch (NumberFormatException ignored) {
            return Integer.MAX_VALUE;
        }
    }

    private EvalResult evalProgram(String input, boolean captureOutput) throws EvalError {
        List<Expr> expressions = new Parser(input).parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("input did not contain any expressions");
        }

        StringBuilder previousOutput = outputBuffer;
        outputBuffer = captureOutput ? new StringBuilder() : null;
        try {
            Value result = VOID;
            for (Expr expression : expressions) {
                result = eval(expression, globalEnv);
            }
            String output = outputBuffer == null ? "" : outputBuffer.toString();
            return new EvalResult(ValueRenderer.render(result), output);
        } finally {
            outputBuffer = previousOutput;
        }
    }

    private Env createGlobalEnv() {
        Env env = new Env(null);
        env.define("+", new BuiltinValue("+", this::add));
        env.define("-", new BuiltinValue("-", this::subtract));
        env.define("*", new BuiltinValue("*", this::multiply));
        env.define("/", new BuiltinValue("/", this::divide));
        env.define("abs", new BuiltinValue("abs", this::abs));
        env.define("modulo", new BuiltinValue("modulo", this::modulo));
        env.define("remainder", new BuiltinValue("remainder", this::remainder));
        env.define("quotient", new BuiltinValue("quotient", this::quotient));
        env.define("min", new BuiltinValue("min", arguments -> extremum(arguments, "min")));
        env.define("max", new BuiltinValue("max", arguments -> extremum(arguments, "max")));
        env.define("expt", new BuiltinValue("expt", this::expt));
        env.define("<", new BuiltinValue("<", arguments -> PredicateBuiltins.compare(arguments, "<")));
        env.define(">", new BuiltinValue(">", arguments -> PredicateBuiltins.compare(arguments, ">")));
        env.define("=", new BuiltinValue("=", arguments -> PredicateBuiltins.compare(arguments, "=")));
        env.define("<=", new BuiltinValue("<=", arguments -> PredicateBuiltins.compare(arguments, "<=")));
        env.define(">=", new BuiltinValue(">=", arguments -> PredicateBuiltins.compare(arguments, ">=")));
        env.define("not", new BuiltinValue("not", PredicateBuiltins::not));
        env.define("zero?", new BuiltinValue("zero?",
                arguments -> PredicateBuiltins.signPredicate("zero?", arguments,
                        value -> value == 0)));
        env.define("positive?", new BuiltinValue("positive?",
                arguments -> PredicateBuiltins.signPredicate("positive?", arguments,
                        value -> value > 0)));
        env.define("negative?", new BuiltinValue("negative?",
                arguments -> PredicateBuiltins.signPredicate("negative?", arguments,
                        value -> value < 0)));
        env.define("odd?", new BuiltinValue("odd?",
                arguments -> PredicateBuiltins.integerNumericPredicate("odd?", arguments,
                        value -> value % 2L != 0L)));
        env.define("even?", new BuiltinValue("even?",
                arguments -> PredicateBuiltins.integerNumericPredicate("even?", arguments,
                        value -> value % 2L == 0L)));
        env.define("cons", new BuiltinValue("cons", this::cons));
        env.define("car", new BuiltinValue("car", this::car));
        env.define("cdr", new BuiltinValue("cdr", this::cdr));
        env.define("null?", new BuiltinValue("null?", arguments ->
                PredicateBuiltins.typePredicate("null?", arguments,
                        value -> value instanceof EmptyListValue)));
        env.define("list", new BuiltinValue("list", CollectionBuiltins::list));
        env.define("vector", new BuiltinValue("vector", CollectionBuiltins::vector));
        env.define("make-vector", new BuiltinValue("make-vector", CollectionBuiltins::makeVector));
        env.define("vector-ref", new BuiltinValue("vector-ref", CollectionBuiltins::vectorRef));
        env.define("vector-set!", new BuiltinValue("vector-set!", CollectionBuiltins::vectorSet));
        env.define("vector-length",
                new BuiltinValue("vector-length", CollectionBuiltins::vectorLength));
        env.define("vector?", new BuiltinValue("vector?", arguments ->
                PredicateBuiltins.typePredicate("vector?", arguments,
                        value -> value instanceof VectorValue)));
        env.define("vector->list",
                new BuiltinValue("vector->list", CollectionBuiltins::vectorToList));
        env.define("list->vector",
                new BuiltinValue("list->vector", CollectionBuiltins::listToVector));
        env.define("map", new BuiltinValue("map", this::map));
        env.define("length", new BuiltinValue("length", this::length));
        env.define("list-ref", new BuiltinValue("list-ref", this::listRef));
        env.define("list-tail", new BuiltinValue("list-tail", this::listTail));
        env.define("list?", new BuiltinValue("list?", this::listPredicate));
        env.define("assoc", new BuiltinValue("assoc", this::assoc));
        env.define("append", new BuiltinValue("append", this::append));
        env.define("apply", new BuiltinValue("apply", this::apply));
        env.define("string?", new BuiltinValue("string?", arguments ->
                PredicateBuiltins.typePredicate("string?", arguments,
                        value -> value instanceof StringValue)));
        env.define("number?", new BuiltinValue("number?", PredicateBuiltins::numberPredicate));
        env.define("integer?", new BuiltinValue("integer?", PredicateBuiltins::integerPredicate));
        env.define("rational?", new BuiltinValue("rational?", PredicateBuiltins::rationalPredicate));
        env.define("exact?", new BuiltinValue("exact?", PredicateBuiltins::exactPredicate));
        env.define("inexact?", new BuiltinValue("inexact?", PredicateBuiltins::inexactPredicate));
        env.define("boolean?", new BuiltinValue("boolean?", arguments ->
                PredicateBuiltins.typePredicate("boolean?", arguments,
                        value -> value instanceof BoolValue)));
        env.define("pair?", new BuiltinValue("pair?", arguments ->
                PredicateBuiltins.typePredicate("pair?", arguments,
                        value -> value instanceof PairValue)));
        env.define("symbol?", new BuiltinValue("symbol?", arguments ->
                PredicateBuiltins.typePredicate("symbol?", arguments,
                        value -> value instanceof SymbolValue)));
        env.define("procedure?",
                new BuiltinValue("procedure?", PredicateBuiltins::procedurePredicate));
        env.define("eq?", new BuiltinValue("eq?", PredicateBuiltins::eq));
        env.define("eqv?", new BuiltinValue("eqv?", PredicateBuiltins::eqv));
        env.define("equal?", new BuiltinValue("equal?", PredicateBuiltins::equal));
        env.define("display", new BuiltinValue("display", this::display));
        env.define("write", new BuiltinValue("write", this::write));
        env.define("newline", new BuiltinValue("newline", this::newline));
        env.define("exact->inexact",
                new BuiltinValue("exact->inexact", PredicateBuiltins::exactToInexact));
        env.define("inexact->exact",
                new BuiltinValue("inexact->exact", PredicateBuiltins::inexactToExact));
        env.define("numerator", new BuiltinValue("numerator", PredicateBuiltins::numerator));
        env.define("denominator", new BuiltinValue("denominator", PredicateBuiltins::denominator));
        env.define("string-append", new BuiltinValue("string-append", this::stringAppend));
        env.define("string-length", new BuiltinValue("string-length", this::stringLength));
        env.define("substring", new BuiltinValue("substring", this::substring));
        env.define("string->number", new BuiltinValue("string->number", this::stringToNumber));
        env.define("number->string", new BuiltinValue("number->string", this::numberToString));
        env.define("symbol->string", new BuiltinValue("symbol->string", this::symbolToString));
        env.define("string->symbol", new BuiltinValue("string->symbol", this::stringToSymbol));
        env.define("string->list", new BuiltinValue("string->list", this::stringToList));
        env.define("list->string", new BuiltinValue("list->string", this::listToString));
        env.define("string-ref", new BuiltinValue("string-ref", this::stringRef));
        env.define("string-set!", new BuiltinValue("string-set!", this::stringSet));
        env.define("string-copy", new BuiltinValue("string-copy", this::stringCopy));
        env.define("string=?", new BuiltinValue("string=?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string=?")));
        env.define("string<?", new BuiltinValue("string<?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string<?")));
        env.define("string-ci=?", new BuiltinValue("string-ci=?",
                arguments -> PredicateBuiltins.compareStrings(arguments, "string-ci=?")));
        env.define("string-upcase", new BuiltinValue("string-upcase", this::stringUpcase));
        env.define("string-downcase", new BuiltinValue("string-downcase", this::stringDowncase));
        env.define("char?", new BuiltinValue("char?", arguments ->
                PredicateBuiltins.typePredicate("char?", arguments,
                        value -> value instanceof CharValue)));
        env.define("char-alphabetic?", new BuiltinValue("char-alphabetic?",
                PredicateBuiltins::charAlphabetic));
        env.define("char-numeric?",
                new BuiltinValue("char-numeric?", PredicateBuiltins::charNumeric));
        env.define("char-upcase", new BuiltinValue("char-upcase", PredicateBuiltins::charUpcase));
        env.define("char-downcase",
                new BuiltinValue("char-downcase", PredicateBuiltins::charDowncase));
        env.define("char->integer", new BuiltinValue("char->integer", this::charToInteger));
        env.define("integer->char", new BuiltinValue("integer->char", this::integerToChar));
        env.define("char=?", new BuiltinValue("char=?",
                arguments -> PredicateBuiltins.compareChars(arguments, "char=?")));
        env.define("char<?", new BuiltinValue("char<?",
                arguments -> PredicateBuiltins.compareChars(arguments, "char<?")));
        return env;
    }

    private Value eval(Expr expr, Env env) throws EvalError {
        try {
            return switch (expr) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case RationalExpr rationalExpr -> exactValue(
                        rationalExpr.numerator(), rationalExpr.denominator());
                case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
                case BoolExpr boolExpr -> boolValue(boolExpr.value());
                case CharExpr charExpr -> new CharValue(charExpr.value());
                case StringExpr stringExpr -> immutableString(stringExpr.value());
                case SymbolExpr symbolExpr -> macroExpander.lookupSymbol(symbolExpr.name(), env);
                case ListExpr listExpr -> evalList(listExpr, env);
            };
        } catch (EvalError error) {
            throw error.withPosition(expr.line(), expr.column());
        }
    }

    private Value evalList(ListExpr listExpr, Env env) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate an empty list");
        }

        Expr operatorExpr = elements.get(0);
        List<Expr> arguments = elements.subList(1, elements.size());
        String operatorName = symbolName(operatorExpr);
        if (operatorName != null) {
            if ("define-syntax".equals(operatorName)) {
                macroExpander.defineSyntax(arguments, env);
                return VOID;
            }

            Optional<Expr> expandedMacro = macroExpander.expandInvocation(operatorName, listExpr);
            if (expandedMacro.isPresent()) {
                return eval(expandedMacro.get(), env);
            }

            return switch (operatorName) {
                case "define" -> evalDefine(arguments, env);
                case "define-record-type" -> evalDefineRecordType(arguments, env);
                case "set!" -> evalSet(arguments, env);
                case "if" -> evalIf(arguments, env);
                case "quote" -> evalQuote(arguments);
                case "lambda" -> evalLambda(arguments, env);
                case "case-lambda" -> evalCaseLambda(arguments, env);
                case "and" -> evalAnd(arguments, env);
                case "or" -> evalOr(arguments, env);
                case "begin" -> evalBegin(arguments, env);
                case "let" -> evalLet(arguments, env);
                case "letrec" -> evalLetRec(arguments, env, false);
                case "letrec*" -> evalLetRec(arguments, env, true);
                case "cond" -> evalCond(arguments, env);
                case "case" -> evalCase(arguments, env);
                case "do" -> evalDo(arguments, env);
                default -> applyProcedure(eval(operatorExpr, env), evalAll(arguments, env));
            };
        }

        return applyProcedure(eval(operatorExpr, env), evalAll(arguments, env));
    }

    private Value evalDefine(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("define expects a name and a value");
        }

        Expr target = arguments.get(0);
        if (target instanceof SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw new EvalError("define expects exactly one value expression");
            }
            Cell binding = env.definePlaceholder(symbolExpr.name());
            binding.set(eval(arguments.get(1), env));
            return VOID;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty() || !(signature.get(0) instanceof SymbolExpr nameExpr)) {
                throw new EvalError("define function name must be a symbol");
            }

            if (arguments.size() < 2) {
                throw new EvalError("define requires a function body");
            }

            Cell binding = env.definePlaceholder(nameExpr.name());
            binding.set(new ClosureValue(
                    parseFormals(signature.subList(1, signature.size())),
                    List.copyOf(arguments.subList(1, arguments.size())),
                    env));
            return VOID;
        }

        throw new EvalError("define target must be a symbol or parameter list");
    }

    private Value evalDefineRecordType(List<Expr> arguments, Env env) throws EvalError {
        requireMinArgs("define-record-type", arguments, 3);

        Expr typeExpr = arguments.get(0);
        if (!(typeExpr instanceof SymbolExpr typeSymbol)) {
            throw new EvalError("define-record-type type name must be a symbol");
        }

        Expr constructorExpr = arguments.get(1);
        if (!(constructorExpr instanceof ListExpr constructorList)
                || constructorList.elements().isEmpty()) {
            throw new EvalError("define-record-type constructor spec must be a list");
        }

        Expr constructorNameExpr = constructorList.elements().get(0);
        if (!(constructorNameExpr instanceof SymbolExpr constructorNameSymbol)) {
            throw new EvalError("define-record-type constructor name must be a symbol");
        }

        Expr predicateExpr = arguments.get(2);
        if (!(predicateExpr instanceof SymbolExpr predicateSymbol)) {
            throw new EvalError("define-record-type predicate name must be a symbol");
        }

        List<RecordFieldSpec> fieldSpecs = new ArrayList<>();
        Map<String, Integer> fieldIndexes = new HashMap<>();
        for (int index = 3; index < arguments.size(); index++) {
            Expr fieldExpr = arguments.get(index);
            if (!(fieldExpr instanceof ListExpr fieldList) || fieldList.elements().size() != 2) {
                throw new EvalError("define-record-type field spec must contain a field and accessor");
            }

            Expr fieldNameExpr = fieldList.elements().get(0);
            if (!(fieldNameExpr instanceof SymbolExpr fieldNameSymbol)) {
                throw new EvalError("define-record-type field name must be a symbol");
            }

            Expr accessorExpr = fieldList.elements().get(1);
            if (!(accessorExpr instanceof SymbolExpr accessorSymbol)) {
                throw new EvalError("define-record-type accessor name must be a symbol");
            }

            String fieldName = fieldNameSymbol.name();
            if (fieldIndexes.containsKey(fieldName)) {
                throw new EvalError("define-record-type field names must be unique");
            }

            fieldIndexes.put(fieldName, fieldSpecs.size());
            fieldSpecs.add(new RecordFieldSpec(fieldName, accessorSymbol.name()));
        }

        List<Expr> constructorFields = constructorList.elements().subList(1,
                constructorList.elements().size());
        if (constructorFields.size() != fieldSpecs.size()) {
            throw new EvalError("define-record-type constructor field count must match field specs");
        }

        int[] constructorOrder = new int[constructorFields.size()];
        boolean[] assignedFields = new boolean[fieldSpecs.size()];
        for (int index = 0; index < constructorFields.size(); index++) {
            Expr fieldExpr = constructorFields.get(index);
            if (!(fieldExpr instanceof SymbolExpr fieldSymbol)) {
                throw new EvalError("define-record-type constructor fields must be symbols");
            }

            Integer fieldIndex = fieldIndexes.get(fieldSymbol.name());
            if (fieldIndex == null) {
                throw new EvalError("define-record-type constructor references an unknown field");
            }
            if (assignedFields[fieldIndex]) {
                throw new EvalError("define-record-type constructor fields must be unique");
            }

            constructorOrder[index] = fieldIndex;
            assignedFields[fieldIndex] = true;
        }

        RecordTypeValue type = new RecordTypeValue(typeSymbol.name());
        env.define(constructorNameSymbol.name(),
                new BuiltinValue(constructorNameSymbol.name(),
                        values -> constructRecord(type, constructorOrder, values,
                                constructorNameSymbol.name())));
        env.define(predicateSymbol.name(),
                new BuiltinValue(predicateSymbol.name(),
                        values -> recordPredicate(type, values, predicateSymbol.name())));
        for (int index = 0; index < fieldSpecs.size(); index++) {
            RecordFieldSpec fieldSpec = fieldSpecs.get(index);
            int fieldIndex = index;
            env.define(fieldSpec.accessorName(),
                    new BuiltinValue(fieldSpec.accessorName(),
                            values -> recordAccessor(type, fieldIndex, values,
                                    fieldSpec.accessorName())));
        }
        return VOID;
    }

    private Value constructRecord(RecordTypeValue type, int[] constructorOrder,
            List<Value> arguments, String constructorName) throws EvalError {
        requireExactArgs(constructorName, arguments, constructorOrder.length);

        Value[] fields = new Value[constructorOrder.length];
        for (int index = 0; index < constructorOrder.length; index++) {
            fields[constructorOrder[index]] = arguments.get(index);
        }
        return new RecordInstanceValue(type, List.of(fields));
    }

    private Value recordPredicate(RecordTypeValue type, List<Value> arguments, String name)
            throws EvalError {
        requireExactArgs(name, arguments, 1);
        return boolValue(arguments.get(0) instanceof RecordInstanceValue recordInstance
                && recordInstance.type() == type);
    }

    private Value recordAccessor(RecordTypeValue type, int fieldIndex, List<Value> arguments,
            String accessorName) throws EvalError {
        requireExactArgs(accessorName, arguments, 1);
        Value value = arguments.get(0);
        if (!(value instanceof RecordInstanceValue recordInstance)
                || recordInstance.type() != type) {
            throw new EvalError(accessorName + " expects a " + type.name() + " record");
        }
        return recordInstance.field(fieldIndex);
    }

    private Value evalSet(List<Expr> arguments, Env env) throws EvalError {
        requireExactArgs("set!", arguments, 2);

        Expr target = arguments.get(0);
        if (!(target instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! target must be a symbol");
        }

        macroExpander.setSymbol(symbolExpr.name(), eval(arguments.get(1), env), env);
        return VOID;
    }

    private String symbolName(Expr expr) {
        if (expr instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        return null;
    }

    private Value evalIf(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("if expected 2 or 3 argument(s)");
        }
        Value condition = eval(arguments.get(0), env);
        if (isTruthy(condition)) {
            return eval(arguments.get(1), env);
        }
        if (arguments.size() == 3) {
            return eval(arguments.get(2), env);
        }
        return VOID;
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        requireExactArgs("quote", arguments, 1);
        return quoteToValue(arguments.get(0));
    }

    private Value evalLambda(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("lambda requires parameters and a body");
        }

        return new ClosureValue(
                parseFormals(arguments.get(0)),
                List.copyOf(arguments.subList(1, arguments.size())),
                env);
    }

    private Value evalCaseLambda(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("case-lambda requires at least one clause");
        }

        List<ProcedureClause> clauses = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            if (!(argument instanceof ListExpr clauseExpr) || clauseExpr.elements().size() < 2) {
                throw new EvalError("case-lambda clause must include parameters and a body");
            }

            List<Expr> clauseElements = clauseExpr.elements();
            clauses.add(new ProcedureClause(
                    parseFormals(clauseElements.get(0)),
                    List.copyOf(clauseElements.subList(1, clauseElements.size()))));
        }
        return new CaseLambdaValue(List.copyOf(clauses), env);
    }

    private Formals parseFormals(Expr parameterExpr) throws EvalError {
        if (parameterExpr instanceof SymbolExpr symbolExpr) {
            return new Formals(List.of(), symbolExpr.name());
        }
        if (!(parameterExpr instanceof ListExpr parameterList)) {
            throw new EvalError("lambda parameters must be a list or symbol");
        }
        return parseFormals(parameterList.elements());
    }

    private Formals parseFormals(List<Expr> parameterExprs) throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;

        for (int index = 0; index < parameterExprs.size(); index++) {
            Expr parameterExpr = parameterExprs.get(index);
            if (parameterExpr instanceof SymbolExpr symbolExpr
                    && ".".equals(symbolExpr.name())) {
                if (index == parameterExprs.size() - 1) {
                    throw new EvalError("lambda rest parameter name is missing");
                }

                Expr restExpr = parameterExprs.get(index + 1);
                if (!(restExpr instanceof SymbolExpr restSymbol)
                        || ".".equals(restSymbol.name())) {
                    throw new EvalError("lambda rest parameter must be a symbol");
                }
                if (index + 2 != parameterExprs.size()) {
                    throw new EvalError("lambda rest parameter must be last");
                }
                restParameter = restSymbol.name();
                break;
            }
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("lambda parameter must be a symbol");
            }
            parameters.add(symbolExpr.name());
        }
        return new Formals(List.copyOf(parameters), restParameter);
    }

    private List<Value> evalAll(List<Expr> arguments, Env env) throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument, env));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments, Env env) throws EvalError {
        Value last = TRUE;
        for (Expr argument : arguments) {
            last = eval(argument, env);
            if (!isTruthy(last)) {
                return last;
            }
        }
        return last;
    }

    private Value evalOr(List<Expr> arguments, Env env) throws EvalError {
        Value last = FALSE;
        for (Expr argument : arguments) {
            last = eval(argument, env);
            if (isTruthy(last)) {
                return last;
            }
        }
        return last;
    }

    private Value evalBegin(List<Expr> arguments, Env env) throws EvalError {
        return evalSequence(arguments, env);
    }

    private Value evalLet(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("let requires bindings and a body");
        }

        Expr firstArgument = arguments.get(0);
        if (firstArgument instanceof SymbolExpr nameExpr) {
            if (arguments.size() < 3) {
                throw new EvalError("named let requires bindings and a body");
            }
            return evalNamedLet(nameExpr.name(), arguments.get(1),
                    arguments.subList(2, arguments.size()), env);
        }

        if (arguments.size() < 2) {
            throw new EvalError("let requires bindings and a body");
        }

        List<BindingSpec> bindings = parseBindings(firstArgument);
        Env letEnv = new Env(env);
        bindValues(letEnv, bindings, evalBindingValues(bindings, env));
        return evalSequence(arguments.subList(1, arguments.size()), letEnv);
    }

    private Value evalLetRec(List<Expr> arguments, Env env, boolean sequential)
            throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError((sequential ? "letrec*" : "letrec")
                    + " requires bindings and a body");
        }

        List<BindingSpec> bindings = parseBindings(arguments.get(0));
        Env letrecEnv = new Env(env);
        List<Cell> cells = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            cells.add(letrecEnv.definePlaceholder(binding.name()));
        }

        if (sequential) {
            for (int index = 0; index < bindings.size(); index++) {
                cells.get(index).set(eval(bindings.get(index).initExpr(), letrecEnv));
            }
        } else {
            List<Value> values = new ArrayList<>(bindings.size());
            for (BindingSpec binding : bindings) {
                values.add(eval(binding.initExpr(), letrecEnv));
            }
            for (int index = 0; index < values.size(); index++) {
                cells.get(index).set(values.get(index));
            }
        }

        return evalSequence(arguments.subList(1, arguments.size()), letrecEnv);
    }

    private Value evalNamedLet(String name, Expr bindingExpr, List<Expr> body, Env env)
            throws EvalError {
        List<BindingSpec> bindings = parseBindings(bindingExpr);
        Env letEnv = new Env(env);
        Cell binding = letEnv.definePlaceholder(name);
        ClosureValue closure = new ClosureValue(
                new Formals(bindingNames(bindings), null),
                List.copyOf(body),
                letEnv);
        binding.set(closure);
        return applyClosure(closure, evalBindingValues(bindings, env));
    }

    private List<String> bindingNames(List<BindingSpec> bindings) {
        List<String> names = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            names.add(binding.name());
        }
        return List.copyOf(names);
    }

    private List<BindingSpec> parseBindings(Expr bindingExpr) throws EvalError {
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw new EvalError("let bindings must be a list");
        }

        List<BindingSpec> bindings = new ArrayList<>(bindingList.elements().size());
        for (Expr entryExpr : bindingList.elements()) {
            if (!(entryExpr instanceof ListExpr entry) || entry.elements().size() != 2) {
                throw new EvalError("let binding must contain a name and value");
            }
            Expr nameExpr = entry.elements().get(0);
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("let binding name must be a symbol");
            }
            bindings.add(new BindingSpec(symbolExpr.name(), entry.elements().get(1)));
        }
        return List.copyOf(bindings);
    }

    private List<Value> evalBindingValues(List<BindingSpec> bindings, Env env) throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (BindingSpec binding : bindings) {
            values.add(eval(binding.initExpr(), env));
        }
        return values;
    }

    private void bindValues(Env env, List<BindingSpec> bindings, List<Value> values) {
        for (int i = 0; i < bindings.size(); i++) {
            env.define(bindings.get(i).name(), values.get(i));
        }
    }

    private Value evalCond(List<Expr> arguments, Env env) throws EvalError {
        for (int index = 0; index < arguments.size(); index++) {
            Expr clauseExpr = arguments.get(index);
            if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
                throw new EvalError("cond clause must be a non-empty list");
            }

            List<Expr> elements = clause.elements();
            Expr testExpr = elements.get(0);
            if (testExpr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("else")) {
                if (index != arguments.size() - 1) {
                    throw new EvalError("cond else clause must be last");
                }
                if (elements.size() == 1) {
                    return TRUE;
                }
                return evalSequence(elements.subList(1, elements.size()), env);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                if (elements.size() == 1) {
                    return testValue;
                }
                return evalSequence(elements.subList(1, elements.size()), env);
            }
        }
        return VOID;
    }

    private Value evalCase(List<Expr> arguments, Env env) throws EvalError {
        requireMinArgs("case", arguments, 1);
        Value key = eval(arguments.get(0), env);

        for (int index = 1; index < arguments.size(); index++) {
            Expr clauseExpr = arguments.get(index);
            if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
                throw new EvalError("case clause must be a non-empty list");
            }

            List<Expr> clauseElements = clause.elements();
            Expr headExpr = clauseElements.get(0);
            if (headExpr instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
                if (index != arguments.size() - 1) {
                    throw new EvalError("case else clause must be last");
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), env);
            }

            if (!(headExpr instanceof ListExpr datumList)) {
                throw new EvalError("case clause datums must be a list");
            }

            for (Expr datumExpr : datumList.elements()) {
                if (eqValues(key, quoteToValue(datumExpr))) {
                    return evalSequence(clauseElements.subList(1, clauseElements.size()), env);
                }
            }
        }

        return VOID;
    }

    private Value evalDo(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("do requires bindings and a test clause");
        }

        List<DoBindingSpec> bindings = parseDoBindings(arguments.get(0));
        if (!(arguments.get(1) instanceof ListExpr testClause) || testClause.elements().isEmpty()) {
            throw new EvalError("do test clause must be a non-empty list");
        }

        List<Value> initValues = new ArrayList<>(bindings.size());
        for (DoBindingSpec binding : bindings) {
            initValues.add(eval(binding.initExpr(), env));
        }

        Env loopEnv = new Env(env);
        List<Cell> cells = new ArrayList<>(bindings.size());
        for (int index = 0; index < bindings.size(); index++) {
            Cell cell = loopEnv.definePlaceholder(bindings.get(index).name());
            cell.set(initValues.get(index));
            cells.add(cell);
        }

        Expr testExpr = testClause.elements().get(0);
        List<Expr> resultExprs = testClause.elements().subList(1, testClause.elements().size());
        List<Expr> bodyExprs = arguments.subList(2, arguments.size());

        while (true) {
            if (isTruthy(eval(testExpr, loopEnv))) {
                return evalSequence(resultExprs, loopEnv);
            }

            evalSequence(bodyExprs, loopEnv);

            List<Value> stepValues = new ArrayList<>(bindings.size());
            for (int index = 0; index < bindings.size(); index++) {
                DoBindingSpec binding = bindings.get(index);
                if (binding.stepExpr() == null) {
                    stepValues.add(cells.get(index).get());
                } else {
                    stepValues.add(eval(binding.stepExpr(), loopEnv));
                }
            }
            for (int index = 0; index < cells.size(); index++) {
                cells.get(index).set(stepValues.get(index));
            }
        }
    }

    private List<DoBindingSpec> parseDoBindings(Expr bindingExpr) throws EvalError {
        if (!(bindingExpr instanceof ListExpr bindingList)) {
            throw new EvalError("do bindings must be a list");
        }

        List<DoBindingSpec> bindings = new ArrayList<>(bindingList.elements().size());
        for (Expr entryExpr : bindingList.elements()) {
            if (!(entryExpr instanceof ListExpr entry)
                    || entry.elements().size() < 2
                    || entry.elements().size() > 3) {
                throw new EvalError("do binding must contain a name, init, and optional step");
            }

            Expr nameExpr = entry.elements().get(0);
            if (!(nameExpr instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("do binding name must be a symbol");
            }

            Expr stepExpr = entry.elements().size() == 3 ? entry.elements().get(2) : null;
            bindings.add(new DoBindingSpec(symbolExpr.name(), entry.elements().get(1), stepExpr));
        }
        return List.copyOf(bindings);
    }

    private Value applyProcedure(Value operator, List<Value> arguments) throws EvalError {
        return switch (operator) {
            case BuiltinValue builtinValue -> builtinValue.implementation().apply(arguments);
            case ClosureValue closureValue -> applyClosure(closureValue, arguments);
            case CaseLambdaValue caseLambdaValue -> applyCaseLambda(caseLambdaValue, arguments);
            default -> throw new EvalError("attempted to call a non-procedure");
        };
    }

    private Value applyClosure(ClosureValue closure, List<Value> arguments) throws EvalError {
        return applyProcedureClause(
                closure.formals(),
                closure.body(),
                closure.env(),
                arguments,
                "lambda");
    }

    private Value applyCaseLambda(CaseLambdaValue caseLambda, List<Value> arguments)
            throws EvalError {
        for (ProcedureClause clause : caseLambda.clauses()) {
            if (clause.formals().matchesArity(arguments.size())) {
                return applyProcedureClause(
                        clause.formals(),
                        clause.body(),
                        caseLambda.env(),
                        arguments,
                        "case-lambda");
            }
        }

        throw new EvalError("case-lambda has no matching clause for "
                + arguments.size() + " argument(s)");
    }

    private Value applyProcedureClause(Formals formals, List<Expr> body, Env definitionEnv,
            List<Value> arguments, String procedureName) throws EvalError {
        int fixedCount = formals.fixedCount();
        if (formals.restParameter() == null) {
            requireExactArgs(procedureName, arguments, fixedCount);
        } else if (arguments.size() < fixedCount) {
            throw new EvalError(procedureName + " expected at least "
                    + fixedCount + " argument(s)");
        }

        Env callEnv = new Env(definitionEnv);
        for (int i = 0; i < fixedCount; i++) {
            callEnv.define(formals.parameters().get(i), arguments.get(i));
        }
        if (formals.restParameter() != null) {
            callEnv.define(formals.restParameter(),
                    listValue(arguments.subList(fixedCount, arguments.size())));
        }
        return evalSequence(body, callEnv);
    }

    private Value evalSequence(List<Expr> expressions, Env env) throws EvalError {
        Value result = VOID;
        for (Expr expression : expressions) {
            result = eval(expression, env);
        }
        return result;
    }

    private Value display(List<Value> arguments) throws EvalError {
        requireExactArgs("display", arguments, 1);
        appendOutput(ValueRenderer.renderDisplay(arguments.get(0)));
        return VOID;
    }

    private Value write(List<Value> arguments) throws EvalError {
        requireExactArgs("write", arguments, 1);
        appendOutput(ValueRenderer.render(arguments.get(0)));
        return VOID;
    }

    private Value newline(List<Value> arguments) throws EvalError {
        requireExactArgs("newline", arguments, 0);
        appendOutput("\n");
        return VOID;
    }

    private Value quoteToValue(Expr expr) {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case RationalExpr rationalExpr -> exactValue(
                    rationalExpr.numerator(), rationalExpr.denominator());
            case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
            case BoolExpr boolExpr -> boolValue(boolExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case StringExpr stringExpr -> immutableString(stringExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> listValue(quoteElements(listExpr.elements()));
        };
    }

    private List<Value> quoteElements(List<Expr> expressions) {
        List<Value> values = new ArrayList<>(expressions.size());
        for (Expr expression : expressions) {
            values.add(quoteToValue(expression));
        }
        return values;
    }

    private Value add(List<Value> arguments) throws EvalError {
        if (containsInexact(arguments)) {
            double total = 0.0;
            for (Value argument : arguments) {
                total += requireNumberAsDouble(argument, "+");
            }
            return new InexactValue(total);
        }

        ExactRational total = new ExactRational(0L, 1L);
        for (Value argument : arguments) {
            total = addExact(total, requireExactRational(argument, "+"));
        }
        return exactValue(total);
    }

    private Value abs(List<Value> arguments) throws EvalError {
        requireExactArgs("abs", arguments, 1);
        Value argument = requireNumericValue(arguments.get(0), "abs");
        if (argument instanceof InexactValue inexactValue) {
            return new InexactValue(Math.abs(inexactValue.value()));
        }

        ExactRational rational = requireExactRational(argument, "abs");
        return exactValue(Math.abs(rational.numerator()), rational.denominator());
    }

    private Value subtract(List<Value> arguments) throws EvalError {
        requireMinArgs("-", arguments, 1);
        if (containsInexact(arguments)) {
            double result = requireNumberAsDouble(arguments.get(0), "-");
            if (arguments.size() == 1) {
                return new InexactValue(-result);
            }

            for (int index = 1; index < arguments.size(); index++) {
                result -= requireNumberAsDouble(arguments.get(index), "-");
            }
            return new InexactValue(result);
        }

        ExactRational result = requireExactRational(arguments.get(0), "-");
        if (arguments.size() == 1) {
            return exactValue(-result.numerator(), result.denominator());
        }

        for (int index = 1; index < arguments.size(); index++) {
            result = subtractExact(result, requireExactRational(arguments.get(index), "-"));
        }
        return exactValue(result);
    }

    private Value multiply(List<Value> arguments) throws EvalError {
        if (containsInexact(arguments)) {
            double total = 1.0;
            for (Value argument : arguments) {
                total *= requireNumberAsDouble(argument, "*");
            }
            return new InexactValue(total);
        }

        ExactRational total = new ExactRational(1L, 1L);
        for (Value argument : arguments) {
            total = multiplyExact(total, requireExactRational(argument, "*"));
        }
        return exactValue(total);
    }

    private Value map(List<Value> arguments) throws EvalError {
        requireMinArgs("map", arguments, 2);

        Value procedure = arguments.get(0);
        List<List<Value>> lists = new ArrayList<>(arguments.size() - 1);
        int expectedLength = -1;
        for (int index = 1; index < arguments.size(); index++) {
            List<Value> list = requireProperList(arguments.get(index), "map");
            if (expectedLength == -1) {
                expectedLength = list.size();
            } else if (list.size() != expectedLength) {
                throw new EvalError("map expects lists of equal length");
            }
            lists.add(list);
        }

        List<Value> results = new ArrayList<>(expectedLength);
        for (int item = 0; item < expectedLength; item++) {
            List<Value> callArguments = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                callArguments.add(list.get(item));
            }
            results.add(applyProcedure(procedure, callArguments));
        }
        return listValue(results);
    }

    private Value cons(List<Value> arguments) throws EvalError {
        requireExactArgs("cons", arguments, 2);
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private Value car(List<Value> arguments) throws EvalError {
        requireExactArgs("car", arguments, 1);
        return requirePair(arguments.get(0), "car").car();
    }

    private Value cdr(List<Value> arguments) throws EvalError {
        requireExactArgs("cdr", arguments, 1);
        return requirePair(arguments.get(0), "cdr").cdr();
    }

    private Value stringAppend(List<Value> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(requireString(argument, "string-append"));
        }
        return immutableString(builder.toString());
    }

    private Value stringLength(List<Value> arguments) throws EvalError {
        requireExactArgs("string-length", arguments, 1);
        return new IntValue(requireString(arguments.get(0), "string-length").length());
    }

    private Value substring(List<Value> arguments) throws EvalError {
        requireExactArgs("substring", arguments, 3);
        String value = requireString(arguments.get(0), "substring");
        int start = requireIndex(arguments.get(1), "substring");
        int end = requireIndex(arguments.get(2), "substring");
        if (start > end || end > value.length()) {
            throw new EvalError("substring index out of bounds");
        }
        return immutableString(value.substring(start, end));
    }

    private Value stringToNumber(List<Value> arguments) throws EvalError {
        requireExactArgs("string->number", arguments, 1);
        String value = requireString(arguments.get(0), "string->number");
        try {
            List<Expr> expressions = new Parser(value).parseProgram();
            if (expressions.size() != 1) {
                return FALSE;
            }

            Expr expression = expressions.get(0);
            if (expression instanceof IntExpr
                    || expression instanceof RationalExpr
                    || expression instanceof InexactExpr) {
                return quoteToValue(expression);
            }
            return FALSE;
        } catch (EvalError error) {
            return FALSE;
        }
    }

    private Value numberToString(List<Value> arguments) throws EvalError {
        requireExactArgs("number->string", arguments, 1);
        return immutableString(ValueRenderer.render(
                requireNumericValue(arguments.get(0), "number->string")));
    }

    private Value symbolToString(List<Value> arguments) throws EvalError {
        requireExactArgs("symbol->string", arguments, 1);
        return immutableString(requireSymbol(arguments.get(0), "symbol->string"));
    }

    private Value stringToSymbol(List<Value> arguments) throws EvalError {
        requireExactArgs("string->symbol", arguments, 1);
        return new SymbolValue(requireString(arguments.get(0), "string->symbol"));
    }

    private Value stringRef(List<Value> arguments) throws EvalError {
        requireExactArgs("string-ref", arguments, 2);
        StringValue value = requireStringValue(arguments.get(0), "string-ref");
        int index = requireIndex(arguments.get(1), "string-ref");
        if (index >= value.length()) {
            throw new EvalError("string-ref index out of bounds");
        }
        return new CharValue(value.charAt(index));
    }

    private Value stringToList(List<Value> arguments) throws EvalError {
        requireExactArgs("string->list", arguments, 1);
        String value = requireString(arguments.get(0), "string->list");
        List<Value> characters = new ArrayList<>(value.length());
        for (int index = 0; index < value.length(); index++) {
            characters.add(new CharValue(value.charAt(index)));
        }
        return listValue(characters);
    }

    private Value listToString(List<Value> arguments) throws EvalError {
        requireExactArgs("list->string", arguments, 1);
        List<Value> characters = requireProperList(arguments.get(0), "list->string");
        StringBuilder builder = new StringBuilder(characters.size());
        for (Value character : characters) {
            builder.append(requireChar(character, "list->string"));
        }
        return immutableString(builder.toString());
    }

    private Value stringSet(List<Value> arguments) throws EvalError {
        requireExactArgs("string-set!", arguments, 3);
        if (immutableStringsEnabled) {
            throw new EvalError("string-set! is not supported on immutable strings");
        }

        StringValue value = requireStringValue(arguments.get(0), "string-set!");
        if (!value.mutable()) {
            throw new EvalError("string-set! expects a mutable string");
        }

        int index = requireIndex(arguments.get(1), "string-set!");
        if (index >= value.length()) {
            throw new EvalError("string-set! index out of bounds");
        }

        value.setCharAt(index, requireChar(arguments.get(2), "string-set!"));
        return VOID;
    }

    private Value stringCopy(List<Value> arguments) throws EvalError {
        requireExactArgs("string-copy", arguments, 1);
        return requireStringValue(arguments.get(0), "string-copy")
                .copy(!immutableStringsEnabled);
    }

    private Value stringUpcase(List<Value> arguments) throws EvalError {
        requireExactArgs("string-upcase", arguments, 1);
        return immutableString(requireString(arguments.get(0), "string-upcase").toUpperCase());
    }

    private Value stringDowncase(List<Value> arguments) throws EvalError {
        requireExactArgs("string-downcase", arguments, 1);
        return immutableString(requireString(arguments.get(0), "string-downcase").toLowerCase());
    }

    private Value charToInteger(List<Value> arguments) throws EvalError {
        requireExactArgs("char->integer", arguments, 1);
        return new IntValue(requireChar(arguments.get(0), "char->integer"));
    }

    private Value integerToChar(List<Value> arguments) throws EvalError {
        requireExactArgs("integer->char", arguments, 1);
        long codePoint = requireInt(arguments.get(0), "integer->char");
        if (codePoint < Character.MIN_VALUE || codePoint > Character.MAX_VALUE) {
            throw new EvalError("integer->char expects a valid character code");
        }

        char value = (char) codePoint;
        if (Character.isSurrogate(value)) {
            throw new EvalError("integer->char expects a valid character code");
        }
        return new CharValue(value);
    }

    private Value length(List<Value> arguments) throws EvalError {
        requireExactArgs("length", arguments, 1);
        return new IntValue(requireProperList(arguments.get(0), "length").size());
    }

    private Value listRef(List<Value> arguments) throws EvalError {
        requireExactArgs("list-ref", arguments, 2);
        List<Value> elements = requireProperList(arguments.get(0), "list-ref");
        int index = requireIndex(arguments.get(1), "list-ref");
        if (index >= elements.size()) {
            throw new EvalError("list-ref index out of bounds");
        }
        return elements.get(index);
    }

    private Value listTail(List<Value> arguments) throws EvalError {
        requireExactArgs("list-tail", arguments, 2);
        int index = requireIndex(arguments.get(1), "list-tail");

        Value current = arguments.get(0);
        for (int i = 0; i < index; i++) {
            if (current instanceof PairValue pairValue) {
                current = pairValue.cdr();
            } else if (current instanceof EmptyListValue) {
                throw new EvalError("list-tail index out of bounds");
            } else {
                throw new EvalError("list-tail expects a proper list");
            }
        }

        if (!isProperListValue(current)) {
            throw new EvalError("list-tail expects a proper list");
        }
        return current;
    }

    private Value listPredicate(List<Value> arguments) throws EvalError {
        requireExactArgs("list?", arguments, 1);
        return boolValue(isProperListValue(arguments.get(0)));
    }

    private Value assoc(List<Value> arguments) throws EvalError {
        requireExactArgs("assoc", arguments, 2);

        Value key = arguments.get(0);
        Value current = arguments.get(1);
        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            if (!(entry instanceof PairValue entryPair)) {
                throw new EvalError("assoc expects an association list");
            }
            if (equalValues(key, entryPair.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }

        if (!(current instanceof EmptyListValue)) {
            throw new EvalError("assoc expects a proper list");
        }
        return FALSE;
    }

    private Value append(List<Value> arguments) throws EvalError {
        if (arguments.isEmpty()) {
            return EMPTY_LIST;
        }

        Value result = arguments.get(arguments.size() - 1);
        for (int index = arguments.size() - 2; index >= 0; index--) {
            List<Value> prefix = requireProperList(arguments.get(index), "append");
            for (int item = prefix.size() - 1; item >= 0; item--) {
                result = new PairValue(prefix.get(item), result);
            }
        }
        return result;
    }

    private Value apply(List<Value> arguments) throws EvalError {
        requireMinArgs("apply", arguments, 2);

        Value operator = arguments.get(0);
        List<Value> appliedArguments = new ArrayList<>();
        for (int i = 1; i < arguments.size() - 1; i++) {
            appliedArguments.add(arguments.get(i));
        }
        appliedArguments.addAll(requireProperList(arguments.get(arguments.size() - 1), "apply"));
        return applyProcedure(operator, appliedArguments);
    }

    private Value divide(List<Value> arguments) throws EvalError {
        requireMinArgs("/", arguments, 2);
        if (containsInexact(arguments)) {
            double result = requireNumberAsDouble(arguments.get(0), "/");
            for (int index = 1; index < arguments.size(); index++) {
                double divisor = requireNumberAsDouble(arguments.get(index), "/");
                if (divisor == 0.0) {
                    throw new EvalError("division by zero");
                }
                result /= divisor;
            }
            return new InexactValue(result);
        }

        ExactRational result = requireExactRational(arguments.get(0), "/");
        for (int index = 1; index < arguments.size(); index++) {
            ExactRational divisor = requireExactRational(arguments.get(index), "/");
            if (divisor.numerator() == 0L) {
                throw new EvalError("division by zero");
            }
            result = divideExact(result, divisor);
        }
        return exactValue(result);
    }

    private Value modulo(List<Value> arguments) throws EvalError {
        requireExactArgs("modulo", arguments, 2);
        long dividend = requireInt(arguments.get(0), "modulo");
        long divisor = requireInt(arguments.get(1), "modulo");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }

        long result = dividend % divisor;
        if (result != 0L && ((result > 0L) != (divisor > 0L))) {
            result += divisor;
        }
        return new IntValue(result);
    }

    private Value remainder(List<Value> arguments) throws EvalError {
        requireExactArgs("remainder", arguments, 2);
        long dividend = requireInt(arguments.get(0), "remainder");
        long divisor = requireInt(arguments.get(1), "remainder");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend % divisor);
    }

    private Value quotient(List<Value> arguments) throws EvalError {
        requireExactArgs("quotient", arguments, 2);
        long dividend = requireInt(arguments.get(0), "quotient");
        long divisor = requireInt(arguments.get(1), "quotient");
        if (divisor == 0L) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend / divisor);
    }

    private Value extremum(List<Value> arguments, String name) throws EvalError {
        requireMinArgs(name, arguments, 1);
        long result = requireInt(arguments.get(0), name);
        for (int index = 1; index < arguments.size(); index++) {
            long candidate = requireInt(arguments.get(index), name);
            if ("min".equals(name)) {
                result = Math.min(result, candidate);
            } else {
                result = Math.max(result, candidate);
            }
        }
        return new IntValue(result);
    }

    private Value expt(List<Value> arguments) throws EvalError {
        requireExactArgs("expt", arguments, 2);
        long base = requireInt(arguments.get(0), "expt");
        long exponent = requireInt(arguments.get(1), "expt");
        if (exponent < 0L) {
            throw new EvalError("expt expects a non-negative exponent");
        }

        long result = 1L;
        long factor = base;
        long remaining = exponent;
        while (remaining > 0L) {
            if ((remaining & 1L) != 0L) {
                result *= factor;
            }
            remaining >>= 1;
            if (remaining > 0L) {
                factor *= factor;
            }
        }
        return new IntValue(result);
    }

    private void appendOutput(String value) {
        if (outputBuffer != null) {
            outputBuffer.append(value);
        }
    }

    private record DoBindingSpec(String name, Expr initExpr, Expr stepExpr) {
    }

    private record RecordFieldSpec(String name, String accessorName) {
    }
}
