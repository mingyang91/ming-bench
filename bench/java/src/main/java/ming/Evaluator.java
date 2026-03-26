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

    private sealed interface EvalStep permits ValueStep, TailStep {
    }

    private record ValueStep(Value value) implements EvalStep {
    }

    private record TailStep(Expr expr, Env env) implements EvalStep {
    }

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
        return EvaluatorBuiltins.createGlobalEnv(
                immutableStringsEnabled,
                this::applyProcedure,
                this::appendOutput,
                this::quoteToValue);
    }

    private Value eval(Expr expr, Env env) throws EvalError {
        Expr currentExpr = expr;
        Env currentEnv = env;
        while (true) {
            try {
                switch (currentExpr) {
                    case IntExpr intExpr -> {
                        return new IntValue(intExpr.value());
                    }
                    case RationalExpr rationalExpr -> {
                        return exactValue(rationalExpr.numerator(), rationalExpr.denominator());
                    }
                    case InexactExpr inexactExpr -> {
                        return new InexactValue(inexactExpr.value());
                    }
                    case BoolExpr boolExpr -> {
                        return boolValue(boolExpr.value());
                    }
                    case CharExpr charExpr -> {
                        return new CharValue(charExpr.value());
                    }
                    case StringExpr stringExpr -> {
                        return immutableString(stringExpr.value());
                    }
                    case SymbolExpr symbolExpr -> {
                        return macroExpander.lookupSymbol(symbolExpr.name(), currentEnv);
                    }
                    case ListExpr listExpr -> {
                        EvalStep step = evalList(listExpr, currentEnv);
                        if (step instanceof ValueStep valueStep) {
                            return valueStep.value();
                        }

                        TailStep tailStep = (TailStep) step;
                        currentExpr = tailStep.expr();
                        currentEnv = tailStep.env();
                    }
                }
            } catch (EvalError error) {
                throw error.withPosition(currentExpr.line(), currentExpr.column());
            }
        }
    }

    private EvalStep evalList(ListExpr listExpr, Env env) throws EvalError {
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
                return new ValueStep(VOID);
            }

            Optional<Expr> expandedMacro = macroExpander.expandInvocation(operatorName, listExpr);
            if (expandedMacro.isPresent()) {
                return new TailStep(expandedMacro.get(), env);
            }

            return switch (operatorName) {
                case "define" -> new ValueStep(evalDefine(arguments, env));
                case "define-record-type" -> new ValueStep(evalDefineRecordType(arguments, env));
                case "set!" -> new ValueStep(evalSet(arguments, env));
                case "if" -> evalIf(arguments, env);
                case "quote" -> new ValueStep(evalQuote(arguments));
                case "lambda" -> new ValueStep(evalLambda(arguments, env));
                case "case-lambda" -> new ValueStep(evalCaseLambda(arguments, env));
                case "and" -> evalAnd(arguments, env);
                case "or" -> evalOr(arguments, env);
                case "begin" -> evalBegin(arguments, env);
                case "let" -> evalLet(arguments, env);
                case "let*" -> evalLetStar(arguments, env);
                case "letrec" -> evalLetRec(arguments, env, false);
                case "letrec*" -> evalLetRec(arguments, env, true);
                case "cond" -> evalCond(arguments, env);
                case "case" -> evalCase(arguments, env);
                case "do" -> evalDo(arguments, env);
                default -> applyProcedureStep(eval(operatorExpr, env), evalAll(arguments, env));
            };
        }

        return applyProcedureStep(eval(operatorExpr, env), evalAll(arguments, env));
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

    private EvalStep evalIf(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("if expected 2 or 3 argument(s)");
        }
        Value condition = eval(arguments.get(0), env);
        if (isTruthy(condition)) {
            return new TailStep(arguments.get(1), env);
        }
        if (arguments.size() == 3) {
            return new TailStep(arguments.get(2), env);
        }
        return new ValueStep(VOID);
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

    private EvalStep evalAnd(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.isEmpty()) {
            return new ValueStep(TRUE);
        }

        for (int index = 0; index < arguments.size() - 1; index++) {
            Value value = eval(arguments.get(index), env);
            if (!isTruthy(value)) {
                return new ValueStep(value);
            }
        }
        return new TailStep(arguments.get(arguments.size() - 1), env);
    }

    private EvalStep evalOr(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.isEmpty()) {
            return new ValueStep(FALSE);
        }

        for (int index = 0; index < arguments.size() - 1; index++) {
            Value value = eval(arguments.get(index), env);
            if (isTruthy(value)) {
                return new ValueStep(value);
            }
        }
        return new TailStep(arguments.get(arguments.size() - 1), env);
    }

    private EvalStep evalBegin(List<Expr> arguments, Env env) throws EvalError {
        return tailSequence(arguments, env);
    }

    private EvalStep evalLet(List<Expr> arguments, Env env) throws EvalError {
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
        return tailSequence(arguments.subList(1, arguments.size()), letEnv);
    }

    private EvalStep evalLetStar(List<Expr> arguments, Env env) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let* requires bindings and a body");
        }

        List<BindingSpec> bindings = parseBindings(arguments.get(0));
        Env letStarEnv = new Env(env);
        for (BindingSpec binding : bindings) {
            Value value = eval(binding.initExpr(), letStarEnv);
            Env nextEnv = new Env(letStarEnv);
            nextEnv.define(binding.name(), value);
            letStarEnv = nextEnv;
        }
        return tailSequence(arguments.subList(1, arguments.size()), letStarEnv);
    }

    private EvalStep evalLetRec(List<Expr> arguments, Env env, boolean sequential)
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

        return tailSequence(arguments.subList(1, arguments.size()), letrecEnv);
    }

    private EvalStep evalNamedLet(String name, Expr bindingExpr, List<Expr> body, Env env)
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

    private EvalStep evalCond(List<Expr> arguments, Env env) throws EvalError {
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
                    return new ValueStep(TRUE);
                }
                return tailSequence(elements.subList(1, elements.size()), env);
            }

            Value testValue = eval(testExpr, env);
            if (isTruthy(testValue)) {
                if (elements.size() == 1) {
                    return new ValueStep(testValue);
                }
                return tailSequence(elements.subList(1, elements.size()), env);
            }
        }
        return new ValueStep(VOID);
    }

    private EvalStep evalCase(List<Expr> arguments, Env env) throws EvalError {
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
                return tailSequence(clauseElements.subList(1, clauseElements.size()), env);
            }

            if (!(headExpr instanceof ListExpr datumList)) {
                throw new EvalError("case clause datums must be a list");
            }

            for (Expr datumExpr : datumList.elements()) {
                if (eqValues(key, quoteToValue(datumExpr))) {
                    return tailSequence(clauseElements.subList(1, clauseElements.size()), env);
                }
            }
        }

        return new ValueStep(VOID);
    }

    private EvalStep evalDo(List<Expr> arguments, Env env) throws EvalError {
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
                return tailSequence(resultExprs, loopEnv);
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
        return completeStep(applyProcedureStep(operator, arguments));
    }

    private Value completeStep(EvalStep step) throws EvalError {
        if (step instanceof ValueStep valueStep) {
            return valueStep.value();
        }

        TailStep tailStep = (TailStep) step;
        return eval(tailStep.expr(), tailStep.env());
    }

    private EvalStep applyProcedureStep(Value operator, List<Value> arguments) throws EvalError {
        return switch (operator) {
            case BuiltinValue builtinValue -> new ValueStep(
                    builtinValue.implementation().apply(arguments));
            case ClosureValue closureValue -> applyClosure(closureValue, arguments);
            case CaseLambdaValue caseLambdaValue -> applyCaseLambda(caseLambdaValue, arguments);
            default -> throw new EvalError("attempted to call a non-procedure");
        };
    }

    private EvalStep applyClosure(ClosureValue closure, List<Value> arguments) throws EvalError {
        return applyProcedureClause(
                closure.formals(),
                closure.body(),
                closure.env(),
                arguments,
                "lambda");
    }

    private EvalStep applyCaseLambda(CaseLambdaValue caseLambda, List<Value> arguments)
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

    private EvalStep applyProcedureClause(Formals formals, List<Expr> body, Env definitionEnv,
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
        return tailSequence(body, callEnv);
    }

    private EvalStep tailSequence(List<Expr> expressions, Env env) throws EvalError {
        if (expressions.isEmpty()) {
            return new ValueStep(VOID);
        }

        for (int index = 0; index < expressions.size() - 1; index++) {
            eval(expressions.get(index), env);
        }
        return new TailStep(expressions.get(expressions.size() - 1), env);
    }

    private Value evalSequence(List<Expr> expressions, Env env) throws EvalError {
        Value result = VOID;
        for (Expr expression : expressions) {
            result = eval(expression, env);
        }
        return result;
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
