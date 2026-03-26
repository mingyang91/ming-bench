package ming;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Scheme interpreter entry point.
 * Agents implement this class.
 */
public class Evaluator {
    private record Binding(String name, Expr valueExpression) {
    }

    private record DoBinding(String name, Expr initExpression, Expr stepExpression) {
    }

    private sealed interface MatchBinding permits SingleMatchBinding, RepeatedMatchBinding {
    }

    private record SingleMatchBinding(Expr value) implements MatchBinding {
    }

    private static final class RepeatedMatchBinding implements MatchBinding {
        private final List<Expr> values = new ArrayList<>();

        List<Expr> values() {
            return values;
        }
    }

    private record ExpansionContext(MacroDefinition macroDefinition,
                                    Environment useEnvironment,
                                    Map<String, MatchBinding> bindings,
                                    Map<String, String> hygienicNames) {
    }

    private static final AtomicLong NEXT_GENSYM = new AtomicLong();

    private StringBuilder outputBuffer;

    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return evalStrWithOutput(input).result();
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        StringBuilder previousOutputBuffer = outputBuffer;
        outputBuffer = new StringBuilder();
        try {
            List<Expr> expressions = new Parser(input).parseProgram();
            if (expressions.isEmpty()) {
                throw new EvalError("expected at least one expression");
            }

            Environment environment = createGlobalEnvironment();
            Value lastValue = VoidValue.INSTANCE;
            for (Expr expression : expressions) {
                lastValue = eval(expression, environment);
            }

            return new EvalResult(lastValue.render(), outputBuffer.toString());
        } finally {
            outputBuffer = previousOutputBuffer;
        }
    }

    Value evalSequence(List<Expr> expressions, Environment environment) throws EvalError {
        Value lastValue = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            lastValue = eval(expression, environment);
        }
        return lastValue;
    }

    private Environment createGlobalEnvironment() {
        Environment environment = new Environment(null);
        environment.define("+", new PrimitiveProcedureValue("+", this::applyAdd));
        environment.define("-", new PrimitiveProcedureValue("-", this::applySubtract));
        environment.define("*", new PrimitiveProcedureValue("*", this::applyMultiply));
        environment.define("/", new PrimitiveProcedureValue("/", this::applyDivide));
        environment.define("<", new PrimitiveProcedureValue("<", arguments -> applyComparison("<", arguments)));
        environment.define(">", new PrimitiveProcedureValue(">", arguments -> applyComparison(">", arguments)));
        environment.define("=", new PrimitiveProcedureValue("=", arguments -> applyComparison("=", arguments)));
        environment.define("<=", new PrimitiveProcedureValue("<=", arguments -> applyComparison("<=", arguments)));
        environment.define(">=", new PrimitiveProcedureValue(">=", arguments -> applyComparison(">=", arguments)));
        environment.define("not", new PrimitiveProcedureValue("not", this::applyNot));
        environment.define("cons", new PrimitiveProcedureValue("cons", this::applyCons));
        environment.define("car", new PrimitiveProcedureValue("car", this::applyCar));
        environment.define("cdr", new PrimitiveProcedureValue("cdr", this::applyCdr));
        environment.define("list", new PrimitiveProcedureValue("list", this::applyList));
        environment.define("append", new PrimitiveProcedureValue("append", this::applyAppend));
        environment.define("length", new PrimitiveProcedureValue("length", this::applyLength));
        environment.define("null?", new PrimitiveProcedureValue("null?", this::applyNullPredicate));
        environment.define("pair?", new PrimitiveProcedureValue("pair?", this::applyPairPredicate));
        environment.define("number?", new PrimitiveProcedureValue("number?", this::applyNumberPredicate));
        environment.define("integer?", new PrimitiveProcedureValue("integer?", this::applyIntegerPredicate));
        environment.define("rational?", new PrimitiveProcedureValue("rational?", this::applyRationalPredicate));
        environment.define("exact?", new PrimitiveProcedureValue("exact?", this::applyExactPredicate));
        environment.define("inexact?", new PrimitiveProcedureValue("inexact?", this::applyInexactPredicate));
        environment.define("string?", new PrimitiveProcedureValue("string?", this::applyStringPredicate));
        environment.define("boolean?", new PrimitiveProcedureValue("boolean?", this::applyBooleanPredicate));
        environment.define("symbol?", new PrimitiveProcedureValue("symbol?", this::applySymbolPredicate));
        environment.define("procedure?", new PrimitiveProcedureValue("procedure?", this::applyProcedurePredicate));
        environment.define("eq?", new PrimitiveProcedureValue("eq?", this::applyEq));
        environment.define("eqv?", new PrimitiveProcedureValue("eqv?", this::applyEqv));
        environment.define("equal?", new PrimitiveProcedureValue("equal?", this::applyEqual));
        environment.define("display", new PrimitiveProcedureValue("display", this::applyDisplay));
        environment.define("write", new PrimitiveProcedureValue("write", this::applyWrite));
        environment.define("newline", new PrimitiveProcedureValue("newline", this::applyNewline));
        environment.define("string-append", new PrimitiveProcedureValue("string-append", this::applyStringAppend));
        environment.define("string-length", new PrimitiveProcedureValue("string-length", this::applyStringLength));
        environment.define("substring", new PrimitiveProcedureValue("substring", this::applySubstring));
        environment.define("string->number", new PrimitiveProcedureValue("string->number", this::applyStringToNumber));
        environment.define("number->string", new PrimitiveProcedureValue("number->string", this::applyNumberToString));
        environment.define("exact->inexact", new PrimitiveProcedureValue("exact->inexact", this::applyExactToInexact));
        environment.define("inexact->exact", new PrimitiveProcedureValue("inexact->exact", this::applyInexactToExact));
        environment.define("numerator", new PrimitiveProcedureValue("numerator", this::applyNumerator));
        environment.define("denominator", new PrimitiveProcedureValue("denominator", this::applyDenominator));
        environment.define("symbol->string", new PrimitiveProcedureValue("symbol->string", this::applySymbolToString));
        environment.define("string->symbol", new PrimitiveProcedureValue("string->symbol", this::applyStringToSymbol));
        environment.define("string-ref", new PrimitiveProcedureValue("string-ref", this::applyStringRef));
        environment.define("string->list", new PrimitiveProcedureValue("string->list", this::applyStringToList));
        environment.define("list->string", new PrimitiveProcedureValue("list->string", this::applyListToString));
        environment.define("string-copy", new PrimitiveProcedureValue("string-copy", this::applyStringCopy));
        environment.define("string-set!", new PrimitiveProcedureValue("string-set!", this::applyStringSet));
        environment.define("string=?", new PrimitiveProcedureValue("string=?", this::applyStringEquality));
        environment.define("string<?", new PrimitiveProcedureValue("string<?", this::applyStringLessThan));
        environment.define("string-ci=?", new PrimitiveProcedureValue("string-ci=?", this::applyStringCiEquality));
        environment.define("string-upcase", new PrimitiveProcedureValue("string-upcase", this::applyStringUpcase));
        environment.define("string-downcase", new PrimitiveProcedureValue("string-downcase", this::applyStringDowncase));
        environment.define("char?", new PrimitiveProcedureValue("char?", this::applyCharPredicate));
        environment.define("char-alphabetic?", new PrimitiveProcedureValue("char-alphabetic?",
                this::applyCharAlphabeticPredicate));
        environment.define("char-numeric?", new PrimitiveProcedureValue("char-numeric?",
                this::applyCharNumericPredicate));
        environment.define("char-upcase", new PrimitiveProcedureValue("char-upcase", this::applyCharUpcase));
        environment.define("char-downcase", new PrimitiveProcedureValue("char-downcase", this::applyCharDowncase));
        environment.define("char=?", new PrimitiveProcedureValue("char=?", this::applyCharEquality));
        environment.define("char<?", new PrimitiveProcedureValue("char<?", this::applyCharLessThan));
        environment.define("char->integer", new PrimitiveProcedureValue("char->integer", this::applyCharToInteger));
        environment.define("integer->char", new PrimitiveProcedureValue("integer->char", this::applyIntegerToChar));
        environment.define("abs", new PrimitiveProcedureValue("abs", this::applyAbs));
        environment.define("modulo", new PrimitiveProcedureValue("modulo", this::applyModulo));
        environment.define("remainder", new PrimitiveProcedureValue("remainder", this::applyRemainder));
        environment.define("quotient", new PrimitiveProcedureValue("quotient", this::applyQuotient));
        environment.define("min", new PrimitiveProcedureValue("min", this::applyMin));
        environment.define("max", new PrimitiveProcedureValue("max", this::applyMax));
        environment.define("expt", new PrimitiveProcedureValue("expt", this::applyExpt));
        environment.define("zero?", new PrimitiveProcedureValue("zero?", this::applyZeroPredicate));
        environment.define("positive?", new PrimitiveProcedureValue("positive?", this::applyPositivePredicate));
        environment.define("negative?", new PrimitiveProcedureValue("negative?", this::applyNegativePredicate));
        environment.define("odd?", new PrimitiveProcedureValue("odd?", this::applyOddPredicate));
        environment.define("even?", new PrimitiveProcedureValue("even?", this::applyEvenPredicate));
        environment.define("list?", new PrimitiveProcedureValue("list?", this::applyListPredicate));
        environment.define("list-ref", new PrimitiveProcedureValue("list-ref", this::applyListRef));
        environment.define("list-tail", new PrimitiveProcedureValue("list-tail", this::applyListTail));
        environment.define("assoc", new PrimitiveProcedureValue("assoc", this::applyAssoc));
        environment.define("apply", new PrimitiveProcedureValue("apply", this::applyApply));
        environment.define("map", new PrimitiveProcedureValue("map", this::applyMap));
        environment.define("vector", new PrimitiveProcedureValue("vector", this::applyVector));
        environment.define("make-vector", new PrimitiveProcedureValue("make-vector", this::applyMakeVector));
        environment.define("vector-ref", new PrimitiveProcedureValue("vector-ref", this::applyVectorRef));
        environment.define("vector-set!", new PrimitiveProcedureValue("vector-set!", this::applyVectorSet));
        environment.define("vector-length", new PrimitiveProcedureValue("vector-length", this::applyVectorLength));
        environment.define("vector?", new PrimitiveProcedureValue("vector?", this::applyVectorPredicate));
        environment.define("vector->list", new PrimitiveProcedureValue("vector->list", this::applyVectorToList));
        environment.define("list->vector", new PrimitiveProcedureValue("list->vector", this::applyListToVector));
        return environment;
    }

    private Value eval(Expr expression, Environment environment) throws EvalError {
        try {
            return switch (expression) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case NumberExpr numberExpr -> Numbers.parseLiteral(numberExpr.token());
                case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
                case StringExpr stringExpr -> new StringValue(stringExpr.value(), false);
                case CharExpr charExpr -> new CharValue(charExpr.value());
                case SymbolExpr symbolExpr -> environment.lookup(symbolExpr.name());
                case ListExpr listExpr -> evalList(listExpr, environment);
            };
        } catch (EvalError error) {
            throw error.withPosition(expression.line(), expression.column());
        }
    }

    private Value evalList(ListExpr listExpr, Environment environment) throws EvalError {
        List<Expr> elements = listExpr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr operatorExpression = elements.getFirst();
        List<Expr> arguments = elements.subList(1, elements.size());

        if (operatorExpression instanceof SymbolExpr symbolExpr) {
            if ("define-syntax".equals(symbolExpr.name())) {
                return evalDefineSyntax(arguments, environment);
            }

            MacroDefinition macroDefinition = environment.lookupMacro(symbolExpr.name());
            if (macroDefinition != null) {
                Expr expanded = expandMacroCall(macroDefinition, listExpr, environment);
                return eval(expanded, environment);
            }

            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, environment);
                case "define-record-type" -> evalDefineRecordType(arguments, environment);
                case "set!" -> evalSet(arguments, environment);
                case "if" -> evalIf(arguments, environment);
                case "quote" -> evalQuote(arguments);
                case "lambda" -> evalLambda(arguments, environment);
                case "case-lambda" -> evalCaseLambda(arguments, environment);
                case "begin" -> evalBegin(arguments, environment);
                case "cond" -> evalCond(arguments, environment);
                case "let" -> evalLet(arguments, environment);
                case "letrec" -> evalLetRec(arguments, environment, false);
                case "letrec*" -> evalLetRec(arguments, environment, true);
                case "case" -> evalCase(arguments, environment);
                case "do" -> evalDo(arguments, environment);
                case "and" -> evalAnd(arguments, environment);
                case "or" -> evalOr(arguments, environment);
                default -> applyProcedure(operatorExpression, arguments, environment);
            };
        }

        return applyProcedure(operatorExpression, arguments, environment);
    }

    private Value applyProcedure(Expr operatorExpression,
                                 List<Expr> argumentExpressions,
                                 Environment environment) throws EvalError {
        Value operator = eval(operatorExpression, environment);
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        List<Value> arguments = new ArrayList<>(argumentExpressions.size());
        for (Expr argumentExpression : argumentExpressions) {
            arguments.add(eval(argumentExpression, environment));
        }
        return procedure.apply(List.copyOf(arguments), this);
    }

    private Value evalDefine(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("define expected a binding target");
        }

        Expr target = arguments.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            requireExactArity("define", arguments.size(), 2);
            Value value = eval(arguments.get(1), environment);
            environment.define(symbolExpr.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signature) {
            List<Expr> signatureElements = signature.elements();
            if (signatureElements.isEmpty()) {
                throw new EvalError("define expected a function name");
            }

            Expr nameExpression = signatureElements.getFirst();
            if (!(nameExpression instanceof SymbolExpr functionName)) {
                throw new EvalError("define expected a function name");
            }

            if (arguments.size() < 2) {
                throw new EvalError("define expected a function body");
            }

            ParameterSpec parameters = parseParameterSpec(
                    signatureElements.subList(1, signatureElements.size()),
                    "define");
            List<Expr> body = arguments.subList(1, arguments.size());
            Value procedure = new LambdaProcedureValue(
                    functionName.name(),
                    parameters.fixedParameters(),
                    parameters.restParameter(),
                    body,
                    environment);
            environment.define(functionName.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("define expected a symbol or function signature");
    }

    private Value evalDefineSyntax(List<Expr> arguments, Environment environment) throws EvalError {
        requireExactArity("define-syntax", arguments.size(), 2);

        Expr nameExpression = arguments.getFirst();
        if (!(nameExpression instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("define-syntax expected a macro name");
        }

        environment.defineMacro(
                symbolExpr.name(),
                parseMacroDefinition(symbolExpr.name(), arguments.get(1), environment));
        return VoidValue.INSTANCE;
    }

    private Value evalDefineRecordType(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 3) {
            throw new EvalError(
                    "define-record-type expected a type name, constructor, predicate, and fields");
        }

        Expr typeNameExpression = arguments.get(0);
        if (!(typeNameExpression instanceof SymbolExpr typeNameSymbol)) {
            throw new EvalError("define-record-type expected a type name");
        }

        Expr constructorExpression = arguments.get(1);
        if (!(constructorExpression instanceof ListExpr constructorList)) {
            throw new EvalError("define-record-type expected a constructor specification");
        }

        List<Expr> constructorElements = constructorList.elements();
        if (constructorElements.isEmpty()) {
            throw new EvalError("define-record-type expected a constructor name");
        }

        Expr constructorNameExpression = constructorElements.getFirst();
        if (!(constructorNameExpression instanceof SymbolExpr constructorSymbol)) {
            throw new EvalError("define-record-type expected a constructor name");
        }

        for (int i = 1; i < constructorElements.size(); i++) {
            if (!(constructorElements.get(i) instanceof SymbolExpr)) {
                throw new EvalError("define-record-type constructor fields must be symbols");
            }
        }

        Expr predicateExpression = arguments.get(2);
        if (!(predicateExpression instanceof SymbolExpr predicateSymbol)) {
            throw new EvalError("define-record-type expected a predicate name");
        }

        List<String> accessorNames = new ArrayList<>(arguments.size() - 3);
        List<String> mutatorNames = new ArrayList<>(arguments.size() - 3);
        for (int i = 3; i < arguments.size(); i++) {
            Expr fieldExpression = arguments.get(i);
            if (!(fieldExpression instanceof ListExpr fieldList)) {
                throw new EvalError("define-record-type field specifications must be lists");
            }

            List<Expr> fieldElements = fieldList.elements();
            if (fieldElements.size() < 2 || fieldElements.size() > 3) {
                throw new EvalError(
                        "define-record-type fields must have a name, accessor, and optional mutator");
            }

            if (!(fieldElements.getFirst() instanceof SymbolExpr)) {
                throw new EvalError("define-record-type field names must be symbols");
            }
            if (!(fieldElements.get(1) instanceof SymbolExpr accessorSymbol)) {
                throw new EvalError("define-record-type accessor names must be symbols");
            }

            String mutatorName = null;
            if (fieldElements.size() == 3) {
                if (!(fieldElements.get(2) instanceof SymbolExpr mutatorSymbol)) {
                    throw new EvalError("define-record-type mutator names must be symbols");
                }
                mutatorName = mutatorSymbol.name();
            }

            accessorNames.add(accessorSymbol.name());
            mutatorNames.add(mutatorName);
        }

        int fieldCount = accessorNames.size();
        if (constructorElements.size() - 1 != fieldCount) {
            throw new EvalError("define-record-type constructor arity does not match field count");
        }

        String constructorName = constructorSymbol.name();
        String predicateName = predicateSymbol.name();
        RecordTypeDescriptor recordType = new RecordTypeDescriptor(typeNameSymbol.name(), fieldCount);

        environment.define(
                constructorName,
                new PrimitiveProcedureValue(constructorName, values -> {
                    requireExactArity(constructorName, values.size(), fieldCount);
                    return new RecordValue(recordType, values);
                }));

        environment.define(
                predicateName,
                new PrimitiveProcedureValue(predicateName, values -> {
                    requireExactArity(predicateName, values.size(), 1);
                    return new BoolValue(values.getFirst() instanceof RecordValue recordValue
                            && recordValue.type() == recordType);
                }));

        for (int i = 0; i < fieldCount; i++) {
            int fieldIndex = i;
            String accessorName = accessorNames.get(i);
            environment.define(
                    accessorName,
                    new PrimitiveProcedureValue(accessorName, values -> {
                        requireExactArity(accessorName, values.size(), 1);
                        return expectRecord(values.getFirst(), recordType, accessorName).field(fieldIndex);
                    }));

            String mutatorName = mutatorNames.get(i);
            if (mutatorName == null) {
                continue;
            }

            environment.define(
                    mutatorName,
                    new PrimitiveProcedureValue(mutatorName, values -> {
                        requireExactArity(mutatorName, values.size(), 2);
                        RecordValue recordValue = expectRecord(values.getFirst(), recordType, mutatorName);
                        recordValue.setField(fieldIndex, values.get(1));
                        return VoidValue.INSTANCE;
                    }));
        }

        return VoidValue.INSTANCE;
    }

    private Value evalSet(List<Expr> arguments, Environment environment) throws EvalError {
        requireExactArity("set!", arguments.size(), 2);

        Expr target = arguments.getFirst();
        if (!(target instanceof SymbolExpr symbolExpr)) {
            throw new EvalError("set! expected a symbol");
        }

        Value value = eval(arguments.get(1), environment);
        environment.set(symbolExpr.name(), value);
        return VoidValue.INSTANCE;
    }

    private Value evalIf(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 2 || arguments.size() > 3) {
            throw new EvalError("if expected 2 or 3 argument(s)");
        }

        Value condition = eval(arguments.get(0), environment);
        if (condition.isTruthy()) {
            return eval(arguments.get(1), environment);
        }
        if (arguments.size() == 2) {
            return VoidValue.INSTANCE;
        }
        return eval(arguments.get(2), environment);
    }

    private Value evalQuote(List<Expr> arguments) throws EvalError {
        requireExactArity("quote", arguments.size(), 1);
        return quote(arguments.getFirst());
    }

    private Value evalLambda(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("lambda expected parameters and a body");
        }

        ParameterSpec parameters = parseParameterSpec(arguments.getFirst(), "lambda");
        List<Expr> body = arguments.subList(1, arguments.size());
        return new LambdaProcedureValue(
                null,
                parameters.fixedParameters(),
                parameters.restParameter(),
                body,
                environment);
    }

    private Value evalCaseLambda(List<Expr> clauses, Environment environment) throws EvalError {
        if (clauses.isEmpty()) {
            throw new EvalError("case-lambda expected at least one clause");
        }

        List<CaseLambdaClause> parsedClauses = new ArrayList<>(clauses.size());
        for (Expr clauseExpression : clauses) {
            if (!(clauseExpression instanceof ListExpr clauseList)) {
                throw new EvalError("case-lambda clauses must be lists");
            }

            List<Expr> clauseElements = clauseList.elements();
            if (clauseElements.size() < 2) {
                throw new EvalError("case-lambda clauses expected parameters and a body");
            }

            parsedClauses.add(new CaseLambdaClause(
                    parseParameterSpec(clauseElements.getFirst(), "case-lambda"),
                    clauseElements.subList(1, clauseElements.size())));
        }

        return new CaseLambdaProcedureValue(List.copyOf(parsedClauses), environment);
    }

    private Value evalBegin(List<Expr> arguments, Environment environment) throws EvalError {
        return evalSequence(arguments, environment);
    }

    private Value evalCond(List<Expr> clauses, Environment environment) throws EvalError {
        for (int i = 0; i < clauses.size(); i++) {
            Expr clauseExpression = clauses.get(i);
            if (!(clauseExpression instanceof ListExpr clauseList)) {
                throw new EvalError("cond clauses must be lists");
            }

            List<Expr> clauseElements = clauseList.elements();
            if (clauseElements.isEmpty()) {
                throw new EvalError("cond clause cannot be empty");
            }

            Expr testExpression = clauseElements.getFirst();
            if (testExpression instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
                if (i != clauses.size() - 1) {
                    throw new EvalError("cond else clause must be last");
                }
                if (clauseElements.size() == 1) {
                    throw new EvalError("cond else clause expected a body");
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }

            Value testValue = eval(testExpression, environment);
            if (testValue.isTruthy()) {
                if (clauseElements.size() == 1) {
                    return testValue;
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalLet(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let expected bindings and a body");
        }

        Expr firstArgument = arguments.getFirst();
        if (firstArgument instanceof SymbolExpr name) {
            return evalNamedLet(name.name(), arguments.subList(1, arguments.size()), environment);
        }

        List<Binding> bindings = parseBindings(firstArgument, "let");
        List<Expr> body = arguments.subList(1, arguments.size());
        Environment localEnvironment = new Environment(environment);
        for (Binding binding : bindings) {
            localEnvironment.define(binding.name(), eval(binding.valueExpression(), environment));
        }
        return evalSequence(body, localEnvironment);
    }

    private Value evalLetRec(List<Expr> arguments,
                             Environment environment,
                             boolean sequential) throws EvalError {
        String formName = sequential ? "letrec*" : "letrec";
        if (arguments.size() < 2) {
            throw new EvalError(formName + " expected bindings and a body");
        }

        List<Binding> bindings = parseBindings(arguments.getFirst(), formName);
        List<Expr> body = arguments.subList(1, arguments.size());
        Environment localEnvironment = new Environment(environment);
        List<BindingCell> cells = new ArrayList<>(bindings.size());

        for (Binding binding : bindings) {
            BindingCell cell = new BindingCell(UninitializedValue.INSTANCE);
            localEnvironment.defineCell(binding.name(), cell);
            cells.add(cell);
        }

        if (sequential) {
            for (int i = 0; i < bindings.size(); i++) {
                cells.get(i).set(eval(bindings.get(i).valueExpression(), localEnvironment));
            }
        } else {
            List<Value> values = new ArrayList<>(bindings.size());
            for (Binding binding : bindings) {
                values.add(eval(binding.valueExpression(), localEnvironment));
            }
            for (int i = 0; i < cells.size(); i++) {
                cells.get(i).set(values.get(i));
            }
        }

        return evalSequence(body, localEnvironment);
    }

    private Value evalCase(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.isEmpty()) {
            throw new EvalError("case expected a key and at least one clause");
        }

        Value key = eval(arguments.getFirst(), environment);
        for (int i = 1; i < arguments.size(); i++) {
            Expr clauseExpression = arguments.get(i);
            if (!(clauseExpression instanceof ListExpr clauseList)) {
                throw new EvalError("case clauses must be lists");
            }

            List<Expr> clauseElements = clauseList.elements();
            if (clauseElements.isEmpty()) {
                throw new EvalError("case clause cannot be empty");
            }

            Expr datumsExpression = clauseElements.getFirst();
            if (datumsExpression instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
                if (i != arguments.size() - 1) {
                    throw new EvalError("case else clause must be last");
                }
                if (clauseElements.size() == 1) {
                    throw new EvalError("case else clause expected a body");
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }

            if (!(datumsExpression instanceof ListExpr datumsList)) {
                throw new EvalError("case clause expected a datum list");
            }

            for (Expr datum : datumsList.elements()) {
                if (eqvValue(key, quote(datum))) {
                    if (clauseElements.size() == 1) {
                        return VoidValue.INSTANCE;
                    }
                    return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
                }
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalDo(List<Expr> arguments, Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("do expected bindings and a termination clause");
        }

        List<DoBinding> bindings = parseDoBindings(arguments.getFirst());
        if (!(arguments.get(1) instanceof ListExpr testClause)) {
            throw new EvalError("do termination clause must be a list");
        }

        List<Expr> testClauseElements = testClause.elements();
        if (testClauseElements.isEmpty()) {
            throw new EvalError("do termination clause cannot be empty");
        }

        Environment loopEnvironment = new Environment(environment);
        List<BindingCell> cells = new ArrayList<>(bindings.size());
        for (DoBinding binding : bindings) {
            BindingCell cell = new BindingCell(eval(binding.initExpression(), environment));
            loopEnvironment.defineCell(binding.name(), cell);
            cells.add(cell);
        }

        List<Expr> body = arguments.subList(2, arguments.size());
        while (true) {
            Value testValue = eval(testClauseElements.getFirst(), loopEnvironment);
            if (testValue.isTruthy()) {
                if (testClauseElements.size() == 1) {
                    return VoidValue.INSTANCE;
                }
                return evalSequence(testClauseElements.subList(1, testClauseElements.size()), loopEnvironment);
            }

            if (!body.isEmpty()) {
                evalSequence(body, loopEnvironment);
            }

            List<Value> nextValues = new ArrayList<>(bindings.size());
            for (int i = 0; i < bindings.size(); i++) {
                Expr stepExpression = bindings.get(i).stepExpression();
                if (stepExpression == null) {
                    nextValues.add(cells.get(i).get());
                } else {
                    nextValues.add(eval(stepExpression, loopEnvironment));
                }
            }

            for (int i = 0; i < cells.size(); i++) {
                cells.get(i).set(nextValues.get(i));
            }
        }
    }

    private Value evalNamedLet(String name,
                               List<Expr> arguments,
                               Environment environment) throws EvalError {
        if (arguments.size() < 2) {
            throw new EvalError("let expected bindings and a body");
        }

        List<Binding> bindings = parseBindings(arguments.getFirst(), "let");
        List<String> parameters = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            parameters.add(binding.name());
        }

        List<Expr> body = arguments.subList(1, arguments.size());
        Environment localEnvironment = new Environment(environment);
        LambdaProcedureValue procedure = new LambdaProcedureValue(
                name,
                List.copyOf(parameters),
                null,
                body,
                localEnvironment);
        localEnvironment.define(name, procedure);

        List<Value> initialValues = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            initialValues.add(eval(binding.valueExpression(), localEnvironment));
        }

        return procedure.apply(List.copyOf(initialValues), this);
    }

    private List<Binding> parseBindings(Expr bindingsExpression, String formName) throws EvalError {
        if (!(bindingsExpression instanceof ListExpr bindingsList)) {
            throw new EvalError(formName + " bindings must be a list");
        }

        List<Binding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpression : bindingsList.elements()) {
            if (!(bindingExpression instanceof ListExpr bindingList)) {
                throw new EvalError(formName + " bindings must be lists");
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError(formName + " bindings must have a name and value");
            }

            Expr nameExpression = bindingElements.getFirst();
            if (!(nameExpression instanceof SymbolExpr symbolExpr)) {
                throw new EvalError(formName + " bindings must start with a symbol");
            }

            bindings.add(new Binding(symbolExpr.name(), bindingElements.get(1)));
        }

        return List.copyOf(bindings);
    }

    private List<DoBinding> parseDoBindings(Expr bindingsExpression) throws EvalError {
        if (!(bindingsExpression instanceof ListExpr bindingsList)) {
            throw new EvalError("do bindings must be a list");
        }

        List<DoBinding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpression : bindingsList.elements()) {
            if (!(bindingExpression instanceof ListExpr bindingList)) {
                throw new EvalError("do bindings must be lists");
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() < 2 || bindingElements.size() > 3) {
                throw new EvalError("do bindings must have a name, init, and optional step");
            }

            if (!(bindingElements.getFirst() instanceof SymbolExpr symbolExpr)) {
                throw new EvalError("do bindings must start with a symbol");
            }

            Expr stepExpression = bindingElements.size() == 3 ? bindingElements.get(2) : null;
            bindings.add(new DoBinding(symbolExpr.name(), bindingElements.get(1), stepExpression));
        }

        return List.copyOf(bindings);
    }

    private ParameterSpec parseParameterSpec(Expr parametersExpression, String formName)
            throws EvalError {
        if (!(parametersExpression instanceof ListExpr parametersList)) {
            throw new EvalError(formName + " parameters must be a list");
        }
        return parseParameterSpec(parametersList.elements(), formName);
    }

    private ParameterSpec parseParameterSpec(List<Expr> parameterExpressions, String formName)
            throws EvalError {
        List<String> parameterNames = new ArrayList<>(parameterExpressions.size());
        String restParameter = null;

        for (int i = 0; i < parameterExpressions.size(); i++) {
            Expr parameterExpression = parameterExpressions.get(i);
            if (parameterExpression instanceof SymbolExpr symbolExpr && ".".equals(symbolExpr.name())) {
                if (i != parameterExpressions.size() - 2) {
                    throw new EvalError(formName + " parameters use invalid dotted form");
                }

                Expr restExpression = parameterExpressions.get(i + 1);
                if (!(restExpression instanceof SymbolExpr restSymbol) || ".".equals(restSymbol.name())) {
                    throw new EvalError(formName + " parameters must be symbols");
                }
                restParameter = restSymbol.name();
                break;
            }

            if (!(parameterExpression instanceof SymbolExpr symbolExpr)) {
                throw new EvalError(formName + " parameters must be symbols");
            }
            parameterNames.add(symbolExpr.name());
        }

        return new ParameterSpec(List.copyOf(parameterNames), restParameter);
    }

    private MacroDefinition parseMacroDefinition(String name,
                                                 Expr transformer,
                                                 Environment environment) throws EvalError {
        if (!(transformer instanceof ListExpr transformerList)) {
            throw new EvalError("define-syntax expected a syntax-rules transformer");
        }

        List<Expr> elements = transformerList.elements();
        if (elements.isEmpty()) {
            throw new EvalError("define-syntax expected a syntax-rules transformer");
        }

        Expr headExpression = elements.getFirst();
        if (!(headExpression instanceof SymbolExpr headSymbol)
                || !"syntax-rules".equals(headSymbol.name())) {
            throw new EvalError("define-syntax expected syntax-rules");
        }
        if (elements.size() < 3) {
            throw new EvalError("syntax-rules expected literals and at least one rule");
        }

        Expr literalsExpression = elements.get(1);
        if (!(literalsExpression instanceof ListExpr literalList)) {
            throw new EvalError("syntax-rules literals must be a list");
        }

        Set<String> literals = new HashSet<>();
        for (Expr literalExpression : literalList.elements()) {
            if (!(literalExpression instanceof SymbolExpr literalSymbol)) {
                throw new EvalError("syntax-rules literals must be identifiers");
            }
            literals.add(literalSymbol.name());
        }

        List<SyntaxRule> rules = new ArrayList<>(elements.size() - 2);
        for (int i = 2; i < elements.size(); i++) {
            Expr ruleExpression = elements.get(i);
            if (!(ruleExpression instanceof ListExpr ruleList)) {
                throw new EvalError("syntax-rules clauses must be lists");
            }
            List<Expr> ruleElements = ruleList.elements();
            if (ruleElements.size() != 2) {
                throw new EvalError("syntax-rules clauses must contain a pattern and template");
            }
            rules.add(new SyntaxRule(ruleElements.get(0), ruleElements.get(1)));
        }

        return new MacroDefinition(name, literals, rules, environment);
    }

    private Expr expandMacroCall(MacroDefinition macroDefinition,
                                 Expr callExpression,
                                 Environment useEnvironment) throws EvalError {
        for (SyntaxRule rule : macroDefinition.rules()) {
            Map<String, MatchBinding> bindings = new HashMap<>();
            if (matchPattern(rule.pattern(), callExpression, macroDefinition, bindings)) {
                ExpansionContext context = new ExpansionContext(
                        macroDefinition,
                        useEnvironment,
                        bindings,
                        new HashMap<>());
                return instantiateTemplate(rule.template(), context, Map.of(), null);
            }
        }

        throw new EvalError("no matching syntax-rules clause for " + macroDefinition.name());
    }

    private boolean matchPattern(Expr pattern,
                                 Expr input,
                                 MacroDefinition macroDefinition,
                                 Map<String, MatchBinding> bindings) throws EvalError {
        if (pattern instanceof IntExpr expected && input instanceof IntExpr found) {
            return expected.value() == found.value();
        }
        if (pattern instanceof NumberExpr expected && input instanceof NumberExpr found) {
            return expected.token().equals(found.token());
        }
        if (pattern instanceof BoolExpr expected && input instanceof BoolExpr found) {
            return expected.value() == found.value();
        }
        if (pattern instanceof StringExpr expected && input instanceof StringExpr found) {
            return expected.value().equals(found.value());
        }
        if (pattern instanceof CharExpr expected && input instanceof CharExpr found) {
            return expected.value() == found.value();
        }
        if (pattern instanceof SymbolExpr symbolPattern) {
            String name = symbolPattern.name();
            if ("...".equals(name)) {
                return false;
            }
            if ("_".equals(name)) {
                return true;
            }
            if (macroDefinition.name().equals(name) || macroDefinition.literals().contains(name)) {
                return input instanceof SymbolExpr found && name.equals(found.name());
            }

            MatchBinding existing = bindings.get(name);
            if (existing instanceof SingleMatchBinding singleBinding) {
                return exprSameStructure(singleBinding.value(), input);
            }
            if (existing instanceof RepeatedMatchBinding) {
                return false;
            }

            bindings.put(name, new SingleMatchBinding(input));
            return true;
        }
        if (pattern instanceof ListExpr patternList && input instanceof ListExpr inputList) {
            return matchPatternList(
                    patternList.elements(),
                    inputList.elements(),
                    macroDefinition,
                    bindings);
        }

        return false;
    }

    private boolean matchPatternList(List<Expr> patternElements,
                                     List<Expr> inputElements,
                                     MacroDefinition macroDefinition,
                                     Map<String, MatchBinding> bindings) throws EvalError {
        if (patternElements.size() >= 2 && isEllipsis(patternElements.getLast())) {
            List<Expr> fixedPatterns = patternElements.subList(0, patternElements.size() - 2);
            Expr repeatedPattern = patternElements.get(patternElements.size() - 2);

            if (inputElements.size() < fixedPatterns.size()) {
                return false;
            }

            for (int i = 0; i < fixedPatterns.size(); i++) {
                if (!matchPattern(fixedPatterns.get(i), inputElements.get(i), macroDefinition, bindings)) {
                    return false;
                }
            }

            initializeRepeatedBindings(repeatedPattern, macroDefinition, bindings);
            for (int i = fixedPatterns.size(); i < inputElements.size(); i++) {
                Map<String, MatchBinding> iterationBindings = new HashMap<>();
                if (!matchPattern(repeatedPattern, inputElements.get(i), macroDefinition, iterationBindings)) {
                    return false;
                }
                if (!mergeRepeatedBindings(bindings, iterationBindings)) {
                    return false;
                }
            }
            return true;
        }

        if (patternElements.size() != inputElements.size()) {
            return false;
        }

        for (int i = 0; i < patternElements.size(); i++) {
            if (!matchPattern(patternElements.get(i), inputElements.get(i), macroDefinition, bindings)) {
                return false;
            }
        }
        return true;
    }

    private void initializeRepeatedBindings(Expr pattern,
                                            MacroDefinition macroDefinition,
                                            Map<String, MatchBinding> bindings) throws EvalError {
        Set<String> variables = new HashSet<>();
        collectPatternVariables(pattern, macroDefinition, variables);

        for (String variable : variables) {
            MatchBinding existing = bindings.get(variable);
            if (existing instanceof SingleMatchBinding) {
                throw new EvalError("macro variable `" + variable + "` used inconsistently");
            }
            bindings.putIfAbsent(variable, new RepeatedMatchBinding());
        }
    }

    private boolean mergeRepeatedBindings(Map<String, MatchBinding> bindings,
                                          Map<String, MatchBinding> iterationBindings) {
        for (Map.Entry<String, MatchBinding> entry : iterationBindings.entrySet()) {
            List<Expr> values = switch (entry.getValue()) {
                case SingleMatchBinding singleBinding -> List.of(singleBinding.value());
                case RepeatedMatchBinding repeatedBinding -> repeatedBinding.values();
            };

            MatchBinding existing = bindings.get(entry.getKey());
            if (existing instanceof RepeatedMatchBinding repeatedBinding) {
                repeatedBinding.values().addAll(values);
                continue;
            }
            if (existing instanceof SingleMatchBinding) {
                return false;
            }

            RepeatedMatchBinding repeatedBinding = new RepeatedMatchBinding();
            repeatedBinding.values().addAll(values);
            bindings.put(entry.getKey(), repeatedBinding);
        }
        return true;
    }

    private void collectPatternVariables(Expr pattern,
                                         MacroDefinition macroDefinition,
                                         Set<String> variables) {
        if (pattern instanceof SymbolExpr symbolExpr) {
            String name = symbolExpr.name();
            if (!"...".equals(name)
                    && !"_".equals(name)
                    && !macroDefinition.name().equals(name)
                    && !macroDefinition.literals().contains(name)) {
                variables.add(name);
            }
            return;
        }

        if (pattern instanceof ListExpr listExpr) {
            for (Expr element : listExpr.elements()) {
                if (!isEllipsis(element)) {
                    collectPatternVariables(element, macroDefinition, variables);
                }
            }
        }
    }

    private Expr instantiateTemplate(Expr template,
                                     ExpansionContext context,
                                     Map<String, String> scope,
                                     Integer repeatIndex) throws EvalError {
        return switch (template) {
            case IntExpr ignored -> template;
            case NumberExpr ignored -> template;
            case BoolExpr ignored -> template;
            case StringExpr ignored -> template;
            case CharExpr ignored -> template;
            case SymbolExpr symbolExpr -> instantiateSymbol(symbolExpr, context, scope, repeatIndex);
            case ListExpr listExpr -> instantiateTemplateList(
                    listExpr,
                    listExpr.elements(),
                    context,
                    scope,
                    repeatIndex);
        };
    }

    private Expr instantiateTemplateList(Expr template,
                                         List<Expr> elements,
                                         ExpansionContext context,
                                         Map<String, String> scope,
                                         Integer repeatIndex) throws EvalError {
        if (!elements.isEmpty() && elements.getFirst() instanceof SymbolExpr headSymbol) {
            if ("let".equals(headSymbol.name())) {
                return instantiateTemplateLet(template, elements, context, scope, repeatIndex);
            }
            if ("lambda".equals(headSymbol.name())) {
                return instantiateTemplateLambda(template, elements, context, scope, repeatIndex);
            }
            if ("case-lambda".equals(headSymbol.name())) {
                return instantiateTemplateCaseLambda(template, elements, context, scope, repeatIndex);
            }
        }

        List<Expr> expanded = new ArrayList<>(elements.size());
        int index = 0;
        while (index < elements.size()) {
            if (index + 1 < elements.size() && isEllipsis(elements.get(index + 1))) {
                int repeatCount = repetitionCount(elements.get(index), context.bindings());
                for (int currentIndex = 0; currentIndex < repeatCount; currentIndex++) {
                    expanded.add(instantiateTemplate(
                            elements.get(index),
                            context,
                            scope,
                            currentIndex));
                }
                index += 2;
                continue;
            }

            expanded.add(instantiateTemplate(elements.get(index), context, scope, repeatIndex));
            index++;
        }

        return new ListExpr(List.copyOf(expanded), template.line(), template.column());
    }

    private Expr instantiateTemplateLet(Expr template,
                                        List<Expr> elements,
                                        ExpansionContext context,
                                        Map<String, String> scope,
                                        Integer repeatIndex) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("macro let template expected bindings and a body");
        }

        Expr bindingsExpression = elements.get(1);
        if (!(bindingsExpression instanceof ListExpr bindingList)) {
            throw new EvalError("macro let template expected a binding list");
        }

        Map<String, String> localScope = new HashMap<>(scope);
        List<Expr> expandedBindings = new ArrayList<>(bindingList.elements().size());
        for (Expr bindingTemplate : bindingList.elements()) {
            if (!(bindingTemplate instanceof ListExpr bindingExprList)) {
                throw new EvalError("macro let bindings must be lists");
            }

            List<Expr> bindingElements = bindingExprList.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError("macro let bindings must contain a name and value");
            }

            Expr bindingName = bindingElements.get(0);
            Expr expandedName;
            if (bindingName instanceof SymbolExpr symbolExpr
                    && !context.bindings().containsKey(symbolExpr.name())) {
                String renamed = freshSymbol(symbolExpr.name());
                localScope.put(symbolExpr.name(), renamed);
                expandedName = new SymbolExpr(renamed, bindingName.line(), bindingName.column());
            } else {
                expandedName = instantiateTemplate(bindingName, context, scope, repeatIndex);
            }

            Expr expandedValue = instantiateTemplate(
                    bindingElements.get(1),
                    context,
                    scope,
                    repeatIndex);
            expandedBindings.add(new ListExpr(
                    List.of(expandedName, expandedValue),
                    bindingTemplate.line(),
                    bindingTemplate.column()));
        }

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(new SymbolExpr("let", elements.getFirst().line(), elements.getFirst().column()));
        expanded.add(new ListExpr(
                List.copyOf(expandedBindings),
                bindingsExpression.line(),
                bindingsExpression.column()));
        for (int i = 2; i < elements.size(); i++) {
            expanded.add(instantiateTemplate(elements.get(i), context, localScope, repeatIndex));
        }
        return new ListExpr(List.copyOf(expanded), template.line(), template.column());
    }

    private Expr instantiateTemplateLambda(Expr template,
                                           List<Expr> elements,
                                           ExpansionContext context,
                                           Map<String, String> scope,
                                           Integer repeatIndex) throws EvalError {
        if (elements.size() < 3) {
            throw new EvalError("macro lambda template expected parameters and a body");
        }

        Map<String, String> localScope = new HashMap<>(scope);
        Expr parameterExpression = instantiateLambdaParameters(
                elements.get(1),
                context,
                scope,
                localScope,
                repeatIndex);

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(new SymbolExpr("lambda", elements.getFirst().line(), elements.getFirst().column()));
        expanded.add(parameterExpression);
        for (int i = 2; i < elements.size(); i++) {
            expanded.add(instantiateTemplate(elements.get(i), context, localScope, repeatIndex));
        }
        return new ListExpr(List.copyOf(expanded), template.line(), template.column());
    }

    private Expr instantiateTemplateCaseLambda(Expr template,
                                               List<Expr> elements,
                                               ExpansionContext context,
                                               Map<String, String> scope,
                                               Integer repeatIndex) throws EvalError {
        if (elements.size() < 2) {
            throw new EvalError("macro case-lambda template expected at least one clause");
        }

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(new SymbolExpr("case-lambda", elements.getFirst().line(), elements.getFirst().column()));
        for (int i = 1; i < elements.size(); i++) {
            Expr clauseExpression = elements.get(i);
            if (!(clauseExpression instanceof ListExpr clauseList)) {
                throw new EvalError("macro case-lambda clauses must be lists");
            }

            List<Expr> clauseElements = clauseList.elements();
            if (clauseElements.size() < 2) {
                throw new EvalError("macro case-lambda clause expected parameters and a body");
            }

            Map<String, String> localScope = new HashMap<>(scope);
            Expr parameterExpression = instantiateLambdaParameters(
                    clauseElements.getFirst(),
                    context,
                    scope,
                    localScope,
                    repeatIndex);

            List<Expr> expandedClause = new ArrayList<>(clauseElements.size());
            expandedClause.add(parameterExpression);
            for (int j = 1; j < clauseElements.size(); j++) {
                expandedClause.add(instantiateTemplate(
                        clauseElements.get(j),
                        context,
                        localScope,
                        repeatIndex));
            }
            expanded.add(new ListExpr(
                    List.copyOf(expandedClause),
                    clauseExpression.line(),
                    clauseExpression.column()));
        }
        return new ListExpr(List.copyOf(expanded), template.line(), template.column());
    }

    private Expr instantiateLambdaParameters(Expr parametersExpression,
                                             ExpansionContext context,
                                             Map<String, String> scope,
                                             Map<String, String> localScope,
                                             Integer repeatIndex) throws EvalError {
        if (parametersExpression instanceof SymbolExpr symbolExpr) {
            if (context.bindings().containsKey(symbolExpr.name())) {
                return instantiateTemplate(parametersExpression, context, scope, repeatIndex);
            }

            String renamed = freshSymbol(symbolExpr.name());
            localScope.put(symbolExpr.name(), renamed);
            return new SymbolExpr(renamed, parametersExpression.line(), parametersExpression.column());
        }

        if (!(parametersExpression instanceof ListExpr parameterList)) {
            throw new EvalError("macro lambda template expected a parameter list");
        }

        List<Expr> expandedParameters = new ArrayList<>(parameterList.elements().size());
        for (Expr parameterExpression : parameterList.elements()) {
            if (parameterExpression instanceof SymbolExpr parameterSymbol) {
                if (".".equals(parameterSymbol.name())) {
                    expandedParameters.add(parameterExpression);
                    continue;
                }
                if (!context.bindings().containsKey(parameterSymbol.name())) {
                    String renamed = freshSymbol(parameterSymbol.name());
                    localScope.put(parameterSymbol.name(), renamed);
                    expandedParameters.add(new SymbolExpr(
                            renamed,
                            parameterExpression.line(),
                            parameterExpression.column()));
                    continue;
                }
            }

            expandedParameters.add(instantiateTemplate(
                    parameterExpression,
                    context,
                    scope,
                    repeatIndex));
        }
        return new ListExpr(
                List.copyOf(expandedParameters),
                parametersExpression.line(),
                parametersExpression.column());
    }

    private Expr instantiateSymbol(SymbolExpr symbolExpr,
                                   ExpansionContext context,
                                   Map<String, String> scope,
                                   Integer repeatIndex) throws EvalError {
        String name = symbolExpr.name();
        String scopedName = scope.get(name);
        if (scopedName != null) {
            return new SymbolExpr(scopedName, symbolExpr.line(), symbolExpr.column());
        }

        MatchBinding binding = context.bindings().get(name);
        if (binding instanceof SingleMatchBinding singleBinding) {
            return singleBinding.value();
        }
        if (binding instanceof RepeatedMatchBinding repeatedBinding) {
            if (repeatIndex == null) {
                throw new EvalError("macro variable `" + name + "` used without ellipsis");
            }
            if (repeatIndex < 0 || repeatIndex >= repeatedBinding.values().size()) {
                throw new EvalError("macro variable `" + name + "` repetition index out of range");
            }
            return repeatedBinding.values().get(repeatIndex);
        }

        return new SymbolExpr(
                hygienicIdentifier(name, context),
                symbolExpr.line(),
                symbolExpr.column());
    }

    private int repetitionCount(Expr template, Map<String, MatchBinding> bindings) throws EvalError {
        int[] count = new int[]{-1};
        collectRepetitionCount(template, bindings, count);
        if (count[0] < 0) {
            throw new EvalError("ellipsis template has no repeated pattern variables");
        }
        return count[0];
    }

    private void collectRepetitionCount(Expr template,
                                        Map<String, MatchBinding> bindings,
                                        int[] count) throws EvalError {
        if (template instanceof SymbolExpr symbolExpr) {
            MatchBinding binding = bindings.get(symbolExpr.name());
            if (binding instanceof RepeatedMatchBinding repeatedBinding) {
                int size = repeatedBinding.values().size();
                if (count[0] >= 0 && count[0] != size) {
                    throw new EvalError("macro template ellipsis lengths do not match");
                }
                count[0] = size;
            }
            return;
        }

        if (template instanceof ListExpr listExpr) {
            for (Expr element : listExpr.elements()) {
                if (!isEllipsis(element)) {
                    collectRepetitionCount(element, bindings, count);
                }
            }
        }
    }

    private String hygienicIdentifier(String name, ExpansionContext context) {
        if (isSpecialForm(name)
                || context.macroDefinition().name().equals(name)
                || context.macroDefinition().definingEnvironment().lookupMacro(name) != null) {
            return name;
        }

        String existing = context.hygienicNames().get(name);
        if (existing != null) {
            return existing;
        }

        String renamed = freshSymbol(name);
        BindingCell cell = context.macroDefinition().definingEnvironment().lookupCell(name);
        if (cell != null) {
            context.useEnvironment().defineCell(renamed, cell);
        }
        context.hygienicNames().put(name, renamed);
        return renamed;
    }

    private String freshSymbol(String base) {
        return "__macro_" + sanitizeSymbol(base) + "_" + NEXT_GENSYM.getAndIncrement();
    }

    private String sanitizeSymbol(String name) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            builder.append(Character.isLetterOrDigit(ch) ? ch : '_');
        }
        return builder.isEmpty() ? "id" : builder.toString();
    }

    private boolean isSpecialForm(String name) {
        return switch (name) {
            case "define",
                    "define-syntax",
                    "define-record-type",
                    "if",
                    "quote",
                    "lambda",
                    "case-lambda",
                    "begin",
                    "cond",
                    "let",
                    "letrec",
                    "letrec*",
                    "case",
                    "do",
                    "and",
                    "or",
                    "set!",
                    "syntax-rules" -> true;
            default -> false;
        };
    }

    private boolean isEllipsis(Expr expression) {
        return expression instanceof SymbolExpr symbolExpr && "...".equals(symbolExpr.name());
    }

    private boolean exprSameStructure(Expr left, Expr right) {
        if (left instanceof IntExpr leftInt && right instanceof IntExpr rightInt) {
            return leftInt.value() == rightInt.value();
        }
        if (left instanceof NumberExpr leftNumber && right instanceof NumberExpr rightNumber) {
            return leftNumber.token().equals(rightNumber.token());
        }
        if (left instanceof BoolExpr leftBool && right instanceof BoolExpr rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof StringExpr leftString && right instanceof StringExpr rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof CharExpr leftChar && right instanceof CharExpr rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolExpr leftSymbol && right instanceof SymbolExpr rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        if (left instanceof ListExpr leftList && right instanceof ListExpr rightList) {
            if (leftList.elements().size() != rightList.elements().size()) {
                return false;
            }
            for (int i = 0; i < leftList.elements().size(); i++) {
                if (!exprSameStructure(leftList.elements().get(i), rightList.elements().get(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private Value quote(Expr expression) throws EvalError {
        return switch (expression) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case NumberExpr numberExpr -> Numbers.parseLiteral(numberExpr.token());
            case BoolExpr boolExpr -> new BoolValue(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value(), false);
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> {
                List<Value> elements = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    elements.add(quote(element));
                }
                yield new ListValue(List.copyOf(elements));
            }
        };
    }

    private Value evalAnd(List<Expr> arguments, Environment environment) throws EvalError {
        Value last = new BoolValue(true);
        for (Expr argument : arguments) {
            last = eval(argument, environment);
            if (!last.isTruthy()) {
                return last;
            }
        }
        return last;
    }

    private Value evalOr(List<Expr> arguments, Environment environment) throws EvalError {
        Value last = new BoolValue(false);
        for (Expr argument : arguments) {
            last = eval(argument, environment);
            if (last.isTruthy()) {
                return last;
            }
        }
        return last;
    }

    private Value applyNot(List<Value> arguments) throws EvalError {
        requireExactArity("not", arguments.size(), 1);
        return new BoolValue(!arguments.getFirst().isTruthy());
    }

    private Value applyCons(List<Value> arguments) throws EvalError {
        requireExactArity("cons", arguments.size(), 2);
        Value tail = arguments.get(1);
        if (tail instanceof ListValue listValue) {
            List<Value> elements = new ArrayList<>(listValue.elements().size() + 1);
            elements.add(arguments.getFirst());
            elements.addAll(listValue.elements());
            return new ListValue(List.copyOf(elements));
        }
        return new PairValue(arguments.getFirst(), tail);
    }

    private Value applyCar(List<Value> arguments) throws EvalError {
        requireExactArity("car", arguments.size(), 1);
        return car(arguments.getFirst(), "car");
    }

    private Value applyCdr(List<Value> arguments) throws EvalError {
        requireExactArity("cdr", arguments.size(), 1);
        return cdr(arguments.getFirst(), "cdr");
    }

    private Value applyList(List<Value> arguments) {
        return new ListValue(List.copyOf(arguments));
    }

    private Value applyAppend(List<Value> arguments) throws EvalError {
        List<Value> appended = new ArrayList<>();
        for (Value argument : arguments) {
            appended.addAll(expectList(argument, "append"));
        }
        return new ListValue(List.copyOf(appended));
    }

    private Value applyLength(List<Value> arguments) throws EvalError {
        requireExactArity("length", arguments.size(), 1);
        return new IntValue(expectList(arguments.getFirst(), "length").size());
    }

    private Value applyNullPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("null?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof ListValue listValue
                && listValue.elements().isEmpty());
    }

    private Value applyPairPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("pair?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof PairValue
                || arguments.getFirst() instanceof ListValue listValue && !listValue.elements().isEmpty());
    }

    private Value applyNumberPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("number?", arguments.size(), 1);
        return new BoolValue(Numbers.isNumber(arguments.getFirst()));
    }

    private Value applyIntegerPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("integer?", arguments.size(), 1);
        return new BoolValue(Numbers.isInteger(arguments.getFirst()));
    }

    private Value applyRationalPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("rational?", arguments.size(), 1);
        return new BoolValue(Numbers.isRational(arguments.getFirst()));
    }

    private Value applyExactPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("exact?", arguments.size(), 1);
        return new BoolValue(Numbers.isExact(arguments.getFirst()));
    }

    private Value applyInexactPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("inexact?", arguments.size(), 1);
        return new BoolValue(Numbers.isInexact(arguments.getFirst()));
    }

    private Value applyStringPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("string?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof StringValue);
    }

    private Value applyBooleanPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("boolean?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof BoolValue);
    }

    private Value applySymbolPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("symbol?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof SymbolValue);
    }

    private Value applyProcedurePredicate(List<Value> arguments) throws EvalError {
        requireExactArity("procedure?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof ProcedureValue);
    }

    private Value applyEq(List<Value> arguments) throws EvalError {
        requireExactArity("eq?", arguments.size(), 2);
        return new BoolValue(eqValue(arguments.get(0), arguments.get(1)));
    }

    private Value applyEqv(List<Value> arguments) throws EvalError {
        requireExactArity("eqv?", arguments.size(), 2);
        return new BoolValue(eqvValue(arguments.get(0), arguments.get(1)));
    }

    private Value applyEqual(List<Value> arguments) throws EvalError {
        requireExactArity("equal?", arguments.size(), 2);
        return new BoolValue(equalValue(arguments.get(0), arguments.get(1)));
    }

    private Value applyListPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("list?", arguments.size(), 1);
        return new BoolValue(isProperList(arguments.getFirst()));
    }

    private Value applyListRef(List<Value> arguments) throws EvalError {
        requireExactArity("list-ref", arguments.size(), 2);
        List<Value> elements = expectList(arguments.getFirst(), "list-ref");
        int index = expectIndex(arguments.get(1), "list-ref");
        if (index >= elements.size()) {
            throw new EvalError("list-ref index out of range");
        }
        return elements.get(index);
    }

    private Value applyListTail(List<Value> arguments) throws EvalError {
        requireExactArity("list-tail", arguments.size(), 2);
        List<Value> elements = expectList(arguments.getFirst(), "list-tail");
        int index = expectIndex(arguments.get(1), "list-tail");
        if (index > elements.size()) {
            throw new EvalError("list-tail index out of range");
        }
        return new ListValue(List.copyOf(elements.subList(index, elements.size())));
    }

    private Value applyAssoc(List<Value> arguments) throws EvalError {
        requireExactArity("assoc", arguments.size(), 2);
        Value key = arguments.getFirst();
        List<Value> entries = expectList(arguments.get(1), "assoc");
        for (Value entry : entries) {
            if (equalValue(key, car(entry, "assoc"))) {
                return entry;
            }
        }
        return new BoolValue(false);
    }

    private Value applyDisplay(List<Value> arguments) throws EvalError {
        requireExactArity("display", arguments.size(), 1);
        appendOutput(renderForDisplay(arguments.getFirst()));
        return VoidValue.INSTANCE;
    }

    private Value applyWrite(List<Value> arguments) throws EvalError {
        requireExactArity("write", arguments.size(), 1);
        appendOutput(arguments.getFirst().render());
        return VoidValue.INSTANCE;
    }

    private Value applyNewline(List<Value> arguments) throws EvalError {
        requireExactArity("newline", arguments.size(), 0);
        appendOutput("\n");
        return VoidValue.INSTANCE;
    }

    private Value applyStringAppend(List<Value> arguments) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(expectString(argument, "string-append"));
        }
        return new StringValue(builder.toString());
    }

    private Value applyStringLength(List<Value> arguments) throws EvalError {
        requireExactArity("string-length", arguments.size(), 1);
        return new IntValue(expectStringValue(arguments.getFirst(), "string-length").length());
    }

    private Value applySubstring(List<Value> arguments) throws EvalError {
        requireExactArity("substring", arguments.size(), 3);
        String value = expectString(arguments.getFirst(), "substring");
        int start = expectIndex(arguments.get(1), "substring");
        int end = expectIndex(arguments.get(2), "substring");
        if (start > end || end > value.length()) {
            throw new EvalError("substring indices out of range");
        }
        return new StringValue(value.substring(start, end));
    }

    private Value applyStringToNumber(List<Value> arguments) throws EvalError {
        requireExactArity("string->number", arguments.size(), 1);
        String value = expectString(arguments.getFirst(), "string->number");
        Value parsed = Numbers.tryParseLiteral(value);
        return parsed == null ? new BoolValue(false) : parsed;
    }

    private Value applyNumberToString(List<Value> arguments) throws EvalError {
        requireExactArity("number->string", arguments.size(), 1);
        return new StringValue(Numbers.expectNumber(arguments.getFirst(), "number->string").render());
    }

    private Value applyExactToInexact(List<Value> arguments) throws EvalError {
        requireExactArity("exact->inexact", arguments.size(), 1);
        return Numbers.exactToInexact(arguments.getFirst(), "exact->inexact");
    }

    private Value applyInexactToExact(List<Value> arguments) throws EvalError {
        requireExactArity("inexact->exact", arguments.size(), 1);
        return Numbers.inexactToExact(arguments.getFirst(), "inexact->exact");
    }

    private Value applyNumerator(List<Value> arguments) throws EvalError {
        requireExactArity("numerator", arguments.size(), 1);
        return Numbers.numerator(arguments.getFirst(), "numerator");
    }

    private Value applyDenominator(List<Value> arguments) throws EvalError {
        requireExactArity("denominator", arguments.size(), 1);
        return Numbers.denominator(arguments.getFirst(), "denominator");
    }

    private Value applySymbolToString(List<Value> arguments) throws EvalError {
        requireExactArity("symbol->string", arguments.size(), 1);
        if (arguments.getFirst() instanceof SymbolValue symbolValue) {
            return new StringValue(symbolValue.name(), false);
        }
        throw new EvalError("symbol->string expects symbol arguments");
    }

    private Value applyStringToSymbol(List<Value> arguments) throws EvalError {
        requireExactArity("string->symbol", arguments.size(), 1);
        return new SymbolValue(expectString(arguments.getFirst(), "string->symbol"));
    }

    private Value applyStringRef(List<Value> arguments) throws EvalError {
        requireExactArity("string-ref", arguments.size(), 2);
        StringValue value = expectStringValue(arguments.getFirst(), "string-ref");
        int index = expectIndex(arguments.get(1), "string-ref");
        if (index >= value.length()) {
            throw new EvalError("string-ref index out of range");
        }
        return new CharValue(value.charAt(index));
    }

    private Value applyStringToList(List<Value> arguments) throws EvalError {
        requireExactArity("string->list", arguments.size(), 1);
        String value = expectString(arguments.getFirst(), "string->list");
        List<Value> characters = new ArrayList<>(value.length());
        for (int i = 0; i < value.length(); i++) {
            characters.add(new CharValue(value.charAt(i)));
        }
        return new ListValue(List.copyOf(characters));
    }

    private Value applyListToString(List<Value> arguments) throws EvalError {
        requireExactArity("list->string", arguments.size(), 1);
        List<Value> characters = expectList(arguments.getFirst(), "list->string");
        StringBuilder builder = new StringBuilder(characters.size());
        for (Value value : characters) {
            builder.append(expectChar(value, "list->string"));
        }
        return new StringValue(builder.toString());
    }

    private Value applyStringCopy(List<Value> arguments) throws EvalError {
        requireExactArity("string-copy", arguments.size(), 1);
        return new StringValue(expectString(arguments.getFirst(), "string-copy"));
    }

    private Value applyStringSet(List<Value> arguments) throws EvalError {
        requireExactArity("string-set!", arguments.size(), 3);
        StringValue value = expectStringValue(arguments.getFirst(), "string-set!");
        if (!value.isMutable()) {
            throw new EvalError("string-set! cannot mutate immutable strings");
        }
        int index = expectIndex(arguments.get(1), "string-set!");
        char ch = expectChar(arguments.get(2), "string-set!");
        if (index >= value.length()) {
            throw new EvalError("string-set! index out of range");
        }
        value.setCharAt(index, ch);
        return VoidValue.INSTANCE;
    }

    private Value applyStringEquality(List<Value> arguments) throws EvalError {
        requireMinimumArity("string=?", arguments.size(), 2);
        String previous = expectString(arguments.getFirst(), "string=?");
        for (int i = 1; i < arguments.size(); i++) {
            String current = expectString(arguments.get(i), "string=?");
            if (!previous.equals(current)) {
                return new BoolValue(false);
            }
            previous = current;
        }
        return new BoolValue(true);
    }

    private Value applyStringLessThan(List<Value> arguments) throws EvalError {
        requireMinimumArity("string<?", arguments.size(), 2);
        String previous = expectString(arguments.getFirst(), "string<?");
        for (int i = 1; i < arguments.size(); i++) {
            String current = expectString(arguments.get(i), "string<?");
            if (previous.compareTo(current) >= 0) {
                return new BoolValue(false);
            }
            previous = current;
        }
        return new BoolValue(true);
    }

    private Value applyStringCiEquality(List<Value> arguments) throws EvalError {
        requireMinimumArity("string-ci=?", arguments.size(), 2);
        String previous = expectString(arguments.getFirst(), "string-ci=?");
        for (int i = 1; i < arguments.size(); i++) {
            String current = expectString(arguments.get(i), "string-ci=?");
            if (!previous.equalsIgnoreCase(current)) {
                return new BoolValue(false);
            }
            previous = current;
        }
        return new BoolValue(true);
    }

    private Value applyStringUpcase(List<Value> arguments) throws EvalError {
        requireExactArity("string-upcase", arguments.size(), 1);
        return new StringValue(expectString(arguments.getFirst(), "string-upcase").toUpperCase(Locale.ROOT));
    }

    private Value applyStringDowncase(List<Value> arguments) throws EvalError {
        requireExactArity("string-downcase", arguments.size(), 1);
        return new StringValue(expectString(arguments.getFirst(), "string-downcase").toLowerCase(Locale.ROOT));
    }

    private Value applyCharPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("char?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof CharValue);
    }

    private Value applyCharAlphabeticPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("char-alphabetic?", arguments.size(), 1);
        return new BoolValue(Character.isLetter(expectChar(arguments.getFirst(), "char-alphabetic?")));
    }

    private Value applyCharNumericPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("char-numeric?", arguments.size(), 1);
        return new BoolValue(Character.isDigit(expectChar(arguments.getFirst(), "char-numeric?")));
    }

    private Value applyCharUpcase(List<Value> arguments) throws EvalError {
        requireExactArity("char-upcase", arguments.size(), 1);
        return new CharValue(Character.toUpperCase(expectChar(arguments.getFirst(), "char-upcase")));
    }

    private Value applyCharDowncase(List<Value> arguments) throws EvalError {
        requireExactArity("char-downcase", arguments.size(), 1);
        return new CharValue(Character.toLowerCase(expectChar(arguments.getFirst(), "char-downcase")));
    }

    private Value applyCharEquality(List<Value> arguments) throws EvalError {
        requireMinimumArity("char=?", arguments.size(), 2);
        char previous = expectChar(arguments.getFirst(), "char=?");
        for (int i = 1; i < arguments.size(); i++) {
            char current = expectChar(arguments.get(i), "char=?");
            if (previous != current) {
                return new BoolValue(false);
            }
            previous = current;
        }
        return new BoolValue(true);
    }

    private Value applyCharLessThan(List<Value> arguments) throws EvalError {
        requireMinimumArity("char<?", arguments.size(), 2);
        char previous = expectChar(arguments.getFirst(), "char<?");
        for (int i = 1; i < arguments.size(); i++) {
            char current = expectChar(arguments.get(i), "char<?");
            if (previous >= current) {
                return new BoolValue(false);
            }
            previous = current;
        }
        return new BoolValue(true);
    }

    private Value applyCharToInteger(List<Value> arguments) throws EvalError {
        requireExactArity("char->integer", arguments.size(), 1);
        return new IntValue(expectChar(arguments.getFirst(), "char->integer"));
    }

    private Value applyIntegerToChar(List<Value> arguments) throws EvalError {
        requireExactArity("integer->char", arguments.size(), 1);
        long codePoint = expectInt(arguments.getFirst(), "integer->char");
        if (codePoint < Character.MIN_VALUE || codePoint > Character.MAX_VALUE) {
            throw new EvalError("integer->char expects a valid character code");
        }
        return new CharValue((char) codePoint);
    }

    private Value applyApply(List<Value> arguments) throws EvalError {
        requireMinimumArity("apply", arguments.size(), 2);

        Value operator = arguments.getFirst();
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        List<Value> spreadArguments = expectList(arguments.getLast(), "apply");
        List<Value> flattenedArguments = new ArrayList<>(arguments.size() - 2 + spreadArguments.size());
        for (int i = 1; i < arguments.size() - 1; i++) {
            flattenedArguments.add(arguments.get(i));
        }
        flattenedArguments.addAll(spreadArguments);
        return procedure.apply(List.copyOf(flattenedArguments), this);
    }

    private Value applyMap(List<Value> arguments) throws EvalError {
        requireMinimumArity("map", arguments.size(), 2);
        if (!(arguments.getFirst() instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        List<List<Value>> lists = new ArrayList<>(arguments.size() - 1);
        int resultLength = Integer.MAX_VALUE;
        for (int i = 1; i < arguments.size(); i++) {
            List<Value> list = expectList(arguments.get(i), "map");
            lists.add(list);
            resultLength = Math.min(resultLength, list.size());
        }

        List<Value> results = new ArrayList<>(resultLength);
        for (int elementIndex = 0; elementIndex < resultLength; elementIndex++) {
            List<Value> invocationArguments = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                invocationArguments.add(list.get(elementIndex));
            }
            results.add(procedure.apply(List.copyOf(invocationArguments), this));
        }

        return new ListValue(List.copyOf(results));
    }

    private Value applyVector(List<Value> arguments) {
        return new VectorValue(arguments);
    }

    private Value applyMakeVector(List<Value> arguments) throws EvalError {
        if (arguments.size() < 1 || arguments.size() > 2) {
            throw new EvalError("make-vector expected 1 or 2 argument(s)");
        }

        int size = expectIndex(arguments.getFirst(), "make-vector");
        Value fill = arguments.size() == 2 ? arguments.get(1) : VoidValue.INSTANCE;
        List<Value> elements = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    private Value applyVectorRef(List<Value> arguments) throws EvalError {
        requireExactArity("vector-ref", arguments.size(), 2);
        VectorValue vector = expectVector(arguments.getFirst(), "vector-ref");
        int index = expectIndex(arguments.get(1), "vector-ref");
        if (index >= vector.length()) {
            throw new EvalError("vector-ref index out of range");
        }
        return vector.element(index);
    }

    private Value applyVectorSet(List<Value> arguments) throws EvalError {
        requireExactArity("vector-set!", arguments.size(), 3);
        VectorValue vector = expectVector(arguments.getFirst(), "vector-set!");
        int index = expectIndex(arguments.get(1), "vector-set!");
        if (index >= vector.length()) {
            throw new EvalError("vector-set! index out of range");
        }
        vector.setElement(index, arguments.get(2));
        return VoidValue.INSTANCE;
    }

    private Value applyVectorLength(List<Value> arguments) throws EvalError {
        requireExactArity("vector-length", arguments.size(), 1);
        return new IntValue(expectVector(arguments.getFirst(), "vector-length").length());
    }

    private Value applyVectorPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("vector?", arguments.size(), 1);
        return new BoolValue(arguments.getFirst() instanceof VectorValue);
    }

    private Value applyVectorToList(List<Value> arguments) throws EvalError {
        requireExactArity("vector->list", arguments.size(), 1);
        return new ListValue(expectVector(arguments.getFirst(), "vector->list").elements());
    }

    private Value applyListToVector(List<Value> arguments) throws EvalError {
        requireExactArity("list->vector", arguments.size(), 1);
        return new VectorValue(expectList(arguments.getFirst(), "list->vector"));
    }

    private Value applyAdd(List<Value> arguments) throws EvalError {
        return Numbers.add(arguments, "+");
    }

    private Value applySubtract(List<Value> arguments) throws EvalError {
        requireMinimumArity("-", arguments.size(), 1);
        return Numbers.subtract(arguments, "-");
    }

    private Value applyMultiply(List<Value> arguments) throws EvalError {
        return Numbers.multiply(arguments, "*");
    }

    private Value applyDivide(List<Value> arguments) throws EvalError {
        requireMinimumArity("/", arguments.size(), 2);
        return Numbers.divide(arguments, "/");
    }

    private Value applyAbs(List<Value> arguments) throws EvalError {
        requireExactArity("abs", arguments.size(), 1);
        return Numbers.abs(arguments.getFirst(), "abs");
    }

    private Value applyModulo(List<Value> arguments) throws EvalError {
        requireExactArity("modulo", arguments.size(), 2);
        long dividend = expectInt(arguments.getFirst(), "modulo");
        long divisor = expectInt(arguments.get(1), "modulo");
        requireNonZeroDivisor("modulo", divisor);
        return new IntValue(Math.floorMod(dividend, divisor));
    }

    private Value applyRemainder(List<Value> arguments) throws EvalError {
        requireExactArity("remainder", arguments.size(), 2);
        long dividend = expectInt(arguments.getFirst(), "remainder");
        long divisor = expectInt(arguments.get(1), "remainder");
        requireNonZeroDivisor("remainder", divisor);
        return new IntValue(dividend % divisor);
    }

    private Value applyQuotient(List<Value> arguments) throws EvalError {
        requireExactArity("quotient", arguments.size(), 2);
        long dividend = expectInt(arguments.getFirst(), "quotient");
        long divisor = expectInt(arguments.get(1), "quotient");
        requireNonZeroDivisor("quotient", divisor);
        return new IntValue(dividend / divisor);
    }

    private Value applyMin(List<Value> arguments) throws EvalError {
        requireMinimumArity("min", arguments.size(), 1);
        return Numbers.min(arguments, "min");
    }

    private Value applyMax(List<Value> arguments) throws EvalError {
        requireMinimumArity("max", arguments.size(), 1);
        return Numbers.max(arguments, "max");
    }

    private Value applyExpt(List<Value> arguments) throws EvalError {
        requireExactArity("expt", arguments.size(), 2);
        long base = expectInt(arguments.getFirst(), "expt");
        long exponent = expectInt(arguments.get(1), "expt");
        if (exponent < 0L) {
            throw new EvalError("expt expects a non-negative exponent");
        }

        long result = 1L;
        long factor = base;
        long power = exponent;
        while (power > 0L) {
            if ((power & 1L) == 1L) {
                result *= factor;
            }
            factor *= factor;
            power >>= 1;
        }
        return new IntValue(result);
    }

    private Value applyZeroPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("zero?", arguments.size(), 1);
        return new BoolValue(Numbers.isZero(arguments.getFirst(), "zero?"));
    }

    private Value applyPositivePredicate(List<Value> arguments) throws EvalError {
        requireExactArity("positive?", arguments.size(), 1);
        return new BoolValue(Numbers.isPositive(arguments.getFirst(), "positive?"));
    }

    private Value applyNegativePredicate(List<Value> arguments) throws EvalError {
        requireExactArity("negative?", arguments.size(), 1);
        return new BoolValue(Numbers.isNegative(arguments.getFirst(), "negative?"));
    }

    private Value applyOddPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("odd?", arguments.size(), 1);
        return new BoolValue((expectInt(arguments.getFirst(), "odd?") & 1L) != 0L);
    }

    private Value applyEvenPredicate(List<Value> arguments) throws EvalError {
        requireExactArity("even?", arguments.size(), 1);
        return new BoolValue((expectInt(arguments.getFirst(), "even?") & 1L) == 0L);
    }

    private Value applyComparison(String operator, List<Value> arguments) throws EvalError {
        requireMinimumArity(operator, arguments.size(), 2);
        return new BoolValue(Numbers.compareChain(operator, arguments));
    }

    private long expectInt(Value value, String operator) throws EvalError {
        if (value instanceof IntValue intValue) {
            return intValue.value();
        }
        throw new EvalError(operator + " expects integer arguments");
    }

    private int expectIndex(Value value, String operator) throws EvalError {
        long index = expectInt(value, operator);
        if (index < 0L || index > Integer.MAX_VALUE) {
            throw new EvalError(operator + " index out of range");
        }
        return (int) index;
    }

    private String expectString(Value value, String operator) throws EvalError {
        return expectStringValue(value, operator).value();
    }

    private StringValue expectStringValue(Value value, String operator) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError(operator + " expects string arguments");
    }

    private char expectChar(Value value, String operator) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError(operator + " expects character arguments");
    }

    private List<Value> expectList(Value value, String operator) throws EvalError {
        if (value instanceof ListValue listValue) {
            return listValue.elements();
        }
        throw new EvalError(operator + " expects list arguments");
    }

    private VectorValue expectVector(Value value, String operator) throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw new EvalError(operator + " expects vector arguments");
    }

    private Value car(Value value, String operator) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue.car();
        }

        List<Value> elements = expectNonEmptyList(value, operator);
        return elements.getFirst();
    }

    private Value cdr(Value value, String operator) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue.cdr();
        }

        List<Value> elements = expectNonEmptyList(value, operator);
        return new ListValue(List.copyOf(elements.subList(1, elements.size())));
    }

    private boolean isProperList(Value value) {
        return value instanceof ListValue;
    }

    private boolean eqValue(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (Numbers.equals(left, right)) {
            return true;
        }
        if (left instanceof BoolValue leftBool && right instanceof BoolValue rightBool) {
            return leftBool.value() == rightBool.value();
        }
        if (left instanceof CharValue leftChar && right instanceof CharValue rightChar) {
            return leftChar.value() == rightChar.value();
        }
        if (left instanceof SymbolValue leftSymbol && right instanceof SymbolValue rightSymbol) {
            return leftSymbol.name().equals(rightSymbol.name());
        }
        return false;
    }

    private boolean eqvValue(Value left, Value right) {
        if (eqValue(left, right)) {
            return true;
        }
        return left instanceof ListValue leftList
                && right instanceof ListValue rightList
                && leftList.elements().isEmpty()
                && rightList.elements().isEmpty();
    }

    private boolean equalValue(Value left, Value right) {
        if (eqvValue(left, right)) {
            return true;
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof ListValue leftList && right instanceof ListValue rightList) {
            if (leftList.elements().size() != rightList.elements().size()) {
                return false;
            }
            for (int i = 0; i < leftList.elements().size(); i++) {
                if (!equalValue(leftList.elements().get(i), rightList.elements().get(i))) {
                    return false;
                }
            }
            return true;
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            return equalValue(leftPair.car(), rightPair.car())
                    && equalValue(leftPair.cdr(), rightPair.cdr());
        }
        if (left instanceof VectorValue leftVector && right instanceof VectorValue rightVector) {
            if (leftVector.length() != rightVector.length()) {
                return false;
            }
            for (int i = 0; i < leftVector.length(); i++) {
                if (!equalValue(leftVector.element(i), rightVector.element(i))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private RecordValue expectRecord(Value value,
                                     RecordTypeDescriptor recordType,
                                     String operator) throws EvalError {
        if (value instanceof RecordValue recordValue && recordValue.type() == recordType) {
            return recordValue;
        }
        throw new EvalError(operator + " expects a " + recordType.name() + " record");
    }

    private void requireNonZeroDivisor(String operator, long divisor) throws EvalError {
        if (divisor == 0L) {
            throw new EvalError(operator + " division by zero");
        }
    }

    private void appendOutput(String output) {
        if (outputBuffer != null) {
            outputBuffer.append(output);
        }
    }

    private String renderForDisplay(Value value) {
        return switch (value) {
            case StringValue stringValue -> stringValue.value();
            case CharValue charValue -> Character.toString(charValue.value());
            case ListValue listValue -> renderListForDisplay(listValue.elements());
            case PairValue pairValue -> renderPairForDisplay(pairValue);
            default -> value.render();
        };
    }

    private String renderListForDisplay(List<Value> elements) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        for (int i = 0; i < elements.size(); i++) {
            if (i > 0) {
                builder.append(' ');
            }
            builder.append(renderForDisplay(elements.get(i)));
        }
        builder.append(')');
        return builder.toString();
    }

    private String renderPairForDisplay(PairValue pairValue) {
        StringBuilder builder = new StringBuilder();
        builder.append('(');
        appendPairForDisplay(builder, pairValue);
        builder.append(')');
        return builder.toString();
    }

    private void appendPairForDisplay(StringBuilder builder, Value value) {
        if (value instanceof PairValue pairValue) {
            builder.append(renderForDisplay(pairValue.car()));
            if (pairValue.cdr() instanceof PairValue nextPair) {
                builder.append(' ');
                appendPairForDisplay(builder, nextPair);
                return;
            }
            if (pairValue.cdr() instanceof ListValue listValue) {
                for (Value element : listValue.elements()) {
                    builder.append(' ');
                    builder.append(renderForDisplay(element));
                }
                return;
            }
            builder.append(" . ");
            builder.append(renderForDisplay(pairValue.cdr()));
            return;
        }

        builder.append(renderForDisplay(value));
    }

    private List<Value> expectNonEmptyList(Value value, String operator) throws EvalError {
        List<Value> elements = expectList(value, operator);
        if (elements.isEmpty()) {
            throw new EvalError(operator + " expected a non-empty list");
        }
        return elements;
    }

    private void requireExactArity(String name, int actual, int expected) throws EvalError {
        if (actual != expected) {
            throw new EvalError(name + " expected " + expected + " argument(s)");
        }
    }

    private void requireMinimumArity(String name, int actual, int minimum) throws EvalError {
        if (actual < minimum) {
            throw new EvalError(name + " expected at least " + minimum + " argument(s)");
        }
    }
}
