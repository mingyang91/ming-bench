package ming;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Scheme interpreter entry point.
 */
public class Evaluator {
    /**
     * Evaluate one or more Scheme expressions and return the string
     * representation of the last result.
     */
    public String evalStr(String input) throws EvalError {
        return ValueFormatter.format(evalProgram(input).value());
    }

    /**
     * Evaluate Scheme expressions and return both the result string
     * and any captured output from display/write/newline.
     */
    public EvalResult evalStrWithOutput(String input) throws EvalError {
        ProgramResult result = evalProgram(input);
        return new EvalResult(ValueFormatter.format(result.value()), result.output());
    }

    private ProgramResult evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> expressions = parser.parseProgram();
        if (expressions.isEmpty()) {
            throw new EvalError("empty input", 1, 1);
        }

        StringBuilder output = new StringBuilder();
        Environment environment = createGlobalEnvironment(output);
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, environment);
        }
        return new ProgramResult(result, output.toString());
    }

    private Environment createGlobalEnvironment(StringBuilder output) {
        Environment environment = new Environment(null);
        environment.define("+", new BuiltinProcedure("+", this::applyAdd));
        environment.define("-", new BuiltinProcedure("-", this::applySubtract));
        environment.define("*", new BuiltinProcedure("*", this::applyMultiply));
        environment.define("/", new BuiltinProcedure("/", this::applyDivide));
        environment.define("abs", new BuiltinProcedure("abs", this::applyAbs));
        environment.define("modulo", new BuiltinProcedure("modulo", this::applyModulo));
        environment.define("remainder", new BuiltinProcedure("remainder", this::applyRemainder));
        environment.define("quotient", new BuiltinProcedure("quotient", this::applyQuotient));
        environment.define("min", new BuiltinProcedure("min", this::applyMin));
        environment.define("max", new BuiltinProcedure("max", this::applyMax));
        environment.define("expt", new BuiltinProcedure("expt", this::applyExpt));
        environment.define("cons", new BuiltinProcedure("cons", this::applyCons));
        environment.define("car", new BuiltinProcedure("car", this::applyCar));
        environment.define("cdr", new BuiltinProcedure("cdr", this::applyCdr));
        environment.define("null?", new BuiltinProcedure("null?", (args, pos) ->
                BoolValue.of(isNull(args, pos))));
        environment.define("list", new BuiltinProcedure("list", (args, pos) -> buildList(args)));
        environment.define("length", new BuiltinProcedure("length", (args, pos) ->
                new IntValue(BigInteger.valueOf(length(args, pos)))));
        environment.define("append", new BuiltinProcedure("append", this::applyAppend));
        environment.define("list-ref", new BuiltinProcedure("list-ref", this::applyListRef));
        environment.define("list-tail", new BuiltinProcedure("list-tail", this::applyListTail));
        environment.define("list?", new BuiltinProcedure("list?", (args, pos) ->
                BoolValue.of(isList(args, pos))));
        environment.define("assoc", new BuiltinProcedure("assoc", this::applyAssoc));
        environment.define("map", new BuiltinProcedure("map", this::applyMap));
        environment.define("<", new BuiltinProcedure("<", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.LESS_THAN))));
        environment.define(">", new BuiltinProcedure(">", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.GREATER_THAN))));
        environment.define("=", new BuiltinProcedure("=", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.EQUAL))));
        environment.define("<=", new BuiltinProcedure("<=", (args, pos) ->
                BoolValue.of(compare(args, pos, Comparison.LESS_EQUAL))));
        environment.define("not", new BuiltinProcedure("not", (args, pos) ->
                BoolValue.of(not(args, pos))));
        environment.define("zero?", new BuiltinProcedure("zero?", (args, pos) ->
                BoolValue.of(isZero(args, pos))));
        environment.define("positive?", new BuiltinProcedure("positive?", (args, pos) ->
                BoolValue.of(isPositive(args, pos))));
        environment.define("negative?", new BuiltinProcedure("negative?", (args, pos) ->
                BoolValue.of(isNegative(args, pos))));
        environment.define("odd?", new BuiltinProcedure("odd?", (args, pos) ->
                BoolValue.of(isOdd(args, pos))));
        environment.define("even?", new BuiltinProcedure("even?", (args, pos) ->
                BoolValue.of(isEven(args, pos))));
        environment.define("eq?", new BuiltinProcedure("eq?", (args, pos) ->
                BoolValue.of(isEq(args, pos))));
        environment.define("equal?", new BuiltinProcedure("equal?", (args, pos) ->
                BoolValue.of(isEqual(args, pos))));
        environment.define("string?", new BuiltinProcedure("string?", (args, pos) ->
                BoolValue.of(isType(args, pos, "string?", StringValue.class))));
        environment.define("number?", new BuiltinProcedure("number?", (args, pos) ->
                BoolValue.of(isType(args, pos, "number?", NumericValue.class))));
        environment.define("exact?", new BuiltinProcedure("exact?", (args, pos) ->
                BoolValue.of(isExact(args, pos))));
        environment.define("inexact?", new BuiltinProcedure("inexact?", (args, pos) ->
                BoolValue.of(isInexact(args, pos))));
        environment.define("integer?", new BuiltinProcedure("integer?", (args, pos) ->
                BoolValue.of(isInteger(args, pos))));
        environment.define("rational?", new BuiltinProcedure("rational?", (args, pos) ->
                BoolValue.of(isRational(args, pos))));
        environment.define("boolean?", new BuiltinProcedure("boolean?", (args, pos) ->
                BoolValue.of(isType(args, pos, "boolean?", BoolValue.class))));
        environment.define("pair?", new BuiltinProcedure("pair?", (args, pos) ->
                BoolValue.of(isType(args, pos, "pair?", PairValue.class))));
        environment.define("symbol?", new BuiltinProcedure("symbol?", (args, pos) ->
                BoolValue.of(isType(args, pos, "symbol?", SymbolValue.class))));
        environment.define("display", new BuiltinProcedure("display", (args, pos) ->
                applyDisplay(args, pos, output)));
        environment.define("write", new BuiltinProcedure("write", (args, pos) ->
                applyWrite(args, pos, output)));
        environment.define("newline", new BuiltinProcedure("newline", (args, pos) ->
                applyNewline(args, pos, output)));
        environment.define("string-append", new BuiltinProcedure("string-append",
                this::applyStringAppend));
        environment.define("string-length", new BuiltinProcedure("string-length",
                this::applyStringLength));
        environment.define("substring", new BuiltinProcedure("substring", this::applySubstring));
        environment.define("string->number", new BuiltinProcedure("string->number",
                this::applyStringToNumber));
        environment.define("number->string", new BuiltinProcedure("number->string",
                this::applyNumberToString));
        environment.define("apply", new BuiltinProcedure("apply", this::applyApply));
        environment.define("symbol->string", new BuiltinProcedure("symbol->string",
                this::applySymbolToString));
        environment.define("string->symbol", new BuiltinProcedure("string->symbol",
                this::applyStringToSymbol));
        environment.define("string-ref", new BuiltinProcedure("string-ref", this::applyStringRef));
        environment.define("string-set!", new BuiltinProcedure("string-set!", this::applyStringSet));
        environment.define("string-copy", new BuiltinProcedure("string-copy", this::applyStringCopy));
        environment.define("string=?", new BuiltinProcedure("string=?", this::applyStringEquals));
        environment.define("string<?", new BuiltinProcedure("string<?", this::applyStringLess));
        environment.define("string-ci=?", new BuiltinProcedure("string-ci=?",
                this::applyStringCiEquals));
        environment.define("string-upcase", new BuiltinProcedure("string-upcase",
                this::applyStringUpcase));
        environment.define("string-downcase", new BuiltinProcedure("string-downcase",
                this::applyStringDowncase));
        environment.define("char?", new BuiltinProcedure("char?", (args, pos) ->
                BoolValue.of(isType(args, pos, "char?", CharValue.class))));
        environment.define("exact->inexact", new BuiltinProcedure("exact->inexact",
                this::applyExactToInexact));
        environment.define("inexact->exact", new BuiltinProcedure("inexact->exact",
                this::applyInexactToExact));
        environment.define("numerator", new BuiltinProcedure("numerator", this::applyNumerator));
        environment.define("denominator", new BuiltinProcedure("denominator",
                this::applyDenominator));
        environment.define("char-alphabetic?", new BuiltinProcedure("char-alphabetic?",
                (args, pos) -> BoolValue.of(isCharAlphabetic(args, pos))));
        environment.define("char-numeric?", new BuiltinProcedure("char-numeric?",
                (args, pos) -> BoolValue.of(isCharNumeric(args, pos))));
        environment.define("char-upcase", new BuiltinProcedure("char-upcase",
                this::applyCharUpcase));
        environment.define("char-downcase", new BuiltinProcedure("char-downcase",
                this::applyCharDowncase));
        environment.define("char=?", new BuiltinProcedure("char=?", (args, pos) ->
                BoolValue.of(compareChars(args, pos, "char=?", true))));
        environment.define("char<?", new BuiltinProcedure("char<?", (args, pos) ->
                BoolValue.of(compareChars(args, pos, "char<?", false))));
        return environment;
    }

    private Value eval(Expr expression, Environment environment) throws EvalError {
        if (expression instanceof NumberExpr numberExpr) {
            return numberExpr.value().toValue();
        }
        if (expression instanceof BoolExpr boolExpr) {
            return BoolValue.of(boolExpr.value());
        }
        if (expression instanceof StringExpr stringExpr) {
            return new StringValue(stringExpr.value());
        }
        if (expression instanceof CharExpr charExpr) {
            return new CharValue(charExpr.value());
        }
        if (expression instanceof SymbolExpr symbolExpr) {
            Environment bindingEnvironment = symbolExpr.lexicalEnvironment() != null
                    ? symbolExpr.lexicalEnvironment()
                    : environment;
            return bindingEnvironment.lookup(symbolExpr.name(), symbolExpr.pos());
        }
        if (expression instanceof ListExpr listExpr) {
            return evalList(listExpr, environment);
        }
        throw new EvalError("unsupported expression", expression.pos().line(),
                expression.pos().column());
    }

    private Value evalList(ListExpr expression, Environment environment) throws EvalError {
        List<Expr> elements = expression.elements();
        if (elements.isEmpty()) {
            throw error("cannot evaluate empty list", expression.pos());
        }

        Expr head = elements.getFirst();
        if (head instanceof SymbolExpr symbolExpr) {
            List<Expr> arguments = elements.subList(1, elements.size());
            return switch (symbolExpr.name()) {
                case "define" -> evalDefine(arguments, environment, symbolExpr.pos());
                case "define-syntax" -> evalDefineSyntax(arguments, environment, symbolExpr.pos());
                case "define-record-type" ->
                        evalDefineRecordType(arguments, environment, symbolExpr.pos());
                case "set!" -> evalSet(arguments, environment, symbolExpr.pos());
                case "if" -> evalIf(arguments, environment, symbolExpr.pos());
                case "quote" -> evalQuote(arguments, symbolExpr.pos());
                case "lambda" -> evalLambda(arguments, environment, symbolExpr.pos());
                case "begin" -> evalBegin(arguments, environment);
                case "cond" -> evalCond(arguments, environment, symbolExpr.pos());
                case "let" -> evalLet(arguments, environment, symbolExpr.pos());
                case "and" -> evalAnd(arguments, environment);
                case "or" -> evalOr(arguments, environment);
                default -> {
                    SyntaxMacro macro = lookupMacro(symbolExpr, environment);
                    if (macro != null) {
                        yield eval(macro.expand(expression), environment);
                    }
                    yield apply(eval(head, environment),
                            evalArguments(arguments, environment), expression.pos());
                }
            };
        }

        return apply(eval(head, environment),
                evalArguments(elements.subList(1, elements.size()), environment),
                expression.pos());
    }

    private Value evalDefine(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.isEmpty()) {
            throw error("'define' expects a target and a value", pos);
        }

        Expr target = arguments.getFirst();
        if (target instanceof SymbolExpr symbolExpr) {
            if (arguments.size() != 2) {
                throw error("'define' expects exactly 2 arguments", pos);
            }
            Value value = eval(arguments.get(1), environment);
            environment.define(symbolExpr.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signatureExpr) {
            List<Expr> signature = signatureExpr.elements();
            if (signature.isEmpty()) {
                throw error("function definition requires a name", target.pos());
            }
            if (!(signature.getFirst() instanceof SymbolExpr nameExpr)) {
                throw error("function definition requires a symbol name", signatureExpr.pos());
            }
            if (arguments.size() < 2) {
                throw error("function definition requires a body", pos);
            }

            ParameterSpec parameters = parseParameterSpec(
                    signature.subList(1, signature.size()), signatureExpr.pos());
            List<Expr> body = new ArrayList<>(arguments.subList(1, arguments.size()));
            ClosureProcedure procedure = new ClosureProcedure(
                    nameExpr.name(), parameters.required(), parameters.rest(), body, environment);
            environment.define(nameExpr.name(), procedure);
            return VoidValue.INSTANCE;
        }

        throw error("'define' target must be a symbol or parameter list", target.pos());
    }

    private Value evalDefineSyntax(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() != 2) {
            throw error("'define-syntax' expects exactly 2 arguments", pos);
        }
        if (!(arguments.getFirst() instanceof SymbolExpr symbolExpr)) {
            throw error("'define-syntax' target must be an identifier", arguments.getFirst().pos());
        }

        environment.defineMacro(symbolExpr.name(),
                SyntaxRulesMacro.fromDefinition(symbolExpr.name(), arguments.get(1), environment));
        return VoidValue.INSTANCE;
    }

    private Value evalDefineRecordType(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() < 3) {
            throw error("'define-record-type' expects a name, constructor, and predicate", pos);
        }

        String typeName = requireSymbolName(arguments.get(0), "record type name");
        if (!(arguments.get(1) instanceof ListExpr constructorExpr)) {
            throw error("'define-record-type' constructor spec must be a list",
                    arguments.get(1).pos());
        }

        List<Expr> constructorElements = constructorExpr.elements();
        if (constructorElements.isEmpty()) {
            throw error("'define-record-type' constructor spec must not be empty",
                    constructorExpr.pos());
        }

        String constructorName = requireSymbolName(constructorElements.getFirst(),
                "record constructor name");
        String predicateName = requireSymbolName(arguments.get(2), "record predicate name");

        List<RecordFieldSpec> fieldSpecs = new ArrayList<>(arguments.size() - 3);
        Map<String, Integer> fieldIndexes = new HashMap<>();
        for (int i = 3; i < arguments.size(); i++) {
            RecordFieldSpec fieldSpec = parseRecordFieldSpec(arguments.get(i));
            if (fieldIndexes.putIfAbsent(fieldSpec.name(), fieldSpecs.size()) != null) {
                throw error("duplicate record field: " + fieldSpec.name(), fieldSpec.pos());
            }
            fieldSpecs.add(fieldSpec);
        }

        List<Integer> constructorFieldIndexes = new ArrayList<>(constructorElements.size() - 1);
        HashSet<String> constructorFields = new HashSet<>();
        for (int i = 1; i < constructorElements.size(); i++) {
            Expr fieldExpr = constructorElements.get(i);
            String fieldName = requireSymbolName(fieldExpr, "record constructor field");
            Integer fieldIndex = fieldIndexes.get(fieldName);
            if (fieldIndex == null) {
                throw error("unknown record field: " + fieldName, fieldExpr.pos());
            }
            if (!constructorFields.add(fieldName)) {
                throw error("duplicate constructor field: " + fieldName, fieldExpr.pos());
            }
            constructorFieldIndexes.add(fieldIndex);
        }

        List<String> fieldNames = new ArrayList<>(fieldSpecs.size());
        for (RecordFieldSpec fieldSpec : fieldSpecs) {
            fieldNames.add(fieldSpec.name());
        }
        RecordType recordType = new RecordType(typeName, List.copyOf(fieldNames));

        environment.define(constructorName, new BuiltinProcedure(constructorName,
                (callArguments, callPos) -> applyRecordConstructor(recordType,
                        constructorFieldIndexes, constructorName, callArguments, callPos)));
        environment.define(predicateName, new BuiltinProcedure(predicateName,
                (callArguments, callPos) -> applyRecordPredicate(recordType,
                        predicateName, callArguments, callPos)));
        for (int i = 0; i < fieldSpecs.size(); i++) {
            RecordFieldSpec fieldSpec = fieldSpecs.get(i);
            int fieldIndex = i;
            environment.define(fieldSpec.accessorName(),
                    new BuiltinProcedure(fieldSpec.accessorName(),
                            (callArguments, callPos) -> applyRecordAccessor(recordType, fieldIndex,
                                    fieldSpec.accessorName(), callArguments, callPos)));
        }
        return VoidValue.INSTANCE;
    }

    private Value evalSet(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() != 2) {
            throw error("'set!' expects exactly 2 arguments", pos);
        }
        if (!(arguments.getFirst() instanceof SymbolExpr symbolExpr)) {
            throw error("'set!' target must be a symbol", arguments.getFirst().pos());
        }

        Value value = eval(arguments.get(1), environment);
        Environment bindingEnvironment = symbolExpr.lexicalEnvironment() != null
                ? symbolExpr.lexicalEnvironment()
                : environment;
        bindingEnvironment.assign(symbolExpr.name(), value, symbolExpr.pos());
        return VoidValue.INSTANCE;
    }

    private Value evalIf(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() != 3) {
            throw error("'if' expects exactly 3 arguments", pos);
        }

        Value condition = eval(arguments.get(0), environment);
        if (isTruthy(condition)) {
            return eval(arguments.get(1), environment);
        }
        return eval(arguments.get(2), environment);
    }

    private Value evalQuote(List<Expr> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() != 1) {
            throw error("'quote' expects exactly 1 argument", pos);
        }
        return QuotedValueBuilder.quote(arguments.getFirst());
    }

    private Value evalLambda(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() < 2) {
            throw error("'lambda' expects a parameter list and a body", pos);
        }

        ParameterSpec parameters = parseLambdaParameters(arguments.getFirst());
        List<Expr> body = new ArrayList<>(arguments.subList(1, arguments.size()));
        return new ClosureProcedure(null, parameters.required(), parameters.rest(), body,
                environment);
    }

    private Value evalBegin(List<Expr> arguments, Environment environment) throws EvalError {
        return evalSequence(arguments, environment);
    }

    private Value evalCond(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        for (int i = 0; i < arguments.size(); i++) {
            Expr clauseExpr = arguments.get(i);
            if (!(clauseExpr instanceof ListExpr clause) || clause.elements().isEmpty()) {
                throw error("'cond' clauses must be non-empty lists", pos);
            }

            List<Expr> clauseElements = clause.elements();
            Expr testExpr = clauseElements.getFirst();
            if (testExpr instanceof SymbolExpr symbolExpr && "else".equals(symbolExpr.name())) {
                if (i != arguments.size() - 1) {
                    throw error("'cond' else clause must be last", symbolExpr.pos());
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }

            Value testValue = eval(testExpr, environment);
            if (isTruthy(testValue)) {
                if (clauseElements.size() == 1) {
                    return testValue;
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), environment);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalLet(List<Expr> arguments, Environment environment, SourcePos pos)
            throws EvalError {
        if (arguments.size() < 2) {
            throw error("'let' expects bindings and a body", pos);
        }

        Expr head = arguments.getFirst();
        if (head instanceof SymbolExpr nameExpr) {
            return evalNamedLet(nameExpr, arguments.subList(1, arguments.size()), environment, pos);
        }
        if (head instanceof ListExpr bindingsExpr) {
            return evalSimpleLet(bindingsExpr, arguments.subList(1, arguments.size()), environment,
                    pos);
        }

        throw error("'let' expects a binding list", head.pos());
    }

    private Value evalSimpleLet(ListExpr bindingsExpr, List<Expr> body, Environment environment,
                                SourcePos pos) throws EvalError {
        if (body.isEmpty()) {
            throw error("'let' expects a body", pos);
        }

        List<LetBinding> bindings = parseBindings(bindingsExpr, pos);
        List<Value> values = evalBindingValues(bindings, environment);
        Environment letEnvironment = new Environment(environment);
        for (int i = 0; i < bindings.size(); i++) {
            letEnvironment.define(bindings.get(i).name(), values.get(i));
        }
        return evalSequence(body, letEnvironment);
    }

    private Value evalNamedLet(SymbolExpr nameExpr, List<Expr> arguments, Environment environment,
                               SourcePos pos) throws EvalError {
        if (arguments.isEmpty() || !(arguments.getFirst() instanceof ListExpr bindingsExpr)) {
            throw error("named 'let' expects a binding list", pos);
        }
        List<Expr> body = arguments.subList(1, arguments.size());
        if (body.isEmpty()) {
            throw error("named 'let' expects a body", pos);
        }

        List<LetBinding> bindings = parseBindings(bindingsExpr, pos);
        List<Value> values = evalBindingValues(bindings, environment);
        List<String> parameters = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            parameters.add(binding.name());
        }

        Environment closureEnvironment = new Environment(environment);
        ClosureProcedure procedure = new ClosureProcedure(nameExpr.name(), parameters, null,
                new ArrayList<>(body), closureEnvironment);
        closureEnvironment.define(nameExpr.name(), procedure);
        return apply(procedure, values, pos);
    }

    private List<LetBinding> parseBindings(ListExpr bindingsExpr, SourcePos pos) throws EvalError {
        List<LetBinding> bindings = new ArrayList<>(bindingsExpr.elements().size());
        for (Expr bindingExpr : bindingsExpr.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw error("'let' bindings must be lists", pos);
            }
            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw error("'let' bindings must contain a name and a value", bindingList.pos());
            }
            if (!(bindingElements.getFirst() instanceof SymbolExpr symbolExpr)) {
                throw error("'let' binding names must be symbols", bindingList.pos());
            }
            bindings.add(new LetBinding(symbolExpr.name(), bindingElements.get(1)));
        }
        return bindings;
    }

    private List<Value> evalBindingValues(List<LetBinding> bindings, Environment environment)
            throws EvalError {
        List<Value> values = new ArrayList<>(bindings.size());
        for (LetBinding binding : bindings) {
            values.add(eval(binding.initializer(), environment));
        }
        return values;
    }

    private Value evalSequence(List<Expr> expressions, Environment environment) throws EvalError {
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, environment);
        }
        return result;
    }

    private ParameterSpec parseLambdaParameters(Expr parameterExpr) throws EvalError {
        if (parameterExpr instanceof ListExpr listExpr) {
            return parseParameterSpec(listExpr.elements(), listExpr.pos());
        }
        if (parameterExpr instanceof SymbolExpr symbolExpr) {
            return new ParameterSpec(List.of(), symbolExpr.name());
        }
        throw error("'lambda' parameters must be a list or a symbol", parameterExpr.pos());
    }

    private ParameterSpec parseParameterSpec(List<Expr> parameterExprs, SourcePos pos)
            throws EvalError {
        List<String> parameters = new ArrayList<>(parameterExprs.size());
        String restParameter = null;
        boolean sawDot = false;

        for (int i = 0; i < parameterExprs.size(); i++) {
            Expr parameterExpr = parameterExprs.get(i);
            if (!(parameterExpr instanceof SymbolExpr symbolExpr)) {
                throw error("parameters must be symbols", parameterExpr.pos());
            }

            if (".".equals(symbolExpr.name())) {
                if (sawDot || i == parameterExprs.size() - 1) {
                    throw error("invalid dotted parameter list", symbolExpr.pos());
                }
                sawDot = true;
                continue;
            }

            if (sawDot) {
                if (i != parameterExprs.size() - 1) {
                    throw error("rest parameter must be last", parameterExpr.pos());
                }
                restParameter = symbolExpr.name();
                return new ParameterSpec(parameters, restParameter);
            }

            parameters.add(symbolExpr.name());
        }

        if (sawDot) {
            throw error("invalid dotted parameter list", pos);
        }

        return new ParameterSpec(parameters, null);
    }

    private List<Value> evalArguments(List<Expr> arguments, Environment environment)
            throws EvalError {
        List<Value> values = new ArrayList<>(arguments.size());
        for (Expr argument : arguments) {
            values.add(eval(argument, environment));
        }
        return values;
    }

    private Value evalAnd(List<Expr> arguments, Environment environment) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr argument : arguments) {
            result = eval(argument, environment);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> arguments, Environment environment) throws EvalError {
        for (Expr argument : arguments) {
            Value result = eval(argument, environment);
            if (isTruthy(result)) {
                return result;
            }
        }
        return BoolValue.FALSE;
    }

    private Value apply(Value procedure, List<Value> arguments, SourcePos pos)
            throws EvalError {
        if (procedure instanceof BuiltinProcedure builtinProcedure) {
            return builtinProcedure.implementation().apply(arguments, pos);
        }
        if (procedure instanceof ClosureProcedure closureProcedure) {
            int requiredCount = closureProcedure.parameters().size();
            if (closureProcedure.restParameter() == null && arguments.size() != requiredCount) {
                throw error("wrong number of arguments", pos);
            }
            if (closureProcedure.restParameter() != null && arguments.size() < requiredCount) {
                throw error("wrong number of arguments", pos);
            }

            Environment callEnvironment = new Environment(closureProcedure.environment());
            for (int i = 0; i < requiredCount; i++) {
                callEnvironment.define(closureProcedure.parameters().get(i), arguments.get(i));
            }
            if (closureProcedure.restParameter() != null) {
                callEnvironment.define(closureProcedure.restParameter(),
                        buildList(arguments.subList(requiredCount, arguments.size())));
            }

            return evalSequence(closureProcedure.body(), callEnvironment);
        }
        throw error("not a procedure", pos);
    }

    private SyntaxMacro lookupMacro(SymbolExpr symbolExpr, Environment environment) {
        Environment macroEnvironment = symbolExpr.lexicalEnvironment() != null
                ? symbolExpr.lexicalEnvironment()
                : environment;
        return macroEnvironment.lookupMacro(symbolExpr.name());
    }

    private Value applyAdd(List<Value> arguments, SourcePos pos) throws EvalError {
        return sum(arguments, pos).toValue();
    }

    private Value applySubtract(List<Value> arguments, SourcePos pos) throws EvalError {
        return subtract(arguments, pos).toValue();
    }

    private Value applyMultiply(List<Value> arguments, SourcePos pos) throws EvalError {
        return product(arguments, pos).toValue();
    }

    private Value applyDivide(List<Value> arguments, SourcePos pos) throws EvalError {
        return divide(arguments, pos).toValue();
    }

    private Value applyAbs(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "abs", pos);
        return asNumber(arguments.getFirst(), "abs", pos).abs().toValue();
    }

    private Value applyModulo(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "modulo", pos);
        BigInteger dividend = asExactInteger(arguments.get(0), "modulo", pos);
        BigInteger divisor = nonZeroExactInteger(arguments.get(1), "modulo", pos);
        BigInteger remainder = dividend.remainder(divisor);
        if (!BigInteger.ZERO.equals(remainder) && remainder.signum() != divisor.signum()) {
            remainder = remainder.add(divisor);
        }
        return new IntValue(remainder);
    }

    private Value applyRemainder(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "remainder", pos);
        BigInteger dividend = asExactInteger(arguments.get(0), "remainder", pos);
        BigInteger divisor = nonZeroExactInteger(arguments.get(1), "remainder", pos);
        return new IntValue(dividend.remainder(divisor));
    }

    private Value applyQuotient(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "quotient", pos);
        BigInteger dividend = asExactInteger(arguments.get(0), "quotient", pos);
        BigInteger divisor = nonZeroExactInteger(arguments.get(1), "quotient", pos);
        return new IntValue(dividend.divide(divisor));
    }

    private Value applyMin(List<Value> arguments, SourcePos pos) throws EvalError {
        requireAtLeastArgs(arguments, 1, "min", pos);
        SchemeNumber result = asNumber(arguments.getFirst(), "min", pos);
        for (int i = 1; i < arguments.size(); i++) {
            SchemeNumber candidate = asNumber(arguments.get(i), "min", pos);
            if (candidate.compareTo(result) < 0) {
                result = candidate;
            }
        }
        return result.toValue();
    }

    private Value applyMax(List<Value> arguments, SourcePos pos) throws EvalError {
        requireAtLeastArgs(arguments, 1, "max", pos);
        SchemeNumber result = asNumber(arguments.getFirst(), "max", pos);
        for (int i = 1; i < arguments.size(); i++) {
            SchemeNumber candidate = asNumber(arguments.get(i), "max", pos);
            if (candidate.compareTo(result) > 0) {
                result = candidate;
            }
        }
        return result.toValue();
    }

    private Value applyExpt(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "expt", pos);
        SchemeNumber base = asNumber(arguments.get(0), "expt", pos);
        BigInteger exponent = asExactInteger(arguments.get(1), "expt", pos);
        if (exponent.signum() < 0) {
            throw error("'expt' expects a non-negative exponent", pos);
        }
        if (exponent.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            throw error("'expt' exponent is too large", pos);
        }
        return base.pow(exponent.intValue()).toValue();
    }

    private Value applyCons(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "cons", pos);
        return new PairValue(arguments.get(0), arguments.get(1));
    }

    private Value applyCar(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "car", pos);
        return asPair(arguments.getFirst(), "car", pos).car();
    }

    private Value applyCdr(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "cdr", pos);
        return asPair(arguments.getFirst(), "cdr", pos).cdr();
    }

    private Value applyAppend(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        Value result = arguments.get(arguments.size() - 1);
        for (int i = arguments.size() - 2; i >= 0; i--) {
            result = copyListOnto(arguments.get(i), result, pos);
        }
        return result;
    }

    private Value applyListRef(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "list-ref", pos);
        int index = asIndex(arguments.get(1), "list-ref", pos);
        Value current = arguments.getFirst();
        for (int i = 0; i < index; i++) {
            if (!(current instanceof PairValue pairValue)) {
                throw error("'list-ref' index out of range", pos);
            }
            current = pairValue.cdr();
        }
        if (current instanceof PairValue pairValue) {
            return pairValue.car();
        }
        throw error("'list-ref' index out of range", pos);
    }

    private Value applyListTail(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "list-tail", pos);
        int index = asIndex(arguments.get(1), "list-tail", pos);
        Value current = arguments.getFirst();
        for (int i = 0; i < index; i++) {
            if (!(current instanceof PairValue pairValue)) {
                throw error("'list-tail' index out of range", pos);
            }
            current = pairValue.cdr();
        }
        return current;
    }

    private Value applyAssoc(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "assoc", pos);
        Value key = arguments.getFirst();
        Value current = arguments.get(1);
        while (current instanceof PairValue pairValue) {
            Value entry = pairValue.car();
            if (!(entry instanceof PairValue entryPair)) {
                throw error("'assoc' expects an association list", pos);
            }
            if (equalValues(key, entryPair.car())) {
                return entry;
            }
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return BoolValue.FALSE;
        }
        throw error("'assoc' expects a proper list", pos);
    }

    private Value applyMap(List<Value> arguments, SourcePos pos) throws EvalError {
        requireAtLeastArgs(arguments, 2, "map", pos);
        Value procedure = arguments.getFirst();
        List<Value> lists = new ArrayList<>(arguments.subList(1, arguments.size()));
        List<Value> results = new ArrayList<>();

        while (true) {
            boolean allEmpty = true;
            boolean anyEmpty = false;
            List<Value> callArguments = new ArrayList<>(lists.size());
            List<Value> nextLists = new ArrayList<>(lists.size());

            for (Value list : lists) {
                if (list instanceof EmptyListValue) {
                    anyEmpty = true;
                    nextLists.add(list);
                    continue;
                }
                if (!(list instanceof PairValue pairValue)) {
                    throw error("'map' expects proper list arguments", pos);
                }

                allEmpty = false;
                callArguments.add(pairValue.car());
                nextLists.add(pairValue.cdr());
            }

            if (allEmpty) {
                return buildList(results);
            }
            if (anyEmpty) {
                throw error("'map' list arguments must have the same length", pos);
            }

            results.add(apply(procedure, callArguments, pos));
            lists = nextLists;
        }
    }

    private Value applyDisplay(List<Value> arguments, SourcePos pos, StringBuilder output)
            throws EvalError {
        requireArgCount(arguments, 1, "display", pos);
        output.append(ValueFormatter.formatDisplay(arguments.getFirst()));
        return VoidValue.INSTANCE;
    }

    private Value applyWrite(List<Value> arguments, SourcePos pos, StringBuilder output)
            throws EvalError {
        requireArgCount(arguments, 1, "write", pos);
        output.append(ValueFormatter.format(arguments.getFirst()));
        return VoidValue.INSTANCE;
    }

    private Value applyNewline(List<Value> arguments, SourcePos pos, StringBuilder output)
            throws EvalError {
        requireArgCount(arguments, 0, "newline", pos);
        output.append('\n');
        return VoidValue.INSTANCE;
    }

    private Value applyStringAppend(List<Value> arguments, SourcePos pos) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value argument : arguments) {
            builder.append(asString(argument, "string-append", pos));
        }
        return new StringValue(builder.toString());
    }

    private Value applyStringLength(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string-length", pos);
        return new IntValue(BigInteger.valueOf(
                asString(arguments.getFirst(), "string-length", pos).length()));
    }

    private Value applySubstring(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 3, "substring", pos);
        String value = asString(arguments.get(0), "substring", pos);
        int start = asIndex(arguments.get(1), "substring", pos);
        int end = asIndex(arguments.get(2), "substring", pos);
        if (start > end || end > value.length()) {
            throw error("'substring' index out of range", pos);
        }
        return new StringValue(value.substring(start, end));
    }

    private Value applyStringToNumber(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string->number", pos);
        String value = asString(arguments.getFirst(), "string->number", pos);
        try {
            return SchemeNumber.parseLiteral(value).toValue();
        } catch (NumberFormatException ignored) {
            return BoolValue.FALSE;
        }
    }

    private Value applyNumberToString(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "number->string", pos);
        return new StringValue(asNumber(arguments.getFirst(), "number->string", pos).format());
    }

    private Value applyApply(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw error("'apply' expects at least 2 arguments", pos);
        }

        Value procedure = arguments.getFirst();
        List<Value> expandedArguments = new ArrayList<>(arguments.size() - 1);
        for (int i = 1; i < arguments.size() - 1; i++) {
            expandedArguments.add(arguments.get(i));
        }
        appendListElements(arguments.get(arguments.size() - 1), expandedArguments, pos);
        return apply(procedure, expandedArguments, pos);
    }

    private Value applySymbolToString(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "symbol->string", pos);
        return new StringValue(asSymbol(arguments.getFirst(), "symbol->string", pos));
    }

    private Value applyStringToSymbol(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string->symbol", pos);
        return new SymbolValue(asString(arguments.getFirst(), "string->symbol", pos));
    }

    private Value applyStringRef(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "string-ref", pos);
        StringValue value = asStringValue(arguments.get(0), "string-ref", pos);
        int index = asIndex(arguments.get(1), "string-ref", pos);
        if (index >= value.length()) {
            throw error("'string-ref' index out of range", pos);
        }
        return new CharValue(value.charAt(index));
    }

    private Value applyStringSet(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 3, "string-set!", pos);
        StringValue value = asStringValue(arguments.get(0), "string-set!", pos);
        int index = asIndex(arguments.get(1), "string-set!", pos);
        if (index >= value.length()) {
            throw error("'string-set!' index out of range", pos);
        }
        if (!value.mutable()) {
            throw error("'string-set!' cannot modify an immutable string", pos);
        }
        value.setCharAt(index, asChar(arguments.get(2), "string-set!", pos));
        return VoidValue.INSTANCE;
    }

    private Value applyStringCopy(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string-copy", pos);
        return asStringValue(arguments.getFirst(), "string-copy", pos).copy(true);
    }

    private Value applyStringEquals(List<Value> arguments, SourcePos pos) throws EvalError {
        return BoolValue.of(compareStrings(arguments, pos, "string=?", false));
    }

    private Value applyStringLess(List<Value> arguments, SourcePos pos) throws EvalError {
        return BoolValue.of(compareStrings(arguments, pos, "string<?", true));
    }

    private Value applyStringCiEquals(List<Value> arguments, SourcePos pos) throws EvalError {
        requireAtLeastArgs(arguments, 2, "string-ci=?", pos);
        String left = asString(arguments.getFirst(), "string-ci=?", pos);
        for (int i = 1; i < arguments.size(); i++) {
            String right = asString(arguments.get(i), "string-ci=?", pos);
            if (!left.equalsIgnoreCase(right)) {
                return BoolValue.FALSE;
            }
            left = right;
        }
        return BoolValue.TRUE;
    }

    private Value applyStringUpcase(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string-upcase", pos);
        return new StringValue(asString(arguments.getFirst(), "string-upcase", pos)
                .toUpperCase(Locale.ROOT));
    }

    private Value applyStringDowncase(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "string-downcase", pos);
        return new StringValue(asString(arguments.getFirst(), "string-downcase", pos)
                .toLowerCase(Locale.ROOT));
    }

    private Value applyCharUpcase(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "char-upcase", pos);
        return new CharValue(Character.toUpperCase(asChar(arguments.getFirst(), "char-upcase", pos)));
    }

    private Value applyCharDowncase(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "char-downcase", pos);
        return new CharValue(Character.toLowerCase(
                asChar(arguments.getFirst(), "char-downcase", pos)));
    }

    private Value applyExactToInexact(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "exact->inexact", pos);
        return asNumber(arguments.getFirst(), "exact->inexact", pos).exactToInexact().toValue();
    }

    private Value applyInexactToExact(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "inexact->exact", pos);
        return asNumber(arguments.getFirst(), "inexact->exact", pos).inexactToExact().toValue();
    }

    private Value applyNumerator(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "numerator", pos);
        return new IntValue(asExactNumber(arguments.getFirst(), "numerator", pos).numeratorExact());
    }

    private Value applyDenominator(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "denominator", pos);
        return new IntValue(
                asExactNumber(arguments.getFirst(), "denominator", pos).denominatorExact());
    }

    private SchemeNumber sum(List<Value> arguments, SourcePos pos) throws EvalError {
        SchemeNumber result = SchemeNumber.exact(BigInteger.ZERO);
        for (Value argument : arguments) {
            result = result.add(asNumber(argument, "+", pos));
        }
        return result;
    }

    private SchemeNumber subtract(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.isEmpty()) {
            throw error("'-' expects at least 1 argument", pos);
        }

        SchemeNumber result = asNumber(arguments.getFirst(), "-", pos);
        if (arguments.size() == 1) {
            return result.negate();
        }

        for (int i = 1; i < arguments.size(); i++) {
            result = result.subtract(asNumber(arguments.get(i), "-", pos));
        }
        return result;
    }

    private SchemeNumber product(List<Value> arguments, SourcePos pos) throws EvalError {
        SchemeNumber result = SchemeNumber.exact(BigInteger.ONE);
        for (Value argument : arguments) {
            result = result.multiply(asNumber(argument, "*", pos));
        }
        return result;
    }

    private SchemeNumber divide(List<Value> arguments, SourcePos pos) throws EvalError {
        if (arguments.size() < 2) {
            throw error("'/' expects at least 2 arguments", pos);
        }

        SchemeNumber result = asNumber(arguments.getFirst(), "/", pos);
        for (int i = 1; i < arguments.size(); i++) {
            SchemeNumber divisor = asNumber(arguments.get(i), "/", pos);
            if (divisor.isZero()) {
                throw error("division by zero", pos);
            }
            result = result.divide(divisor);
        }
        return result;
    }

    private boolean compare(List<Value> arguments, SourcePos pos, Comparison comparison)
            throws EvalError {
        if (arguments.size() < 2) {
            throw error("comparison expects at least 2 arguments", pos);
        }

        SchemeNumber left = asNumber(arguments.getFirst(), comparison.name, pos);
        for (int i = 1; i < arguments.size(); i++) {
            SchemeNumber right = asNumber(arguments.get(i), comparison.name, pos);
            if (!comparison.matches(left.compareTo(right))) {
                return false;
            }
            left = right;
        }
        return true;
    }

    private boolean not(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "not", pos);
        return !isTruthy(arguments.getFirst());
    }

    private boolean isNull(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "null?", pos);
        return arguments.getFirst() instanceof EmptyListValue;
    }

    private long length(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "length", pos);
        return listLength(arguments.getFirst(), "length", pos);
    }

    private boolean isZero(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "zero?", pos);
        return asNumber(arguments.getFirst(), "zero?", pos).isZero();
    }

    private boolean isPositive(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "positive?", pos);
        return asNumber(arguments.getFirst(), "positive?", pos).signum() > 0;
    }

    private boolean isNegative(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "negative?", pos);
        return asNumber(arguments.getFirst(), "negative?", pos).signum() < 0;
    }

    private boolean isOdd(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "odd?", pos);
        return !asExactInteger(arguments.getFirst(), "odd?", pos).mod(BigInteger.TWO)
                .equals(BigInteger.ZERO);
    }

    private boolean isEven(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "even?", pos);
        return asExactInteger(arguments.getFirst(), "even?", pos).mod(BigInteger.TWO)
                .equals(BigInteger.ZERO);
    }

    private boolean isExact(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "exact?", pos);
        SchemeNumber number = asMaybeNumber(arguments.getFirst());
        return number != null && number.isExact();
    }

    private boolean isInexact(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "inexact?", pos);
        SchemeNumber number = asMaybeNumber(arguments.getFirst());
        return number != null && !number.isExact();
    }

    private boolean isInteger(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "integer?", pos);
        SchemeNumber number = asMaybeNumber(arguments.getFirst());
        return number != null && number.isInteger();
    }

    private boolean isRational(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "rational?", pos);
        SchemeNumber number = asMaybeNumber(arguments.getFirst());
        return number != null && number.isExact();
    }

    private boolean isEq(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "eq?", pos);
        return eqValues(arguments.get(0), arguments.get(1));
    }

    private boolean isEqual(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 2, "equal?", pos);
        return equalValues(arguments.get(0), arguments.get(1));
    }

    private boolean isList(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "list?", pos);
        Value current = arguments.getFirst();
        while (current instanceof PairValue pairValue) {
            current = pairValue.cdr();
        }
        return current instanceof EmptyListValue;
    }

    private boolean isCharAlphabetic(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "char-alphabetic?", pos);
        return Character.isLetter(asChar(arguments.getFirst(), "char-alphabetic?", pos));
    }

    private boolean isCharNumeric(List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, "char-numeric?", pos);
        return Character.isDigit(asChar(arguments.getFirst(), "char-numeric?", pos));
    }

    private boolean compareChars(List<Value> arguments, SourcePos pos, String name,
                                 boolean equalityOnly) throws EvalError {
        requireAtLeastArgs(arguments, 2, name, pos);
        char left = asChar(arguments.getFirst(), name, pos);
        for (int i = 1; i < arguments.size(); i++) {
            char right = asChar(arguments.get(i), name, pos);
            if (equalityOnly) {
                if (left != right) {
                    return false;
                }
            } else if (left >= right) {
                return false;
            }
            left = right;
        }
        return true;
    }

    private boolean compareStrings(List<Value> arguments, SourcePos pos, String name,
                                   boolean lessThan) throws EvalError {
        requireAtLeastArgs(arguments, 2, name, pos);
        String left = asString(arguments.getFirst(), name, pos);
        for (int i = 1; i < arguments.size(); i++) {
            String right = asString(arguments.get(i), name, pos);
            int comparison = left.compareTo(right);
            if (lessThan) {
                if (comparison >= 0) {
                    return false;
                }
            } else if (comparison != 0) {
                return false;
            }
            left = right;
        }
        return true;
    }

    private RecordFieldSpec parseRecordFieldSpec(Expr expression) throws EvalError {
        if (!(expression instanceof ListExpr fieldExpr)) {
            throw error("'define-record-type' field specs must be lists", expression.pos());
        }

        List<Expr> fieldElements = fieldExpr.elements();
        if (fieldElements.size() != 2) {
            throw error("record field spec must contain a field name and an accessor",
                    fieldExpr.pos());
        }
        return new RecordFieldSpec(
                requireSymbolName(fieldElements.get(0), "record field name"),
                requireSymbolName(fieldElements.get(1), "record accessor name"),
                fieldExpr.pos());
    }

    private String requireSymbolName(Expr expression, String description) throws EvalError {
        if (expression instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        throw error(description + " must be a symbol", expression.pos());
    }

    private Value applyRecordConstructor(RecordType recordType, List<Integer> constructorFieldIndexes,
                                         String name, List<Value> arguments, SourcePos pos)
            throws EvalError {
        requireArgCount(arguments, constructorFieldIndexes.size(), name, pos);
        List<Value> fields = new ArrayList<>(recordType.fieldNames().size());
        for (int i = 0; i < recordType.fieldNames().size(); i++) {
            fields.add(VoidValue.INSTANCE);
        }
        for (int i = 0; i < constructorFieldIndexes.size(); i++) {
            fields.set(constructorFieldIndexes.get(i), arguments.get(i));
        }
        return new RecordValue(recordType, List.copyOf(fields));
    }

    private Value applyRecordPredicate(RecordType recordType, String name, List<Value> arguments,
                                       SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, name, pos);
        return BoolValue.of(arguments.getFirst() instanceof RecordValue recordValue
                && recordValue.type() == recordType);
    }

    private Value applyRecordAccessor(RecordType recordType, int fieldIndex, String name,
                                      List<Value> arguments, SourcePos pos) throws EvalError {
        requireArgCount(arguments, 1, name, pos);
        return asRecord(arguments.getFirst(), recordType, name, pos).field(fieldIndex);
    }

    private boolean eqValues(Value left, Value right) {
        if (left == right) {
            return true;
        }
        if (left instanceof NumericValue leftNumber && right instanceof NumericValue rightNumber) {
            return SchemeNumber.fromValue(leftNumber).equals(SchemeNumber.fromValue(rightNumber));
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
        return left instanceof EmptyListValue && right instanceof EmptyListValue;
    }

    private boolean equalValues(Value left, Value right) {
        if (eqValues(left, right)) {
            return true;
        }
        if (left instanceof NumericValue leftNumber && right instanceof NumericValue rightNumber) {
            return SchemeNumber.fromValue(leftNumber)
                    .numericallyEquals(SchemeNumber.fromValue(rightNumber));
        }
        if (left instanceof StringValue leftString && right instanceof StringValue rightString) {
            return leftString.value().equals(rightString.value());
        }
        if (left instanceof PairValue leftPair && right instanceof PairValue rightPair) {
            return equalValues(leftPair.car(), rightPair.car())
                    && equalValues(leftPair.cdr(), rightPair.cdr());
        }
        return false;
    }

    private boolean isType(List<Value> arguments, SourcePos pos, String name,
                           Class<? extends Value> expectedType) throws EvalError {
        requireArgCount(arguments, 1, name, pos);
        return expectedType.isInstance(arguments.getFirst());
    }

    private void requireAtLeastArgs(List<Value> arguments, int expected, String name, SourcePos pos)
            throws EvalError {
        if (arguments.size() < expected) {
            throw error("'" + name + "' expects at least " + expected + " argument"
                    + (expected == 1 ? "" : "s"), pos);
        }
    }

    private void requireArgCount(List<Value> arguments, int expected, String name, SourcePos pos)
            throws EvalError {
        if (arguments.size() != expected) {
            throw error("'" + name + "' expects exactly " + expected + " argument"
                    + (expected == 1 ? "" : "s"), pos);
        }
    }

    private BigInteger nonZeroExactInteger(Value value, String operator, SourcePos pos)
            throws EvalError {
        BigInteger divisor = asExactInteger(value, operator, pos);
        if (BigInteger.ZERO.equals(divisor)) {
            throw error("division by zero", pos);
        }
        return divisor;
    }

    private SchemeNumber asNumber(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof NumericValue numericValue) {
            return SchemeNumber.fromValue(numericValue);
        }
        throw error("'" + operator + "' expects numeric arguments", pos);
    }

    private SchemeNumber asExactNumber(Value value, String operator, SourcePos pos)
            throws EvalError {
        SchemeNumber number = asNumber(value, operator, pos);
        if (!number.isExact()) {
            throw error("'" + operator + "' expects an exact number", pos);
        }
        return number;
    }

    private BigInteger asExactInteger(Value value, String operator, SourcePos pos)
            throws EvalError {
        SchemeNumber number = asNumber(value, operator, pos);
        if (!number.isExact() || !number.isInteger()) {
            throw error("'" + operator + "' expects an exact integer", pos);
        }
        return number.integerExact();
    }

    private SchemeNumber asMaybeNumber(Value value) {
        if (value instanceof NumericValue numericValue) {
            return SchemeNumber.fromValue(numericValue);
        }
        return null;
    }

    private PairValue asPair(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw error("'" + operator + "' expects a pair", pos);
    }

    private RecordValue asRecord(Value value, RecordType expectedType, String operator,
                                 SourcePos pos) throws EvalError {
        if (value instanceof RecordValue recordValue && recordValue.type() == expectedType) {
            return recordValue;
        }
        throw error("'" + operator + "' expects a " + expectedType.name() + " record", pos);
    }

    private String asString(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue.value();
        }
        throw error("'" + operator + "' expects string arguments", pos);
    }

    private StringValue asStringValue(Value value, String operator, SourcePos pos)
            throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw error("'" + operator + "' expects string arguments", pos);
    }

    private String asSymbol(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof SymbolValue symbolValue) {
            return symbolValue.name();
        }
        throw error("'" + operator + "' expects a symbol", pos);
    }

    private char asChar(Value value, String operator, SourcePos pos) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw error("'" + operator + "' expects a character", pos);
    }

    private int asIndex(Value value, String operator, SourcePos pos) throws EvalError {
        BigInteger index = asExactInteger(value, operator, pos);
        if (index.signum() < 0 || index.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
            throw error("'" + operator + "' expects a non-negative integer index", pos);
        }
        return index.intValue();
    }

    private Value buildList(List<Value> values) {
        Value result = EmptyListValue.INSTANCE;
        for (int i = values.size() - 1; i >= 0; i--) {
            result = new PairValue(values.get(i), result);
        }
        return result;
    }

    private long listLength(Value value, String operator, SourcePos pos) throws EvalError {
        long length = 0;
        Value current = value;
        while (current instanceof PairValue pairValue) {
            length++;
            current = pairValue.cdr();
        }
        if (current instanceof EmptyListValue) {
            return length;
        }
        throw error("'" + operator + "' expects a proper list", pos);
    }

    private Value copyListOnto(Value list, Value tail, SourcePos pos) throws EvalError {
        List<Value> prefix = new ArrayList<>();
        Value current = list;
        while (current instanceof PairValue pairValue) {
            prefix.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw error("'append' expects list arguments", pos);
        }

        Value result = tail;
        for (int i = prefix.size() - 1; i >= 0; i--) {
            result = new PairValue(prefix.get(i), result);
        }
        return result;
    }

    private void appendListElements(Value list, List<Value> destination, SourcePos pos)
            throws EvalError {
        Value current = list;
        while (current instanceof PairValue pairValue) {
            destination.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (!(current instanceof EmptyListValue)) {
            throw error("'apply' expects a proper list as its last argument", pos);
        }
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private EvalError error(String message, SourcePos pos) {
        return new EvalError(message, pos.line(), pos.column());
    }

    private record LetBinding(String name, Expr initializer) {
    }

    private record ParameterSpec(List<String> required, String rest) {
    }

    private record RecordFieldSpec(String name, String accessorName, SourcePos pos) {
    }

    private record ProgramResult(Value value, String output) {
    }
}
