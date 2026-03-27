package ming;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Scheme interpreter entry point.
 */
public class Evaluator {
    private final Environment globalEnv = createGlobalEnv();
    private final Map<String, Macro> macros = new HashMap<>();
    private StringBuilder currentOutput;
    private long macroExpansionCounter;

    public String evalStr(String input) throws EvalError {
        return evalProgram(input).result();
    }

    public EvalResult evalStrWithOutput(String input) throws EvalError {
        return evalProgram(input);
    }

    private EvalResult evalProgram(String input) throws EvalError {
        Parser parser = new Parser(input);
        List<Expr> program = parser.parseProgram();
        if (program.isEmpty()) {
            throw new EvalError("empty input", 1, 1);
        }

        StringBuilder previousOutput = currentOutput;
        currentOutput = new StringBuilder();
        try {
            Value last = VoidValue.INSTANCE;
            for (Expr expr : program) {
                last = eval(expr, globalEnv);
            }

            return new EvalResult(last.toSchemeString(), currentOutput.toString());
        } finally {
            currentOutput = previousOutput;
        }
    }

    private Environment createGlobalEnv() {
        Environment env = new Environment(null);
        env.define("+", new BuiltinProcedure("+", this::builtinAdd));
        env.define("-", new BuiltinProcedure("-", this::builtinSub));
        env.define("*", new BuiltinProcedure("*", this::builtinMul));
        env.define("/", new BuiltinProcedure("/", this::builtinDiv));
        env.define("abs", new BuiltinProcedure("abs", this::builtinAbs));
        env.define("modulo", new BuiltinProcedure("modulo", this::builtinModulo));
        env.define("remainder", new BuiltinProcedure("remainder", this::builtinRemainder));
        env.define("quotient", new BuiltinProcedure("quotient", this::builtinQuotient));
        env.define("min", new BuiltinProcedure("min", this::builtinMin));
        env.define("max", new BuiltinProcedure("max", this::builtinMax));
        env.define("expt", new BuiltinProcedure("expt", this::builtinExpt));
        env.define("<", new BuiltinProcedure("<", args -> builtinComparison(args, Comparison.LT, "<")));
        env.define(">", new BuiltinProcedure(">", args -> builtinComparison(args, Comparison.GT, ">")));
        env.define("=", new BuiltinProcedure("=", args -> builtinComparison(args, Comparison.EQ, "=")));
        env.define("<=", new BuiltinProcedure("<=", args -> builtinComparison(args, Comparison.LE, "<=")));
        env.define("not", new BuiltinProcedure("not", this::builtinNot));
        env.define("zero?", new BuiltinProcedure("zero?", this::builtinZeroPredicate));
        env.define("positive?", new BuiltinProcedure("positive?", this::builtinPositivePredicate));
        env.define("negative?", new BuiltinProcedure("negative?", this::builtinNegativePredicate));
        env.define("odd?", new BuiltinProcedure("odd?", this::builtinOddPredicate));
        env.define("even?", new BuiltinProcedure("even?", this::builtinEvenPredicate));
        env.define("cons", new BuiltinProcedure("cons", this::builtinCons));
        env.define("car", new BuiltinProcedure("car", this::builtinCar));
        env.define("cdr", new BuiltinProcedure("cdr", this::builtinCdr));
        env.define("null?", new BuiltinProcedure("null?", this::builtinNull));
        env.define("list", new BuiltinProcedure("list", this::builtinList));
        env.define("length", new BuiltinProcedure("length", this::builtinLength));
        env.define("append", new BuiltinProcedure("append", this::builtinAppend));
        env.define("apply", new BuiltinProcedure("apply", this::builtinApply));
        env.define("list-ref", new BuiltinProcedure("list-ref", this::builtinListRef));
        env.define("list-tail", new BuiltinProcedure("list-tail", this::builtinListTail));
        env.define("list?", new BuiltinProcedure("list?", this::builtinListPredicate));
        env.define("assoc", new BuiltinProcedure("assoc", this::builtinAssoc));
        env.define("map", new BuiltinProcedure("map", this::builtinMap));
        env.define("string?", new BuiltinProcedure("string?", this::builtinStringPredicate));
        env.define("number?", new BuiltinProcedure("number?", this::builtinNumberPredicate));
        env.define("exact?", new BuiltinProcedure("exact?", this::builtinExactPredicate));
        env.define("inexact?", new BuiltinProcedure("inexact?", this::builtinInexactPredicate));
        env.define("integer?", new BuiltinProcedure("integer?", this::builtinIntegerPredicate));
        env.define("rational?", new BuiltinProcedure("rational?", this::builtinRationalPredicate));
        env.define("exact->inexact", new BuiltinProcedure("exact->inexact", this::builtinExactToInexact));
        env.define("inexact->exact", new BuiltinProcedure("inexact->exact", this::builtinInexactToExact));
        env.define("numerator", new BuiltinProcedure("numerator", this::builtinNumerator));
        env.define("denominator", new BuiltinProcedure("denominator", this::builtinDenominator));
        env.define("boolean?", new BuiltinProcedure("boolean?", this::builtinBooleanPredicate));
        env.define("pair?", new BuiltinProcedure("pair?", this::builtinPairPredicate));
        env.define("symbol?", new BuiltinProcedure("symbol?", this::builtinSymbolPredicate));
        env.define("eq?", new BuiltinProcedure("eq?", this::builtinEq));
        env.define("equal?", new BuiltinProcedure("equal?", this::builtinEqual));
        env.define("display", new BuiltinProcedure("display", this::builtinDisplay));
        env.define("write", new BuiltinProcedure("write", this::builtinWrite));
        env.define("newline", new BuiltinProcedure("newline", this::builtinNewline));
        env.define("string-append", new BuiltinProcedure("string-append", this::builtinStringAppend));
        env.define("string-length", new BuiltinProcedure("string-length", this::builtinStringLength));
        env.define("string=?", new BuiltinProcedure("string=?", this::builtinStringEquals));
        env.define("string<?", new BuiltinProcedure("string<?", this::builtinStringLessThan));
        env.define("string-ci=?", new BuiltinProcedure("string-ci=?", this::builtinStringCaseInsensitiveEquals));
        env.define("string-upcase", new BuiltinProcedure("string-upcase", this::builtinStringUpcase));
        env.define("string-downcase", new BuiltinProcedure("string-downcase", this::builtinStringDowncase));
        env.define("substring", new BuiltinProcedure("substring", this::builtinSubstring));
        env.define("string->number", new BuiltinProcedure("string->number", this::builtinStringToNumber));
        env.define("number->string", new BuiltinProcedure("number->string", this::builtinNumberToString));
        env.define("symbol->string", new BuiltinProcedure("symbol->string", this::builtinSymbolToString));
        env.define("string->symbol", new BuiltinProcedure("string->symbol", this::builtinStringToSymbol));
        env.define("string-copy", new BuiltinProcedure("string-copy", this::builtinStringCopy));
        env.define("string-ref", new BuiltinProcedure("string-ref", this::builtinStringRef));
        env.define("string-set!", new BuiltinProcedure("string-set!", this::builtinStringSet));
        env.define("char?", new BuiltinProcedure("char?", this::builtinCharPredicate));
        env.define("char-alphabetic?", new BuiltinProcedure("char-alphabetic?", this::builtinCharAlphabeticPredicate));
        env.define("char-numeric?", new BuiltinProcedure("char-numeric?", this::builtinCharNumericPredicate));
        env.define("char-upcase", new BuiltinProcedure("char-upcase", this::builtinCharUpcase));
        env.define("char-downcase", new BuiltinProcedure("char-downcase", this::builtinCharDowncase));
        env.define("char=?", new BuiltinProcedure("char=?", args -> builtinCharComparison(args, Comparison.EQ, "char=?")));
        env.define("char<?", new BuiltinProcedure("char<?", args -> builtinCharComparison(args, Comparison.LT, "char<?")));
        return env;
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        try {
            return switch (expr) {
                case IntExpr intExpr -> new IntValue(intExpr.value());
                case RationalExpr rationalExpr -> exactFractionToValue(
                        new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator()));
                case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
                case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
                case StringExpr stringExpr -> new StringValue(stringExpr.value());
                case CharExpr charExpr -> new CharValue(charExpr.value());
                case SymbolExpr symbolExpr -> env.lookup(symbolExpr.name());
                case CapturedSymbolExpr symbolExpr -> symbolExpr.env().lookup(symbolExpr.name());
                case ListExpr listExpr -> evalList(listExpr, env);
            };
        } catch (EvalError err) {
            throw attachPosition(err, expr);
        }
    }

    private Value evalList(ListExpr expr, Environment env) throws EvalError {
        List<Expr> elements = expr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> args = elements.subList(1, elements.size());
        if (head instanceof SymbolExpr symbol) {
            String name = symbol.name();
            return switch (name) {
                case "define" -> evalDefine(args, env);
                case "define-syntax" -> evalDefineSyntax(args, env);
                case "define-record-type" -> evalDefineRecordType(args, env);
                case "set!" -> evalSet(args, env);
                case "if" -> evalIf(args, env);
                case "quote" -> evalQuote(args);
                case "lambda" -> evalLambda(args, env);
                case "begin" -> evalBegin(args, env);
                case "let" -> evalLet(args, env);
                case "cond" -> evalCond(args, env);
                case "and" -> evalAnd(args, env);
                case "or" -> evalOr(args, env);
                default -> {
                    Macro macro = macros.get(name);
                    if (macro != null) {
                        yield eval(expandMacro(macro, expr), env);
                    }
                    yield evalApplication(head, args, env);
                }
            };
        }

        return evalApplication(head, args, env);
    }

    private Value evalApplication(Expr operatorExpr, List<Expr> argExprs, Environment env)
            throws EvalError {
        Value operator = eval(operatorExpr, env);
        List<Value> args = new ArrayList<>(argExprs.size());
        for (Expr argExpr : argExprs) {
            args.add(eval(argExpr, env));
        }
        return apply(operator, args);
    }

    private Value evalDefine(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'define' expects a target and a value");
        }

        Expr target = args.getFirst();
        if (target instanceof SymbolExpr symbol) {
            if (args.size() != 2) {
                throw new EvalError("variable define expects exactly 2 arguments");
            }
            Value value = eval(args.get(1), env);
            env.define(symbol.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof ListExpr signature) {
            List<Expr> signatureElements = signature.elements();
            if (signatureElements.isEmpty()) {
                throw new EvalError("function define requires a function name");
            }

            Expr nameExpr = signatureElements.getFirst();
            if (!(nameExpr instanceof SymbolExpr functionName)) {
                throw new EvalError("function define requires a symbol name");
            }

            ParameterSpec params = parseParameterSpec(
                    signatureElements.subList(1, signatureElements.size()));
            List<Expr> body = List.copyOf(args.subList(1, args.size()));
            ClosureValue closure = new ClosureValue(functionName.name(), params, body, env);
            env.define(functionName.name(), closure);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("invalid define target");
    }

    private Value evalDefineSyntax(List<Expr> args, Environment env) throws EvalError {
        requireArgCount(args.size(), 2, "define-syntax");
        if (!(args.getFirst() instanceof SymbolExpr name)) {
            throw new EvalError("'define-syntax' expects a symbol name");
        }

        macros.put(name.name(), parseMacro(name.name(), args.get(1), env));
        return VoidValue.INSTANCE;
    }

    private Value evalDefineRecordType(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 3) {
            throw new EvalError("'define-record-type' expects a type, constructor, predicate, and fields");
        }

        if (!(args.getFirst() instanceof SymbolExpr typeNameExpr)) {
            throw new EvalError("'define-record-type' expects a symbolic type name");
        }

        if (!(args.get(1) instanceof ListExpr constructorExpr)) {
            throw new EvalError("'define-record-type' expects a constructor specification");
        }

        List<Expr> constructorParts = constructorExpr.elements();
        if (constructorParts.isEmpty()) {
            throw new EvalError("'define-record-type' constructor specification cannot be empty");
        }
        if (!(constructorParts.getFirst() instanceof SymbolExpr constructorNameExpr)) {
            throw new EvalError("'define-record-type' constructor name must be a symbol");
        }

        if (!(args.get(2) instanceof SymbolExpr predicateNameExpr)) {
            throw new EvalError("'define-record-type' predicate name must be a symbol");
        }

        List<String> fieldNames = new ArrayList<>(constructorParts.size() - 1);
        for (int i = 1; i < constructorParts.size(); i++) {
            Expr fieldExpr = constructorParts.get(i);
            if (!(fieldExpr instanceof SymbolExpr fieldNameExpr)) {
                throw new EvalError("'define-record-type' constructor fields must be symbols");
            }
            fieldNames.add(fieldNameExpr.name());
        }

        List<FieldSpec> fieldSpecs = new ArrayList<>(args.size() - 3);
        for (int i = 3; i < args.size(); i++) {
            Expr fieldExpr = args.get(i);
            if (!(fieldExpr instanceof ListExpr fieldList)) {
                throw new EvalError("'define-record-type' field specifications must be lists");
            }

            List<Expr> fieldParts = fieldList.elements();
            if (fieldParts.size() != 2) {
                throw new EvalError("'define-record-type' field specifications must have 2 parts");
            }
            if (!(fieldParts.getFirst() instanceof SymbolExpr fieldNameExpr)) {
                throw new EvalError("'define-record-type' field name must be a symbol");
            }
            if (!(fieldParts.get(1) instanceof SymbolExpr accessorNameExpr)) {
                throw new EvalError("'define-record-type' accessor name must be a symbol");
            }

            fieldSpecs.add(new FieldSpec(fieldNameExpr.name(), accessorNameExpr.name()));
        }

        if (fieldNames.size() != fieldSpecs.size()) {
            throw new EvalError("'define-record-type' constructor and field count must match");
        }

        RecordType recordType = new RecordType(typeNameExpr.name());
        String constructorName = constructorNameExpr.name();
        env.define(constructorName, new BuiltinProcedure(
                constructorName,
                constructorArgs -> {
                    requireArgCount(constructorArgs.size(), fieldSpecs.size(), constructorName);
                    return new RecordValue(recordType, List.copyOf(constructorArgs));
                }));

        String predicateName = predicateNameExpr.name();
        env.define(predicateName, new BuiltinProcedure(
                predicateName,
                predicateArgs -> {
                    requireArgCount(predicateArgs.size(), 1, predicateName);
                    return BoolValue.of(predicateArgs.getFirst() instanceof RecordValue recordValue
                            && recordValue.type() == recordType);
                }));

        for (int i = 0; i < fieldSpecs.size(); i++) {
            FieldSpec fieldSpec = fieldSpecs.get(i);
            String expectedField = fieldNames.get(i);
            if (!fieldSpec.fieldName().equals(expectedField)) {
                throw new EvalError("'define-record-type' field order must match constructor fields");
            }

            int fieldIndex = i;
            String accessorName = fieldSpec.accessorName();
            env.define(accessorName, new BuiltinProcedure(
                    accessorName,
                    accessorArgs -> {
                        requireArgCount(accessorArgs.size(), 1, accessorName);
                        RecordValue recordValue = requireRecordOfType(
                                accessorArgs.getFirst(),
                                recordType,
                                accessorName);
                        return recordValue.fields().get(fieldIndex);
                    }));
        }

        return VoidValue.INSTANCE;
    }

    private Value evalSet(List<Expr> args, Environment env) throws EvalError {
        requireArgCount(args.size(), 2, "set!");

        Expr target = args.getFirst();
        Value value = eval(args.get(1), env);

        if (target instanceof SymbolExpr symbol) {
            env.set(symbol.name(), value);
            return VoidValue.INSTANCE;
        }

        if (target instanceof CapturedSymbolExpr symbol) {
            symbol.env().set(symbol.name(), value);
            return VoidValue.INSTANCE;
        }

        throw new EvalError("'set!' expects a symbol target");
    }

    private Value evalIf(List<Expr> args, Environment env) throws EvalError {
        requireArgCount(args.size(), 3, "if");
        Value condition = eval(args.get(0), env);
        if (isTruthy(condition)) {
            return eval(args.get(1), env);
        }
        return eval(args.get(2), env);
    }

    private Value evalQuote(List<Expr> args) throws EvalError {
        requireArgCount(args.size(), 1, "quote");
        return quoteToValue(args.getFirst());
    }

    private Value evalLambda(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'lambda' expects a parameter list and a body");
        }

        ParameterSpec params = parseLambdaParameterSpec(args.getFirst());
        List<Expr> body = List.copyOf(args.subList(1, args.size()));
        return new ClosureValue(null, params, body, env);
    }

    private Value evalBegin(List<Expr> args, Environment env) throws EvalError {
        return evalSequence(args, env);
    }

    private Value evalLet(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let' expects bindings and a body");
        }

        Expr head = args.getFirst();
        if (head instanceof SymbolExpr name) {
            return evalNamedLet(name.name(), args.subList(1, args.size()), env);
        }

        List<Binding> bindings = parseBindings(head);
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            values.add(eval(binding.valueExpr(), env));
        }

        Environment letEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            letEnv.define(bindings.get(i).name(), values.get(i));
        }

        return evalSequence(args.subList(1, args.size()), letEnv);
    }

    private Value evalNamedLet(String name, List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        List<String> params = new ArrayList<>(bindings.size());
        List<Value> values = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            params.add(binding.name());
            values.add(eval(binding.valueExpr(), env));
        }

        Environment closureEnv = new Environment(env);
        ClosureValue closure = new ClosureValue(
                name,
                new ParameterSpec(List.copyOf(params), null),
                List.copyOf(args.subList(1, args.size())),
                closureEnv);
        closureEnv.define(name, closure);
        return applyClosure(closure, values);
    }

    private List<Binding> parseBindings(Expr bindingsExpr) throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw new EvalError("'let' expects a binding list");
        }

        List<Binding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("binding must be a list");
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError("binding must contain a name and a value");
            }
            if (!(bindingElements.getFirst() instanceof SymbolExpr name)) {
                throw new EvalError("binding name must be a symbol");
            }

            bindings.add(new Binding(name.name(), bindingElements.get(1)));
        }
        return List.copyOf(bindings);
    }

    private Value evalCond(List<Expr> args, Environment env) throws EvalError {
        for (int i = 0; i < args.size(); i++) {
            Expr clauseExpr = args.get(i);
            if (!(clauseExpr instanceof ListExpr clause)) {
                throw new EvalError("'cond' clauses must be lists");
            }

            List<Expr> clauseElements = clause.elements();
            if (clauseElements.isEmpty()) {
                throw new EvalError("'cond' clause cannot be empty");
            }

            Expr testExpr = clauseElements.getFirst();
            boolean isElseClause = testExpr instanceof SymbolExpr symbol
                    && symbol.name().equals("else");
            if (isElseClause && i != args.size() - 1) {
                throw new EvalError("'cond' else clause must be last");
            }

            Value testResult = isElseClause ? BoolValue.TRUE : eval(testExpr, env);
            if (isTruthy(testResult)) {
                if (clauseElements.size() == 1) {
                    return testResult;
                }
                return evalSequence(clauseElements.subList(1, clauseElements.size()), env);
            }
        }

        return VoidValue.INSTANCE;
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        Value result = VoidValue.INSTANCE;
        for (Expr expression : expressions) {
            result = eval(expression, env);
        }
        return result;
    }

    private ParameterSpec parseLambdaParameterSpec(Expr paramsExpr) throws EvalError {
        return switch (paramsExpr) {
            case SymbolExpr symbolExpr -> {
                if (symbolExpr.name().equals(".")) {
                    throw new EvalError("invalid rest parameter");
                }
                yield new ParameterSpec(List.of(), symbolExpr.name());
            }
            case ListExpr listExpr -> parseParameterSpec(listExpr.elements());
            default -> throw new EvalError("'lambda' expects a parameter list");
        };
    }

    private ParameterSpec parseParameterSpec(List<Expr> paramExprs) throws EvalError {
        List<String> params = new ArrayList<>(paramExprs.size());
        String restParam = null;

        for (int i = 0; i < paramExprs.size(); i++) {
            Expr paramExpr = paramExprs.get(i);
            if (!(paramExpr instanceof SymbolExpr symbol)) {
                throw new EvalError("parameter names must be symbols");
            }

            if (!symbol.name().equals(".")) {
                params.add(symbol.name());
                continue;
            }

            if (i != paramExprs.size() - 2) {
                throw new EvalError("invalid dotted parameter list");
            }

            Expr restExpr = paramExprs.get(i + 1);
            if (!(restExpr instanceof SymbolExpr restSymbol) || restSymbol.name().equals(".")) {
                throw new EvalError("rest parameter name must be a symbol");
            }

            restParam = restSymbol.name();
            i++;
        }

        return new ParameterSpec(List.copyOf(params), restParam);
    }

    private Value evalAnd(List<Expr> args, Environment env) throws EvalError {
        Value result = BoolValue.TRUE;
        for (Expr arg : args) {
            result = eval(arg, env);
            if (!isTruthy(result)) {
                return result;
            }
        }
        return result;
    }

    private Value evalOr(List<Expr> args, Environment env) throws EvalError {
        for (Expr arg : args) {
            Value result = eval(arg, env);
            if (isTruthy(result)) {
                return result;
            }
        }
        return BoolValue.FALSE;
    }

    private Macro parseMacro(String name, Expr transformerExpr, Environment env) throws EvalError {
        if (!(transformerExpr instanceof ListExpr transformer)) {
            throw new EvalError("'define-syntax' expects a syntax-rules transformer");
        }

        List<Expr> transformerElements = transformer.elements();
        if (transformerElements.size() < 3
                || !(transformerElements.getFirst() instanceof SymbolExpr keyword)
                || !keyword.name().equals("syntax-rules")) {
            throw new EvalError("'define-syntax' expects a syntax-rules transformer");
        }

        Set<String> literals = parseMacroLiterals(transformerElements.get(1));
        List<MacroRule> rules = new ArrayList<>(transformerElements.size() - 2);
        for (int i = 2; i < transformerElements.size(); i++) {
            Expr ruleExpr = transformerElements.get(i);
            if (!(ruleExpr instanceof ListExpr ruleList)) {
                throw new EvalError("'syntax-rules' clauses must be lists");
            }

            List<Expr> ruleElements = ruleList.elements();
            if (ruleElements.size() != 2) {
                throw new EvalError("'syntax-rules' clauses must contain a pattern and template");
            }

            Expr pattern = ruleElements.getFirst();
            Expr template = ruleElements.get(1);
            Set<String> patternVariables = new HashSet<>();
            Set<String> repeatedVariables = new HashSet<>();
            collectPatternVariables(pattern, name, literals, patternVariables, repeatedVariables, false);
            rules.add(new MacroRule(
                    pattern,
                    template,
                    Set.copyOf(patternVariables),
                    Set.copyOf(repeatedVariables)));
        }

        if (rules.isEmpty()) {
            throw new EvalError("'syntax-rules' expects at least one clause");
        }
        return new Macro(name, Set.copyOf(literals), List.copyOf(rules), env);
    }

    private Set<String> parseMacroLiterals(Expr literalsExpr) throws EvalError {
        if (!(literalsExpr instanceof ListExpr literalsList)) {
            throw new EvalError("'syntax-rules' expects a literal identifier list");
        }

        Set<String> literals = new HashSet<>(literalsList.elements().size());
        for (Expr literalExpr : literalsList.elements()) {
            if (!(literalExpr instanceof SymbolExpr literalSymbol)) {
                throw new EvalError("'syntax-rules' literals must be symbols");
            }
            literals.add(literalSymbol.name());
        }
        return literals;
    }

    private void collectPatternVariables(
            Expr pattern,
            String macroName,
            Set<String> literals,
            Set<String> patternVariables,
            Set<String> repeatedVariables,
            boolean repeatedContext) {
        switch (pattern) {
            case SymbolExpr symbolExpr -> {
                String name = symbolExpr.name();
                if (!name.equals("...")
                        && !name.equals(macroName)
                        && !literals.contains(name)) {
                    patternVariables.add(name);
                    if (repeatedContext) {
                        repeatedVariables.add(name);
                    }
                }
            }
            case ListExpr listExpr -> {
                List<Expr> elements = listExpr.elements();
                for (int i = 0; i < elements.size(); i++) {
                    Expr element = elements.get(i);
                    boolean repeated = i + 1 < elements.size()
                            && isEllipsisExpr(elements.get(i + 1));
                    collectPatternVariables(
                            element,
                            macroName,
                            literals,
                            patternVariables,
                            repeatedVariables,
                            repeatedContext || repeated);
                    if (repeated) {
                        i++;
                    }
                }
            }
            default -> {
            }
        }
    }

    private Expr expandMacro(Macro macro, ListExpr invocation) throws EvalError {
        for (MacroRule rule : macro.rules()) {
            PatternBindings bindings = new PatternBindings();
            if (matchPattern(rule.pattern(), invocation, macro, bindings, false)) {
                return instantiateTemplate(
                        rule.template(),
                        macro,
                        rule,
                        bindings,
                        Map.of(),
                        null);
            }
        }
        throw new EvalError("no matching syntax-rules clause");
    }

    private boolean matchPattern(
            Expr pattern,
            Expr input,
            Macro macro,
            PatternBindings bindings,
            boolean repeatedContext) throws EvalError {
        return switch (pattern) {
            case IntExpr intExpr -> input instanceof IntExpr other
                    && intExpr.value() == other.value();
            case RationalExpr rationalExpr -> input instanceof RationalExpr other
                    && rationalExpr.numerator().equals(other.numerator())
                    && rationalExpr.denominator().equals(other.denominator());
            case InexactExpr inexactExpr -> input instanceof InexactExpr other
                    && inexactExpr.value() == other.value();
            case BoolExpr boolExpr -> input instanceof BoolExpr other
                    && boolExpr.value() == other.value();
            case StringExpr stringExpr -> input instanceof StringExpr other
                    && stringExpr.value().equals(other.value());
            case CharExpr charExpr -> input instanceof CharExpr other
                    && charExpr.value() == other.value();
            case SymbolExpr symbolExpr -> matchPatternSymbol(
                    symbolExpr,
                    input,
                    macro,
                    bindings,
                    repeatedContext);
            case ListExpr listExpr -> input instanceof ListExpr other
                    && matchPatternList(
                    listExpr.elements(),
                    other.elements(),
                    macro,
                    bindings,
                    repeatedContext);
            case CapturedSymbolExpr ignored -> false;
        };
    }

    private boolean matchPatternSymbol(
            SymbolExpr pattern,
            Expr input,
            Macro macro,
            PatternBindings bindings,
            boolean repeatedContext) {
        String name = pattern.name();
        if (name.equals("...")) {
            return false;
        }
        if (isPatternLiteral(name, macro)) {
            String identifierName = identifierName(input);
            return identifierName != null && identifierName.equals(name);
        }
        return bindings.bind(name, input, repeatedContext);
    }

    private boolean matchPatternList(
            List<Expr> patterns,
            List<Expr> inputs,
            Macro macro,
            PatternBindings bindings,
            boolean repeatedContext) throws EvalError {
        return matchPatternList(patterns, 0, inputs, 0, macro, bindings, repeatedContext);
    }

    private boolean matchPatternList(
            List<Expr> patterns,
            int patternIndex,
            List<Expr> inputs,
            int inputIndex,
            Macro macro,
            PatternBindings bindings,
            boolean repeatedContext) throws EvalError {
        if (patternIndex == patterns.size()) {
            return inputIndex == inputs.size();
        }

        if (patternIndex + 1 < patterns.size() && isEllipsisExpr(patterns.get(patternIndex + 1))) {
            Expr repeatedPattern = patterns.get(patternIndex);
            for (int consumed = 0; inputIndex + consumed <= inputs.size(); consumed++) {
                PatternBindings trial = bindings.copy();
                boolean matched = true;
                for (int i = 0; i < consumed; i++) {
                    if (!matchPattern(
                            repeatedPattern,
                            inputs.get(inputIndex + i),
                            macro,
                            trial,
                            true)) {
                        matched = false;
                        break;
                    }
                }
                if (matched && matchPatternList(
                        patterns,
                        patternIndex + 2,
                        inputs,
                        inputIndex + consumed,
                        macro,
                        trial,
                        repeatedContext)) {
                    bindings.replaceWith(trial);
                    return true;
                }
            }
            return false;
        }

        if (inputIndex >= inputs.size()) {
            return false;
        }
        if (!matchPattern(patterns.get(patternIndex), inputs.get(inputIndex), macro, bindings, repeatedContext)) {
            return false;
        }
        return matchPatternList(
                patterns,
                patternIndex + 1,
                inputs,
                inputIndex + 1,
                macro,
                bindings,
                repeatedContext);
    }

    private Expr instantiateTemplate(
            Expr template,
            Macro macro,
            MacroRule rule,
            PatternBindings bindings,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        return switch (template) {
            case IntExpr ignored -> template;
            case RationalExpr ignored -> template;
            case InexactExpr ignored -> template;
            case BoolExpr ignored -> template;
            case StringExpr ignored -> template;
            case CharExpr ignored -> template;
            case SymbolExpr symbolExpr -> instantiateTemplateSymbol(
                    symbolExpr,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex);
            case ListExpr listExpr -> instantiateTemplateList(
                    listExpr,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex);
            case CapturedSymbolExpr ignored -> template;
        };
    }

    private Expr instantiateTemplateSymbol(
            SymbolExpr symbolExpr,
            Macro macro,
            MacroRule rule,
            PatternBindings bindings,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        String name = symbolExpr.name();
        if (rule.repeatedVariables().contains(name)) {
            if (repetitionIndex == null) {
                throw new EvalError("repeated pattern variable used without ellipsis: " + name);
            }
            return bindings.repeated(name, repetitionIndex);
        }
        if (rule.patternVariables().contains(name)) {
            return bindings.single(name);
        }

        String renamed = lexicalRenames.get(name);
        if (renamed != null) {
            return new SymbolExpr(renamed, symbolExpr.loc());
        }
        if (isSyntaxKeyword(name) || macros.containsKey(name)) {
            return symbolExpr;
        }
        return new CapturedSymbolExpr(name, symbolExpr.loc(), macro.definitionEnv());
    }

    private Expr instantiateTemplateList(
            ListExpr listExpr,
            Macro macro,
            MacroRule rule,
            PatternBindings bindings,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        if (!listExpr.elements().isEmpty()
                && listExpr.elements().getFirst() instanceof SymbolExpr headSymbol
                && !rule.patternVariables().contains(headSymbol.name())
                && !rule.repeatedVariables().contains(headSymbol.name())
                && !lexicalRenames.containsKey(headSymbol.name())
                && headSymbol.name().equals("let")) {
            return instantiateTemplateLet(
                    listExpr,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex);
        }

        List<Expr> elements = listExpr.elements();
        List<Expr> expanded = new ArrayList<>(elements.size());
        for (int i = 0; i < elements.size(); i++) {
            Expr element = elements.get(i);
            if (i + 1 < elements.size() && isEllipsisExpr(elements.get(i + 1))) {
                int count = determineRepetitionCount(element, rule, bindings);
                for (int repetition = 0; repetition < count; repetition++) {
                    expanded.add(instantiateTemplate(
                            element,
                            macro,
                            rule,
                            bindings,
                            lexicalRenames,
                            repetition));
                }
                i++;
                continue;
            }
            expanded.add(instantiateTemplate(
                    element,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex));
        }
        return new ListExpr(List.copyOf(expanded), listExpr.loc());
    }

    private Expr instantiateTemplateLet(
            ListExpr template,
            Macro macro,
            MacroRule rule,
            PatternBindings bindings,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = template.elements();
        if (elements.size() < 3) {
            return instantiateTemplateListFallback(
                    template,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex);
        }

        Expr bindingExpr = elements.get(1);
        if (!(bindingExpr instanceof ListExpr bindingListExpr)) {
            return instantiateTemplateListFallback(
                    template,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex);
        }

        Map<String, String> innerRenames = new HashMap<>(lexicalRenames);
        List<Expr> rewrittenBindings = new ArrayList<>(bindingListExpr.elements().size());
        for (Expr rawBinding : bindingListExpr.elements()) {
            if (!(rawBinding instanceof ListExpr bindingList)) {
                return instantiateTemplateListFallback(
                        template,
                        macro,
                        rule,
                        bindings,
                        lexicalRenames,
                        repetitionIndex);
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2
                    || !(bindingElements.getFirst() instanceof SymbolExpr bindingName)
                    || rule.patternVariables().contains(bindingName.name())
                    || rule.repeatedVariables().contains(bindingName.name())) {
                return instantiateTemplateListFallback(
                        template,
                        macro,
                        rule,
                        bindings,
                        lexicalRenames,
                        repetitionIndex);
            }

            String freshName = freshMacroName(bindingName.name());
            innerRenames.put(bindingName.name(), freshName);
            rewrittenBindings.add(new ListExpr(List.of(
                    new SymbolExpr(freshName, bindingName.loc()),
                    instantiateTemplate(
                            bindingElements.get(1),
                            macro,
                            rule,
                            bindings,
                            lexicalRenames,
                            repetitionIndex)),
                    rawBinding.loc()));
        }

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(elements.getFirst());
        expanded.add(new ListExpr(List.copyOf(rewrittenBindings), bindingExpr.loc()));
        for (int i = 2; i < elements.size(); i++) {
            expanded.add(instantiateTemplate(
                    elements.get(i),
                    macro,
                    rule,
                    bindings,
                    innerRenames,
                    repetitionIndex));
        }
        return new ListExpr(List.copyOf(expanded), template.loc());
    }

    private Expr instantiateTemplateListFallback(
            ListExpr listExpr,
            Macro macro,
            MacroRule rule,
            PatternBindings bindings,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = listExpr.elements();
        List<Expr> expanded = new ArrayList<>(elements.size());
        for (int i = 0; i < elements.size(); i++) {
            Expr element = elements.get(i);
            if (i + 1 < elements.size() && isEllipsisExpr(elements.get(i + 1))) {
                int count = determineRepetitionCount(element, rule, bindings);
                for (int repetition = 0; repetition < count; repetition++) {
                    expanded.add(instantiateTemplate(
                            element,
                            macro,
                            rule,
                            bindings,
                            lexicalRenames,
                            repetition));
                }
                i++;
                continue;
            }
            expanded.add(instantiateTemplate(
                    element,
                    macro,
                    rule,
                    bindings,
                    lexicalRenames,
                    repetitionIndex));
        }
        return new ListExpr(List.copyOf(expanded), listExpr.loc());
    }

    private int determineRepetitionCount(Expr template, MacroRule rule, PatternBindings bindings)
            throws EvalError {
        Integer count = determineRepetitionCountOrNull(template, rule, bindings);
        if (count == null) {
            throw new EvalError("ellipsis template must include a repeated pattern variable");
        }
        return count;
    }

    private Integer determineRepetitionCountOrNull(Expr template, MacroRule rule, PatternBindings bindings)
            throws EvalError {
        return switch (template) {
            case SymbolExpr symbolExpr -> rule.repeatedVariables().contains(symbolExpr.name())
                    ? bindings.repeatedCount(symbolExpr.name())
                    : null;
            case ListExpr listExpr -> {
                Integer count = null;
                List<Expr> elements = listExpr.elements();
                for (int i = 0; i < elements.size(); i++) {
                    if (isEllipsisExpr(elements.get(i))) {
                        continue;
                    }
                    Integer nested = determineRepetitionCountOrNull(elements.get(i), rule, bindings);
                    if (nested == null) {
                        continue;
                    }
                    if (count != null && !count.equals(nested)) {
                        throw new EvalError("mismatched ellipsis lengths in template");
                    }
                    count = nested;
                }
                yield count;
            }
            default -> null;
        };
    }

    private boolean isPatternLiteral(String name, Macro macro) {
        return name.equals(macro.name()) || macro.literals().contains(name);
    }

    private String identifierName(Expr expr) {
        return switch (expr) {
            case SymbolExpr symbolExpr -> symbolExpr.name();
            case CapturedSymbolExpr symbolExpr -> symbolExpr.name();
            default -> null;
        };
    }

    private boolean isSyntaxKeyword(String name) {
        return switch (name) {
            case "define", "define-syntax", "define-record-type", "set!", "if", "quote",
                    "lambda", "begin", "let", "cond", "and", "or", "else", "." -> true;
            default -> false;
        };
    }

    private boolean isEllipsisExpr(Expr expr) {
        return expr instanceof SymbolExpr symbolExpr && symbolExpr.name().equals("...");
    }

    private String freshMacroName(String original) {
        macroExpansionCounter++;
        return "__macro_" + macroExpansionCounter + "_" + original;
    }

    private Value quoteToValue(Expr expr) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new IntValue(intExpr.value());
            case RationalExpr rationalExpr -> exactFractionToValue(
                    new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator()));
            case InexactExpr inexactExpr -> new InexactValue(inexactExpr.value());
            case BoolExpr boolExpr -> BoolValue.of(boolExpr.value());
            case StringExpr stringExpr -> new StringValue(stringExpr.value());
            case CharExpr charExpr -> new CharValue(charExpr.value());
            case SymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case CapturedSymbolExpr symbolExpr -> new SymbolValue(symbolExpr.name());
            case ListExpr listExpr -> {
                List<Value> values = new ArrayList<>(listExpr.elements().size());
                for (Expr element : listExpr.elements()) {
                    values.add(quoteToValue(element));
                }
                yield new ListValue(List.copyOf(values));
            }
        };
    }

    private Value apply(Value operator, List<Value> args) throws EvalError {
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        return switch (procedure) {
            case BuiltinProcedure builtin -> builtin.fn().apply(args);
            case ClosureValue closure -> applyClosure(closure, args);
        };
    }

    private Value applyClosure(ClosureValue closure, List<Value> args) throws EvalError {
        ParameterSpec params = closure.params();
        if (!params.accepts(args.size())) {
            throw new EvalError("wrong number of arguments");
        }

        Environment callEnv = new Environment(closure.env());
        for (int i = 0; i < params.fixedParams().size(); i++) {
            callEnv.define(params.fixedParams().get(i), args.get(i));
        }
        if (params.restParam() != null) {
            callEnv.define(
                    params.restParam(),
                    new ListValue(List.copyOf(args.subList(params.fixedParams().size(), args.size()))));
        }

        Value result = VoidValue.INSTANCE;
        for (Expr bodyExpr : closure.body()) {
            result = eval(bodyExpr, callEnv);
        }
        return result;
    }

    private Value builtinAdd(List<Value> args) throws EvalError {
        if (containsInexact(args)) {
            double sum = 0.0;
            for (Value arg : args) {
                sum += toInexactDouble(requireNumber(arg));
            }
            return new InexactValue(sum);
        }

        ExactFraction sum = ExactFraction.ZERO;
        for (Value arg : args) {
            sum = sum.add(toExactFraction(requireNumber(arg)));
        }
        return exactFractionToValue(sum);
    }

    private Value builtinSub(List<Value> args) throws EvalError {
        if (args.isEmpty()) {
            throw new EvalError("'-' expects at least 1 argument");
        }

        if (containsInexact(args)) {
            double result = toInexactDouble(requireNumber(args.getFirst()));
            if (args.size() == 1) {
                return new InexactValue(-result);
            }

            for (int i = 1; i < args.size(); i++) {
                result -= toInexactDouble(requireNumber(args.get(i)));
            }
            return new InexactValue(result);
        }

        ExactFraction result = toExactFraction(requireNumber(args.getFirst()));
        if (args.size() == 1) {
            return exactFractionToValue(result.negate());
        }

        for (int i = 1; i < args.size(); i++) {
            result = result.subtract(toExactFraction(requireNumber(args.get(i))));
        }
        return exactFractionToValue(result);
    }

    private Value builtinMul(List<Value> args) throws EvalError {
        if (containsInexact(args)) {
            double product = 1.0;
            for (Value arg : args) {
                product *= toInexactDouble(requireNumber(arg));
            }
            return new InexactValue(product);
        }

        ExactFraction product = ExactFraction.ONE;
        for (Value arg : args) {
            product = product.multiply(toExactFraction(requireNumber(arg)));
        }
        return exactFractionToValue(product);
    }

    private Value builtinDiv(List<Value> args) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'/' expects at least 2 arguments");
        }

        if (containsInexact(args)) {
            double result = toInexactDouble(requireNumber(args.getFirst()));
            for (int i = 1; i < args.size(); i++) {
                double divisor = toInexactDouble(requireNumber(args.get(i)));
                if (divisor == 0.0) {
                    throw new EvalError("division by zero");
                }
                result /= divisor;
            }
            return new InexactValue(result);
        }

        ExactFraction result = toExactFraction(requireNumber(args.getFirst()));
        for (int i = 1; i < args.size(); i++) {
            ExactFraction divisor = toExactFraction(requireNumber(args.get(i)));
            if (divisor.isZero()) {
                throw new EvalError("division by zero");
            }
            result = result.divide(divisor);
        }
        return exactFractionToValue(result);
    }

    private Value builtinAbs(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "abs");
        NumericValue value = requireNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return new InexactValue(Math.abs(inexactValue.value()));
        }
        return exactFractionToValue(toExactFraction(value).abs());
    }

    private Value builtinModulo(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "modulo");
        long dividend = requireInt(args.getFirst());
        long divisor = requireInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }

        long remainder = dividend % divisor;
        if (remainder != 0 && ((remainder < 0 && divisor > 0)
                || (remainder > 0 && divisor < 0))) {
            remainder += divisor;
        }
        return new IntValue(remainder);
    }

    private Value builtinRemainder(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "remainder");
        long dividend = requireInt(args.getFirst());
        long divisor = requireInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend % divisor);
    }

    private Value builtinQuotient(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "quotient");
        long dividend = requireInt(args.getFirst());
        long divisor = requireInt(args.get(1));
        if (divisor == 0) {
            throw new EvalError("division by zero");
        }
        return new IntValue(dividend / divisor);
    }

    private Value builtinMin(List<Value> args) throws EvalError {
        requireAtLeastArgCount(args.size(), 1, "min");
        NumericValue result = requireNumber(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            NumericValue current = requireNumber(args.get(i));
            if (compareNumbers(current, result) < 0) {
                result = current;
            }
        }
        return result;
    }

    private Value builtinMax(List<Value> args) throws EvalError {
        requireAtLeastArgCount(args.size(), 1, "max");
        NumericValue result = requireNumber(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            NumericValue current = requireNumber(args.get(i));
            if (compareNumbers(current, result) > 0) {
                result = current;
            }
        }
        return result;
    }

    private Value builtinExpt(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "expt");
        long base = requireInt(args.getFirst());
        long exponent = requireInt(args.get(1));
        if (exponent < 0) {
            throw new EvalError("'expt' expects a non-negative exponent");
        }

        long result = 1;
        long factor = base;
        long remaining = exponent;
        while (remaining > 0) {
            if ((remaining & 1L) != 0) {
                result *= factor;
            }
            factor *= factor;
            remaining >>= 1;
        }
        return new IntValue(result);
    }

    private Value builtinComparison(List<Value> args, Comparison comparison, String name)
            throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'" + name + "' expects at least 2 arguments");
        }

        NumericValue previous = requireNumber(args.getFirst());
        for (int i = 1; i < args.size(); i++) {
            NumericValue current = requireNumber(args.get(i));
            if (!comparison.test(compareNumbers(previous, current))) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private Value builtinNot(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "not");
        return BoolValue.of(!isTruthy(args.getFirst()));
    }

    private Value builtinZeroPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "zero?");
        return BoolValue.of(isZero(requireNumber(args.getFirst())));
    }

    private Value builtinPositivePredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "positive?");
        return BoolValue.of(compareNumberToZero(requireNumber(args.getFirst())) > 0);
    }

    private Value builtinNegativePredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "negative?");
        return BoolValue.of(compareNumberToZero(requireNumber(args.getFirst())) < 0);
    }

    private Value builtinOddPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "odd?");
        return BoolValue.of(requireExactInteger(args.getFirst(), "odd?")
                .abs()
                .remainder(BigInteger.TWO)
                .equals(BigInteger.ONE));
    }

    private Value builtinEvenPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "even?");
        return BoolValue.of(requireExactInteger(args.getFirst(), "even?")
                .abs()
                .remainder(BigInteger.TWO)
                .equals(BigInteger.ZERO));
    }

    private Value builtinCons(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "cons");
        Value tail = args.get(1);
        if (tail instanceof ListValue listValue) {
            List<Value> result = new ArrayList<>(listValue.elements().size() + 1);
            result.add(args.getFirst());
            result.addAll(listValue.elements());
            return new ListValue(List.copyOf(result));
        }
        return new PairValue(args.getFirst(), tail);
    }

    private Value builtinCar(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "car");
        if (args.getFirst() instanceof PairValue pairValue) {
            return pairValue.car();
        }
        return requireNonEmptyList(args.getFirst(), "car").getFirst();
    }

    private Value builtinCdr(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "cdr");
        if (args.getFirst() instanceof PairValue pairValue) {
            return pairValue.cdr();
        }
        List<Value> elements = requireNonEmptyList(args.getFirst(), "cdr");
        return new ListValue(List.copyOf(elements.subList(1, elements.size())));
    }

    private Value builtinNull(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "null?");
        return BoolValue.of(args.getFirst() instanceof ListValue listValue
                && listValue.elements().isEmpty());
    }

    private Value builtinList(List<Value> args) {
        return new ListValue(List.copyOf(args));
    }

    private Value builtinLength(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "length");
        return new IntValue(requireList(args.getFirst(), "length").size());
    }

    private Value builtinAppend(List<Value> args) throws EvalError {
        List<Value> result = new ArrayList<>();
        for (Value arg : args) {
            result.addAll(requireList(arg, "append"));
        }
        return new ListValue(List.copyOf(result));
    }

    private Value builtinApply(List<Value> args) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, "apply");

        List<Value> appliedArgs = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) {
            appliedArgs.add(args.get(i));
        }
        appliedArgs.addAll(requireList(args.getLast(), "apply"));
        return apply(args.getFirst(), appliedArgs);
    }

    private Value builtinListRef(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "list-ref");
        List<Value> list = requireList(args.getFirst(), "list-ref");
        int index = requireElementIndex(args.get(1), list.size(), "list-ref");
        return list.get(index);
    }

    private Value builtinListTail(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "list-tail");
        List<Value> list = requireList(args.getFirst(), "list-tail");
        int index = requireListTailIndex(args.get(1), list.size(), "list-tail");
        return new ListValue(List.copyOf(list.subList(index, list.size())));
    }

    private Value builtinListPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "list?");
        return BoolValue.of(args.getFirst() instanceof ListValue);
    }

    private Value builtinAssoc(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "assoc");
        Value key = args.getFirst();
        List<Value> alist = requireList(args.get(1), "assoc");
        for (Value entry : alist) {
            Value entryKey = switch (entry) {
                case PairValue pairValue -> pairValue.car();
                case ListValue listValue -> {
                    if (listValue.elements().isEmpty()) {
                        throw new EvalError("'assoc' expects pairs");
                    }
                    yield listValue.elements().getFirst();
                }
                default -> throw new EvalError("'assoc' expects pairs");
            };

            if (valuesEqual(key, entryKey)) {
                return entry;
            }
        }
        return BoolValue.FALSE;
    }

    private Value builtinMap(List<Value> args) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, "map");
        Value procedure = args.getFirst();
        List<List<Value>> lists = new ArrayList<>(args.size() - 1);
        int limit = Integer.MAX_VALUE;
        for (int i = 1; i < args.size(); i++) {
            List<Value> list = requireList(args.get(i), "map");
            lists.add(list);
            limit = Math.min(limit, list.size());
        }

        List<Value> result = new ArrayList<>(limit);
        for (int i = 0; i < limit; i++) {
            List<Value> callArgs = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                callArgs.add(list.get(i));
            }
            result.add(apply(procedure, callArgs));
        }
        return new ListValue(List.copyOf(result));
    }

    private Value builtinStringPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string?");
        return BoolValue.of(args.getFirst() instanceof StringValue);
    }

    private Value builtinNumberPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "number?");
        return BoolValue.of(args.getFirst() instanceof NumericValue);
    }

    private Value builtinExactPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "exact?");
        return BoolValue.of(isExactNumber(args.getFirst()));
    }

    private Value builtinInexactPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "inexact?");
        return BoolValue.of(args.getFirst() instanceof InexactValue);
    }

    private Value builtinIntegerPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "integer?");
        Value value = args.getFirst();
        return BoolValue.of(value instanceof NumericValue number && isIntegerValue(number));
    }

    private Value builtinRationalPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "rational?");
        return BoolValue.of(valueIsRational(args.getFirst()));
    }

    private Value builtinExactToInexact(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "exact->inexact");
        return new InexactValue(toInexactDouble(requireNumber(args.getFirst())));
    }

    private Value builtinInexactToExact(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "inexact->exact");
        NumericValue value = requireNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return exactFractionToValue(ExactFraction.fromBigDecimal(BigDecimal.valueOf(inexactValue.value())));
        }
        return value;
    }

    private Value builtinNumerator(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "numerator");
        return bigIntegerToExactValue(requireRational(args.getFirst(), "numerator").numerator());
    }

    private Value builtinDenominator(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "denominator");
        return bigIntegerToExactValue(requireRational(args.getFirst(), "denominator").denominator());
    }

    private Value builtinBooleanPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "boolean?");
        return BoolValue.of(args.getFirst() instanceof BoolValue);
    }

    private Value builtinPairPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "pair?");
        return BoolValue.of(args.getFirst() instanceof PairValue
                || args.getFirst() instanceof ListValue listValue && !listValue.elements().isEmpty());
    }

    private Value builtinSymbolPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "symbol?");
        return BoolValue.of(args.getFirst() instanceof SymbolValue);
    }

    private Value builtinEq(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "eq?");
        return BoolValue.of(isEq(args.getFirst(), args.get(1)));
    }

    private Value builtinEqual(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "equal?");
        return BoolValue.of(valuesEqual(args.getFirst(), args.get(1)));
    }

    private Value builtinDisplay(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "display");
        appendOutput(args.getFirst().toDisplayString());
        return VoidValue.INSTANCE;
    }

    private Value builtinWrite(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "write");
        appendOutput(args.getFirst().toSchemeString());
        return VoidValue.INSTANCE;
    }

    private Value builtinNewline(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 0, "newline");
        appendOutput("\n");
        return VoidValue.INSTANCE;
    }

    private Value builtinStringAppend(List<Value> args) throws EvalError {
        StringBuilder builder = new StringBuilder();
        for (Value arg : args) {
            builder.append(requireString(arg, "string-append"));
        }
        return new StringValue(builder.toString());
    }

    private Value builtinStringLength(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string-length");
        return new IntValue(requireString(args.getFirst(), "string-length").length());
    }

    private Value builtinStringEquals(List<Value> args) throws EvalError {
        return builtinStringComparison(args, "string=?", (left, right) -> left.equals(right));
    }

    private Value builtinStringLessThan(List<Value> args) throws EvalError {
        return builtinStringComparison(args, "string<?", (left, right) -> left.compareTo(right) < 0);
    }

    private Value builtinStringCaseInsensitiveEquals(List<Value> args) throws EvalError {
        return builtinStringComparison(args, "string-ci=?",
                (left, right) -> left.equalsIgnoreCase(right));
    }

    private Value builtinStringUpcase(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string-upcase");
        return new StringValue(requireString(args.getFirst(), "string-upcase")
                .toUpperCase(Locale.ROOT));
    }

    private Value builtinStringDowncase(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string-downcase");
        return new StringValue(requireString(args.getFirst(), "string-downcase")
                .toLowerCase(Locale.ROOT));
    }

    private Value builtinSubstring(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 3, "substring");
        String value = requireString(args.getFirst(), "substring");
        int start = requireSubstringIndex(args.get(1), value.length(), "substring");
        int end = requireSubstringIndex(args.get(2), value.length(), "substring");
        if (end < start) {
            throw new EvalError("'substring' expects start <= end");
        }
        return new StringValue(value.substring(start, end));
    }

    private Value builtinStringToNumber(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string->number");
        try {
            Value parsed = parseNumberLiteralValue(requireString(args.getFirst(), "string->number"));
            if (parsed == null) {
                return BoolValue.FALSE;
            }
            return parsed;
        } catch (EvalError err) {
            return BoolValue.FALSE;
        }
    }

    private Value builtinNumberToString(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "number->string");
        return new StringValue(requireNumber(args.getFirst()).toSchemeString());
    }

    private Value builtinSymbolToString(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "symbol->string");
        if (args.getFirst() instanceof SymbolValue symbolValue) {
            return new StringValue(symbolValue.name());
        }
        throw new EvalError("'symbol->string' expects a symbol");
    }

    private Value builtinStringToSymbol(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string->symbol");
        return new SymbolValue(requireString(args.getFirst(), "string->symbol"));
    }

    private Value builtinStringCopy(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string-copy");
        return requireStringValue(args.getFirst(), "string-copy").mutableCopy();
    }

    private Value builtinStringRef(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "string-ref");
        String value = requireString(args.getFirst(), "string-ref");
        int index = requireElementIndex(args.get(1), value.length(), "string-ref");
        return new CharValue(value.charAt(index));
    }

    private Value builtinStringSet(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 3, "string-set!");
        StringValue value = requireMutableString(args.getFirst(), "string-set!");
        int index = requireElementIndex(args.get(1), value.length(), "string-set!");
        value.set(index, requireChar(args.get(2), "string-set!"));
        return VoidValue.INSTANCE;
    }

    private Value builtinCharPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "char?");
        return BoolValue.of(args.getFirst() instanceof CharValue);
    }

    private Value builtinCharAlphabeticPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "char-alphabetic?");
        return BoolValue.of(Character.isLetter(requireChar(args.getFirst(), "char-alphabetic?")));
    }

    private Value builtinCharNumericPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "char-numeric?");
        return BoolValue.of(Character.isDigit(requireChar(args.getFirst(), "char-numeric?")));
    }

    private Value builtinCharUpcase(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "char-upcase");
        return new CharValue(Character.toUpperCase(requireChar(args.getFirst(), "char-upcase")));
    }

    private Value builtinCharDowncase(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "char-downcase");
        return new CharValue(Character.toLowerCase(requireChar(args.getFirst(), "char-downcase")));
    }

    private Value builtinCharComparison(List<Value> args, Comparison comparison, String name)
            throws EvalError {
        requireAtLeastArgCount(args.size(), 2, name);
        char previous = requireChar(args.getFirst(), name);
        for (int i = 1; i < args.size(); i++) {
            char current = requireChar(args.get(i), name);
            if (!comparison.test(Character.compare(previous, current))) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private EvalError attachPosition(EvalError err, Expr expr) {
        if (err.hasPosition()) {
            return err;
        }
        return err.withPosition(expr.loc().line(), expr.loc().column());
    }

    private void requireArgCount(int actual, int expected, String procedure) throws EvalError {
        if (actual != expected) {
            throw new EvalError("'" + procedure + "' expects exactly "
                    + expected + " argument" + (expected == 1 ? "" : "s"));
        }
    }

    private void requireAtLeastArgCount(int actual, int expected, String procedure)
            throws EvalError {
        if (actual < expected) {
            throw new EvalError("'" + procedure + "' expects at least "
                    + expected + " argument" + (expected == 1 ? "" : "s"));
        }
    }

    private long requireInt(Value value) throws EvalError {
        try {
            return requireExactInteger(value, null).longValueExact();
        } catch (ArithmeticException ex) {
            throw new EvalError("expected integer");
        }
    }

    private NumericValue requireNumber(Value value) throws EvalError {
        if (value instanceof NumericValue numberValue) {
            return numberValue;
        }
        throw new EvalError("expected number");
    }

    private boolean containsInexact(List<Value> values) throws EvalError {
        for (Value value : values) {
            if (requireNumber(value) instanceof InexactValue) {
                return true;
            }
        }
        return false;
    }

    private boolean isExactNumber(Value value) {
        return value instanceof NumericValue && !(value instanceof InexactValue);
    }

    private boolean valueIsRational(Value value) {
        return switch (value) {
            case IntValue ignored -> true;
            case RationalValue ignored -> true;
            case InexactValue inexactValue -> Double.isFinite(inexactValue.value());
            default -> false;
        };
    }

    private boolean isIntegerValue(NumericValue value) {
        return switch (value) {
            case IntValue ignored -> true;
            case RationalValue rationalValue -> rationalValue.denominator().equals(BigInteger.ONE);
            case InexactValue inexactValue -> Double.isFinite(inexactValue.value())
                    && inexactValue.value() == Math.rint(inexactValue.value());
        };
    }

    private ExactFraction requireRational(Value value, String procedure) throws EvalError {
        NumericValue number = requireNumber(value);
        if (number instanceof InexactValue inexactValue) {
            if (!Double.isFinite(inexactValue.value())) {
                throw new EvalError("'" + procedure + "' expects a finite number");
            }
            return ExactFraction.fromBigDecimal(BigDecimal.valueOf(inexactValue.value()));
        }
        return toExactFraction(number);
    }

    private BigInteger requireExactInteger(Value value, String procedure) throws EvalError {
        NumericValue number = requireNumber(value);
        if (number instanceof InexactValue) {
            if (procedure == null) {
                throw new EvalError("expected integer");
            }
            throw new EvalError("'" + procedure + "' expects an exact integer");
        }

        ExactFraction exact = toExactFraction(number);
        if (!exact.isInteger()) {
            if (procedure == null) {
                throw new EvalError("expected integer");
            }
            throw new EvalError("'" + procedure + "' expects an exact integer");
        }
        return exact.numerator();
    }

    private ExactFraction toExactFraction(NumericValue value) {
        return switch (value) {
            case IntValue intValue -> ExactFraction.of(BigInteger.valueOf(intValue.value()));
            case RationalValue rationalValue ->
                    new ExactFraction(rationalValue.numerator(), rationalValue.denominator());
            case InexactValue ignored ->
                    throw new IllegalStateException("cannot convert inexact value to exact fraction");
        };
    }

    private double toInexactDouble(NumericValue value) {
        return switch (value) {
            case IntValue intValue -> intValue.value();
            case RationalValue rationalValue ->
                    rationalValue.numerator().doubleValue() / rationalValue.denominator().doubleValue();
            case InexactValue inexactValue -> inexactValue.value();
        };
    }

    private int compareNumbers(NumericValue left, NumericValue right) {
        if (left instanceof InexactValue || right instanceof InexactValue) {
            return compareInexact(toInexactDouble(left), toInexactDouble(right));
        }
        return toExactFraction(left).compareTo(toExactFraction(right));
    }

    private int compareInexact(double left, double right) {
        if (left == right) {
            return 0;
        }
        return left < right ? -1 : 1;
    }

    private int compareNumberToZero(NumericValue value) {
        if (value instanceof InexactValue inexactValue) {
            if (inexactValue.value() == 0.0) {
                return 0;
            }
            return inexactValue.value() < 0.0 ? -1 : 1;
        }
        return toExactFraction(value).signum();
    }

    private boolean isZero(NumericValue value) {
        if (value instanceof InexactValue inexactValue) {
            return inexactValue.value() == 0.0;
        }
        return toExactFraction(value).isZero();
    }

    private static Value exactFractionToValue(ExactFraction fraction) {
        if (fraction.isInteger()) {
            return bigIntegerToExactValue(fraction.numerator());
        }
        return new RationalValue(fraction.numerator(), fraction.denominator());
    }

    private static Value bigIntegerToExactValue(BigInteger value) {
        try {
            return new IntValue(value.longValueExact());
        } catch (ArithmeticException ex) {
            return new RationalValue(value, BigInteger.ONE);
        }
    }

    private String requireString(Value value, String procedure) throws EvalError {
        return requireStringValue(value, procedure).value();
    }

    private StringValue requireStringValue(Value value, String procedure) throws EvalError {
        if (value instanceof StringValue stringValue) {
            return stringValue;
        }
        throw new EvalError("'" + procedure + "' expects a string");
    }

    private StringValue requireMutableString(Value value, String procedure) throws EvalError {
        StringValue stringValue = requireStringValue(value, procedure);
        if (stringValue.isMutable()) {
            return stringValue;
        }
        throw new EvalError("'" + procedure + "' expects a mutable string");
    }

    private char requireChar(Value value, String procedure) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError("'" + procedure + "' expects a character");
    }

    private List<Value> requireList(Value value, String procedure) throws EvalError {
        if (value instanceof ListValue listValue) {
            return listValue.elements();
        }
        throw new EvalError("'" + procedure + "' expects a list");
    }

    private int requireElementIndex(Value value, int size, String procedure) throws EvalError {
        long index = requireInt(value);
        if (index < 0 || index >= size) {
            throw new EvalError("'" + procedure + "' index out of range");
        }
        return Math.toIntExact(index);
    }

    private int requireSubstringIndex(Value value, int size, String procedure) throws EvalError {
        long index = requireInt(value);
        if (index < 0 || index > size) {
            throw new EvalError("'" + procedure + "' index out of range");
        }
        return Math.toIntExact(index);
    }

    private int requireListTailIndex(Value value, int size, String procedure) throws EvalError {
        long index = requireInt(value);
        if (index < 0 || index > size) {
            throw new EvalError("'" + procedure + "' index out of range");
        }
        return Math.toIntExact(index);
    }

    private List<Value> requireNonEmptyList(Value value, String procedure) throws EvalError {
        List<Value> elements = requireList(value, procedure);
        if (!elements.isEmpty()) {
            return elements;
        }
        throw new EvalError("'" + procedure + "' expects a non-empty list");
    }

    private RecordValue requireRecordOfType(Value value, RecordType expectedType, String procedure)
            throws EvalError {
        if (value instanceof RecordValue recordValue && recordValue.type() == expectedType) {
            return recordValue;
        }
        throw new EvalError("'" + procedure + "' expects a " + expectedType.name());
    }

    private boolean isTruthy(Value value) {
        return !(value instanceof BoolValue boolValue) || boolValue.value();
    }

    private boolean isEq(Value left, Value right) {
        if (left == right) {
            return true;
        }
        return switch (left) {
            case IntValue intValue when right instanceof IntValue other ->
                    intValue.value() == other.value();
            case RationalValue rationalValue when right instanceof RationalValue other ->
                    rationalValue.numerator().equals(other.numerator())
                            && rationalValue.denominator().equals(other.denominator());
            case InexactValue inexactValue when right instanceof InexactValue other ->
                    inexactValue.value() == other.value();
            case BoolValue boolValue when right instanceof BoolValue other ->
                    boolValue.value() == other.value();
            case CharValue charValue when right instanceof CharValue other ->
                    charValue.value() == other.value();
            case SymbolValue symbolValue when right instanceof SymbolValue other ->
                    symbolValue.name().equals(other.name());
            case VoidValue ignored when right instanceof VoidValue -> true;
            default -> false;
        };
    }

    private boolean valuesEqual(Value left, Value right) {
        if (isEq(left, right)) {
            return true;
        }

        if (left instanceof NumericValue leftNumber && right instanceof NumericValue rightNumber) {
            return compareNumbers(leftNumber, rightNumber) == 0;
        }

        return switch (left) {
            case StringValue stringValue when right instanceof StringValue other ->
                    stringValue.value().equals(other.value());
            case ListValue listValue when right instanceof ListValue other ->
                    listElementsEqual(listValue.elements(), other.elements());
            case PairValue pairValue when right instanceof PairValue other ->
                    valuesEqual(pairValue.car(), other.car())
                            && valuesEqual(pairValue.cdr(), other.cdr());
            default -> false;
        };
    }

    private boolean listElementsEqual(List<Value> left, List<Value> right) {
        if (left.size() != right.size()) {
            return false;
        }
        for (int i = 0; i < left.size(); i++) {
            if (!valuesEqual(left.get(i), right.get(i))) {
                return false;
            }
        }
        return true;
    }

    private Value builtinStringComparison(
            List<Value> args,
            String procedure,
            StringRelation relation) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, procedure);
        String previous = requireString(args.getFirst(), procedure);
        for (int i = 1; i < args.size(); i++) {
            String current = requireString(args.get(i), procedure);
            if (!relation.test(previous, current)) {
                return BoolValue.FALSE;
            }
            previous = current;
        }
        return BoolValue.TRUE;
    }

    private void appendOutput(String text) {
        if (currentOutput != null) {
            currentOutput.append(text);
        }
    }

    private static boolean exprSyntaxEquals(Expr left, Expr right) {
        if (left == right) {
            return true;
        }
        return switch (left) {
            case IntExpr intExpr when right instanceof IntExpr other ->
                    intExpr.value() == other.value();
            case RationalExpr rationalExpr when right instanceof RationalExpr other ->
                    rationalExpr.numerator().equals(other.numerator())
                            && rationalExpr.denominator().equals(other.denominator());
            case InexactExpr inexactExpr when right instanceof InexactExpr other ->
                    inexactExpr.value() == other.value();
            case BoolExpr boolExpr when right instanceof BoolExpr other ->
                    boolExpr.value() == other.value();
            case StringExpr stringExpr when right instanceof StringExpr other ->
                    stringExpr.value().equals(other.value());
            case CharExpr charExpr when right instanceof CharExpr other ->
                    charExpr.value() == other.value();
            case SymbolExpr symbolExpr when right instanceof SymbolExpr other ->
                    symbolExpr.name().equals(other.name());
            case SymbolExpr symbolExpr when right instanceof CapturedSymbolExpr other ->
                    symbolExpr.name().equals(other.name());
            case CapturedSymbolExpr symbolExpr when right instanceof SymbolExpr other ->
                    symbolExpr.name().equals(other.name());
            case CapturedSymbolExpr symbolExpr when right instanceof CapturedSymbolExpr other ->
                    symbolExpr.name().equals(other.name());
            case ListExpr listExpr when right instanceof ListExpr other -> {
                if (listExpr.elements().size() != other.elements().size()) {
                    yield false;
                }
                boolean matches = true;
                for (int i = 0; i < listExpr.elements().size(); i++) {
                    if (!exprSyntaxEquals(listExpr.elements().get(i), other.elements().get(i))) {
                        matches = false;
                        break;
                    }
                }
                yield matches;
            }
            default -> false;
        };
    }

    private enum Comparison {
        LT {
            @Override
            boolean test(int ordering) {
                return ordering < 0;
            }
        },
        GT {
            @Override
            boolean test(int ordering) {
                return ordering > 0;
            }
        },
        EQ {
            @Override
            boolean test(int ordering) {
                return ordering == 0;
            }
        },
        LE {
            @Override
            boolean test(int ordering) {
                return ordering <= 0;
            }
        };

        abstract boolean test(int ordering);
    }

    private record ExactFraction(BigInteger numerator, BigInteger denominator)
            implements Comparable<ExactFraction> {
        private static final ExactFraction ZERO =
                new ExactFraction(BigInteger.ZERO, BigInteger.ONE);
        private static final ExactFraction ONE =
                new ExactFraction(BigInteger.ONE, BigInteger.ONE);

        private ExactFraction {
            if (denominator.signum() == 0) {
                throw new IllegalArgumentException("rational denominator cannot be zero");
            }

            if (denominator.signum() < 0) {
                numerator = numerator.negate();
                denominator = denominator.negate();
            }

            BigInteger gcd = numerator.gcd(denominator);
            if (!gcd.equals(BigInteger.ONE)) {
                numerator = numerator.divide(gcd);
                denominator = denominator.divide(gcd);
            }
        }

        private static ExactFraction of(BigInteger integer) {
            return new ExactFraction(integer, BigInteger.ONE);
        }

        private static ExactFraction fromBigDecimal(BigDecimal value) {
            BigInteger unscaled = value.unscaledValue();
            int scale = value.scale();
            if (scale >= 0) {
                return new ExactFraction(unscaled, BigInteger.TEN.pow(scale));
            }
            return new ExactFraction(unscaled.multiply(BigInteger.TEN.pow(-scale)), BigInteger.ONE);
        }

        private boolean isZero() {
            return numerator.signum() == 0;
        }

        private boolean isInteger() {
            return denominator.equals(BigInteger.ONE);
        }

        private int signum() {
            return numerator.signum();
        }

        private ExactFraction add(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.denominator)
                            .add(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator));
        }

        private ExactFraction subtract(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.denominator)
                            .subtract(other.numerator.multiply(denominator)),
                    denominator.multiply(other.denominator));
        }

        private ExactFraction multiply(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.numerator),
                    denominator.multiply(other.denominator));
        }

        private ExactFraction divide(ExactFraction other) {
            return new ExactFraction(
                    numerator.multiply(other.denominator),
                    denominator.multiply(other.numerator));
        }

        private ExactFraction negate() {
            return new ExactFraction(numerator.negate(), denominator);
        }

        private ExactFraction abs() {
            if (numerator.signum() >= 0) {
                return this;
            }
            return new ExactFraction(numerator.abs(), denominator);
        }

        @Override
        public int compareTo(ExactFraction other) {
            return numerator.multiply(other.denominator)
                    .compareTo(other.numerator.multiply(denominator));
        }
    }

    private record SourcePos(int line, int column) {
    }

    private sealed interface Expr permits IntExpr, RationalExpr, InexactExpr, BoolExpr, StringExpr,
            CharExpr, SymbolExpr, CapturedSymbolExpr, ListExpr {
        SourcePos loc();
    }

    private record IntExpr(long value, SourcePos loc) implements Expr {
    }

    private record RationalExpr(BigInteger numerator, BigInteger denominator, SourcePos loc)
            implements Expr {
    }

    private record InexactExpr(double value, SourcePos loc) implements Expr {
    }

    private record BoolExpr(boolean value, SourcePos loc) implements Expr {
    }

    private record StringExpr(String value, SourcePos loc) implements Expr {
    }

    private record CharExpr(char value, SourcePos loc) implements Expr {
    }

    private record SymbolExpr(String name, SourcePos loc) implements Expr {
    }

    private record CapturedSymbolExpr(String name, SourcePos loc, Environment env) implements Expr {
    }

    private record ListExpr(List<Expr> elements, SourcePos loc) implements Expr {
    }

    private record Binding(String name, Expr valueExpr) {
    }

    private record FieldSpec(String fieldName, String accessorName) {
    }

    private record Macro(
            String name,
            Set<String> literals,
            List<MacroRule> rules,
            Environment definitionEnv) {
    }

    private record MacroRule(
            Expr pattern,
            Expr template,
            Set<String> patternVariables,
            Set<String> repeatedVariables) {
    }

    private static final class PatternBindings {
        private final Map<String, Expr> single = new HashMap<>();
        private final Map<String, List<Expr>> repeated = new HashMap<>();

        private boolean bind(String name, Expr value, boolean repeatedContext) {
            if (repeatedContext) {
                repeated.computeIfAbsent(name, ignored -> new ArrayList<>()).add(value);
                return true;
            }

            Expr existing = single.get(name);
            if (existing == null) {
                single.put(name, value);
                return true;
            }
            return exprSyntaxEquals(existing, value);
        }

        private Expr single(String name) throws EvalError {
            Expr value = single.get(name);
            if (value == null) {
                throw new EvalError("missing pattern binding: " + name);
            }
            return value;
        }

        private Expr repeated(String name, int index) throws EvalError {
            List<Expr> values = repeated.get(name);
            if (values == null || index < 0 || index >= values.size()) {
                throw new EvalError("missing repeated pattern binding: " + name);
            }
            return values.get(index);
        }

        private int repeatedCount(String name) {
            List<Expr> values = repeated.get(name);
            return values == null ? 0 : values.size();
        }

        private PatternBindings copy() {
            PatternBindings copy = new PatternBindings();
            copy.single.putAll(single);
            for (Map.Entry<String, List<Expr>> entry : repeated.entrySet()) {
                copy.repeated.put(entry.getKey(), new ArrayList<>(entry.getValue()));
            }
            return copy;
        }

        private void replaceWith(PatternBindings other) {
            single.clear();
            single.putAll(other.single);
            repeated.clear();
            for (Map.Entry<String, List<Expr>> entry : other.repeated.entrySet()) {
                repeated.put(entry.getKey(), new ArrayList<>(entry.getValue()));
            }
        }
    }

    private record ParameterSpec(List<String> fixedParams, String restParam) {
        private boolean accepts(int argCount) {
            if (restParam == null) {
                return argCount == fixedParams.size();
            }
            return argCount >= fixedParams.size();
        }
    }

    private sealed interface Value permits NumericValue, BoolValue, StringValue, SymbolValue,
            CharValue, ListValue, PairValue, RecordValue, ProcedureValue, VoidValue {
        String toSchemeString();

        default String toDisplayString() {
            return toSchemeString();
        }
    }

    private sealed interface NumericValue extends Value permits IntValue, RationalValue, InexactValue {
    }

    private sealed interface ProcedureValue extends Value permits BuiltinProcedure, ClosureValue {
    }

    private record IntValue(long value) implements NumericValue {
        @Override
        public String toSchemeString() {
            return Long.toString(value);
        }
    }

    private record RationalValue(BigInteger numerator, BigInteger denominator) implements NumericValue {
        private RationalValue {
            ExactFraction normalized = new ExactFraction(numerator, denominator);
            numerator = normalized.numerator();
            denominator = normalized.denominator();
        }

        @Override
        public String toSchemeString() {
            if (denominator.equals(BigInteger.ONE)) {
                return numerator.toString();
            }
            return numerator + "/" + denominator;
        }
    }

    private record InexactValue(double value) implements NumericValue {
        @Override
        public String toSchemeString() {
            return Double.toString(value);
        }
    }

    private record BoolValue(boolean value) implements Value {
        private static final BoolValue TRUE = new BoolValue(true);
        private static final BoolValue FALSE = new BoolValue(false);

        private static BoolValue of(boolean value) {
            return value ? TRUE : FALSE;
        }

        @Override
        public String toSchemeString() {
            return value ? "#t" : "#f";
        }
    }

    private static final class StringValue implements Value {
        private final StringBuilder value;
        private final boolean mutable;

        private StringValue(String value) {
            this(value, false);
        }

        private StringValue(String value, boolean mutable) {
            this.value = new StringBuilder(value);
            this.mutable = mutable;
        }

        private String value() {
            return value.toString();
        }

        private int length() {
            return value.length();
        }

        private boolean isMutable() {
            return mutable;
        }

        private void set(int index, char ch) {
            value.setCharAt(index, ch);
        }

        private StringValue mutableCopy() {
            return new StringValue(value(), true);
        }

        @Override
        public String toSchemeString() {
            return quoteString(value());
        }

        @Override
        public String toDisplayString() {
            return value();
        }
    }

    private record SymbolValue(String name) implements Value {
        @Override
        public String toSchemeString() {
            return name;
        }
    }

    private record CharValue(char value) implements Value {
        @Override
        public String toSchemeString() {
            return charToSchemeString(value);
        }

        @Override
        public String toDisplayString() {
            return Character.toString(value);
        }
    }

    private record ListValue(List<Value> elements) implements Value {
        @Override
        public String toSchemeString() {
            if (elements.isEmpty()) {
                return "()";
            }

            StringBuilder builder = new StringBuilder();
            builder.append('(');
            for (int i = 0; i < elements.size(); i++) {
                if (i > 0) {
                    builder.append(' ');
                }
                builder.append(elements.get(i).toSchemeString());
            }
            builder.append(')');
            return builder.toString();
        }
    }

    private record PairValue(Value car, Value cdr) implements Value {
        @Override
        public String toSchemeString() {
            StringBuilder builder = new StringBuilder();
            builder.append('(');
            appendPairContents(builder, this);
            builder.append(')');
            return builder.toString();
        }
    }

    private static final class RecordType {
        private final String name;

        private RecordType(String name) {
            this.name = name;
        }

        private String name() {
            return name;
        }
    }

    private record RecordValue(RecordType type, List<Value> fields) implements Value {
        @Override
        public String toSchemeString() {
            return "#<record:" + type.name() + ">";
        }
    }

    private record BuiltinProcedure(String name, BuiltinFn fn) implements ProcedureValue {
        @Override
        public String toSchemeString() {
            return "#<procedure:" + name + ">";
        }
    }

    private record ClosureValue(String name, ParameterSpec params, List<Expr> body, Environment env)
            implements ProcedureValue {
        @Override
        public String toSchemeString() {
            if (name == null) {
                return "#<procedure>";
            }
            return "#<procedure:" + name + ">";
        }
    }

    private enum VoidValue implements Value {
        INSTANCE;

        @Override
        public String toSchemeString() {
            return "";
        }
    }

    @FunctionalInterface
    private interface BuiltinFn {
        Value apply(List<Value> args) throws EvalError;
    }

    @FunctionalInterface
    private interface StringRelation {
        boolean test(String left, String right);
    }

    private static final class Environment {
        private final Environment parent;
        private final Map<String, Value> bindings = new HashMap<>();

        private Environment(Environment parent) {
            this.parent = parent;
        }

        private void define(String name, Value value) {
            bindings.put(name, value);
        }

        private Value lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) {
                return bindings.get(name);
            }
            if (parent != null) {
                return parent.lookup(name);
            }
            throw new EvalError("unbound variable: " + name);
        }

        private void set(String name, Value value) throws EvalError {
            if (bindings.containsKey(name)) {
                bindings.put(name, value);
                return;
            }
            if (parent != null) {
                parent.set(name, value);
                return;
            }
            throw new EvalError("unbound variable: " + name);
        }
    }

    private static String quoteString(String value) {
        StringBuilder builder = new StringBuilder();
        builder.append('"');
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
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

    private static void appendPairContents(StringBuilder builder, PairValue pair) {
        builder.append(pair.car().toSchemeString());
        Value tail = pair.cdr();
        if (tail instanceof PairValue nextPair) {
            builder.append(' ');
            appendPairContents(builder, nextPair);
            return;
        }
        if (tail instanceof ListValue listValue) {
            for (Value element : listValue.elements()) {
                builder.append(' ');
                builder.append(element.toSchemeString());
            }
            return;
        }
        builder.append(" . ");
        builder.append(tail.toSchemeString());
    }

    private static String charToSchemeString(char value) {
        return switch (value) {
            case ' ' -> "#\\space";
            case '\n' -> "#\\newline";
            default -> "#\\" + value;
        };
    }

    private static Value parseNumberLiteralValue(String text) throws EvalError {
        ExactFraction rational = parseRationalLiteral(text);
        if (rational != null) {
            return exactFractionToValue(rational);
        }

        Double inexact = parseInexactLiteral(text);
        if (inexact != null) {
            return new InexactValue(inexact);
        }

        Long integer = parseIntegerLiteral(text);
        if (integer != null) {
            return new IntValue(integer);
        }
        if (isIntegerLiteral(text)) {
            throw new EvalError("invalid integer literal: " + text);
        }
        return null;
    }

    private static Expr numberValueToExpr(Value value, SourcePos loc) {
        return switch (value) {
            case IntValue intValue -> new IntExpr(intValue.value(), loc);
            case RationalValue rationalValue ->
                    new RationalExpr(rationalValue.numerator(), rationalValue.denominator(), loc);
            case InexactValue inexactValue -> new InexactExpr(inexactValue.value(), loc);
            default -> throw new IllegalArgumentException("not a numeric value");
        };
    }

    private static Long parseIntegerLiteral(String text) {
        if (!isIntegerLiteral(text)) {
            return null;
        }
        try {
            return Long.parseLong(text);
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private static ExactFraction parseRationalLiteral(String text) throws EvalError {
        int slashIndex = text.indexOf('/');
        if (slashIndex <= 0 || slashIndex != text.lastIndexOf('/') || slashIndex == text.length() - 1) {
            return null;
        }

        String numeratorText = text.substring(0, slashIndex);
        String denominatorText = text.substring(slashIndex + 1);
        if (!isIntegerLiteral(numeratorText) || !isIntegerLiteral(denominatorText)) {
            return null;
        }

        BigInteger numerator = new BigInteger(numeratorText);
        BigInteger denominator = new BigInteger(denominatorText);
        if (denominator.signum() == 0) {
            throw new EvalError("invalid rational literal: " + text);
        }
        return new ExactFraction(numerator, denominator);
    }

    private static Double parseInexactLiteral(String text) throws EvalError {
        if (!isInexactLiteral(text)) {
            return null;
        }

        try {
            return Double.parseDouble(text);
        } catch (NumberFormatException ex) {
            throw new EvalError("invalid inexact literal: " + text);
        }
    }

    private static boolean isIntegerLiteral(String text) {
        if (text.isEmpty()) {
            return false;
        }
        int start = (text.charAt(0) == '+' || text.charAt(0) == '-') ? 1 : 0;
        if (start == text.length()) {
            return false;
        }
        for (int i = start; i < text.length(); i++) {
            if (!Character.isDigit(text.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isInexactLiteral(String text) {
        if (text.isEmpty()) {
            return false;
        }

        int start = (text.charAt(0) == '+' || text.charAt(0) == '-') ? 1 : 0;
        if (start == text.length()) {
            return false;
        }

        boolean sawDot = false;
        boolean sawDigit = false;
        for (int i = start; i < text.length(); i++) {
            char ch = text.charAt(i);
            if (ch == '.') {
                if (sawDot) {
                    return false;
                }
                sawDot = true;
                continue;
            }
            if (!Character.isDigit(ch)) {
                return false;
            }
            sawDigit = true;
        }

        return sawDot && sawDigit;
    }

    private static Character parseCharacterLiteral(String text) {
        if (!text.startsWith("#\\")) {
            return null;
        }

        String literal = text.substring(2);
        return switch (literal) {
            case "space" -> ' ';
            case "newline" -> '\n';
            default -> literal.length() == 1 ? literal.charAt(0) : null;
        };
    }

    private static final class Parser {
        private final String input;
        private int index;
        private int line = 1;
        private int column = 1;

        private Parser(String input) {
            this.input = input;
        }

        private List<Expr> parseProgram() throws EvalError {
            List<Expr> expressions = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd()) {
                expressions.add(parseExpr());
                skipIgnored();
            }
            return expressions;
        }

        private Expr parseExpr() throws EvalError {
            skipIgnored();
            if (isAtEnd()) {
                throw new EvalError("unexpected end of input", line, column);
            }

            SourcePos start = currentPosition();
            char ch = currentChar();
            return switch (ch) {
                case '(' -> parseList(start);
                case '\'' -> parseQuoteAbbreviation(start);
                case '"' -> parseString(start);
                case ')' -> throw new EvalError("unexpected ')'", start.line(), start.column());
                default -> parseAtom(start);
            };
        }

        private Expr parseQuoteAbbreviation(SourcePos start) throws EvalError {
            advance();
            return new ListExpr(List.of(new SymbolExpr("quote", start), parseExpr()), start);
        }

        private Expr parseList(SourcePos start) throws EvalError {
            advance();
            List<Expr> elements = new ArrayList<>();
            skipIgnored();
            while (!isAtEnd() && currentChar() != ')') {
                elements.add(parseExpr());
                skipIgnored();
            }
            if (isAtEnd()) {
                throw new EvalError("unterminated list", start.line(), start.column());
            }
            advance();
            return new ListExpr(List.copyOf(elements), start);
        }

        private Expr parseString(SourcePos start) throws EvalError {
            advance();
            StringBuilder builder = new StringBuilder();
            while (!isAtEnd()) {
                char ch = advance();
                if (ch == '"') {
                    return new StringExpr(builder.toString(), start);
                }
                if (ch == '\\') {
                    if (isAtEnd()) {
                        throw new EvalError("unterminated string", start.line(), start.column());
                    }
                    char escaped = advance();
                    switch (escaped) {
                        case 'n' -> builder.append('\n');
                        case 'r' -> builder.append('\r');
                        case 't' -> builder.append('\t');
                        case '\\' -> builder.append('\\');
                        case '"' -> builder.append('"');
                        default -> builder.append(escaped);
                    }
                } else {
                    builder.append(ch);
                }
            }
            throw new EvalError("unterminated string", start.line(), start.column());
        }

        private Expr parseAtom(SourcePos start) throws EvalError {
            int atomStartIndex = index;
            while (!isAtEnd()) {
                char ch = currentChar();
                if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == '\'' || ch == ';') {
                    break;
                }
                advance();
            }

            String atom = input.substring(atomStartIndex, index);
            if (atom.isEmpty()) {
                throw new EvalError("unexpected token", start.line(), start.column());
            }
            if (atom.equals("#t")) {
                return new BoolExpr(true, start);
            }
            if (atom.equals("#f")) {
                return new BoolExpr(false, start);
            }
            Character charValue = parseCharacterLiteral(atom);
            if (charValue != null) {
                return new CharExpr(charValue, start);
            }
            if (atom.startsWith("#\\")) {
                throw new EvalError("invalid character literal: " + atom,
                        start.line(), start.column());
            }

            try {
                Value numericValue = parseNumberLiteralValue(atom);
                if (numericValue != null) {
                    return numberValueToExpr(numericValue, start);
                }
            } catch (EvalError err) {
                throw new EvalError(err.getMessage(), start.line(), start.column());
            }

            return new SymbolExpr(atom, start);
        }

        private void skipIgnored() {
            while (!isAtEnd()) {
                char ch = currentChar();
                if (Character.isWhitespace(ch)) {
                    advance();
                    continue;
                }
                if (ch == ';') {
                    while (!isAtEnd() && currentChar() != '\n') {
                        advance();
                    }
                    continue;
                }
                break;
            }
        }

        private SourcePos currentPosition() {
            return new SourcePos(line, column);
        }

        private boolean isAtEnd() {
            return index >= input.length();
        }

        private char currentChar() {
            return input.charAt(index);
        }

        private char advance() {
            char ch = input.charAt(index++);
            if (ch == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            return ch;
        }
    }
}
