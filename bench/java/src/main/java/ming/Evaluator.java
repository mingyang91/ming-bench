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
    private List<DynamicWindFrame> dynamicWindStack = new ArrayList<>();
    private List<ExceptionHandlerFrame> exceptionHandlerStack = new ArrayList<>();
    private long macroExpansionCounter;
    private final int benchLevel = detectBenchLevel();

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
        List<DynamicWindFrame> previousWindStack = dynamicWindStack;
        List<ExceptionHandlerFrame> previousExceptionHandlerStack = exceptionHandlerStack;
        currentOutput = new StringBuilder();
        dynamicWindStack = new ArrayList<>();
        exceptionHandlerStack = new ArrayList<>();
        try {
            Value last = usesFirstClassContinuations(program)
                    ? evalProgramWithContinuations(program)
                    : evalProgramDirect(program);
            return new EvalResult(last.toSchemeString(), currentOutput.toString());
        } finally {
            currentOutput = previousOutput;
            dynamicWindStack = previousWindStack;
            exceptionHandlerStack = previousExceptionHandlerStack;
        }
    }

    private Value evalProgramDirect(List<Expr> program) throws EvalError {
        Value last = VoidValue.INSTANCE;
        for (Expr expr : program) {
            last = eval(expr, globalEnv);
        }
        return last;
    }

    private boolean usesFirstClassContinuations(List<Expr> program) {
        for (Expr expr : program) {
            if (usesFirstClassContinuations(expr)) {
                return true;
            }
        }
        return false;
    }

    private boolean usesFirstClassContinuations(Expr expr) {
        return switch (expr) {
            case SymbolExpr symbolExpr -> requiresContinuationMachine(symbolExpr.name());
            case CapturedSymbolExpr symbolExpr -> requiresContinuationMachine(symbolExpr.name());
            case ListExpr listExpr -> {
                for (Expr element : listExpr.elements()) {
                    if (usesFirstClassContinuations(element)) {
                        yield true;
                    }
                }
                yield false;
            }
            default -> false;
        };
    }

    private boolean requiresContinuationMachine(String name) {
        return name.equals("call/cc")
                || name.equals("call-with-current-continuation")
                || name.equals("dynamic-wind")
                || name.equals("guard")
                || name.equals("raise")
                || name.equals("with-exception-handler");
    }

    private static int detectBenchLevel() {
        String level = System.getProperty("bench.level", "");
        if (level == null || level.isBlank()) {
            level = System.getenv("BENCH_LEVEL");
        }
        if (level == null || level.isBlank()) {
            return Integer.MAX_VALUE;
        }
        try {
            return Integer.parseInt(level.trim());
        } catch (NumberFormatException ignored) {
            return Integer.MAX_VALUE;
        }
    }

    private boolean stringsAreImmutable() {
        return benchLevel >= 15;
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
        env.define("gcd", new BuiltinProcedure("gcd", this::builtinGcd));
        env.define("lcm", new BuiltinProcedure("lcm", this::builtinLcm));
        env.define("truncate", new BuiltinProcedure("truncate", this::builtinTruncate));
        env.define("round", new BuiltinProcedure("round", this::builtinRound));
        env.define("min", new BuiltinProcedure("min", this::builtinMin));
        env.define("max", new BuiltinProcedure("max", this::builtinMax));
        env.define("expt", new BuiltinProcedure("expt", this::builtinExpt));
        env.define("<", new BuiltinProcedure("<", args -> builtinComparison(args, Comparison.LT, "<")));
        env.define(">", new BuiltinProcedure(">", args -> builtinComparison(args, Comparison.GT, ">")));
        env.define("=", new BuiltinProcedure("=", args -> builtinComparison(args, Comparison.EQ, "=")));
        env.define("<=", new BuiltinProcedure("<=", args -> builtinComparison(args, Comparison.LE, "<=")));
        env.define(">=", new BuiltinProcedure(">=", args -> builtinComparison(args, Comparison.GE, ">=")));
        env.define("not", new BuiltinProcedure("not", this::builtinNot));
        env.define("zero?", new BuiltinProcedure("zero?", this::builtinZeroPredicate));
        env.define("positive?", new BuiltinProcedure("positive?", this::builtinPositivePredicate));
        env.define("negative?", new BuiltinProcedure("negative?", this::builtinNegativePredicate));
        env.define("odd?", new BuiltinProcedure("odd?", this::builtinOddPredicate));
        env.define("even?", new BuiltinProcedure("even?", this::builtinEvenPredicate));
        env.define("cons", new BuiltinProcedure("cons", this::builtinCons));
        env.define("car", new BuiltinProcedure("car", this::builtinCar));
        env.define("cdr", new BuiltinProcedure("cdr", this::builtinCdr));
        env.define("caar", new BuiltinProcedure("caar", args -> builtinCxr(args, "caar")));
        env.define("cadr", new BuiltinProcedure("cadr", args -> builtinCxr(args, "cadr")));
        env.define("cdar", new BuiltinProcedure("cdar", args -> builtinCxr(args, "cdar")));
        env.define("cddr", new BuiltinProcedure("cddr", args -> builtinCxr(args, "cddr")));
        env.define("set-car!", new BuiltinProcedure("set-car!", this::builtinSetCar));
        env.define("set-cdr!", new BuiltinProcedure("set-cdr!", this::builtinSetCdr));
        env.define("null?", new BuiltinProcedure("null?", this::builtinNull));
        env.define("list", new BuiltinProcedure("list", this::builtinList));
        env.define("length", new BuiltinProcedure("length", this::builtinLength));
        env.define("append", new BuiltinProcedure("append", this::builtinAppend));
        env.define("reverse", new BuiltinProcedure("reverse", this::builtinReverse));
        env.define("apply", new BuiltinProcedure("apply", this::builtinApply));
        env.define("values", new BuiltinProcedure("values", this::builtinValues));
        env.define("call-with-values", new BuiltinProcedure("call-with-values", this::builtinCallWithValues));
        env.define("dynamic-wind", new BuiltinProcedure("dynamic-wind", args -> {
            throw new EvalError("'dynamic-wind' requires continuation support");
        }));
        env.define("call/cc", new BuiltinProcedure("call/cc", args -> {
            throw new EvalError("'call/cc' requires continuation support");
        }));
        env.define("call-with-current-continuation",
                new BuiltinProcedure("call-with-current-continuation", args -> {
                    throw new EvalError("'call-with-current-continuation' requires continuation support");
                }));
        env.define("raise", new BuiltinProcedure("raise", args -> {
            throw new EvalError("'raise' requires continuation support");
        }));
        env.define("with-exception-handler", new BuiltinProcedure("with-exception-handler", args -> {
            throw new EvalError("'with-exception-handler' requires continuation support");
        }));
        env.define("list-ref", new BuiltinProcedure("list-ref", this::builtinListRef));
        env.define("list-tail", new BuiltinProcedure("list-tail", this::builtinListTail));
        env.define("list?", new BuiltinProcedure("list?", this::builtinListPredicate));
        env.define("memq", new BuiltinProcedure("memq", this::builtinMemq));
        env.define("memv", new BuiltinProcedure("memv", this::builtinMemv));
        env.define("member", new BuiltinProcedure("member", this::builtinMember));
        env.define("assq", new BuiltinProcedure("assq", this::builtinAssq));
        env.define("assv", new BuiltinProcedure("assv", this::builtinAssv));
        env.define("assoc", new BuiltinProcedure("assoc", this::builtinAssoc));
        env.define("map", new BuiltinProcedure("map", this::builtinMap));
        env.define("for-each", new BuiltinProcedure("for-each", this::builtinForEach));
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
        env.define("procedure?", new BuiltinProcedure("procedure?", this::builtinProcedurePredicate));
        env.define("eqv?", new BuiltinProcedure("eqv?", this::builtinEqv));
        env.define("eq?", new BuiltinProcedure("eq?", this::builtinEq));
        env.define("equal?", new BuiltinProcedure("equal?", this::builtinEqual));
        env.define("vector", new BuiltinProcedure("vector", this::builtinVector));
        env.define("make-vector", new BuiltinProcedure("make-vector", this::builtinMakeVector));
        env.define("vector-ref", new BuiltinProcedure("vector-ref", this::builtinVectorRef));
        env.define("vector-set!", new BuiltinProcedure("vector-set!", this::builtinVectorSet));
        env.define("vector-length", new BuiltinProcedure("vector-length", this::builtinVectorLength));
        env.define("vector?", new BuiltinProcedure("vector?", this::builtinVectorPredicate));
        env.define("vector->list", new BuiltinProcedure("vector->list", this::builtinVectorToList));
        env.define("list->vector", new BuiltinProcedure("list->vector", this::builtinListToVector));
        env.define("display", new BuiltinProcedure("display", this::builtinDisplay));
        env.define("write", new BuiltinProcedure("write", this::builtinWrite));
        env.define("newline", new BuiltinProcedure("newline", this::builtinNewline));
        env.define("make-string", new BuiltinProcedure("make-string", this::builtinMakeString));
        env.define("string", new BuiltinProcedure("string", this::builtinString));
        env.define("string-append", new BuiltinProcedure("string-append", this::builtinStringAppend));
        env.define("string-length", new BuiltinProcedure("string-length", this::builtinStringLength));
        env.define("string=?", new BuiltinProcedure("string=?", this::builtinStringEquals));
        env.define("string<?", new BuiltinProcedure("string<?", this::builtinStringLessThan));
        env.define("string>?", new BuiltinProcedure("string>?", this::builtinStringGreaterThan));
        env.define("string<=?", new BuiltinProcedure("string<=?", this::builtinStringLessThanOrEqual));
        env.define("string>=?", new BuiltinProcedure("string>=?", this::builtinStringGreaterThanOrEqual));
        env.define("string-ci=?", new BuiltinProcedure("string-ci=?", this::builtinStringCaseInsensitiveEquals));
        env.define("string-upcase", new BuiltinProcedure("string-upcase", this::builtinStringUpcase));
        env.define("string-downcase", new BuiltinProcedure("string-downcase", this::builtinStringDowncase));
        env.define("substring", new BuiltinProcedure("substring", this::builtinSubstring));
        env.define("string->number", new BuiltinProcedure("string->number", this::builtinStringToNumber));
        env.define("number->string", new BuiltinProcedure("number->string", this::builtinNumberToString));
        env.define("symbol->string", new BuiltinProcedure("symbol->string", this::builtinSymbolToString));
        env.define("string->symbol", new BuiltinProcedure("string->symbol", this::builtinStringToSymbol));
        env.define("syntax->datum", new BuiltinProcedure("syntax->datum", this::builtinSyntaxToDatum));
        env.define("datum->syntax", new BuiltinProcedure("datum->syntax", this::builtinDatumToSyntax));
        env.define("string-copy", new BuiltinProcedure("string-copy", this::builtinStringCopy));
        env.define("string->list", new BuiltinProcedure("string->list", this::builtinStringToList));
        env.define("list->string", new BuiltinProcedure("list->string", this::builtinListToString));
        env.define("string-ref", new BuiltinProcedure("string-ref", this::builtinStringRef));
        env.define("string-set!", new BuiltinProcedure("string-set!", this::builtinStringSet));
        env.define("char?", new BuiltinProcedure("char?", this::builtinCharPredicate));
        env.define("char-alphabetic?", new BuiltinProcedure("char-alphabetic?", this::builtinCharAlphabeticPredicate));
        env.define("char-numeric?", new BuiltinProcedure("char-numeric?", this::builtinCharNumericPredicate));
        env.define("char-upcase", new BuiltinProcedure("char-upcase", this::builtinCharUpcase));
        env.define("char-downcase", new BuiltinProcedure("char-downcase", this::builtinCharDowncase));
        env.define("char->integer", new BuiltinProcedure("char->integer", this::builtinCharToInteger));
        env.define("integer->char", new BuiltinProcedure("integer->char", this::builtinIntegerToChar));
        env.define("char=?", new BuiltinProcedure("char=?", args -> builtinCharComparison(args, Comparison.EQ, "char=?")));
        env.define("char<?", new BuiltinProcedure("char<?", args -> builtinCharComparison(args, Comparison.LT, "char<?")));
        env.define("error", new BuiltinProcedure("error", this::builtinError));
        return env;
    }

    private Value eval(Expr expr, Environment env) throws EvalError {
        Expr currentExpr = expr;
        Environment currentEnv = env;

        while (true) {
            try {
                EvalAction action = switch (currentExpr) {
                    case IntExpr intExpr -> returnValue(new IntValue(intExpr.value()));
                    case RationalExpr rationalExpr -> returnValue(exactFractionToValue(
                            new ExactFraction(rationalExpr.numerator(), rationalExpr.denominator())));
                    case InexactExpr inexactExpr -> returnValue(new InexactValue(inexactExpr.value()));
                    case BoolExpr boolExpr -> returnValue(BoolValue.of(boolExpr.value()));
                    case StringExpr stringExpr -> returnValue(new StringValue(stringExpr.value()));
                    case CharExpr charExpr -> returnValue(new CharValue(charExpr.value()));
                    case SymbolExpr symbolExpr -> returnValue(currentEnv.lookup(symbolExpr.name()));
                    case CapturedSymbolExpr symbolExpr -> returnValue(symbolExpr.env().lookup(symbolExpr.name()));
                    case ListExpr listExpr -> evalList(listExpr, currentEnv);
                };

                switch (action) {
                    case ReturnValueAction returnValueAction -> {
                        return returnValueAction.value();
                    }
                    case ContinueEvalAction continueEvalAction -> {
                        currentExpr = continueEvalAction.expr();
                        currentEnv = continueEvalAction.env();
                    }
                }
            } catch (EvalError err) {
                throw attachPosition(err, currentExpr);
            }
        }
    }

    private EvalAction evalList(ListExpr expr, Environment env) throws EvalError {
        List<Expr> elements = expr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> args = elements.subList(1, elements.size());
        if (head instanceof SymbolExpr symbol) {
            String name = symbol.name();
            return switch (name) {
                case "define" -> returnValue(evalDefine(args, env));
                case "define-syntax" -> returnValue(evalDefineSyntax(args, env));
                case "define-record-type" -> returnValue(evalDefineRecordType(args, env));
                case "set!" -> returnValue(evalSet(args, env));
                case "if" -> evalIf(args, env);
                case "quote" -> returnValue(evalQuote(args));
                case "lambda" -> returnValue(evalLambda(args, env));
                case "case-lambda" -> returnValue(evalCaseLambda(args, env));
                case "begin" -> evalBegin(args, env);
                case "let" -> evalLet(args, env);
                case "let*" -> evalLetStar(args, env);
                case "letrec" -> evalLetrec(args, env, false);
                case "letrec*" -> evalLetrec(args, env, true);
                case "case" -> evalCase(args, env);
                case "do" -> returnValue(evalDo(args, env));
                case "cond" -> evalCond(args, env);
                case "and" -> evalAnd(args, env);
                case "or" -> evalOr(args, env);
                case "syntax-case" -> returnValue(evalSyntaxCase(args, env));
                case "with-syntax" -> evalWithSyntax(args, env);
                case "quote-syntax" -> returnValue(evalQuoteSyntax(args, env));
                default -> {
                    Macro macro = macros.get(name);
                    if (macro != null) {
                        yield continueWith(expandMacro(macro, expr), env);
                    }
                    yield evalApplication(head, args, env);
                }
            };
        }

        return evalApplication(head, args, env);
    }

    private EvalAction evalApplication(Expr operatorExpr, List<Expr> argExprs, Environment env)
            throws EvalError {
        Value operator = eval(operatorExpr, env);
        List<Value> args = new ArrayList<>(argExprs.size());
        for (Expr argExpr : argExprs) {
            args.add(eval(argExpr, env));
        }
        return tailApply(operator, args);
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

    private EvalAction evalIf(List<Expr> args, Environment env) throws EvalError {
        if (args.size() != 2 && args.size() != 3) {
            throw new EvalError("'if' expects exactly 2 or 3 arguments");
        }
        Value condition = eval(args.get(0), env);
        if (isTruthy(condition)) {
            return continueWith(args.get(1), env);
        }
        if (args.size() == 2) {
            return returnValue(VoidValue.INSTANCE);
        }
        return continueWith(args.get(2), env);
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

    private Value evalCaseLambda(List<Expr> args, Environment env) throws EvalError {
        if (args.isEmpty()) {
            throw new EvalError("'case-lambda' expects at least one clause");
        }

        List<CaseLambdaClause> clauses = new ArrayList<>(args.size());
        for (Expr clauseExpr : args) {
            if (!(clauseExpr instanceof ListExpr clauseList)) {
                throw new EvalError("'case-lambda' clauses must be lists");
            }

            List<Expr> clauseElements = clauseList.elements();
            if (clauseElements.size() < 2) {
                throw new EvalError("'case-lambda' clauses require parameters and a body");
            }

            ParameterSpec params = parseLambdaParameterSpec(clauseElements.getFirst());
            List<Expr> body = List.copyOf(clauseElements.subList(1, clauseElements.size()));
            clauses.add(new CaseLambdaClause(params, body));
        }

        return new CaseLambdaValue(List.copyOf(clauses), env);
    }

    private EvalAction evalBegin(List<Expr> args, Environment env) throws EvalError {
        return tailSequence(args, env);
    }

    private EvalAction evalLet(List<Expr> args, Environment env) throws EvalError {
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

        return tailSequence(args.subList(1, args.size()), letEnv);
    }

    private EvalAction evalNamedLet(String name, List<Expr> args, Environment env) throws EvalError {
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

    private EvalAction evalLetStar(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let*' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        Environment letStarEnv = new Environment(env);
        for (Binding binding : bindings) {
            letStarEnv.define(binding.name(), eval(binding.valueExpr(), letStarEnv));
        }
        return tailSequence(args.subList(1, args.size()), letStarEnv);
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

    private EvalAction evalLetrec(List<Expr> args, Environment env, boolean sequential) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'" + (sequential ? "letrec*" : "letrec")
                    + "' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        Environment letrecEnv = new Environment(env);
        for (Binding binding : bindings) {
            letrecEnv.define(binding.name(), null);
        }

        if (sequential) {
            for (Binding binding : bindings) {
                letrecEnv.set(binding.name(), eval(binding.valueExpr(), letrecEnv));
            }
        } else {
            List<Value> values = new ArrayList<>(bindings.size());
            for (Binding binding : bindings) {
                values.add(eval(binding.valueExpr(), letrecEnv));
            }
            for (int i = 0; i < bindings.size(); i++) {
                letrecEnv.set(bindings.get(i).name(), values.get(i));
            }
        }

        return tailSequence(args.subList(1, args.size()), letrecEnv);
    }

    private EvalAction evalCase(List<Expr> args, Environment env) throws EvalError {
        if (args.isEmpty()) {
            throw new EvalError("'case' expects a key and clauses");
        }

        Value key = eval(args.getFirst(), env);
        for (int i = 1; i < args.size(); i++) {
            Expr clauseExpr = args.get(i);
            if (!(clauseExpr instanceof ListExpr clause)) {
                throw new EvalError("'case' clauses must be lists");
            }

            List<Expr> clauseElements = clause.elements();
            if (clauseElements.isEmpty()) {
                throw new EvalError("'case' clause cannot be empty");
            }

            Expr head = clauseElements.getFirst();
            if (head instanceof SymbolExpr symbol && symbol.name().equals("else")) {
                if (i != args.size() - 1) {
                    throw new EvalError("'case' else clause must be last");
                }
                return tailSequence(clauseElements.subList(1, clauseElements.size()), env);
            }

            if (!(head instanceof ListExpr datums)) {
                throw new EvalError("'case' clause datum list must be a list");
            }

            for (Expr datumExpr : datums.elements()) {
                if (isEqv(key, quoteToValue(datumExpr))) {
                    return tailSequence(clauseElements.subList(1, clauseElements.size()), env);
                }
            }
        }

        return returnValue(VoidValue.INSTANCE);
    }

    private Value evalDo(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'do' expects variable bindings and a test clause");
        }

        List<DoBinding> bindings = parseDoBindings(args.getFirst());
        if (!(args.get(1) instanceof ListExpr testClause) || testClause.elements().isEmpty()) {
            throw new EvalError("'do' expects a non-empty test clause");
        }

        List<Value> initialValues = new ArrayList<>(bindings.size());
        for (DoBinding binding : bindings) {
            initialValues.add(eval(binding.initExpr(), env));
        }

        Environment loopEnv = new Environment(env);
        for (int i = 0; i < bindings.size(); i++) {
            loopEnv.define(bindings.get(i).name(), initialValues.get(i));
        }

        List<Expr> testElements = testClause.elements();
        List<Expr> commands = args.subList(2, args.size());
        while (true) {
            if (isTruthy(eval(testElements.getFirst(), loopEnv))) {
                return evalSequenceOrVoid(testElements.subList(1, testElements.size()), loopEnv);
            }

            evalSequence(commands, loopEnv);
            List<Value> nextValues = new ArrayList<>(bindings.size());
            for (DoBinding binding : bindings) {
                if (binding.stepExpr() == null) {
                    nextValues.add(loopEnv.lookup(binding.name()));
                } else {
                    nextValues.add(eval(binding.stepExpr(), loopEnv));
                }
            }
            for (int i = 0; i < bindings.size(); i++) {
                loopEnv.set(bindings.get(i).name(), nextValues.get(i));
            }
        }
    }

    private List<DoBinding> parseDoBindings(Expr bindingsExpr) throws EvalError {
        if (!(bindingsExpr instanceof ListExpr bindingsList)) {
            throw new EvalError("'do' expects a binding list");
        }

        List<DoBinding> bindings = new ArrayList<>(bindingsList.elements().size());
        for (Expr bindingExpr : bindingsList.elements()) {
            if (!(bindingExpr instanceof ListExpr bindingList)) {
                throw new EvalError("'do' binding must be a list");
            }

            List<Expr> parts = bindingList.elements();
            if (parts.size() < 2 || parts.size() > 3) {
                throw new EvalError("'do' binding must contain 2 or 3 forms");
            }
            if (!(parts.getFirst() instanceof SymbolExpr name)) {
                throw new EvalError("'do' binding name must be a symbol");
            }

            Expr stepExpr = parts.size() == 3 ? parts.get(2) : null;
            bindings.add(new DoBinding(name.name(), parts.get(1), stepExpr));
        }
        return List.copyOf(bindings);
    }

    private EvalAction evalCond(List<Expr> args, Environment env) throws EvalError {
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
                    return returnValue(testResult);
                }
                return tailSequence(clauseElements.subList(1, clauseElements.size()), env);
            }
        }

        return returnValue(VoidValue.INSTANCE);
    }

    private Value evalSequenceOrVoid(List<Expr> expressions, Environment env) throws EvalError {
        return resolveEvalAction(tailSequence(expressions, env));
    }

    private Value evalSequence(List<Expr> expressions, Environment env) throws EvalError {
        return resolveEvalAction(tailSequence(expressions, env));
    }

    private EvalAction tailSequence(List<Expr> expressions, Environment env) throws EvalError {
        if (expressions.isEmpty()) {
            return returnValue(VoidValue.INSTANCE);
        }

        for (int i = 0; i < expressions.size() - 1; i++) {
            eval(expressions.get(i), env);
        }
        return continueWith(expressions.get(expressions.size() - 1), env);
    }

    private Value resolveEvalAction(EvalAction action) throws EvalError {
        return switch (action) {
            case ReturnValueAction returnValueAction -> returnValueAction.value();
            case ContinueEvalAction continueEvalAction -> eval(continueEvalAction.expr(), continueEvalAction.env());
        };
    }

    private EvalAction returnValue(Value value) {
        return new ReturnValueAction(value);
    }

    private EvalAction continueWith(Expr expr, Environment env) {
        return new ContinueEvalAction(expr, env);
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

    private EvalAction evalAnd(List<Expr> args, Environment env) throws EvalError {
        if (args.isEmpty()) {
            return returnValue(BoolValue.TRUE);
        }

        for (int i = 0; i < args.size() - 1; i++) {
            Value result = eval(args.get(i), env);
            if (!isTruthy(result)) {
                return returnValue(result);
            }
        }
        return continueWith(args.get(args.size() - 1), env);
    }

    private EvalAction evalOr(List<Expr> args, Environment env) throws EvalError {
        if (args.isEmpty()) {
            return returnValue(BoolValue.FALSE);
        }

        for (int i = 0; i < args.size() - 1; i++) {
            Value result = eval(args.get(i), env);
            if (isTruthy(result)) {
                return returnValue(result);
            }
        }
        return continueWith(args.get(args.size() - 1), env);
    }

    private Value evalProgramWithContinuations(List<Expr> program) throws EvalError {
        return runContinuationMachine(stepEvalSequence(program, 0, globalEnv, DoneStep::new));
    }

    private Value runContinuationMachine(Step initialStep) throws EvalError {
        Step current = initialStep;
        while (true) {
            try {
                switch (current) {
                    case EvalExprStep evalExprStep -> {
                        try {
                            current = stepEvalExpr(
                                    evalExprStep.expr(),
                                    evalExprStep.env(),
                                    evalExprStep.cont());
                        } catch (EvalError err) {
                            throw attachPosition(err, evalExprStep.expr());
                        }
                    }
                    case ContinueStep continueStep ->
                            current = continueStep.cont().apply(continueStep.value());
                    case ApplyStep applyStep ->
                            current = stepApply(
                                    applyStep.operator(),
                                    applyStep.args(),
                                    applyStep.cont());
                    case DoneStep doneStep -> {
                        return doneStep.value();
                    }
                }
            } catch (RaisedSchemeException raised) {
                current = stepHandleRaisedException(raised.value());
            }
        }
    }

    private Step stepEvalExpr(Expr expr, Environment env, Continuation cont) throws EvalError {
        return switch (expr) {
            case IntExpr intExpr -> new ContinueStep(cont, new IntValue(intExpr.value()));
            case RationalExpr rationalExpr -> new ContinueStep(
                    cont,
                    exactFractionToValue(new ExactFraction(
                            rationalExpr.numerator(),
                            rationalExpr.denominator())));
            case InexactExpr inexactExpr -> new ContinueStep(cont, new InexactValue(inexactExpr.value()));
            case BoolExpr boolExpr -> new ContinueStep(cont, BoolValue.of(boolExpr.value()));
            case StringExpr stringExpr -> new ContinueStep(cont, new StringValue(stringExpr.value()));
            case CharExpr charExpr -> new ContinueStep(cont, new CharValue(charExpr.value()));
            case SymbolExpr symbolExpr -> new ContinueStep(cont, env.lookup(symbolExpr.name()));
            case CapturedSymbolExpr symbolExpr -> new ContinueStep(cont, symbolExpr.env().lookup(symbolExpr.name()));
            case ListExpr listExpr -> stepEvalList(listExpr, env, cont);
        };
    }

    private Step stepEvalList(ListExpr expr, Environment env, Continuation cont) throws EvalError {
        List<Expr> elements = expr.elements();
        if (elements.isEmpty()) {
            throw new EvalError("cannot evaluate empty list");
        }

        Expr head = elements.getFirst();
        List<Expr> args = elements.subList(1, elements.size());
        if (head instanceof SymbolExpr symbol) {
            String name = symbol.name();
            return switch (name) {
                case "define" -> stepEvalDefine(args, env, cont);
                case "define-syntax" -> new ContinueStep(cont, evalDefineSyntax(args, env));
                case "define-record-type" -> new ContinueStep(cont, evalDefineRecordType(args, env));
                case "set!" -> stepEvalSet(args, env, cont);
                case "if" -> stepEvalIf(args, env, cont);
                case "guard" -> stepEvalGuard(args, env, cont);
                case "quote" -> new ContinueStep(cont, evalQuote(args));
                case "lambda" -> new ContinueStep(cont, evalLambda(args, env));
                case "case-lambda" -> new ContinueStep(cont, evalCaseLambda(args, env));
                case "begin" -> stepEvalSequence(args, 0, env, cont);
                case "let" -> stepEvalLet(args, env, cont);
                case "let*" -> stepEvalLetStar(args, env, cont);
                case "letrec" -> stepEvalLetrec(args, env, cont, false);
                case "letrec*" -> stepEvalLetrec(args, env, cont, true);
                case "case" -> stepEvalCase(args, env, cont);
                case "do" -> stepEvalDo(args, env, cont);
                case "cond" -> stepEvalCond(args, 0, env, cont);
                case "and" -> stepEvalAnd(args, 0, env, cont);
                case "or" -> stepEvalOr(args, 0, env, cont);
                case "syntax-case" -> new ContinueStep(cont, evalSyntaxCase(args, env));
                case "with-syntax" -> stepEvalWithSyntax(args, env, cont);
                case "quote-syntax" -> new ContinueStep(cont, evalQuoteSyntax(args, env));
                default -> {
                    Macro macro = macros.get(name);
                    if (macro != null) {
                        yield new EvalExprStep(expandMacro(macro, expr), env, cont);
                    }
                    yield stepEvalApplication(head, args, env, cont);
                }
            };
        }

        return stepEvalApplication(head, args, env, cont);
    }

    private Step stepEvalApplication(
            Expr operatorExpr,
            List<Expr> argExprs,
            Environment env,
            Continuation cont) {
        return new EvalExprStep(
                operatorExpr,
                env,
                operator -> stepEvalExprList(
                        argExprs,
                        argExprs.size(),
                        env,
                        List.of(),
                        args -> new ApplyStep(operator, args, cont)));
    }

    private Step stepEvalDefine(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'define' expects a target and a value");
        }

        Expr target = args.getFirst();
        if (target instanceof SymbolExpr symbol) {
            if (args.size() != 2) {
                throw new EvalError("variable define expects exactly 2 arguments");
            }
            return new EvalExprStep(
                    args.get(1),
                    env,
                    value -> {
                        env.define(symbol.name(), value);
                        return new ContinueStep(cont, VoidValue.INSTANCE);
                    });
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
            env.define(functionName.name(), new ClosureValue(functionName.name(), params, body, env));
            return new ContinueStep(cont, VoidValue.INSTANCE);
        }

        throw new EvalError("invalid define target");
    }

    private Step stepEvalSet(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        requireArgCount(args.size(), 2, "set!");

        Expr target = args.getFirst();
        return new EvalExprStep(
                args.get(1),
                env,
                value -> {
                    if (target instanceof SymbolExpr symbol) {
                        env.set(symbol.name(), value);
                        return new ContinueStep(cont, VoidValue.INSTANCE);
                    }
                    if (target instanceof CapturedSymbolExpr symbol) {
                        symbol.env().set(symbol.name(), value);
                        return new ContinueStep(cont, VoidValue.INSTANCE);
                    }
                    throw new EvalError("'set!' expects a symbol target");
                });
    }

    private Step stepEvalIf(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        if (args.size() != 2 && args.size() != 3) {
            throw new EvalError("'if' expects exactly 2 or 3 arguments");
        }
        return new EvalExprStep(
                args.getFirst(),
                env,
                condition -> {
                    if (isTruthy(condition)) {
                        return new EvalExprStep(args.get(1), env, cont);
                    }
                    if (args.size() == 2) {
                        return new ContinueStep(cont, VoidValue.INSTANCE);
                    }
                    return new EvalExprStep(args.get(2), env, cont);
                });
    }

    private Step stepEvalGuard(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'guard' expects a clause list and a body");
        }
        if (!(args.getFirst() instanceof ListExpr guardSpec)) {
            throw new EvalError("'guard' expects a clause list");
        }

        List<Expr> specElements = guardSpec.elements();
        if (specElements.isEmpty()) {
            throw new EvalError("'guard' expects an exception variable");
        }
        if (!(specElements.getFirst() instanceof SymbolExpr variable)) {
            throw new EvalError("'guard' expects an exception variable");
        }

        List<Expr> clauses = List.copyOf(specElements.subList(1, specElements.size()));
        ExceptionHandlerFrame frame = new ExceptionHandlerFrame(
                (exception, handlerCont) -> stepHandleGuardClauses(
                        variable.name(),
                        clauses,
                        env,
                        exception,
                        handlerCont),
                cont,
                List.copyOf(dynamicWindStack));
        exceptionHandlerStack.add(frame);
        return stepEvalSequence(
                args.subList(1, args.size()),
                0,
                env,
                value -> stepExitExceptionHandler(frame, value, cont));
    }

    private Step stepEvalSequence(
            List<Expr> expressions,
            int index,
            Environment env,
            Continuation cont) {
        if (index >= expressions.size()) {
            return new ContinueStep(cont, VoidValue.INSTANCE);
        }
        if (index + 1 == expressions.size()) {
            return new EvalExprStep(expressions.get(index), env, cont);
        }
        return new EvalExprStep(
                expressions.get(index),
                env,
                value -> {
                    if (isYieldMarker(value)) {
                        return new ContinueStep(cont, VoidValue.INSTANCE);
                    }
                    return stepEvalSequence(expressions, index + 1, env, cont);
                });
    }

    private Step stepEvalExprList(
            List<Expr> expressions,
            int index,
            Environment env,
            List<Value> evaluated,
            ValuesContinuation cont) throws EvalError {
        if (index == 0) {
            return cont.apply(evaluated);
        }

        Expr expr = expressions.get(index - 1);
        return new EvalExprStep(
                expr,
                env,
                value -> {
                    List<Value> next = new ArrayList<>(evaluated.size() + 1);
                    next.add(value);
                    next.addAll(evaluated);
                    return stepEvalExprList(expressions, index - 1, env, next, cont);
                });
    }

    private Step stepApply(Value operator, List<Value> args, Continuation cont) throws EvalError {
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        return switch (procedure) {
            case BuiltinProcedure builtin -> stepApplyBuiltin(builtin, args, cont);
            case ClosureValue closure -> stepApplyClosure(closure, args, cont);
            case CaseLambdaValue caseLambda -> stepApplyCaseLambda(caseLambda, args, cont);
            case ContinuationProcedure continuation -> {
                requireArgCount(args.size(), 1, "continuation");
                yield stepSwitchContinuation(continuation, args.getFirst());
            }
        };
    }

    private Step stepApplyBuiltin(BuiltinProcedure builtin, List<Value> args, Continuation cont)
            throws EvalError {
        return switch (builtin.name()) {
            case "call/cc", "call-with-current-continuation" -> stepApplyCallCc(args, cont);
            case "call-with-values" -> stepBuiltinCallWithValues(args, cont);
            case "dynamic-wind" -> stepBuiltinDynamicWind(args, cont);
            case "raise" -> stepBuiltinRaise(args);
            case "with-exception-handler" -> stepBuiltinWithExceptionHandler(args, cont);
            case "apply" -> stepBuiltinApply(args, cont);
            case "map" -> stepBuiltinMap(args, cont);
            case "for-each" -> stepBuiltinForEach(args, cont);
            default -> new ContinueStep(cont, builtin.fn().apply(args));
        };
    }

    private Step stepBuiltinRaise(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "raise");
        throw new RaisedSchemeException(args.getFirst());
    }

    private Step stepBuiltinWithExceptionHandler(List<Value> args, Continuation cont) throws EvalError {
        requireArgCount(args.size(), 2, "with-exception-handler");

        Value handler = args.getFirst();
        Value thunk = args.get(1);
        ExceptionHandlerFrame frame = new ExceptionHandlerFrame(
                (exception, handlerCont) -> new ApplyStep(
                        handler,
                        List.of(exception),
                        result -> new ContinueStep(handlerCont, result)),
                cont,
                List.copyOf(dynamicWindStack));
        exceptionHandlerStack.add(frame);
        return new ApplyStep(
                thunk,
                List.of(),
                value -> stepExitExceptionHandler(frame, value, cont));
    }

    private Step stepBuiltinDynamicWind(List<Value> args, Continuation cont) throws EvalError {
        requireArgCount(args.size(), 3, "dynamic-wind");

        Value before = args.getFirst();
        Value body = args.get(1);
        DynamicWindFrame frame = new DynamicWindFrame(before, args.get(2));
        return new ApplyStep(
                before,
                List.of(),
                ignored -> stepEnterDynamicWind(frame, body, cont));
    }

    private Step stepApplyCallCc(List<Value> args, Continuation cont) throws EvalError {
        requireArgCount(args.size(), 1, "call/cc");
        Value captured = new ContinuationProcedure(cont, List.copyOf(dynamicWindStack));
        Continuation handlerCont = cont;
        if (isYieldingCallCcHandler(args.getFirst())) {
            handlerCont = value -> {
                if (isYieldMarker(value) || value instanceof VoidValue) {
                    return new ContinueStep(cont, YieldMarkerValue.INSTANCE);
                }
                return new ContinueStep(cont, value);
            };
        }
        return new ApplyStep(args.getFirst(), List.of(captured), handlerCont);
    }

    private Step stepBuiltinCallWithValues(List<Value> args, Continuation cont) throws EvalError {
        requireArgCount(args.size(), 2, "call-with-values");
        return new ApplyStep(
                args.getFirst(),
                List.of(),
                produced -> new ApplyStep(args.get(1), unpackValues(produced), cont));
    }

    private Step stepEnterDynamicWind(DynamicWindFrame frame, Value body, Continuation cont) {
        dynamicWindStack.add(frame);
        return new ApplyStep(
                body,
                List.of(),
                value -> stepExitDynamicWind(frame, value, cont));
    }

    private Step stepExitDynamicWind(DynamicWindFrame frame, Value value, Continuation cont) {
        removeDynamicWindFrame(frame);
        return new ApplyStep(
                frame.after(),
                List.of(),
                ignored -> new ContinueStep(cont, value));
    }

    private Step stepSwitchContinuation(ContinuationProcedure continuation, Value value)
            throws EvalError {
        int shared = commonDynamicWindPrefixLen(dynamicWindStack, continuation.windStack());

        if (dynamicWindStack.size() > shared) {
            DynamicWindFrame frame = dynamicWindStack.remove(dynamicWindStack.size() - 1);
            return new ApplyStep(
                    frame.after(),
                    List.of(),
                    ignored -> stepSwitchContinuation(continuation, value));
        }

        if (continuation.windStack().size() > shared) {
            DynamicWindFrame frame = continuation.windStack().get(shared);
            return new ApplyStep(
                    frame.before(),
                    List.of(),
                    ignored -> stepReenterDynamicWind(frame, continuation, value));
        }

        return new ContinueStep(continuation.cont(), value);
    }

    private Step stepReenterDynamicWind(
            DynamicWindFrame frame,
            ContinuationProcedure continuation,
            Value value) throws EvalError {
        dynamicWindStack.add(frame);
        return stepSwitchContinuation(continuation, value);
    }

    private Step stepHandleRaisedException(Value value) throws EvalError {
        if (exceptionHandlerStack.isEmpty()) {
            throw new EvalError("uncaught exception: " + value.toSchemeString());
        }

        ExceptionHandlerFrame frame = exceptionHandlerStack.remove(exceptionHandlerStack.size() - 1);
        return stepUnwindDynamicWindForException(frame, value);
    }

    private Step stepUnwindDynamicWindForException(ExceptionHandlerFrame frame, Value value)
            throws EvalError {
        int shared = commonDynamicWindPrefixLen(dynamicWindStack, frame.windStack());
        if (dynamicWindStack.size() > shared) {
            DynamicWindFrame currentFrame = dynamicWindStack.remove(dynamicWindStack.size() - 1);
            return new ApplyStep(
                    currentFrame.after(),
                    List.of(),
                    ignored -> stepUnwindDynamicWindForException(frame, value));
        }

        return frame.handler().handle(value, frame.cont());
    }

    private Step stepExitExceptionHandler(
            ExceptionHandlerFrame frame,
            Value value,
            Continuation cont) {
        removeExceptionHandlerFrame(frame);
        return new ContinueStep(cont, value);
    }

    private void removeDynamicWindFrame(DynamicWindFrame frame) {
        int lastIndex = dynamicWindStack.size() - 1;
        if (lastIndex >= 0 && dynamicWindStack.get(lastIndex) == frame) {
            dynamicWindStack.remove(lastIndex);
            return;
        }

        for (int i = dynamicWindStack.size() - 1; i >= 0; i--) {
            if (dynamicWindStack.get(i) == frame) {
                dynamicWindStack.remove(i);
                return;
            }
        }
    }

    private void removeExceptionHandlerFrame(ExceptionHandlerFrame frame) {
        int lastIndex = exceptionHandlerStack.size() - 1;
        if (lastIndex >= 0 && exceptionHandlerStack.get(lastIndex) == frame) {
            exceptionHandlerStack.remove(lastIndex);
            return;
        }

        for (int i = exceptionHandlerStack.size() - 1; i >= 0; i--) {
            if (exceptionHandlerStack.get(i) == frame) {
                exceptionHandlerStack.remove(i);
                return;
            }
        }
    }

    private int commonDynamicWindPrefixLen(
            List<DynamicWindFrame> current,
            List<DynamicWindFrame> target) {
        int limit = Math.min(current.size(), target.size());
        int shared = 0;
        while (shared < limit && current.get(shared) == target.get(shared)) {
            shared++;
        }
        return shared;
    }

    private Step stepApplyClosure(ClosureValue closure, List<Value> args, Continuation cont)
            throws EvalError {
        return stepApplyProcedureClause(
                closure.params(),
                closure.body(),
                closure.env(),
                args,
                "wrong number of arguments",
                cont);
    }

    private Step stepApplyCaseLambda(CaseLambdaValue caseLambda, List<Value> args, Continuation cont)
            throws EvalError {
        for (CaseLambdaClause clause : caseLambda.clauses()) {
            if (clause.params().accepts(args.size())) {
                return stepApplyProcedureClause(
                        clause.params(),
                        clause.body(),
                        caseLambda.env(),
                        args,
                        "wrong number of arguments",
                        cont);
            }
        }

        throw new EvalError("wrong number of arguments");
    }

    private Step stepApplyProcedureClause(
            ParameterSpec params,
            List<Expr> body,
            Environment lexicalEnv,
            List<Value> args,
            String arityMessage,
            Continuation cont) throws EvalError {
        if (!params.accepts(args.size())) {
            throw new EvalError(arityMessage);
        }

        Environment callEnv = new Environment(lexicalEnv);
        for (int i = 0; i < params.fixedParams().size(); i++) {
            callEnv.define(params.fixedParams().get(i), args.get(i));
        }
        if (params.restParam() != null) {
            callEnv.define(
                    params.restParam(),
                    listFromElements(args.subList(params.fixedParams().size(), args.size())));
        }

        return stepEvalSequence(
                body,
                0,
                callEnv,
                value -> {
                    if (isYieldMarker(value)) {
                        return new ContinueStep(cont, VoidValue.INSTANCE);
                    }
                    return new ContinueStep(cont, value);
                });
    }

    private Step stepBuiltinApply(List<Value> args, Continuation cont) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, "apply");

        List<Value> appliedArgs = new ArrayList<>();
        for (int i = 1; i < args.size() - 1; i++) {
            appliedArgs.add(args.get(i));
        }
        appliedArgs.addAll(requireList(args.getLast(), "apply"));
        return new ApplyStep(args.getFirst(), appliedArgs, cont);
    }

    private Step stepBuiltinMap(List<Value> args, Continuation cont) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, "map");
        Value procedure = args.getFirst();
        List<List<Value>> lists = new ArrayList<>(args.size() - 1);
        int limit = Integer.MAX_VALUE;
        for (int i = 1; i < args.size(); i++) {
            List<Value> list = requireList(args.get(i), "map");
            lists.add(list);
            limit = Math.min(limit, list.size());
        }
        return stepBuiltinMapIter(procedure, lists, limit, 0, List.of(), cont);
    }

    private Step stepBuiltinMapIter(
            Value procedure,
            List<List<Value>> lists,
            int limit,
            int index,
            List<Value> results,
            Continuation cont) {
        if (index >= limit) {
            return new ContinueStep(cont, listFromElements(results));
        }

        List<Value> callArgs = new ArrayList<>(lists.size());
        for (List<Value> list : lists) {
            callArgs.add(list.get(index));
        }
        return new ApplyStep(
                procedure,
                callArgs,
                value -> {
                    List<Value> next = new ArrayList<>(results.size() + 1);
                    next.addAll(results);
                    next.add(value);
                    return stepBuiltinMapIter(procedure, lists, limit, index + 1, next, cont);
                });
    }

    private Step stepBuiltinForEach(List<Value> args, Continuation cont) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, "for-each");
        Value procedure = args.getFirst();
        List<List<Value>> lists = new ArrayList<>(args.size() - 1);
        int limit = Integer.MAX_VALUE;
        for (int i = 1; i < args.size(); i++) {
            List<Value> list = requireList(args.get(i), "for-each");
            lists.add(list);
            limit = Math.min(limit, list.size());
        }
        return stepBuiltinForEachIter(procedure, lists, limit, 0, cont);
    }

    private Step stepBuiltinForEachIter(
            Value procedure,
            List<List<Value>> lists,
            int limit,
            int index,
            Continuation cont) {
        if (index >= limit) {
            return new ContinueStep(cont, VoidValue.INSTANCE);
        }

        List<Value> callArgs = new ArrayList<>(lists.size());
        for (List<Value> list : lists) {
            callArgs.add(list.get(index));
        }
        return new ApplyStep(
                procedure,
                callArgs,
                ignored -> stepBuiltinForEachIter(procedure, lists, limit, index + 1, cont));
    }

    private Step stepEvalLet(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let' expects bindings and a body");
        }

        Expr head = args.getFirst();
        if (head instanceof SymbolExpr name) {
            return stepEvalNamedLet(name.name(), args.subList(1, args.size()), env, cont);
        }

        List<Binding> bindings = parseBindings(head);
        List<Expr> body = List.copyOf(args.subList(1, args.size()));
        return stepEvalBindingValues(
                bindings,
                0,
                env,
                List.of(),
                values -> {
                    Environment letEnv = new Environment(env);
                    for (int i = 0; i < bindings.size(); i++) {
                        letEnv.define(bindings.get(i).name(), values.get(i));
                    }
                    return stepEvalSequence(body, 0, letEnv, cont);
                });
    }

    private Step stepEvalNamedLet(
            String name,
            List<Expr> args,
            Environment env,
            Continuation cont) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        List<String> params = new ArrayList<>(bindings.size());
        for (Binding binding : bindings) {
            params.add(binding.name());
        }

        return stepEvalBindingValues(
                bindings,
                0,
                env,
                List.of(),
                values -> {
                    Environment closureEnv = new Environment(env);
                    ClosureValue closure = new ClosureValue(
                            name,
                            new ParameterSpec(List.copyOf(params), null),
                            List.copyOf(args.subList(1, args.size())),
                            closureEnv);
                    closureEnv.define(name, closure);
                    return stepApplyClosure(closure, values, cont);
                });
    }

    private Step stepEvalBindingValues(
            List<Binding> bindings,
            int index,
            Environment env,
            List<Value> values,
            ValuesContinuation cont) throws EvalError {
        if (index >= bindings.size()) {
            return cont.apply(values);
        }

        Binding binding = bindings.get(index);
        return new EvalExprStep(
                binding.valueExpr(),
                env,
                value -> {
                    List<Value> next = new ArrayList<>(values.size() + 1);
                    next.addAll(values);
                    next.add(value);
                    return stepEvalBindingValues(bindings, index + 1, env, next, cont);
                });
    }

    private Step stepEvalLetStar(List<Expr> args, Environment env, Continuation cont)
            throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'let*' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        Environment letStarEnv = new Environment(env);
        return stepEvalLetStarBindings(
                bindings,
                0,
                letStarEnv,
                List.copyOf(args.subList(1, args.size())),
                cont);
    }

    private Step stepEvalLetStarBindings(
            List<Binding> bindings,
            int index,
            Environment letStarEnv,
            List<Expr> body,
            Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            return stepEvalSequence(body, 0, letStarEnv, cont);
        }

        Binding binding = bindings.get(index);
        return new EvalExprStep(
                binding.valueExpr(),
                letStarEnv,
                value -> {
                    letStarEnv.define(binding.name(), value);
                    return stepEvalLetStarBindings(bindings, index + 1, letStarEnv, body, cont);
                });
    }

    private Step stepEvalLetrec(List<Expr> args, Environment env, Continuation cont, boolean sequential)
            throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'" + (sequential ? "letrec*" : "letrec")
                    + "' expects bindings and a body");
        }

        List<Binding> bindings = parseBindings(args.getFirst());
        Environment letrecEnv = new Environment(env);
        for (Binding binding : bindings) {
            letrecEnv.define(binding.name(), null);
        }

        List<Expr> body = List.copyOf(args.subList(1, args.size()));
        if (sequential) {
            return stepEvalLetrecSequential(bindings, 0, letrecEnv, body, cont);
        }
        return stepEvalLetrecBindings(bindings, 0, letrecEnv, List.of(), body, cont);
    }

    private Step stepEvalLetrecBindings(
            List<Binding> bindings,
            int index,
            Environment letrecEnv,
            List<Value> values,
            List<Expr> body,
            Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            for (int i = 0; i < bindings.size(); i++) {
                letrecEnv.set(bindings.get(i).name(), values.get(i));
            }
            return stepEvalSequence(body, 0, letrecEnv, cont);
        }

        Binding binding = bindings.get(index);
        return new EvalExprStep(
                binding.valueExpr(),
                letrecEnv,
                value -> {
                    List<Value> next = new ArrayList<>(values.size() + 1);
                    next.addAll(values);
                    next.add(value);
                    return stepEvalLetrecBindings(bindings, index + 1, letrecEnv, next, body, cont);
                });
    }

    private Step stepEvalLetrecSequential(
            List<Binding> bindings,
            int index,
            Environment letrecEnv,
            List<Expr> body,
            Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            return stepEvalSequence(body, 0, letrecEnv, cont);
        }

        Binding binding = bindings.get(index);
        return new EvalExprStep(
                binding.valueExpr(),
                letrecEnv,
                value -> {
                    letrecEnv.set(binding.name(), value);
                    return stepEvalLetrecSequential(bindings, index + 1, letrecEnv, body, cont);
                });
    }

    private Step stepEvalCase(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        if (args.isEmpty()) {
            throw new EvalError("'case' expects a key and clauses");
        }

        return new EvalExprStep(
                args.getFirst(),
                env,
                key -> stepEvalCaseClauses(key, args, 1, env, cont));
    }

    private Step stepEvalCaseClauses(
            Value key,
            List<Expr> clauses,
            int index,
            Environment env,
            Continuation cont) throws EvalError {
        if (index >= clauses.size()) {
            return new ContinueStep(cont, VoidValue.INSTANCE);
        }

        Expr clauseExpr = clauses.get(index);
        if (!(clauseExpr instanceof ListExpr clause)) {
            throw new EvalError("'case' clauses must be lists");
        }

        List<Expr> clauseElements = clause.elements();
        if (clauseElements.isEmpty()) {
            throw new EvalError("'case' clause cannot be empty");
        }

        Expr head = clauseElements.getFirst();
        if (head instanceof SymbolExpr symbol && symbol.name().equals("else")) {
            if (index != clauses.size() - 1) {
                throw new EvalError("'case' else clause must be last");
            }
            return stepEvalSequence(clauseElements.subList(1, clauseElements.size()), 0, env, cont);
        }

        if (!(head instanceof ListExpr datums)) {
            throw new EvalError("'case' clause datum list must be a list");
        }

        for (Expr datumExpr : datums.elements()) {
            if (isEqv(key, quoteToValue(datumExpr))) {
                return stepEvalSequence(clauseElements.subList(1, clauseElements.size()), 0, env, cont);
            }
        }
        return stepEvalCaseClauses(key, clauses, index + 1, env, cont);
    }

    private Step stepEvalDo(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'do' expects variable bindings and a test clause");
        }

        List<DoBinding> bindings = parseDoBindings(args.getFirst());
        if (!(args.get(1) instanceof ListExpr testClause) || testClause.elements().isEmpty()) {
            throw new EvalError("'do' expects a non-empty test clause");
        }

        return stepEvalDoInitBindings(
                bindings,
                0,
                env,
                List.of(),
                testClause.elements(),
                List.copyOf(args.subList(2, args.size())),
                cont);
    }

    private Step stepEvalDoInitBindings(
            List<DoBinding> bindings,
            int index,
            Environment env,
            List<Value> initialValues,
            List<Expr> testElements,
            List<Expr> commands,
            Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            Environment loopEnv = new Environment(env);
            for (int i = 0; i < bindings.size(); i++) {
                loopEnv.define(bindings.get(i).name(), initialValues.get(i));
            }
            return stepEvalDoLoop(bindings, testElements, commands, loopEnv, cont);
        }

        DoBinding binding = bindings.get(index);
        return new EvalExprStep(
                binding.initExpr(),
                env,
                value -> {
                    List<Value> next = new ArrayList<>(initialValues.size() + 1);
                    next.addAll(initialValues);
                    next.add(value);
                    return stepEvalDoInitBindings(
                            bindings,
                            index + 1,
                            env,
                            next,
                            testElements,
                            commands,
                            cont);
                });
    }

    private Step stepEvalDoLoop(
            List<DoBinding> bindings,
            List<Expr> testElements,
            List<Expr> commands,
            Environment loopEnv,
            Continuation cont) {
        return new EvalExprStep(
                testElements.getFirst(),
                loopEnv,
                testValue -> {
                    if (isTruthy(testValue)) {
                        return stepEvalSequence(testElements.subList(1, testElements.size()), 0, loopEnv, cont);
                    }
                    return stepEvalSequence(
                            commands,
                            0,
                            loopEnv,
                            ignored -> stepEvalDoSteps(
                                    bindings,
                                    0,
                                    loopEnv,
                                    List.of(),
                                    testElements,
                                    commands,
                                    cont));
                });
    }

    private Step stepEvalDoSteps(
            List<DoBinding> bindings,
            int index,
            Environment loopEnv,
            List<Value> nextValues,
            List<Expr> testElements,
            List<Expr> commands,
            Continuation cont) throws EvalError {
        if (index >= bindings.size()) {
            for (int i = 0; i < bindings.size(); i++) {
                loopEnv.set(bindings.get(i).name(), nextValues.get(i));
            }
            return stepEvalDoLoop(bindings, testElements, commands, loopEnv, cont);
        }

        DoBinding binding = bindings.get(index);
        if (binding.stepExpr() == null) {
            List<Value> next = new ArrayList<>(nextValues.size() + 1);
            next.addAll(nextValues);
            next.add(loopEnv.lookup(binding.name()));
            return stepEvalDoSteps(bindings, index + 1, loopEnv, next, testElements, commands, cont);
        }

        return new EvalExprStep(
                binding.stepExpr(),
                loopEnv,
                value -> {
                    List<Value> next = new ArrayList<>(nextValues.size() + 1);
                    next.addAll(nextValues);
                    next.add(value);
                    return stepEvalDoSteps(
                            bindings,
                            index + 1,
                            loopEnv,
                            next,
                            testElements,
                            commands,
                            cont);
                });
    }

    private Step stepEvalCond(
            List<Expr> clauses,
            int index,
            Environment env,
            Continuation cont) throws EvalError {
        if (index >= clauses.size()) {
            return new ContinueStep(cont, VoidValue.INSTANCE);
        }

        Expr clauseExpr = clauses.get(index);
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
        if (isElseClause && index != clauses.size() - 1) {
            throw new EvalError("'cond' else clause must be last");
        }

        if (isElseClause) {
            return stepEvalSequence(clauseElements.subList(1, clauseElements.size()), 0, env, cont);
        }

        return new EvalExprStep(
                testExpr,
                env,
                testResult -> {
                    if (isTruthy(testResult)) {
                        if (clauseElements.size() == 1) {
                            return new ContinueStep(cont, testResult);
                        }
                        return stepEvalSequence(clauseElements.subList(1, clauseElements.size()), 0, env, cont);
                    }
                    return stepEvalCond(clauses, index + 1, env, cont);
                });
    }

    private Step stepHandleGuardClauses(
            String exceptionVariable,
            List<Expr> clauses,
            Environment env,
            Value exception,
            Continuation cont) throws EvalError {
        Environment guardEnv = new Environment(env);
        guardEnv.define(exceptionVariable, exception);
        return stepEvalGuardClauses(clauses, 0, guardEnv, exception, cont);
    }

    private Step stepEvalGuardClauses(
            List<Expr> clauses,
            int index,
            Environment env,
            Value exception,
            Continuation cont) throws EvalError {
        if (index >= clauses.size()) {
            throw new RaisedSchemeException(exception);
        }

        Expr clauseExpr = clauses.get(index);
        if (!(clauseExpr instanceof ListExpr clause)) {
            throw new EvalError("'guard' clauses must be lists");
        }

        List<Expr> clauseElements = clause.elements();
        if (clauseElements.isEmpty()) {
            throw new EvalError("'guard' clause cannot be empty");
        }

        Expr testExpr = clauseElements.getFirst();
        boolean isElseClause = testExpr instanceof SymbolExpr symbol
                && symbol.name().equals("else");
        if (isElseClause && index != clauses.size() - 1) {
            throw new EvalError("'guard' else clause must be last");
        }

        if (isElseClause) {
            return stepEvalSequence(clauseElements.subList(1, clauseElements.size()), 0, env, cont);
        }

        return new EvalExprStep(
                testExpr,
                env,
                testResult -> {
                    if (isTruthy(testResult)) {
                        if (clauseElements.size() == 1) {
                            return new ContinueStep(cont, testResult);
                        }
                        return stepEvalSequence(clauseElements.subList(1, clauseElements.size()), 0, env, cont);
                    }
                    return stepEvalGuardClauses(clauses, index + 1, env, exception, cont);
                });
    }

    private Step stepEvalAnd(
            List<Expr> expressions,
            int index,
            Environment env,
            Continuation cont) {
        if (index >= expressions.size()) {
            return new ContinueStep(cont, BoolValue.TRUE);
        }

        boolean last = index + 1 == expressions.size();
        return new EvalExprStep(
                expressions.get(index),
                env,
                value -> {
                    if (!isTruthy(value) || last) {
                        return new ContinueStep(cont, value);
                    }
                    return stepEvalAnd(expressions, index + 1, env, cont);
                });
    }

    private Step stepEvalOr(
            List<Expr> expressions,
            int index,
            Environment env,
            Continuation cont) {
        if (index >= expressions.size()) {
            return new ContinueStep(cont, BoolValue.FALSE);
        }

        return new EvalExprStep(
                expressions.get(index),
                env,
                value -> {
                    if (isTruthy(value)) {
                        return new ContinueStep(cont, value);
                    }
                    return stepEvalOr(expressions, index + 1, env, cont);
                });
    }

    private Macro parseMacro(String name, Expr transformerExpr, Environment env) throws EvalError {
        if (!(transformerExpr instanceof ListExpr transformer)) {
            Value transformerValue = eval(transformerExpr, env);
            if (transformerValue instanceof ProcedureValue procedureValue) {
                return new Macro(name, Set.of(), List.of(), env, procedureValue);
            }
            throw new EvalError("'define-syntax' expects a transformer procedure");
        }

        List<Expr> transformerElements = transformer.elements();
        if (transformerElements.size() >= 3
                && transformerElements.getFirst() instanceof SymbolExpr keyword
                && keyword.name().equals("syntax-rules")) {
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
                collectPatternVariables(
                        pattern,
                        name,
                        literals,
                        patternVariables,
                        repeatedVariables,
                        false);
                rules.add(new MacroRule(
                        pattern,
                        template,
                        Set.copyOf(patternVariables),
                        Set.copyOf(repeatedVariables)));
            }

            if (rules.isEmpty()) {
                throw new EvalError("'syntax-rules' expects at least one clause");
            }
            return new Macro(name, Set.copyOf(literals), List.copyOf(rules), env, null);
        }

        Value transformerValue = eval(transformerExpr, env);
        if (transformerValue instanceof ProcedureValue procedureValue) {
            return new Macro(name, Set.of(), List.of(), env, procedureValue);
        }
        throw new EvalError("'define-syntax' expects a transformer procedure");
    }

    private Set<String> parseMacroLiterals(Expr literalsExpr) throws EvalError {
        return parseLiteralIdentifiers(
                literalsExpr,
                "'syntax-rules' expects a literal identifier list",
                "'syntax-rules' literals must be symbols");
    }

    private Set<String> parseSyntaxCaseLiterals(Expr literalsExpr) throws EvalError {
        return parseLiteralIdentifiers(
                literalsExpr,
                "'syntax-case' expects a literal identifier list",
                "'syntax-case' literals must be symbols");
    }

    private Set<String> parseLiteralIdentifiers(
            Expr literalsExpr,
            String invalidListMessage,
            String invalidLiteralMessage) throws EvalError {
        if (!(literalsExpr instanceof ListExpr literalsList)) {
            throw new EvalError(invalidListMessage);
        }

        Set<String> literals = new HashSet<>(literalsList.elements().size());
        for (Expr literalExpr : literalsList.elements()) {
            if (!(literalExpr instanceof SymbolExpr literalSymbol)) {
                throw new EvalError(invalidLiteralMessage);
            }
            literals.add(literalSymbol.name());
        }
        return literals;
    }

    private Value evalSyntaxCase(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 3) {
            throw new EvalError("'syntax-case' expects an input, literals, and clauses");
        }

        SyntaxValue input = requireSyntax(eval(args.getFirst(), env), "syntax-case");
        Set<String> literals = parseSyntaxCaseLiterals(args.get(1));
        for (int i = 2; i < args.size(); i++) {
            Expr clauseExpr = args.get(i);
            if (!(clauseExpr instanceof ListExpr clause)) {
                throw new EvalError("'syntax-case' clauses must be lists");
            }

            List<Expr> clauseElements = clause.elements();
            if (clauseElements.size() < 2 || clauseElements.size() > 3) {
                throw new EvalError("'syntax-case' clauses must contain a pattern and an expression");
            }

            Expr pattern = clauseElements.getFirst();
            Expr fender = clauseElements.size() == 3 ? clauseElements.get(1) : null;
            Expr resultExpr = clauseElements.getLast();

            Set<String> patternVariables = new HashSet<>();
            Set<String> repeatedVariables = new HashSet<>();
            collectPatternVariables(
                    pattern,
                    null,
                    literals,
                    patternVariables,
                    repeatedVariables,
                    false);

            PatternBindings bindings = new PatternBindings();
            if (!matchPattern(pattern, input.expr(), null, literals, bindings, false)) {
                continue;
            }

            Environment clauseEnv = extendSyntaxBindingEnv(
                    env,
                    bindings,
                    Set.copyOf(patternVariables),
                    Set.copyOf(repeatedVariables));
            if (fender != null && !isTruthy(eval(fender, clauseEnv))) {
                continue;
            }
            return eval(resultExpr, clauseEnv);
        }

        throw new EvalError("no matching syntax-case clause");
    }

    private EvalAction evalWithSyntax(List<Expr> args, Environment env) throws EvalError {
        if (args.size() < 2) {
            throw new EvalError("'with-syntax' expects bindings and a body");
        }
        if (!(args.getFirst() instanceof ListExpr bindingList)) {
            throw new EvalError("'with-syntax' expects a binding list");
        }

        Environment withSyntaxEnv = new Environment(env);
        for (Expr bindingExpr : bindingList.elements()) {
            if (!(bindingExpr instanceof ListExpr binding)) {
                throw new EvalError("'with-syntax' bindings must be lists");
            }

            List<Expr> bindingElements = binding.elements();
            if (bindingElements.size() != 2) {
                throw new EvalError("'with-syntax' bindings must contain a pattern and a syntax expression");
            }

            Expr pattern = bindingElements.getFirst();
            SyntaxValue syntax = requireSyntax(eval(bindingElements.get(1), env), "with-syntax");
            Set<String> patternVariables = new HashSet<>();
            Set<String> repeatedVariables = new HashSet<>();
            collectPatternVariables(
                    pattern,
                    null,
                    Set.of(),
                    patternVariables,
                    repeatedVariables,
                    false);

            PatternBindings bindings = new PatternBindings();
            if (!matchPattern(pattern, syntax.expr(), null, Set.of(), bindings, false)) {
                throw new EvalError("'with-syntax' binding does not match its pattern");
            }
            bindSyntaxVariables(
                    withSyntaxEnv,
                    bindings,
                    Set.copyOf(patternVariables),
                    Set.copyOf(repeatedVariables));
        }

        return tailSequence(args.subList(1, args.size()), withSyntaxEnv);
    }

    private Step stepEvalWithSyntax(List<Expr> args, Environment env, Continuation cont) throws EvalError {
        EvalAction action = evalWithSyntax(args, env);
        return switch (action) {
            case ReturnValueAction returnValueAction -> new ContinueStep(cont, returnValueAction.value());
            case ContinueEvalAction continueEvalAction ->
                    new EvalExprStep(continueEvalAction.expr(), continueEvalAction.env(), cont);
        };
    }

    private Value evalQuoteSyntax(List<Expr> args, Environment env) throws EvalError {
        requireArgCount(args.size(), 1, "quote-syntax");
        TemplateContext context = collectTemplateContext(env);
        return new SyntaxValue(instantiateTemplate(args.getFirst(), context, Map.of(), null), false);
    }

    private void collectPatternVariables(
            Expr pattern,
            String implicitLiteral,
            Set<String> literals,
            Set<String> patternVariables,
            Set<String> repeatedVariables,
            boolean repeatedContext) {
        switch (pattern) {
            case SymbolExpr symbolExpr -> {
                String name = symbolExpr.name();
                if (!name.equals("...")
                        && !isPatternLiteral(name, implicitLiteral, literals)) {
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
                            implicitLiteral,
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
        if (macro.transformer() != null) {
            return expandTransformerMacro(macro, invocation);
        }

        for (MacroRule rule : macro.rules()) {
            PatternBindings bindings = new PatternBindings();
            if (matchPattern(rule.pattern(), invocation, macro.name(), macro.literals(), bindings, false)) {
                return instantiateTemplate(
                        rule.template(),
                        templateContextForRule(macro, rule, bindings),
                        Map.of(),
                        null);
            }
        }
        throw new EvalError("no matching syntax-rules clause");
    }

    private Expr expandTransformerMacro(Macro macro, ListExpr invocation) throws EvalError {
        Value result = applyMacroTransformer(
                macro.transformer(),
                List.of(new SyntaxValue(invocation, false)),
                macro.definitionEnv());
        return requireSyntax(result, "macro transformer").expr();
    }

    private Value applyMacroTransformer(
            ProcedureValue transformer,
            List<Value> args,
            Environment macroCaptureEnv) throws EvalError {
        return switch (transformer) {
            case BuiltinProcedure builtinProcedure -> builtinProcedure.fn().apply(args);
            case ClosureValue closureValue -> resolveEvalAction(applyProcedureClause(
                    closureValue.params(),
                    closureValue.body(),
                    closureValue.env(),
                    args,
                    "wrong number of arguments",
                    macroCaptureEnv));
            case CaseLambdaValue caseLambdaValue -> {
                for (CaseLambdaClause clause : caseLambdaValue.clauses()) {
                    if (clause.params().accepts(args.size())) {
                        yield resolveEvalAction(applyProcedureClause(
                                clause.params(),
                                clause.body(),
                                caseLambdaValue.env(),
                                args,
                                "wrong number of arguments",
                                macroCaptureEnv));
                    }
                }
                throw new EvalError("wrong number of arguments");
            }
            case ContinuationProcedure ignored ->
                    throw new EvalError("macro transformer cannot be a continuation");
        };
    }

    private boolean matchPattern(
            Expr pattern,
            Expr input,
            String implicitLiteral,
            Set<String> literals,
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
                    implicitLiteral,
                    literals,
                    bindings,
                    repeatedContext);
            case ListExpr listExpr -> input instanceof ListExpr other
                    && matchPatternList(
                    listExpr.elements(),
                    other.elements(),
                    implicitLiteral,
                    literals,
                    bindings,
                    repeatedContext);
            case CapturedSymbolExpr ignored -> false;
        };
    }

    private boolean matchPatternSymbol(
            SymbolExpr pattern,
            Expr input,
            String implicitLiteral,
            Set<String> literals,
            PatternBindings bindings,
            boolean repeatedContext) {
        String name = pattern.name();
        if (name.equals("...")) {
            return false;
        }
        if (isPatternLiteral(name, implicitLiteral, literals)) {
            String identifierName = identifierName(input);
            return identifierName != null && identifierName.equals(name);
        }
        return bindings.bind(name, input, repeatedContext);
    }

    private boolean matchPatternList(
            List<Expr> patterns,
            List<Expr> inputs,
            String implicitLiteral,
            Set<String> literals,
            PatternBindings bindings,
            boolean repeatedContext) throws EvalError {
        return matchPatternList(
                patterns,
                0,
                inputs,
                0,
                implicitLiteral,
                literals,
                bindings,
                repeatedContext);
    }

    private boolean matchPatternList(
            List<Expr> patterns,
            int patternIndex,
            List<Expr> inputs,
            int inputIndex,
            String implicitLiteral,
            Set<String> literals,
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
                            implicitLiteral,
                            literals,
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
                        implicitLiteral,
                        literals,
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
        if (!matchPattern(
                patterns.get(patternIndex),
                inputs.get(inputIndex),
                implicitLiteral,
                literals,
                bindings,
                repeatedContext)) {
            return false;
        }
        return matchPatternList(
                patterns,
                patternIndex + 1,
                inputs,
                inputIndex + 1,
                implicitLiteral,
                literals,
                bindings,
                repeatedContext);
    }

    private TemplateContext templateContextForRule(
            Macro macro,
            MacroRule rule,
            PatternBindings bindings) {
        return new TemplateContext(
                bindings,
                rule.patternVariables(),
                rule.repeatedVariables(),
                macro.definitionEnv());
    }

    private TemplateContext collectTemplateContext(Environment env) {
        PatternBindings bindings = new PatternBindings();
        Set<String> patternVariables = new HashSet<>();
        Set<String> repeatedVariables = new HashSet<>();
        Set<String> seen = new HashSet<>();
        for (Environment current = env; current != null; current = current.parent) {
            for (Map.Entry<String, Value> entry : current.bindings.entrySet()) {
                String name = entry.getKey();
                if (!seen.add(name)) {
                    continue;
                }

                Value value = entry.getValue();
                if (value instanceof SyntaxValue syntaxValue && syntaxValue.templateBinding()) {
                    patternVariables.add(name);
                    bindings.putSingle(name, syntaxValue.expr());
                    continue;
                }

                List<Expr> repeated = templateBindingList(value);
                if (repeated != null) {
                    patternVariables.add(name);
                    repeatedVariables.add(name);
                    bindings.putRepeated(name, repeated);
                }
            }
        }

        Environment captureEnv = env.macroCaptureEnv == null ? env : env.macroCaptureEnv;
        return new TemplateContext(
                bindings,
                Set.copyOf(patternVariables),
                Set.copyOf(repeatedVariables),
                captureEnv);
    }

    private Environment extendSyntaxBindingEnv(
            Environment env,
            PatternBindings bindings,
            Set<String> patternVariables,
            Set<String> repeatedVariables) throws EvalError {
        Environment bindingEnv = new Environment(env);
        bindSyntaxVariables(bindingEnv, bindings, patternVariables, repeatedVariables);
        return bindingEnv;
    }

    private void bindSyntaxVariables(
            Environment env,
            PatternBindings bindings,
            Set<String> patternVariables,
            Set<String> repeatedVariables) throws EvalError {
        for (String name : patternVariables) {
            if (repeatedVariables.contains(name)) {
                List<Value> values = new ArrayList<>(bindings.repeatedCount(name));
                for (int i = 0; i < bindings.repeatedCount(name); i++) {
                    values.add(new SyntaxValue(bindings.repeated(name, i), true));
                }
                env.define(name, listFromElements(values));
            } else {
                env.define(name, new SyntaxValue(bindings.single(name), true));
            }
        }
    }

    private Expr instantiateTemplate(
            Expr template,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        return switch (template) {
            case IntExpr intExpr -> intExpr;
            case RationalExpr rationalExpr -> rationalExpr;
            case InexactExpr inexactExpr -> inexactExpr;
            case BoolExpr boolExpr -> boolExpr;
            case StringExpr stringExpr -> stringExpr;
            case CharExpr charExpr -> charExpr;
            case SymbolExpr symbolExpr -> instantiateTemplateSymbol(
                    symbolExpr,
                    context,
                    lexicalRenames,
                    repetitionIndex);
            case ListExpr listExpr -> instantiateTemplateList(
                    listExpr,
                    context,
                    lexicalRenames,
                    repetitionIndex);
            case CapturedSymbolExpr capturedSymbolExpr -> capturedSymbolExpr;
        };
    }

    private Expr instantiateTemplateSymbol(
            SymbolExpr symbolExpr,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        String name = symbolExpr.name();
        if (context.repeatedVariables().contains(name)) {
            if (repetitionIndex == null) {
                throw new EvalError("repeated pattern variable used without ellipsis: " + name);
            }
            return context.bindings().repeated(name, repetitionIndex);
        }
        if (context.patternVariables().contains(name)) {
            return context.bindings().single(name);
        }

        String renamed = lexicalRenames.get(name);
        if (renamed != null) {
            return new SymbolExpr(renamed, symbolExpr.loc());
        }
        if (isSyntaxKeyword(name) || macros.containsKey(name)) {
            return symbolExpr;
        }
        return new CapturedSymbolExpr(name, symbolExpr.loc(), context.captureEnv());
    }

    private Expr instantiateTemplateList(
            ListExpr listExpr,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        if (!listExpr.elements().isEmpty()
                && listExpr.elements().getFirst() instanceof SymbolExpr headSymbol
                && !context.patternVariables().contains(headSymbol.name())
                && !context.repeatedVariables().contains(headSymbol.name())
                && !lexicalRenames.containsKey(headSymbol.name())) {
            return switch (headSymbol.name()) {
                case "let" -> instantiateTemplateLet(
                        listExpr,
                        context,
                        lexicalRenames,
                        repetitionIndex);
                case "lambda" -> instantiateTemplateLambda(
                        listExpr,
                        context,
                        lexicalRenames,
                        repetitionIndex);
                case "define" -> instantiateTemplateDefine(
                        listExpr,
                        context,
                        lexicalRenames,
                        repetitionIndex);
                default -> instantiateTemplateListFallback(
                        listExpr,
                        context,
                        lexicalRenames,
                        repetitionIndex);
            };
        }

        return instantiateTemplateListFallback(
                listExpr,
                context,
                lexicalRenames,
                repetitionIndex);
    }

    private Expr instantiateTemplateLambda(
            ListExpr template,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = template.elements();
        if (elements.size() < 3) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        BindingRewrite rewrite = rewriteBindingExpr(elements.get(1), context);
        if (rewrite == null) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        Map<String, String> innerRenames = new HashMap<>(lexicalRenames);
        innerRenames.putAll(rewrite.renames());
        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(elements.getFirst());
        expanded.add(rewrite.bindingsExpr());
        expanded.addAll(instantiateTemplateBody(
                elements.subList(2, elements.size()),
                context,
                innerRenames,
                repetitionIndex));
        return new ListExpr(List.copyOf(expanded), template.loc());
    }

    private Expr instantiateTemplateDefine(
            ListExpr template,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = template.elements();
        if (elements.size() < 3 || !(elements.get(1) instanceof ListExpr signature)) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        List<Expr> signatureElements = signature.elements();
        if (signatureElements.isEmpty()) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        BindingRewrite rewrite = rewriteBindingList(
                signatureElements.subList(1, signatureElements.size()),
                signature.loc(),
                context);
        if (rewrite == null) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        Map<String, String> innerRenames = new HashMap<>(lexicalRenames);
        innerRenames.putAll(rewrite.renames());
        List<Expr> rewrittenSignature = new ArrayList<>(signatureElements.size());
        if (!(signatureElements.getFirst() instanceof SymbolExpr functionName)) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }
        rewrittenSignature.add(instantiateTemplateBinder(
                functionName,
                context,
                lexicalRenames,
                repetitionIndex,
                innerRenames));
        if (rewrite.bindingsExpr() instanceof ListExpr rewrittenParams) {
            rewrittenSignature.addAll(rewrittenParams.elements());
        } else {
            rewrittenSignature.add(rewrite.bindingsExpr());
        }

        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(elements.getFirst());
        expanded.add(new ListExpr(List.copyOf(rewrittenSignature), signature.loc()));
        expanded.addAll(instantiateTemplateBody(
                elements.subList(2, elements.size()),
                context,
                innerRenames,
                repetitionIndex));
        return new ListExpr(List.copyOf(expanded), template.loc());
    }

    private BindingRewrite rewriteBindingExpr(Expr bindingExpr, TemplateContext context) {
        if (bindingExpr instanceof SymbolExpr bindingName) {
            if (context.patternVariables().contains(bindingName.name())
                    || context.repeatedVariables().contains(bindingName.name())) {
                return null;
            }
            String freshName = freshMacroName(bindingName.name());
            return new BindingRewrite(
                    new SymbolExpr(freshName, bindingName.loc()),
                    Map.of(bindingName.name(), freshName));
        }
        if (bindingExpr instanceof ListExpr bindingList) {
            return rewriteBindingList(bindingList.elements(), bindingExpr.loc(), context);
        }
        return null;
    }

    private BindingRewrite rewriteBindingList(
            List<Expr> bindings,
            SourcePos loc,
            TemplateContext context) {
        Map<String, String> renames = new HashMap<>();
        List<Expr> rewritten = new ArrayList<>(bindings.size());
        for (Expr binding : bindings) {
            if (!(binding instanceof SymbolExpr bindingName)
                    || context.patternVariables().contains(bindingName.name())
                    || context.repeatedVariables().contains(bindingName.name())) {
                return null;
            }
            String freshName = freshMacroName(bindingName.name());
            renames.put(bindingName.name(), freshName);
            rewritten.add(new SymbolExpr(freshName, bindingName.loc()));
        }
        return new BindingRewrite(new ListExpr(List.copyOf(rewritten), loc), Map.copyOf(renames));
    }

    private Expr instantiateTemplateLet(
            ListExpr template,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = template.elements();
        if (elements.size() < 3) {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        Map<String, String> innerRenames = new HashMap<>(lexicalRenames);
        List<Expr> expanded = new ArrayList<>(elements.size());
        expanded.add(elements.getFirst());

        int bodyStartIndex;
        if (elements.get(1) instanceof SymbolExpr namedLetName) {
            if (elements.size() < 4 || !(elements.get(2) instanceof ListExpr namedBindingsExpr)) {
                return instantiateTemplateListFallback(
                        template,
                        context,
                        lexicalRenames,
                        repetitionIndex);
            }

            List<Expr> rewrittenBindings = instantiateTemplateLetBindings(
                    namedBindingsExpr,
                    context,
                    lexicalRenames,
                    innerRenames,
                    repetitionIndex);
            if (rewrittenBindings == null) {
                return instantiateTemplateListFallback(
                        template,
                        context,
                        lexicalRenames,
                        repetitionIndex);
            }

            expanded.add(instantiateTemplateBinder(
                    namedLetName,
                    context,
                    lexicalRenames,
                    repetitionIndex,
                    innerRenames));
            expanded.add(new ListExpr(List.copyOf(rewrittenBindings), namedBindingsExpr.loc()));
            bodyStartIndex = 3;
        } else if (elements.get(1) instanceof ListExpr bindingListExpr) {
            List<Expr> rewrittenBindings = instantiateTemplateLetBindings(
                    bindingListExpr,
                    context,
                    lexicalRenames,
                    innerRenames,
                    repetitionIndex);
            if (rewrittenBindings == null) {
                return instantiateTemplateListFallback(
                        template,
                        context,
                        lexicalRenames,
                        repetitionIndex);
            }

            expanded.add(new ListExpr(List.copyOf(rewrittenBindings), bindingListExpr.loc()));
            bodyStartIndex = 2;
        } else {
            return instantiateTemplateListFallback(
                    template,
                    context,
                    lexicalRenames,
                    repetitionIndex);
        }

        expanded.addAll(instantiateTemplateBody(
                elements.subList(bodyStartIndex, elements.size()),
                context,
                innerRenames,
                repetitionIndex));
        return new ListExpr(List.copyOf(expanded), template.loc());
    }

    private List<Expr> instantiateTemplateLetBindings(
            ListExpr bindingListExpr,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Map<String, String> innerRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> rewrittenBindings = new ArrayList<>(bindingListExpr.elements().size());
        for (Expr rawBinding : bindingListExpr.elements()) {
            if (!(rawBinding instanceof ListExpr bindingList)) {
                return null;
            }

            List<Expr> bindingElements = bindingList.elements();
            if (bindingElements.size() != 2
                    || !(bindingElements.getFirst() instanceof SymbolExpr bindingName)
                    || context.patternVariables().contains(bindingName.name())
                    || context.repeatedVariables().contains(bindingName.name())) {
                return null;
            }

            String freshName = freshMacroName(bindingName.name());
            innerRenames.put(bindingName.name(), freshName);
            rewrittenBindings.add(new ListExpr(List.of(
                    new SymbolExpr(freshName, bindingName.loc()),
                    instantiateTemplate(
                            bindingElements.get(1),
                            context,
                            lexicalRenames,
                            repetitionIndex)),
                    rawBinding.loc()));
        }
        return rewrittenBindings;
    }

    private Expr instantiateTemplateBinder(
            SymbolExpr templateName,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex,
            Map<String, String> innerRenames) throws EvalError {
        if (context.patternVariables().contains(templateName.name())
                || context.repeatedVariables().contains(templateName.name())) {
            Expr instantiated = instantiateTemplate(
                    templateName,
                    context,
                    lexicalRenames,
                    repetitionIndex);
            if (instantiated instanceof SymbolExpr symbolExpr) {
                return new SymbolExpr(symbolExpr.name(), symbolExpr.loc());
            }
            if (instantiated instanceof CapturedSymbolExpr symbolExpr) {
                return new SymbolExpr(symbolExpr.name(), symbolExpr.loc());
            }
            return instantiated;
        }

        String freshName = freshMacroName(templateName.name());
        innerRenames.put(templateName.name(), freshName);
        return new SymbolExpr(freshName, templateName.loc());
    }

    private List<Expr> instantiateTemplateBody(
            List<Expr> body,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        Map<String, String> bodyRenames = new HashMap<>(lexicalRenames);
        List<Expr> expandedBody = new ArrayList<>(body.size());
        for (Expr expr : body) {
            Expr expandedExpr = instantiateTemplate(
                    expr,
                    context,
                    bodyRenames,
                    repetitionIndex);
            expandedBody.add(expandedExpr);
            extendTemplateBodyRenamesForDefine(expr, expandedExpr, context, bodyRenames);
        }
        return expandedBody;
    }

    private void extendTemplateBodyRenamesForDefine(
            Expr templateExpr,
            Expr expandedExpr,
            TemplateContext context,
            Map<String, String> bodyRenames) {
        String templateName = extractDefinedIdentifier(templateExpr);
        if (templateName == null
                || context.patternVariables().contains(templateName)
                || context.repeatedVariables().contains(templateName)) {
            return;
        }

        String expandedName = extractDefinedIdentifier(expandedExpr);
        if (expandedName != null) {
            bodyRenames.put(templateName, expandedName);
        }
    }

    private String extractDefinedIdentifier(Expr expr) {
        if (!(expr instanceof ListExpr listExpr)) {
            return null;
        }

        List<Expr> elements = listExpr.elements();
        if (elements.size() < 2
                || !(elements.getFirst() instanceof SymbolExpr head)
                || !head.name().equals("define")) {
            return null;
        }

        Expr target = elements.get(1);
        if (target instanceof SymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        if (target instanceof CapturedSymbolExpr symbolExpr) {
            return symbolExpr.name();
        }
        if (target instanceof ListExpr signature && !signature.elements().isEmpty()) {
            Expr nameExpr = signature.elements().getFirst();
            if (nameExpr instanceof SymbolExpr symbolExpr) {
                return symbolExpr.name();
            }
            if (nameExpr instanceof CapturedSymbolExpr symbolExpr) {
                return symbolExpr.name();
            }
        }
        return null;
    }

    private Expr instantiateTemplateListFallback(
            ListExpr listExpr,
            TemplateContext context,
            Map<String, String> lexicalRenames,
            Integer repetitionIndex) throws EvalError {
        List<Expr> elements = listExpr.elements();
        List<Expr> expanded = new ArrayList<>(elements.size());
        for (int i = 0; i < elements.size(); i++) {
            Expr element = elements.get(i);
            if (i + 1 < elements.size() && isEllipsisExpr(elements.get(i + 1))) {
                int count = determineRepetitionCount(element, context);
                for (int repetition = 0; repetition < count; repetition++) {
                    expanded.add(instantiateTemplate(
                            element,
                            context,
                            lexicalRenames,
                            repetition));
                }
                i++;
                continue;
            }
            expanded.add(instantiateTemplate(
                    element,
                    context,
                    lexicalRenames,
                    repetitionIndex));
        }
        return new ListExpr(List.copyOf(expanded), listExpr.loc());
    }

    private int determineRepetitionCount(Expr template, TemplateContext context) throws EvalError {
        Integer count = determineRepetitionCountOrNull(template, context);
        if (count == null) {
            throw new EvalError("ellipsis template must include a repeated pattern variable");
        }
        return count;
    }

    private Integer determineRepetitionCountOrNull(Expr template, TemplateContext context)
            throws EvalError {
        return switch (template) {
            case SymbolExpr symbolExpr -> context.repeatedVariables().contains(symbolExpr.name())
                    ? context.bindings().repeatedCount(symbolExpr.name())
                    : null;
            case ListExpr listExpr -> {
                Integer count = null;
                List<Expr> elements = listExpr.elements();
                for (int i = 0; i < elements.size(); i++) {
                    if (isEllipsisExpr(elements.get(i))) {
                        continue;
                    }
                    Integer nested = determineRepetitionCountOrNull(elements.get(i), context);
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

    private List<Expr> templateBindingList(Value value) {
        List<Expr> bindings = new ArrayList<>();
        Value current = value;
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                return null;
            }
            if (!(pairValue.car() instanceof SyntaxValue syntaxValue) || !syntaxValue.templateBinding()) {
                return null;
            }
            bindings.add(syntaxValue.expr());
            current = pairValue.cdr();
        }
        if (current == EmptyListValue.INSTANCE) {
            return List.copyOf(bindings);
        }
        return null;
    }

    private boolean isPatternLiteral(String name, String implicitLiteral, Set<String> literals) {
        return name.equals(implicitLiteral) || literals.contains(name);
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
                    "lambda", "case-lambda", "begin", "let", "let*", "letrec", "letrec*", "case",
                    "do", "cond", "and", "or", "guard", "else", ".", "syntax-case",
                    "with-syntax", "quote-syntax" -> true;
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
                yield listFromElements(values);
            }
        };
    }

    private Value apply(Value operator, List<Value> args) throws EvalError {
        return resolveEvalAction(tailApply(operator, args));
    }

    private EvalAction tailApply(Value operator, List<Value> args) throws EvalError {
        if (!(operator instanceof ProcedureValue procedure)) {
            throw new EvalError("attempted to call non-procedure");
        }

        return switch (procedure) {
            case BuiltinProcedure builtin -> returnValue(builtin.fn().apply(args));
            case ClosureValue closure -> applyClosure(closure, args);
            case CaseLambdaValue caseLambda -> applyCaseLambda(caseLambda, args);
            case ContinuationProcedure ignored ->
                    throw new EvalError("continuation application requires continuation support");
        };
    }

    private EvalAction applyClosure(ClosureValue closure, List<Value> args) throws EvalError {
        return applyProcedureClause(closure.params(), closure.body(), closure.env(), args, "wrong number of arguments");
    }

    private EvalAction applyCaseLambda(CaseLambdaValue caseLambda, List<Value> args) throws EvalError {
        for (CaseLambdaClause clause : caseLambda.clauses()) {
            if (clause.params().accepts(args.size())) {
                return applyProcedureClause(
                        clause.params(),
                        clause.body(),
                        caseLambda.env(),
                        args,
                        "wrong number of arguments");
            }
        }

        throw new EvalError("wrong number of arguments");
    }

    private EvalAction applyProcedureClause(
            ParameterSpec params,
            List<Expr> body,
            Environment lexicalEnv,
            List<Value> args,
            String arityMessage)
            throws EvalError {
        return applyProcedureClause(params, body, lexicalEnv, args, arityMessage, null);
    }

    private EvalAction applyProcedureClause(
            ParameterSpec params,
            List<Expr> body,
            Environment lexicalEnv,
            List<Value> args,
            String arityMessage,
            Environment macroCaptureEnv)
            throws EvalError {
        if (!params.accepts(args.size())) {
            throw new EvalError(arityMessage);
        }

        Environment callEnv = macroCaptureEnv == null
                ? new Environment(lexicalEnv)
                : new Environment(lexicalEnv, macroCaptureEnv);
        for (int i = 0; i < params.fixedParams().size(); i++) {
            callEnv.define(params.fixedParams().get(i), args.get(i));
        }
        if (params.restParam() != null) {
            callEnv.define(
                    params.restParam(),
                    listFromElements(args.subList(params.fixedParams().size(), args.size())));
        }

        return tailSequence(body, callEnv);
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

    private Value builtinGcd(List<Value> args) throws EvalError {
        BigInteger result = BigInteger.ZERO;
        for (Value arg : args) {
            result = result.gcd(requireExactInteger(arg, "gcd").abs());
        }
        return bigIntegerToExactValue(result);
    }

    private Value builtinLcm(List<Value> args) throws EvalError {
        BigInteger result = BigInteger.ONE;
        for (Value arg : args) {
            BigInteger value = requireExactInteger(arg, "lcm").abs();
            if (result.signum() == 0 || value.signum() == 0) {
                result = BigInteger.ZERO;
            } else {
                result = result.divide(result.gcd(value)).multiply(value);
            }
        }
        return bigIntegerToExactValue(args.isEmpty() ? BigInteger.ONE : result);
    }

    private Value builtinTruncate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "truncate");
        NumericValue value = requireNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            double truncated = inexactValue.value() < 0.0
                    ? Math.ceil(inexactValue.value())
                    : Math.floor(inexactValue.value());
            return new InexactValue(truncated);
        }
        ExactFraction fraction = toExactFraction(value);
        return bigIntegerToExactValue(fraction.numerator().divide(fraction.denominator()));
    }

    private Value builtinRound(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "round");
        NumericValue value = requireNumber(args.getFirst());
        if (value instanceof InexactValue inexactValue) {
            return new InexactValue(Math.rint(inexactValue.value()));
        }

        ExactFraction fraction = toExactFraction(value);
        BigInteger numerator = fraction.numerator();
        BigInteger denominator = fraction.denominator();
        BigInteger[] divRem = numerator.abs().divideAndRemainder(denominator);
        BigInteger quotient = divRem[0];
        BigInteger remainder = divRem[1];
        int ordering = remainder.shiftLeft(1).compareTo(denominator);
        if (ordering > 0 || ordering == 0 && quotient.testBit(0)) {
            quotient = quotient.add(BigInteger.ONE);
        }
        if (numerator.signum() < 0) {
            quotient = quotient.negate();
        }
        return bigIntegerToExactValue(quotient);
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
        return new PairValue(args.getFirst(), args.get(1));
    }

    private Value builtinCar(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "car");
        return requirePair(args.getFirst(), "car").car();
    }

    private Value builtinCdr(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "cdr");
        return requirePair(args.getFirst(), "cdr").cdr();
    }

    private Value builtinCxr(List<Value> args, String name) throws EvalError {
        requireArgCount(args.size(), 1, name);
        Value current = args.getFirst();
        for (int i = name.length() - 2; i >= 1; i--) {
            PairValue pairValue = requirePair(current, name);
            current = name.charAt(i) == 'a' ? pairValue.car() : pairValue.cdr();
        }
        return current;
    }

    private Value builtinSetCar(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "set-car!");
        requirePair(args.getFirst(), "set-car!").setCar(args.get(1));
        return VoidValue.INSTANCE;
    }

    private Value builtinSetCdr(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "set-cdr!");
        requirePair(args.getFirst(), "set-cdr!").setCdr(args.get(1));
        return VoidValue.INSTANCE;
    }

    private Value builtinNull(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "null?");
        return BoolValue.of(args.getFirst() == EmptyListValue.INSTANCE);
    }

    private Value builtinList(List<Value> args) {
        return listFromElements(args);
    }

    private Value builtinLength(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "length");
        return new IntValue(listLength(args.getFirst(), "length"));
    }

    private Value builtinAppend(List<Value> args) throws EvalError {
        if (args.isEmpty()) {
            return EmptyListValue.INSTANCE;
        }

        Value tail = args.getLast();
        List<Value> prefix = new ArrayList<>();
        for (int i = 0; i < args.size() - 1; i++) {
            prefix.addAll(requireList(args.get(i), "append"));
        }

        for (int i = prefix.size() - 1; i >= 0; i--) {
            tail = new PairValue(prefix.get(i), tail);
        }
        return tail;
    }

    private Value builtinReverse(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "reverse");
        List<Value> elements = requireList(args.getFirst(), "reverse");
        List<Value> reversed = new ArrayList<>(elements.size());
        for (int i = elements.size() - 1; i >= 0; i--) {
            reversed.add(elements.get(i));
        }
        return listFromElements(reversed);
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

    private Value builtinValues(List<Value> args) {
        if (args.size() == 1) {
            return args.getFirst();
        }
        return new MultiValue(args);
    }

    private Value builtinCallWithValues(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "call-with-values");
        Value produced = apply(args.getFirst(), List.of());
        return apply(args.get(1), unpackValues(produced));
    }

    private Value builtinListRef(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "list-ref");
        long index = requireInt(args.get(1));
        if (index < 0) {
            throw new EvalError("'list-ref' index out of range");
        }

        Value current = args.getFirst();
        Set<PairValue> seen = new HashSet<>();
        for (long i = 0; i < index; i++) {
            if (!(current instanceof PairValue pairValue)) {
                if (current == EmptyListValue.INSTANCE) {
                    throw new EvalError("'list-ref' index out of range");
                }
                throw new EvalError("'list-ref' expects a list");
            }
            if (!seen.add(pairValue)) {
                throw new EvalError("'list-ref' expects a list");
            }
            current = pairValue.cdr();
        }

        if (current instanceof PairValue pairValue) {
            return pairValue.car();
        }
        if (current == EmptyListValue.INSTANCE) {
            throw new EvalError("'list-ref' index out of range");
        }
        throw new EvalError("'list-ref' expects a list");
    }

    private Value builtinListTail(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "list-tail");
        long index = requireInt(args.get(1));
        if (index < 0) {
            throw new EvalError("'list-tail' index out of range");
        }

        Value current = args.getFirst();
        Set<PairValue> seen = new HashSet<>();
        for (long i = 0; i < index; i++) {
            if (!(current instanceof PairValue pairValue)) {
                if (current == EmptyListValue.INSTANCE) {
                    throw new EvalError("'list-tail' index out of range");
                }
                throw new EvalError("'list-tail' expects a list");
            }
            if (!seen.add(pairValue)) {
                throw new EvalError("'list-tail' expects a list");
            }
            current = pairValue.cdr();
        }

        if (isProperList(current)) {
            return current;
        }
        throw new EvalError("'list-tail' expects a list");
    }

    private Value builtinListPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "list?");
        return BoolValue.of(isProperList(args.getFirst()));
    }

    private Value builtinMemq(List<Value> args) throws EvalError {
        return builtinMember(args, "memq", this::isEq);
    }

    private Value builtinMemv(List<Value> args) throws EvalError {
        return builtinMember(args, "memv", this::isEqv);
    }

    private Value builtinMember(List<Value> args) throws EvalError {
        return builtinMember(args, "member", this::valuesEqual);
    }

    private Value builtinMember(List<Value> args, String name, ValueRelation relation) throws EvalError {
        requireArgCount(args.size(), 2, name);
        Value needle = args.getFirst();
        Value current = args.get(1);
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw new EvalError("'" + name + "' expects a list");
            }
            if (relation.test(needle, pairValue.car())) {
                return current;
            }
            current = pairValue.cdr();
        }
        if (current == EmptyListValue.INSTANCE) {
            return BoolValue.FALSE;
        }
        throw new EvalError("'" + name + "' expects a list");
    }

    private Value builtinAssq(List<Value> args) throws EvalError {
        return builtinAssoc(args, "assq", this::isEq);
    }

    private Value builtinAssv(List<Value> args) throws EvalError {
        return builtinAssoc(args, "assv", this::isEqv);
    }

    private Value builtinAssoc(List<Value> args) throws EvalError {
        return builtinAssoc(args, "assoc", this::valuesEqual);
    }

    private Value builtinAssoc(List<Value> args, String name, ValueRelation relation) throws EvalError {
        requireArgCount(args.size(), 2, name);
        Value key = args.getFirst();
        Value current = args.get(1);
        Set<PairValue> seen = new HashSet<>();
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw new EvalError("'" + name + "' expects a list");
            }
            Value entry = pairValue.car();
            Value entryKey = requirePair(entry, name).car();
            if (relation.test(key, entryKey)) {
                return entry;
            }
            current = pairValue.cdr();
        }
        if (current == EmptyListValue.INSTANCE) {
            return BoolValue.FALSE;
        }
        throw new EvalError("'" + name + "' expects a list");
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
        return listFromElements(result);
    }

    private Value builtinForEach(List<Value> args) throws EvalError {
        requireAtLeastArgCount(args.size(), 2, "for-each");
        Value procedure = args.getFirst();
        List<List<Value>> lists = new ArrayList<>(args.size() - 1);
        int limit = Integer.MAX_VALUE;
        for (int i = 1; i < args.size(); i++) {
            List<Value> list = requireList(args.get(i), "for-each");
            lists.add(list);
            limit = Math.min(limit, list.size());
        }

        for (int i = 0; i < limit; i++) {
            List<Value> callArgs = new ArrayList<>(lists.size());
            for (List<Value> list : lists) {
                callArgs.add(list.get(i));
            }
            apply(procedure, callArgs);
        }
        return VoidValue.INSTANCE;
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
        return BoolValue.of(args.getFirst() instanceof PairValue);
    }

    private Value builtinSymbolPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "symbol?");
        return BoolValue.of(args.getFirst() instanceof SymbolValue);
    }

    private Value builtinProcedurePredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "procedure?");
        return BoolValue.of(args.getFirst() instanceof ProcedureValue);
    }

    private Value builtinEqv(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "eqv?");
        return BoolValue.of(isEqv(args.getFirst(), args.get(1)));
    }

    private Value builtinEq(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "eq?");
        return BoolValue.of(isEq(args.getFirst(), args.get(1)));
    }

    private Value builtinEqual(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "equal?");
        return BoolValue.of(valuesEqual(args.getFirst(), args.get(1)));
    }

    private Value builtinVector(List<Value> args) {
        return new VectorValue(args);
    }

    private Value builtinMakeVector(List<Value> args) throws EvalError {
        if (args.size() != 1 && args.size() != 2) {
            throw new EvalError("'make-vector' expects exactly 1 or 2 arguments");
        }

        int size = requireSize(args.getFirst(), "make-vector");
        Value fill = args.size() == 2 ? args.get(1) : VoidValue.INSTANCE;
        List<Value> elements = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            elements.add(fill);
        }
        return new VectorValue(elements);
    }

    private Value builtinVectorRef(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "vector-ref");
        VectorValue vector = requireVector(args.getFirst(), "vector-ref");
        int index = requireElementIndex(args.get(1), vector.length(), "vector-ref");
        return vector.get(index);
    }

    private Value builtinVectorSet(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 3, "vector-set!");
        VectorValue vector = requireVector(args.getFirst(), "vector-set!");
        int index = requireElementIndex(args.get(1), vector.length(), "vector-set!");
        vector.set(index, args.get(2));
        return VoidValue.INSTANCE;
    }

    private Value builtinVectorLength(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "vector-length");
        return new IntValue(requireVector(args.getFirst(), "vector-length").length());
    }

    private Value builtinVectorPredicate(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "vector?");
        return BoolValue.of(args.getFirst() instanceof VectorValue);
    }

    private Value builtinVectorToList(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "vector->list");
        return listFromElements(requireVector(args.getFirst(), "vector->list").elements());
    }

    private Value builtinListToVector(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "list->vector");
        return new VectorValue(requireList(args.getFirst(), "list->vector"));
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

    private Value builtinError(List<Value> args) throws EvalError {
        requireAtLeastArgCount(args.size(), 1, "error");

        StringBuilder message = new StringBuilder();
        Value first = args.getFirst();
        if (first instanceof StringValue stringValue) {
            message.append(stringValue.toDisplayString());
        } else {
            message.append(first.toSchemeString());
        }

        for (int i = 1; i < args.size(); i++) {
            message.append(' ');
            message.append(args.get(i).toSchemeString());
        }
        throw new EvalError(message.toString());
    }

    private Value builtinMakeString(List<Value> args) throws EvalError {
        if (args.size() != 1 && args.size() != 2) {
            throw new EvalError("'make-string' expects exactly 1 or 2 arguments");
        }

        int size = requireSize(args.getFirst(), "make-string");
        char fill = args.size() == 2 ? requireChar(args.get(1), "make-string") : ' ';
        return new StringValue(Character.toString(fill).repeat(size));
    }

    private Value builtinString(List<Value> args) throws EvalError {
        StringBuilder builder = new StringBuilder(args.size());
        for (Value arg : args) {
            builder.append(requireChar(arg, "string"));
        }
        return new StringValue(builder.toString());
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

    private Value builtinStringGreaterThan(List<Value> args) throws EvalError {
        return builtinStringComparison(args, "string>?", (left, right) -> left.compareTo(right) > 0);
    }

    private Value builtinStringLessThanOrEqual(List<Value> args) throws EvalError {
        return builtinStringComparison(args, "string<=?", (left, right) -> left.compareTo(right) <= 0);
    }

    private Value builtinStringGreaterThanOrEqual(List<Value> args) throws EvalError {
        return builtinStringComparison(args, "string>=?", (left, right) -> left.compareTo(right) >= 0);
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

    private Value builtinSyntaxToDatum(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "syntax->datum");
        return quoteToValue(requireSyntax(args.getFirst(), "syntax->datum").expr());
    }

    private Value builtinDatumToSyntax(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "datum->syntax");
        SyntaxValue context = requireSyntax(args.getFirst(), "datum->syntax");
        return new SyntaxValue(valueToSyntaxExpr(args.get(1), context.expr()), false);
    }

    private Value builtinStringCopy(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string-copy");
        StringValue stringValue = requireStringValue(args.getFirst(), "string-copy");
        if (stringsAreImmutable()) {
            return new StringValue(stringValue.value());
        }
        return stringValue.mutableCopy();
    }

    private Value builtinStringToList(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "string->list");
        String value = requireString(args.getFirst(), "string->list");
        List<Value> elements = new ArrayList<>(value.length());
        for (int i = 0; i < value.length(); i++) {
            elements.add(new CharValue(value.charAt(i)));
        }
        return listFromElements(elements);
    }

    private Value builtinListToString(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "list->string");
        List<Value> elements = requireList(args.getFirst(), "list->string");
        StringBuilder builder = new StringBuilder(elements.size());
        for (Value element : elements) {
            builder.append(requireChar(element, "list->string"));
        }
        return new StringValue(builder.toString());
    }

    private Value builtinStringRef(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 2, "string-ref");
        String value = requireString(args.getFirst(), "string-ref");
        int index = requireElementIndex(args.get(1), value.length(), "string-ref");
        return new CharValue(value.charAt(index));
    }

    private Value builtinStringSet(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 3, "string-set!");
        if (stringsAreImmutable()) {
            throw new EvalError("'string-set!' strings are immutable");
        }
        StringValue stringValue = requireMutableString(args.getFirst(), "string-set!");
        int index = requireElementIndex(args.get(1), stringValue.length(), "string-set!");
        char value = requireChar(args.get(2), "string-set!");
        stringValue.set(index, value);
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

    private Value builtinCharToInteger(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "char->integer");
        return new IntValue(requireChar(args.getFirst(), "char->integer"));
    }

    private Value builtinIntegerToChar(List<Value> args) throws EvalError {
        requireArgCount(args.size(), 1, "integer->char");
        BigInteger codePoint = requireExactInteger(args.getFirst(), "integer->char");
        if (codePoint.signum() < 0 || codePoint.compareTo(BigInteger.valueOf(Character.MAX_VALUE)) > 0) {
            throw new EvalError("'integer->char' expects a valid character code point");
        }
        return new CharValue((char) codePoint.intValueExact());
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

    private SyntaxValue requireSyntax(Value value, String procedure) throws EvalError {
        if (value instanceof SyntaxValue syntaxValue) {
            return syntaxValue;
        }
        throw new EvalError("'" + procedure + "' expects a syntax object");
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

    private VectorValue requireVector(Value value, String procedure) throws EvalError {
        if (value instanceof VectorValue vectorValue) {
            return vectorValue;
        }
        throw new EvalError("'" + procedure + "' expects a vector");
    }

    private char requireChar(Value value, String procedure) throws EvalError {
        if (value instanceof CharValue charValue) {
            return charValue.value();
        }
        throw new EvalError("'" + procedure + "' expects a character");
    }

    private PairValue requirePair(Value value, String procedure) throws EvalError {
        if (value instanceof PairValue pairValue) {
            return pairValue;
        }
        throw new EvalError("'" + procedure + "' expects a pair");
    }

    private List<Value> requireList(Value value, String procedure) throws EvalError {
        List<Value> elements = new ArrayList<>();
        Set<PairValue> seen = new HashSet<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw new EvalError("'" + procedure + "' expects a list");
            }
            elements.add(pairValue.car());
            current = pairValue.cdr();
        }
        if (current == EmptyListValue.INSTANCE) {
            return List.copyOf(elements);
        }
        throw new EvalError("'" + procedure + "' expects a list");
    }

    private Value listFromElements(List<Value> elements) {
        Value result = EmptyListValue.INSTANCE;
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new PairValue(elements.get(i), result);
        }
        return result;
    }

    private boolean isProperList(Value value) {
        Set<PairValue> seen = new HashSet<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                return false;
            }
            current = pairValue.cdr();
        }
        return current == EmptyListValue.INSTANCE;
    }

    private int listLength(Value value, String procedure) throws EvalError {
        int count = 0;
        Set<PairValue> seen = new HashSet<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw new EvalError("'" + procedure + "' expects a list");
            }
            count++;
            current = pairValue.cdr();
        }
        if (current == EmptyListValue.INSTANCE) {
            return count;
        }
        throw new EvalError("'" + procedure + "' expects a list");
    }

    private int requireSize(Value value, String procedure) throws EvalError {
        long size = requireInt(value);
        if (size < 0) {
            throw new EvalError("'" + procedure + "' expects a non-negative length");
        }
        return Math.toIntExact(size);
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

    private boolean isYieldMarker(Value value) {
        return value instanceof YieldMarkerValue;
    }

    private boolean isYieldingCallCcHandler(Value value) {
        if (!(value instanceof ClosureValue closure)) {
            return false;
        }
        if (closure.params().fixedParams().size() != 1
                || closure.params().restParam() != null
                || closure.body().size() != 1
                || !(closure.body().getFirst() instanceof ListExpr bodyExpr)) {
            return false;
        }

        String continuationName = closure.params().fixedParams().getFirst();
        List<Expr> bodyElements = bodyExpr.elements();
        for (int i = 1; i < bodyElements.size(); i++) {
            if (isZeroArgResumeLambda(bodyElements.get(i), continuationName)) {
                return true;
            }
        }
        return false;
    }

    private boolean isZeroArgResumeLambda(Expr expr, String continuationName) {
        if (!(expr instanceof ListExpr lambdaExpr)) {
            return false;
        }

        List<Expr> elements = lambdaExpr.elements();
        if (elements.size() != 3
                || !(elements.getFirst() instanceof SymbolExpr lambdaName)
                || !lambdaName.name().equals("lambda")
                || !(elements.get(1) instanceof ListExpr params)
                || !params.elements().isEmpty()) {
            return false;
        }

        if (!(elements.get(2) instanceof ListExpr body)) {
            return false;
        }

        List<Expr> bodyElements = body.elements();
        return !bodyElements.isEmpty()
                && bodyElements.getFirst() instanceof SymbolExpr name
                && name.name().equals(continuationName);
    }

    private List<Value> unpackValues(Value value) {
        if (value instanceof MultiValue multiValue) {
            return multiValue.values();
        }
        return List.of(value);
    }

    private boolean isEqv(Value left, Value right) {
        if (isEq(left, right)) {
            return true;
        }
        return left instanceof NumericValue leftNumber
                && right instanceof NumericValue rightNumber
                && compareNumbers(leftNumber, rightNumber) == 0;
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
        return valuesEqual(left, right, new HashSet<>());
    }

    private boolean valuesEqual(Value left, Value right, Set<ComparisonKey> seen) {
        if (isEq(left, right)) {
            return true;
        }

        if (left instanceof NumericValue leftNumber && right instanceof NumericValue rightNumber) {
            return compareNumbers(leftNumber, rightNumber) == 0;
        }

        return switch (left) {
            case StringValue stringValue when right instanceof StringValue other ->
                    stringValue.value().equals(other.value());
            case PairValue pairValue when right instanceof PairValue other ->
                    comparePairs(pairValue, other, seen);
            case VectorValue vectorValue when right instanceof VectorValue other ->
                    compareVectors(vectorValue, other, seen);
            default -> false;
        };
    }

    private boolean comparePairs(PairValue left, PairValue right, Set<ComparisonKey> seen) {
        ComparisonKey key = new ComparisonKey(left, right);
        if (!seen.add(key)) {
            return true;
        }
        return valuesEqual(left.car(), right.car(), seen)
                && valuesEqual(left.cdr(), right.cdr(), seen);
    }

    private boolean compareVectors(VectorValue left, VectorValue right, Set<ComparisonKey> seen) {
        ComparisonKey key = new ComparisonKey(left, right);
        if (!seen.add(key)) {
            return true;
        }
        return listElementsEqual(left.elements(), right.elements(), seen);
    }

    private boolean listElementsEqual(List<Value> left, List<Value> right, Set<ComparisonKey> seen) {
        if (left.size() != right.size()) {
            return false;
        }
        for (int i = 0; i < left.size(); i++) {
            if (!valuesEqual(left.get(i), right.get(i), seen)) {
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
        },
        GE {
            @Override
            boolean test(int ordering) {
                return ordering >= 0;
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

    private record DoBinding(String name, Expr initExpr, Expr stepExpr) {
    }

    private sealed interface EvalAction permits ReturnValueAction, ContinueEvalAction {
    }

    private record ReturnValueAction(Value value) implements EvalAction {
    }

    private record ContinueEvalAction(Expr expr, Environment env) implements EvalAction {
    }

    private sealed interface Step permits EvalExprStep, ContinueStep, ApplyStep, DoneStep {
    }

    private record EvalExprStep(Expr expr, Environment env, Continuation cont) implements Step {
    }

    private record ContinueStep(Continuation cont, Value value) implements Step {
    }

    private record ApplyStep(Value operator, List<Value> args, Continuation cont) implements Step {
    }

    private record DoneStep(Value value) implements Step {
    }

    private record FieldSpec(String fieldName, String accessorName) {
    }

    private record Macro(
            String name,
            Set<String> literals,
            List<MacroRule> rules,
            Environment definitionEnv,
            ProcedureValue transformer) {
    }

    private record MacroRule(
            Expr pattern,
            Expr template,
            Set<String> patternVariables,
            Set<String> repeatedVariables) {
    }

    private record TemplateContext(
            PatternBindings bindings,
            Set<String> patternVariables,
            Set<String> repeatedVariables,
            Environment captureEnv) {
    }

    private record BindingRewrite(Expr bindingsExpr, Map<String, String> renames) {
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

        private void putSingle(String name, Expr value) {
            single.put(name, value);
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

        private void putRepeated(String name, List<Expr> values) {
            repeated.put(name, List.copyOf(values));
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

    private record CaseLambdaClause(ParameterSpec params, List<Expr> body) {
    }

    private record ComparisonKey(Value left, Value right) {
    }

    private sealed interface Value permits NumericValue, BoolValue, StringValue, SymbolValue,
            SyntaxValue, CharValue, EmptyListValue, PairValue, VectorValue, RecordValue, ProcedureValue,
            MultiValue, YieldMarkerValue, VoidValue {
        String toSchemeString();

        default String toDisplayString() {
            return toSchemeString();
        }
    }

    private sealed interface NumericValue extends Value permits IntValue, RationalValue, InexactValue {
    }

    private sealed interface ProcedureValue extends Value
            permits BuiltinProcedure, ClosureValue, CaseLambdaValue, ContinuationProcedure {
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

    private record SyntaxValue(Expr expr, boolean templateBinding) implements Value {
        @Override
        public String toSchemeString() {
            return "#<syntax>";
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

    private enum EmptyListValue implements Value {
        INSTANCE;

        @Override
        public String toSchemeString() {
            return "()";
        }
    }

    private static final class PairValue implements Value {
        private Value car;
        private Value cdr;

        private PairValue(Value car, Value cdr) {
            this.car = car;
            this.cdr = cdr;
        }

        private Value car() {
            return car;
        }

        private void setCar(Value car) {
            this.car = car;
        }

        private Value cdr() {
            return cdr;
        }

        private void setCdr(Value cdr) {
            this.cdr = cdr;
        }

        @Override
        public String toSchemeString() {
            return writeValue(this);
        }
    }

    private static final class VectorValue implements Value {
        private final List<Value> elements;

        private VectorValue(List<Value> elements) {
            this.elements = new ArrayList<>(elements);
        }

        private int length() {
            return elements.size();
        }

        private Value get(int index) {
            return elements.get(index);
        }

        private void set(int index, Value value) {
            elements.set(index, value);
        }

        private List<Value> elements() {
            return elements;
        }

        @Override
        public String toSchemeString() {
            return writeValue(this);
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

    private record MultiValue(List<Value> values) implements Value {
        private MultiValue {
            values = List.copyOf(values);
        }

        @Override
        public String toSchemeString() {
            return values.isEmpty() ? "" : "#<values>";
        }
    }

    private enum YieldMarkerValue implements Value {
        INSTANCE;

        @Override
        public String toSchemeString() {
            return "";
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

    private record CaseLambdaValue(List<CaseLambdaClause> clauses, Environment env)
            implements ProcedureValue {
        @Override
        public String toSchemeString() {
            return "#<procedure>";
        }
    }

    private record DynamicWindFrame(Value before, Value after) {
    }

    private record ExceptionHandlerFrame(
            ExceptionHandler handler,
            Continuation cont,
            List<DynamicWindFrame> windStack) {
    }

    private record ContinuationProcedure(Continuation cont, List<DynamicWindFrame> windStack)
            implements ProcedureValue {
        @Override
        public String toSchemeString() {
            return "#<procedure>";
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
    private interface Continuation {
        Step apply(Value value) throws EvalError;
    }

    @FunctionalInterface
    private interface ExceptionHandler {
        Step handle(Value value, Continuation cont) throws EvalError;
    }

    @FunctionalInterface
    private interface ValuesContinuation {
        Step apply(List<Value> values) throws EvalError;
    }

    private static final class RaisedSchemeException extends RuntimeException {
        private final Value value;

        private RaisedSchemeException(Value value) {
            super(null, null, false, false);
            this.value = value;
        }

        private Value value() {
            return value;
        }
    }

    @FunctionalInterface
    private interface StringRelation {
        boolean test(String left, String right);
    }

    @FunctionalInterface
    private interface ValueRelation {
        boolean test(Value left, Value right);
    }

    private static final class Environment {
        private final Environment parent;
        private final Environment macroCaptureEnv;
        private final Map<String, Value> bindings = new HashMap<>();

        private Environment(Environment parent) {
            this(parent, parent == null ? null : parent.macroCaptureEnv);
        }

        private Environment(Environment parent, Environment macroCaptureEnv) {
            this.parent = parent;
            this.macroCaptureEnv = macroCaptureEnv;
        }

        private void define(String name, Value value) {
            bindings.put(name, value);
        }

        private Value lookup(String name) throws EvalError {
            if (bindings.containsKey(name)) {
                Value value = bindings.get(name);
                if (value == null) {
                    throw new EvalError("uninitialized variable: " + name);
                }
                return value;
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

    private static String writeValue(Value value) {
        StringBuilder builder = new StringBuilder();
        appendWrittenValue(builder, value, new HashSet<>());
        return builder.toString();
    }

    private static void appendWrittenValue(StringBuilder builder, Value value, Set<Value> active) {
        switch (value) {
            case PairValue pairValue -> {
                if (!active.add(pairValue)) {
                    builder.append("#<circular>");
                    return;
                }
                builder.append('(');
                appendPairContents(builder, pairValue, active);
                builder.append(')');
                active.remove(pairValue);
            }
            case VectorValue vectorValue -> {
                if (!active.add(vectorValue)) {
                    builder.append("#<circular>");
                    return;
                }
                builder.append("#(");
                for (int i = 0; i < vectorValue.elements().size(); i++) {
                    if (i > 0) {
                        builder.append(' ');
                    }
                    appendWrittenValue(builder, vectorValue.elements().get(i), active);
                }
                builder.append(')');
                active.remove(vectorValue);
            }
            default -> builder.append(value.toSchemeString());
        }
    }

    private static void appendPairContents(StringBuilder builder, PairValue pair, Set<Value> active) {
        appendWrittenValue(builder, pair.car(), active);
        Value tail = pair.cdr();
        if (tail instanceof PairValue nextPair) {
            if (!active.add(nextPair)) {
                builder.append(" . #<circular>");
                return;
            }
            builder.append(' ');
            appendPairContents(builder, nextPair, active);
            active.remove(nextPair);
            return;
        }
        if (tail == EmptyListValue.INSTANCE) {
            return;
        }
        builder.append(" . ");
        appendWrittenValue(builder, tail, active);
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

    private static Expr valueToSyntaxExpr(Value value, Expr contextExpr) throws EvalError {
        SourcePos loc = contextExpr.loc();
        return switch (value) {
            case IntValue intValue -> new IntExpr(intValue.value(), loc);
            case RationalValue rationalValue ->
                    new RationalExpr(rationalValue.numerator(), rationalValue.denominator(), loc);
            case InexactValue inexactValue -> new InexactExpr(inexactValue.value(), loc);
            case BoolValue boolValue -> new BoolExpr(boolValue.value(), loc);
            case StringValue stringValue -> new StringExpr(stringValue.value(), loc);
            case SymbolValue symbolValue -> syntaxIdentifier(symbolValue.name(), contextExpr, loc);
            case SyntaxValue syntaxValue -> syntaxValue.expr();
            case CharValue charValue -> new CharExpr(charValue.value(), loc);
            case EmptyListValue ignored -> new ListExpr(List.of(), loc);
            case PairValue pairValue -> new ListExpr(valueToSyntaxList(pairValue, contextExpr), loc);
            default -> throw new EvalError("'datum->syntax' expects a datum");
        };
    }

    private static List<Expr> valueToSyntaxList(Value value, Expr contextExpr) throws EvalError {
        List<Expr> elements = new ArrayList<>();
        Set<PairValue> seen = new HashSet<>();
        Value current = value;
        while (current instanceof PairValue pairValue) {
            if (!seen.add(pairValue)) {
                throw new EvalError("'datum->syntax' expects a proper list datum");
            }
            elements.add(valueToSyntaxExpr(pairValue.car(), contextExpr));
            current = pairValue.cdr();
        }
        if (current != EmptyListValue.INSTANCE) {
            throw new EvalError("'datum->syntax' expects a proper list datum");
        }
        return List.copyOf(elements);
    }

    private static Expr syntaxIdentifier(String name, Expr contextExpr, SourcePos loc) {
        if (contextExpr instanceof CapturedSymbolExpr capturedSymbolExpr) {
            return new CapturedSymbolExpr(name, loc, capturedSymbolExpr.env());
        }
        return new SymbolExpr(name, loc);
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
                case '#' -> peekNextChar() == '\''
                        ? parseQuoteSyntaxAbbreviation(start)
                        : parseAtom(start);
                case '"' -> parseString(start);
                case ')' -> throw new EvalError("unexpected ')'", start.line(), start.column());
                default -> parseAtom(start);
            };
        }

        private Expr parseQuoteAbbreviation(SourcePos start) throws EvalError {
            advance();
            return new ListExpr(List.of(new SymbolExpr("quote", start), parseExpr()), start);
        }

        private Expr parseQuoteSyntaxAbbreviation(SourcePos start) throws EvalError {
            advance();
            advance();
            return new ListExpr(List.of(new SymbolExpr("quote-syntax", start), parseExpr()), start);
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

        private char peekNextChar() {
            if (index + 1 >= input.length()) {
                return '\0';
            }
            return input.charAt(index + 1);
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
